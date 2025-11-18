#include "ir_gen.h"
#include <futex.h>
#include "targets/targets.h"

// Maps param_num -> TB_Node*
static thread_local TB_Node** parameter_map;

static thread_local Cuik_Type* function_type;
static thread_local const char* function_name;

static thread_local TB_PassingRule func_return_rule;

static _Thread_local TB_Node* current_scope;

static void emit_location(TranslationUnit* tu, TB_Function* func, SourceLoc loc);

static int switch_cmp(const void* a, const void* b) {
    const TB_SwitchEntry* aa = a;
    const TB_SwitchEntry* bb = b;
    return (aa->key > bb->key) - (aa->key < bb->key);
}

static uint64_t get_ir_ordinal(TranslationUnit* tu, Stmt* stmt) {
    return ((uint64_t) tu->local_ordinal << 32ull) | stmt->decl.local_ordinal;
}

// in theory, multiple threads can race here but the meme is that they
// all produce the same values so we can just let them race lmao.
TB_DebugType* cuik__as_tb_debug_type(TB_Module* mod, Cuik_Type* t) {
    TB_DebugType* old_val = atomic_load(&t->debug_type);
    if (old_val != NULL) {
        return old_val;
    }

    TB_DebugType* result = NULL;
    switch (t->kind) {
        case KIND_VOID:
        result = tb_debug_get_void(mod);
        break;

        case KIND_BOOL:
        result = tb_debug_get_bool(mod);
        break;

        case KIND_CHAR: case KIND_SHORT: case KIND_INT: case KIND_LONG: case KIND_LLONG:
        result = tb_debug_get_integer(mod, !t->is_unsigned, t->size * 8);
        break;

        case KIND_FLOAT:
        result = tb_debug_get_float32(mod);
        break;

        case KIND_DOUBLE:
        result = tb_debug_get_float64(mod);
        break;

        case KIND_ENUM:
        result = tb_debug_get_integer(mod, true, 32);
        break;

        case KIND_PTR:
        result = tb_debug_create_ptr(mod, cuik__as_tb_debug_type(mod, cuik_canonical_type(t->ptr_to)));
        break;

        case KIND_ARRAY:
        result = tb_debug_create_array(mod, cuik__as_tb_debug_type(mod, cuik_canonical_type(t->array.of)), t->array.count);
        break;

        case KIND_STRUCT:
        case KIND_UNION: {
            Member* kids = t->record.kids;
            size_t count = t->record.kid_count;
            const char* tag = t->also_known_as ? t->also_known_as : t->record.name;

            result = t->kind == KIND_STRUCT ? tb_debug_create_struct(mod, -1, tag) : tb_debug_create_union(mod, -1, tag);
            atomic_exchange(&t->debug_type, result);

            TB_DebugType** list = tb_debug_record_begin(mod, result, count);
            for (size_t i = 0; i < count; i++) {
                Member* member = &kids[i];

                TB_DebugType* base = cuik__as_tb_debug_type(mod, cuik_canonical_type(member->type));
                TB_DebugType* field = NULL;
                if (member->name == NULL) {
                    // if we have unnamed members we just do _N where N is just ticked by the counter
                    char buf[8];
                    snprintf(buf, 8, "_%zu", i);

                    field = tb_debug_create_field(mod, base, -1, buf, member->offset);
                } else {
                    field = tb_debug_create_field(mod, base, -1, member->name, member->offset);
                }

                list[i] = field;
            }

            tb_debug_record_end(result, t->size, t->align);
            return result;
        }

        case KIND_FUNC: {
            bool has_return = cuik_canonical_type(t->func.return_type)->kind != KIND_VOID;
            result = tb_debug_create_func(mod, TB_STDCALL, t->func.param_count, has_return, t->has_varargs);

            if (has_return) {
                *tb_debug_func_returns(result) = cuik__as_tb_debug_type(mod, cuik_canonical_type(t->func.return_type));
            }

            TB_DebugType** params = tb_debug_func_params(result);
            Param* param_list = t->func.param_list;
            for (size_t i = 0; i < t->func.param_count; i++) {
                TB_DebugType* type = cuik__as_tb_debug_type(mod, cuik_canonical_type(param_list[i].type));

                if (param_list[i].name == NULL) {
                    // if we have unnamed members we just do _N where N is just ticked by the counter
                    char buf[8];
                    snprintf(buf, 8, "arg%zu", i);

                    params[i] = tb_debug_create_field(mod, type, -1, buf, 0);
                } else {
                    params[i] = tb_debug_create_field(mod, type, -1, param_list[i].name, 0);
                }
            }
            break;
        }

        default:
        abort(); // TODO
    }

    assert(result);
    atomic_exchange(&t->debug_type, result);
    return result;
}

int count_max_tb_init_objects(InitNode* root_node) {
    int sum = root_node->kids_count;
    for (InitNode* k = root_node->kid; k != NULL; k = k->next) {
        sum += count_max_tb_init_objects(k);
        if (k->expr && get_root_subexpr(k->expr)->op == EXPR_CONST && get_root_subexpr(k->expr)->const_val.tag == CUIK_CONST_ADDR) {
            sum += 1;
        }
    }

    return sum;
}

TB_ModuleSectionHandle get_variable_storage(TB_Module* m, const Attribs* attrs, bool is_const) {
    if (attrs->is_tls) {
        return tb_module_get_tls(m);
    } else if (is_const) {
        return tb_module_get_rdata(m);
    } else {
        return tb_module_get_data(m);
    }
}

static void eval_global_initializer(TranslationUnit* tu, TB_Global* g, InitNode* n, int offset);
static void gen_global_initializer(TranslationUnit* tu, TB_Global* g, Cuik_Type* type, Cuik_Expr* e, size_t offset) {
    assert(type != NULL);
    size_t type_size = type->size;

    // defaults to zeros because that's how TB initializers work
    if (e == NULL) {
        return;
    }

    // string literals
    Subexpr* s = get_root_subexpr(e);
    if (s->op == EXPR_STR || s->op == EXPR_WSTR) {
        size_t len = atoms_len(s->str) + 1;
        if (type->kind == KIND_PTR) {
            uint32_t hash = tb__murmur3_32(s->str, len);

            TB_Global* dummy = tb_global_create(tu->ir_mod, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
            ((TB_Symbol*) dummy)->ordinal = ((uint64_t) tu->local_ordinal << 32ull) | hash;
            tb_global_set_storage(tu->ir_mod, tb_module_get_rdata(tu->ir_mod), dummy, len, cuik_canonical_type(type->ptr_to)->align, 1);

            char* dst = tb_global_add_region(tu->ir_mod, dummy, 0, len);
            memcpy(dst, s->str, len);

            tb_global_add_symbol_reloc(tu->ir_mod, g, offset, (TB_Symbol*) dummy);
        } else {
            char* dst = tb_global_add_region(tu->ir_mod, g, offset, type->size);
            memcpy(dst, s->str, len);
        }
        return;
    }

    // try to emit global initializer
    if (s->op == EXPR_INITIALIZER) {
        Subexpr* s = get_root_subexpr(e);
        eval_global_initializer(tu, g, s->init.root, offset);
        return;
    }

    // try to emit constant integer + constant addresses
    assert(s->op == EXPR_CONST);

    Cuik_ConstVal value = s->const_val;
    uint64_t int_form = 0;
    if (value.tag == CUIK_CONST_ADDR) {
        Stmt* stmt = value.s.base;
        assert((stmt->op == STMT_GLOBAL_DECL || stmt->op == STMT_FUNC_DECL) && "could not resolve as constant initializer");

        tb_global_add_symbol_reloc(tu->ir_mod, g, offset, stmt->backing.s);
        int_form = value.s.offset;
    } else if (value.tag == CUIK_CONST_STR) {
        size_t len = atoms_len(value.str);
        if (type->kind == KIND_PTR) {
            uint32_t hash = tb__murmur3_32(value.str, len);

            TB_Global* dummy = tb_global_create(tu->ir_mod, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
            ((TB_Symbol*) dummy)->ordinal = ((uint64_t) tu->local_ordinal << 32ull) | hash;
            tb_global_set_storage(tu->ir_mod, tb_module_get_rdata(tu->ir_mod), dummy, len, cuik_canonical_type(type->ptr_to)->align, 1);

            char* dst = tb_global_add_region(tu->ir_mod, dummy, 0, len);
            memcpy(dst, value.str, len);

            tb_global_add_symbol_reloc(tu->ir_mod, g, offset, (TB_Symbol*) dummy);
        } else {
            char* dst = tb_global_add_region(tu->ir_mod, g, offset, type->size);
            memcpy(dst, value.str, len);
        }
    } else if (value.tag == CUIK_CONST_INT) {
        int_form = value.i;
    } else if (value.tag == CUIK_CONST_FLOAT) {
        Cuik_TypeKind kind = cuik_canonical_type(e->cast_types[e->count - 1])->kind;
        if (kind == KIND_DOUBLE) {
            typedef union { double f; uint64_t u; } F64U64;
            int_form = (F64U64){ value.f }.u;
        } else if (kind == KIND_FLOAT) {
            typedef union { float f; uint32_t u; } F32U32;
            int_form = (F32U32){ value.f }.u;
        } else {
            assert(0 && "TODO");
        }
    } else {
        assert(0 && "TODO");
    }

    if (int_form != 0) {
        uint8_t* region = tb_global_add_region(tu->ir_mod, g, offset, type_size);
        if (TARGET_NEEDS_BYTESWAP(tu->target)) {
            // reverse copy
            uint8_t* src = (uint8_t*) &int_form;
            size_t top = type_size - 1;

            for (size_t i = 0; i < type_size; i++) {
                region[i] = src[top - i];
            }
        } else {
            memcpy(region, &int_form, type->size);
        }
    }
}

static void eval_global_initializer(TranslationUnit* tu, TB_Global* g, InitNode* n, int offset) {
    if (n->kid != NULL) {
        for (InitNode* k = n->kid; k != NULL; k = k->next) {
            eval_global_initializer(tu, g, k, offset);
        }
    } else {
        Cuik_Type* child_type = cuik_canonical_type(n->type);
        gen_global_initializer(tu, g, child_type, n->expr, offset + n->offset);
    }
}

TB_Module* cuik_get_tb_module(TranslationUnit* restrict tu) {
    return tu->ir_mod;
}

void cuikcg_allocate_ir(TranslationUnit* tu, TB_Module* m, bool debug) {
    tu->ir_mod = tu->parent->ir_mod = m;
    tu->has_tb_debug_info = debug;

    CUIK_TIMED_BLOCK("IR alloc") {
        size_t count = dyn_array_length(tu->top_level_stmts);
        for (size_t i = 0; i < count; i++) {
            Stmt* s = tu->top_level_stmts[i];
            if ((s->flags & STMT_FLAGS_HAS_IR_BACKING) == 0) continue;
            if (!s->decl.attrs.is_used) continue;

            Atom name = s->decl.name;
            size_t len = atoms_len(name);

            TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
            TB_SymbolTag tag   = s->op == STMT_FUNC_DECL ? TB_SYMBOL_FUNCTION : TB_SYMBOL_GLOBAL;

            if (s->op == STMT_FUNC_DECL) {
                TB_Function* func = tb_function_create(tu->ir_mod, len, name, linkage);
                s->backing.f = func;
            } else {
                Cuik_Type* type = cuik_canonical_type(s->decl.type);

                // if we have a TB module, fill it up with declarations
                if (s->decl.attrs.is_tls) {
                    tb_module_set_tls_index(tu->ir_mod, sizeof("_tls_index")-1, "_tls_index");
                }

                TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
                TB_DebugType* dbg_type = NULL;
                if (tu->has_tb_debug_info) {
                    dbg_type = cuik__as_tb_debug_type(tu->ir_mod, cuik_canonical_type(s->decl.type));
                }

                // allocate new
                s->backing.g = tb_global_create(tu->ir_mod, len, name, dbg_type, linkage);
            }
            s->backing.s->ordinal = get_ir_ordinal(tu, s);
        }
    }
}
