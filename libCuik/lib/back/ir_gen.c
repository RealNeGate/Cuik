#ifdef CUIK_USE_TB
#include "ir_gen.h"
#include <futex.h>
#include <targets/targets.h>

atomic_flag irgen_defined_tls_index;

// Maps param_num -> TB_Node*
static thread_local TB_Node** parameter_map;

static thread_local Cuik_Type* function_type;
static thread_local const char* function_name;

// For aggregate returns
static _Thread_local TB_Node* return_value_address;

static void fallthrough_label(TB_Function* func, TB_Label target) {
    if (!tb_basic_block_is_complete(func, tb_inst_get_label(func))) {
        tb_inst_goto(func, target);
    }
    tb_inst_set_label(func, target);
}

TB_DebugType* cuik__as_tb_debug_type(TB_Module* mod, Cuik_Type* t) {
    if (t->debug_type != NULL) {
        return t->debug_type;
    }

    TB_DebugType* result = NULL;
    switch (t->kind) {
        case KIND_VOID:
        return (t->debug_type = tb_debug_get_void(mod));

        case KIND_BOOL:
        return (t->debug_type = tb_debug_get_bool(mod));

        case KIND_CHAR: case KIND_SHORT: case KIND_INT: case KIND_LONG: case KIND_LLONG:
        return (t->debug_type = tb_debug_get_integer(mod, !t->is_unsigned, t->size * 8));

        case KIND_FLOAT:
        return (t->debug_type = tb_debug_get_float(mod, TB_FLT_32));

        case KIND_DOUBLE:
        return (t->debug_type = tb_debug_get_float(mod, TB_FLT_64));

        case KIND_ENUM:
        return (t->debug_type = tb_debug_get_integer(mod, true, 32));

        case KIND_PTR:
        return (t->debug_type = tb_debug_create_ptr(mod, cuik__as_tb_debug_type(mod, cuik_canonical_type(t->ptr_to))));

        case KIND_FUNC:
        return (t->debug_type = tb_debug_create_ptr(mod, tb_debug_get_void(mod)));

        case KIND_ARRAY:
        return (t->debug_type = tb_debug_create_array(mod, cuik__as_tb_debug_type(mod, cuik_canonical_type(t->array_of)), t->array_count));

        case KIND_STRUCT:
        case KIND_UNION: {
            Member* kids = t->record.kids;
            size_t count = t->record.kid_count;
            const char* tag = t->record.name;

            TB_DebugType** list = cuik_malloc(count * sizeof(TB_DebugType*));
            TB_DebugType* rec = t->kind == KIND_STRUCT ? tb_debug_create_struct(mod, tag) : tb_debug_create_union(mod, tag);
            t->debug_type = rec;

            int unnamed_count = 0;
            for (size_t i = 0; i < count; i++) {
                Member* member = &kids[i];

                TB_DebugType* base = cuik__as_tb_debug_type(mod, cuik_canonical_type(member->type));
                TB_DebugType* field = NULL;
                if (member->name == NULL) {
                    // if we have unnamed members we just do _N where N is just ticked by the counter
                    char buf[8];
                    snprintf(buf, 8, "_%d", unnamed_count++);

                    field = tb_debug_create_field(mod, base, buf, member->offset);
                } else {
                    field = tb_debug_create_field(mod, base, member->name, member->offset);
                }

                list[i] = field;
            }

            tb_debug_complete_record(rec, list, count, t->size, t->align);
            return rec;
        }

        default:
        abort(); // TODO
    }
}

static TB_Node* cast_reg(TB_Function* func, TB_Node* reg, const Cuik_Type* src, const Cuik_Type* dst) {
    if (dst->kind == KIND_VOID) {
        return reg;
    }

    // Cast into correct type
    if (cuik_type_is_integer(src) && cuik_type_is_integer(dst)) {
        if (dst->size > src->size) {
            // up-casts
            if (src->is_unsigned) {
                reg = tb_inst_zxt(func, reg, ctype_to_tbtype(dst));
            } else {
                reg = tb_inst_sxt(func, reg, ctype_to_tbtype(dst));
            }
        } else if (dst->size < src->size) {
            // down-casts
            reg = tb_inst_trunc(func, reg, ctype_to_tbtype(dst));
        }
    } else if (src->kind == KIND_ARRAY && dst->kind == KIND_BOOL) {
        reg = tb_inst_bool(func, true);
    } else if (src->kind != KIND_BOOL && dst->kind == KIND_BOOL) {
        TB_DataType dt = reg->dt;
        TB_Node* comparand;
        if (dt.type == TB_FLOAT && dt.data == TB_FLT_32) {
            comparand = tb_inst_float32(func, 0.0f);
        } else if (dt.type == TB_FLOAT && dt.data == TB_FLT_64) {
            comparand = tb_inst_float64(func, 0.0);
        } else {
            comparand = tb_inst_uint(func, dt, 0);
        }

        reg = tb_inst_cmp_ne(func, reg, comparand);
    } else if (src->kind == KIND_BOOL && cuik_type_is_integer(dst)) {
        reg = tb_inst_zxt(func, reg, ctype_to_tbtype(dst));
    } else if (cuik_type_is_integer(src) && dst->kind == KIND_FUNC) {
        // integer -> function
        reg = tb_inst_int2ptr(func, reg);
    } else if (src->kind == KIND_FUNC && cuik_type_is_integer(dst)) {
        // function -> integer
        reg = tb_inst_ptr2int(func, reg, ctype_to_tbtype(dst));
    } else if (cuik_type_is_integer(src) && dst->kind == KIND_PTR) {
        reg = tb_inst_int2ptr(func, reg);
    } else if (src->kind == KIND_PTR && cuik_type_is_integer(dst)) {
        reg = tb_inst_ptr2int(func, reg, ctype_to_tbtype(dst));
    } else if (src->kind == KIND_PTR && dst->kind == KIND_PTR) {
        /* TB has opaque pointers, nothing needs to be done. */
    } else if (src->kind == KIND_FLOAT && dst->kind == KIND_DOUBLE) {
        TB_DataType dt = reg->dt;

        if (!(dt.type == TB_FLOAT && dt.data == TB_FLT_64 && dt.width == 0)) {
            reg = tb_inst_fpxt(func, reg, TB_TYPE_F64);
        }
    } else if (src->kind == KIND_DOUBLE && dst->kind == KIND_FLOAT) {
        TB_DataType dt = reg->dt;

        if (!(dt.type == TB_FLOAT && dt.data == TB_FLT_32 && dt.width == 0)) {
            reg = tb_inst_trunc(func, reg, TB_TYPE_F32);
        }
    } else if (cuik_type_is_float(src) && cuik_type_is_integer(dst)) {
        reg = tb_inst_float2int(func, reg, ctype_to_tbtype(dst), !dst->is_unsigned);
    } else if (cuik_type_is_integer(src) && cuik_type_is_float(dst)) {
        reg = tb_inst_int2float(func, reg, ctype_to_tbtype(dst), !src->is_unsigned);
    }

    assert(reg);
    return reg;
}

static TB_Node* cvt2rval(TranslationUnit* tu, TB_Function* func, const IRVal v, const Expr* e) {
    Cuik_Type* src = cuik_canonical_type(e->type);
    bool is_volatile = CUIK_QUAL_TYPE_HAS(e->type, CUIK_QUAL_VOLATILE);

    TB_Node* reg = 0;
    switch (v.value_type) {
        case RVALUE: {
            reg = v.reg;
            break;
        }
        case RVALUE_PHI: {
            TB_Label merger = tb_basic_block_create(func);

            fallthrough_label(func, v.phi.if_true);
            TB_Node* one = tb_inst_bool(func, true);
            tb_inst_goto(func, merger);

            tb_inst_set_label(func, v.phi.if_false);
            TB_Node* zero = tb_inst_bool(func, false);
            tb_inst_goto(func, merger);

            tb_inst_set_label(func, merger);
            reg = tb_inst_phi2(func, v.phi.if_true, one, v.phi.if_false, zero);
            break;
        }
        case LVALUE: {
            // Implicit array to pointer
            if (src->kind == KIND_ARRAY) {
                // just pass the address don't load
                src = cuik_canonical_type(e->cast_type);
                reg = v.reg;
            } else {
                reg = tb_inst_load(func, ctype_to_tbtype(src), v.reg, src->align, is_volatile);
            }
            break;
        }
        case LVALUE_BITS: {
            uint64_t mask = (UINT64_MAX >> (64ull - v.bits.width));
            TB_DataType dt = ctype_to_tbtype(src);

            reg = tb_inst_load(func, dt, v.reg, src->align, is_volatile);
            if (v.bits.width != (src->size * 8)) {
                reg = tb_inst_and(func, reg, tb_inst_uint(func, dt, mask));
            }

            if (v.bits.offset) {
                reg = tb_inst_shr(func, reg, tb_inst_uint(func, dt, v.bits.offset));
            }
            break;
        }
        case LVALUE_SYMBOL: {
            reg = tb_inst_get_symbol_address(func, v.sym);

            // Implicit array to pointer
            if (src->kind == KIND_FUNC) {
                // do nothing
            } else if (src->kind == KIND_ARRAY) {
                // just pass the address don't load
                src = cuik_canonical_type(e->cast_type);
            } else {
                reg = tb_inst_load(func, ctype_to_tbtype(src), reg, src->align, is_volatile);
            }
            break;
        }
        default:
        abort();
    }

    const Cuik_Type* dst = cuik_canonical_type(e->cast_type);
    if (dst->kind == KIND_VOID) {
        return 0;
    }

    return src != dst ? cast_reg(func, reg, src, dst) : reg;
}

TB_Node* irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, Expr* e) {
    return cvt2rval(tu, func, irgen_expr(tu, func, e), e);
}

TB_Node* irgen_as_lvalue(TranslationUnit* tu, TB_Function* func, Expr* e) {
    IRVal v = irgen_expr(tu, func, e);

    if (v.value_type == LVALUE) {
        return v.reg;
    } else if (v.value_type == LVALUE_SYMBOL) {
        return tb_inst_get_symbol_address(func, v.sym);
    } else if (v.value_type == RVALUE) {
        Cuik_Type* t = cuik_canonical_type(e->cast_type);
        bool is_volatile = CUIK_QUAL_TYPE_HAS(e->cast_type, CUIK_QUAL_VOLATILE);

        // spawn a lil temporary
        TB_DataType dt = v.reg->dt;

        TB_Node* temporary = tb_inst_local(func, t->size, t->align);
        tb_inst_store(func, dt, temporary, v.reg, t->align, is_volatile);
        return temporary;
    } else {
        abort();
    }
}

static TB_Node* inc_or_dec(TranslationUnit* tu, TB_Function* func, IRVal address, Expr* e, Cuik_Type* type, bool postfix, bool is_inc, bool is_volatile) {
    TB_DataType dt = ctype_to_tbtype(type);
    bool is_atomic = false; /* type->is_atomic; */

    TB_Node* loaded = TB_NULL_REG;
    if (is_atomic) {
        TB_Node* stride;
        if (type->kind == KIND_PTR) {
            // pointer arithmatic
            stride = tb_inst_uint(func, TB_TYPE_PTR, cuik_canonical_type(type->ptr_to)->size);
        } else {
            stride = tb_inst_uint(func, ctype_to_tbtype(type), 1);
        }

        loaded = is_inc
            ? tb_inst_atomic_add(func, address.reg, stride, TB_MEM_ORDER_SEQ_CST)
            : tb_inst_atomic_sub(func, address.reg, stride, TB_MEM_ORDER_SEQ_CST);

        // for pre-increment atomics we can just stop here since we've done the arithmatic.
        //
        // for post-increment we need to redo the arithmatic on the loaded value (it's
        // already been done to the value in memory so we don't writeback)
        if (!postfix) {
            return loaded;
        }
    } else {
        loaded = cvt2rval(tu, func, address, e);
    }

    TB_Node* operation;
    if (type->kind == KIND_PTR) {
        // pointer arithmatic
        int32_t stride = cuik_canonical_type(type->ptr_to)->size;

        operation = is_inc
            ? tb_inst_member_access(func, loaded,  stride)
            : tb_inst_member_access(func, loaded, -stride);
    } else {
        TB_Node* one = tb_inst_uint(func, dt, 1);
        TB_ArithmaticBehavior ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW;

        operation = is_inc
            ? tb_inst_add(func, loaded, one, ab)
            : tb_inst_sub(func, loaded, one, ab);
    }

    if (!is_atomic) {
        assert(address.value_type == LVALUE);
        tb_inst_store(func, dt, address.reg, operation, type->align, is_volatile);
    }

    return postfix ? loaded : operation;
}

int count_max_tb_init_objects(InitNode* root_node) {
    int sum = root_node->kids_count;
    for (InitNode* k = root_node->kid; k != NULL; k = k->next) {
        sum += count_max_tb_init_objects(k);
        if (k->expr && k->expr->op == EXPR_ADDR) sum += 1;
    }

    return sum;
}

static void gen_global_initializer(TranslationUnit* tu, TB_Global* g, Cuik_Type* type, Expr* initial, size_t offset);

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

static void eval_local_initializer(TranslationUnit* tu, TB_Function* func, TB_Node* addr, InitNode* n) {
    if (n->kid != NULL) {
        for (InitNode* k = n->kid; k != NULL; k = k->next) {
            eval_local_initializer(tu, func, addr, k);
        }
    } else {
        Cuik_Type* child_type = cuik_canonical_type(n->type);
        int offset = n->offset;

        TB_Node* val = irgen_as_rvalue(tu, func, n->expr);
        TB_DataType dt = val->dt;

        Cuik_Type* type = cuik_canonical_type(n->expr->type);
        if (n->mode == INIT_ARRAY && n->count > 1) {
            size_t size = child_type->size;
            size_t count = n->count;

            for (size_t i = 0; i < count; i++) {
                TB_Node* addr_offset = tb_inst_member_access(func, addr, n->offset + (i * size));
                tb_inst_store(func, dt, addr_offset, val, type->align, false);
            }
        } else {
            TB_Node* addr_offset = tb_inst_member_access(func, addr, n->offset);
            tb_inst_store(func, dt, addr_offset, val, type->align, false);
        }
    }
}

static void gen_local_initializer(TranslationUnit* tu, TB_Function* func, TB_Node* addr, Cuik_Type* type, InitNode* root_node) {
    TB_Node* size_reg = tb_inst_uint(func, TB_TYPE_I64, type->size);
    TB_Node* val_reg = tb_inst_uint(func, TB_TYPE_I8, 0);
    tb_inst_memset(func, addr, val_reg, size_reg, type->align, false);

    eval_local_initializer(tu, func, addr, root_node);
}

TB_ModuleSection* get_variable_storage(TB_Module* m, const Attribs* attrs, bool is_const) {
    if (attrs->is_tls) {
        return tb_module_get_tls(m);
    } else if (is_const) {
        return tb_module_get_rdata(m);
    } else {
        return tb_module_get_data(m);
    }
}

static void gen_global_initializer(TranslationUnit* tu, TB_Global* g, Cuik_Type* type, Expr* initial, size_t offset) {
    assert(type != NULL);

    if (initial != NULL) {
        if (initial->op == EXPR_STR || initial->op == EXPR_WSTR) {
            size_t len = initial->str.end - initial->str.start;

            if (type->kind == KIND_PTR) {
                TB_Global* dummy = tb_global_create(tu->ir_mod, NULL, NULL, TB_LINKAGE_PRIVATE);
                tb_global_set_storage(tu->ir_mod, tb_module_get_rdata(tu->ir_mod), dummy, 0, len, 1);

                char* dst = tb_global_add_region(tu->ir_mod, dummy, 0, len);
                memcpy(dst, initial->str.start, len);

                tb_global_add_symbol_reloc(tu->ir_mod, g, offset, (TB_Symbol*) dummy);
            } else {
                char* dst = tb_global_add_region(tu->ir_mod, g, offset, type->size);
                memcpy(dst, initial->str.start, len);
            }
        } else if (initial->op == EXPR_INT || initial->op == EXPR_FLOAT32 || initial->op == EXPR_FLOAT64) {
            void* region = tb_global_add_region(tu->ir_mod, g, offset, type->size);

            Cuik_Type* expr_type = cuik_canonical_type(initial->type);
            Cuik_Type* cast_type = cuik_canonical_type(initial->cast_type);
            if (initial->op == EXPR_INT && (cast_type->kind == KIND_FLOAT || cast_type->kind == KIND_DOUBLE)) {
                initial->op = cast_type->kind == KIND_DOUBLE ? EXPR_FLOAT64 : EXPR_FLOAT32;

                if (cuik_type_is_signed(expr_type)) {
                    int64_t old = initial->int_num.num;
                    initial->float_num = (double) old;
                } else {
                    uint64_t old = initial->int_num.num;
                    initial->float_num = (double) old;
                }
            }

            if (initial->op == EXPR_INT) {
                uint64_t value = initial->int_num.num;
                memcpy(region, &value, type->size);
            } else if (initial->op == EXPR_FLOAT32) {
                float value = initial->float_num;

                _Static_assert(sizeof(float) == sizeof(uint32_t), "Wtf is your float?");
                memcpy(region, &value, sizeof(value));
            } else if (initial->op == EXPR_FLOAT64) {
                double value = initial->float_num;

                _Static_assert(sizeof(double) == sizeof(uint64_t), "Wtf is your double?");
                memcpy(region, &value, sizeof(value));
            }
        } else if (initial->op == EXPR_INITIALIZER) {
            Cuik_Type* t = cuik_canonical_type(initial->init.type);
            if (t->kind == KIND_VOID) {
                t = type;
            }

            // Initialize all const expressions
            eval_global_initializer(tu, g, initial->init.root, offset);
        } else if (initial->op == EXPR_ADDR) {
            void* region = tb_global_add_region(tu->ir_mod, g, offset, type->size);
            uint64_t offset = 0;

            // &some_global[a][b][c]
            Expr* base = initial->unary_op.src;
            while (base->op == EXPR_SUBSCRIPT) {
                uint64_t stride = cuik_canonical_type(base->type)->size;
                if (stride == 0) stride = 1;

                Expr* index_expr = cuik__optimize_ast(NULL, base->subscript.index);
                assert(index_expr->op == EXPR_INT && "could not resolve as constant initializer");

                uint64_t index = index_expr->int_num.num;
                offset += (index * stride);
                base = base->subscript.base;
            }
            assert(base->op == EXPR_SYMBOL && "could not resolve as constant initializer");

            // TODO(NeGate): Assumes we're on a 64bit target...
            memcpy(region, &offset, sizeof(uint64_t));

            Stmt* stmt = base->symbol;
            assert(stmt->op == STMT_GLOBAL_DECL && "could not resolve as constant initializer");
            tb_global_add_symbol_reloc(tu->ir_mod, g, offset, stmt->backing.s);
        } else {
            fprintf(stderr, "internal compiler error: cannot compile global initializer as constant (%s).\n", tu->filepath);
            abort();
        }
    }
}

static void insert_label(TB_Function* func) {
    TB_Label last = tb_inst_get_label(func);
    if (tb_basic_block_is_complete(func, last)) {
        tb_inst_set_label(func, tb_basic_block_create(func));
    }
}

IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, Expr* e) {
    switch (e->op) {
        case EXPR_CHAR:
        case EXPR_WCHAR: {
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_uint(func, cuik_canonical_type(e->type)->kind == KIND_SHORT ? TB_TYPE_I16 : TB_TYPE_I32, e->char_lit)
            };
        }
        case EXPR_INT: {
            Cuik_Type* t = cuik_canonical_type(e->type);
            TB_DataType dt = ctype_to_tbtype(t);

            if (t->kind == KIND_FLOAT) {
                return (IRVal){
                    .value_type = RVALUE,
                    .reg = tb_inst_float32(func, e->int_num.num),
                };
            } else if (t->kind == KIND_DOUBLE) {
                return (IRVal){
                    .value_type = RVALUE,
                    .reg = tb_inst_float64(func, e->int_num.num),
                };
            } else if (t->is_unsigned) {
                return (IRVal){
                    .value_type = RVALUE,
                    .reg = tb_inst_uint(func, dt, e->int_num.num),
                };
            } else {
                // TODO(NeGate): maybe this should use tb_inst_sint?
                return (IRVal){
                    .value_type = RVALUE,
                    .reg = tb_inst_uint(func, dt, e->int_num.num),
                };
            }
        }
        case EXPR_ENUM: {
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_sint(func, TB_TYPE_I32, e->enum_val.num->value),
            };
        }
        case EXPR_FLOAT32:
        case EXPR_FLOAT64: {
            bool is_float32 = cuik_canonical_type(e->cast_type)->kind == KIND_FLOAT;

            return (IRVal){
                .value_type = RVALUE,
                .reg = is_float32
                    ? tb_inst_float32(func, e->float_num)
                    : tb_inst_float64(func, e->float_num),
            };
        }
        case EXPR_STR:
        case EXPR_WSTR: {
            // The string is preprocessed to be a flat and nice byte buffer by the semantics pass
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_string(func, e->str.end - e->str.start, (char*)e->str.start),
            };
        }
        case EXPR_INITIALIZER: {
            Cuik_Type* type = cuik_canonical_type(e->init.type);
            TB_Node* addr = tb_inst_local(func, type->size, type->align);

            gen_local_initializer(tu, func, addr, type, e->init.root);

            return (IRVal){
                .value_type = LVALUE,
                .reg = addr
            };
        }
        case EXPR_FUNCTION: {
            assert(e->func.src->op == STMT_FUNC_DECL);

            return (IRVal){
                .value_type = LVALUE_SYMBOL,
                .sym = e->func.src->backing.s
            };
        }
        case EXPR_VA_ARG: {
            IRVal src = irgen_expr(tu, func, e->va_arg_.src);
            assert(src.value_type == LVALUE);

            // post-increment
            // NOTE: this assumes pointer size is 64bit
            TB_Node* pre = tb_inst_load(func, TB_TYPE_PTR, src.reg, 8, false);
            TB_Node* post = tb_inst_member_access(func, pre, 8);
            tb_inst_store(func, TB_TYPE_PTR, src.reg, post, 8, false);

            return (IRVal){
                .value_type = LVALUE,
                .reg = pre
            };
        }
        case EXPR_SYMBOL: {
            Stmt* stmt = e->symbol;
            assert(stmt->op == STMT_DECL || stmt->op == STMT_LABEL || stmt->op == STMT_GLOBAL_DECL || stmt->op == STMT_FUNC_DECL);

            Cuik_Type* type = cuik_canonical_type(stmt->decl.type);
            if (stmt->op == STMT_LABEL) {
                if (stmt->backing.l == 0) {
                    stmt->backing.l = tb_basic_block_create(func);
                }

                return (IRVal){
                    .value_type = LVALUE_LABEL,
                    .label = stmt->backing.l,
                };
            } else if (stmt->op == STMT_FUNC_DECL) {
                return (IRVal){
                    .value_type = LVALUE_SYMBOL,
                    .sym = stmt->backing.s,
                };
            } else if (type->kind == KIND_FUNC || stmt->op == STMT_GLOBAL_DECL || (stmt->op == STMT_DECL && stmt->decl.attrs.is_static)) {
                if (stmt->backing.s == NULL) {
                    // check if it's defined by another TU
                    // functions are external by default
                    const char* name = (const char*) stmt->decl.name;

                    if (tu->parent != NULL) {
                        // It's either a proper external or links to
                        // a file within the compilation unit, we don't
                        // know yet
                        CompilationUnit* restrict cu = tu->parent;
                        cuik_lock_compilation_unit(cu);

                        ptrdiff_t search = nl_strmap_get_cstr(cu->export_table, name);
                        if (search >= 0) {
                            // Figure out what the symbol is and link it together
                            Stmt* real_symbol = cu->export_table[search];

                            if (real_symbol->op == STMT_FUNC_DECL) {
                                stmt->backing.f = real_symbol->backing.f;
                            } else if (real_symbol->op == STMT_GLOBAL_DECL) {
                                stmt->backing.g = real_symbol->backing.g;
                            } else {
                                abort();
                            }
                        } else {
                            // Always creates a real external in this case
                            stmt->backing.e = tb_extern_create(tu->ir_mod, name, TB_EXTERNAL_SO_LOCAL);
                        }

                        // NOTE(NeGate): we might wanna move this mutex unlock earlier
                        // it doesn't seem like we might need it honestly...
                        cuik_unlock_compilation_unit(cu);
                    } else {
                        mtx_lock(&tu->arena_mutex);
                        stmt->backing.e = tb_extern_create(tu->ir_mod, name, TB_EXTERNAL_SO_LOCAL);
                        mtx_unlock(&tu->arena_mutex);
                    }
                }

                assert(stmt->backing.s != NULL);
                return (IRVal){
                    .value_type = LVALUE_SYMBOL,
                    .sym = stmt->backing.s,
                };
            } else {
                return (IRVal){
                    .value_type = LVALUE,
                    .reg = stmt->backing.r,
                };
            }
        }
        case EXPR_PARAM: {
            int param_num = e->param_num;
            TB_Node* reg = parameter_map[param_num];

            Cuik_Type* arg_type = cuik_canonical_type(function_type->func.param_list[param_num].type);
            assert(arg_type != NULL);

            reg = tu->target->get_parameter(tu, func, arg_type, reg);
            return (IRVal){
                .value_type = LVALUE,
                .reg = reg
            };
        }
        case EXPR_GENERIC: {
            assert(e->generic_.case_count == 0);
            return irgen_expr(tu, func, e->generic_.controlling_expr);
        }
        case EXPR_ADDR: {
            IRVal src = irgen_expr(tu, func, e->unary_op.src);
            if (src.value_type == LVALUE) {
                src.value_type = RVALUE;
                return src;
            } else if (src.value_type == LVALUE_SYMBOL) {
                src.value_type = RVALUE;
                src.reg = tb_inst_get_symbol_address(func, src.sym);
                return src;
            } else {
                abort();
            }
        }
        case EXPR_LOGICAL_NOT: {
            TB_Node* reg = irgen_as_rvalue(tu, func, e->unary_op.src);
            TB_DataType dt = reg->dt;

            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_cmp_eq(func, reg, tb_inst_uint(func, dt, 0)),
            };
        }
        case EXPR_NOT: {
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_not(func, irgen_as_rvalue(tu, func, e->unary_op.src)),
            };
        }
        case EXPR_NEGATE: {
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_neg(func, irgen_as_rvalue(tu, func, e->unary_op.src)),
            };
        }
        case EXPR_CAST: {
            TB_Node* src = irgen_as_rvalue(tu, func, e->cast.src);
            Cuik_Type* t = cuik_canonical_type(e->cast.type);

            // stuff like ((void) x)
            if (t->kind == KIND_VOID) {
                return (IRVal){.value_type = RVALUE, .reg = 0};
            }

            return (IRVal){
                .value_type = RVALUE,
                .reg = src
            };
        }
        case EXPR_DEREF: {
            TB_Node* reg = irgen_as_rvalue(tu, func, e->unary_op.src);

            if (cuik_canonical_type(e->type)->kind == KIND_FUNC) {
                return (IRVal){
                    .value_type = RVALUE,
                    .reg = reg,
                };
            }

            return (IRVal){
                .value_type = LVALUE,
                .reg = reg,
            };
        }
        case EXPR_CALL: {
            Expr** args = e->call.param_start;
            int arg_count = e->call.param_count;

            // Try to see if it's an intrinsic
            if (e->call.target->op == EXPR_SYMBOL) {
                Stmt* sym = e->call.target->symbol;

                if (sym->op == STMT_DECL) {
                    const char* name = (const char*)sym->decl.name;

                    // all builtins start with an underscore
                    if (*name == '_') {
                        ptrdiff_t search = nl_strmap_get_cstr(tu->target->builtin_func_map, name);
                        if (search >= 0) {
                            TB_Node* val = tu->target->compile_builtin(tu, func, name, arg_count, args);

                            return (IRVal){
                                .value_type = RVALUE,
                                .reg = val,
                            };
                        }
                    }
                }
            } else if (e->call.target->op == EXPR_BUILTIN_SYMBOL) {
                const char* name = (const char*) e->call.target->builtin_sym.name;
                TB_Node* val = tu->target->compile_builtin(tu, func, name, arg_count, args);

                return (IRVal){
                    .value_type = RVALUE,
                    .reg = val,
                };
            }

            // Resolve ABI arg count
            bool is_aggregate_return = !tu->target->pass_return_via_reg(tu, cuik_canonical_type(e->type));
            size_t real_arg_count = is_aggregate_return ? 1 : 0;

            for (size_t i = 0; i < arg_count; i++) {
                real_arg_count += tu->target->deduce_parameter_usage(tu, args[i]->type);
            }

            TB_Node** ir_args = tls_push(real_arg_count * sizeof(TB_Node*));
            Cuik_Type* return_type = cuik_canonical_type(e->type);
            if (is_aggregate_return) {
                ir_args[0] = tb_inst_local(func, return_type->size, return_type->align);
            }

            // point at which it stops being know which parameter types we're
            // mapping to, if it's arg_count then there's really none
            size_t varargs_cutoff = arg_count;
            Cuik_Type* func_type = cuik_canonical_type(e->call.target->type);
            if (func_type->kind == KIND_PTR) {
                func_type = cuik_canonical_type(func_type->ptr_to);
            }

            if (func_type->func.has_varargs) {
                varargs_cutoff = func_type->func.param_count;
            }

            #if 0
            size_t ir_arg_count = real_arg_count;
            for (size_t i = arg_count; i--;) {
                ir_arg_count -= tu->target->deduce_parameter_usage(tu, args[i]->type);
                tu->target->pass_parameter(tu, func, args[i], i >= varargs_cutoff, &ir_args[ir_arg_count]);
            }
            assert(ir_arg_count == (is_aggregate_return ? 1 : 0));
            #else
            size_t ir_arg_count = is_aggregate_return ? 1 : 0;
            for (size_t i = 0; i < arg_count; i++) {
                ir_arg_count += tu->target->pass_parameter(tu, func, args[i], i >= varargs_cutoff, &ir_args[ir_arg_count]);
            }
            assert(ir_arg_count == real_arg_count);
            #endif

            // Resolve call target
            //
            // NOTE(NeGate): Could have been resized in the parameter's irgen_expr
            // so we reload the pointer.
            IRVal func_ptr = irgen_expr(tu, func, e->call.target);

            TB_DataType dt = ctype_to_tbtype(return_type);
            if (is_aggregate_return) dt = TB_TYPE_VOID;

            TB_Node* target_reg = cvt2rval(tu, func, func_ptr, e->call.target);
            TB_Node* r = tb_inst_call(func, dt, target_reg, real_arg_count, ir_args);

            if (is_aggregate_return) {
                TB_Node* result = ir_args[0];
                tls_restore(ir_args);

                return (IRVal){
                    .value_type = LVALUE,
                    .reg = result,
                };
            } else {
                tls_restore(ir_args);

                if (return_type->kind == KIND_STRUCT || return_type->kind == KIND_UNION) {
                    TB_DataType dt = r->dt;

                    // spawn a lil temporary
                    TB_Node* addr = tb_inst_local(func, return_type->size, return_type->align);
                    tb_inst_store(func, dt, addr, r, return_type->align, false);

                    return (IRVal){
                        .value_type = LVALUE,
                        .reg = addr,
                    };
                }

                return (IRVal){
                    .value_type = RVALUE,
                    .reg = r,
                };
            }
        }
        case EXPR_SUBSCRIPT: {
            TB_Node* base = irgen_as_rvalue(tu, func, e->subscript.base);
            TB_Node* index = irgen_as_rvalue(tu, func, e->subscript.index);

            int stride = cuik_canonical_type(e->type)->size;
            if (stride == 0) stride = 1;

            return (IRVal){
                .value_type = LVALUE,
                .reg = tb_inst_array_access(func, base, index, stride),
            };
        }
        case EXPR_DOT_R: {
            TB_Node* src = irgen_as_lvalue(tu, func, e->dot_arrow.base);

            Member* member = e->dot_arrow.member;
            assert(member != NULL);

            if (member->is_bitfield) {
                return (IRVal){
                    .value_type = LVALUE_BITS,
                    .bits = {
                        .reg = tb_inst_member_access(func, src, e->dot_arrow.offset),
                        .offset = member->bit_offset,
                        .width = member->bit_width,
                    },
                };
            } else {
                return (IRVal){
                    .value_type = LVALUE,
                    .reg = tb_inst_member_access(func, src, e->dot_arrow.offset),
                };
            }
        }
        case EXPR_ARROW_R: {
            TB_Node* src = irgen_as_rvalue(tu, func, e->dot_arrow.base);

            Member* member = e->dot_arrow.member;
            assert(member != NULL);

            if (member->is_bitfield) {
                return (IRVal){
                    .value_type = LVALUE_BITS,
                    .bits = {
                        .reg = tb_inst_member_access(func, src, e->dot_arrow.offset),
                        .offset = member->bit_offset,
                        .width = member->bit_width,
                    },
                };
            } else {
                return (IRVal){
                    .value_type = LVALUE,
                    .reg = tb_inst_member_access(func, src, e->dot_arrow.offset),
                };
            }
        }
        case EXPR_PRE_INC:
        case EXPR_PRE_DEC: {
            bool is_inc = (e->op == EXPR_PRE_INC);
            bool is_volatile = CUIK_QUAL_TYPE_HAS(e->type, CUIK_QUAL_VOLATILE);

            IRVal src = {
                .value_type = LVALUE,
                .reg = irgen_as_lvalue(tu, func, e->unary_op.src)
            };

            return (IRVal){
                .value_type = RVALUE,
                .reg = inc_or_dec(tu, func, src, e->unary_op.src, cuik_canonical_type(e->type), false, is_inc, is_volatile),
            };
        }
        case EXPR_POST_INC:
        case EXPR_POST_DEC: {
            bool is_inc = (e->op == EXPR_POST_INC);
            bool is_volatile = CUIK_QUAL_TYPE_HAS(e->type, CUIK_QUAL_VOLATILE);

            IRVal src = {
                .value_type = LVALUE,
                .reg = irgen_as_lvalue(tu, func, e->unary_op.src)
            };

            return (IRVal){
                .value_type = RVALUE,
                .reg = inc_or_dec(tu, func, src, e->unary_op.src, cuik_canonical_type(e->type), true, is_inc, is_volatile),
            };
        }
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR: {
            // a && b
            //
            //          if (a) { goto try_rhs } else { goto false }
            // try_rhs: if (b) { goto true    } else { goto false }
            //
            //
            // a || b
            //
            //          if (a) { goto true    } else { goto try_rhs }
            // try_rhs: if (b) { goto true    } else { goto false }
            bool is_and = (e->op == EXPR_LOGICAL_AND);
            TB_Label try_rhs_lbl = tb_basic_block_create(func);

            // Eval first operand
            IRVal a = irgen_expr(tu, func, e->bin_op.left);

            TB_Label true_lbl, false_lbl;
            if (a.value_type == RVALUE_PHI) {
                // chain with previous phi.
                // OR  chains on false,
                // AND chains on true
                if (is_and) {
                    tb_inst_set_label(func, a.phi.if_true);
                    tb_inst_goto(func, try_rhs_lbl);

                    true_lbl = tb_basic_block_create(func);
                    false_lbl = a.phi.if_false;
                } else {
                    tb_inst_set_label(func, a.phi.if_false);
                    tb_inst_goto(func, try_rhs_lbl);

                    true_lbl = a.phi.if_true;
                    false_lbl = tb_basic_block_create(func);
                }
            } else {
                true_lbl = tb_basic_block_create(func);
                false_lbl = tb_basic_block_create(func);

                TB_Node* a_reg = cvt2rval(tu, func, a, e->bin_op.left);
                if (is_and) {
                    tb_inst_if(func, a_reg, try_rhs_lbl, false_lbl);
                } else {
                    tb_inst_if(func, a_reg, true_lbl, try_rhs_lbl);
                }
            }

            // Eval second operand
            tb_inst_set_label(func, try_rhs_lbl);

            TB_Node* b = irgen_as_rvalue(tu, func, e->bin_op.right);
            tb_inst_if(func, b, true_lbl, false_lbl);

            // Just in case
            //insert_label(func);

            return (IRVal){
                .value_type = RVALUE_PHI,
                .phi = { true_lbl, false_lbl },
            };
        }
        case EXPR_COMMA: {
            irgen_expr(tu, func, e->bin_op.left);
            return irgen_expr(tu, func, e->bin_op.right);
        }
        case EXPR_PTRADD:
        case EXPR_PTRSUB: {
            TB_Node* l = irgen_as_rvalue(tu, func, e->bin_op.left);
            TB_Node* r = irgen_as_rvalue(tu, func, e->bin_op.right);

            Cuik_Type* type = cuik_canonical_type(e->type);

            // pointer arithmatic
            int dir = e->op == EXPR_PTRADD ? 1 : -1;
            int stride = cuik_canonical_type(type->ptr_to)->size;

            assert(stride);
            return (IRVal){
                .value_type = RVALUE,
                .reg = tb_inst_array_access(func, l, r, dir * stride),
            };
        }
        case EXPR_PTRDIFF: {
            TB_Node* l = irgen_as_rvalue(tu, func, e->bin_op.left);
            TB_Node* r = irgen_as_rvalue(tu, func, e->bin_op.right);

            Cuik_Type* type = cuik_canonical_type(e->bin_op.left->cast_type);
            int stride = cuik_canonical_type(type->ptr_to)->size;

            l = tb_inst_ptr2int(func, l, TB_TYPE_I64);
            r = tb_inst_ptr2int(func, r, TB_TYPE_I64);

            TB_Node* diff = tb_inst_sub(func, l, r, TB_ARITHMATIC_NSW | TB_ARITHMATIC_NUW);
            TB_Node* diff_in_elems = tb_inst_div(func, diff, tb_inst_sint(func, diff->dt, stride), true);

            return (IRVal){
                .value_type = RVALUE,
                .reg = diff_in_elems,
            };
        }
        case EXPR_PLUS:
        case EXPR_MINUS:
        case EXPR_TIMES:
        case EXPR_SLASH:
        case EXPR_PERCENT:
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_XOR:
        case EXPR_SHL:
        case EXPR_SHR: {
            TB_Node* l = irgen_as_rvalue(tu, func, e->bin_op.left);
            TB_Node* r = irgen_as_rvalue(tu, func, e->bin_op.right);
            Cuik_Type* restrict type = cuik_canonical_type(e->type);

            TB_Node* data;
            if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                switch (e->op) {
                    case EXPR_PLUS:  data = tb_inst_fadd(func, l, r); break;
                    case EXPR_MINUS: data = tb_inst_fsub(func, l, r); break;
                    case EXPR_TIMES: data = tb_inst_fmul(func, l, r); break;
                    case EXPR_SLASH: data = tb_inst_fdiv(func, l, r); break;
                    default: TODO();
                }
            } else {
                TB_ArithmaticBehavior ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW;

                switch (e->op) {
                    case EXPR_PLUS:    data = tb_inst_add(func, l, r, ab);                 break;
                    case EXPR_MINUS:   data = tb_inst_sub(func, l, r, ab);                 break;
                    case EXPR_TIMES:   data = tb_inst_mul(func, l, r, ab);                 break;
                    case EXPR_SLASH:   data = tb_inst_div(func, l, r, !type->is_unsigned); break;
                    case EXPR_PERCENT: data = tb_inst_mod(func, l, r, !type->is_unsigned); break;
                    case EXPR_AND:     data = tb_inst_and(func, l, r);                     break;
                    case EXPR_OR:      data = tb_inst_or(func, l, r);                      break;
                    case EXPR_XOR:     data = tb_inst_xor(func, l, r);                     break;
                    case EXPR_SHL:     data = tb_inst_shl(func, l, r, ab);                 break;
                    case EXPR_SHR:     data = type->is_unsigned ? tb_inst_shr(func, l, r) : tb_inst_sar(func, l, r); break;
                    default: TODO();
                }

                if (type->kind == KIND_BOOL) {
                    // convert into proper bool
                    data = tb_inst_cmp_ne(func, data, tb_inst_uint(func, TB_TYPE_BOOL, 0));
                }
            }

            return (IRVal){
                .value_type = RVALUE,
                .reg = data,
            };
        }
        case EXPR_CMPEQ:
        case EXPR_CMPNE: {
            TB_Node* l = irgen_as_rvalue(tu, func, e->bin_op.left);
            TB_Node* r = irgen_as_rvalue(tu, func, e->bin_op.right);

            TB_Node* result;
            if (e->op == EXPR_CMPEQ) {
                result = tb_inst_cmp_eq(func, l, r);
            } else {
                result = tb_inst_cmp_ne(func, l, r);
            }

            return (IRVal){
                .value_type = RVALUE,
                .reg = result,
            };
        }
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE: {
            TB_Node* l = irgen_as_rvalue(tu, func, e->bin_op.left);
            TB_Node* r = irgen_as_rvalue(tu, func, e->bin_op.right);

            Cuik_Type* type = cuik_canonical_type(e->bin_op.left->cast_type);
            TB_Node* data;
            if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                switch (e->op) {
                    case EXPR_CMPGT: data = tb_inst_cmp_fgt(func, l, r); break;
                    case EXPR_CMPGE: data = tb_inst_cmp_fge(func, l, r); break;
                    case EXPR_CMPLT: data = tb_inst_cmp_flt(func, l, r); break;
                    case EXPR_CMPLE: data = tb_inst_cmp_fle(func, l, r); break;
                    default: TODO();
                }
            } else if (type->kind == KIND_PTR) {
                switch (e->op) {
                    case EXPR_CMPGT: data = tb_inst_cmp_igt(func, l, r, false); break;
                    case EXPR_CMPGE: data = tb_inst_cmp_ige(func, l, r, false); break;
                    case EXPR_CMPLT: data = tb_inst_cmp_ilt(func, l, r, false); break;
                    case EXPR_CMPLE: data = tb_inst_cmp_ile(func, l, r, false); break;
                    default: TODO();
                }
            } else {
                switch (e->op) {
                    case EXPR_CMPGT: data = tb_inst_cmp_igt(func, l, r, !type->is_unsigned); break;
                    case EXPR_CMPGE: data = tb_inst_cmp_ige(func, l, r, !type->is_unsigned); break;
                    case EXPR_CMPLT: data = tb_inst_cmp_ilt(func, l, r, !type->is_unsigned); break;
                    case EXPR_CMPLE: data = tb_inst_cmp_ile(func, l, r, !type->is_unsigned); break;
                    default: TODO();
                }
            }

            return (IRVal){
                .value_type = RVALUE,
                .reg = data,
            };
        }
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
            Cuik_Type* type = cuik_canonical_type(e->type);
            bool is_volatile = CUIK_QUAL_TYPE_HAS(e->type, CUIK_QUAL_VOLATILE);

            if (CUIK_QUAL_TYPE_HAS(e->type, CUIK_QUAL_ATOMIC)) {
                // Load inputs
                IRVal rhs = irgen_expr(tu, func, e->bin_op.right);
                IRVal lhs = irgen_expr(tu, func, e->bin_op.left);

                // convert into address
                if (lhs.value_type == LVALUE_SYMBOL) {
                    lhs = (IRVal){
                        .value_type = LVALUE,
                        .reg = tb_inst_get_symbol_address(func, lhs.sym),
                    };
                }
                assert(lhs.value_type == LVALUE);

                if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                    // Implement big atomic copy
                    abort();
                } else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                    TB_Node* r = cvt2rval(tu, func, rhs, e->bin_op.right);

                    // float assignment can be done atomic by using the normal
                    // integer stuff
                    if (e->op == EXPR_ASSIGN) {
                        tb_inst_atomic_xchg(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);

                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = r,
                        };
                    } else {
                        // floats don't really have any atomic operations so just
                        // emulate them all using CAS:
                        //
                        // fn atomic_operate(p: *T, a: T): T
                        //   done := false
                        //   while not done
                        //     value = *p # doesn't really need to be atomic
                        //     done = cas(ptr=p, old_val=value, new_val=operate(value, a))
                        //
                        //   return value + a
                        //
                        assert(0 && "TODO");
                    }
                } else {
                    TB_Node* r = cvt2rval(tu, func, rhs, e->bin_op.right);

                    if (e->op == EXPR_ASSIGN) {
                        tb_inst_atomic_xchg(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);

                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = r,
                        };
                    } else if (e->op == EXPR_PLUS_ASSIGN) {
                        TB_Node* op = tb_inst_atomic_add(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
                        op = tb_inst_add(func, op, r, 0);

                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = op,
                        };
                    } else if (e->op == EXPR_MINUS_ASSIGN) {
                        TB_Node* op = tb_inst_atomic_sub(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
                        op = tb_inst_sub(func, op, r, 0);

                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = op,
                        };
                    } else if (e->op == EXPR_AND_ASSIGN) {
                        TB_Node* op = tb_inst_atomic_and(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
                        op = tb_inst_and(func, op, r);

                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = op,
                        };
                    } else {
                        assert(0 && "TODO atomic operation not ready");
                    }
                }
            } else {
                // Load inputs
                IRVal lhs = irgen_expr(tu, func, e->bin_op.left);
                if (lhs.value_type == LVALUE_SYMBOL) {
                    // we want a TB reg not just an extern's id
                    lhs.value_type = LVALUE;
                    lhs.reg = tb_inst_get_symbol_address(func, lhs.sym);
                }

                TB_Node* l = TB_NULL_REG;
                if (e->op != EXPR_ASSIGN) {
                    // don't do this conversion for ASSIGN, since it won't
                    // be needing it
                    l = cvt2rval(tu, func, lhs, e->bin_op.left);
                }

                IRVal rhs = irgen_expr(tu, func, e->bin_op.right);

                // Try pointer arithmatic
                if ((e->op == EXPR_PLUS_ASSIGN || e->op == EXPR_MINUS_ASSIGN) && type->kind == KIND_PTR) {
                    int dir = e->op == EXPR_PLUS_ASSIGN ? 1 : -1;
                    int stride = cuik_canonical_type(type->ptr_to)->size;
                    assert(stride);

                    TB_Node* r = cvt2rval(tu, func, rhs, e->bin_op.right);
                    TB_Node* arith = tb_inst_array_access(func, l, r, dir * stride);

                    assert(lhs.value_type == LVALUE);
                    tb_inst_store(func, TB_TYPE_PTR, lhs.reg, arith, type->align, is_volatile);
                    return lhs;
                }

                TB_DataType dt = ctype_to_tbtype(type);

                TB_Node* data = TB_NULL_REG;
                if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                    if (e->op != EXPR_ASSIGN) abort();

                    TB_Node* size_reg = tb_inst_uint(func, TB_TYPE_I64, type->size);
                    tb_inst_memcpy(func, lhs.reg, rhs.reg, size_reg, type->align, is_volatile);
                    data = rhs.reg;
                } else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
                    TB_Node* r = cvt2rval(tu, func, rhs, e->bin_op.right);

                    switch (e->op) {
                        case EXPR_ASSIGN:       data = r;                        break;
                        case EXPR_PLUS_ASSIGN:  data = tb_inst_fadd(func, l, r); break;
                        case EXPR_MINUS_ASSIGN: data = tb_inst_fsub(func, l, r); break;
                        case EXPR_TIMES_ASSIGN: data = tb_inst_fmul(func, l, r); break;
                        case EXPR_SLASH_ASSIGN: data = tb_inst_fdiv(func, l, r); break;
                        default: assert(0 && "TODO");
                    }

                    assert(lhs.value_type == LVALUE);
                    tb_inst_store(func, dt, lhs.reg, data, type->align, is_volatile);
                } else {
                    TB_Node* r = cvt2rval(tu, func, rhs, e->bin_op.right);
                    TB_ArithmaticBehavior ab = type->is_unsigned ? 0 : TB_ARITHMATIC_NSW;

                    switch (e->op) {
                        case EXPR_ASSIGN:         data = r;                                           break;
                        case EXPR_PLUS_ASSIGN:    data = tb_inst_add(func, l, r, ab);                 break;
                        case EXPR_MINUS_ASSIGN:   data = tb_inst_sub(func, l, r, ab);                 break;
                        case EXPR_TIMES_ASSIGN:   data = tb_inst_mul(func, l, r, ab);                 break;
                        case EXPR_SLASH_ASSIGN:   data = tb_inst_div(func, l, r, !type->is_unsigned); break;
                        case EXPR_PERCENT_ASSIGN: data = tb_inst_mod(func, l, r, !type->is_unsigned); break;
                        case EXPR_AND_ASSIGN:     data = tb_inst_and(func, l, r);                     break;
                        case EXPR_OR_ASSIGN:      data = tb_inst_or(func, l, r);                      break;
                        case EXPR_XOR_ASSIGN:     data = tb_inst_xor(func, l, r);                     break;
                        case EXPR_SHL_ASSIGN:     data = tb_inst_shl(func, l, r, ab);                 break;
                        case EXPR_SHR_ASSIGN:     data = type->is_unsigned ? tb_inst_shr(func, l, r) : tb_inst_sar(func, l, r); break;
                        default: assert(0 && "TODO");
                    }

                    if (lhs.value_type == LVALUE_BITS && lhs.bits.width != (type->size * 8)) {
                        // NOTE(NeGate): the semantics around volatile bitfields are janky at best
                        TB_Node* old_value = tb_inst_load(func, dt, lhs.reg, type->align, is_volatile);

                        // mask out the space for our bitfield member
                        uint64_t clear_mask = ~((UINT64_MAX >> (64ull - lhs.bits.width)) << lhs.bits.offset);
                        old_value = tb_inst_and(func, old_value, tb_inst_uint(func, dt, ~clear_mask));

                        // mask source value and position it correctly
                        uint64_t insert_mask = (UINT64_MAX >> (64ull - lhs.bits.width));
                        data = tb_inst_and(func, data, tb_inst_uint(func, dt, insert_mask));

                        if (lhs.bits.offset) {
                            // nuw & nsw are used since we statically know that the bits.offset won't overflow without some sort of UB
                            data = tb_inst_shl(func, data, tb_inst_uint(func, dt, lhs.bits.offset), TB_ARITHMATIC_NSW | TB_ARITHMATIC_NUW);
                        }

                        // merge
                        data = tb_inst_or(func, old_value, data);
                    } else {
                        assert(lhs.value_type == LVALUE);
                    }

                    tb_inst_store(func, dt, lhs.reg, data, type->align, is_volatile);

                    if (e->op == EXPR_ASSIGN) {
                        assert(data);
                        return (IRVal){
                            .value_type = RVALUE,
                            .reg = data
                        };
                    }

                    return lhs;
                }
            }
            case EXPR_TERNARY: {
                TB_Node* cond = irgen_as_rvalue(tu, func, e->ternary_op.left);

                TB_Label if_true = tb_basic_block_create(func);
                TB_Label if_false = tb_basic_block_create(func);
                TB_Label exit = tb_basic_block_create(func);

                tb_inst_if(func, cond, if_true, if_false);

                TB_Node* true_val;
                {
                    tb_inst_set_label(func, if_true);

                    true_val = irgen_as_rvalue(tu, func, e->ternary_op.middle);
                    if_true = tb_inst_get_label(func);
                    tb_inst_goto(func, exit);
                }

                TB_Node* false_val;
                {
                    tb_inst_set_label(func, if_false);

                    false_val = irgen_as_rvalue(tu, func, e->ternary_op.right);
                    if_false = tb_inst_get_label(func);
                    tb_inst_goto(func, exit);
                }
                tb_inst_set_label(func, exit);

                return (IRVal){
                    .value_type = RVALUE,
                    .reg = tb_inst_phi2(func, if_true, true_val, if_false, false_val),
                };
            }

            default: TODO();
        }
    }
}

static void emit_location(TranslationUnit* tu, TB_Function* func, SourceLoc loc) {
    if (!tu->has_tb_debug_info) {
        return;
    }

    // TODO(NeGate): Fix this up later!!!
    static thread_local TB_FileID last_file_id;
    static thread_local const char* last_filepath;

    ResolvedSourceLoc rloc = cuikpp_find_location(&tu->tokens, loc);
    if (rloc.file->filename[0] != '<') {
        if (rloc.file->filename != last_filepath) {
            last_filepath = rloc.file->filename;
            last_file_id = tb_file_create(tu->ir_mod, rloc.file->filename);
        }

        tb_inst_loc(func, last_file_id, rloc.line);
    }
}

void irgen_stmt(TranslationUnit* tu, TB_Function* func, Stmt* restrict s) {
    if (s == NULL) return;

    insert_label(func);
    emit_location(tu, func, s->loc.start);

    switch (s->op) {
        case STMT_NONE: {
            break;
        }
        case STMT_LABEL: {
            if (s->backing.l == 0) {
                s->backing.l = tb_basic_block_create(func);
            }

            fallthrough_label(func, s->backing.l);
            break;
        }
        case STMT_GOTO: {
            IRVal target = irgen_expr(tu, func, s->goto_.target);

            if (target.value_type == LVALUE_LABEL) {
                tb_inst_goto(func, target.label);
            } else {
                // TODO(NeGate): Handle computed goto case
                abort();
            }
            break;
        }
        case STMT_COMPOUND: {
            Stmt** kids = s->compound.kids;
            size_t count = s->compound.kids_count;

            for (size_t i = 0; i < count; i++) {
                irgen_stmt(tu, func, kids[i]);
            }
            break;
        }
        case STMT_DECL: {
            Attribs attrs = s->decl.attrs;

            Cuik_Type* type = cuik_canonical_type(s->decl.type);
            int kind = type->kind, size = type->size, align = type->align;

            if (attrs.is_static) {
                // Static initialization
                char* name = tls_push(1024);
                int name_len = snprintf(name, 1024, "%s$%s@%d", function_name, s->decl.name, tu->id_gen++);
                if (name_len < 0 || name_len >= 1024) {
                    assert(0 && "temporary global name too long!");
                }

                TB_DebugType* dbg_type = NULL;
                if (tu->has_tb_debug_info) {
                    dbg_type = cuik__as_tb_debug_type(tu->ir_mod, cuik_canonical_type(s->decl.type));
                }

                TB_Global* g = tb_global_create(tu->ir_mod, name, dbg_type, TB_LINKAGE_PRIVATE);
                tls_restore(name);

                TB_ModuleSection* section = get_variable_storage(tu->ir_mod, &attrs, s->decl.type.raw & CUIK_QUAL_CONST);

                int max_tb_objects;
                if (s->decl.initial->op == EXPR_ADDR) {
                    max_tb_objects = 2;
                } else if (s->decl.initial->op == EXPR_INITIALIZER) {
                    max_tb_objects = count_max_tb_init_objects(s->decl.initial->init.root);
                } else {
                    max_tb_objects = 1;
                }

                tb_global_set_storage(tu->ir_mod, section, g, type->size, type->align, max_tb_objects);
                gen_global_initializer(tu, g, type, s->decl.initial, 0);

                if (attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
                    tb_module_set_tls_index(tu->ir_mod, (TB_Symbol*) tb_extern_create(tu->ir_mod, "_tls_index", TB_EXTERNAL_SO_LOCAL));
                }

                s->backing.g = g;
                break;
            }

            if (kind == KIND_FUNC) {
                break;
            }

            TB_Node* addr = tb_inst_local(func, size, align);
            if (tu->has_tb_debug_info && s->decl.name != NULL) {
                tb_function_attrib_variable(func, addr, s->decl.name, cuik__as_tb_debug_type(tu->ir_mod, type));
            }

            if (s->decl.initial) {
                Expr* e = s->decl.initial;

                if (e->op == EXPR_INITIALIZER) {
                    gen_local_initializer(tu, func, addr, type, e->init.root);
                } else {
                    if (kind == KIND_ARRAY && (e->op == EXPR_STR || e->op == EXPR_WSTR)) {
                        IRVal v = irgen_expr(tu, func, s->decl.initial);
                        TB_Node* size_reg = tb_inst_uint(func, TB_TYPE_I64, size);

                        tb_inst_memcpy(func, addr, v.reg, size_reg, align, false);
                    } else if (kind == KIND_STRUCT || kind == KIND_UNION) {
                        IRVal v = irgen_expr(tu, func, s->decl.initial);
                        TB_Node* size_reg = tb_inst_uint(func, TB_TYPE_I64, size);

                        tb_inst_memcpy(func, addr, v.reg, size_reg, align, false);
                    } else {
                        TB_Node* v = irgen_as_rvalue(tu, func, s->decl.initial);

                        tb_inst_store(func, ctype_to_tbtype(type), addr, v, align, false);
                    }
                }
            } else {
                /* uninitialized */
            }

            s->backing.r = addr;
            break;
        }
        case STMT_EXPR: {
            irgen_expr(tu, func, s->expr.expr);
            break;
        }
        case STMT_RETURN: {
            Expr* e = s->return_.expr;

            if (e) {
                Cuik_Type* type = cuik_canonical_type(e->cast_type);
                bool is_aggregate_return = !tu->target->pass_return_via_reg(tu, type);

                if (is_aggregate_return) {
                    IRVal v = irgen_expr(tu, func, e);

                    // returning aggregates just copies into the first parameter
                    // which is agreed to be a caller owned buffer.
                    int size = type->size;
                    int align = type->align;

                    TB_Node* dst_address = tb_inst_load(func, TB_TYPE_PTR, return_value_address, 8, false);
                    TB_Node* size_reg = tb_inst_uint(func, TB_TYPE_I64, size);

                    tb_inst_memcpy(func, dst_address, v.reg, size_reg, align, false);
                    tb_inst_ret(func, TB_NULL_REG);
                } else {
                    IRVal v = irgen_expr(tu, func, e);
                    TB_Node* r = TB_NULL_REG;
                    if (v.value_type == LVALUE) {
                        // Implicit array to pointer
                        if (type->kind == KIND_ARRAY) {
                            r = v.reg;
                        } else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                            assert(type->size <= 8);
                            TB_DataType dt = { { TB_INT, 0, type->size * 8 } };

                            r = tb_inst_load(func, dt, v.reg, type->align, false);
                        }
                    }

                    // if it wasn't set before, resolve it now
                    if (r == TB_NULL_REG) r = cvt2rval(tu, func, v, e);
                    tb_inst_ret(func, r);
                }
            } else {
                tb_inst_ret(func, TB_NULL_REG);
            }
            break;
        }
        case STMT_IF: {
            TB_Node* cond = irgen_as_rvalue(tu, func, s->if_.cond);

            TB_Label if_true = tb_basic_block_create(func);
            TB_Label if_false = tb_basic_block_create(func);

            // Cast to bool
            tb_inst_if(func, cond, if_true, if_false);

            tb_inst_set_label(func, if_true);
            irgen_stmt(tu, func, s->if_.body);

            if (s->if_.next) {
                TB_Label exit = tb_basic_block_create(func);
                if (!tb_basic_block_is_complete(func, tb_inst_get_label(func))) {
                    tb_inst_goto(func, exit);
                }

                tb_inst_set_label(func, if_false);
                irgen_stmt(tu, func, s->if_.next);

                fallthrough_label(func, exit);
            } else {
                fallthrough_label(func, if_false);
            }
            break;
        }
        case STMT_WHILE: {
            TB_Label header = tb_basic_block_create(func);
            TB_Label body = tb_basic_block_create(func);
            TB_Label exit = tb_basic_block_create(func);
            s->backing.l = exit;

            // NOTE(NeGate): this is hacky but as long as it doesn't
            // break we should be good... what am i saying im the
            // developer of TB :p
            // essentially we can store both the header and exit labels
            // implicitly as one if they're next to each other
            assert(header == exit - 2);
            fallthrough_label(func, header);

            TB_Node* cond = irgen_as_rvalue(tu, func, s->while_.cond);
            tb_inst_if(func, cond, body, exit);

            tb_inst_set_label(func, body);
            if (s->while_.body) {
                emit_location(tu, func, s->while_.body->loc.start);
                irgen_stmt(tu, func, s->while_.body);
            }

            fallthrough_label(func, header);
            // tb_inst_goto(func, header);
            tb_inst_set_label(func, exit);
            break;
        }
        case STMT_DO_WHILE: {
            TB_Label body = tb_basic_block_create(func);
            TB_Label exit = tb_basic_block_create(func);

            // NOTE(NeGate): this is hacky but as long as it doesn't
            // break we should be good... what am i saying im the
            // developer of TB :p
            assert(body == exit - 1);
            s->backing.l = exit;

            fallthrough_label(func, body);
            if (s->do_while.body) {
                emit_location(tu, func, s->do_while.body->loc.start);
                irgen_stmt(tu, func, s->do_while.body);
            }

            insert_label(func);

            TB_Node* cond = irgen_as_rvalue(tu, func, s->do_while.cond);
            tb_inst_if(func, cond, body, exit);
            tb_inst_set_label(func, exit);
            break;
        }
        case STMT_FOR: {
            TB_Label header = tb_basic_block_create(func);
            TB_Label body = tb_basic_block_create(func);
            TB_Label next = tb_basic_block_create(func);
            TB_Label exit = tb_basic_block_create(func);
            s->backing.l = exit;

            // NOTE(NeGate): this is hacky but as long as it doesn't
            // break we should be good... what am i saying im the
            // developer of TB :p
            // essentially we can store both the header and exit labels
            // implicitly as one if they're next to each other
            assert(next == exit - 1);

            if (s->for_.first) {
                emit_location(tu, func, s->for_.first->loc.start);
                irgen_stmt(tu, func, s->for_.first);
                tb_inst_goto(func, header);
            }
            fallthrough_label(func, header);

            if (s->for_.cond) {
                TB_Node* cond = irgen_as_rvalue(tu, func, s->for_.cond);
                tb_inst_if(func, cond, body, exit);
            } else {
                tb_inst_goto(func, body);
            }

            tb_inst_set_label(func, body);
            irgen_stmt(tu, func, s->for_.body);

            if (s->for_.next) {
                fallthrough_label(func, next);
                emit_location(tu, func, s->for_.next->loc.start);
                irgen_expr(tu, func, s->for_.next);
            }

            if (!tb_basic_block_is_complete(func, tb_inst_get_label(func))) {
                tb_inst_goto(func, header);
            }
            tb_inst_set_label(func, exit);
            break;
        }
        case STMT_CONTINUE: {
            // this is really hacky but we always store the loop header label one
            // behind the exit label in terms of IDs.
            if (s->continue_.target->op == STMT_WHILE) {
                tb_inst_goto(func, s->continue_.target->backing.l - 2);
            } else {
                tb_inst_goto(func, s->continue_.target->backing.l - 1);
            }
            break;
        }
        case STMT_BREAK: {
            tb_inst_goto(func, s->break_.target->backing.l);
            break;
        }
        case STMT_DEFAULT: {
            assert(s->backing.l);
            fallthrough_label(func, s->backing.l);
            irgen_stmt(tu, func, s->default_.body);
            break;
        }
        case STMT_CASE: {
            assert(s->backing.l);
            while (s->case_.body && s->case_.body->op == STMT_CASE) {
                fallthrough_label(func, s->backing.l);
                s = s->case_.body;
            }

            fallthrough_label(func, s->backing.l);
            irgen_stmt(tu, func, s->case_.body);
            break;
        }
        case STMT_SWITCH: {
            Stmt* head = s->switch_.next;

            size_t entry_count = 0;
            TB_SwitchEntry* entries = tls_save();

            TB_Label default_label = 0;
            while (head) {
                // reserve label
                assert(head->op == STMT_CASE || head->op == STMT_DEFAULT);

                TB_Label label = tb_basic_block_create(func);
                head->backing.l = label;

                if (head->op == STMT_CASE) {
                    assert(head->case_.key < UINT32_MAX);
                    tls_push(sizeof(TB_SwitchEntry));
                    entries[entry_count++] = (TB_SwitchEntry){.key = head->case_.key, .value = label};

                    head = head->case_.next;
                } else if (head->op == STMT_DEFAULT) {
                    assert(default_label == 0);
                    default_label = label;

                    head = head->default_.next;
                } else assert(0);
            }

            TB_Label break_label = tb_basic_block_create(func);
            s->backing.l = break_label;

            // default to fallthrough
            if (!default_label) {
                default_label = break_label;
            }

            TB_Node* key = irgen_as_rvalue(tu, func, s->switch_.condition);

            tb_inst_branch(func, key->dt, key, default_label, entry_count, entries);

            tb_inst_set_label(func, tb_basic_block_create(func));
            irgen_stmt(tu, func, s->switch_.body);

            fallthrough_label(func, break_label);
            break;
        }

        default: TODO();
    }
}

TB_Symbol* cuikcg_top_level(TranslationUnit* restrict tu, TB_Module* m, Stmt* restrict s) {
    if (s->op == STMT_FUNC_DECL) {
        if ((s->decl.attrs.is_static || s->decl.attrs.is_inline) && !s->decl.attrs.is_used) {
            return NULL;
        }

        Cuik_Type* type = cuik_canonical_type(s->decl.type);
        assert(type->kind == KIND_FUNC);

        // Clear temporary storage
        tls_init();
        assert(type);

        TB_Function* func = s->backing.f;

        // Parameters
        size_t param_count = type->func.param_count;

        TB_Node** params = parameter_map = tls_push(param_count * sizeof(TB_Node*));
        Cuik_Type* return_type = cuik_canonical_type(type->func.return_type);

        bool is_aggregate_return = !tu->target->pass_return_via_reg(tu, return_type);
        if (is_aggregate_return) {
            return_value_address = tb_inst_param_addr(func, 0);

            // gimme stack slots
            for (size_t i = 0; i < param_count; i++) {
                params[i] = tb_inst_param_addr(func, 1 + i);
            }
        } else {
            return_value_address = TB_NULL_REG;

            // gimme stack slots
            for (size_t i = 0; i < param_count; i++) {
                params[i] = tb_inst_param_addr(func, i);
            }
        }

        // compile body
        {
            function_type = type;
            function_name = s->decl.name;

            irgen_stmt(tu, func, s->decl.initial_as_stmt);

            function_name = NULL;
            function_type = 0;
        }

        // append return if none exists
        TB_Label last = tb_inst_get_label(func);
        if (!tb_basic_block_is_complete(func, last)) {
            if (cstr_equals(s->decl.name, "main")) {
                tb_inst_ret(func, tb_inst_uint(func, TB_TYPE_I32, 0));
            } else {
                tb_inst_ret(func, TB_NULL_REG);
            }
        }

        //tb_inst_set_scope(func, old_tb_scope);
        return (TB_Symbol*) func;
    } else if (s->flags & STMT_FLAGS_HAS_IR_BACKING) {
        Cuik_Type* type = cuik_canonical_type(s->decl.type);

        TB_ModuleSection* section = get_variable_storage(tu->ir_mod, &s->decl.attrs, s->decl.type.raw & CUIK_QUAL_CONST);
        int max_tb_objects;
        if (s->decl.initial->op == EXPR_ADDR) {
            max_tb_objects = 2;
        } else if (s->decl.initial->op == EXPR_INITIALIZER) {
            max_tb_objects = count_max_tb_init_objects(s->decl.initial->init.root);
        } else {
            max_tb_objects = 1;
        }

        tb_global_set_storage(tu->ir_mod, section, (TB_Global*) s->backing.s, type->size, type->align, max_tb_objects);
        gen_global_initializer(tu, (TB_Global*) s->backing.s, type, s->decl.initial, 0);
        return s->backing.s;
    }

    return NULL;
}

TB_Module* cuik_get_tb_module(TranslationUnit* restrict tu) {
    return tu->ir_mod;
}

typedef struct {
    TB_Module* mod;
    TranslationUnit* tu;

    Stmt** stmts;
    size_t count;

    size_t ordinal_base;
    Futex* remaining;
} IRAllocTask;

static void ir_alloc_task(void* task) {
    CUIK_TIMED_BLOCK("ir_alloc_task") {
        IRAllocTask t = *(IRAllocTask*) task;

        for (size_t i = 0; i < t.count; i++) {
            Stmt* s = t.stmts[i];
            if ((s->flags & STMT_FLAGS_HAS_IR_BACKING) == 0) continue;

            if (s->op == STMT_FUNC_DECL) {
                TB_FunctionPrototype* proto = t.tu->target->create_prototype(t.tu, cuik_canonical_type(s->decl.type));
                TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;

                // TODO(NeGate): Fix this up because it's possibly wrong, essentially
                // inline linkage means all the definitions must match which isn't
                // necessarily the same as static where they all can share a name but
                // are different and internal.
                TB_Function* func;
                const char* name = s->decl.name;
                if (s->decl.attrs.is_inline) {
                    linkage = TB_LINKAGE_PRIVATE;

                    char temp[1024];
                    snprintf(temp, 1024, "_K%d_%s", t.tu->id_gen++, name ? name : "<unnamed>");

                    func = tb_function_create(t.tu->ir_mod, temp, linkage);
                } else {
                    func = tb_function_create(t.tu->ir_mod, name, linkage);
                }

                tb_symbol_set_ordinal((TB_Symbol*) func, t.ordinal_base + i);
                tb_function_set_prototype(func, proto);
                s->backing.f = func;
            } else if (s->decl.attrs.is_used && !s->decl.attrs.is_typedef) {
                Cuik_Type* type = cuik_canonical_type(s->decl.type);
                bool is_external_sym = (type->kind == KIND_FUNC && s->decl.initial_as_stmt == NULL);
                if (s->decl.attrs.is_extern) is_external_sym = true;

                const char* name = s->decl.name;
                if (!is_external_sym) {
                    // if we have a TB module, fill it up with declarations
                    if (s->decl.attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
                        tb_module_set_tls_index(t.tu->ir_mod, (TB_Symbol*) tb_extern_create(t.tu->ir_mod, "_tls_index", TB_EXTERNAL_SO_LOCAL));
                    }

                    TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
                    TB_DebugType* dbg_type = NULL;
                    if (t.tu->has_tb_debug_info) {
                        dbg_type = cuik__as_tb_debug_type(t.tu->ir_mod, cuik_canonical_type(s->decl.type));
                    }

                    s->backing.g = tb_global_create(t.tu->ir_mod, name, dbg_type, linkage);
                    tb_symbol_set_ordinal((TB_Symbol*) s->backing.g, t.ordinal_base + i);
                }
            }
        }

        futex_dec(t.remaining);
    }
}

void cuikcg_allocate_ir(CompilationUnit* restrict cu, Cuik_IThreadpool* restrict thread_pool, TB_Module* m) {
    // we actually fill the remaining count while we dispatch tasks, it's ok for it to hit 0
    // occasionally (very rare realistically).
    enum { BATCH_SIZE = 65536 };

    Futex remaining = 0;
    size_t ordinal = 0;
    CUIK_FOR_EACH_TU(tu, cu) {
        size_t count = dyn_array_length(tu->top_level_stmts);
        remaining += (count + (BATCH_SIZE - 1)) / BATCH_SIZE;

        Stmt** top_level = tu->top_level_stmts;
        tu->ir_mod = m;

        for (size_t i = 0; i < count; i += BATCH_SIZE) {
            size_t end = i + BATCH_SIZE;
            if (end >= count) end = count;

            IRAllocTask t = {
                .mod = m,
                .tu = tu,
                .stmts = &top_level[i],
                .count = end - i,
                .remaining = &remaining,
                .ordinal_base = ordinal,
            };

            ordinal += t.count;
            if (thread_pool) {
                CUIK_CALL(thread_pool, submit, ir_alloc_task, sizeof(t), &t);
            } else {
                ir_alloc_task(&t);
            }
        }
    }

    if (thread_pool) futex_wait_eq(&remaining, 0);
}

#endif /* CUIK_USE_TB */
