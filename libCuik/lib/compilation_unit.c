#include "compilation_unit.h"
#include "targets/targets.h"
#include <futex.h>

enum {
    BATCH_SIZE = 65536
};

void cuik_create_compilation_unit(CompilationUnit* restrict cu) {
    *cu = (CompilationUnit){0};
    cu->lock = cuik_malloc(sizeof(mtx_t));
    mtx_init((mtx_t*) cu->lock, mtx_plain);
}

void cuik_lock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_lock((mtx_t*) cu->lock);
}

void cuik_unlock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_unlock((mtx_t*) cu->lock);
}

void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    cuik_lock_compilation_unit(cu);

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;
    cu->count += 1;

    cuik_unlock_compilation_unit(cu);
}

void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
    // walk all the TUs and free them (if they're not freed already)
    TranslationUnit* tu = cu->head;
    while (tu != NULL) {
        TranslationUnit* next = tu->next;
        cuik_destroy_translation_unit(tu);
        tu = next;
    }

    mtx_destroy((mtx_t*) cu->lock);
    cuik_free(cu->lock);
    *cu = (CompilationUnit){0};
}

size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu) {
    return cu->count;
}

#ifdef CUIK_USE_TB
typedef struct {
    TB_Module* mod;
    TranslationUnit* tu;

    Stmt** stmts;
    size_t count;

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

                    s->backing.g = tb_global_create(t.tu->ir_mod, name, s->decl.attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, dbg_type, linkage);
                }
            }
        }

        futex_dec(t.remaining);
    }
}
#endif

#ifdef CUIK_USE_TB
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu, Cuik_IThreadpool* restrict thread_pool, TB_Module* mod, int debug_info_level)
#else
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu, Cuik_IThreadpool* restrict thread_pool, void* mod, int debug_info_level)
#endif
{
    size_t task_cap = 0;
    CUIK_TIMED_BLOCK("internal link") {
        FOR_EACH_TU(tu, cu) {
            size_t count = dyn_array_length(tu->top_level_stmts);
            task_cap += (count + (BATCH_SIZE - 1)) / BATCH_SIZE;

            tu->ir_mod = mod;
            tu->has_tb_debug_info = debug_info_level;

            for (size_t i = 0; i < count; i++) {
                Stmt* s = tu->top_level_stmts[i];
                const char* name = s->decl.name;

                if (s->op == STMT_FUNC_DECL) {
                    if (!s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
                        nl_strmap_put_cstr(cu->export_table, s->decl.name, s);
                    }

                    if (s->decl.attrs.is_static || s->decl.attrs.is_inline) {
                        if (!s->decl.attrs.is_used) continue;
                    }

                    s->flags |= STMT_FLAGS_HAS_IR_BACKING;
                } else if (s->op == STMT_GLOBAL_DECL || s->op == STMT_DECL) {
                    if (!s->decl.attrs.is_static  && !s->decl.attrs.is_extern &&
                        !s->decl.attrs.is_typedef && !s->decl.attrs.is_inline &&
                        s->decl.name && cuik_canonical_type(s->decl.type)->kind != KIND_FUNC) {
                        nl_strmap_put_cstr(cu->export_table, s->decl.name, s);
                    }

                    s->flags |= STMT_FLAGS_HAS_IR_BACKING;
                }
            }
        }
    }

    #ifdef CUIK_USE_TB
    Futex remaining = task_cap;
    FOR_EACH_TU(tu, cu) {
        size_t count = dyn_array_length(tu->top_level_stmts);
        Stmt** top_level = tu->top_level_stmts;

        for (size_t i = 0; i < count; i += BATCH_SIZE) {
            size_t end = i + BATCH_SIZE;
            if (end >= count) end = count;

            IRAllocTask t = {
                .mod = mod,
                .tu = tu,
                .stmts = &top_level[i],
                .count = end - i,
                .remaining = &remaining
            };
            CUIK_CALL(thread_pool, submit, ir_alloc_task, sizeof(t), &t);
        }
    }

    if (thread_pool) futex_wait_eq(&remaining, 0);
    #endif
}
