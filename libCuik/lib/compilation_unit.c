#include <common.h>
#include <cuik.h>
#include <threads.h>
#include <futex.h>

#include "front/parser.h"

CUIK_API CompilationUnit* cuik_create_compilation_unit(void) {
    CompilationUnit* cu = cuik_calloc(1, sizeof(CompilationUnit));
    mtx_init(&cu->lock, mtx_plain);
    return cu;
}

CUIK_API void cuik_lock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_lock(&cu->lock);
}

CUIK_API void cuik_unlock_compilation_unit(CompilationUnit* restrict cu) {
    mtx_unlock(&cu->lock);
}

CUIK_API TranslationUnit* cuik_first_translation_unit(CompilationUnit* restrict cu) {
    return cu->head;
}

CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    cuik_lock_compilation_unit(cu);

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;
    cu->count += 1;

    cuik_unlock_compilation_unit(cu);
}

CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
    if (cu) {
        // walk all the TUs and free them (if they're not freed already)
        TranslationUnit* tu = cu->head;
        while (tu != NULL) {
            TranslationUnit* next = tu->next;
            cuik_destroy_translation_unit(tu);
            tu = next;
        }

        mtx_destroy(&cu->lock);
        *cu = (CompilationUnit){0};
    }
}

CUIK_API size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu) {
    return cu->count;
}

CUIK_API void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu, Cuik_IThreadpool* restrict thread_pool, int debug_info_level) {
    CUIK_TIMED_BLOCK("internal link") {
        CUIK_FOR_EACH_TU(tu, cu) {
            size_t count = dyn_array_length(tu->top_level_stmts);
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
                        ptrdiff_t search = nl_strmap_get_cstr(cu->export_table, name);

                        // only enter one of them and whichever goes in, will have IR backing
                        if (search < 0) {
                            s->flags |= STMT_FLAGS_HAS_IR_BACKING;
                            nl_strmap_put_cstr(cu->export_table, s->decl.name, s);
                        }
                    }
                }
            }
        }
    }
}
