#include "compilation_unit.h"
#include <timer.h>

CUIK_API void cuik_create_compilation_unit(CompilationUnit* restrict cu) {
    *cu = (CompilationUnit){0};
    mtx_init(&cu->mutex, mtx_plain);
}

CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    mtx_lock(&cu->mutex);

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;

    mtx_unlock(&cu->mutex);
}

CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
    // walk all the TUs and free them (if they're not freed already)
    TranslationUnit* tu = cu->head;
    while (tu != NULL) {
        TranslationUnit* next = tu->next;
        cuik_destroy_translation_unit(tu);
        tu = next;
    }

    mtx_destroy(&cu->mutex);
    *cu = (CompilationUnit){0};
}

CUIK_API void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu) {
    FOR_EACH_TU(tu, cu) {
        size_t count = arrlen(tu->top_level_stmts);

        for (size_t i = 0; i < count; i++) {
            Stmt* s = tu->top_level_stmts[i];

            if (s->op == STMT_FUNC_DECL) {
                if (!s->decl.attrs.is_static &&
                    !s->decl.attrs.is_inline) {
                    //printf("Export! %s (Function: %d)\n", s->decl.name, s->backing.f);

                    shput(cu->export_table, s->decl.name, s);
                }
            } else if (s->op == STMT_GLOBAL_DECL ||
                       s->op == STMT_DECL) {
                if (!s->decl.attrs.is_static &&
                    !s->decl.attrs.is_extern &&
                    !s->decl.attrs.is_typedef &&
                    !s->decl.attrs.is_inline &&
                    s->decl.initial != 0) {
                    //printf("Export! %s (Global: %d)\n", s->decl.name, s->backing.g);

                    shput(cu->export_table, s->decl.name, s);
                }
            }
        }
    }
}
