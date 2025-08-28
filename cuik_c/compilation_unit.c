#include <common.h>
#include <cuik.h>
#include "parser.h"

CompilationUnit* cuik_create_compilation_unit(void) {
    CompilationUnit* cu = cuik_calloc(1, sizeof(CompilationUnit));
    #if CUIK_ALLOW_THREADS
    mtx_init(&cu->lock, mtx_plain);
    #endif
    return cu;
}

TranslationUnit* cuik_first_translation_unit(CompilationUnit* restrict cu) {
    return cu->head;
}

#ifdef CONFIG_HAS_TB
void cuik_compilation_unit_set_tb_module(CompilationUnit* restrict cu, TB_Module* mod) {
    cu->ir_mod = mod;
}

TB_Module* cuik_compilation_unit_tb_module(CompilationUnit* restrict cu) {
    return cu->ir_mod;
}
#endif

void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    #if CUIK_ALLOW_THREADS
    mtx_lock(&cu->lock);
    #endif

    #ifdef CONFIG_HAS_TB
    tu->ir_mod = cu->ir_mod;
    #endif

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;
    cu->count += 1;

    #if CUIK_ALLOW_THREADS
    mtx_unlock(&cu->lock);
    #endif
}

void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
    if (cu == NULL) {
        return;
    }

    // walk all the TUs and free them (if they're not freed already)
    TranslationUnit* tu = cu->head;
    while (tu != NULL) {
        TranslationUnit* next = tu->next;
        cuik_destroy_translation_unit(tu);
        tu = next;
    }

    #if CUIK_ALLOW_THREADS
    mtx_destroy(&cu->lock);
    #endif

    cuik_free(cu);
}

size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu) {
    return cu->count;
}
