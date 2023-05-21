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

#ifdef CUIK_USE_TB
CUIK_API TB_Module* cuik_compilation_unit_tb_module(CompilationUnit* restrict cu) {
    return cu->ir_mod;
}
#endif

CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu) {
    assert(tu->next == NULL && "somehow the TU is already attached to something...");
    cuik_lock_compilation_unit(cu);

    #ifdef CUIK_USE_TB
    tu->ir_mod = cu->ir_mod;
    #endif

    tu->parent = cu;

    if (cu->tail == NULL) cu->head = tu;
    else cu->tail->next = tu;
    cu->tail = tu;
    cu->count += 1;

    cuik_unlock_compilation_unit(cu);
}

CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu) {
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

    nl_map_free(cu->export_table);
    mtx_destroy(&cu->lock);
    cuik_free(cu);
}

CUIK_API size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu) {
    return cu->count;
}
