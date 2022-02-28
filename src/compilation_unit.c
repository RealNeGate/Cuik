#include "compilation_unit.h"

void compilation_unit_init(CompilationUnit* cu) {
	*cu = (CompilationUnit){ 0 };
	mtx_init(&cu->mutex, mtx_plain);
}

void compilation_unit_append(CompilationUnit* cu, TranslationUnit* tu) {
	assert(tu->next == NULL && "somehow the TU is attached already...");
	mtx_lock(&cu->mutex);
	
	if (cu->tail == NULL) cu->head = tu;
	else cu->tail->next = tu;
	
	cu->tail = tu;
	
	mtx_unlock(&cu->mutex);
}

void compilation_unit_deinit(CompilationUnit* cu) {
	// walk all the TUs and free them (if they're not freed already)
	TranslationUnit* tu = cu->head;
	while (tu) {
		TranslationUnit* next = tu->next;
		translation_unit_deinit(tu);
		free(tu);
		
		tu = next;
	}
	
	mtx_destroy(&cu->mutex);
	*cu = (CompilationUnit){ 0 };
}
