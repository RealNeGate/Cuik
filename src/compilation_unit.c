#include "compilation_unit.h"

void compilation_unit_init(CompilationUnit* cu) {
	*cu = (CompilationUnit){ 0 };
	mtx_init(&cu->mutex, mtx_plain);
}

void compilation_unit_append(CompilationUnit* cu, TranslationUnit* tu) {
	assert(tu->next == NULL && "somehow the TU is attached already...");
	mtx_lock(&cu->mutex);
	
	tu->parent = cu;
	
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

void compilation_unit_internal_link(CompilationUnit* cu) {
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
