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
		for (size_t i = 0, count = arrlen(tu->top_level_stmts); i < count; i++) {
			StmtIndex stmt = tu->top_level_stmts[i];
			Stmt* restrict sp = &tu->stmts[stmt];
			
			if (sp->op == STMT_FUNC_DECL) {
				if (!sp->decl.attrs.is_static &&
					!sp->decl.attrs.is_inline) {
					//printf("Export! %s (Function: %d)\n", sp->decl.name, sp->backing.f);
					
					ExportedSymbol sym = { tu, stmt };
					shput(cu->export_table, sp->decl.name, sym);
				}
			} else if (sp->op == STMT_GLOBAL_DECL ||
					   sp->op == STMT_DECL) {
				if (!sp->decl.attrs.is_static &&
					!sp->decl.attrs.is_extern &&
					!sp->decl.attrs.is_typedef &&
					!sp->decl.attrs.is_inline &&
					sp->decl.initial != 0) {
					//printf("Export! %s (Global: %d)\n", sp->decl.name, sp->backing.g);
					
					ExportedSymbol sym = { tu, stmt };
					shput(cu->export_table, sp->decl.name, sym);
				}
			}
		}
		
		//printf("\n\n");
	}
}
