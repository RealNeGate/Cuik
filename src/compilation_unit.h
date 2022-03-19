#pragma once
#include <common.h>
#include <front/parser.h>
#include <ext/threads.h>

typedef struct CompilationUnit {
	// anything extern might map to a different translation unit within
	// the same compilation unit which means it's not technically external
	ExportedSymbolEntry* export_table;
	mtx_t mutex;
	
	// linked list of all TUs referenced
	TranslationUnit* head;
	TranslationUnit* tail;
} CompilationUnit;

void compilation_unit_init(CompilationUnit* cu);
void compilation_unit_append(CompilationUnit* cu, TranslationUnit* tu);
void compilation_unit_deinit(CompilationUnit* cu);
void compilation_unit_internal_link(CompilationUnit* cu);

#define FOR_EACH_TU(it, cu) \
for (TranslationUnit* it = (cu)->head; it; it = it->next)
