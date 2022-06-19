#pragma once
#include <common.h>
#include <cuik.h>
#include <ext/threads.h>
#include <front/parser.h>

struct CompilationUnit {
    // anything extern might map to a different translation unit within
    // the same compilation unit which means it's not technically external
    ExportedSymbolEntry* export_table;
    mtx_t mutex;

    // linked list of all TUs referenced
    TranslationUnit* head;
    TranslationUnit* tail;
};

#define FOR_EACH_TU(it, cu) \
for (TranslationUnit* it = (cu)->head; it; it = it->next)
