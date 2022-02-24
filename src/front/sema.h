#pragma once
#include <common.h>
#include <arena.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern _Atomic int sema_error_count;

void ast_dump(TranslationUnit* tu, FILE* stream);
void sema_pass(CompilationUnit* cu, TranslationUnit* tu);
