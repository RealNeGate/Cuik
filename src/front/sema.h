#pragma once
#include <common.h>
#include <arena.h>
#include <timer.h>
#include <compilation_unit.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern _Atomic int sema_error_count;

void ast_dump(TranslationUnit* tu, FILE* stream);
void ast_dump_stats(TranslationUnit* tu, FILE* stream);

TypeIndex sema_expr(TranslationUnit* tu, ExprIndex e);
void sema_pass(CompilationUnit* cu, TranslationUnit* tu, bool frontend_only);
