#pragma once
#include <common.h>
#include <arena.h>

#include "parser.h"

bool type_compatible(TranslationUnit* tu, Cuik_Type* a, Cuik_Type* b, Subexpr* a_expr);

// check that each type matches the cast type
bool cuik__type_check_args(TranslationUnit* tu, Cuik_Expr* e, int arg_count, Subexpr** args);
Cuik_QualType cuik__sema_expr(TranslationUnit* tu, Cuik_Expr* restrict e);
void sema_stmt(TranslationUnit* tu, Stmt* restrict s);
