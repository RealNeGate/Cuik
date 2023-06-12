#pragma once
#include <common.h>
#include <arena.h>

#include "parser.h"

bool type_compatible(TranslationUnit* tu, Cuik_Type* a, Cuik_Type* b, Subexpr* a_expr);

// Semantics pass
// out_offset is added onto so it needs to be initialized
Member* sema_traverse_members(Cuik_Type* record_type, Atom name, uint32_t* out_offset);

// check that each type matches the cast type
bool cuik__type_check_args(TranslationUnit* tu, Cuik_Expr* e, int arg_count, Subexpr** args);
Cuik_QualType cuik__sema_expr(TranslationUnit* tu, Cuik_Expr* restrict e);
