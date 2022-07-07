#pragma once
#include <arena.h>
#include <common.h>
#include <compilation_unit.h>
#include <timer.h>

#include <stb_ds.h>
#include <front/parser.h>

extern atomic_int sema_error_count;
extern thread_local bool in_the_semantic_phase;

bool type_compatible(TranslationUnit* tu, Cuik_Type* a, Cuik_Type* b, Expr* a_expr);

// Semantics pass
// out_offset is added onto so it needs to be initialized
Member* sema_resolve_member_access(TranslationUnit* tu, Expr* e, uint32_t* out_offset);
Member* sema_traverse_members(TranslationUnit* tu, Cuik_Type* record_type, Atom name, uint32_t* out_offset);

Cuik_Type* sema_guess_type(TranslationUnit* tu, Stmt* restrict s);
Cuik_Type* sema_expr(TranslationUnit* tu, Expr* e);
