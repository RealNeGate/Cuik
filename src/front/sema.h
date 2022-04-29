#pragma once
#include <common.h>
#include <arena.h>
#include <timer.h>
#include <compilation_unit.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern _Atomic int sema_error_count;
extern thread_local bool in_the_semantic_phase;

#define sema_info(loc, ...) report(REPORT_INFO, &tu->tokens.line_arena[loc], __VA_ARGS__)
#define sema_warn(loc, ...) report(REPORT_WARNING, &tu->tokens.line_arena[loc], __VA_ARGS__)
#define sema_error(loc, ...) report(REPORT_ERROR, &tu->tokens.line_arena[loc], __VA_ARGS__)

bool type_compatible(TranslationUnit* tu, TypeIndex a, TypeIndex b, Expr* a_expr);

// Analysis pass
void analysis_pass(TranslationUnit* tu);

// AST dump
void ast_dump(TranslationUnit* tu, FILE* stream);
void ast_dump_stats(TranslationUnit* tu, FILE* stream);
void ast_dump_type(TranslationUnit* tu, TypeIndex type, int depth, int offset);

// Semantics pass
// out_offset is added onto so it needs to be initialized
Member* sema_resolve_member_access(TranslationUnit* tu, Expr* e, uint32_t* out_offset);
Member* sema_traverse_members(TranslationUnit* tu, Type* record_type, Atom name, uint32_t* out_offset);

TypeIndex sema_guess_type(TranslationUnit* tu, Stmt* restrict s);

TypeIndex sema_expr(TranslationUnit* tu, Expr* e);
void sema_pass(CompilationUnit* cu, TranslationUnit* tu, bool frontend_only);
