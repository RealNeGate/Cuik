#pragma once
#include <arena.h>
#include <common.h>
#include <compilation_unit.h>
#include <timer.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern atomic_int sema_error_count;
extern thread_local bool in_the_semantic_phase;

bool type_compatible(TranslationUnit* tu, Type* a, Type* b, Expr* a_expr);

// Analysis pass
void analysis_pass(TranslationUnit* tu);

// AST dump
void ast_dump(TranslationUnit* tu, FILE* stream);
void ast_dump_stats(TranslationUnit* tu, FILE* stream);
void ast_dump_type(TranslationUnit* tu, Type* type, int depth, int offset);

// Semantics pass
// out_offset is added onto so it needs to be initialized
Member* sema_resolve_member_access(TranslationUnit* tu, Expr* e, uint32_t* out_offset);
Member* sema_traverse_members(TranslationUnit* tu, Type* record_type, Atom name, uint32_t* out_offset);

Type* sema_guess_type(TranslationUnit* tu, Stmt* restrict s);

Type* sema_expr(TranslationUnit* tu, Expr* e);

// if thread_pool is NULL, the semantics are done single threaded
void sema_pass(CompilationUnit* cu, TranslationUnit* tu, threadpool_t* thread_pool, bool frontend_only);
