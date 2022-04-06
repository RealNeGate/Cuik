#pragma once
#include <common.h>
#include <arena.h>
#include <timer.h>
#include <compilation_unit.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern _Atomic int sema_error_count;
extern thread_local bool in_the_semantic_phase;

void ast_dump(TranslationUnit* tu, FILE* stream);
void ast_dump_stats(TranslationUnit* tu, FILE* stream);

// out_offset is added onto so it needs to be initialized
Member* sema_resolve_member_access(TranslationUnit* tu, ExprIndex e, uint32_t* out_offset);
Member* sema_traverse_members(TranslationUnit* tu, Type* record_type, Atom name, uint32_t* out_offset);

TypeIndex sema_guess_type(TranslationUnit* tu, StmtIndex s);

TypeIndex sema_expr(TranslationUnit* tu, ExprIndex e);
void sema_pass(CompilationUnit* cu, TranslationUnit* tu, bool frontend_only);
