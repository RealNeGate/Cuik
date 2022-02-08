#pragma once
#include <common.h>
#include <arena.h>

#include <ext/stb_ds.h>
#include <front/parser.h>

extern _Atomic int sema_error_count;

void sema_check(TranslationUnit* tu, StmtIndex s);
void sema_remove_unused(TranslationUnit* tu);
