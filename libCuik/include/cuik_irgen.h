////////////////////////////////////////////
// TB IR generation
////////////////////////////////////////////
// This is the necessary code to integrate TB with Cuik, it'll generate IR for top level
// statements.
//
// Notes:
//   * cuikcg_function and cuikcg_decl can be run on multiple threads
//     for the same module so long as the AST node is different.
//
//   * the generator functions will return NULL if the AST node is incompatible.
//
//   * unused AST nodes do not NEED be compiled.
//
#pragma once
#include "cuik_prelude.h"
#include <tb.h>

// allocates all the functions and globals necessary to do IRgen.
CUIK_API void cuikcg_allocate_ir(TranslationUnit* restrict tu, Cuik_IThreadpool* restrict thread_pool, TB_Module* m, bool debug);

// same as cuikcg_allocate_ir except it's used for single-TU setups
CUIK_API void cuikcg_allocate_ir2(TranslationUnit* tu, TB_Module* m, bool debug);

// returns NULL on failure
CUIK_API TB_Symbol* cuikcg_top_level(TranslationUnit* restrict tu, TB_Module* m, TB_Arena* ir, TB_Arena* tmp, Stmt* restrict s);
