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
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <tb.h>

// returns NULL on failure
TB_Symbol* cuikcg_top_level(TranslationUnit* restrict tu, TB_Module* m, Stmt* restrict s);
