#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <tb.h>

#define CUIK_API

// opaque structs
typedef struct threadpool_t threadpool_t;
typedef struct TokenStream TokenStream;
typedef struct Stmt Stmt;
typedef struct Token Token;
typedef struct TranslationUnit TranslationUnit;
typedef struct CompilationUnit CompilationUnit;
typedef struct Cuik_SystemLibs Cuik_SystemLibs;

////////////////////////////////////////////
// C preprocessor
////////////////////////////////////////////
typedef unsigned int SourceLocIndex;
typedef struct Cuik_CPP Cuik_CPP;

typedef struct Cuik_FileEntry {
    size_t parent_id;
    int depth;
    SourceLocIndex include_loc;

    const char* filepath;
    uint8_t* content;
} Cuik_FileEntry;

typedef struct Cuik_DefineRef {
    uint32_t bucket, id;
} Cuik_DefineRef;

typedef struct Cuik_Define {
    SourceLocIndex loc;

    struct {
        size_t len;
        const char* data;
    } key;

    struct {
        size_t len;
        const char* data;
    } value;
} Cuik_Define;

CUIK_API void cuik_init(void);

CUIK_API void cuikpp_init(Cuik_CPP* ctx);
CUIK_API void cuikpp_deinit(Cuik_CPP* ctx);
CUIK_API void cuikpp_dump(Cuik_CPP* ctx);

// You can't preprocess any more files after this
CUIK_API void cuikpp_finalize(Cuik_CPP* ctx);

// The file table may contain duplicates (for now...) but it stores all
// the loaded files by this instance of the preprocessor, in theory one
// could write a proper incremental compilation model using this for TU
// dependency tracking but... incremental compilation is a skill issue
CUIK_API size_t cuikpp_get_file_table_count(Cuik_CPP* ctx);
CUIK_API Cuik_FileEntry* cuikpp_get_file_table(Cuik_CPP* ctx);

// Locates an include file from the `path` and copies it's fully qualified path into `output`
CUIK_API bool cuikpp_find_include_include(Cuik_CPP* ctx, char output[FILENAME_MAX], const char* path);

// Adds include directory to the search list
CUIK_API void cuikpp_add_include_directory(Cuik_CPP* ctx, const char dir[]);

// Basically just `#define key`
CUIK_API void cuikpp_define_empty(Cuik_CPP* ctx, const char key[]);

// Basically just `#define key value`
CUIK_API void cuikpp_define(Cuik_CPP* ctx, const char key[], const char value[]);

// Convert C preprocessor state and an input file into a final preprocessed stream
CUIK_API TokenStream cuikpp_run(Cuik_CPP* ctx, const char filepath[FILENAME_MAX]);

// Used to make iterators for the define list, for example:
//
// Cuik_DefineRef it, curr = cuikpp_first_define(cpp);
// while (it = curr, cuikpp_next_define(cpp, &curr)) { }
CUIK_API Cuik_DefineRef cuikpp_first_define(Cuik_CPP* ctx);
CUIK_API bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineRef* src);

// Get the information from a define reference
CUIK_API Cuik_Define cuikpp_get_define(Cuik_CPP* ctx, Cuik_DefineRef src);

// this will return a Cuik_CPP through out_cpp that you have to free once you're
// done with it (after all frontend work is done), the out_cpp can also be finalized if
// you dont need the defines table.
CUIK_API TokenStream cuik_preprocess_simple(Cuik_CPP* restrict out_cpp, const char* filepath, Cuik_SystemLibs* libs, size_t include_count, const char* includes[]);

// Locates all system libraries
CUIK_API Cuik_SystemLibs* cuik_get_system_includes(const char* cuik_crt_directory);

////////////////////////////////////////////
// C parsing
////////////////////////////////////////////
// if thread_pool is NULL, parsing is single threaded
//
// if ir_module is NULL then translation unit will not be used for IR generation,
// multiple translation units can be created for the same module you just have to
// attach them to each other with a compilation unit and internally link them.
CUIK_API TranslationUnit* cuik_parse_translation_unit(TB_Module* restrict ir_module, TokenStream* restrict s, threadpool_t* restrict thread_pool);
CUIK_API void cuik_destroy_translation_unit(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Token stream
////////////////////////////////////////////
CUIK_API Token* cuik_get_tokens(TokenStream* restrict s);
CUIK_API size_t cuik_get_token_count(TokenStream* restrict s);

////////////////////////////////////////////
// IR generation
////////////////////////////////////////////
// Generates TBIR for a specific top-level statement
CUIK_API void cuik_generate_ir(TranslationUnit* restrict tu, Stmt* restrict s);

////////////////////////////////////////////
// Translation unit management
////////////////////////////////////////////
typedef void Cuik_TopLevelVisitor(TranslationUnit* restrict tu, Stmt* restrict s, void* user_data);

CUIK_API void cuik_visit_top_level(TranslationUnit* restrict tu, void* user_data, Cuik_TopLevelVisitor* visitor);
CUIK_API void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist);

CUIK_API bool cuik_is_in_main_file(TranslationUnit* restrict tu, SourceLocIndex loc);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
CUIK_API void cuik_create_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu);

#include "cuik_private.h"
