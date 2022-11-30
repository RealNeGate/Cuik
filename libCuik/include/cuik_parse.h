////////////////////////////////
// C parser
////////////////////////////////
// This module can parse C code (currently C11 with Microsoft and GNU extensions)
#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum Cuik_ExtensionFlags {
    CUIK_EXTENSIONS_MSVC  = (1u << 0u),
    CUIK_EXTENSIONS_CLANG = (1u << 1u),
    CUIK_EXTENSIONS_GCC   = (1u << 2u),
    CUIK_EXTENSIONS_CUIK  = (1u << 3u),
} Cuik_ExtensionFlags;

typedef enum Cuik_ParseVersion {
    CUIK_VERSION_C89,
    CUIK_VERSION_C99,
    CUIK_VERSION_C11,
    CUIK_VERSION_C23,
} Cuik_ParseVersion;

typedef enum Cuik_Entrypoint {
    CUIK_ENTRYPOINT_NONE,

    CUIK_ENTRYPOINT_MAIN,
    CUIK_ENTRYPOINT_WINMAIN,
} Cuik_Entrypoint;

// Standard parsing
typedef struct Cuik_GlobalSymbols Cuik_GlobalSymbols;

// This is generated from
//    #pragma comment(lib, "somelib.lib")
typedef struct Cuik_ImportRequest {
    struct Cuik_ImportRequest* next;
    const char* lib_name;
} Cuik_ImportRequest;

typedef struct Cuik_ParseResult {
    int error_count;

    TranslationUnit* tu;         // if error_count == 0, then tu is a valid TU.
    Cuik_ImportRequest* imports; // linked list of imported libs.
} Cuik_ParseResult;

Cuik_ParseResult cuikparse_run(Cuik_ParseVersion version, TokenStream* restrict s, Cuik_Target* target);















typedef struct Cuik_Warnings {
    // implicitly converting between types and losing information
    bool data_loss : 1;
    bool unused_decls : 1;
    bool unused_funcs : 1;
} Cuik_Warnings;

// used to initialize translation units with cuik_parse_translation_unit
typedef struct Cuik_TranslationUnitDesc {
    // tokens CANNOT be NULL
    TokenStream* tokens;

    // warnings CANNOT be NULL
    const Cuik_Warnings* warnings;

    #ifdef CUIK_USE_TB
    // if ir_module is non-NULL then translation unit will be used for
    // IR generation and function and global signatures will be filled
    // in accordingly, multiple translation units can be created for the
    // same module you just have to attach them to each other with a
    // compilation unit and internally link them.
    TB_Module* ir_module;
    bool has_debug_info;
    #endif

    // if target is non-NULL, builtins will be used based on said target.
    Cuik_Target* target;

    // if thread_pool is NULL, parsing is single threaded
    Cuik_IThreadpool* thread_pool;
} Cuik_TranslationUnitDesc;

TranslationUnit* cuik_parse_translation_unit(const Cuik_TranslationUnitDesc* restrict desc);

// sets the user data field, returns the old value
void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud);

// returns user data field
void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu);

// force delete the translation unit regardless of references held
void cuik_destroy_translation_unit(TranslationUnit* restrict tu);

////////////////////////////////
// Standard parsing
////////////////////////////////
// does this translation unit have a main? what type?
Cuik_Entrypoint cuik_get_entrypoint_status(TranslationUnit* restrict tu);

TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu);

Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu);
size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu);
