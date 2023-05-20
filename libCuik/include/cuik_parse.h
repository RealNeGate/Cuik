////////////////////////////////
// C parser
////////////////////////////////
// This module can parse C code (currently C11 with Microsoft and GNU extensions)
#pragma once
#include "cuik_prelude.h"

typedef struct Cuik_Warnings {
    // implicitly converting between types and losing information
    bool data_loss : 1;
    bool unused_decls : 1;
    bool unused_funcs : 1;
} Cuik_Warnings;

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

CUIK_API Cuik_ParseResult cuikparse_run(Cuik_Version version, TokenStream* restrict s, Cuik_Target* target, Arena* restrict arena, bool only_code_index);

// sets the user data field, returns the old value
CUIK_API void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud);

// returns user data field
CUIK_API void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu);

// force delete the translation unit regardless of references held
CUIK_API void cuik_destroy_translation_unit(TranslationUnit* restrict tu);

// does this translation unit have a main? what type?
CUIK_API Cuik_Entrypoint cuik_get_entrypoint_status(TranslationUnit* restrict tu);

CUIK_API TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu);

CUIK_API Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu);
CUIK_API size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu);

CUIK_API Cuik_Parser* cuikdg_get_parser(Cuik_Diagnostics* diag);

