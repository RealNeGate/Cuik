#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

enum {
    SourceLoc_IsMacro          = 1u << 31u,

    // if it's a macro
    // refers to an invocation ID tracked by the token stream
    SourceLoc_MacroIDBits      = 20u,
    SourceLoc_MacroOffsetBits  = 31u - SourceLoc_MacroIDBits,

    // if not a macro
    SourceLoc_FileIDBits       = 14u,
    SourceLoc_FilePosBits      = 31u - SourceLoc_FileIDBits,
};

typedef struct Cuik_CPP Cuik_CPP;

// TODO(NeGate): move this into common.h
typedef struct String {
    size_t length;
    const unsigned char* data;
} String;

typedef struct SourceLoc {
    uint32_t raw;
} SourceLoc;

typedef struct SourceRange {
    SourceLoc start, end;
} SourceRange;

// This is what FileIDs refer to
typedef struct {
    const char* filename;

    int depth;
    SourceLoc include_site;
    // describes how far from the start of the file we are.
    // used by line_map on big files
    uint32_t file_pos_bias;

    // NOTE: this is the size of this file chunk, big files consist
    // of multiple chunks so you should use...
    //
    // TODO(NeGate): make function for doing this
    uint32_t content_length;
    const char* content;

    // a DynArray(uint32_t) sorted to make it possible to binary search
    //   [line] = file_pos
    uint32_t* line_map;
} Cuik_File;

typedef struct Token {
    // it's a TknType but GCC doesn't like incomplete enums
    int type     : 31;
    int hit_line : 1;

    SourceLoc location;
    String content;
} Token;

// This is what MacroIDs refer to
typedef struct MacroInvoke {
    String name;

    // 0 means it's got no parent
    uint32_t parent;

    SourceRange def_site;
    SourceLoc call_site;
} MacroInvoke;

typedef struct TokenList {
    // DynArray(Token)
    struct Token* tokens;
    size_t current;
} TokenList;

typedef struct TokenStream {
    const char* filepath;

    TokenList list;
    // DynArray(Token)
    // struct Token* tokens;
    // size_t current;

    // if true, the preprocessor is allowed to delete after completion.
    // this shouldn't enabled when caching files
    bool is_owned;

    // DynArray(MacroInvoke)
    MacroInvoke* invokes;

    // DynArray(Cuik_File)
    Cuik_File* files;
} TokenStream;

typedef struct ResolvedSourceLoc {
    Cuik_File* file;
    const char* line_str;
    uint32_t line, column;
} ResolvedSourceLoc;

typedef struct Cuik_FileLoc {
    Cuik_File* file;
    uint32_t pos; // in bytes
} Cuik_FileLoc;

// Used to make iterators for the define list, for example:
//
// Cuik_DefineIter it = cuikpp_first_define(cpp);
// while (cuikpp_next_define(cpp, &it)) { ... }
typedef struct Cuik_DefineIter {
    // public
    SourceLoc loc;
    struct Cuik_DefineKey {
        size_t len;
        const char* data;
    } key;
    struct Cuik_DefineVal {
        size_t len;
        const char* data;
    } value;

    // internal
    uint32_t bucket, id;
} Cuik_DefineIter;

#define CUIKPP_FOR_DEFINES(it, ctx) for (Cuik_DefineIter it = cuikpp_first_define(ctx); cuikpp_next_define(ctx, &it);)

Cuik_DefineIter cuikpp_first_define(Cuik_CPP* ctx);
bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* src);

// This is an iterator for include search list in the preprocessor:
//
// Cuik_File* f = NULL;
// while ((f = cuikpp_next_file(f)))
Cuik_File* cuikpp_next_file(Cuik_CPP* ctx, Cuik_File* f);

////////////////////////////////
// Preprocessor module
////////////////////////////////
// Initialize preprocessor, allocates memory which needs to be freed via cuikpp_deinit
void cuikpp_init(Cuik_CPP* ctx, const char filepath[FILENAME_MAX]);

// NOTE: it doesn't own the memory for the files it may have used
// and thus you must free them, this can be done by iterating over
// them using CUIKPP_FOR_FILES
void cuikpp_deinit(Cuik_CPP* ctx);

// You can't preprocess any more files after this
void cuikpp_finalize(Cuik_CPP* ctx);

// returns the final token stream (should not be called if you
// haven't finished iterating through cuikpp_next)
TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx);

////////////////////////////////
// Line info
////////////////////////////////
// converts macro token to the physical token in a file
SourceLoc cuikpp_get_physical_location(TokenStream* tokens, SourceLoc loc);

// returns true on success
ResolvedSourceLoc cuikpp_find_location(TokenStream* tokens, SourceLoc loc);
ResolvedSourceLoc cuikpp_find_location2(TokenStream* tokens, Cuik_FileLoc loc);
Cuik_FileLoc cuikpp_find_location_in_bytes(TokenStream* tokens, SourceLoc loc);

// returns NULL on an invalid source location
Cuik_File* cuikpp_find_file(TokenStream* tokens, SourceLoc loc);

// returns NULL for non-macros
MacroInvoke* cuikpp_find_macro(TokenStream* tokens, SourceLoc loc);

////////////////////////////////
// Preprocessor coroutine
////////////////////////////////
typedef struct Cuikpp_Packet {
    enum {
        CUIKPP_PACKET_NONE,
        CUIKPP_PACKET_GET_FILE,
        CUIKPP_PACKET_QUERY_FILE,
        CUIKPP_PACKET_CANONICALIZE,
    } tag;
    union {
        // in case of GET_FILE:
        //   read a file from input_path and pass back a mutable buffer of the contents.
        struct {
            // input
            const char* input_path;
            bool is_primary;

            // output
            size_t length;
            char* data;
        } file;
        // in case of QUERY_FILE:
        //   found is set true if you found a file at 'input_path'
        //
        struct {
            // input
            const char* input_path;

            // output
            bool found;
        } query;
        // in case of CANONICALIZE:
        //   convert the filepath 'input_path' into a new filepath which is
        //   absolute, note that 'output_path' has the memory provided for you
        //   and is FILENAME_MAX chars long.
        struct {
            // input
            const char* input_path;

            // output
            char* output_path;
        } canonicalize;
    };
} Cuikpp_Packet;

typedef enum {
    CUIKPP_CONTINUE,
    CUIKPP_DONE,
    CUIKPP_ERROR,
} Cuikpp_Status;


////////////////////////////////
// Preprocessor includes
////////////////////////////////
// Adds include directory to the search list
void cuikpp_add_include_directory(Cuik_CPP* ctx, const char dir[]);

// Locates an include file from the `path` and copies it's fully qualified path into `output`
bool cuikpp_find_include_include(Cuik_CPP* ctx, char output[FILENAME_MAX], const char* path);

////////////////////////////////
// Preprocessor symbol table
////////////////////////////////
// Basically just `#define key`
void cuikpp_define_empty_cstr(Cuik_CPP* ctx, const char key[]);
void cuikpp_define_empty(Cuik_CPP* ctx, size_t keylen, const char key[]);
// Basically just `#define key value`
void cuikpp_define_cstr(Cuik_CPP* ctx, const char key[], const char value[]);
void cuikpp_define(Cuik_CPP* ctx, size_t keylen, const char key[], size_t vallen, const char value[]);
// Basically just `#undef key`
bool cuikpp_undef_cstr(Cuik_CPP* ctx, const char* key);
bool cuikpp_undef(Cuik_CPP* ctx, size_t keylen, const char* key);

////////////////////////////////
// C preprocessor pretty printer
////////////////////////////////
void cuikpp_dump_defines(Cuik_CPP* ctx);

////////////////////////////////
// Diagnostic engine
////////////////////////////////
// We extended onto the standard printf format when it starts with %_T, here's the
// full table of additions:
//
//     %_T       Cuik_Type
//     %_S       String
void diag_note(TokenStream* tokens, SourceRange loc, const char* fmt, ...);
void diag_warn(TokenStream* tokens, SourceRange loc, const char* fmt, ...);
void diag_err(TokenStream* tokens, SourceRange loc, const char* fmt, ...);
