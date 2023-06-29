#pragma once
#include "cuik_prelude.h"
#include "cuik_fs.h"

#ifdef _WIN32
#define CUIK_PATH_SLASH_SEP '\\'
#else
#define CUIK_PATH_SLASH_SEP '/'
#endif

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
typedef struct Cuik_Target Cuik_Target;
typedef struct Cuik_Parser Cuik_Parser;
typedef struct Cuik_Diagnostics Cuik_Diagnostics;

// These are your options for arguments in diagnostics
typedef enum {
    DIAG_NOTE,
    DIAG_WARN,
    DIAG_ERR,
} DiagType;

typedef void (*Cuik_DiagCallback)(Cuik_Diagnostics* diag, void* userdata, DiagType type);

// TODO(NeGate): move this into common.h
typedef struct String {
    size_t length;
    const unsigned char* data;
} String;

typedef struct {
    uint16_t length;
    char data[];
} Cuik_PathFlex;

typedef enum Cuik_ExtensionFlags {
    CUIK_EXTENSIONS_MSVC  = (1u << 0u),
    CUIK_EXTENSIONS_CLANG = (1u << 1u),
    CUIK_EXTENSIONS_GCC   = (1u << 2u),
    CUIK_EXTENSIONS_CUIK  = (1u << 3u),
} Cuik_ExtensionFlags;

typedef enum Cuik_Version {
    // C language
    CUIK_VERSION_C89,
    CUIK_VERSION_C99,
    CUIK_VERSION_C11,
    CUIK_VERSION_C23,

    // GL shading language
    CUIK_VERSION_GLSL,
} Cuik_Version;

typedef enum Cuik_IntSuffix {
    //                u   l   l
    INT_SUFFIX_NONE = 0 + 0 + 0,
    INT_SUFFIX_U    = 1 + 0 + 0,
    INT_SUFFIX_L    = 0 + 2 + 0,
    INT_SUFFIX_UL   = 1 + 2 + 0,
    INT_SUFFIX_LL   = 0 + 2 + 2,
    INT_SUFFIX_ULL  = 1 + 2 + 2,
} Cuik_IntSuffix;

typedef struct SourceLoc {
    uint32_t raw;
} SourceLoc;

typedef struct SourceRange {
    SourceLoc start, end;
} SourceRange;

// This is what FileIDs refer to
typedef struct {
    const char* filename;
    bool is_system;

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
    char* content;

    // a DynArray(uint32_t) sorted to make it possible to binary search
    //   [line] = file_pos
    uint32_t* line_map;
} Cuik_FileEntry;

typedef struct Token {
    // it's a TknType but GCC doesn't like incomplete enums
    int type     : 30;
    int expanded : 1;
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

typedef struct TokenArray {
    // DynArray(Token)
    struct Token* tokens;
    size_t current;
} TokenArray;

typedef struct TokenStream {
    const char* filepath;
    TokenArray list;

    Cuik_Diagnostics* diag;

    // if true, the preprocessor is allowed to delete after completion.
    // this shouldn't enabled when caching files
    bool is_owned;

    // DynArray(MacroInvoke)
    MacroInvoke* invokes;

    // DynArray(Cuik_FileEntry)
    Cuik_FileEntry* files;
} TokenStream;

typedef struct ResolvedSourceLoc {
    Cuik_FileEntry* file;
    const char* line_str;
    uint32_t line, column;
} ResolvedSourceLoc;

typedef struct Cuik_FileLoc {
    Cuik_FileEntry* file;
    uint32_t pos; // in bytes
} Cuik_FileLoc;

bool cuik_path_set_no_ext(Cuik_Path* restrict dst, const char* src);
bool cuik_path_set_dir(Cuik_Path* restrict dst, const char* src);
bool cuik_path_set(Cuik_Path* restrict dst, const char* src);
bool cuik_path_append(Cuik_Path* restrict dst, const Cuik_Path* restrict a, size_t b_len, const char b[]);
bool cuik_path_append2(Cuik_Path* restrict dst, size_t a_len, const char a[], size_t b_len, const char b[]);

bool cuik_path_is_in(const Cuik_Path* restrict dst, const char* src);
bool cuik_path_has_ext(const Cuik_Path* restrict dst, const char* ext);
bool cuik_path_has_ext2(const Cuik_Path* restrict src);

// Used to make iterators for the define list, for example:
//
// Cuik_DefineIter it = cuikpp_first_define(cpp);
// while (cuikpp_next_define(cpp, &it)) { ... }
typedef struct Cuik_DefineIter {
    // public
    SourceLoc loc;
    String key, value;

    size_t index;
} Cuik_DefineIter;

#define CUIKPP_FOR_DEFINES(it, ctx) for (Cuik_DefineIter it = cuikpp_first_define(ctx); cuikpp_next_define(ctx, &it);)

CUIK_API Cuik_DefineIter cuikpp_first_define(Cuik_CPP* ctx);
CUIK_API bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* src);

// This is an iterator for include search list in the preprocessor:
//
// Cuik_FileEntry* f = NULL;
// while ((f = cuikpp_next_file(f)))
CUIK_API Cuik_FileEntry* cuikpp_next_file(Cuik_CPP* ctx, Cuik_FileEntry* f);

////////////////////////////////
// Preprocessor module
////////////////////////////////
typedef struct Cuik_FileResult {
    size_t length;
    char* data;
} Cuik_FileResult;

// returns true if it found the file, it'll also return the canonical name
typedef bool (*Cuikpp_LocateFile)(void* user_data, const Cuik_Path* restrict input, Cuik_Path* output, bool case_insensitive);
typedef bool (*Cuikpp_GetFile)(void* user_data, const Cuik_Path* restrict input, Cuik_FileResult* out_result, bool case_insensitive);

typedef struct {
    const char* filepath;
    Cuik_Version version;
    bool case_insensitive;

    // some callbacks :P
    void* diag_data;
    Cuik_DiagCallback diag;

    void* fs_data;
    Cuikpp_LocateFile locate;
    Cuikpp_GetFile fs;
} Cuik_CPPDesc;

// Initialize preprocessor, allocates memory which needs to be freed via cuikpp_free
CUIK_API Cuik_CPP* cuikpp_make(const Cuik_CPPDesc* restrict desc);

// NOTE: it doesn't own the memory for the files it may have used
// and thus you must free them, this can be done by iterating over
// them using CUIKPP_FOR_FILES.
CUIK_API void cuikpp_free(Cuik_CPP* ctx);

// You can't preprocess any more files after this
CUIK_API void cuikpp_finalize(Cuik_CPP* ctx);

// returns the final token stream (should not be called if you
// haven't finished iterating through cuikpp_next)
CUIK_API TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx);
CUIK_API void cuiklex_free_tokens(TokenStream* tokens);

CUIK_API Token* cuikpp_get_tokens(TokenStream* restrict s);
CUIK_API size_t cuikpp_get_token_count(TokenStream* restrict s);

CUIK_API Cuik_FileEntry* cuikpp_get_files(TokenStream* restrict s);
CUIK_API size_t cuikpp_get_file_count(TokenStream* restrict s);

////////////////////////////////
// Line info
////////////////////////////////
// converts macro token to the physical token in a file
CUIK_API SourceLoc cuikpp_get_physical_location(TokenStream* tokens, SourceLoc loc);

// returns true on success
CUIK_API ResolvedSourceLoc cuikpp_find_location(TokenStream* tokens, SourceLoc loc);
CUIK_API ResolvedSourceLoc cuikpp_find_location2(TokenStream* tokens, Cuik_FileLoc loc);
CUIK_API Cuik_FileLoc cuikpp_find_location_in_bytes(TokenStream* tokens, SourceLoc loc);

// returns NULL on an invalid source location
CUIK_API Cuik_FileEntry* cuikpp_find_file(TokenStream* tokens, SourceLoc loc);

// returns NULL for non-macros
CUIK_API MacroInvoke* cuikpp_find_macro(TokenStream* tokens, SourceLoc loc);

////////////////////////////////
// Preprocessor callback
////////////////////////////////
typedef enum {
    CUIKPP_CONTINUE,
    CUIKPP_DONE,
    CUIKPP_ERROR,
} Cuikpp_Status;

static bool cuiklex_is_macro_loc(SourceLoc loc) {
    return loc.raw & SourceLoc_IsMacro;
}

// simplifies whitespace for the lexer
CUIK_API void cuiklex_canonicalize(size_t length, char* data);

CUIK_API bool cuikpp_locate_file(void* user_data, const Cuik_Path* restrict input, Cuik_Path* output, bool case_insensitive);
CUIK_API bool cuikpp_default_fs(void* user_data, const Cuik_Path* restrict input, Cuik_FileResult* out_result, bool case_insensitive);

// Returns entire preprocessor on input state
CUIK_API Cuikpp_Status cuikpp_run(Cuik_CPP* restrict ctx);

// is the source location in the source file (none of the includes)
CUIK_API bool cuikpp_is_in_main_file(TokenStream* tokens, SourceLoc loc);

CUIK_API const char* cuikpp_get_main_file(TokenStream* tokens);

////////////////////////////////
// Preprocessor symbol table
////////////////////////////////
CUIK_API bool cuikpp_find_define_cstr(Cuik_CPP* restrict c, Cuik_DefineIter* out_ref, const char* key);
CUIK_API bool cuikpp_find_define(Cuik_CPP* restrict c, Cuik_DefineIter* out_ref, size_t keylen, const char key[]);
// Basically just `#define key`
CUIK_API void cuikpp_define_empty_cstr(Cuik_CPP* ctx, const char key[]);
CUIK_API void cuikpp_define_empty(Cuik_CPP* ctx, size_t keylen, const char key[]);
// Basically just `#define key value`
CUIK_API void cuikpp_define_cstr(Cuik_CPP* ctx, const char key[], const char value[]);
CUIK_API void cuikpp_define(Cuik_CPP* ctx, size_t keylen, const char key[], size_t vallen, const char value[]);
// Basically just `#undef key`
CUIK_API bool cuikpp_undef_cstr(Cuik_CPP* ctx, const char* key);
CUIK_API bool cuikpp_undef(Cuik_CPP* ctx, size_t keylen, const char* key);

////////////////////////////////
// Preprocessor includes
////////////////////////////////
// Adds include directory to the search list
CUIK_API void cuikpp_add_include_directory(Cuik_CPP* ctx, bool is_system, const char dir[]);

// Adds include directory to the search list but with printf formatting
CUIK_API void cuikpp_add_include_directoryf(Cuik_CPP* ctx, bool is_system, const char* fmt, ...);

// Locates an include file from the `path` and copies it's fully qualified path into `output`
// This is built for the cuikpp_default_run preprocessor handling, if you have a custom file system
// you'll need to iterate the include directories yourself
CUIK_API bool cuikpp_find_include_include(Cuik_CPP* ctx, char output[FILENAME_MAX], const char* path);

typedef struct {
    bool is_system;
    Cuik_Path* path;
} Cuik_IncludeDir;

CUIK_API Cuik_IncludeDir* cuikpp_get_include_dirs(Cuik_CPP* ctx);
CUIK_API size_t cuikpp_get_include_dir_count(Cuik_CPP* ctx);

////////////////////////////////
// C preprocessor pretty printer
////////////////////////////////
CUIK_API void cuikpp_dump_defines(Cuik_CPP* ctx);
CUIK_API void cuikpp_dump_tokens(TokenStream* s);

////////////////////////////////
// Diagnostic engine
////////////////////////////////
// We extended onto the standard printf format when it starts with `%!`, here's the
// full table of additions:
//
//     %!T       Cuik_Type
//     %!S       String
//
// fixit diagnostics are added by placing a # at the start of the format string and
// writing out a DiagFixit at the start of the var args
CUIK_API void diag_note(TokenStream* tokens, SourceRange loc, const char* fmt, ...);
CUIK_API void diag_warn(TokenStream* tokens, SourceRange loc, const char* fmt, ...);
CUIK_API void diag_err(TokenStream* tokens, SourceRange loc, const char* fmt, ...);

CUIK_API void cuikdg_dump_to_file(TokenStream* tokens, FILE* out);
CUIK_API void cuikdg_dump_to_stderr(TokenStream* tokens);
