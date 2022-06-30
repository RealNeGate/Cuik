#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <tb.h>

#define CUIK_API

#ifdef _WIN32
// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup _strdup
#else
int sprintf_s(char* buffer, size_t len, const char* format, ...);
#endif

// opaque structs
typedef struct TokenStream TokenStream;
typedef struct Stmt Stmt;
typedef struct Token Token;
typedef struct TranslationUnit TranslationUnit;
typedef struct CompilationUnit CompilationUnit;

////////////////////////////////////////////
// OOP enjoyers
////////////////////////////////////////////
// this is an interface for a threadpool that Cuik may use if you
// wish to ask it to multithread different internal components
typedef struct Cuik_IThreadpool {
    // fed into the member functions here
    void* user_data;

    // runs the function fn with arg as the parameter on a thread
    void (*submit)(void* user_data, void fn(void*), void* arg);

    // tries to work one job before returning (can also not work at all)
    void (*work_one_job)(void* user_data);
} Cuik_IThreadpool;

typedef struct Cuik_File {
    bool found;

    size_t length;
    char* data;
} Cuik_File;

typedef struct Cuik_IFileSystem {
    void* user_data;

    // is_query will only set .found if it finds a file, if it's not is_query then it attempts to
    // read the file and return a valid memory buffer (with at least 16 extra bytes on the end zeroed)
    Cuik_File (*get_file)(void* user_data, bool is_query, const char* path, size_t* out_length);

    // converts into an absolute path (powers the #pragma once subsystem)
    bool (*canonicalize)(void* user_data, char output[FILENAME_MAX], const char* input);
} Cuik_IFileSystem;

// default file system (just OS crap)
CUIK_API Cuik_IFileSystem cuik_default_fs;

////////////////////////////////////////////
// Target descriptor
////////////////////////////////////////////
typedef struct Cuik_TargetDesc Cuik_TargetDesc;

// these can be fed into the preprocessor and parser to define
// the correct builtins and predefined macros
const Cuik_TargetDesc* cuik_get_x64_target_desc(void);

////////////////////////////////////////////
// Profiler
////////////////////////////////////////////
// opens a starts writing a file about the timing info
CUIK_API void cuik_start_global_profiler(const char* filepath);
// will emit a JSON file of the profiled output you can feed into chrome://tracing or speedscopes
CUIK_API void cuik_stop_global_profiler(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
CUIK_API uint64_t cuik_time_in_nanos(void);

// Reports a region of time in the profiler file
CUIK_API void cuik_profile_region(uint64_t start, const char* fmt, ...);

// Usage:
// CUIK_TIMED_BLOCK("Beans %d", 5) {
//   ...
// }
#define CUIK_TIMED_BLOCK(...) for (uint64_t __t1 = cuik_time_in_nanos(), __i = 0; __i < 1; __i++, cuik_profile_region(__t1, __VA_ARGS__))

////////////////////////////////////////////
// General Cuik stuff
////////////////////////////////////////////
CUIK_API void cuik_init(void);

// locates the system includes, libraries and other tools. this is a global
// operation meaning that once it's only done once for the process.
CUIK_API void cuik_find_system_deps(const char* cuik_crt_directory);

// can only be called after cuik_find_system_deps
CUIK_API size_t cuik_get_system_search_path_count(void);
CUIK_API void cuik_get_system_search_paths(const char** out, size_t n);

CUIK_API bool cuik_lex_is_keyword(size_t length, const char* str);

////////////////////////////////////////////
// C preprocessor
////////////////////////////////////////////
typedef unsigned int SourceLocIndex;
typedef struct Cuik_CPP Cuik_CPP;

#define SOURCE_LOC_GET_DATA(loc) ((loc) & ~0xC0000000u)
#define SOURCE_LOC_GET_TYPE(loc) (((loc) & 0xC0000000u) >> 30u)
#define SOURCE_LOC_SET_TYPE(type, raw) (((type << 30) & 0xC0000000u) | ((raw) & ~0xC0000000u))

typedef enum SourceLocType {
    SOURCE_LOC_UNKNOWN = 0,
    SOURCE_LOC_NORMAL = 1,
    SOURCE_LOC_MACRO = 2,
    SOURCE_LOC_FILE = 3
} SourceLocType;

typedef struct SourceRange {
    SourceLocIndex start, end;
} SourceRange;

typedef struct SourceLine {
    const char* filepath;
    const unsigned char* line_str;
    SourceLocIndex parent;
    int line;
} SourceLine;

typedef struct SourceLoc {
    SourceLine* line;
    short columns;
    short length;
} SourceLoc;

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

CUIK_API void cuikpp_init(Cuik_CPP* ctx, const Cuik_IFileSystem* fs);
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
CUIK_API TokenStream cuik_preprocess_simple(
    Cuik_CPP* restrict out_cpp, const char* filepath,
    const Cuik_IFileSystem* fs, const Cuik_TargetDesc* target_desc,
    bool system_includes, size_t include_count, const char* includes[]
);

////////////////////////////////////////////
// C parsing
////////////////////////////////////////////
typedef enum Cuik_IntSuffix {
    //                u   l   l
    INT_SUFFIX_NONE = 0 + 0 + 0,
    INT_SUFFIX_U    = 1 + 0 + 0,
    INT_SUFFIX_L    = 0 + 2 + 0,
    INT_SUFFIX_UL   = 1 + 2 + 0,
    INT_SUFFIX_LL   = 0 + 2 + 2,
    INT_SUFFIX_ULL  = 1 + 2 + 2,
} Cuik_IntSuffix;

typedef unsigned char* Atom;
typedef struct Cuik_Type Cuik_Type;

// if thread_pool is NULL, parsing is single threaded
//
// if ir_module is NULL then translation unit will not be used for IR generation,
// multiple translation units can be created for the same module you just have to
// attach them to each other with a compilation unit and internally link them.
CUIK_API TranslationUnit* cuik_parse_translation_unit(
    TB_Module* restrict ir_module, TokenStream* restrict s,
    const Cuik_TargetDesc* target_desc, Cuik_IThreadpool* restrict thread_pool
);

CUIK_API void cuik_destroy_translation_unit(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Token stream
////////////////////////////////////////////
CUIK_API const char* cuik_get_location_file(TokenStream* restrict s, SourceLocIndex loc);
CUIK_API int cuik_get_location_line(TokenStream* restrict s, SourceLocIndex loc);

CUIK_API Token* cuik_get_tokens(TokenStream* restrict s);
CUIK_API size_t cuik_get_token_count(TokenStream* restrict s);

////////////////////////////////////////////
// IR generation
////////////////////////////////////////////
// Generates TBIR for a specific top-level statement, returns a pointer to the TB_Function
// it just generated such that a user could do TB related operations on it
CUIK_API TB_Module* cuik_get_tb_module(TranslationUnit* restrict tu);
CUIK_API TB_Function* cuik_stmt_gen_ir(TranslationUnit* restrict tu, Stmt* restrict s);

////////////////////////////////////////////
// Translation unit management
////////////////////////////////////////////
typedef void Cuik_TopLevelVisitor(TranslationUnit* restrict tu, Stmt* restrict s, void* user_data);

CUIK_API void cuik_visit_top_level(TranslationUnit* restrict tu, void* user_data, Cuik_TopLevelVisitor* visitor);
CUIK_API void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist);

CUIK_API bool cuik_is_in_main_file(TranslationUnit* restrict tu, SourceLocIndex loc);
CUIK_API TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
CUIK_API void cuik_create_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu);

////////////////////////////////////////////
// Linker
////////////////////////////////////////////
typedef struct Cuik_Linker Cuik_Linker;

// True if success
bool cuiklink_init(Cuik_Linker* l);
void cuiklink_deinit(Cuik_Linker* l);

// uses the system library paths located by cuik_find_system_deps
void cuiklink_add_default_libpaths(Cuik_Linker* l);

// Adds a directory to the library searches
void cuiklink_add_libpath(Cuik_Linker* l, const char* filepath);

#if _WIN32
// Windows native strings are UTF-16 so i provide an option for that if you want
void cuiklink_add_libpath_wide(Cuik_Linker* l, const wchar_t* filepath);
#endif

// This can be a static library or object file
void cuiklink_add_input_file(Cuik_Linker* l, const char* filepath);

// Calls the system linker
// return true if it succeeds
bool cuiklink_invoke(Cuik_Linker* l, const char* filename, const char* crt_name);

#include "cuik_private.h"
