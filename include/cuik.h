#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef CUIK_USE_TB
#include <tb.h>
#endif

#define CUIK_API extern

#ifdef _WIN32
// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup _strdup
#else
int sprintf_s(char* buffer, size_t len, const char* format, ...);
#endif

// opaque structs
typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Token Token;
typedef struct TranslationUnit TranslationUnit;
typedef struct CompilationUnit CompilationUnit;
typedef struct Cuik_Type Cuik_Type;

/*typedef struct Cuik_Report {
    Cuik_ReportFormat format;

    // NULL for not applicable
    Expr* exprs[2];
    Stmt* stmts[2];
    Cuik_Type* types[2];
    SourceLoc* locations[2];
} Cuik_Report;*/

////////////////////////////////////////////
// Interfaces
////////////////////////////////////////////
typedef struct Cuik_IThreadpool {
    // fed into the member functions here
    void* user_data;

    // runs the function fn with arg as the parameter on a thread
    void (*submit)(void* user_data, void fn(void*), void* arg);

    // tries to work one job before returning (can also not work at all)
    void (*work_one_job)(void* user_data);
} Cuik_IThreadpool;

typedef struct Cuik_IProfiler {
    void* user_data;

    void (*start)(void* user_data);
    void (*stop)(void* user_data);

    void (*begin_plot)(void* user_data, uint64_t nanos, const char* label);
    void (*end_plot)(void* user_data, uint64_t nanos);
} Cuik_IProfiler;

typedef struct Cuik_IDiagnostic {
    void* user_data;

    //void(*report)(void* user_data, const Cuik_Report* r);
} Cuik_IDiagnostic;

// for doing calls on the interfaces
#define CUIK_CALL(object, action, ...) ((object)->action((object)->user_data, ##__VA_ARGS__))

////////////////////////////////////////////
// Target descriptor
////////////////////////////////////////////
typedef struct Cuik_ArchDesc Cuik_ArchDesc;

// these can be fed into the preprocessor and parser to define
// the correct builtins and predefined macros
const Cuik_ArchDesc* cuik_get_x64_target_desc(void);

////////////////////////////////////////////
// Profiler
////////////////////////////////////////////
// lock_on_plot is true if the profiler->plot function cannot be called on multiple threads at
// the same time.
CUIK_API void cuik_start_global_profiler(const Cuik_IProfiler* profiler, bool lock_on_plot);
CUIK_API void cuik_stop_global_profiler(void);
CUIK_API bool cuik_is_profiling(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
CUIK_API uint64_t cuik_time_in_nanos(void);

// Reports a region of time to the profiler callback
CUIK_API void cuik_profile_region_start(uint64_t now, const char* fmt, ...);
CUIK_API void cuik_profile_region_end(void);

// Usage:
// CUIK_TIMED_BLOCK("Beans %d", 5) {
//   ...
// }
#define CUIK_TIMED_BLOCK(...) for (uint64_t __i = (cuik_profile_region_start(cuik_time_in_nanos(), __VA_ARGS__), 0); __i < 1; __i++, cuik_profile_region_end())

////////////////////////////////////////////
// General Cuik stuff
////////////////////////////////////////////
// identical to the TB_System enum
typedef enum Cuik_System {
    CUIK_SYSTEM_WINDOWS,
    CUIK_SYSTEM_LINUX,
    CUIK_SYSTEM_MACOS,

    // Not supported yet
    CUIK_SYSTEM_ANDROID
} Cuik_System;

typedef struct {
    Cuik_System sys;
    const Cuik_ArchDesc* arch;
} Cuik_Target;

#ifdef CUIK_USE_TB
inline static TB_System cuik_system_to_tb(Cuik_System s) { return (TB_System) s; }
#endif

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
typedef struct Cuik_FileCache Cuik_FileCache;

typedef enum SourceLocType {
    SOURCE_LOC_UNKNOWN = 0,
    SOURCE_LOC_NORMAL = 1,
    SOURCE_LOC_MACRO = 2,
    SOURCE_LOC_FILE = 3
} SourceLocType;

typedef struct SourceLoc {
    // SourceLine is located relative_loc_index SourceLocs before this entry
    uint16_t relative_loc_index;

    struct SourceLine* line;
    SourceLocIndex expansion;

    uint16_t columns;
    uint16_t length    : 14;
    SourceLocType type : 2;
} SourceLoc;

typedef struct SourceLine {
    const char* filepath;
    const unsigned char* line_str;
    SourceLocIndex parent;
    int line;
} SourceLine;

typedef struct TokenStream {
    const char* filepath;

    // if true, the preprocessor is allowed to delete after completion.
    // this shouldn't enabled when caching files
    bool is_owned;

    // DynArray(Token)
    struct Token* tokens;
    size_t current;

    // DynArray(SourceLoc)
    struct SourceLoc* locations;
} TokenStream;

typedef struct Cuik_FileEntry {
    size_t parent_id;
    int depth;
    SourceLocIndex include_loc;

    const char* filepath;
    TokenStream tokens;
} Cuik_FileEntry;

CUIK_API Cuik_FileCache* cuik_fscache_create(void);
CUIK_API void cuik_fscache_destroy(Cuik_FileCache* restrict c);
CUIK_API void cuik_fscache_put(Cuik_FileCache* restrict c, const char* filepath, const TokenStream* tokens);
CUIK_API bool cuik_fscache_lookup(Cuik_FileCache* restrict c, const char* filepath, TokenStream* out_tokens);
CUIK_API bool cuik_fscache_query(Cuik_FileCache* restrict c, const char* filepath);

// simplifies whitespace for the lexer
CUIK_API void cuiklex_canonicalize(size_t length, char* data);

// filepath is just annotated in the token stream and does not access the file system.
// the contents string is a Cstring with a "fat" null terminator (16 bytes long of zeroes).
// NOTE: contents must have been canonicalized using cuiklex_canonicalize before being fed
// into here
CUIK_API TokenStream cuiklex_buffer(const char* filepath, const char* contents);

CUIK_API void cuikpp_init(Cuik_CPP* ctx, const char filepath[FILENAME_MAX]);

// this will delete all the contents of the preprocessor
//
// NOTES: it doesn't own the memory for the files it may have used
// and thus you must free them, this can be done by iterating over
// them using CUIKPP_FOR_FILES
CUIK_API void cuikpp_deinit(Cuik_CPP* ctx);
CUIK_API void cuikpp_dump(Cuik_CPP* ctx);

// You can't preprocess any more files after this
CUIK_API void cuikpp_finalize(Cuik_CPP* ctx);

// returns the final token stream (should not be called if you haven't finished
// iterating through cuikpp_next)
CUIK_API TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx);

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
CUIK_API void cuikpp_define_empty_slice(Cuik_CPP* ctx, size_t keylen, const char key[]);

// Basically just `#define key value`
CUIK_API void cuikpp_define(Cuik_CPP* ctx, const char key[], const char value[]);
CUIK_API void cuikpp_define_slice(Cuik_CPP* ctx, size_t keylen, const char key[], size_t vallen, const char value[]);

// This is written out by cuikpp_next
typedef struct Cuikpp_Packet {
    enum {
        CUIKPP_PACKET_NONE,
        CUIKPP_PACKET_GET_FILE,
        CUIKPP_PACKET_QUERY_FILE,
        CUIKPP_PACKET_CANONICALIZE,
    } tag;
    union {
        // in case of GET_FILE:
        //   lex the file at input_path using cuiklex_buffer(...)
        struct {
            // input
            const char* input_path;
            bool is_primary;

            // output
            TokenStream tokens;
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

// Used by cuikpp_default_packet_handler, it canonicalizes paths according to the OS
// NOTE: it doesn't guarentee the paths map to existing files.
//
// returns true on success
CUIK_API bool cuik_canonicalize_path(char output[FILENAME_MAX], const char* input);

// Iterates through all the cuikpp_next calls using cuikpp_default_packet_handler
// and returns the final status. if cache is NULL then it's unused.
CUIK_API Cuikpp_Status cuikpp_default_run(Cuik_CPP* ctx, Cuik_FileCache* cache);

// Keep iterating through this and filling in the packets accordingly to preprocess a file.
// returns CUIKPP_CONTINUE if it needs to keep running
CUIK_API Cuikpp_Status cuikpp_next(Cuik_CPP* ctx, Cuikpp_Packet* packet);

// Handles the default behavior of the packet written by cuikpp_next, if cache is NULL then
// it's unused.
//
// returns true if it succeeded in whatever packet handling (loading the file correctly)
CUIK_API bool cuikpp_default_packet_handler(Cuik_CPP* ctx, Cuikpp_Packet* packet, Cuik_FileCache* cache);

// if target is non-NULL it'll add predefined macros based on the target.
CUIK_API void cuikpp_set_common_defines(Cuik_CPP* restrict out_cpp, const Cuik_Target* target, bool use_system_includes);

// is the source location in the source file (none of the includes)
CUIK_API bool cuikpp_is_in_main_file(TokenStream* tokens, SourceLocIndex loc);

CUIK_API const char* cuikpp_get_main_file(TokenStream* tokens);

// This is an iterator for include search list in the preprocessor:
//
// Cuik_FileIter it = cuikpp_first_file(cpp);
// while (cuikpp_next_file(cpp, &it)) { ... }
typedef struct Cuik_FileIter {
    // public
    Cuik_FileEntry* file;

    // internal
    size_t i;
} Cuik_FileIter;

#define CUIKPP_FOR_FILES(it, ctx) \
for (Cuik_FileIter it = cuikpp_first_file(ctx); cuikpp_next_file(ctx, &it);)

CUIK_API Cuik_FileIter cuikpp_first_file(Cuik_CPP* ctx);
CUIK_API bool cuikpp_next_file(Cuik_CPP* ctx, Cuik_FileIter* it);

// This is an iterator for include search list in the preprocessor:
//
// Cuik_IncludeIter it = cuikpp_first_include_search(cpp);
// while (cuikpp_next_include_search(cpp, &it)) { ... }
typedef struct Cuik_IncludeIter {
    // public
    const char* directory;

    // internal
    size_t i;
} Cuik_IncludeIter;

#define CUIKPP_FOR_INCLUDE_SEARCHES(it, ctx) \
for (Cuik_IncludeIter it = cuikpp_first_include_search(ctx); cuikpp_next_include_search(ctx, &it);)

CUIK_API Cuik_IncludeIter cuikpp_first_include_search(Cuik_CPP* ctx);
CUIK_API bool cuikpp_next_include_search(Cuik_CPP* ctx, Cuik_IncludeIter* it);

// Used to make iterators for the define list, for example:
//
// Cuik_DefineIter it = cuikpp_first_define(cpp);
// while (cuikpp_next_define(cpp, &it)) { ... }
typedef struct Cuik_DefineIter {
    // public
    SourceLocIndex loc;
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

#define CUIKPP_FOR_DEFINES(it, ctx) \
for (Cuik_DefineIter it = cuikpp_first_define(ctx); cuikpp_next_define(ctx, &it);)

CUIK_API Cuik_DefineIter cuikpp_first_define(Cuik_CPP* ctx);
CUIK_API bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* src);

////////////////////////////////////////////
// C parsing
////////////////////////////////////////////
typedef enum Cuik_Entrypoint {
    CUIK_ENTRYPOINT_NONE,

    CUIK_ENTRYPOINT_MAIN,
    CUIK_ENTRYPOINT_WINMAIN,

    CUIK_ENTRYPOINT_CUSTOM
} Cuik_Entrypoint;

typedef enum Cuik_IntSuffix {
    //                u   l   l
    INT_SUFFIX_NONE = 0 + 0 + 0,
    INT_SUFFIX_U    = 1 + 0 + 0,
    INT_SUFFIX_L    = 0 + 2 + 0,
    INT_SUFFIX_UL   = 1 + 2 + 0,
    INT_SUFFIX_LL   = 0 + 2 + 2,
    INT_SUFFIX_ULL  = 1 + 2 + 2,
} Cuik_IntSuffix;

typedef enum Cuik_ReportLevel {
    REPORT_VERBOSE,
    REPORT_INFO,
    REPORT_WARNING,
    REPORT_ERROR,
    REPORT_MAX
} Cuik_ReportLevel;

typedef struct Cuik_Warnings {
    // implicitly converting between types and losing information
    bool data_loss : 1;
    bool unused_decls : 1;
    bool unused_funcs : 1;
} Cuik_Warnings;

typedef struct Cuik_ErrorStatus {
    int tally[REPORT_MAX];
} Cuik_ErrorStatus;

typedef char* Atom;
typedef struct Cuik_Type Cuik_Type;

typedef struct Cuik_Attribute {
    struct Cuik_Attribute* prev;
    SourceLocIndex start_loc, end_loc;

    Atom name;
    // TODO(NeGate): implement parameter list
} Cuik_Attribute;

// used to initialize translation units with cuik_parse_translation_unit
typedef struct Cuik_TranslationUnitDesc {
    // tokens CANNOT be NULL
    TokenStream* tokens;

    // errors CANNOT be NULL
    Cuik_ErrorStatus* errors;

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
    const Cuik_Target* target;

    // if thread_pool is NULL, parsing is single threaded
    Cuik_IThreadpool* thread_pool;
} Cuik_TranslationUnitDesc;

CUIK_API TranslationUnit* cuik_parse_translation_unit(const Cuik_TranslationUnitDesc* restrict desc);

// sets the user data field, returns the old value
CUIK_API void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud);

// returns user data field
CUIK_API void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu);

// add a new reference to the TU, returns the old ref count
CUIK_API int cuik_acquire_translation_unit(TranslationUnit* restrict tu);

// remove a reference from the TU, if it reaches 0 it'll destroy the TU
CUIK_API void cuik_release_translation_unit(TranslationUnit* restrict tu);

// force delete the translation unit regardless of references held
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
#ifdef CUIK_USE_TB
// Generates TBIR for a specific top-level statement, returns a pointer to the TB_Function
// it just generated such that a user could do TB related operations on it
CUIK_API TB_Module* cuik_get_tb_module(TranslationUnit* restrict tu);
CUIK_API TB_Function* cuik_stmt_gen_ir(TranslationUnit* restrict tu, Stmt* restrict s);
#endif

////////////////////////////////////////////
// Translation unit management
////////////////////////////////////////////
typedef struct {
    // public
    Expr* expr;

    // internal
    size_t index_;
    Expr* parent_;
} Cuik_ExprIter;

typedef struct {
    // public
    Stmt* stmt;

    // internal
    size_t index_;
    Stmt* parent_;
} Cuik_StmtIter;

typedef struct {
    // public
    Stmt** start;
    size_t count;

    // internal
    size_t index_;
    size_t limit_;
    Stmt** stmts_;
} Cuik_TopLevelIter;

#define CUIK_FOR_KID_IN_STMT(it, parent_) \
for (Cuik_StmtIter it = { .parent = (parent_) }; cuik_next_stmt_kid(&it);)

#define CUIK_FOR_KID_IN_EXPR(it, parent_) \
for (Cuik_ExprIter it = { .parent = (parent_) }; cuik_next_expr_kid(&it);)

#define CUIK_FOR_TOP_LEVEL_STMT(it, tu_, step_) \
for (Cuik_TopLevelIter it = cuik_first_top_level_stmt(tu_); cuik_next_top_level_stmt(&it, step_);)

CUIK_API bool cuik_next_expr_kid(Cuik_ExprIter* it);
CUIK_API bool cuik_next_stmt_kid(Cuik_StmtIter* it);

CUIK_API Cuik_TopLevelIter cuik_first_top_level_stmt(TranslationUnit* restrict tu);
CUIK_API bool cuik_next_top_level_stmt(Cuik_TopLevelIter* iter, int step);

CUIK_API Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu);
CUIK_API size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu);

CUIK_API void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist);

// if the translation units are in a compilation unit you can walk this chain of pointers
// to read them
CUIK_API TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu);

// does this translation unit have a main? what type?
CUIK_API Cuik_Entrypoint cuik_get_entrypoint_status(TranslationUnit* restrict tu);

CUIK_API TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
#define FOR_EACH_TU(it, cu) for (TranslationUnit* it = (cu)->head; it; it = cuik_next_translation_unit(it))

CUIK_API void cuik_create_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_lock_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_unlock_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu);
CUIK_API size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu);

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

void cuiklink_subsystem_windows(Cuik_Linker* l);

// Calls the system linker
// return true if it succeeds
bool cuiklink_invoke(Cuik_Linker* l, const char* filename, const char* crt_name);

#include "cuik_private.h"
