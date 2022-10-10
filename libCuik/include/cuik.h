#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "cuik_lex.h"

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

#ifdef CUIK_USE_TB
#include "cuik_irgen.h"
#endif

// This is generated from
//    #pragma comment(lib, "somelib.lib")
typedef struct Cuik_ImportRequest {
    struct Cuik_ImportRequest* next;
    const char* lib_name;
} Cuik_ImportRequest;

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
void cuik_start_global_profiler(const Cuik_IProfiler* profiler, bool lock_on_plot);
void cuik_stop_global_profiler(void);
bool cuik_is_profiling(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
uint64_t cuik_time_in_nanos(void);

// Reports a region of time to the profiler callback
void cuik_profile_region_start(uint64_t now, const char* fmt, ...);
void cuik_profile_region_end(void);

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

void cuik_init(void);

// This should be called before exiting
void cuik_free_thread_resources(void);

// locates the system includes, libraries and other tools. this is a global
// operation meaning that once it's only done once for the process.
void cuik_find_system_deps(const char* cuik_crt_directory);

// can only be called after cuik_find_system_deps
size_t cuik_get_system_search_path_count(void);
void cuik_get_system_search_paths(const char** out, size_t n);

bool cuik_lex_is_keyword(size_t length, const char* str);

////////////////////////////////////////////
// C preprocessor
////////////////////////////////////////////
typedef struct Cuik_FileCache Cuik_FileCache;

Cuik_FileCache* cuik_fscache_create(void);
void cuik_fscache_destroy(Cuik_FileCache* restrict c);
void cuik_fscache_put(Cuik_FileCache* restrict c, const char* filepath, const TokenStream* tokens);
bool cuik_fscache_lookup(Cuik_FileCache* restrict c, const char* filepath, TokenStream* out_tokens);
bool cuik_fscache_query(Cuik_FileCache* restrict c, const char* filepath);

// simplifies whitespace for the lexer
void cuiklex_canonicalize(size_t length, char* data);

// filepath is just annotated in the token stream and does not access the file system.
// the contents string is a Cstring with a "fat" null terminator (16 bytes long of zeroes).
// NOTE: contents must have been canonicalized using cuiklex_canonicalize before being fed
// into here
TokenStream cuiklex_buffer(const char* filepath, char* contents);


// Used by cuikpp_default_packet_handler, it canonicalizes paths according to the OS
// NOTE: it doesn't guarentee the paths map to existing files.
//
// returns true on success
bool cuik_canonicalize_path(char output[FILENAME_MAX], const char* input);

// Iterates through all the cuikpp_next calls using cuikpp_default_packet_handler
// and returns the final status. if cache is NULL then it's unused.
Cuikpp_Status cuikpp_default_run(Cuik_CPP* ctx, Cuik_FileCache* cache);

// Keep iterating through this and filling in the packets accordingly to preprocess a file.
// returns CUIKPP_CONTINUE if it needs to keep running
Cuikpp_Status cuikpp_next(Cuik_CPP* ctx, Cuikpp_Packet* packet);

// Handles the default behavior of the packet written by cuikpp_next, if cache is NULL then
// it's unused.
//
// returns true if it succeeded in whatever packet handling (loading the file correctly)
bool cuikpp_default_packet_handler(Cuik_CPP* ctx, Cuikpp_Packet* packet, Cuik_FileCache* cache);

// if target is non-NULL it'll add predefined macros based on the target.
void cuikpp_set_common_defines(Cuik_CPP* restrict out_cpp, const Cuik_Target* target, bool use_system_includes);

// is the source location in the source file (none of the includes)
bool cuikpp_is_in_main_file(TokenStream* tokens, SourceLoc loc);

const char* cuikpp_get_main_file(TokenStream* tokens);

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

Cuik_IncludeIter cuikpp_first_include_search(Cuik_CPP* ctx);
bool cuikpp_next_include_search(Cuik_CPP* ctx, Cuik_IncludeIter* it);

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
    SourceRange loc;

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

TranslationUnit* cuik_parse_translation_unit(const Cuik_TranslationUnitDesc* restrict desc);

// sets the user data field, returns the old value
void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud);

// returns user data field
void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu);

// add a new reference to the TU, returns the old ref count
int cuik_acquire_translation_unit(TranslationUnit* restrict tu);

// remove a reference from the TU, if it reaches 0 it'll destroy the TU
void cuik_release_translation_unit(TranslationUnit* restrict tu);

// force delete the translation unit regardless of references held
void cuik_destroy_translation_unit(TranslationUnit* restrict tu);

Cuik_ImportRequest* cuik_translation_unit_import_requests(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Token stream
////////////////////////////////////////////
Token* cuik_get_tokens(TokenStream* restrict s);
size_t cuik_get_token_count(TokenStream* restrict s);

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

bool cuik_next_expr_kid(Cuik_ExprIter* it);
bool cuik_next_stmt_kid(Cuik_StmtIter* it);

Cuik_TopLevelIter cuik_first_top_level_stmt(TranslationUnit* restrict tu);
bool cuik_next_top_level_stmt(Cuik_TopLevelIter* iter, int step);

Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu);
size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu);

void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist);

// if the translation units are in a compilation unit you can walk this chain of pointers
// to read them
TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu);

// does this translation unit have a main? what type?
Cuik_Entrypoint cuik_get_entrypoint_status(TranslationUnit* restrict tu);

TokenStream* cuik_get_token_stream_from_tu(TranslationUnit* restrict tu);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
#define FOR_EACH_TU(it, cu) for (TranslationUnit* it = (cu)->head; it; it = cuik_next_translation_unit(it))

void cuik_create_compilation_unit(CompilationUnit* restrict cu);
void cuik_lock_compilation_unit(CompilationUnit* restrict cu);
void cuik_unlock_compilation_unit(CompilationUnit* restrict cu);
void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu);
size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu);

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
