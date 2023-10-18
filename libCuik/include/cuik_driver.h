////////////////////////////////
// C driver
////////////////////////////////
// This is the build system like interface for Cuik
#pragma once
#include "cuik_prelude.h"

typedef struct Cuik_Linker Cuik_Linker;

struct Cuik_Toolchain {
    // we expect this to be heap allocated because cuik_toolchain_free
    void* ctx;
    bool case_insensitive;

    void* (*init)(void);
    void  (*add_libraries)(void* ctx, bool nocrt, Cuik_Linker* linker);
    void  (*set_preprocessor)(void* ctx, bool nocrt, Cuik_CPP* cpp);
    void  (*print_verbose)(void* ctx, bool nocrt);
    bool  (*invoke_link)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename);
};

struct Cuik_DriverArgs {
    Cuik_Version version;

    #ifdef CUIK_USE_TB
    TB_OutputFlavor flavor;
    #endif

    Cuik_Target* target;
    Cuik_Toolchain toolchain;

    int threads, opt_level;
    const char* output_name;
    const char* entrypoint;

    void* diag_userdata;
    Cuik_DiagCallback diag_callback;

    DynArray(Cuik_Path*) sources;
    DynArray(Cuik_Path*) includes;
    DynArray(Cuik_Path*) libraries;
    DynArray(Cuik_Path*) libpaths;
    DynArray(char*) defines;

    TB_WindowsSubsystem subsystem;

    bool emit_ir         : 1;
    bool emit_dot        : 1;
    bool assembly        : 1;
    bool ast             : 1;
    bool run             : 1;
    bool bake            : 1;
    bool nocrt           : 1;
    bool live            : 1;
    bool time            : 1;
    bool verbose         : 1;
    bool syntax_only     : 1;
    bool test_preproc    : 1;
    bool debug_info      : 1;
    bool preprocess      : 1;
    bool think           : 1;
    bool based           : 1;
    bool preserve_ast    : 1;
};

typedef struct Cuik_Arg Cuik_Arg;
struct Cuik_Arg {
    Cuik_Arg* prev;
    const char* value;
};

// represented parsed arguments, you can feed these into a Cuik_DriverArgs to
// use in compilation.
typedef struct Cuik_Arguments Cuik_Arguments;

CUIK_API Cuik_Arguments* cuik_alloc_args(void);
CUIK_API void cuik_free_args(Cuik_Arguments* args);

CUIK_API void cuik_parse_args(Cuik_Arguments* restrict args, int argc, const char* argv[]);
CUIK_API bool cuik_args_to_driver(Cuik_DriverArgs* comp_args, Cuik_Arguments* restrict args);

CUIK_API bool cuik_parse_driver_args(Cuik_DriverArgs* comp_args, int argc, const char* argv[]);
CUIK_API void cuik_free_driver_args(Cuik_DriverArgs* args);

// consumes text argument and fills the relevant values in the Cuik_DriverArgs.
// unless you need to introduce custom arguments, it's recommended to use cuik_parse_args.
CUIK_API int cuik_parse_arg(Cuik_DriverArgs* args, int argc, const char* argv[]);

CUIK_API bool cuik_driver_get_output_name(Cuik_DriverArgs* args, int len, char path[]);

// if target is non-NULL it'll add predefined macros based on the target.
CUIK_API void cuik_set_standard_defines(Cuik_CPP* cpp, const Cuik_DriverArgs* args);

CUIK_API Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_DriverArgs* args, bool should_finalize);
CUIK_API bool cuik_driver_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_DriverArgs* restrict args, bool destroy_cu_after_ir, bool destroy_ir, CompilationUnit** out_cu);

CUIK_API Cuik_CPP* cuik_driver_preprocess_str(String source, const Cuik_DriverArgs* args, bool should_finalize);
CUIK_API Cuik_CPP* cuik_driver_preprocess_cstr(const char* source, const Cuik_DriverArgs* args, bool should_finalize);

#ifdef CUIK_USE_TB
CUIK_API void cuik_apply_tb_toolchain_libs(TB_Linker* l);
#endif

CUIK_API Cuik_Toolchain cuik_toolchain_msvc(void);
CUIK_API Cuik_Toolchain cuik_toolchain_darwin(void);
CUIK_API Cuik_Toolchain cuik_toolchain_gnu(void);

CUIK_API Cuik_Toolchain cuik_toolchain_host(void);
CUIK_API void cuik_toolchain_free(Cuik_Toolchain* toolchain);

////////////////////////////////////////////
// Linker
////////////////////////////////////////////
// This is a wrapper over the system linker, eventually TB will be capable of
// this job but until then...
struct Cuik_Linker {
    Cuik_Toolchain toolchain;

    DynArray(char*) inputs;
    DynArray(char*) libpaths;
};

// True if success
CUIK_API bool cuiklink_init(Cuik_Linker* l);
CUIK_API void cuiklink_deinit(Cuik_Linker* l);

// uses the system library paths located by cuik_find_system_deps
CUIK_API void cuiklink_apply_toolchain_libs(Cuik_Linker* l, bool nocrt);

// Adds a directory to the library searches
CUIK_API void cuiklink_add_libpath(Cuik_Linker* l, const char* filepath);
CUIK_API void cuiklink_add_libpathf(Cuik_Linker* l, const char* fmt, ...);

// This can be a static library or object file
CUIK_API void cuiklink_add_input_file(Cuik_Linker* l, const char* filepath);

CUIK_API bool cuiklink_find_library(Cuik_Linker* l, char output[FILENAME_MAX], const char* filepath);

// return true if it succeeds
CUIK_API bool cuiklink_invoke(Cuik_Linker* l, Cuik_DriverArgs* args, const char* output, const char* filename);

////////////////////////////////
// Integrated build system API
////////////////////////////////
typedef struct Cuik_BuildStep Cuik_BuildStep;

// performs system command
CUIK_API Cuik_BuildStep* cuik_driver_sys(Cuik_DriverArgs* args, const char* cmd);

// generates Cuik compile for a single file
CUIK_API Cuik_BuildStep* cuik_driver_cc(Cuik_DriverArgs* args, const char* source);

// links against all the input steps (must all be TU producing)
CUIK_API Cuik_BuildStep* cuik_driver_ld(Cuik_DriverArgs* args, int dep_count, Cuik_BuildStep** deps);

CUIK_API TranslationUnit* cuik_driver_cc_get_tu(Cuik_BuildStep* s);
CUIK_API CompilationUnit* cuik_driver_ld_get_cu(Cuik_BuildStep* s);

// returns true on success
CUIK_API bool cuik_step_run(Cuik_BuildStep* s, Cuik_IThreadpool* thread_pool);

// frees s including all dependencies
CUIK_API void cuik_step_free(Cuik_BuildStep* s);

CUIK_API bool cuik_driver_does_codegen(const Cuik_DriverArgs* args);

////////////////////////////////
// Scheduling
////////////////////////////////
// This is used to help users multithread their actions (optimizations, codegen, etc)
#ifdef CUIK_USE_TB
typedef void (*CuikSched_PerFunction)(TB_Function* f, void* ctx);

CUIK_API void cuiksched_per_function(Cuik_IThreadpool* restrict thread_pool, int num_threads, TB_Module* m, void* ctx, CuikSched_PerFunction func);
#endif
