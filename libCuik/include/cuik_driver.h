////////////////////////////////
// C parser
////////////////////////////////
// This module can parse C code (currently C11 with Microsoft and GNU extensions)
#pragma once
#include "cuik_prelude.h"

struct Cuik_Toolchain {
    // we expect this to be heap allocated because cuik_toolchain_free
    void* ctx;

    void(*add_libraries)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker);
    void(*set_preprocessor)(void* ctx, const Cuik_DriverArgs* args, Cuik_CPP* cpp);

    bool(*invoke_link)(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename);
};

struct Cuik_DriverArgs {
    Cuik_ParseVersion version;
    TB_OutputFlavor flavor;

    Cuik_Target* target;
    Cuik_Toolchain toolchain;

    int threads, opt_level;
    const char* output_name;

    void* diag_userdata;
    Cuik_DiagCallback diag_callback;

    DynArray(char*) sources;
    DynArray(char*) includes;
    DynArray(char*) libraries;
    DynArray(char*) defines;

    bool ir              : 1;
    bool ast             : 1;
    bool types           : 1;
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
    bool silent_progress : 1;
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
CUIK_API CompilationUnit* cuik_driver_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_DriverArgs* restrict args, bool destroy_cu_after_ir);

#ifdef CUIK_USE_TB
CUIK_API void cuik_apply_tb_toolchain_libs(TB_Linker* l);
#endif

#ifdef _WIN32
CUIK_API Cuik_Toolchain cuik_toolchain_msvc(void);
#elif __APPLE__
CUIK_API Cuik_Toolchain cuik_toolchain_darwin(void);
#endif

CUIK_API Cuik_Toolchain cuik_toolchain_host(void);
CUIK_API void cuik_toolchain_free(Cuik_Toolchain* toolchain);
