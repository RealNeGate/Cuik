////////////////////////////////
// C parser
////////////////////////////////
// This module can parse C code (currently C11 with Microsoft and GNU extensions)
#pragma once
#include "cuik_prelude.h"

struct Cuik_Toolchain {
    // we expect this to be heap allocated because cuik_toolchain_free
    void* ctx;

    void(*add_libraries)(void* ctx, const Cuik_CompilerArgs* args, Cuik_Linker* linker);
    void(*set_preprocessor)(void* ctx, const Cuik_CompilerArgs* args, Cuik_CPP* cpp);

    bool(*invoke_link)(void* ctx, const Cuik_CompilerArgs* args, Cuik_Linker* linker, const char* filename);
};

struct Cuik_CompilerArgs {
    Cuik_ParseVersion version;
    TB_OutputFlavor flavor;

    Cuik_Target* target;
    Cuik_Toolchain toolchain;

    int threads, opt_level;
    const char* output_name;
    // this is the freestanding includes folder
    const char* core_dirpath;

    DynArray(const char*) sources;
    DynArray(const char*) includes;
    DynArray(const char*) libraries;
    DynArray(const char*) defines;

    bool ir            : 1;
    bool ast           : 1;
    bool types         : 1;
    bool run           : 1;
    bool bake          : 1;
    bool nocrt         : 1;
    bool pprepl        : 1;
    bool live          : 1;
    bool time          : 1;
    bool verbose       : 1;
    bool syntax_only   : 1;
    bool test_preproc  : 1;
    bool debug_info    : 1;
    bool preprocess    : 1;
    bool think         : 1;
    bool based         : 1;
};

// consumes text argument and fills the relevant values in the Cuik_CompilerArgs.
// unless you need to introduce custom arguments, it's recommended to use cuik_parse_args.
CUIK_API int cuik_parse_arg(Cuik_CompilerArgs* args, int argc, const char* argv[]);
// parses all arguments
CUIK_API void cuik_parse_args(Cuik_CompilerArgs* args, int argc, const char* argv[]);
// without an extension
CUIK_API bool cuik_driver_get_output_name(Cuik_CompilerArgs* args, int len, char path[]);

// if target is non-NULL it'll add predefined macros based on the target.
CUIK_API void cuik_set_standard_defines(Cuik_CPP* cpp, const Cuik_CompilerArgs* args);

CUIK_API Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_CompilerArgs* args, bool should_finalize);
CUIK_API int cuik_driver_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, bool destroy_cu_after_ir);

CUIK_API Cuik_Toolchain cuik_toolchain_host(void);

#ifdef _WIN32
CUIK_API Cuik_Toolchain cuik_toolchain_msvc(void);
#endif

CUIK_API void cuik_toolchain_free(Cuik_Toolchain* toolchain);