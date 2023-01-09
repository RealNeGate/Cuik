////////////////////////////////
// C parser
////////////////////////////////
// This module can parse C code (currently C11 with Microsoft and GNU extensions)
#pragma once
#include "cuik_prelude.h"

typedef struct Cuik_CompilerArgs {
    // Boolean flags
    bool ir            : 1;
    bool ast           : 1;
    bool types         : 1;
    bool run           : 1;
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

    int threads;
    int opt_level;
    Cuik_ParseVersion version;
    TB_OutputFlavor flavor;
    Cuik_Target* target;

    const char* output_name;
    const char* crt_dirpath;

    DynArray(const char*) sources;
    DynArray(const char*) includes;
    DynArray(const char*) libraries;
    DynArray(const char*) defines;
} Cuik_CompilerArgs;

// consumes text argument and fills the relevant values in the Cuik_CompilerArgs.
// unless you need to introduce custom arguments, it's recommended to use cuik_parse_args.
int cuik_parse_arg(Cuik_CompilerArgs* args, int argc, const char* argv[]);

// parses all arguments
void cuik_parse_args(Cuik_CompilerArgs* args, int argc, const char* argv[]);

Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_CompilerArgs* args, bool should_finalize);
int cuik_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, bool destroy_cu_after_ir);
