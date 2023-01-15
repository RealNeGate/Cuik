#pragma once
#include "cuik_prelude.h"

////////////////////////////////////////////
// Linker
////////////////////////////////////////////
// This is a wrapper over the system linker, eventually TB will be capable of
// this job but until then...
typedef struct Cuik_Linker {
    bool subsystem_windows;

    DynArray(char*) inputs;
    DynArray(char*) libpaths;
} Cuik_Linker;

// True if success
CUIK_API bool cuiklink_init(Cuik_Linker* l);
CUIK_API void cuiklink_deinit(Cuik_Linker* l);

// uses the system library paths located by cuik_find_system_deps
CUIK_API void cuiklink_apply_toolchain_libs(Cuik_Linker* l, Cuik_CompilerArgs* args);

// Adds a directory to the library searches
CUIK_API void cuiklink_add_libpath(Cuik_Linker* l, const char* filepath);
CUIK_API void cuiklink_add_libpathf(Cuik_Linker* l, const char* fmt, ...);

// This can be a static library or object file
CUIK_API void cuiklink_add_input_file(Cuik_Linker* l, const char* filepath);

// return true if it succeeds
CUIK_API bool cuiklink_invoke(Cuik_Linker* l, Cuik_CompilerArgs* args, const char* filename);
