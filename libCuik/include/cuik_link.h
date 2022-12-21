#pragma once
#include "cuik_prelude.h"

////////////////////////////////////////////
// Linker
////////////////////////////////////////////
// This is a wrapper over the system linker, eventually TB will be capable of
// this job but until then...
typedef struct Cuik_Linker Cuik_Linker;

// True if success
CUIK_API bool cuiklink_init(Cuik_Linker* l);
CUIK_API void cuiklink_deinit(Cuik_Linker* l);

// uses the system library paths located by cuik_find_system_deps
CUIK_API void cuiklink_add_default_libpaths(Cuik_Linker* l);

// Adds a directory to the library searches
CUIK_API void cuiklink_add_libpath(Cuik_Linker* l, const char* filepath);

#if _WIN32
// Windows native strings are UTF-16 so i provide an option for that if you want
CUIK_API void cuiklink_add_libpath_wide(Cuik_Linker* l, const wchar_t* filepath);
#endif

// This can be a static library or object file
CUIK_API void cuiklink_add_input_file(Cuik_Linker* l, const char* filepath);

CUIK_API void cuiklink_subsystem_windows(Cuik_Linker* l);

// Calls the system linker
// return true if it succeeds
CUIK_API bool cuiklink_invoke(Cuik_Linker* l, const char* filename, const char* crt_name);
