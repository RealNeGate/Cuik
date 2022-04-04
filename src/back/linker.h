#pragma once
#include <common.h>

#if _WIN32
#include "microsoft_craziness.h"

extern MicrosoftCraziness_Find_Result s_vswhere;
#endif

////////////////////////////////
// Public interface:
////////////////////////////////
// All strings you might put in should be UTF-8
typedef struct Linker Linker;

// True if success
bool linker_init(Linker* l);
void linker_deinit(Linker* l);

// This just locates the system libraries and adds them
void linker_add_default_libpaths(Linker* l);

// Adds a directory to the library searches
void linker_add_libpath(Linker* l, const char* filepath);

#if _WIN32
// Windows native strings are UTF-16 so i provide an option for that if you
// want.
void linker_add_libpath_wide(Linker* l, const wchar_t* filepath);
#endif

// This can be a static library or object file
void linker_add_input_file(Linker* l, const char* filepath);

// Calls the system linker
// return true if it succeeds
bool linker_invoke_system(Linker* l, const char* filename);

// Calls the internal cuik linker
// returns true if it succeeds
bool linker_invoke_tb(Linker* l, const char* filename);

////////////////////////////////
// Private definitions
////////////////////////////////
// I'd recommend not messing with the internals
// here...
struct Linker {
	// translation units
	OS_String input_file_buffer;
	size_t input_file_top;
	size_t input_file_count;
	
	// system libraries
	OS_String libpaths_buffer;
	size_t libpaths_top;
	size_t libpaths_count;
};
