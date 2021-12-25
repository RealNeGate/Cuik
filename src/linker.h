#pragma once
#include "common.h"

#if _WIN32
#include "microsoft_craziness.h"
#endif

////////////////////////////////
// Public interface:
////////////////////////////////
// All strings you might put in should be UTF-8
typedef struct Linker Linker;

void linker_init(Linker* l);
void linker_deinit(Linker* l);

// This just locates the system libraries and adds them
void linker_add_default_libpaths(Linker* l);

// Adds a directory to the library searches
void linker_add_libpath(Linker* l, const char filepath[]);

#if _WIN32
// Windows native strings are UTF-16 so i provide an option for that if you
// want.
void linker_add_libpath_wide(Linker* l, const wchar_t filepath[]);
#endif

// This can be a static library or object file
void linker_add_input_file(Linker* l, const char filename[]);

// Calls the system linker
// return true if it succeeds
bool linker_invoke(Linker* l, const char filename[], bool linked_with_crt);

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
	
	// NOTE(NeGate): Maybe we should move this out because it's can
	// be shared across Linker objects... but it doesn't really matters
	// for our use cases.
#if _WIN32
	MicrosoftCraziness_Find_Result vswhere;
#endif
};
