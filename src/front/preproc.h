#pragma once
#include <common.h>
#include "lexer.h"

#define SLOTS_PER_MACRO_BUCKET 32768
#define MACRO_BUCKET_COUNT 32

#define THE_SHTUFFS_SIZE (32 << 20)

////////////////////////////////
// Simple tutorial:
////////////////////////////////
//
// CPP_Context ctx;
// cpp_init(&ctx);
// cpp_add_include_directory(&ctx, "some/folder/");
//
// cpp_define(&ctx, "SOME_DEFINE", "10");
// cpp_define_empty(&ctx, "SOME_DEFINE");
// 
// TokenStream tokens = cpp_process(&ctx, "a.txt");
//
// cpp_finalize(&ctx);
//
// ... use tokens ...
//
// cpp_deinit(&ctx);

////////////////////////////////
// Public interface:
////////////////////////////////
typedef struct CPP_Context CPP_Context;

// NOTE: If you're going to preprocess isolated files
// then you should create multiple of these but if you
// want to share defines between them (like a weird unity
// build) then create one CPP_Context and cpp_process()
// multiple files.
void cpp_init(CPP_Context* ctx);
void cpp_deinit(CPP_Context* ctx);

// After finalize you can't preprocess any more files because the
// resources were freed
void cpp_finalize(CPP_Context* ctx);

void cpp_add_include_directory(CPP_Context* ctx, const char dir[]);
void cpp_define_empty(CPP_Context* ctx, const char key[]);
void cpp_define(CPP_Context* ctx, const char key[], const char value[]);
void cpp_dump(CPP_Context* ctx);

TokenStream cpp_process(CPP_Context* ctx, const char filepath[]);

////////////////////////////////
// Private definitions
////////////////////////////////
// I'd recommend not messing with the internals
// here...
typedef struct PragmaOnceEntry {
	char* key;
	int value;
} PragmaOnceEntry;

typedef struct CPP_IncludeOnce {
	char* key;
	int value;
} CPP_IncludeOnce;

enum { CPP_MAX_SCOPE_DEPTH = 1024 };

struct CPP_Context {
	// used to store macro expansion results
	size_t the_shtuffs_size;
	unsigned char* the_shtuffs;
	
	// hashmap
	CPP_IncludeOnce* include_once;
	
	// system libraries
	char** system_include_dirs;
	
	// how deep into directive scopes (#if, #ifndef, #ifdef) is it
	int depth;
	
	// TODO(NeGate): Remove this and put a proper hash map or literally anything else
	const unsigned char** macro_bucket_keys;
	size_t* macro_bucket_keys_length;
	const unsigned char** macro_bucket_values_start;
	const unsigned char** macro_bucket_values_end;
	int macro_bucket_count[MACRO_BUCKET_COUNT];
	
	// tells you if the current scope has had an entry evaluated,
	// this is important for choosing when to check #elif and #endif
	int scope_eval[CPP_MAX_SCOPE_DEPTH];
};
