#pragma once
#include "lexer.h"
#include <big_array.h>
#include <common.h>

#define SLOTS_PER_MACRO_BUCKET 1024
#define MACRO_BUCKET_COUNT 1024

#define THE_SHTUFFS_SIZE (16 << 20)

////////////////////////////////
// Simple tutorial:
////////////////////////////////
//
// CPP_Context ctx;
// cpp_init(&ctx);
// cpp_add_include_directory(&ctx, "some/folder/");
//
// cpp_define(&ctx, "SOME_DEFINE", "10");
// cpp_define_empty(&ctx, "OTHER_DEFINE");
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

typedef struct CPP_FileEntry {
    struct CPP_FileEntry* parent;
    SourceLocIndex include_loc;

    const char* filepath;
    uint8_t* content;
} CPP_FileEntry;

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

// The file table may contain duplicates (for now...) but it stores all
// the loaded files by this instance of the preprocessor, in theory one
// could write a proper incremental compilation model using this for TU
// dependency tracking but... incremental compilation is a skill issue
size_t cpp_get_file_table_count(CPP_Context* ctx);
CPP_FileEntry* cpp_get_file_table(CPP_Context* ctx);

// After you've parsed code with the token stream, you don't generally need
// to maintain the file memory, all strings are now part of the atoms
void cpp_free_file_memory(CPP_Context* ctx);

// Locates an include file from the `path` and copies it's fully qualified path into `output`
bool cpp_find_include_include(CPP_Context* ctx, char output[MAX_PATH], const char* path);

// Adds include directory to the search list
void cpp_add_include_directory(CPP_Context* ctx, const char dir[]);

// Basically just `#define key`
void cpp_define_empty(CPP_Context* ctx, const char key[]);

// Basically just `#define key value`
void cpp_define(CPP_Context* ctx, const char key[], const char value[]);

// Dumps all the macros defined at that time
void cpp_dump(CPP_Context* ctx);

// Invokes the preprocessor
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

enum { CPP_MAX_SCOPE_DEPTH = 4096 };

struct CPP_Context {
    // used to store macro expansion results
    size_t the_shtuffs_size;
    unsigned char* the_shtuffs;

    // hashmap
    CPP_IncludeOnce* include_once;

    // system libraries
    char** system_include_dirs;

    BigArray(CPP_FileEntry) files;

    // how deep into directive scopes (#if, #ifndef, #ifdef) is it
    int depth;
    SourceLine* current_source_line;

    // TODO(NeGate): Remove this and put a proper hash map or literally anything else
    const unsigned char** macro_bucket_keys;
    size_t* macro_bucket_keys_length;
    const unsigned char** macro_bucket_values_start;
    const unsigned char** macro_bucket_values_end;
    SourceLocIndex* macro_bucket_source_locs;
    int macro_bucket_count[MACRO_BUCKET_COUNT];

    // tells you if the current scope has had an entry evaluated,
    // this is important for choosing when to check #elif and #endif
    bool scope_eval[CPP_MAX_SCOPE_DEPTH];
};
