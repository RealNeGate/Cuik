// I'd recommend not messing with the internals
// here...
#define SLOTS_PER_MACRO_BUCKET 1024
#define MACRO_BUCKET_COUNT 1024

#define THE_SHTUFFS_SIZE (32 << 20)
#define CUIK__CPP_STATS 0

typedef struct PragmaOnceEntry {
    char* key;
    int value;
} PragmaOnceEntry;

enum {
    CPP_MAX_SCOPE_DEPTH = 4096,
};

struct Cuik_CPP {
    // used to store macro expansion results
    size_t the_shtuffs_size;
    unsigned char* the_shtuffs;

    TokenStream tokens;
    TokenList scratch_list;

    // powers __COUNTER__
    int unique_counter;
    bool included_system_header;

    // we got a little state machine design
    // to emulate some bootleg coroutines :P
    enum {
        CUIK__CPP_NONE,
        CUIK__CPP_FIRST_FILE,
        CUIK__CPP_LIB_INCLUDE, // <foo.h>
        CUIK__CPP_USR_INCLUDE, // "bar.h"
        CUIK__CPP_CANONICALIZE,
        CUIK__CPP_GET_FILE,
    } state1;
    int state2;

    // preprocessor stack
    int stack_ptr;
    struct CPPStackSlot* stack;

    // stats
    #if CUIK__CPP_STATS
    uint64_t total_lex_time;
    uint64_t total_fstats;
    uint64_t total_include_time;
    uint64_t total_files_read;
    uint64_t total_io_time;

    // define table
    uint64_t total_define_access_time;
    uint64_t total_define_accesses;
    #endif

    // NL_Strmap(int)
    int* include_once;

    // system libraries
    // DynArray(Cuik_IncludeDir)
    Cuik_IncludeDir* system_include_dirs;

    // how deep into directive scopes (#if, #ifndef, #ifdef) is it
    int depth;

    // TODO(NeGate): Remove this and put a proper hash map or literally anything else PLEASE
    const unsigned char** macro_bucket_keys;
    size_t* macro_bucket_keys_length;
    const unsigned char** macro_bucket_values_start;
    const unsigned char** macro_bucket_values_end;
    SourceLoc* macro_bucket_source_locs;
    int macro_bucket_count[MACRO_BUCKET_COUNT];

    // tells you if the current scope has had an entry evaluated,
    // this is important for choosing when to check #elif and #endif
    struct Cuikpp_ScopeEval {
        SourceLoc start;
        bool value;
    } scope_eval[CPP_MAX_SCOPE_DEPTH];
};

struct Cuik_Linker {
    bool subsystem_windows;

    // translation units
    #ifdef _WIN32
    wchar_t* input_file_buffer;
    #else
    char* input_file_buffer;
    #endif

    size_t input_file_top;
    size_t input_file_count;

    // system libraries
    #ifdef _WIN32
    wchar_t* libpaths_buffer;
    #else
    char* libpaths_buffer;
    #endif
    size_t libpaths_top;
    size_t libpaths_count;
};

struct CompilationUnit {
    // avoid exposing the mtx_t since it's messy
    void* lock;
    size_t count;

    // NL_Strmap but like without the macro wrapping
    Stmt** export_table;

    // linked list of all TUs referenced
    TranslationUnit* head;
    TranslationUnit* tail;
};
