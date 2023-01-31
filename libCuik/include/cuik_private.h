// I'd recommend not messing with the internals
// here...
#define THE_SHTUFFS_SIZE (32 << 20)
#define CUIK__CPP_STATS 0

#define MACRO_DEF_TOMBSTONE SIZE_MAX

typedef struct PragmaOnceEntry {
    char* key;
    int value;
} PragmaOnceEntry;

enum {
    CPP_MAX_SCOPE_DEPTH = 4096,
};

typedef struct {
    String value;
    SourceLoc loc;
} MacroDef;

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

    struct {
        size_t exp, len;
        String* keys;   // [1 << exp]
        MacroDef* vals; // [1 << exp]
    } macros;

    // tells you if the current scope has had an entry evaluated,
    // this is important for choosing when to check #elif and #endif
    struct Cuikpp_ScopeEval {
        SourceLoc start;
        bool value;
    } scope_eval[CPP_MAX_SCOPE_DEPTH];
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
