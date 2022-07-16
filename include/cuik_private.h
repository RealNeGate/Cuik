// I'd recommend not messing with the internals
// here...
#define SLOTS_PER_MACRO_BUCKET 1024
#define MACRO_BUCKET_COUNT 1024

#define THE_SHTUFFS_SIZE (16 << 20)
#define CUIK__CPP_STATS 0

typedef struct Token {
    int type /* TknType but GCC doesn't like incomplete enums */;
    SourceLocIndex location;
    const unsigned char* start;
    const unsigned char* end;
} Token;

typedef struct PragmaOnceEntry {
    char* key;
    int value;
} PragmaOnceEntry;

typedef struct IncludeOnceEntry {
    char* key;
    int value;
} IncludeOnceEntry;

enum { CPP_MAX_SCOPE_DEPTH = 4096 };

struct Cuik_CPP {
    // used to store macro expansion results
    size_t the_shtuffs_size;
    unsigned char* the_shtuffs;

    const Cuik_IFileSystem* file_system;

    // stats
    #if CUIK__CPP_STATS
    uint64_t total_fstats;
    uint64_t total_include_time;
    uint64_t total_files_read;
    uint64_t total_io_time;
    #endif

    // stb_ds hashmap
    IncludeOnceEntry* include_once;

    // system libraries
    char** system_include_dirs;

    // DynArray(Cuik_FileEntry)
    Cuik_FileEntry* files;

    // how deep into directive scopes (#if, #ifndef, #ifdef) is it
    int depth;
    struct SourceLine* current_source_line;

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

struct TokenStream {
    const char* filepath;

    // stb_ds array
    struct Token* tokens;
    size_t current;

    // stb_ds array
    struct SourceLoc* locations;
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

typedef struct {
    Atom key;
    Stmt* value;
} ExportedSymbolEntry;

struct CompilationUnit {
    // avoid exposing the mtx_t since it's messy
    void* lock;

    // anything extern might map to a different translation unit within
    // the same compilation unit which means it's not technically external
    ExportedSymbolEntry* export_table;

    // linked list of all TUs referenced
    TranslationUnit* head;
    TranslationUnit* tail;
};
