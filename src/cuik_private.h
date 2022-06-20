// I'd recommend not messing with the internals
// here...
#include "big_array.h"

#define SLOTS_PER_MACRO_BUCKET 1024
#define MACRO_BUCKET_COUNT 1024

#define THE_SHTUFFS_SIZE (16 << 20)

typedef enum TknType TknType;

typedef struct Token {
    TknType type;
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

    // hashmap
    IncludeOnceEntry* include_once;

    // system libraries
    char** system_include_dirs;

    BigArray(Cuik_FileEntry) files;

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

#define SOURCE_LOC_GET_DATA(loc) ((loc) & ~0xC0000000u)
#define SOURCE_LOC_GET_TYPE(loc) (((loc) & 0xC0000000u) >> 30u)
#define SOURCE_LOC_SET_TYPE(type, raw) (((type << 30) & 0xC0000000u) | ((raw) & ~0xC0000000u))

typedef enum SourceLocType {
    SOURCE_LOC_UNKNOWN = 0,
    SOURCE_LOC_NORMAL = 1,
    SOURCE_LOC_MACRO = 2,
    SOURCE_LOC_FILE = 3
} SourceLocType;

typedef struct SourceRange {
    SourceLocIndex start, end;
} SourceRange;

typedef struct SourceLine {
    const char* filepath;
    const unsigned char* line_str;
    SourceLocIndex parent;
    int line;
} SourceLine;

typedef struct SourceLoc {
    SourceLine* line;
    short columns;
    short length;
    int bias;
} SourceLoc;
