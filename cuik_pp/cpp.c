// TODO(NeGate): Refactor this code...
//
// NOTE(NeGate): This code leaks the filename strings but it doesn't actually matter
// because this is a compiler and momma aint raised no bitch.
//
// NOTE(NeGate): the_shtuffs is the simple linear allocator in this preprocessor, just avoids
// wasting time on the heap allocator
#include <cuik.h>
#include <assert.h>
#include <str.h>
#include "diagnostic.h"

#include "lexer.h"
#include <setjmp.h>
#include <sys/stat.h>
#include <hash_map.h>

#if USE_INTRIN
#include <x86intrin.h>
#endif

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
    Cuik_Version version;
    bool case_insensitive;

    // file system stuff
    Cuikpp_LocateFile locate;
    Cuikpp_GetFile fs;
    void* user_data;

    // used to store macro expansion results
    size_t the_shtuffs_size;
    unsigned char* the_shtuffs;

    TokenStream tokens;

    // powers __COUNTER__
    int unique_counter;

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

    NL_Strmap(int) include_once;

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

typedef struct TokenNode TokenNode;
struct TokenNode {
    TokenNode* next;
    Token t;
};

typedef struct {
    const uint8_t* start;
    TokenNode *head, *tail;
} TokenList;

typedef enum {
    LOCATE_FOUND  = 1,
    LOCATE_SYSTEM = 2,
} LocateResult;

// GOD I HATE FORWARD DECLARATIONS
static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length);
static void expect(TokenArray* restrict in, char ch);
static intmax_t eval(Cuik_CPP* restrict c, TokenArray* restrict in);

static void* gimme_the_shtuffs(Cuik_CPP* restrict c, size_t len);
static void* gimme_the_shtuffs_fill(Cuik_CPP* restrict c, const char* str);
static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top);

static bool push_scope(Cuik_CPP* restrict ctx, TokenArray* restrict in, bool initial);
static bool pop_scope(Cuik_CPP* restrict ctx, TokenArray* restrict in);

static void expand(Cuik_CPP* restrict c, TokenNode* restrict head, uint32_t parent_macro, TokenArray* rest);
static TokenList expand_ident(Cuik_CPP* restrict c, TokenArray* in, TokenNode* head, uint32_t parent_macro, TokenArray* rest);

static Cuik_Path* alloc_path(Cuik_CPP* restrict ctx, const char* filepath);
static Cuik_Path* alloc_directory_path(Cuik_CPP* restrict ctx, const char* filepath);
static void compute_line_map(TokenStream* s, bool is_system, int depth, SourceLoc include_site, const char* filename, char* data, size_t length);

enum {
    MAX_CPP_STACK_DEPTH = 1024,
};

typedef struct CPPStackSlot {
    Cuik_Path* filepath;
    Cuik_Path* directory;

    uint32_t file_id;
    SourceLoc loc; // location of the #include
    TokenArray tokens;

    // https://gcc.gnu.org/onlinedocs/cppinternals/Guard-Macros.html
    struct CPPIncludeGuard {
        enum {
            INCLUDE_GUARD_INVALID = -1,

            INCLUDE_GUARD_LOOKING_FOR_IFNDEF,
            INCLUDE_GUARD_LOOKING_FOR_DEFINE,
            INCLUDE_GUARD_LOOKING_FOR_ENDIF,
            INCLUDE_GUARD_EXPECTING_NOTHING,
        } status;
        int if_depth; // the depth value we expect the include guard to be at
        String define;
    } include_guard;
} CPPStackSlot;

// just the value (doesn't track the name of the parameter)
typedef struct {
    String content;
    SourceRange loc;
} MacroArg;

typedef struct {
    int key_count;
    String* keys;

    int value_count;
    MacroArg* values;

    bool has_varargs;
} MacroArgs;

static Token peek(TokenArray* restrict in) {
    return in->tokens[in->current];
}

static bool at_token_list_end(TokenArray* restrict in) {
    return in->current >= dyn_array_length(in->tokens)-1;
}

static Token consume(TokenArray* restrict in) {
    assert(in->current < dyn_array_length(in->tokens));
    return in->tokens[in->current++];
}

static void expect_from_lexer(Cuik_CPP* c, Lexer* l, char ch) {
    Token t = lexer_read(l);
    if (t.type != ch) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t.location);

        fprintf(stderr, "error %s:%d: expected '%c' got '%.*s'", r.file->filename, r.line, ch, (int) t.content.length, t.content.data);
        abort();
    }
}

static TokenArray convert_to_token_list(Cuik_CPP* restrict c, uint32_t file_id, size_t length, char* data) {
    Lexer l = {
        .file_id = file_id,
        .start = (unsigned char*) data,
        .current = (unsigned char*) data,
    };

    size_t expected = 32 + ((length + 2) / 3);
    TokenArray list = { 0 };
    list.tokens = dyn_array_create(Token, expected);

    #if 1
    DynArrayHeader* header = ((DynArrayHeader*) list.tokens) - 1;
    size_t i = 0, cap = header->capacity;
    for (;;) {
        // hot loop based on capacity
        assert(i == dyn_array_length(list.tokens));
        while (i < cap) {
            Token t = lexer_read(&l);
            if (__builtin_expect(t.type == 0, 0)) goto exit;
            list.tokens[i++] = t;
        }

        // resync & cold array resize
        header->size = i;
        list.tokens = dyn_array_internal_reserve(list.tokens, sizeof(Token), 1);
        header = ((DynArrayHeader*) list.tokens) - 1;
        cap = header->capacity;
    }

    exit:
    header->size = i;
    #else
    for (;;) {
        Token t = lexer_read(&l);
        if (t.type == 0) break;
        dyn_array_put(list.tokens, t);
    }
    #endif

    dyn_array_put(list.tokens, (Token){ 0 });
    return list;
}

static String get_token_as_string(TokenStream* restrict in) {
    return tokens_get(in)->content;
}

static LocateResult locate_file(Cuik_CPP* ctx, bool search_lib_first, const Cuik_Path* restrict dir, const char* og_path, Cuik_Path* restrict canonical) {
    size_t og_path_len = strlen(og_path);

    Cuik_Path tmp;
    if (!search_lib_first && cuik_path_append(&tmp, dir, og_path_len, og_path)) {
        #if CUIK__CPP_STATS
        ctx->total_fstats++;
        #endif

        if (ctx->locate(ctx->user_data, &tmp, canonical, ctx->case_insensitive)) {
            return LOCATE_FOUND;
        }
    }

    dyn_array_for(i, ctx->system_include_dirs) {
        if (cuik_path_append(&tmp, ctx->system_include_dirs[i].path, og_path_len, og_path)) {
            #if CUIK__CPP_STATS
            ctx->total_fstats++;
            #endif

            if (ctx->locate(ctx->user_data, &tmp, canonical, ctx->case_insensitive)) {
                return LOCATE_FOUND | (ctx->system_include_dirs[i].is_system << 1);
            }
        }
    }

    if (search_lib_first && cuik_path_append(&tmp, dir, og_path_len, og_path)) {
        #if CUIK__CPP_STATS
        ctx->total_fstats++;
        #endif

        if (ctx->locate(ctx->user_data, &tmp, canonical, ctx->case_insensitive)) {
            return LOCATE_FOUND;
        }
    }

    return false;
}

// this is used when NULL is passed into the cuikpp_make filepath, it makes it
// easy to check for empty string which aren't NULL.
static const char MAGIC_EMPTY_STRING[] = "";

// include this if you want to enable the preprocessor debugger
// #include "cpp_dbg.h"

// Basically a mini-unity build that takes up just the CPP module
#include "cpp_symtab.h"
#include "cpp_expand.h"
#include "cpp_fs.h"
#include "cpp_expr.h"
#include "cpp_directive.h"
#include "cpp_iters.h"

const char* cuikpp_get_main_file(TokenStream* tokens) {
    return tokens->filepath;
}

bool cuikpp_is_in_main_file(TokenStream* tokens, SourceLoc loc) {
    // TODO(NeGate): macros can be in the main file, we should be walking the macro
    // trace to check for that
    // assert((loc.raw & SourceLoc_IsMacro) == 0 && "TODO: support macro in cuikpp_is_in_main_file");
    if (loc.raw & SourceLoc_IsMacro) {
        return false;
    }

    Cuik_FileEntry* f = &tokens->files[loc.raw >> SourceLoc_FilePosBits];
    return f->filename == tokens->filepath;
}

Cuik_CPP* cuikpp_make(const Cuik_CPPDesc* desc) {
    const char* filepath = desc->filepath ? desc->filepath : MAGIC_EMPTY_STRING;

    Cuik_CPP* ctx = cuik_malloc(sizeof(Cuik_CPP));
    *ctx = (Cuik_CPP){
        .version   = desc->version,
        .locate    = desc->locate,
        .fs        = desc->fs,
        .user_data = desc->fs_data,
        .case_insensitive = desc->case_insensitive,

        .stack = cuik__valloc(MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot)),
        .macros = {
            .exp = 24,
            .keys = cuik__valloc((1u << 24) * sizeof(String)),
            .vals = cuik__valloc((1u << 24) * sizeof(MacroDef)),
        },
        .the_shtuffs = cuik__valloc(THE_SHTUFFS_SIZE),
    };

    // initialize dynamic arrays
    ctx->system_include_dirs = dyn_array_create(char*, 64);

    ctx->tokens.diag = cuikdg_make(desc->diag, desc->diag_data);
    ctx->tokens.filepath = filepath;
    ctx->tokens.invokes = dyn_array_create(MacroInvoke, 4096);
    ctx->tokens.files = dyn_array_create(Cuik_FileEntry, 256);

    // MacroID 0 is a null invocation
    dyn_array_put(ctx->tokens.invokes, (MacroInvoke){ 0 });

    // FileID 0 is the builtin macro file or the NULL file depending on who you ask
    dyn_array_put(ctx->tokens.files, (Cuik_FileEntry){ .filename = "<builtin>", .content_length = (1u << SourceLoc_FilePosBits) - 1u });
    tls_init();

    {
        ctx->stack_ptr = 1;
        ctx->stack[0] = (CPPStackSlot){ 0 };
        ctx->stack[0].filepath = alloc_path(ctx, filepath);
        ctx->stack[0].directory = alloc_directory_path(ctx, filepath);
    }

    return ctx;
}

Cuik_FileEntry* cuikpp_get_files(TokenStream* restrict s) {
    return &s->files[1];
}

size_t cuikpp_get_file_count(TokenStream* restrict s) {
    return dyn_array_length(s->files) - 1;
}

Token* cuikpp_get_tokens(TokenStream* restrict s) {
    return &s->list.tokens[0];
}

size_t cuikpp_get_token_count(TokenStream* restrict s) {
    // don't tell them about the EOF token :P
    return dyn_array_length(s->list.tokens) - 1;
}

void cuiklex_free_tokens(TokenStream* tokens) {
    dyn_array_for(i, tokens->files) {
        // only free the root line_map, all the others are offsets of this one
        if (tokens->files[i].file_pos_bias == 0) {
            dyn_array_destroy(tokens->files[i].line_map);

            // TODO(NeGate): we theoretically can allocate file buffers which
            // aren't in virtual memory but we'll assume not for now
            if (tokens->files[i].content != NULL) {
                cuik__vfree(tokens->files[i].content, tokens->files[i].content_length + 16);
            }
        }
    }

    dyn_array_destroy(tokens->files);
    dyn_array_destroy(tokens->list.tokens);
    dyn_array_destroy(tokens->invokes);
    cuikdg_free(tokens->diag);
}

void cuikpp_finalize(Cuik_CPP* ctx) {
    #if CUIK__CPP_STATS
    fprintf(stderr, " %80s | %.06f ms read+lex\t| %4zu files read\t| %zu fstats\t| %f ms (%zu defines)\n",
        ctx->tokens.filepath,
        ctx->total_io_time / 1000000.0,
        ctx->total_files_read,
        ctx->total_fstats,
        ctx->total_define_access_time / 1000000.0,
        ctx->total_define_accesses
    );
    #endif

    CUIK_TIMED_BLOCK("cuikpp_finalize") {
        cuik__vfree(ctx->macros.keys, (1u << ctx->macros.exp) * sizeof(String));
        cuik__vfree(ctx->macros.vals, (1u << ctx->macros.exp) * sizeof(MacroDef));
        cuik__vfree(ctx->stack, MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot));

        ctx->macros.keys = NULL;
        ctx->macros.vals = NULL;
        ctx->stack = NULL;
    }

    nl_map_free(ctx->include_once);
}

void cuikpp_free(Cuik_CPP* ctx) {
    dyn_array_destroy(ctx->system_include_dirs);

    if (ctx->macros.keys) {
        cuikpp_finalize(ctx);
    }

    cuik__vfree((void*) ctx->the_shtuffs, THE_SHTUFFS_SIZE);
    cuik_free(ctx);
}

// we can infer the column and line from doing a binary search on the TokenStream's line map
static ResolvedSourceLoc find_location(Cuik_FileEntry* file, uint32_t file_pos) {
    if (file->line_map == NULL) {
        return (ResolvedSourceLoc){
            .file = file,
            .line_str = "",
            .line = 1, .column = file_pos
        };
    }

    file_pos += file->file_pos_bias;

    size_t left = 0;
    size_t right = dyn_array_length(file->line_map);
    while (left < right) {
        size_t middle = (left + right) / 2;
        if (file->line_map[middle] > file_pos) {
            right = middle;
        } else {
            left = middle + 1;
        }
    }

    uint32_t l = file->line_map[right - 1];
    assert(file_pos >= l);

    // NOTE(NeGate): it's possible that l is lesser than file->file_pos_bias in the
    // line_str calculation, this is fine (it happens when the line starts in the last
    // chunk and crosses the boundary) we just need to do the math with ptrdiff_t
    // such that if it goes negative it'll refer to the previous chunk of the content
    return (ResolvedSourceLoc){
        .file = file,
        .line_str = &file->content[(ptrdiff_t)l - (ptrdiff_t)file->file_pos_bias],
        .line = right, .column = file_pos - l
    };
}

MacroInvoke* cuikpp_find_macro(TokenStream* tokens, SourceLoc loc) {
    if ((loc.raw & SourceLoc_IsMacro) == 0) {
        return NULL;
    }

    uint32_t macro_id = (loc.raw >> SourceLoc_MacroOffsetBits) & ((1u << SourceLoc_MacroIDBits) - 1);
    return &tokens->invokes[macro_id];
}

Cuik_FileEntry* cuikpp_find_file(TokenStream* tokens, SourceLoc loc) {
    while (loc.raw & SourceLoc_IsMacro) {
        uint32_t macro_id = (loc.raw >> SourceLoc_MacroOffsetBits) & ((1u << SourceLoc_MacroIDBits) - 1);
        loc = tokens->invokes[macro_id].call_site;
    }

    return &tokens->files[loc.raw >> SourceLoc_FilePosBits];
}

Cuik_FileLoc cuikpp_find_location_in_bytes(TokenStream* tokens, SourceLoc loc) {
    assert((loc.raw & SourceLoc_IsMacro) == 0);
    assert((loc.raw >> SourceLoc_FilePosBits) < dyn_array_length(tokens->files));
    Cuik_FileEntry* f = &tokens->files[loc.raw >> SourceLoc_FilePosBits];
    uint32_t pos = loc.raw & ((1u << SourceLoc_FilePosBits) - 1);

    return (Cuik_FileLoc){ f, pos };

    /*if ((loc.raw & SourceLoc_IsMacro) == 0) {
        assert((loc.raw >> SourceLoc_FilePosBits) < dyn_array_length(tokens->files));
        Cuik_FileEntry* f = &tokens->files[loc.raw >> SourceLoc_FilePosBits];
        uint32_t pos = loc.raw & ((1u << SourceLoc_FilePosBits) - 1);

        return (Cuik_FileLoc){ f, pos };
    } else {
        uint32_t macro_id = (loc.raw & ((1u << SourceLoc_MacroIDBits) - 1)) >> SourceLoc_MacroOffsetBits;
        uint32_t macro_off = loc.raw & ((1u << SourceLoc_MacroOffsetBits) - 1);

        Cuik_FileLoc fl = cuikpp_find_location_in_bytes(tokens, tokens->invokes[macro_id].def_site.start);
        return (Cuik_FileLoc){ fl.file, fl.pos + macro_off };
    }*/
}

SourceLoc cuikpp_get_physical_location(TokenStream* tokens, SourceLoc loc) {
    MacroInvoke* m;
    while ((m = cuikpp_find_macro(tokens, loc)) != NULL) { loc = m->call_site; }
    return loc;
}

ResolvedSourceLoc cuikpp_find_location2(TokenStream* tokens, Cuik_FileLoc loc) {
    return find_location(loc.file, loc.pos);
}

ResolvedSourceLoc cuikpp_find_location(TokenStream* tokens, SourceLoc loc) {
    MacroInvoke* m;
    while ((m = cuikpp_find_macro(tokens, loc)) != NULL) {
        loc = m->call_site;
    }

    Cuik_FileLoc fl = cuikpp_find_location_in_bytes(tokens, loc);
    return find_location(fl.file, fl.pos);
}

static void compute_line_map(TokenStream* s, bool is_system, int depth, SourceLoc include_site, const char* filename, char* data, size_t length) {
    DynArray(uint32_t) line_map = dyn_array_create(uint32_t, (length / 20) + 32);

    #if 1
    // !USE_INTRIN
    dyn_array_put(line_map, 0);

    for (size_t i = 0; i < length;) {
        while (i < length && data[i] != '\n') i += 1;

        i += 1;
        dyn_array_put(line_map, i);
    }
    #else
    // TODO(NeGate): make non-x64 SIMD variants
    for (size_t i = 0; i < length; i += 16) {
        while (i < length) {
            __m128i bytes = _mm_load_si128((__m128i*) &data[i]);
            unsigned int mask = _mm_movemask_epi8(_mm_cmpeq_epi8(bytes, _mm_set1_epi8('\n')));
            int len = __builtin_ffs(~mask));

            if (len) {
                i += len;
                break;
            } else {
                i += 16;
            }
        }

        dyn_array_put(line_map, i);
    }
    #endif

    // files bigger than the SourceLoc_FilePosBits allows will be fit into multiple sequencial files
    size_t i = 0, single_file_limit = (1u << SourceLoc_FilePosBits);
    do {
        size_t chunk_end = i + single_file_limit;
        if (chunk_end > length) chunk_end = length;

        dyn_array_put(s->files, (Cuik_FileEntry){ filename, is_system, depth, include_site, i, chunk_end - i, &data[i], line_map });
        i += single_file_limit;
    } while (i < length);
}

static Cuik_Path* alloc_path(Cuik_CPP* restrict ctx, const char* filepath) {
    size_t len = strlen(filepath);

    Cuik_Path* new_path = gimme_the_shtuffs(ctx, sizeof(Cuik_PathFlex) + len + 1);
    cuik_path_set(new_path, filepath);
    return new_path;
}

static Cuik_Path* alloc_directory_path(Cuik_CPP* restrict ctx, const char* filepath) {
    size_t len = strlen(filepath);

    Cuik_Path* new_dir = gimme_the_shtuffs(ctx, sizeof(Cuik_PathFlex) + len + 1);
    cuik_path_set_dir(new_dir, filepath);
    return new_dir;
}

Cuikpp_Status cuikpp_run(Cuik_CPP* restrict ctx) {
    assert(ctx->stack_ptr > 0);
    CPPStackSlot* restrict slot = &ctx->stack[ctx->stack_ptr - 1];

    ////////////////////////////////
    // first file doesn't need to check include paths
    ////////////////////////////////
    slot->include_guard = (struct CPPIncludeGuard){ 0 };

    #if CUIK__CPP_STATS
    uint64_t start_time = cuik_time_in_nanos();
    #endif

    Cuik_FileResult main_file;
    CUIK_TIMED_BLOCK("load main file") {
        if (!ctx->fs(ctx->user_data, slot->filepath, &main_file, ctx->case_insensitive)) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: file \"%s\" doesn't exist.\n", slot->filepath->data);
            return CUIKPP_ERROR;
        }
    }

    #if CUIK__CPP_STATS
    ctx->total_io_time += (cuik_time_in_nanos() - start_time);
    ctx->total_files_read += 1;
    #endif

    // initialize the lexer in the stack slot & record the file entry
    slot->file_id = dyn_array_length(ctx->tokens.files);
    CUIK_TIMED_BLOCK("convert to tokens") {
        slot->tokens = convert_to_token_list(ctx, dyn_array_length(ctx->tokens.files), main_file.length, main_file.data);
    }
    compute_line_map(&ctx->tokens, false, 0, (SourceLoc){ 0 }, slot->filepath->data, main_file.data, main_file.length);

    // continue along to the actual preprocessing now
    #ifdef CPP_DBG
    cppdbg__break();
    #endif /* CPP_DBG */

    if (cuikperf_is_active()) {
        cuikperf_region_start("preprocess", slot->filepath->data);
    }

    TokenStream* restrict s = &ctx->tokens;

    // estimate a good final token count, if we get this right we'll zip past without resizes
    size_t expected = dyn_array_length(slot->tokens.tokens);
    if (expected < 4096) expected = 4096;
    s->list.tokens = dyn_array_create(Token, expected);

    for (;;) yield: {
        slot = &ctx->stack[ctx->stack_ptr - 1];

        TokenArray* restrict in = &slot->tokens;
        for (;;) {
            // Hot code, just copying tokens over
            Token first;
            for (;;) {
                if (at_token_list_end(in)) goto pop_stack;

                if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
                    slot->include_guard.status = INCLUDE_GUARD_INVALID;
                }

                first = consume(in);
                if (first.type == TOKEN_IDENTIFIER) {
                    bool is_glsl = ctx->version == CUIK_VERSION_GLSL;

                    // check if it's actually a macro, if not categorize it if it's a keyword
                    if (!is_defined(ctx, first.content.data, first.content.length)) {
                        // FAST PATH
                        first.type = classify_ident(first.content.data, first.content.length, is_glsl);
                        dyn_array_put(s->list.tokens, first);
                    } else {
                        // SLOW PATH BECAUSE IT NEEDS TO SPAWN POSSIBLY METRIC SHIT LOADS
                        // OF TOKENS AND EXPAND WITH THE AVERAGE C PREPROCESSOR SPOOKIES
                        if (expand_builtin_idents(ctx, &first)) {
                            dyn_array_put(s->list.tokens, first);
                        } else {
                            in->current -= 1;
                            void* savepoint = tls_save();

                            TokenList l = expand_ident(ctx, in, NULL, 0, NULL);
                            for (TokenNode* n = l.head; n != l.tail; n = n->next) {
                                Token* restrict t = &n->t;
                                if (t->type == 0) {
                                    continue;
                                } else if (t->type == TOKEN_IDENTIFIER) {
                                    t->type = classify_ident(t->content.data, t->content.length, is_glsl);
                                }

                                dyn_array_put(s->list.tokens, *t);
                            }

                            tls_restore(savepoint);
                        }
                    }
                } else if (first.type == TOKEN_HASH) {
                    // slow path
                    break;
                } else {
                    dyn_array_put(s->list.tokens, first);
                }
            }

            // Slow code, defines
            DirectiveResult result = DIRECTIVE_UNKNOWN;
            String directive = consume(in).content;

            // shorthand for calling the directives in cpp_directive.h
            #define MATCH(str)                                         \
            if (memcmp(directive.data, #str, sizeof(#str) - 1) == 0) { \
                result = cpp__ ## str(ctx, slot, in);                  \
                break;                                                 \
            }

            // all the directives go here
            switch (directive.length) {
                case 2:
                MATCH(if);
                break;

                case 4:
                MATCH(elif);
                MATCH(else);
                break;

                case 5:
                MATCH(undef);
                MATCH(error);
                MATCH(ifdef);
                MATCH(endif);
                MATCH(embed);
                break;

                case 6:
                MATCH(define);
                MATCH(pragma);
                MATCH(ifndef);
                break;

                case 7:
                MATCH(include);
                MATCH(warning);
                MATCH(version);
                break;

                case 9:
                MATCH(extension);
                break;
            }
            #undef MATCH

            if (result == DIRECTIVE_YIELD) {
                goto yield;
            } else if (result == DIRECTIVE_ERROR) {
                return CUIKPP_ERROR;
            } else if (result == DIRECTIVE_UNKNOWN) {
                SourceRange r = { first.location, get_end_location(&in->tokens[in->current - 1]) };
                diag_err(s, r, "unknown directive %_S", directive);
                return CUIKPP_ERROR;
            }
        }

        // this is called when we're done with a specific file
        pop_stack:
        ctx->stack_ptr -= 1;

        if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
            // the file is practically pragma once
            nl_map_put_cstr(ctx->include_once, slot->filepath->data, 0);
        }

        // write out profile entry
        if (cuikperf_is_active()) {
            cuikperf_region_end();
        }

        // free the token stream
        dyn_array_destroy(in->tokens);

        // if this is the last file, just exit
        if (ctx->stack_ptr == 0) {
            // place last token
            dyn_array_put(s->list.tokens, (Token){ 0 });

            s->list.current = 0;
            return CUIKPP_DONE;
        }
    }
}

TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx) {
    return &ctx->tokens;
}

void cuikpp_dump_defines(Cuik_CPP* ctx) {
    int count = 0;

    size_t cap = 1u << ctx->macros.exp;
    for (size_t i = 0; i < cap; i++) {
        if (ctx->macros.keys[i].length != 0 && ctx->macros.keys[i].length != MACRO_DEF_TOMBSTONE) {
            String key = ctx->macros.keys[i];
            String val = ctx->macros.vals[i].value;

            printf("  #define %.*s %.*s\n", (int)key.length, key.data, (int)val.length, val.data);
        }
    }

    printf("\n// Macro defines active: %d\n", count);
}

static void* gimme_the_shtuffs(Cuik_CPP* restrict c, size_t len) {
    unsigned char* allocation = c->the_shtuffs + c->the_shtuffs_size;

    c->the_shtuffs_size += len;
    if (c->the_shtuffs_size >= THE_SHTUFFS_SIZE) {
        printf("Preprocessor: out of memory!\n");
        abort();
    }

    return allocation;
}

static void* gimme_the_shtuffs_fill(Cuik_CPP* restrict c, const char* str) {
    size_t len = strlen(str) + 1;
    unsigned char* allocation = c->the_shtuffs + c->the_shtuffs_size;

    c->the_shtuffs_size += len;
    if (c->the_shtuffs_size >= THE_SHTUFFS_SIZE) {
        printf("Preprocessor: out of memory!\n");
        abort();
    }

    memcpy(allocation, str, len);
    return allocation;
}

static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top) {
    size_t i = ((uint8_t*)new_top) - c->the_shtuffs;
    assert(i <= c->the_shtuffs_size);
    c->the_shtuffs_size = i;
}

static bool push_scope(Cuik_CPP* restrict ctx, TokenArray* restrict in, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
        diag_err(&ctx->tokens, get_token_range(&in->tokens[in->current - 1]), "too many #ifs");
        return false;
    }

    ctx->scope_eval[ctx->depth++] = (struct Cuikpp_ScopeEval){ in->tokens[in->current - 1].location, initial };
    return true;
}

static bool pop_scope(Cuik_CPP* restrict ctx, TokenArray* restrict in) {
    if (ctx->depth == 0) {
        diag_err(&ctx->tokens, get_token_range(&in->tokens[in->current - 1]), "too many #endif");
        diag_note(&ctx->tokens, (SourceRange){ ctx->scope_eval[0].start, ctx->scope_eval[0].start }, "expected for:");
        return false;
    }

    ctx->depth--;
    return true;
}
