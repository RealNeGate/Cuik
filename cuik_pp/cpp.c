// TODO(NeGate): Refactor this code...
//
// NOTE(NeGate): This code leaks the filename strings but it doesn't actually matter
// because this is a compiler and momma aint raised no bitch.
//
// NOTE(NeGate): the_shtuffs is the simple linear allocator in this preprocessor, just avoids
// wasting time on the heap allocator
#include <cuik.h>
#include <assert.h>
#include <arena_array.h>
#include <new_hash_map.h>
#include <str.h>
#include "diagnostic.h"

#include "lexer.h"
#include <setjmp.h>
#include <sys/stat.h>
#include <hash_map.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

#define CUIK__CPP_STATS 0
#define MACRO_DEF_TOMBSTONE SIZE_MAX

typedef struct {
    // if non-zero length, this holds the macro
    // that is queried for the guard.
    String name;
    // if true, the header is only included if the
    // name is a defined macro.
    bool expected;
} IncludeGuardEntry;

enum {
    CPP_MAX_SCOPE_DEPTH = 4096,
};

typedef struct {
    String key;
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

    TB_Arena perm_arena;

    // used during macro expansions
    TB_Arena tmp_arena;
    TokenStream tokens;

    // powers __COUNTER__
    int unique_counter;

    Token directive_token;

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

    NL_Strmap(IncludeGuardEntry) include_once;

    // system libraries
    // DynArray(Cuik_IncludeDir)
    Cuik_IncludeDir* system_include_dirs;

    // how deep into directive scopes (#if, #ifndef, #ifdef) is it
    int depth;
    NL_HashSet macros;

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
static intmax_t eval(Cuik_CPP* restrict c, Lexer* restrict in);

static bool push_scope(Cuik_CPP* restrict ctx, SourceRange r, bool initial);
static bool pop_scope(Cuik_CPP* restrict ctx, SourceRange r);

static Token quote_token_array(Cuik_CPP* restrict ctx, SourceLoc loc, int start, int end);

static Cuik_Path* alloc_path(Cuik_CPP* restrict ctx, const char* filepath);
static Cuik_Path* alloc_directory_path(Cuik_CPP* restrict ctx, const char* filepath);
static void compute_line_map(Cuik_CPP* restrict ctx, bool is_system, int depth, SourceLoc include_site, const char* filename, char* data, size_t length);

enum {
    MAX_CPP_STACK_DEPTH = 1024,
};

typedef struct CPPStackSlot {
    Cuik_Path* filepath;
    Cuik_Path* directory;

    uint32_t file_id;
    SourceLoc loc; // location of the #include
    Lexer lexer;

    // https://gcc.gnu.org/onlinedocs/cppinternals/Guard-Macros.html
    struct CPPIncludeGuard {
        enum {
            INCLUDE_GUARD_INVALID = -1,
            INCLUDE_GUARD_LOOKING_FOR_IF,    // #ifndef MACRO_NAME
            INCLUDE_GUARD_LOOKING_FOR_ENDIF, // #endif
            INCLUDE_GUARD_EXPECTING_NOTHING,
        } status;
        int if_depth; // the depth value we expect the include guard to be at
        String define;
    } include_guard;
} CPPStackSlot;

// just the value (doesn't track the name of the parameter)
typedef struct {
    // for the first token
    TknType type;
    String key;
    String val;
    SourceRange loc;

    // location in the local token cache of the expanded macros
    int token_start, token_end;
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

static void push_token(Cuik_CPP* ctx, Token t) {
    dyn_array_put(ctx->tokens.list.tokens, t);
}

static void expect_from_lexer(Cuik_CPP* c, Lexer* l, char ch) {
    Token t = lexer_read(l);
    if (t.type != ch) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t.location);

        fprintf(stderr, "error %s:%d: expected '%c' got '%.*s'", r.file->filename, r.line, ch, (int) t.content.length, t.content.data);
        abort();
    }
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

#include "atoms.c"

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
    };

    tb_arena_create(&ctx->tmp_arena, NULL);
    tb_arena_create(&ctx->perm_arena, NULL);
    ctx->macros = nl_hashset_alloc(2048);

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
    fprintf(stderr, " %-80s | %.06f ms read+lex\t| %4zu files read\t| %zu fstats\t| %f ms (%zu defines)\n",
        ctx->tokens.filepath,
        ctx->total_io_time / 1000000.0,
        ctx->total_files_read,
        ctx->total_fstats,
        ctx->total_define_access_time / 1000000.0,
        ctx->total_define_accesses
    );
    #endif

    CUIK_TIMED_BLOCK("cuikpp_finalize") {
        nl_hashset_free(ctx->macros);
        cuik__vfree(ctx->stack, MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot));
        ctx->stack = NULL;
    }

    // tb_arena_destroy(&ctx->tmp_arena);
    nl_map_free(ctx->include_once);
}

void cuikpp_free(Cuik_CPP* ctx) {
    dyn_array_destroy(ctx->system_include_dirs);
    if (ctx->stack != NULL) {
        cuikpp_finalize(ctx);
    }
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

static void compute_line_map(Cuik_CPP* restrict ctx, bool is_system, int depth, SourceLoc include_site, const char* filename, char* data, size_t length) {
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

        dyn_array_put(ctx->tokens.files, (Cuik_FileEntry){ filename, is_system, depth, include_site, i, chunk_end - i, &data[i], line_map });
        i += single_file_limit;
    } while (i < length);
}

static Cuik_Path* alloc_path(Cuik_CPP* restrict ctx, const char* filepath) {
    size_t len = strlen(filepath);

    Cuik_Path* new_path = tb_arena_alloc(&ctx->tmp_arena, sizeof(Cuik_PathFlex) + len + 1);
    cuik_path_set(new_path, filepath);
    return new_path;
}

static Cuik_Path* alloc_directory_path(Cuik_CPP* restrict ctx, const char* filepath) {
    size_t len = strlen(filepath);

    Cuik_Path* new_dir = tb_arena_alloc(&ctx->tmp_arena, sizeof(Cuik_PathFlex) + len + 1);
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
            fprintf(stderr, "\x1b[31merror\x1b[0m: main file \"%s\" doesn't exist.\n", slot->filepath->data);
            return CUIKPP_ERROR;
        }
    }

    #if CUIK__CPP_STATS
    ctx->total_io_time += (cuik_time_in_nanos() - start_time);
    ctx->total_files_read += 1;
    #endif

    // initialize the lexer in the stack slot & record the file entry
    slot->file_id = dyn_array_length(ctx->tokens.files);
    slot->lexer = (Lexer){
        .file_id = dyn_array_length(ctx->tokens.files),
        .start = (unsigned char*) main_file.data,
        .current = (unsigned char*) main_file.data,
    };

    compute_line_map(ctx, false, 0, (SourceLoc){ 0 }, slot->filepath->data, main_file.data, main_file.length);
    // Token t = lexer_read(&slot->lexer);

    // continue along to the actual preprocessing now
    #ifdef CPP_DBG
    cppdbg__break();
    #endif /* CPP_DBG */

    if (cuikperf_is_active()) {
        cuikperf_region_start("preprocess", slot->filepath->data);
    }

    TokenStream* restrict s = &ctx->tokens;
    bool is_glsl = ctx->version == CUIK_VERSION_GLSL;

    // estimate a good final token count, if we get this right we'll zip past without resizes
    ctx->tokens.list.tokens = dyn_array_create(Token, 1024);

    int ttt = 0;
    for (;;) yield: {
        slot = &ctx->stack[ctx->stack_ptr - 1];

        Lexer* restrict in = &slot->lexer;
        for (;;) {
            // Hot code, just copying tokens over
            Token first;
            for (;;) {
                ttt++;
                first = lexer_read(in);
                if (first.type == 0) {
                    goto pop_stack;
                }

                if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
                    slot->include_guard.status = INCLUDE_GUARD_INVALID;
                }

                if (first.type == TOKEN_HASH) {
                    break;
                } else if (first.type == TOKEN_IDENTIFIER) {
                    MacroDef* def = find_define(ctx, first.content.data, first.content.length);
                    if (def != NULL) {
                        int start = dyn_array_length(ctx->tokens.list.tokens);
                        push_token(ctx, first);

                        cuikperf_region_start2("expand", first.content.length, (const char*) first.content.data);
                        expand_identifier(ctx, in, NULL, start, start+1, 0, def, 0, NULL);
                        cuikperf_region_end();

                        // classify any newly-generated identifiers
                        DynArray(Token) tokens = ctx->tokens.list.tokens;
                        FOR_N(i, start, dyn_array_length(tokens)) {
                            if (tokens[i].type == TOKEN_IDENTIFIER) {
                                tokens[i].type = classify_ident(tokens[i].content.data, tokens[i].content.length, is_glsl);
                            }
                            // diag_note(s, get_token_range(&tokens[i]), "A");
                        }
                    } else {
                        first.type = classify_ident(first.content.data, first.content.length, is_glsl);
                        push_token(ctx, first);
                    }
                } else {
                    push_token(ctx, first);
                }
            }

            first = lexer_read(in);
            ctx->directive_token = first;

            // Slow code, defines
            DirectiveResult result = DIRECTIVE_UNKNOWN;
            String directive = first.content;

            // shorthand for calling the directives in cpp_directive.h
            #define MATCH(str)                                         \
            if (memcmp(directive.data, #str, sizeof(#str) - 1) == 0) { \
                result = cpp__ ## str(ctx, slot, in);                  \
                break;                                                 \
            }

            // all the directives go here
            cuikperf_region_start2("directive", first.content.length, (const char*) first.content.data);
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
                // MATCH(error);
                MATCH(ifdef);
                MATCH(endif);
                // MATCH(embed);
                break;

                case 6:
                MATCH(define);
                MATCH(pragma);
                MATCH(ifndef);
                break;

                case 7:
                MATCH(include);
                // MATCH(warning);
                // MATCH(version);
                break;

                case 9:
                // MATCH(extension);
                break;
            }
            #undef MATCH

            if (result != DIRECTIVE_YIELD && cuikperf_is_active()) {
                cuikperf_region_end();
            }

            if (result == DIRECTIVE_YIELD) {
                goto yield;
            } else if (result == DIRECTIVE_ERROR) {
                return CUIKPP_ERROR;
            } else if (result == DIRECTIVE_UNKNOWN) {
                SourceRange r = get_token_range(&first);
                diag_err(s, r, "unknown directive %_S", directive);
                return CUIKPP_ERROR;
            }
        }

        // this is called when we're done with a specific file
        pop_stack:
        ctx->stack_ptr -= 1;

        if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
            IncludeGuardEntry e = { slot->include_guard.define };
            nl_map_put_cstr(ctx->include_once, slot->filepath->data, e);
        }

        // write out profile entry
        if (cuikperf_is_active()) {
            cuikperf_region_end();
        }

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
    /*int count = 0;

    size_t cap = 1u << ctx->macros.exp;
    for (size_t i = 0; i < cap; i++) {
        if (ctx->macros.keys[i].length != 0 && ctx->macros.keys[i].length != MACRO_DEF_TOMBSTONE) {
            String key = ctx->macros.keys[i];
            String val = ctx->macros.vals[i].value;

            printf("  #define %.*s %.*s\n", (int)key.length, key.data, (int)val.length, val.data);
        }
    }

    printf("\n// Macro defines active: %d\n", count);*/
    assert(0 && "TODO");
}

static bool push_scope(Cuik_CPP* restrict ctx, SourceRange r, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
        diag_err(&ctx->tokens, r, "too many #ifs");
        return false;
    }

    ctx->scope_eval[ctx->depth++] = (struct Cuikpp_ScopeEval){ r.start, initial };
    return true;
}

static bool pop_scope(Cuik_CPP* restrict ctx, SourceRange r) {
    if (ctx->depth == 0) {
        diag_err(&ctx->tokens, r, "too many #endif");
        diag_note(&ctx->tokens, (SourceRange){ ctx->scope_eval[0].start, ctx->scope_eval[0].start }, "expected for:");
        return false;
    }

    ctx->depth--;
    return true;
}
