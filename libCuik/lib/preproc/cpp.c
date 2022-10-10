// TODO(NeGate): Refactor this code...
//
// NOTE(NeGate): This code leaks the filename strings but it doesn't actually matter
// because this is a compiler and momma aint raised no bitch.
//
// NOTE(NeGate): the_shtuffs is the simple linear allocator in this preprocessor, just avoids
// wasting time on the heap allocator
#include <cuik.h>
#include <assert.h>
#include "../str.h"
#include "../diagnostic.h"

#include "lexer.h"
#include <sys/stat.h>

#if USE_INTRIN
#include <x86intrin.h>
#endif

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include "../string_map.h"

// GOD I HATE FORWARD DECLARATIONS
static uint64_t hash_ident(const void* key, size_t len);
static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length);
static void expect(TokenList* restrict in, char ch);
static intmax_t eval(Cuik_CPP* restrict c, TokenList* restrict in);

// static _Noreturn void generic_error(Lexer* restrict in, const char* msg);
#define generic_error(...)

static void* gimme_the_shtuffs(Cuik_CPP* restrict c, size_t len);
static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top);

static void push_scope(Cuik_CPP* restrict ctx, TokenList* restrict in, bool initial);
static void pop_scope(Cuik_CPP* restrict ctx, TokenList* restrict in);

static void print_token_stream(TokenStream* s, size_t start, size_t end);

static bool expand(Cuik_CPP* restrict c, TokenList* restrict out_tokens, TokenList* restrict in, uint32_t parent_macro);
static bool expand_ident(Cuik_CPP* restrict c, TokenList* restrict out_tokens, TokenList* restrict in, uint32_t parent_macro);

#define MAX_CPP_STACK_DEPTH 1024

typedef struct CPPStackSlot {
    const char* filepath;
    const char* directory;
    uint64_t start_time;

    uint32_t file_id;
    SourceLoc loc; // location of the #include
    TokenList tokens;

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

static Token peek(TokenList* restrict in) {
    return in->tokens[in->current];
}

static SourceLoc get_end_location(TokenList* restrict in) {
    Token* t = &in->tokens[in->current - 1];
    return (SourceLoc){ t->location.raw + t->content.length };
}

static bool at_token_list_end(TokenList* restrict in) {
    return in->current >= dyn_array_length(in->tokens)-1;
}

static Token consume(TokenList* restrict in) {
    assert(in->current < dyn_array_length(in->tokens));
    return in->tokens[in->current++];
}

static void expect_from_lexer(Cuik_CPP* c, Lexer* l, char ch) {
    Token t = lexer_read(l);
    if (t.type != ch) {
        ResolvedSourceLoc r;
        if (!cuikpp_find_location(&c->tokens, t.location, &r)) {
            assert(0 && "TODO find_location");
        }

        fprintf(stderr, "error %s:%d: expected '%c' got '%.*s'", r.filename, r.line, ch, (int) t.content.length, t.content.data);
        abort();
    }
}

static TokenList convert_to_token_list(Cuik_CPP* restrict c, uint32_t file_id, size_t length, char* data) {
    Lexer l = {
        .file_id = file_id,
        .start = (unsigned char*) data,
        .current = (unsigned char*) data,
    };

    TokenList list = { 0 };
    list.tokens = dyn_array_create_with_initial_cap(Token, 32 + ((length + 7) / 8));
    for (;;) {
        Token t = lexer_read(&l);
        if (t.type == 0) break;

        dyn_array_put(list.tokens, t);
    }

    dyn_array_put(list.tokens, (Token){ 0 });
    return list;
}

static String get_token_as_string(TokenStream* restrict in) {
    return tokens_get(in)->content;
}

static size_t push_expansion(Cuik_CPP* c, TokenList* restrict out_tokens, TokenList* restrict in) {
    // we're gonna temporarily expand the filepath (in case some dumbfuck put macros in it)
    size_t save = dyn_array_length(out_tokens->tokens);
    out_tokens->current = save;

    expand(c, out_tokens, in, 0);
    assert(out_tokens->current != dyn_array_length(out_tokens->tokens) && "Expected the macro expansion to add something");
    return save;
}

static void pop_expansion(TokenList* out_tokens, size_t save) {
    // reset token stream
    dyn_array_set_length(out_tokens->tokens, save);
    out_tokens->current = 0;
}

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
    assert((loc.raw & SourceLoc_IsMacro) == 0 && "TODO: support macro in cuikpp_is_in_main_file");

    Cuik_File* f = &tokens->files[loc.raw >> SourceLoc_FilePosBits];
    return f->filename == tokens->filepath;
}

void cuikpp_init(Cuik_CPP* ctx, const char filepath[FILENAME_MAX]) {
    size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
    size_t sz2 = sizeof(SourceRange) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

    *ctx = (Cuik_CPP){
        .stack = cuik__valloc(MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot)),

        .macro_bucket_keys = cuik__valloc(sz),
        .macro_bucket_keys_length = cuik__valloc(sz),
        .macro_bucket_values_start = cuik__valloc(sz),
        .macro_bucket_values_end = cuik__valloc(sz),
        .macro_bucket_source_locs = cuik__valloc(sz2),

        .the_shtuffs = cuik__valloc(THE_SHTUFFS_SIZE),
    };

    // initialize dynamic arrays
    ctx->system_include_dirs = dyn_array_create(char*);
    ctx->stack_ptr = 1;

    char* slash = strrchr(filepath, '\\');
    if (!slash) slash = strrchr(filepath, '/');

    char* directory = gimme_the_shtuffs(ctx, FILENAME_MAX);
    if (slash) {
        #if _WIN32
        sprintf_s(directory, FILENAME_MAX, "%.*s\\", (int)(slash - filepath), filepath);
        #else
        snprintf(directory, FILENAME_MAX, "%.*s/", (int)(slash - filepath), filepath);
        #endif
    } else {
        directory[0] = '\0';
    }

    ctx->tokens.filepath = filepath;
    ctx->tokens.list.tokens = dyn_array_create(Token);
    ctx->tokens.invokes = dyn_array_create(MacroInvoke);
    ctx->tokens.files = dyn_array_create_with_initial_cap(Cuik_File, 256);

    ctx->scratch_list.tokens = dyn_array_create(Token);

    // MacroID 0 is a null invocation
    dyn_array_put(ctx->tokens.invokes, (MacroInvoke){ 0 });
    tls_init();

    ctx->state1 = CUIK__CPP_FIRST_FILE;
    ctx->stack[0] = (CPPStackSlot){
        .filepath = filepath,
        .directory = directory,
    };
}

// we can infer the column and line from doing a binary search on the TokenStream's line map
static bool find_location(Cuik_File* file, uint32_t file_pos, ResolvedSourceLoc* out_result) {
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

    *out_result = (ResolvedSourceLoc){
        .filename = file->filename,
        .line_str = &file->content[l - file->file_pos_bias],
        .line = right, .column = file_pos - l
    };
    return true;
}

Cuik_File* cuikpp_find_file(TokenStream* tokens, SourceLoc loc) {
    assert((loc.raw & SourceLoc_IsMacro) == 0 && "TODO: support macro find_location");
    return &tokens->files[loc.raw >> SourceLoc_FilePosBits];
}

bool cuikpp_find_location_in_bytes(TokenStream* tokens, SourceLoc loc, Cuik_FileLoc* out_result) {
    if ((loc.raw & SourceLoc_IsMacro) == 0) {
        Cuik_File* f = &tokens->files[loc.raw >> SourceLoc_FilePosBits];
        uint32_t pos = loc.raw & ((1u << SourceLoc_FilePosBits) - 1);

        *out_result = (Cuik_FileLoc){ f, pos };
        return true;
    } else {
        uint32_t macro_id = (loc.raw & ((1u << SourceLoc_MacroIDBits) - 1)) >> SourceLoc_MacroOffsetBits;
        uint32_t macro_off = loc.raw & ((1u << SourceLoc_MacroOffsetBits) - 1);

        Cuik_FileLoc fl;
        if (!cuikpp_find_location_in_bytes(tokens, tokens->invokes[macro_id].call_site, &fl)) {
            assert(0 && "TODO");
        }

        *out_result = (Cuik_FileLoc){ fl.file, fl.pos + macro_off };
        return true;
    }
}

bool cuikpp_find_location(TokenStream* tokens, SourceLoc loc, ResolvedSourceLoc* out_result) {
    Cuik_FileLoc fl;
    if (!cuikpp_find_location_in_bytes(tokens, loc, &fl)) {
        return false;
    }

    return find_location(fl.file, fl.pos, out_result);
}

static void compute_line_map(TokenStream* s, const char* filename, char* data, size_t length) {
    DynArray(uint32_t) line_map = dyn_array_create_with_initial_cap(uint32_t, (length / 20) + 32);

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
        dyn_array_put(s->files, (Cuik_File){ filename, i, &data[i], line_map });
        i += single_file_limit;
    } while (i + single_file_limit < length);
}

Cuikpp_Status cuikpp_next(Cuik_CPP* ctx, Cuikpp_Packet* packet) {
    assert(ctx->stack_ptr > 0);
    CPPStackSlot* restrict slot = &ctx->stack[ctx->stack_ptr - 1];

    // we know the filepath but haven't resolved it yet
    if (ctx->state1 == CUIK__CPP_FIRST_FILE) {
        ////////////////////////////////
        // first file doesn't need to check include paths
        ////////////////////////////////
        // state2 just means
        //  0 ask for file
        //  1 get a file and initialize it
        ////////////////////////////////
        switch (ctx->state2++) {
            // ask to get file
            case 0: {
                slot->start_time = cuik_time_in_nanos();
                slot->include_guard = (struct CPPIncludeGuard){ 0 };

                if (cuik_is_profiling()) {
                    cuik_profile_region_start(cuik_time_in_nanos(), "preprocess: %s", slot->filepath);
                }

                packet->tag = CUIKPP_PACKET_GET_FILE;
                packet->file.input_path = slot->filepath;
                packet->file.is_primary = true;
                packet->file.length = 0;
                packet->file.data = NULL;
                return CUIKPP_CONTINUE;
            }

            // get back a file
            case 1: {
                if (packet->file.length == 0) {
                    return CUIKPP_ERROR;
                }

                #if CUIK__CPP_STATS
                ctx->total_io_time += (cuik_time_in_nanos() - slot->start_time);
                ctx->total_files_read += 1;
                #endif

                // initialize the lexer in the stack slot & record the file entry
                slot->tokens = convert_to_token_list(ctx, dyn_array_length(ctx->tokens.files), packet->file.length, packet->file.data);
                compute_line_map(&ctx->tokens, packet->file.input_path, packet->file.data, packet->file.length);

                // we finished resolving
                ctx->state1 = CUIK__CPP_NONE;

                // continue along to the actual preprocessing now
                break;
            }

            default:
            __builtin_unreachable();
            break;
        }
    } else if (ctx->state1 == CUIK__CPP_USR_INCLUDE || ctx->state1 == CUIK__CPP_LIB_INCLUDE) {
        ////////////////////////////////
        // include paths need to query all the search paths
        // and also canonicalize the filepath
        ////////////////////////////////
        // state2 just means
        //  0   is local include
        //  1+i is search path include
        //  n   is local include (only run for LIB_INCLUDE)
        ////////////////////////////////
        assert(packet->tag == CUIKPP_PACKET_QUERY_FILE);
        if (!packet->query.found) {
            #if CUIK__CPP_STATS
            ctx->total_fstats += 1;
            #endif

            // we didn't find a match
            ctx->state2 += 1;
            int index = ctx->state2 - 1;

            int endpoint = dyn_array_length(ctx->system_include_dirs);
            if (index == endpoint) {
                assert(ctx->stack_ptr > 1);
                CPPStackSlot* restrict prev_slot = &ctx->stack[ctx->stack_ptr - 2];

                // int loc = tokens_get_location_line(&prev_slot->tokens);
                // fprintf(stderr, "error %s:%d: Could not find file! %s\n", prev_slot->tokens.filepath, loc, slot->filepath);
                return CUIKPP_ERROR;
            }

            // ask for the next filepath
            // it's ok this const removal is based
            char* path = (char*) packet->query.input_path;
            sprintf_s(path, FILENAME_MAX, "%s%s", ctx->system_include_dirs[index], slot->filepath);

            packet->query.found = false;
            return CUIKPP_CONTINUE;
        }

        const char* filepath = packet->file.input_path;

        packet->tag = CUIKPP_PACKET_CANONICALIZE;
        packet->canonicalize.input_path = filepath;
        packet->canonicalize.output_path = arena_alloc(&thread_arena, FILENAME_MAX, 1);

        // we finished resolving
        ctx->state1 = CUIK__CPP_CANONICALIZE;
        return CUIKPP_CONTINUE;
    } else if (ctx->state1 == CUIK__CPP_CANONICALIZE) {
        const char* filepath = packet->canonicalize.output_path;

        ptrdiff_t search = nl_strmap_get_cstr(ctx->include_once, filepath);
        if (search < 0) {
            // for (int i = 0; i < ctx->stack_ptr; i++) printf("  ");
            // printf("%s\n", filepath);

            // identify directory path
            char* slash = strrchr(filepath, '/');
            if (!slash) slash = strrchr(filepath, '\\');

            char* new_dir = NULL;
            if (slash) {
                size_t slash_pos = slash - filepath;

                new_dir = arena_alloc(&thread_arena, slash_pos + 2, 1);
                memcpy(new_dir, filepath, slash_pos);
                new_dir[slash_pos] = '/';
                new_dir[slash_pos + 1] = 0;
            } else {
                new_dir = arena_alloc(&thread_arena, 2, 1);
                new_dir[0] = '/';
                new_dir[1] = 0;
            }

            // restore the shtuffs (the value is the filename from the original include code)
            trim_the_shtuffs(ctx, (void*) slot->filepath);

            slot->filepath = filepath;
            slot->directory = new_dir;

            ctx->state1 = CUIK__CPP_GET_FILE;

            packet->tag = CUIKPP_PACKET_GET_FILE;
            packet->file.input_path = filepath;
            packet->file.length = 0;
            packet->file.data = NULL;
            packet->file.is_primary = false;

            // fprintf(stderr, "PRAGMA ONCE DONT GOT IT %s\n", filepath);
            return CUIKPP_CONTINUE;
        } else {
            // fprintf(stderr, "\x1b[36mPRAGMA ONCE GOT IT %s\x1b[0m\n", filepath);
        }

        // revert since it's only allowed to include once and we already did it
        // then just continue
        ctx->stack_ptr -= 1;
        slot = &ctx->stack[ctx->stack_ptr - 1];
    } else if (ctx->state1 == CUIK__CPP_GET_FILE) {
        const char* filepath = packet->file.input_path;
        if (cuik_is_profiling()) {
            cuik_profile_region_start(cuik_time_in_nanos(), "preprocess: %s", filepath);
        }

        #if CUIK__CPP_STATS
        ctx->total_io_time += (cuik_time_in_nanos() - slot->start_time);
        ctx->total_files_read += 1;
        #endif

        // initialize the file & lexer in the stack slot
        slot->include_guard = (struct CPPIncludeGuard){ 0 };
        // initialize the lexer in the stack slot & record file entry
        slot->tokens = convert_to_token_list(ctx, dyn_array_length(ctx->tokens.files), packet->file.length, packet->file.data);
        compute_line_map(&ctx->tokens, packet->file.input_path, packet->file.data, packet->file.length);

        // we finished resolving
        ctx->state1 = CUIK__CPP_NONE;

        // continue along to the actual preprocessing now
    }

    TokenList* restrict in = &slot->tokens;
    TokenStream* restrict s = &ctx->tokens;

    for (;;) {
        // FAST PATH: raw tokens
        while (!at_token_list_end(in)) {
            Token t = consume(in);
            if (t.type == 0 || t.type == TOKEN_HASH || t.type == TOKEN_DOUBLE_HASH) {
                // Fallback to slow path
                in->current -= 1;
                break;
            } else if (t.type == TOKEN_IDENTIFIER) {
                if (is_defined(ctx, t.content.data, t.content.length)) {
                    in->current -= 1;
                    break;
                }

                t.type = classify_ident(t.content.data, t.content.length);
            }

            dyn_array_put(s->list.tokens, t);
        }

        Token first = consume(in);
        if (first.type != 0 && slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
            slot->include_guard.status = INCLUDE_GUARD_INVALID;
        }

        if (first.type == 0) {
            ctx->stack_ptr -= 1;

            if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
                // the file is practically pragma once
                nl_strmap_put_cstr(ctx->include_once, (const char*) slot->filepath, 0);
            }

            // write out profile entry
            if (cuik_is_profiling()) {
                cuik_profile_region_end();
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

            // step out of this file into the previous one
            slot = &ctx->stack[ctx->stack_ptr - 1];

            in = &slot->tokens;
            continue;
        } else if (first.type == TOKEN_HASH) {
            DirectiveResult result = DIRECTIVE_UNKNOWN;
            String directive = consume(in).content;

            // shorthand for calling the directives in cpp_directive.h
            #define MATCH(str)                                         \
            if (memcmp(directive.data, #str, sizeof(#str) - 1) == 0) { \
                result = cpp__ ## str(ctx, slot, in, packet);          \
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
                break;

                case 6:
                MATCH(define);
                MATCH(pragma);
                MATCH(ifndef);
                break;

                case 7:
                MATCH(include);
                MATCH(warning);
                break;
            }
            #undef MATCH

            if (result == DIRECTIVE_ERROR) {
                return CUIKPP_ERROR;
            } else if (result == DIRECTIVE_YIELD) {
                return CUIKPP_CONTINUE;
            } else if (result == DIRECTIVE_UNKNOWN) {
                SourceRange r = { first.location, get_end_location(in) };
                diag(s, r, &cuikdg_unknown_directive, directive);
                return CUIKPP_ERROR;
            }
        } else if (first.type == TOKEN_IDENTIFIER) {
            // check if it's actually a macro, if not categorize it if it's a keyword
            if (!is_defined(ctx, first.content.data, first.content.length)) {
                // FAST PATH
                first.type = classify_ident(first.content.data, first.content.length);
                dyn_array_put(s->list.tokens, first);
            } else {
                in->current -= 1;

                // SLOW PATH BECAUSE IT NEEDS TO SPAWN POSSIBLY METRIC SHIT LOADS
                // OF TOKENS AND EXPAND WITH THE AVERAGE C PREPROCESSOR SPOOKIES
                expand_ident(ctx, &s->list, in, 0);
            }
        }
    }
}

void cuikpp_deinit(Cuik_CPP* ctx) {
    #if CUIK__CPP_STATS
    //printf("%40s | %zu file read | %zu fstats\n", ctx->files[0].filepath, ctx->total_files_read, ctx->total_fstats);
    #if 1
    printf(" %40s | %.06f ms read+lex\t| %4zu files read\t| %zu fstats\t| %f ms (%zu defines)\n",
        ctx->files[0].filepath,
        ctx->total_io_time / 1000000.0,
        ctx->total_files_read,
        ctx->total_fstats,
        ctx->total_define_access_time / 1000000.0,
        ctx->total_define_accesses);
    #else
    dyn_array_for(i, ctx->files) {
        printf("%s,%zu\n", ctx->files[i].filepath, ctx->files[i].content_len);
    }
    /*printf("%s,%.06f,%.06f,%4zu,%.06f,%zu,\n",
        ctx->files[0].filepath,
        ctx->total_lex_time / 1000000.0,
        ctx->total_io_time / 1000000.0,
        ctx->total_files_read,
        ctx->total_io_space / 1000000.0,
        ctx->total_fstats);*/
    #endif
    #endif

    if (ctx->macro_bucket_keys) {
        cuikpp_finalize(ctx);
    }

    /*if (ctx->files != NULL) {
        size_t count = dyn_array_length(ctx->files);

        for (size_t i = 0; i < count; i++) {
            CUIK_CALL(ctx->file_system, free_file, &ctx->files[i]);
        }
    }*/

    cuik__vfree(ctx->stack, MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot));
    cuik__vfree((void*) ctx->the_shtuffs, THE_SHTUFFS_SIZE);
    ctx->stack = NULL;
    ctx->the_shtuffs = NULL;
}

Cuikpp_Status cuikpp_default_run(Cuik_CPP* ctx, Cuik_FileCache* cache) {
    Cuikpp_Packet packet;
    for (;;) {
        Cuikpp_Status status = cuikpp_next(ctx, &packet);
        if (status != CUIKPP_CONTINUE) return status;

        cuikpp_default_packet_handler(ctx, &packet, cache);
    }
}

void cuikpp_finalize(Cuik_CPP* ctx) {
    CUIK_TIMED_BLOCK("cuikpp_finalize") {
        size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
        size_t sz2 = sizeof(SourceRange) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

        cuik__vfree((void*)ctx->macro_bucket_keys, sz);
        cuik__vfree((void*)ctx->macro_bucket_keys_length, sz);
        cuik__vfree((void*)ctx->macro_bucket_values_start, sz);
        cuik__vfree((void*)ctx->macro_bucket_values_end, sz);
        cuik__vfree((void*)ctx->macro_bucket_source_locs, sz2);
        cuik__vfree((void*)ctx->stack, 1024 * sizeof(CPPStackSlot));

        ctx->macro_bucket_keys = NULL;
        ctx->macro_bucket_keys_length = NULL;
        ctx->macro_bucket_values_start = NULL;
        ctx->macro_bucket_values_end = NULL;
        ctx->macro_bucket_source_locs = NULL;
        ctx->stack = NULL;
    }
}

TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx) {
    return &ctx->tokens;
}

/*
size_t cuikpp_get_file_table_count(Cuik_CPP* ctx) {
    return dyn_array_length(ctx->files);
}

Cuik_FileEntry* cuikpp_get_file_table(Cuik_CPP* ctx) {
    return &ctx->files[0];
}
*/

void cuikpp_dump_defines(Cuik_CPP* ctx) {
    int count = 0;

    for (int i = 0; i < MACRO_BUCKET_COUNT; i++) {
        for (int j = 0; j < ctx->macro_bucket_count[i]; j++) {
            size_t e = (i * SLOTS_PER_MACRO_BUCKET) + j;

            size_t keylen = ctx->macro_bucket_keys_length[e];
            const char* key = (const char*)ctx->macro_bucket_keys[e];

            size_t vallen = ctx->macro_bucket_values_end[e] - ctx->macro_bucket_values_start[e];
            const char* val = (const char*)ctx->macro_bucket_values_start[e];

            printf("  #define %.*s %.*s\n", (int)keylen, key, (int)vallen, val);
        }

        count += ctx->macro_bucket_count[i];
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

static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top) {
    size_t i = ((uint8_t*)new_top) - c->the_shtuffs;
    assert(i <= c->the_shtuffs_size);
    c->the_shtuffs_size = i;
}

static void push_scope(Cuik_CPP* restrict ctx, TokenList* restrict in, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
        generic_error(in, "Exceeded max scope depth!");
    }

    ctx->scope_eval[ctx->depth++] = initial;
}

static void pop_scope(Cuik_CPP* restrict ctx, TokenList* restrict in) {
    if (ctx->depth == 0) {
        generic_error(in, "Too many endifs\n");
    }
    ctx->depth--;
}

/*static _Noreturn void generic_error(Lexer* restrict in, const char* msg) {
    fprintf(stderr, "error: %s\n", msg);
    abort();
}*/

static void expect(TokenList* restrict in, char ch) {
    Token t = consume(in);
    if (t.type != ch) {
        // report(REPORT_ERROR, NULL, in, tokens_get_location_index(in), "expected '%c' got '%.*s'", ch, (int) t.content.length, t.content.data);
        abort();
    }
}
