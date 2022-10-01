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

static void preprocess_file(Cuik_CPP* restrict c, TokenStream* restrict s, size_t parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth);
static uint64_t hash_ident(const void* key, size_t len);
static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length);
static void expect(TokenStream* restrict in, char ch);
static void skip_directive_body(TokenStream* s);
static intmax_t eval(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc);
static _Noreturn void generic_error(TokenStream* restrict in, const char* msg);

static void* gimme_the_shtuffs(Cuik_CPP* restrict c, size_t len);
static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top);
static SourceLocIndex get_source_location(Cuik_CPP* restrict c, TokenStream* restrict in, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type);

static void expand(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, size_t in_stream_end, bool exit_on_hit_line, SourceLocIndex parent_loc);
static void expand_ident(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc);
static void push_scope(Cuik_CPP* restrict ctx, TokenStream* restrict in, bool initial, SourceLocIndex loc);
static void pop_scope(Cuik_CPP* restrict ctx, TokenStream* restrict in, SourceLocIndex loc);

// if end is NULL, it'll just be null terminated
static TokenStream get_all_tokens_in_buffer(const char* filepath, uint8_t* data, uint8_t* end);
static void free_token_stream(TokenStream* s);
static void print_token_stream(TokenStream* s, size_t start, size_t end);

//static void expand_double_hash(Cuik_CPP* restrict c, TokenStream* restrict s, Token* last, Lexer* restrict l, SourceLocIndex loc);

#define MAX_CPP_STACK_DEPTH 1024

typedef struct CPPStackSlot {
    const char* filepath;
    const char* directory;

    size_t file_id;
    uint64_t start_time;
    SourceLocIndex include_loc;

    TokenStream tokens;

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

static void expect_from_lexer(Lexer* l, char ch) {
    if (l->token_type != ch) {
        int loc = l->current_line;
        fprintf(stderr, "error %s:%d: expected '%c' got '%.*s'", l->filepath, loc, ch, (int)(l->token_end - l->token_start), l->token_start);
        abort();
    }

    lexer_read(l);
}

// Basically a mini-unity build that takes up just the CPP module
#include "cpp_symtab.h"
#include "cpp_expand.h"
#include "cpp_fs.h"
#include "cpp_expr.h"
#include "cpp_iters.h"

static void warn_if_newline(TokenStream* s) {
    while (!tokens_eof(s) && !tokens_hit_line(s)) {
        tokens_next(s);
    }
}

static void expect_no_newline(TokenStream* s, int start_line) {
    // preprocessor usually expects it's statement to be on the same line
    // TODO(NeGate): make this a real error message
    assert(!tokens_hit_line(s));
}

static String get_pp_tokens_until_newline(Cuik_CPP* ctx, TokenStream* s) {
    const unsigned char* start = s->tokens[s->current].start;
    const unsigned char* end = start;

    bool is_str = tokens_is(s, TOKEN_STRING_WIDE_SINGLE_QUOTE) || tokens_is(s, TOKEN_STRING_WIDE_DOUBLE_QUOTE);
    while (!tokens_eof(s) && !tokens_hit_line(s)) {
        end = s->tokens[s->current].end;
        tokens_next(s);
    }

    if (is_str) {
        start -= 1;
    }
    return (String){ .length = end - start, .data = start };
}

static String get_token_as_string(TokenStream* restrict in) {
    Token* t = tokens_get(in);
    return (String){ .length = t->end - t->start, .data = t->start };
}

CUIK_API const char* cuikpp_get_main_file(TokenStream* tokens) {
    return tokens->filepath;
}

CUIK_API bool cuikpp_is_in_main_file(TokenStream* tokens, SourceLocIndex loc) {
    if (loc != 0) {
        return false;
    }

    SourceLoc* l = &tokens->locations[loc];
    return l->line->filepath == tokens->filepath;
}

CUIK_API void cuikpp_init(Cuik_CPP* ctx, const char filepath[FILENAME_MAX]) {
    size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
    size_t sz2 = sizeof(SourceLocIndex) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

    *ctx = (Cuik_CPP){
        .files = dyn_array_create(Cuik_FileEntry),

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
    ctx->files = dyn_array_create(Cuik_FileEntry);

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
    ctx->tokens.locations = dyn_array_create(SourceLoc);
    ctx->tokens.tokens = dyn_array_create(Token);

    tls_init();

    ctx->state1 = CUIK__CPP_FIRST_FILE;
    ctx->stack[0] = (CPPStackSlot){
        .filepath = filepath,
        .directory = directory,
    };
}

CUIK_API TokenStream cuiklex_buffer(const char* filepath, char* contents) {
    return get_all_tokens_in_buffer(filepath, (uint8_t*) contents, NULL);
}

static void print_token_stream(TokenStream* s, size_t start, size_t end) {
    int last_line = 0;

    printf("Tokens:\n");
    for (size_t i = start; i < end; i++) {
        Token* t = &s->tokens[i];
        SourceLoc* loc = &s->locations[t->location];

        if (loc->line != NULL && last_line != loc->line->line) {
            printf("\n  # line %3d\n", loc->line->line);
            last_line = loc->line->line;
        }

        printf("%.*s ", (int) (t->end - t->start), t->start);
    }
    printf("\n\n\n");
}

static void free_token_stream(TokenStream* s) {
    // TODO(NeGate): we wanna free the source locations but that's weird currently
    // so we might invest into smarter allocation schemes here
    dyn_array_destroy(s->locations);
    dyn_array_destroy(s->tokens);
}

static TokenStream get_all_tokens_in_buffer(const char* filepath, uint8_t* data, uint8_t* end) {
    TokenStream s = { filepath };

    CUIK_TIMED_BLOCK("lex: %s", filepath) {
        if (end != NULL) {
            // mark end as the place where that the token_start lands on
            Lexer end_lexer = { filepath, end, end, 1 };
            lexer_read(&end_lexer);

            end = end_lexer.token_start;
        }

        s.locations = dyn_array_create_with_initial_cap(SourceLoc, 8192);
        s.tokens = dyn_array_create_with_initial_cap(Token, 8192);

        Lexer l = { filepath, data, data, 1 };

        int current_line_num = 0;
        SourceLine* current_line = NULL;

        for (;;) {
            lexer_read(&l);
            if (l.token_type == 0 || end == l.token_start) break;

            if (l.line_current == NULL) {
                l.line_current = l.start;
            }

            // slap a source location
            ptrdiff_t columns = l.token_start - l.line_current;
            ptrdiff_t length = l.token_end - l.token_start;
            assert(columns <= UINT16_MAX && length <= UINT16_MAX);

            if (current_line_num != l.current_line) {
                // make a new source line... we'll miss our fallen brother, he's not
                // dead but for when he dies...
                current_line = arena_alloc(&thread_arena, sizeof(SourceLine), _Alignof(SourceLine));
                current_line->filepath = l.filepath;
                current_line->line_str = l.line_current;
                current_line->parent = 0;
                current_line->line = l.current_line;

                current_line_num = l.current_line;
            }

            assert(current_line != NULL);
            dyn_array_put_uninit(s.locations, 1);
            SourceLocIndex loc_index = dyn_array_length(s.locations) - 1;
            s.locations[loc_index] = (SourceLoc) {
                .line = current_line,
                .columns = columns,
                .length = length,
            };

            // insert token
            Token t = { l.token_type, l.hit_line, loc_index, l.token_start, l.token_end };
            dyn_array_put(s.tokens, t);
            l.hit_line = false;
        }

        // Add EOF token
        Token t = {0, true, 0, NULL, NULL};
        dyn_array_put(s.tokens, t);

        // trim up the memory
        // dyn_array_trim(s.locations);
        // dyn_array_trim(s.tokens);
    }

    return s;
}

CUIK_API Cuikpp_Status cuikpp_next(Cuik_CPP* ctx, Cuikpp_Packet* packet) {
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
                packet->file.tokens = (TokenStream){ 0 };
                packet->file.is_primary = true;
                return CUIKPP_CONTINUE;
            }

            // get back a file
            case 1: {
                if (packet->file.tokens.tokens == NULL) {
                    fprintf(stderr, "preprocessor error: could not read file! %s\n", slot->filepath);
                    return CUIKPP_ERROR;
                }

                #if CUIK__CPP_STATS
                ctx->total_io_time += (cuik_time_in_nanos() - slot->start_time);
                ctx->total_files_read += 1;
                #endif

                // initialize the file & lexer in the stack slot
                slot->file_id = dyn_array_length(ctx->files);
                slot->tokens = packet->file.tokens;
                slot->tokens.current = 0;

                // record the file entry
                Cuik_FileEntry file_entry = {
                    .depth = ctx->stack_ptr,
                    .filepath = packet->file.input_path,
                    .tokens = packet->file.tokens,
                };
                dyn_array_put(ctx->files, file_entry);

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

                int loc = tokens_get_location_line(&prev_slot->tokens);
                fprintf(stderr, "error %s:%d: Could not find file! %s\n", prev_slot->tokens.filepath, loc, slot->filepath);
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
            packet->file.tokens = (TokenStream){ 0 };
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
        slot->file_id = dyn_array_length(ctx->files);
        slot->tokens = packet->file.tokens;
        slot->tokens.current = 0;

        // record the file entry
        Cuik_FileEntry file_entry = {
            .depth = ctx->stack_ptr,
            .include_loc = slot->include_loc,
            .filepath = filepath,
            .tokens = packet->file.tokens,
        };
        dyn_array_put(ctx->files, file_entry);

        // we finished resolving
        ctx->state1 = CUIK__CPP_NONE;

        // continue along to the actual preprocessing now
    }

    TokenStream* restrict in = &slot->tokens;
    TokenStream* restrict s = &ctx->tokens;
    SourceLocIndex include_loc = slot->include_loc;

    for (;;) {
        int start_line_for_pp_stmt = tokens_get_location_line(in);

        // FAST PATH: raw tokens
        for (;;) {
            Token t = *tokens_get(in);
            if (t.type == 0 || t.type == TOKEN_HASH || t.type == TOKEN_DOUBLE_HASH) {
                // Fallback to slow path
                break;
            } else if (t.type == TOKEN_IDENTIFIER) {
                if (is_defined(ctx, t.start, t.end - t.start)) break;

                t.type = classify_ident(t.start, t.end - t.start);
            }

            t.location = get_source_location(ctx, in, s, include_loc, SOURCE_LOC_NORMAL);
            dyn_array_put(s->tokens, t);
            tokens_next(in);
        }

        TknType first_token = tokens_get(in)->type;
        if (first_token != 0 && slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "placed stuff outside of the include guard (FAILURE!)");
            slot->include_guard.status = INCLUDE_GUARD_INVALID;
        }

        if (first_token == 0) {
            ctx->stack_ptr -= 1;

            if (slot->include_guard.status == INCLUDE_GUARD_EXPECTING_NOTHING) {
                // the file is practically pragma once
                // fprintf(stderr, "%s: this file has an include guard around it called '%.*s'\n", slot->filepath, (int)slot->include_guard.define.length, (const char*)slot->include_guard.define.data);
                nl_strmap_put_cstr(ctx->include_once, (const char*) slot->filepath, 0);
            } else {
                // fprintf(stderr, "%s: could not detect include guard\n", slot->filepath);
            }

            // write out profile entry
            if (cuik_is_profiling()) {
                cuik_profile_region_end();
            }

            // free the token stream if we have ownership
            if (in->is_owned) {
                dyn_array_destroy(in->tokens);
                dyn_array_destroy(in->locations);
            }

            // if this is the last file, just exit
            if (ctx->stack_ptr == 0) {
                // place last token
                Token t = {0, true, 0, NULL, NULL};
                dyn_array_put(s->tokens, t);

                s->current = 0;
                return CUIKPP_DONE;
            }

            // step out of this file into the previous one
            slot = &ctx->stack[ctx->stack_ptr - 1];

            in = &slot->tokens;
            include_loc = slot->include_loc;
            continue;
        } else if (first_token == TOKEN_HASH) {
            // all the directives go here
            bool success = false;

            SourceLocIndex directive_loc = tokens_get_location_index(in);
            tokens_next(in); // skip the hash
            String directive = get_token_as_string(in);

            switch (directive.length) {
                // 'if' EXPR NEWLINE
                case 2:
                if (memcmp(directive.data, "if", 2) == 0) {
                    success = true;
                    SourceLocIndex loc = get_source_location(ctx, in, s, include_loc, SOURCE_LOC_NORMAL);
                    tokens_next(in);

                    expect_no_newline(in, start_line_for_pp_stmt);
                    if (eval(ctx, s, in, loc)) {
                        push_scope(ctx, in, true, directive_loc);
                    } else {
                        push_scope(ctx, in, false, directive_loc);
                        skip_directive_body(in);
                    }

                    // we should be one a different line now
                    warn_if_newline(in);
                }
                break;

                // 'elif' EXPR NEWLINE
                case 4:
                if (memcmp(directive.data, "elif", 4) == 0) {
                    success = true;
                    SourceLocIndex loc = get_source_location(ctx, in, s, include_loc, SOURCE_LOC_NORMAL);
                    tokens_next(in);

                    expect_no_newline(in, start_line_for_pp_stmt);

                    // if it didn't evaluate any of the other options
                    // try to do this
                    int last_scope = ctx->depth - 1;

                    if (!ctx->scope_eval[last_scope] && eval(ctx, s, in, loc)) {
                        ctx->scope_eval[last_scope] = true;
                    } else {
                        skip_directive_body(in);
                    }

                    // we should be one a different line now
                    warn_if_newline(in);
                } else if (memcmp(directive.data, "else", 4) == 0) {
                    success = true;
                    tokens_next(in);

                    // if it didn't evaluate any of the other options
                    // do this
                    int last_scope = ctx->depth - 1;

                    if (!ctx->scope_eval[last_scope]) {
                        ctx->scope_eval[last_scope] = true;
                    } else {
                        skip_directive_body(in);
                    }
                }
                break;

                // 'undef' IDENT     NEWLINE
                // 'error' PP-TOKENS NEWLINE
                // 'endif'           NEWLINE
                case 5:
                if (memcmp(directive.data, "undef", 5) == 0) {
                    success = true;
                    tokens_next(in);

                    if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                        generic_error(in, "expected identifier!");
                    }

                    String key = get_token_as_string(in);
                    tokens_next(in);

                    // Hash name
                    uint64_t slot = hash_ident(key.data, key.length);
                    size_t base = slot * SLOTS_PER_MACRO_BUCKET;
                    size_t count = ctx->macro_bucket_count[slot];

                    // TODO(NeGate): We might wanna invest into a faster data structure.
                    for (size_t i = 0; i < count; i++) {
                        size_t e = base + i;

                        if (ctx->macro_bucket_keys_length[e] == key.length &&
                            memcmp(ctx->macro_bucket_keys[e], key.data, key.length) == 0) {
                            // remove swap
                            size_t last = base + (count - 1);

                            if (i != last) {
                                ctx->macro_bucket_keys_length[e] = ctx->macro_bucket_keys_length[last];
                                ctx->macro_bucket_keys[e] = ctx->macro_bucket_keys[last];
                                ctx->macro_bucket_values_start[e] = ctx->macro_bucket_values_start[last];
                                ctx->macro_bucket_values_end[e] = ctx->macro_bucket_values_end[last];
                                ctx->macro_bucket_source_locs[e] = ctx->macro_bucket_source_locs[last];
                            }
                            ctx->macro_bucket_count[slot] -= 1;
                            break;
                        }
                    }
                } else if (memcmp(directive.data, "error", 5) == 0) {
                    success = true;
                    SourceLocIndex loc = get_source_location(
                        ctx, in, s, include_loc, SOURCE_LOC_NORMAL
                    );

                    tokens_next(in);
                    String msg = get_pp_tokens_until_newline(ctx, in);

                    report(
                        REPORT_ERROR, NULL, s, loc,
                        "directive: %.*s", (int)msg.length, msg.data
                    );
                    return CUIKPP_DONE;
                } else if (memcmp(directive.data, "ifdef", 5) == 0) {
                    success = true;
                    tokens_next(in);

                    if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                        generic_error(in, "expected identifier!");
                    }

                    Token* t = tokens_get(in);
                    if (is_defined(ctx, t->start, t->end - t->start)) {
                        push_scope(ctx, in, true, directive_loc);
                        tokens_next(in);
                    } else {
                        push_scope(ctx, in, false, directive_loc);
                        skip_directive_body(in);
                    }
                } else if (memcmp(directive.data, "endif", 5) == 0) {
                    success = true;
                    tokens_next(in);

                    if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_ENDIF && slot->include_guard.if_depth == ctx->depth) {
                        if (!is_defined(ctx, slot->include_guard.define.data, slot->include_guard.define.length)) {
                            // the ifndef's macro needs to stay defined or else the include guard doesn't make sense
                            slot->include_guard.status = INCLUDE_GUARD_INVALID;
                            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "INVALID");
                        } else {
                            slot->include_guard.status = INCLUDE_GUARD_EXPECTING_NOTHING;
                            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "EXPECTING_NOTHING");
                        }
                    }

                    warn_if_newline(in);
                    pop_scope(ctx, in, directive_loc);
                }
                break;

                // 'define' IDENT PP-TOKENS NEWLINE
                // 'pragma' PP-TOKENS       NEWLINE
                // 'ifndef' IDENT           NEWLINE
                case 6:
                if (memcmp(directive.data, "define", 6) == 0) {
                    success = true;
                    tokens_next(in);

                    SourceLocIndex macro_loc = get_source_location(
                        ctx, in, s, include_loc, SOURCE_LOC_MACRO
                    );
                    if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                        generic_error(in, "expected identifier!");
                    }

                    // Hash name
                    String key = get_token_as_string(in);
                    if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_DEFINE) {
                        // as long as the define matches the include guard we're good
                        if (string_equals(&slot->include_guard.define, &key)) {
                            slot->include_guard.status = INCLUDE_GUARD_LOOKING_FOR_ENDIF;
                            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "LOOKING_FOR_ENDIF");
                        } else {
                            slot->include_guard.status = INCLUDE_GUARD_INVALID;
                            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "INVALID");
                        }
                    }

                    uint64_t slot = hash_ident(key.data, key.length);
                    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

                    // Insert into buckets
                    if (ctx->macro_bucket_count[slot] >= SLOTS_PER_MACRO_BUCKET) {
                        generic_error(in, "cannot store macro, out of memory!");
                    }

                    ctx->macro_bucket_count[slot] += 1;
                    ctx->macro_bucket_keys[e] = key.data;
                    ctx->macro_bucket_keys_length[e] = key.length;

                    // if there's a parenthesis directly after the identifier
                    // it's a macro function... yes this is an purposeful off-by-one
                    // it's mostly ok tho
                    if (key.data[key.length] == '(') {
                        tokens_next(in);
                        expect(in, '(');

                        int arg_count = 0;
                        while (!tokens_is(in, ')')) {
                            if (arg_count) {
                                expect(in, ',');
                            }

                            if (!tokens_is(in, TOKEN_TRIPLE_DOT) && !tokens_is(in, TOKEN_IDENTIFIER)) {
                                generic_error(in, "expected identifier!");
                            }

                            tokens_next(in);
                            arg_count++;
                        }

                        expect_no_newline(in, start_line_for_pp_stmt);
                        expect(in, ')');
                    } else {
                        expect_no_newline(in, start_line_for_pp_stmt);
                        tokens_next(in);
                    }

                    String value = get_pp_tokens_until_newline(ctx, in);

                    ctx->macro_bucket_values_start[e] = value.data;
                    ctx->macro_bucket_values_end[e] = value.data + value.length;
                    ctx->macro_bucket_source_locs[e] = macro_loc;
                } else if (memcmp(directive.data, "pragma", 6) == 0) {
                    success = true;
                    tokens_next(in);

                    SourceLocIndex loc = get_source_location(
                        ctx, in, s, include_loc, SOURCE_LOC_NORMAL
                    );

                    if (tokens_match(in, 4, "once")) {
                        nl_strmap_put_cstr(ctx->include_once, (const char*) slot->filepath, 0);
                        tokens_next(in);

                        // We gotta hit a line by now
                        warn_if_newline(in);
                    } else if (tokens_match(in, 7, "message")) {
                        success = true;
                        tokens_next(in);

                        String msg = get_pp_tokens_until_newline(ctx, in);
                        report(
                            REPORT_INFO, NULL, s, loc,
                            "directive: %.*s", (int)msg.length, msg.data
                        );
                    } else {
                        // convert to #pragma blah => _Pragma("blah")
                        unsigned char* str = gimme_the_shtuffs(ctx, sizeof("_Pragma"));
                        memcpy(str, "_Pragma", sizeof("_Pragma"));
                        Token t = { TOKEN_KW_Pragma, false, loc, str, str + 7 };
                        dyn_array_put(s->tokens, t);

                        str = gimme_the_shtuffs(ctx, sizeof("("));
                        str[0] = '(';
                        str[1] = 0;
                        t = (Token){ '(', false, loc, str, str + 1 };
                        dyn_array_put(s->tokens, t);

                        // Skip until we hit a newline
                        expect_no_newline(in, start_line_for_pp_stmt);

                        String payload = get_pp_tokens_until_newline(ctx, in);

                        // convert pragma content into string
                        {
                            str = gimme_the_shtuffs(ctx, (payload.length * 2) + 3);
                            unsigned char* curr = str;

                            *curr++ = '\"';
                            for (size_t i = 0; i < payload.length; i++) {
                                if (payload.data[i] == '\"') {
                                    *curr++ = '\\';
                                    *curr++ = '\"';
                                } else {
                                    *curr++ = payload.data[i];
                                }
                            }
                            *curr++ = '\"';
                            *curr++ = '\0';

                            t = (Token){ TOKEN_STRING_DOUBLE_QUOTE, false, loc, str, curr - 1 };
                            dyn_array_put(s->tokens, t);
                        }

                        str = gimme_the_shtuffs(ctx, sizeof(")"));
                        str[0] = ')';
                        str[1] = 0;
                        t = (Token){ ')', false, loc, str, str + 1 };
                        dyn_array_put(s->tokens, t);
                    }
                } else if (memcmp(directive.data, "ifndef", 6) == 0) {
                    success = true;
                    tokens_next(in);

                    if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                        generic_error(in, "expected identifier!");
                    }

                    Token* t = tokens_get(in);
                    if (!is_defined(ctx, t->start, t->end - t->start)) {
                        push_scope(ctx, in, true, directive_loc);
                        tokens_next(in);

                        // if we don't skip the body then maybe just maybe it's a guard macro
                        if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_IFNDEF) {
                            slot->include_guard.status = INCLUDE_GUARD_LOOKING_FOR_DEFINE;
                            slot->include_guard.define = string_from_range(t->start, t->end);
                            slot->include_guard.if_depth = ctx->depth;
                            // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "LOOKING_FOR_DEFINE");
                        }
                    } else {
                        push_scope(ctx, in, false, directive_loc);
                        skip_directive_body(in);
                    }
                }
                break;

                // 'include' HSTRING NEWLINE
                //           QSTRING NEWLINE
                //
                // 'warning' PP-TOKENS NEWLINE
                case 7:
                if (memcmp(directive.data, "include", 7) == 0) {
                    success = true;
                    SourceLocIndex new_include_loc = get_source_location(
                        ctx, in, s, include_loc, SOURCE_LOC_NORMAL
                    );
                    tokens_next(in);

                    char* filename = gimme_the_shtuffs(ctx, MAX_PATH);

                    bool is_lib_include = false;
                    // Evaluate
                    if (tokens_is(in, '<')) {
                        tokens_next(in);

                        is_lib_include = true;
                        size_t len = 0;

                        // Hacky but mostly works
                        do {
                            Token* t = tokens_get(in);
                            size_t token_len = t->end - t->start;
                            if (len + token_len > MAX_PATH) {
                                generic_error(in, "filename too long!");
                            }

                            memcpy(&filename[len], t->start, token_len);
                            len += token_len;

                            tokens_next(in);
                        } while (!tokens_is(in, '>'));

                        // slap that null terminator on it like a boss bitch
                        filename[len] = '\0';

                        if (!tokens_is(in, '>')) {
                            generic_error(in, "expected '>' for #include");
                        }

                        tokens_next(in);
                    } else {
                        size_t old_tokens_length = dyn_array_length(s->tokens);
                        s->current = old_tokens_length;

                        expand(ctx, s, in, dyn_array_length(in->tokens), true, new_include_loc);
                        assert(s->current != dyn_array_length(s->tokens) && "Expected the macro expansion to add something");

                        // Insert a null token at the end
                        Token t = {0, true, dyn_array_length(s->locations) - 1, NULL, NULL};
                        dyn_array_put(s->tokens, t);

                        if (tokens_is(s, TOKEN_STRING_DOUBLE_QUOTE)) {
                            Token* t2 = tokens_get(s);
                            size_t len = (t2->end - t2->start) - 2;
                            if (len > MAX_PATH) {
                                report(REPORT_ERROR, NULL, s, t2->location, "Filename too long");
                                abort();
                            }

                            memcpy(filename, t2->start + 1, len);
                            filename[len] = '\0';

                            tokens_next(s);
                        } else {
                            generic_error(in, "expected file path!");
                        }

                        // reset token stream
                        dyn_array_set_length(s->tokens, old_tokens_length);
                        s->current = 0;
                    }

                    // insert incomplete new stack slot
                    ctx->stack[ctx->stack_ptr++] = (CPPStackSlot){
                        .filepath = filename,
                        .include_loc = new_include_loc,
                        .start_time = cuik_time_in_nanos()
                    };

                    // reset the state machine
                    ctx->state1 = is_lib_include ? CUIK__CPP_LIB_INCLUDE : CUIK__CPP_USR_INCLUDE;

                    // we'll trim_the_shtuffs once we've resolved a name
                    char* path = gimme_the_shtuffs(ctx, FILENAME_MAX);
                    size_t num_system_include_dirs = dyn_array_length(ctx->system_include_dirs);

                    // quote includes will prioritize the local directory over the search paths
                    // if we don't have any search paths then we'll also run this first since it's
                    // our only real option.
                    if (!is_lib_include || (num_system_include_dirs == 0 && is_lib_include)) {
                        #if CUIK__CPP_STATS
                        ctx->total_fstats += 1;
                        #endif

                        // Try local includes
                        ctx->state2 = 0;
                        sprintf_s(path, FILENAME_MAX, "%s%s", slot->directory, filename);
                    } else {
                        // try the first include search path
                        assert(num_system_include_dirs > 0);

                        ctx->state2 = 1;
                        sprintf_s(path, FILENAME_MAX, "%s%s", ctx->system_include_dirs[0], filename);
                    }

                    packet->tag = CUIKPP_PACKET_QUERY_FILE;
                    packet->file.input_path = path;
                    packet->file.tokens = (TokenStream){ 0 };
                    return CUIKPP_CONTINUE;
                } else if (memcmp(directive.data, "warning", 7) == 0) {
                    success = true;
                    SourceLocIndex loc = get_source_location(
                        ctx, in, s, include_loc, SOURCE_LOC_NORMAL
                    );

                    String msg = get_pp_tokens_until_newline(ctx, in);
                    report(
                        REPORT_WARNING, NULL, s, loc,
                        "directive: %.*s", (int)msg.length, msg.data
                    );
                }
                break;

                default: break;
            }

            if (!success) {
                report(
                    REPORT_ERROR, NULL, in, directive_loc,
                    "unknown directive: %.*s", (int)directive.length, directive.data
                );
                return CUIKPP_ERROR;
            }
        } else if (first_token == TOKEN_DOUBLE_HASH) {
            tokens_next(in);

            assert(dyn_array_length(s->tokens) > 0);
            Token* last = &s->tokens[dyn_array_length(s->tokens) - 1];

            expand_double_hash(ctx, s, last, in, last->location);
        } else if (first_token == TOKEN_IDENTIFIER) {
            // check if it's actually a macro, if not categorize it if it's a keyword
            SourceLocIndex loc = get_source_location(ctx, in, s, include_loc, SOURCE_LOC_NORMAL);

            Token* t = tokens_get(in);
            if (!is_defined(ctx, t->start, t->end - t->start)) {
                // FAST PATH
                Token final_token = {
                    classify_ident(t->start, t->end - t->start),
                    t->hit_line, loc, t->start, t->end,
                };
                dyn_array_put(s->tokens, final_token);

                tokens_next(in);
            } else {
                // SLOW PATH BECAUSE IT NEEDS TO SPAWN POSSIBLY METRIC SHIT LOADS
                // OF TOKENS AND EXPAND WITH THE AVERAGE C PREPROCESSOR SPOOKIES
                expand_ident(ctx, s, in, loc);
            }
        }
    }
}

CUIK_API void cuikpp_deinit(Cuik_CPP* ctx) {
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

    cuik__vfree((void*)ctx->the_shtuffs, THE_SHTUFFS_SIZE);
    dyn_array_destroy(ctx->files);
    ctx->the_shtuffs = NULL;
    ctx->files = NULL;
}

CUIK_API Cuikpp_Status cuikpp_default_run(Cuik_CPP* ctx, Cuik_FileCache* cache) {
    Cuikpp_Packet packet;
    for (;;) {
        Cuikpp_Status status = cuikpp_next(ctx, &packet);
        if (status != CUIKPP_CONTINUE) return status;

        cuikpp_default_packet_handler(ctx, &packet, cache);
    }
}

CUIK_API void cuikpp_finalize(Cuik_CPP* ctx) {
    CUIK_TIMED_BLOCK("cuikpp_finalize") {
        size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
        size_t sz2 = sizeof(SourceLocIndex) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

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

CUIK_API TokenStream* cuikpp_get_token_stream(Cuik_CPP* ctx) {
    return &ctx->tokens;
}

CUIK_API size_t cuikpp_get_file_table_count(Cuik_CPP* ctx) {
    return dyn_array_length(ctx->files);
}

CUIK_API Cuik_FileEntry* cuikpp_get_file_table(Cuik_CPP* ctx) {
    return &ctx->files[0];
}

CUIK_API void cuikpp_dump(Cuik_CPP* ctx) {
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

static SourceLocIndex get_source_location(Cuik_CPP* restrict c, TokenStream* restrict in, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type) {
    // just extract the one that's attached to the input token and chop it's heading
    SourceLoc* old = &in->locations[in->tokens[in->current].location];

    if (c->last_source_line == NULL || c->last_source_line->line != old->line->line) {
        // make a new source line... we'll miss our fallen brother, he's not
        // dead but for when he dies...
        SourceLine* l = arena_alloc(&thread_arena, sizeof(SourceLine), _Alignof(SourceLine));
        l->filepath = old->line->filepath;
        l->line_str = old->line->line_str;
        l->parent = parent_loc;
        l->line = old->line->line;

        c->last_source_line = l;
    }

    // generate the output source locs (we don't wanna keep the old streams)
    dyn_array_put_uninit(s->locations, 1);
    SourceLocIndex loc_index = dyn_array_length(s->locations) - 1;
    s->locations[loc_index].type = loc_type;
    s->locations[loc_index].line = c->last_source_line;
    s->locations[loc_index].columns = old->columns;
    s->locations[loc_index].length = old->length;

    return loc_index;
}

static void push_scope(Cuik_CPP* restrict ctx, TokenStream* restrict in, bool initial, SourceLocIndex loc) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
        generic_error(in, "Exceeded max scope depth!");
    }

    // report(REPORT_INFO, NULL, in, loc, "PUSH");
    ctx->scope_eval[ctx->depth++] = initial;
}

static void pop_scope(Cuik_CPP* restrict ctx, TokenStream* restrict in, SourceLocIndex loc) {
    // report(REPORT_INFO, NULL, in, loc, "POP");
    if (ctx->depth == 0) {
        generic_error(in, "Too many endifs\n");
    }
    ctx->depth--;
}

static void skip_directive_body(TokenStream* restrict in) {
    // report(REPORT_INFO, NULL, in, tokens_get_last_location_index(in), "SKIP START");
    int depth = 0;

    while (!tokens_eof(in)) {
        if (tokens_is(in, '#')) {
            tokens_next(in);

            if (tokens_is(in, TOKEN_IDENTIFIER)) {
                if (tokens_match(in, 2, "if") || tokens_match(in, 5, "ifdef") || tokens_match(in, 6, "ifndef")) {
                    depth++;
                } else if (tokens_match(in, 4, "elif") || tokens_match(in, 4, "else")) {
                    // else/elif does both entering a scope and exiting one
                    if (depth == 0) {
                        in->current -= 1;
                        // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "SKIP END");
                        return;
                    }
                } else if (tokens_match(in, 5, "endif")) {
                    if (depth == 0) {
                        // revert both the identifier and hash
                        in->current -= 1;
                        // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "SKIP END");
                        return;
                    }
                    depth--;
                }
            }
        }

        tokens_next(in);
    }

    generic_error(in, "Unclosed macro conditional");
}

static _Noreturn void generic_error(TokenStream* restrict in, const char* msg) {
    fprintf(stderr, "error %s:%d: %s\n", in->filepath, tokens_get_location_line(in), msg);
    abort();
}

static void expect(TokenStream* restrict in, char ch) {
    if (!tokens_is(in, ch)) {
        Token* t = tokens_get(in);
        report(REPORT_ERROR, NULL, in, tokens_get_location_index(in), "expected '%c' got '%.*s'", ch, (int)(t->end - t->start), t->start);
        abort();
    }

    tokens_next(in);
}
