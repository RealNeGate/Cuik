// TODO(NeGate): Refactor this code...
//
// NOTE(NeGate): This code leaks the filename strings but it doesn't actually matter
// because this is a compiler and momma aint raised no bitch.
//
// NOTE(NeGate): the_shtuffs is the simple linear allocator in this preprocessor, just avoids
// wasting time on the heap allocator
#include "cpp.h"
#include <str.h>
#include <diagnostic.h>
#include <memory.h>
#include <timer.h>
#include <front/file_io.h>
#include <sys/stat.h>
#include <stb_ds.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <x86intrin.h>

static void preprocess_file(Cuik_CPP* restrict c, TokenStream* restrict s, size_t parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth);
static uint64_t hash_ident(const unsigned char* at, size_t length);
static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length);
static void expect(Lexer* l, char ch);
static void skip_directive_body(Lexer* l);
static intmax_t eval(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static _Noreturn void generic_error(Lexer* l, const char* msg);

static void* gimme_the_shtuffs(Cuik_CPP* restrict c, size_t len);
static void trim_the_shtuffs(Cuik_CPP* restrict c, void* new_top);
static SourceLocIndex get_source_location(Cuik_CPP* restrict c, Lexer* restrict l, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type);

static void expand(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static void expand_ident(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
//static void expand_double_hash(Cuik_CPP* restrict c, TokenStream* restrict s, Token* last, Lexer* restrict l, SourceLocIndex loc);

#define MAX_CPP_STACK_DEPTH 1024

typedef struct CPPStackSlot {
    const char* filepath;
    const char* directory;

    size_t file_id;
    SourceLocIndex include_loc;

    Lexer l;
} CPPStackSlot;

// Basically a mini-unity build that takes up just the CPP module
#include "cpp_symtab.h"
#include "cpp_expand.h"
#include "cpp_fs.h"
#include "cpp_expr.h"
#include "cpp_raw.h"

/*static CPPStackSlot* push_file(Cuik_CPP* ctx, const char* filepath, size_t parent_entry, SourceLocIndex include_loc, int depth) {
    #if CUIK__CPP_STATS
    uint64_t start = cuik_time_in_nanos();
    #endif

    Cuik_File file = CUIK_CALL(ctx->file_system, get_file, false, filepath);
    if (!file.found) {
        panic("preprocessor error: could not read file! %s\n", filepath);
    }

    // convert all the weird whitespace into something normal
    remove_weird_whitespace(file.length, file.data);
    unsigned char* text = (unsigned char*)file.data;

    // record the file entry
    size_t file_entry_id = dyn_array_length(ctx->files);
    Cuik_FileEntry file_entry = {
        .parent_id = parent_entry,
        .depth = depth,
        .include_loc = include_loc,
        .filepath = filepath,
        .content = text,
        .content_len = file.length,
    };
    dyn_array_put(ctx->files, file_entry);

    CPPStackSlot* slot = &ctx->stack[ctx->stack_ptr++];
    *slot = (CPPStackSlot){
        .filepath = filepath,
        .file_id = file_entry_id,
        .l = { filepath, text, text, 1 },
    };
    lexer_read(&slot->l);

    #if CUIK__CPP_STATS
    uint64_t end = cuik_time_in_nanos();
    c->total_files_read += 1;
    c->total_io_time = (io_time_done - timer_start);
    #endif

    return slot;
}

static CPPStackSlot* pop_file(Cuik_CPP* ctx) {
    assert(ctx->stack_ptr > 0);
    ctx->stack_ptr -= 1;

    return ctx->stack_ptr != 0 ? &ctx->stack[ctx->stack_ptr - 1] : NULL;
}*/

static String get_pp_tokens_until_newline(Lexer* restrict l) {
    const unsigned char* start = l->token_start;
    const unsigned char* end = l->token_start;
    while (!l->hit_line) {
        end = l->token_end;
        lexer_read(l);
    }

    return (String){ .length = end - start, .data = start };
}

static String get_token_as_string(Lexer* l) {
    return (String){ .length = l->token_end - l->token_start, .data = l->token_start };
}

CUIK_API void cuikpp_init(Cuik_CPP* ctx, const Cuik_IFileSystem* fs, const char filepath[FILENAME_MAX]) {
    // default amirite
    if (fs == NULL) fs = &cuik_default_fs;

    size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
    size_t sz2 = sizeof(SourceLocIndex) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

    *ctx = (Cuik_CPP){
        .file_system = fs,
        .files = dyn_array_create(Cuik_FileEntry),

        .stack = cuik__valloc(MAX_CPP_STACK_DEPTH * sizeof(CPPStackSlot)),

        .macro_bucket_keys = cuik__valloc(sz),
        .macro_bucket_keys_length = cuik__valloc(sz),
        .macro_bucket_values_start = cuik__valloc(sz),
        .macro_bucket_values_end = cuik__valloc(sz),
        .macro_bucket_source_locs = cuik__valloc(sz2),

        .the_shtuffs = cuik__valloc(THE_SHTUFFS_SIZE),
    };

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

    ctx->stack[0] = (CPPStackSlot){ .filepath = filepath, .directory = directory };
    ctx->state1 = CUIK__CPP_FIRST_FILE;

    tls_init();
}

CUIK_API bool cuikpp_next(Cuik_CPP* ctx, Cuikpp_Packet* packet) {
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
                packet->tag = CUIKPP_PACKET_GET_FILE;
                packet->get_file.input_path = slot->filepath;
                packet->get_file.found = false;
                packet->get_file.content_length = 0;
                packet->get_file.content = NULL;
                return true;
            }

            // get back a file
            case 1: {
                if (!packet->get_file.found && packet->get_file.content_length == 0) {
                    fprintf(stderr, "preprocessor error: could not read file! %s\n", slot->filepath);
                    return false;
                }

                const char* filepath = packet->get_file.input_path;
                size_t file_length = packet->get_file.content_length;
                uint8_t* file_data = packet->get_file.content;

                // convert all the weird whitespace into something normal
                remove_weird_whitespace(file_length, file_data);

                // initialize the file & lexer in the stack slot
                slot->file_id = dyn_array_length(ctx->files);
                slot->l = (Lexer){ filepath, file_data, file_data, 1 };
                lexer_read(&slot->l);

                // record the file entry
                Cuik_FileEntry file_entry = {
                    .filepath = filepath,
                    .content = file_data,
                    .content_len = file_length,
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
        if (!packet->get_file.found && packet->get_file.content_length == 0) {
            #if CUIK__CPP_STATS
            ctx->total_fstats += 1;
            #endif

            // we didn't find a match
            ctx->state2 += 1;
            int index = ctx->state2 - 1;

            int endpoint = arrlen(ctx->system_include_dirs);
            if (index == endpoint) {
                assert(ctx->stack_ptr > 1);
                CPPStackSlot* restrict prev_slot = &ctx->stack[ctx->stack_ptr - 2];

                int loc = prev_slot->l.current_line;
                fprintf(stderr, "error %s:%d: Could not find file! %s\n", prev_slot->l.filepath, loc, slot->filepath);
                return false;
            }

            // ask for the next filepath
            // it's ok this const removal is based
            char* path = (char*) packet->get_file.input_path;
            sprintf_s(path, FILENAME_MAX, "%s%s", ctx->system_include_dirs[index], slot->filepath);

            packet->get_file.found = false;
            packet->get_file.content_length = 0;
            packet->get_file.content = NULL;
            return true;
        }

        const char* filepath = packet->get_file.input_path;

        packet->tag = CUIKPP_PACKET_CANONICALIZE;
        packet->canonicalize.input_path = filepath;
        packet->canonicalize.output_path = arena_alloc(&thread_arena, FILENAME_MAX, 1);

        // we finished resolving
        ctx->state1 = CUIK__CPP_CANONICALIZE;
        return true;
    } else if (ctx->state1 == CUIK__CPP_CANONICALIZE) {
        const char* filepath = packet->canonicalize.output_path;

        ptrdiff_t search = shgeti(ctx->include_once, filepath);
        if (search >= 0) {
            // revert since it's only allowed to include once and we already did it
            ctx->stack_ptr -= 1;
            return true;
        }

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
        packet->get_file.input_path = filepath;
        packet->get_file.found = false;
        packet->get_file.content_length = 0;
        packet->get_file.content = NULL;
        return true;
    } else if (ctx->state1 == CUIK__CPP_GET_FILE) {
        const char* filepath = packet->get_file.input_path;
        size_t file_length = packet->get_file.content_length;
        uint8_t* file_data = packet->get_file.content;

        // convert all the weird whitespace into something normal
        remove_weird_whitespace(file_length, file_data);

        // initialize the file & lexer in the stack slot
        slot->file_id = dyn_array_length(ctx->files);
        slot->l = (Lexer){ filepath, file_data, file_data, 1 };
        lexer_read(&slot->l);

        // record the file entry
        Cuik_FileEntry file_entry = {
            .filepath = filepath,
            .content = file_data,
            .content_len = file_length,
        };
        dyn_array_put(ctx->files, file_entry);

        // we finished resolving
        ctx->state1 = CUIK__CPP_NONE;

        // continue along to the actual preprocessing now
    }

    Lexer* restrict l = &slot->l;
    TokenStream* restrict s = &ctx->tokens;
    SourceLocIndex include_loc = slot->include_loc;

    // this is set to true whenever a line is hit, we
    // reset it before running preproc stuff
    l->hit_line = false;
    if (l->token_type == TOKEN_HASH) {
        // all the directives go here
        bool success = false;

        lexer_read(l); // skip the hash
        String directive = get_token_as_string(l);

        switch (directive.length) {
            // 'undef' IDENT NEWLINE
            // 'error' PP-TOKENS NEWLINE
            case 5:
            if (memcmp(directive.data, "undef", 5) == 0) {
                success = true;
                lexer_read(l);

                if (l->token_type != TOKEN_IDENTIFIER) {
                    generic_error(l, "expected identifier!");
                }

                String key = get_token_as_string(l);
                lexer_read(l);

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
                    ctx, l, s, include_loc, SOURCE_LOC_NORMAL
                );

                String msg = get_pp_tokens_until_newline(l);
                report(
                    REPORT_ERROR, NULL, s, loc,
                    "directive: %.*s", (int)msg.length, msg.data
                );
                return false;
            }
            break;

            // 'define' IDENT PP-TOKENS NEWLINE
            case 6:
            if (memcmp(directive.data, "define", 6) == 0) {
                lexer_read(l);

                SourceLocIndex macro_loc = get_source_location(
                    ctx, l, s, include_loc, SOURCE_LOC_MACRO
                );
                if (l->token_type != TOKEN_IDENTIFIER) {
                    generic_error(l, "expected identifier!");
                }

                // Hash name
                uint64_t slot = hash_ident(l->token_start, l->token_end - l->token_start);
                uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

                // Insert into buckets
                if (ctx->macro_bucket_count[slot] >= SLOTS_PER_MACRO_BUCKET) {
                    generic_error(l, "cannot store macro, out of memory!");
                }

                ctx->macro_bucket_count[slot] += 1;
                ctx->macro_bucket_keys[e] = l->token_start;

                size_t token_length = l->token_end - l->token_start;
                ctx->macro_bucket_keys_length[e] = token_length;

                // if there's a parenthesis directly after the identifier
                // it's a macro function
                if (*l->token_end == '(') {
                    lexer_read(l);
                    expect(l, '(');

                    int arg_count = 0;
                    while (l->token_type != ')') {
                        if (arg_count) {
                            expect(l, ',');
                        }

                        if (l->token_type != TOKEN_TRIPLE_DOT &&
                            l->token_type != TOKEN_IDENTIFIER) {
                            generic_error(l, "expected identifier!");
                        }

                        lexer_read(l);
                        arg_count++;
                    }

                    assert(!l->hit_line);
                    expect(l, ')');
                } else {
                    // Skip until we hit a newline
                    assert(!l->hit_line);
                    lexer_read(l);
                }

                String value = get_pp_tokens_until_newline(l);

                ctx->macro_bucket_values_start[e] = value.data;
                ctx->macro_bucket_values_end[e] = value.data + value.length;
                ctx->macro_bucket_source_locs[e] = macro_loc;
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
                    ctx, l, s, include_loc, SOURCE_LOC_NORMAL
                );
                lexer_read(l);

                #if CUIK__CPP_STATS
                uint64_t include_start_time = cuik_time_in_nanos();
                #endif

                char* filename = gimme_the_shtuffs(ctx, MAX_PATH);

                bool is_lib_include = false;
                // Evaluate
                if (l->token_type == '<') {
                    lexer_read(l);

                    is_lib_include = true;
                    size_t len = 0;

                    // Hacky but mostly works
                    do {
                        size_t token_len = l->token_end - l->token_start;
                        if (len + token_len > MAX_PATH) {
                            generic_error(l, "filename too long!");
                        }

                        memcpy(&filename[len], l->token_start, token_len);
                        len += token_len;

                        lexer_read(l);
                    } while (l->token_type != '>');

                    // slap that null terminator on it like a boss bitch
                    filename[len] = '\0';

                    if (l->token_type != '>') {
                        generic_error(l, "expected '>' for #include");
                    }

                    lexer_read(l);
                } else {
                    size_t old_tokens_length = arrlen(s->tokens);
                    s->current = old_tokens_length;

                    expand(ctx, s, l, new_include_loc);
                    assert(s->current != arrlen(s->tokens) && "Expected the macro expansion to add something");

                    // Insert a null token at the end
                    Token t = {0, arrlen(s->locations) - 1, NULL, NULL};
                    arrput(s->tokens, t);

                    if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
                        Token* t = tokens_get(s);
                        size_t len = (t->end - t->start) - 2;
                        if (len > MAX_PATH) {
                            report(REPORT_ERROR, NULL, s, t->location, "Filename too long");
                            abort();
                        }

                        memcpy(filename, t->start + 1, len);
                        filename[len] = '\0';

                        tokens_next(s);
                    } else {
                        generic_error(l, "expected file path!");
                    }

                    // reset token stream
                    arrsetlen(s->tokens, old_tokens_length);
                    s->current = 0;
                }

                // insert incomplete new stack slot
                ctx->stack[ctx->stack_ptr++] = (CPPStackSlot){ .filepath = filename };

                // reset the state machine
                ctx->state1 = is_lib_include ? CUIK__CPP_LIB_INCLUDE : CUIK__CPP_USR_INCLUDE;

                // we'll trim_the_shtuffs once we've resolved a name
                char* path = gimme_the_shtuffs(ctx, FILENAME_MAX);
                size_t num_system_include_dirs = arrlen(ctx->system_include_dirs);

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
                packet->get_file.input_path = path;
                packet->get_file.found = false;
                packet->get_file.content_length = 0;
                packet->get_file.content = NULL;
                return true;
            } else if (memcmp(directive.data, "warning", 7) == 0) {
                success = true;
                SourceLocIndex loc = get_source_location(
                    ctx, l, s, include_loc, SOURCE_LOC_NORMAL
                );

                String msg = get_pp_tokens_until_newline(l);
                report(
                    REPORT_ERROR, NULL, s, loc,
                    "directive: %.*s", (int)msg.length, msg.data
                );
                return false;
            }
            break;

            default: break;
        }

        if (!success) {
            generic_error(l, "unknown directive!");
            return false;
        }
    } else if (l->token_type == TOKEN_IDENTIFIER) {
        // check if it's actually a macro, if not categorize it if it's a keyword
        __debugbreak();
    } else {
        __debugbreak();
    }

    return false;
}

CUIK_API void cuikpp_deinit(Cuik_CPP* ctx) {
    #if CUIK__CPP_STATS
    printf("%40s | %f us / %zu file read (%f ms / %zu fstats)\n",
        ctx->files[0].filepath,
        ctx->total_io_time / 1000.0, ctx->total_files_read,
        ctx->total_include_time / 10000000.0, ctx->total_fstats);
    #endif

    if (ctx->macro_bucket_keys) {
        cuikpp_finalize(ctx);
    }

    if (ctx->files != NULL) {
        size_t count = dyn_array_length(ctx->files);

        for (size_t i = 0; i < count; i++) {
            CUIK_CALL(ctx->file_system, free_file, &ctx->files[i]);
        }

        dyn_array_destroy(ctx->files);
        ctx->files = NULL;
    }

    cuik__vfree((void*)ctx->the_shtuffs, THE_SHTUFFS_SIZE);
    ctx->the_shtuffs = NULL;
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

CUIK_API size_t cuikpp_get_file_table_count(Cuik_CPP* ctx) {
    return dyn_array_length(ctx->files);
}

CUIK_API Cuik_FileEntry* cuikpp_get_file_table(Cuik_CPP* ctx) {
    return &ctx->files[0];
}

CUIK_API bool cuikpp_find_include_include(Cuik_CPP* ctx, char output[MAX_PATH], const char* path) {
    size_t num_system_include_dirs = arrlen(ctx->system_include_dirs);

    for (size_t i = 0; i < num_system_include_dirs; i++) {
        sprintf_s(output, FILENAME_MAX, "%s%s", ctx->system_include_dirs[i], path);

        if (CUIK_CALL(ctx->file_system, get_file, true, output).found) {
            return true;
        }
    }

    return false;
}

CUIK_API void cuikpp_add_include_directory(Cuik_CPP* ctx, const char dir[]) {
    arrput(ctx->system_include_dirs, strdup(dir));
}

CUIK_API Cuik_DefineRef cuikpp_first_define(Cuik_CPP* ctx) {
    for (int i = 0; i < MACRO_BUCKET_COUNT; i++) {
        if (ctx->macro_bucket_count[i] != 0) {
            // first slot in that non-empty bucket
            return (Cuik_DefineRef){ i, 0 };
        }
    }

    return (Cuik_DefineRef){ MACRO_BUCKET_COUNT, 0 };
}

CUIK_API bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineRef* src) {
    // we outta bounds
    if (src->bucket >= MACRO_BUCKET_COUNT) return false;

    // find next bucket
    src->id += 1;
    while (src->id >= ctx->macro_bucket_count[src->bucket]) {
        src->id = 0;
        src->bucket += 1;

        if (src->bucket >= MACRO_BUCKET_COUNT) {
            return false;
        }
    }

    return true;
}

CUIK_API Cuik_Define cuikpp_get_define(Cuik_CPP* ctx, Cuik_DefineRef src) {
    size_t e = (src.bucket * SLOTS_PER_MACRO_BUCKET) + src.id;

    size_t keylen = ctx->macro_bucket_keys_length[e];
    const char* key = (const char*)ctx->macro_bucket_keys[e];

    size_t vallen = ctx->macro_bucket_values_end[e] - ctx->macro_bucket_values_start[e];
    const char* val = (const char*)ctx->macro_bucket_values_start[e];

    return (Cuik_Define){
        .loc = ctx->macro_bucket_source_locs[e],
        .key = { keylen, key },
        .value = { vallen, val },
    };
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

CUIK_API TokenStream cuikpp_run(Cuik_CPP* ctx, const char filepath[]) {
    char* slash = strrchr(filepath, '\\');
    if (!slash) slash = strrchr(filepath, '/');

    char directory[FILENAME_MAX];
    if (slash) {
        #if _WIN32
        sprintf_s(directory, FILENAME_MAX, "%.*s\\", (int)(slash - filepath), filepath);
        #else
        snprintf(directory, FILENAME_MAX, "%.*s/", (int)(slash - filepath), filepath);
        #endif
    } else {
        directory[0] = '\0';
    }

    TokenStream s = { .filepath = filepath };
    s.filepath = filepath;
    arrsetcap(s.tokens, 0x25000);

    preprocess_file(ctx, &s, 0, 0, directory, filepath, 1);

    Token t = {0, 0, NULL, NULL};
    arrput(s.tokens, t);

    // printf("%zu\n", arrlen(s.tokens));
    return s;
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

static SourceLocIndex get_source_location(Cuik_CPP* restrict c, Lexer* restrict l, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type) {
    SourceLocIndex i = arrlen(s->locations);
    if (l->line_current == NULL) {
        l->line_current = l->start;
    }

    // we only make a new one if things change
    SourceLine* source_line = NULL;

    if (c->current_source_line == NULL ||
        c->current_source_line->filepath != l->filepath ||
        c->current_source_line->parent != parent_loc ||
        c->current_source_line->line != l->current_line) {
        source_line = arena_alloc(&thread_arena, sizeof(SourceLine), _Alignof(SourceLine));
        source_line->filepath = l->filepath;
        source_line->line_str = l->line_current;
        source_line->parent = parent_loc;
        source_line->line = l->current_line;

        c->current_source_line = source_line;
    } else {
        source_line = c->current_source_line;
    }

    ptrdiff_t columns = l->token_start - l->line_current;
    ptrdiff_t length = l->token_end - l->token_start;
    assert(columns >= 0 && columns < UINT32_MAX && length >= 0 && length < UINT32_MAX);

    SourceLoc loc = {
        .line = source_line,
        .columns = columns,
        .length = length,
    };
    arrput(s->locations, loc);

    return SOURCE_LOC_SET_TYPE(loc_type, i);
}

static void push_scope(Cuik_CPP* restrict ctx, Lexer* restrict l, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
        generic_error(l, "Exceeded max scope depth!");
    }

    ctx->scope_eval[ctx->depth++] = initial;
}

static void pop_scope(Cuik_CPP* restrict ctx, Lexer* restrict l) {
    if (ctx->depth == 0) {
        generic_error(l, "Too many endifs\n");
    }
    ctx->depth--;
}

static void preprocess_file(Cuik_CPP* restrict c, TokenStream* restrict s, size_t parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth) {
    // hacky but i don't wanna wrap it in a timed_block
    uint64_t timer_start = cuik_time_in_nanos();

    Cuik_File file = CUIK_CALL(c->file_system, get_file, false, filepath);
    if (!file.found) {
        panic("preprocessor error: could not read file! %s\n", filepath);
    }

    // convert all the weird whitespace into something normal
    unsigned char* text = (unsigned char*)file.data;
    remove_weird_whitespace(file.length, text);

    size_t file_entry_id = dyn_array_length(c->files);
    Cuik_FileEntry file_entry = {
        .parent_id = parent_entry,
        .depth = depth,
        .include_loc = include_loc,
        .filepath = filepath,
        .content = text,
        .content_len = file.length,
    };
    dyn_array_put(c->files, file_entry);

    #if CUIK__CPP_STATS
    uint64_t io_time_done = cuik_time_in_nanos();
    c->total_files_read += 1;
    c->total_io_time = (io_time_done - timer_start);
    #endif

    Lexer l = {filepath, text, text, 1};
    lexer_read(&l);
    do {
        l.hit_line = false;

        if (l.token_type == TOKEN_IDENTIFIER) {
            SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);

            if (!is_defined(c, l.token_start, l.token_end - l.token_start)) {
                // FAST PATH
                Token t = {
                    classify_ident(l.token_start, l.token_end - l.token_start),
                    loc,
                    l.token_start, l.token_end,
                };
                arrput(s->tokens, t);

                lexer_read(&l);
            } else {
                // SLOW PATH BECAUSE IT NEEDS TO SPAWN POSSIBLY METRIC SHIT LOADS
                // OF TOKENS AND EXPAND WITH THE AVERAGE C PREPROCESSOR SPOOKIES
                expand_ident(c, s, &l, loc);
            }
        } else if (l.token_type == TOKEN_DOUBLE_HASH) {
            int line = l.current_line;
            lexer_read(&l);

            assert(arrlen(s->tokens) > 0);
            Token* last = &s->tokens[arrlen(s->tokens) - 1];

            expand_double_hash(c, s, last, &l, line);
        } else if (l.token_type == '#') {
            lexer_read(&l);

            if (l.token_type == TOKEN_IDENTIFIER) {
                if (lexer_match(&l, 2, "if")) {
                    SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_MACRO);
                    lexer_read(&l);

                    assert(!l.hit_line);
                    if (eval(c, s, &l, loc)) {
                        push_scope(c, &l, true);
                    } else {
                        push_scope(c, &l, false);
                        skip_directive_body(&l);
                    }

                    // we should be one a different line now
                    assert(l.hit_line);
                } else if (lexer_match(&l, 6, "ifndef")) {
                    lexer_read(&l);

                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    if (!is_defined(c, l.token_start, l.token_end - l.token_start)) {
                        push_scope(c, &l, true);
                        lexer_read(&l);
                    } else {
                        push_scope(c, &l, false);
                        skip_directive_body(&l);
                    }
                } else if (lexer_match(&l, 5, "ifdef")) {
                    lexer_read(&l);

                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    if (is_defined(c, l.token_start, l.token_end - l.token_start)) {
                        push_scope(c, &l, true);
                        lexer_read(&l);
                    } else {
                        push_scope(c, &l, false);
                        skip_directive_body(&l);
                    }
                } else if (lexer_match(&l, 4, "else")) {
                    lexer_read(&l);
                    //assert(l.hit_line);

                    // if it didn't evaluate any of the other options
                    // do this
                    int last_scope = c->depth - 1;

                    if (!c->scope_eval[last_scope]) {
                        c->scope_eval[last_scope] = true;
                    } else {
                        skip_directive_body(&l);
                    }
                } else if (lexer_match(&l, 4, "elif")) {
                    SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_MACRO);
                    lexer_read(&l);

                    assert(!l.hit_line);

                    // if it didn't evaluate any of the other options
                    // try to do this
                    int last_scope = c->depth - 1;

                    if (!c->scope_eval[last_scope] && eval(c, s, &l, loc)) {
                        c->scope_eval[last_scope] = true;
                    } else {
                        skip_directive_body(&l);
                    }

                    // we should be one a different line now
                    assert(l.hit_line);
                } else if (lexer_match(&l, 5, "endif")) {
                    lexer_read(&l);

                    if (!l.hit_line) {
                        // TODO(NeGate): warning people if they add extra tokens
                        // to the endif
                        while (!l.hit_line) lexer_read(&l);
                    }

                    pop_scope(c, &l);
                } else if (lexer_match(&l, 6, "define")) {
                    lexer_read(&l);

                    SourceLocIndex macro_loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_MACRO);
                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    // Hash name
                    uint64_t slot = hash_ident(l.token_start, l.token_end - l.token_start);
                    uint64_t e = c->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

                    // Insert into buckets
                    if (c->macro_bucket_count[slot] >= SLOTS_PER_MACRO_BUCKET) {
                        generic_error(&l, "cannot store macro, out of memory!");
                    }

                    c->macro_bucket_count[slot] += 1;
                    c->macro_bucket_keys[e] = l.token_start;

                    size_t token_length = l.token_end - l.token_start;
                    c->macro_bucket_keys_length[e] = token_length;

                    // if there's a parenthesis directly after the identifier
                    // it's a macro function
                    if (*l.token_end == '(') {
                        lexer_read(&l);
                        expect(&l, '(');

                        int arg_count = 0;
                        while (l.token_type != ')') {
                            if (arg_count) {
                                expect(&l, ',');
                            }

                            if (l.token_type != TOKEN_TRIPLE_DOT &&
                                l.token_type != TOKEN_IDENTIFIER) {
                                generic_error(&l, "expected identifier!");
                            }

                            lexer_read(&l);
                            arg_count++;
                        }

                        assert(!l.hit_line);
                        expect(&l, ')');
                    } else {
                        // Skip until we hit a newline
                        assert(!l.hit_line);
                        lexer_read(&l);
                    }

                    const unsigned char* start = l.token_start;
                    const unsigned char* end = l.token_start;
                    while (!l.hit_line) {
                        end = l.token_end;
                        lexer_read(&l);
                    }

                    c->macro_bucket_values_start[e] = start;
                    c->macro_bucket_values_end[e] = end;
                    c->macro_bucket_source_locs[e] = macro_loc;
                } else if (lexer_match(&l, 7, "include")) {
                    SourceLocIndex new_include_loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);
                    lexer_read(&l);

                    #if CUIK__CPP_STATS
                    uint64_t include_start_time = cuik_time_in_nanos();
                    #endif

                    unsigned char* filename = tls_push(MAX_PATH);

                    bool is_lib_include = false;
                    // Evaluate
                    if (l.token_type == '<') {
                        lexer_read(&l);

                        is_lib_include = true;
                        size_t len = 0;

                        // Hacky but mostly works
                        do {
                            size_t token_len = l.token_end - l.token_start;
                            if (len + token_len > MAX_PATH) {
                                generic_error(&l, "filename too long!");
                            }

                            memcpy(&filename[len], l.token_start, token_len);
                            len += token_len;

                            lexer_read(&l);
                        } while (l.token_type != '>');

                        // slap that null terminator on it like a boss bitch
                        filename[len] = '\0';

                        if (l.token_type != '>') {
                            generic_error(&l, "expected '>' for #include");
                        }

                        lexer_read(&l);
                    } else {
                        size_t old_tokens_length = arrlen(s->tokens);
                        s->current = old_tokens_length;

                        expand(c, s, &l, new_include_loc);
                        assert(s->current != arrlen(s->tokens) && "Expected the macro expansion to add something");

                        // Insert a null token at the end
                        Token t = {0, arrlen(s->locations) - 1, NULL, NULL};
                        arrput(s->tokens, t);

                        if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
                            Token* t = tokens_get(s);
                            size_t len = (t->end - t->start) - 2;
                            if (len > MAX_PATH) {
                                report(REPORT_ERROR, NULL, s, t->location, "Filename too long");
                                abort();
                            }

                            memcpy(filename, t->start + 1, len);
                            filename[len] = '\0';

                            tokens_next(s);
                        } else {
                            generic_error(&l, "expected file path!");
                        }

                        // reset token stream
                        arrsetlen(s->tokens, old_tokens_length);
                        s->current = 0;
                    }

                    // Search for file in system libs
                    char path[FILENAME_MAX];
                    bool success = false;

                    if (!is_lib_include) {
                        // Try local includes
                        sprintf_s(path, FILENAME_MAX, "%s%s", directory, filename);

                        #if CUIK__CPP_STATS
                        c->total_fstats += 1;
                        #endif

                        if (CUIK_CALL(c->file_system, get_file, true, path).found) success = true;
                    }

                    if (!success) {
                        size_t num_system_include_dirs = arrlen(c->system_include_dirs);

                        for (size_t i = 0; i < num_system_include_dirs; i++) {
                            sprintf_s(path, FILENAME_MAX, "%s%s", c->system_include_dirs[i], filename);

                            #if CUIK__CPP_STATS
                            c->total_fstats += 1;
                            #endif

                            if (CUIK_CALL(c->file_system, get_file, true, path).found) {                                success = true;
                                break;
                            }
                        }
                    }

                    if (!success && is_lib_include) {
                        // Try local includes
                        sprintf_s(path, FILENAME_MAX, "%s%s", directory, filename);

                        #if CUIK__CPP_STATS
                        c->total_fstats += 1;
                        #endif

                        if (CUIK_CALL(c->file_system, get_file, true, path).found) {
                            success = true;
                        }
                    }

                    if (!success) {
                        int loc = l.current_line;
                        fprintf(stderr, "error %s:%d: Could not find file! %s\n", l.filepath, loc, filename);
                        abort();
                    }
                    tls_restore(filename);

                    // get me an absolute path
                    char* new_path = arena_alloc(&thread_arena, FILENAME_MAX, 1);
                    CUIK_CALL(c->file_system, canonicalize, new_path, path);

                    ptrdiff_t search = shgeti(c->include_once, new_path);

                    #if CUIK__CPP_STATS
                    uint64_t include_end_time = cuik_time_in_nanos();
                    c->total_include_time += (include_end_time - include_start_time);
                    #endif

                    if (search < 0) {
                        char* slash = strrchr(new_path, '/');
                        if (!slash) slash = strrchr(new_path, '\\');

                        char* new_dir;
                        if (slash) {
                            size_t slash_pos = slash - new_path;

                            new_dir = arena_alloc(&thread_arena, slash_pos + 2, 1);
                            memcpy(new_dir, new_path, slash_pos);
                            new_dir[slash_pos] = '/';
                            new_dir[slash_pos + 1] = 0;
                        } else {
                            new_dir = arena_alloc(&thread_arena, 2, 1);
                            new_dir[0] = '/';
                            new_dir[1] = 0;
                        }

                        if (0) {
                            for (int i = 0; i < depth; i++) printf("  ");
                            printf("%s : %s\n", new_dir, new_path);
                        }

                        preprocess_file(c, s, file_entry_id, new_include_loc, new_dir, new_path, depth + 1);
                    }
                } else if (lexer_match(&l, 6, "pragma")) {
                    lexer_read(&l);

                    if (lexer_match(&l, 4, "once")) {
                        shput(c->include_once, (char*)filepath, 0);
                        lexer_read(&l);

                        // We gotta hit a line by now
                        assert(l.hit_line);
                    } else if (lexer_match(&l, 7, "message")) {
                        lexer_read(&l);

                        printf("message %s:%d:  ", l.filepath, l.current_line);
                        while (!l.hit_line) {
                            printf("%.*s ", (int)(l.token_end - l.token_start), l.token_start);
                            lexer_read(&l);
                        }
                        printf("\n");
                    } else {
                        // convert to #pragma blah => _Pragma("blah")
                        SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);

                        unsigned char* str = gimme_the_shtuffs(c, sizeof("_Pragma"));
                        memcpy(str, "_Pragma", sizeof("_Pragma"));
                        Token t = (Token){TOKEN_KW_Pragma, loc, str, str + 7};
                        arrput(s->tokens, t);

                        str = gimme_the_shtuffs(c, sizeof("("));
                        str[0] = '(';
                        str[1] = 0;
                        t = (Token){'(', loc, str, str + 1};
                        arrput(s->tokens, t);

                        // Skip until we hit a newline
                        assert(!l.hit_line);

                        const unsigned char* start = l.token_start;
                        const unsigned char* end = l.token_start;
                        while (!l.hit_line) {
                            end = l.token_end;
                            lexer_read(&l);
                        }

                        // convert pragma content into string
                        {
                            size_t len = end - start;
                            str = gimme_the_shtuffs(c, (len * 2) + 3);
                            unsigned char* curr = str;

                            *curr++ = '\"';
                            for (size_t i = 0; i < len; i++) {
                                if (start[i] == '\"') {
                                    *curr++ = '\\';
                                    *curr++ = '\"';
                                } else {
                                    *curr++ = start[i];
                                }
                            }
                            *curr++ = '\"';
                            *curr++ = '\0';

                            t = (Token){
                                TOKEN_STRING_DOUBLE_QUOTE,
                                loc,
                                str, curr - 1,
                            };
                            arrput(s->tokens, t);
                        }

                        str = gimme_the_shtuffs(c, sizeof(")"));
                        str[0] = ')';
                        str[1] = 0;
                        t = (Token){
                            ')',
                            loc,
                            str, str + 1,
                        };
                        arrput(s->tokens, t);
                    }
                } else if (lexer_match(&l, 5, "undef")) {
                    lexer_read(&l);

                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    const unsigned char* start = l.token_start;
                    size_t length = l.token_end - l.token_start;

                    lexer_read(&l);

                    // Hash name
                    uint64_t slot = hash_ident(start, length);
                    uint64_t base = slot * SLOTS_PER_MACRO_BUCKET;
                    uint64_t count = c->macro_bucket_count[slot];

                    // TODO(NeGate): We might wanna invest into a faster data structure.
                    for (size_t i = 0; i < count; i++) {
                        uint64_t e = base + i;

                        if (c->macro_bucket_keys_length[e] == length &&
                            memcmp(c->macro_bucket_keys[e], start, length) == 0) {
                            // remove swap
                            uint64_t last = base + (count - 1);

                            if (i != last) {
                                c->macro_bucket_keys_length[e] = c->macro_bucket_keys_length[last];
                                c->macro_bucket_keys[e] = c->macro_bucket_keys[last];
                                c->macro_bucket_values_start[e] = c->macro_bucket_values_start[last];
                                c->macro_bucket_values_end[e] = c->macro_bucket_values_end[last];
                                c->macro_bucket_source_locs[e] = c->macro_bucket_source_locs[last];
                            }
                            c->macro_bucket_count[slot]--;
                            break;
                        }
                    }
                } else if (lexer_match(&l, 7, "warning")) {
                    lexer_read(&l);

                    SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);

                    const unsigned char* start = l.token_start;
                    const unsigned char* end = l.token_start;
                    while (!l.hit_line) {
                        end = l.token_end;
                        lexer_read(&l);
                    }

                    report(REPORT_WARNING, NULL, s, loc, "directive: %.*s", (int)(end - start), start);
                } else if (lexer_match(&l, 5, "error")) {
                    lexer_read(&l);

                    SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);

                    const unsigned char* start = l.token_start;
                    const unsigned char* end = l.token_start;
                    while (!l.hit_line) {
                        end = l.token_end;
                        lexer_read(&l);
                    }

                    report(REPORT_ERROR, NULL, s, loc, "directive: %.*s", (int)(end - start), start);
                    exit(1);
                } else {
                    generic_error(&l, "unknown directive!");
                }
            } else {
                generic_error(&l, "unknown directive!");
            }
        } else {
            Token t = {
                l.token_type,
                get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL),
                l.token_start,
                l.token_end,
            };

            arrput(s->tokens, t);
            lexer_read(&l);
        }
    } while (l.token_type);

    {
        char temp[256];
        snprintf(temp, sizeof(temp), "preprocess: %s", filepath);
        temp[sizeof(temp) - 1] = 0;

        char* p = temp;
        for (; *p; p++) {
            if (*p == '\\') *p = '/';
        }

        cuik_profile_region(timer_start, "%s", temp);
    }
}

static void skip_directive_body(Lexer* l) {
    int depth = 0;

    do {
        if (l->token_type == '#') {
            // TODO(NeGate): Future me... fix it
            Lexer saved = *l;
            lexer_read(l);

            if (l->token_type == TOKEN_IDENTIFIER) {
                if (lexer_match(l, 2, "if")) depth++;
                if (lexer_match(l, 5, "ifdef"))
                    depth++;
                else if (lexer_match(l, 6, "ifndef"))
                    depth++;
                else if (lexer_match(l, 4, "elif") || lexer_match(l, 4, "else")) {
                    // else/elif does both entering a scope and exiting one
                    if (depth == 0) {
                        *l = saved;
                        return;
                    }
                } else if (lexer_match(l, 5, "endif")) {
                    if (depth == 0) {
                        *l = saved;
                        return;
                    }
                    depth--;
                }
            }
        }

        lexer_read(l);
    } while (l->token_type);

    generic_error(l, "Unclosed macro conditional");
}

static _Noreturn void generic_error(Lexer* l, const char* msg) {
    int loc = l->current_line;
    fprintf(stderr, "error %s:%d: %s\n", l->filepath, loc, msg);
    abort();
}

static void expect(Lexer* l, char ch) {
    if (l->token_type != ch) {
        int loc = l->current_line;
        fprintf(stderr, "error %s:%d: expected '%c' got '%.*s'", l->filepath, loc, ch, (int)(l->token_end - l->token_start), l->token_start);
        abort();
    }

    lexer_read(l);
}
