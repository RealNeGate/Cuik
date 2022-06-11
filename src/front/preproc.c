// TODO(NeGate): Refactor this code...

// NOTE(NeGate): This code leaks the filename strings but it doesn't actually matter
// because this is a compiler and momma aint raised no bitch.
#include "preproc.h"
#include "memory.h"
#include "../timer.h"
#include "file_io.h"
#include "diagnostic.h"
#include <ext/stb_ds.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define strdup(x) _strdup(x)
#endif

#include <intrin.h>

static void preprocess_file(CPP_Context* restrict c, TokenStream* restrict s, CPP_FileEntry* parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth);
static uint64_t hash_ident(const unsigned char* at, size_t length);
static bool is_defined(CPP_Context* restrict c, const unsigned char* start, size_t length);
static void expect(Lexer* l, char ch);
static void skip_directive_body(Lexer* l);
static intmax_t eval(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static _Noreturn void generic_error(Lexer* l, const char* msg);

static void expand_ident(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static void expand(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static void* gimme_the_shtuffs(CPP_Context* restrict c, size_t len);

static bool find_define(CPP_Context* restrict c, size_t* out_index, const unsigned char* start, size_t length);
static size_t insert_define(CPP_Context* restrict c, const unsigned char* start, size_t length);

void cpp_init(CPP_Context* ctx) {
    size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
    size_t sz2 = sizeof(SourceLocIndex) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

    *ctx = (CPP_Context){
        .macro_bucket_keys = malloc(sz),
        .macro_bucket_keys_length = malloc(sz),
        .macro_bucket_values_start = malloc(sz),
        .macro_bucket_values_end = malloc(sz),
        .macro_bucket_source_locs = malloc(sz2),

        .the_shtuffs = malloc(THE_SHTUFFS_SIZE)
    };
    ctx->files = big_array_create(CPP_FileEntry, false);

    tls_init();
}

void cpp_deinit(CPP_Context* ctx) {
    if (ctx->macro_bucket_keys) {
        cpp_finalize(ctx);
    }

    if (ctx->files != NULL) {
        cpp_free_file_memory(ctx);
    }

    free((void*)ctx->the_shtuffs);
    ctx->the_shtuffs = NULL;
}

void cpp_finalize(CPP_Context* ctx) {
    //__builtin_dump_struct(&perf_tally, &printf);
    //cpp_dump(ctx);

    free((void*)ctx->macro_bucket_keys);
    free((void*)ctx->macro_bucket_keys_length);
    free((void*)ctx->macro_bucket_values_start);
    free((void*)ctx->macro_bucket_values_end);
    free((void*)ctx->macro_bucket_source_locs);

    ctx->macro_bucket_keys = NULL;
    ctx->macro_bucket_keys_length = NULL;
    ctx->macro_bucket_values_start = NULL;
    ctx->macro_bucket_values_end = NULL;
    ctx->macro_bucket_source_locs = NULL;
}

void cpp_free_file_memory(CPP_Context* ctx) {
    size_t count = big_array_length(ctx->files);

    for (size_t i = 0; i < count; i++) {
        free(ctx->files[i].content);
    }

    big_array_destroy(ctx->files);
    ctx->files = NULL;
}

size_t cpp_get_file_table_count(CPP_Context* ctx) {
    return big_array_length(ctx->files);
}

CPP_FileEntry* cpp_get_file_table(CPP_Context* ctx) {
    return &ctx->files[0];
}

bool cpp_find_include_include(CPP_Context* ctx, char output[MAX_PATH], const char* path) {
    size_t num_system_include_dirs = arrlen(ctx->system_include_dirs);

    for (size_t i = 0; i < num_system_include_dirs; i++) {
        snprintf(output, 260, "%s%s", ctx->system_include_dirs[i], path);

        if (file_exists(output)) {
            return true;
        }
    }

    return false;
}

void cpp_add_include_directory(CPP_Context* ctx, const char dir[]) {
    arrput(ctx->system_include_dirs, strdup(dir));
}

void cpp_define_empty(CPP_Context* ctx, const char* key) {
    size_t len = strlen(key);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (len + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, len);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < len && *paren != '(') paren++;
    len = *paren == '(' ? paren - newkey : len;

    uint64_t slot = hash_ident((const unsigned char*)newkey, len);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = len;

    ctx->macro_bucket_values_start[e] = NULL;
    ctx->macro_bucket_values_end[e] = NULL;
    ctx->macro_bucket_source_locs[e] = SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0);
}

void cpp_define(CPP_Context* ctx, const char* key, const char* value) {
    // TODO(NeGate): Fix up this code a bit because i really dislike how the
    // parenthesis are detected
    size_t len = strlen(key);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (len + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, len);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < len && *paren != '(') paren++;
    len = *paren == '(' ? paren - newkey : len;

    uint64_t slot = hash_ident((const unsigned char*)newkey, len);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = len;

    {
        len = strlen(value);

        size_t pad_len = (len + 15) & ~15;
        char* newvalue = gimme_the_shtuffs(ctx, pad_len);
        memcpy(newvalue, value, len);

        size_t rem = pad_len - len;
        memset(newvalue + len, 0, rem);

        ctx->macro_bucket_values_start[e] = (const unsigned char*) newvalue;
        ctx->macro_bucket_values_end[e] = (const unsigned char*) newvalue + len;
        ctx->macro_bucket_source_locs[e] = SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0);
    }
}

void cpp_dump(CPP_Context* ctx) {
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

    printf("\n# Macro defines active: %d\n", count);
}

TokenStream cpp_process(CPP_Context* ctx, const char filepath[]) {
    char* slash = strrchr(filepath, '\\');
    if (!slash) slash = strrchr(filepath, '/');

    char directory[260];
    if (slash) {
#if _WIN32
        sprintf_s(directory, 260, "%.*s\\", (int)(slash - filepath), filepath);
#else
        snprintf(directory, 260, "%.*s/", (int)(slash - filepath), filepath);
#endif
    } else {
        directory[0] = '\0';
    }

    TokenStream s = { 0 };
    preprocess_file(ctx, &s, NULL, 0, directory, filepath, 1);

    Token t = { 0, 0, NULL, NULL };
    arrput(s.tokens, t);

    return s;
}

static void* gimme_the_shtuffs(CPP_Context* restrict c, size_t len) {
    unsigned char* allocation = c->the_shtuffs + c->the_shtuffs_size;

    c->the_shtuffs_size += len;
    if (c->the_shtuffs_size >= THE_SHTUFFS_SIZE) {
        printf("Preprocessor: out of memory!\n");
        abort();
    }

    return allocation;
}

static void trim_the_shtuffs(CPP_Context* restrict c, void* new_top) {
    size_t i = ((uint8_t*)new_top) - c->the_shtuffs;
    assert(i <= c->the_shtuffs_size);
    c->the_shtuffs_size = i;
}

static SourceLocIndex get_source_location(CPP_Context* restrict c, Lexer* restrict l, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type) {
    SourceLocIndex i = arrlen(s->locations);
    if (l->line_current == NULL) {
        l->line_current = l->start;
    }

    // we only make a new one if things change
    SourceLine* source_line = NULL;

    if (c->current_source_line == NULL ||
        c->current_source_line->filepath != l->filepath ||
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
    assert(columns <= UINT16_MAX && length <= UINT16_MAX);

    SourceLoc loc = {
        .line = source_line,
        .columns = columns,
        .length = length
    };
    arrput(s->locations, loc);

    return SOURCE_LOC_SET_TYPE(loc_type, i);
}

static void cpp_push_scope(CPP_Context* restrict ctx, Lexer* restrict l, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH-1) {
        generic_error(l, "Exceeded max scope depth!");
    }

    ctx->scope_eval[ctx->depth++] = initial;
}

static void cpp_pop_scope(CPP_Context* restrict ctx, Lexer* restrict l) {
    if (ctx->depth == 0) {
        generic_error(l, "Too many endifs\n");
    }
    ctx->depth--;
}

// TODO(NeGate): Fix this up please...
static void expand_double_hash(CPP_Context* restrict c, TokenStream* restrict s, Token* last, Lexer* restrict l, SourceLocIndex loc) {
    unsigned char* out_start = gimme_the_shtuffs(c, 256);
    unsigned char* out = out_start;

    // if the concat fails we return here.
    Lexer savepoint;
    {
        // TODO(NeGate): possible buffer overflow here
        // with unchecked memcpys on static and small allocation
        memcpy(out, last->start, last->end - last->start);
        out += last->end - last->start;

        memcpy(out, l->token_start, l->token_end - l->token_start);
        out += l->token_end - l->token_start;

        *out++ = '\0';

        savepoint = *l;
        lexer_read(l);
    }

    // join tokens
    Lexer tmp_lex = (Lexer) { l->filepath, out_start, out_start };
    lexer_read(&tmp_lex);

    // make nice joined token
    if (tmp_lex.token_type == TOKEN_IDENTIFIER) {
        if (!is_defined(c, tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start)) {
            *last = (Token){
                classify_ident(tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start),
                loc,
                tmp_lex.token_start,
                tmp_lex.token_end
            };
        } else {
            arrdelswap(s->tokens, arrlen(s->tokens) - 1);
            expand_ident(c, s, &tmp_lex, loc);
        }
    } else {
        *last = (Token){
            tmp_lex.token_type,
            loc,
            tmp_lex.token_start,
            tmp_lex.token_end
        };
    }
    lexer_read(&tmp_lex);

    // NOTE(NeGate): So you're not supposed to have multiple tokens
    // once you've concaternated but... eh
    if (tmp_lex.token_type) {
        *l = savepoint;
    }
}

static void preprocess_file(CPP_Context* restrict c, TokenStream* restrict s, CPP_FileEntry* parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth) {
    // hacky but i don't wanna wrap it in a timed_block
    uint64_t timer_start = timer_now();

    unsigned char* text = (unsigned char*)read_entire_file(filepath);
    CPP_FileEntry file_entry = {
        .parent = parent_entry,
        .include_loc = include_loc,
        .filepath = filepath,
        .content = text
    };
    big_array_put(c->files, file_entry);

    Lexer l = { filepath, text, text, 1 };
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
                    l.token_start, l.token_end
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
                        cpp_push_scope(c, &l, true);
                    } else {
                        cpp_push_scope(c, &l, false);
                        skip_directive_body(&l);
                    }

                    // we should be one a different line now
                    assert(l.hit_line);
                }  else if (lexer_match(&l, 6, "ifndef")) {
                    lexer_read(&l);

                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    if (!is_defined(c, l.token_start, l.token_end - l.token_start)) {
                        cpp_push_scope(c, &l, true);
                        lexer_read(&l);
                    } else {
                        cpp_push_scope(c, &l, false);
                        skip_directive_body(&l);
                    }
                } else if (lexer_match(&l, 5, "ifdef")) {
                    lexer_read(&l);

                    if (l.token_type != TOKEN_IDENTIFIER) {
                        generic_error(&l, "expected identifier!");
                    }

                    if (is_defined(c, l.token_start, l.token_end - l.token_start)) {
                        cpp_push_scope(c, &l, true);
                        lexer_read(&l);
                    } else {
                        cpp_push_scope(c, &l, false);
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

                    cpp_pop_scope(c, &l);
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
                        Token t = { 0, arrlen(s->locations) - 1, NULL, NULL };
                        arrput(s->tokens, t);

                        if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
                            Token* t = tokens_get(s);
                            size_t len = (t->end - t->start) - 2;
                            if (len > MAX_PATH) {
                                report(REPORT_ERROR, s, t->location, "Filename too long");
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
                    char path[260];
                    bool success = false;

                    if (!is_lib_include) {
                        // Try local includes
                        sprintf_s(path, 260, "%s%s", directory, filename);
                        if (file_exists(path)) success = true;
                    }

                    if (!success) {
                        size_t num_system_include_dirs = arrlen(c->system_include_dirs);

                        for (size_t i = 0; i < num_system_include_dirs; i++) {
                            sprintf_s(path, 260, "%s%s", c->system_include_dirs[i], filename);

                            if (file_exists(path)) {
                                success = true;
                                break;
                            }
                        }
                    }

                    if (!success && is_lib_include) {
                        // Try local includes
                        sprintf_s(path, 260, "%s%s", directory, filename);
                        if (file_exists(path)) success = true;
                    }

                    if (!success) {
                        int loc = l.current_line;
                        printf("error %s:%d: Could not find file! %s\n", l.filepath, loc, filename);
                        abort();
                    }
                    tls_restore(filename);

#ifdef _WIN32
                    char* filepart;
                    char* new_path = arena_alloc(&thread_arena, MAX_PATH, 1);
                    if (GetFullPathNameA(path, MAX_PATH, new_path, &filepart) == 0) {
                        int loc = l.current_line;
                        printf("error %s:%d: Could not resolve path: %s\n", l.filepath, loc, path);
                        abort();
                    }

                    if (filepart == NULL) {
                        int loc = l.current_line;
                        printf("error %s:%d: Cannot include directory %s\n", l.filepath, loc, new_path);
                        abort();
                    }

                    // Convert file paths into something more comfortable
                    // The windows file paths are case insensitive
                    for (char* p = new_path; *p; p++) {
                        if (*p == '\\') {
                            *p = '/';
                        } else if (*p >= 'A' && *p <= 'Z') {
                            *p -= ('A' - 'a');
                        }
                    }
#else
                    char* new_path = arena_alloc(&thread_arena, PATH_MAX, 1);
                    realpath(path, new_path);
#endif

                    ptrdiff_t search = shgeti(c->include_once, new_path);
                    if (search < 0) {
                        // TODO(NeGate): Remove these heap allocations later
                        // they're... evil!!!
#ifdef _WIN32
                        char* new_dir = strdup(new_path);
                        new_dir[filepart - new_path] = '\0';
#else
                        char* new_dir = strdup(new_path);
                        char* slash = strrchr(new_dir, '/');
                        if (slash) {
                            slash[1] = '/';
                        } else {
                            new_dir[0] = '/';
                            new_dir[1] = '\0';
                        }
#endif

                        if (0) {
                            for (int i = 0; i < depth; i++) printf("  ");
                            printf("%s\n", new_path);
                        }

                        preprocess_file(c, s, &file_entry, new_include_loc, new_dir, new_path, depth + 1);
                    }
                } else if (lexer_match(&l, 6, "pragma")) {
                    lexer_read(&l);

                    if (lexer_match(&l, 4, "once")) {
                        shput(c->include_once, (char*)filepath, 0);
                        lexer_read(&l);

                        // We gotta hit a line by now
                        assert(l.hit_line);
                    }  else if (lexer_match(&l, 7, "message")) {
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
                        Token t = (Token) {
                            TOKEN_KW_Pragma, loc, str, str + 7
                        };
                        arrput(s->tokens, t);

                        str = gimme_the_shtuffs(c, sizeof("("));
                        str[0] = '('; str[1] = 0;
                        t = (Token){
                            '(', loc, str, str + 1
                        };
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
                            str = gimme_the_shtuffs(c, (len*2)+3);
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
                                str, curr-1
                            };
                            arrput(s->tokens, t);
                        }

                        str = gimme_the_shtuffs(c, sizeof(")"));
                        str[0] = ')'; str[1] = 0;
                        t = (Token){
                            ')',
                            loc,
                            str, str + 1
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
                                c->macro_bucket_keys_length[e]  = c->macro_bucket_keys_length[last];
                                c->macro_bucket_keys[e]         = c->macro_bucket_keys[last];
                                c->macro_bucket_values_start[e] = c->macro_bucket_values_start[last];
                                c->macro_bucket_values_end[e]   = c->macro_bucket_values_end[last];
                                c->macro_bucket_source_locs[e]  = c->macro_bucket_source_locs[last];
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

                    report(REPORT_WARNING, s, loc, "directive: %.*s", (int)(end - start), start);
                } else if (lexer_match(&l, 5, "error")) {
                    lexer_read(&l);

                    SourceLocIndex loc = get_source_location(c, &l, s, include_loc, SOURCE_LOC_NORMAL);

                    const unsigned char* start = l.token_start;
                    const unsigned char* end = l.token_start;
                    while (!l.hit_line) {
                        end = l.token_end;
                        lexer_read(&l);
                    }

                    report(REPORT_ERROR, s, loc, "directive: %.*s", (int)(end - start), start);
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
                l.token_end
            };

            arrput(s->tokens, t);
            lexer_read(&l);
        }
    } while (l.token_type);

    {
        char temp[256];
        sprintf_s(temp, 256, "preprocess: %s", filepath);

        char* p = temp;
        for (; *p; p++) {
            if (*p == '\\') *p = '/';
        }

        timer_end(timer_start, temp);
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
                if (lexer_match(l, 5, "ifdef")) depth++;
                else if (lexer_match(l, 6, "ifndef")) depth++;
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
    printf("error %s:%d: %s\n", l->filepath, loc, msg);
    abort();
}

static void expect(Lexer* l, char ch) {
    if (l->token_type != ch) {
        int loc = l->current_line;
        printf("error %s:%d: expected '%c' got '%.*s'", l->filepath, loc, ch, (int)(l->token_end - l->token_start), l->token_start);
        abort();
    }

    lexer_read(l);
}

//
// Hashing
// based on the refterm hash by Casey Muratori
//
static char unsigned overhang_mask[32] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0
};

static char unsigned default_seed[16] = {
    178, 201, 95, 240, 40, 41, 143, 216,
    2, 209, 178, 114, 232, 4, 176, 188
};

static uint64_t hash_ident(const unsigned char* at, size_t length) {
#if !USE_INTRIN
    uint32_t hash = 0x811c9dc5;

    for (size_t i = 0; i < length; i++) {
        hash ^= (uint32_t) at[i];
        hash *= 0x01000193; // 32bit magic shit
    }

    return hash % MACRO_BUCKET_COUNT;
#else
    __m128i hash = _mm_cvtsi64_si128(length);
    hash = _mm_xor_si128(hash, _mm_loadu_si128((__m128i*)default_seed));

    size_t chunk_count = length / 16;
    while(chunk_count--) {
        __m128i in = _mm_loadu_si128((__m128i*) at);
        at += 16;

        hash = _mm_xor_si128(hash, in);
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    }

    size_t overhang = length % 16;
    __m128i in = _mm_loadu_si128((__m128i*) at);

    in = _mm_and_si128(in, _mm_loadu_si128((__m128i*) (overhang_mask + 16 - overhang)));
    hash = _mm_xor_si128(hash, in);
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());

    return ((size_t)_mm_extract_epi32(hash, 0)) % MACRO_BUCKET_COUNT;
#endif
}

static bool is_defined(CPP_Context* restrict c, const unsigned char* start, size_t length) {
    size_t garbage;
    return find_define(c, &garbage, start, length);
}

// 16byte based compare
// it doesn't need to be aligned but the valid range must be (len + 15) & ~15
static bool memory_equals16(const unsigned char* src1, const unsigned char* src2, size_t length) {
#if !USE_INTRIN
    return memcmp(src1, src2, length) == 0;
#else
    size_t i = 0;
    size_t chunk_count = length / 16;
    while (chunk_count--) {
        __m128i in1 = _mm_loadu_si128((__m128i*) &src1[i]);
        __m128i in2 = _mm_loadu_si128((__m128i*) &src2[i]);

        int compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
        if (compare != 0xFFFF) return false;

        i += 16;
    }

    size_t overhang = length % 16;
    __m128i mask = _mm_loadu_si128((__m128i*) (overhang_mask + 16 - overhang));

    __m128i in1 = _mm_and_si128(_mm_loadu_si128((__m128i*) &src1[i]), mask);
    __m128i in2 = _mm_and_si128(_mm_loadu_si128((__m128i*) &src2[i]), mask);

    int compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
    return compare == 0xFFFF;
#endif
}

static bool find_define(CPP_Context* restrict c, size_t* out_index, const unsigned char* start, size_t length) {
    uint64_t slot = hash_ident(start, length);
    size_t count = c->macro_bucket_count[slot];
    size_t base = (slot * SLOTS_PER_MACRO_BUCKET);

    size_t i = 0;
    while (i < count) {
        size_t e = base + i;

        if (c->macro_bucket_keys_length[e] == length) {
            if (memory_equals16(c->macro_bucket_keys[e], start, length)) {
                *out_index = e;
                return true;
            }
        }

        i++;
    }

    return false;
}

static void expand_ident(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    size_t token_length = l->token_end - l->token_start;
    const unsigned char* token_data = l->token_start;

    if (lexer_match(l, 8, "__FILE__") || lexer_match(l, 9, "L__FILE__")) {
        // filepath as a string
        unsigned char* out_start = gimme_the_shtuffs(c, MAX_PATH+4);
        unsigned char* out = out_start;

        bool is_wide = (token_data[0] == 'L');
        if (is_wide) *out++ = 'L';

        *out++ = '\"';
        {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*)l->filepath;
            if (strlen(in) >= MAX_PATH) {
                generic_error(l, "preprocessor error: __FILE__ generated a file path that was too long\n");
                abort();
            }

            while (*in) {
                if (*in == '\\') {
                    *out++ = '\\';
                    *out++ = '\\';
                    in++;
                } else {
                    *out++ = *in++;
                }
            }
        }

        *out++ = '\"';
        *out++ = '\0';
        trim_the_shtuffs(c, out);

        Token t = {
            is_wide ? TOKEN_STRING_WIDE_DOUBLE_QUOTE : TOKEN_STRING_DOUBLE_QUOTE,
            get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
            out_start,
            out
        };
        arrput(s->tokens, t);
        lexer_read(l);
    } else if (lexer_match(l, 8, "__LINE__")) {
        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", l->current_line);

        trim_the_shtuffs(c, &out[length + 1]);
        Token t = {
            TOKEN_INTEGER,
            get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
            out,
            out + length
        };
        arrput(s->tokens, t);
        lexer_read(l);
    } else if (lexer_match(l, 7, "defined")) {
        lexer_read(l);

        const unsigned char* start = NULL;
        const unsigned char* end = NULL;

        // optional parenthesis
        if (l->token_type == '(') {
            lexer_read(l);

            if (l->token_type != TOKEN_IDENTIFIER) {
                generic_error(l, "expected identifier!");
            }

            start = l->token_start;
            end = l->token_end;

            lexer_read(l);
            expect(l, ')');
        } else if (l->token_type == TOKEN_IDENTIFIER) {
            start = l->token_start;
            end = l->token_end;
            lexer_read(l);
        } else {
            generic_error(l, "expected identifier!");
        }

        bool found = is_defined(c, start, end - start);

        // we really just allocated a single byte just to store this lmao
        unsigned char* out = gimme_the_shtuffs(c, 2);
        out[0] = found ? '1' : '0';
        out[1] = '\0';

        //printf("Is '%.*s' defined? %s\n", (int)(end-start), start, found?"Yes":"No");

        Token t = {
            TOKEN_INTEGER,
            get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
            out,
            out + 1
        };
        arrput(s->tokens, t);
    } else {
        size_t def_i;
        if (find_define(c, &def_i, token_data, token_length)) {
            int line_of_expansion = l->current_line;
            SourceLocIndex macro_expansion_loc = get_source_location(c, l, s,
                                                                     c->macro_bucket_source_locs[def_i],
                                                                     SOURCE_LOC_NORMAL);

            // Identify macro definition
            lexer_read(l);

            string def = {
                .data = c->macro_bucket_values_start[def_i],
                .length = c->macro_bucket_values_end[def_i] - c->macro_bucket_values_start[def_i]
            };

            const unsigned char* args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];

            // Sometimes we have a layer of indirection when doing
            // preprocessor expansion:
            //   #define PEAR(X) X;
            //   #define APPLE PEAR
            //   APPLE(int a)
            while (def.length && *args != '(' && l->token_type == '(') {
                // expand and append
                Lexer temp_lex = (Lexer) { l->filepath, def.data, def.data };
                lexer_read(&temp_lex);

                size_t token_length = temp_lex.token_end - temp_lex.token_start;
                const unsigned char* token_data = temp_lex.token_start;

                if (!find_define(c, &def_i, token_data, token_length)) break;

                def = (string){
                    .data = c->macro_bucket_values_start[def_i],
                    .length = c->macro_bucket_values_end[def_i] - c->macro_bucket_values_start[def_i]
                };
                args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];
            }

            // function macro
            if (*args == '(' && l->token_type == '(') {
                ////////////////////////////////
                // Parse the parameters into a map
                ////////////////////////////////
                // make the start and end of the params, interleaved
                // start: value_ranges[i*2 + 0], end: value_ranges[i*2 + 1]
                const unsigned char** value_ranges = tls_save();
                int value_count = 0;

                lexer_read(l);
                while (l->token_type != ')') {
                    tls_push(2 * sizeof(const unsigned char*));
                    int i = value_count++;

                    int paren_depth = 0;
                    const unsigned char* start = l->token_start;
                    const unsigned char* end = l->token_start;
                    while (true) {
                        if (l->token_type == '(') {
                            paren_depth++;
                        } else if (l->token_type == ')') {
                            if (paren_depth == 0) break;
                            paren_depth--;
                        } else if (l->token_type == ',') {
                            if (paren_depth == 0) break;
                        }

                        end = l->token_end;
                        lexer_read(l);
                    }

                    value_ranges[i*2 + 0] = start;
                    value_ranges[i*2 + 1] = end;

                    if (l->token_type == ',') lexer_read(l);
                    l->hit_line = false;
                }
                expect(l, ')');

                // We dont need to parse this part if it expands into nothing
                if (def.length) {
                    // Parse macro function arg names
                    const unsigned char** key_ranges = tls_save();
                    int key_count = 0;
                    bool has_varargs = false;

                    {
                        Lexer arg_lex = (Lexer) { l->filepath, args, args };
                        lexer_read(&arg_lex);
                        expect(&arg_lex, '(');

                        while (arg_lex.token_type != ')') {
                            if (key_count) {
                                expect(&arg_lex, ',');
                            }

                            if (arg_lex.token_type == TOKEN_TRIPLE_DOT) {
                                has_varargs = true;
                                lexer_read(&arg_lex);
                                break;
                            } else if (arg_lex.token_type == TOKEN_IDENTIFIER) {
                                tls_push(2 * sizeof(const unsigned char*));

                                int i = key_count++;
                                key_ranges[i*2 + 0] = arg_lex.token_start;
                                key_ranges[i*2 + 1] = arg_lex.token_end;

                                lexer_read(&arg_lex);
                            } else {
                                generic_error(&arg_lex, "expected identifier or ...!");
                            }
                        }

                        expect(&arg_lex, ')');
                    }

                    ////////////////////////////////
                    // Stream over the text hoping
                    // to replace some identifiers
                    // then expand the result one more
                    // time
                    ////////////////////////////////
                    unsigned char* temp_expansion_start = gimme_the_shtuffs(c, 4096);
                    unsigned char* temp_expansion = temp_expansion_start;

                    Lexer def_lex = (Lexer) { l->filepath, def.data, def.data };
                    def_lex.current_line = line_of_expansion;
                    lexer_read(&def_lex);

                    // set when a # happens, we expect a macro parameter afterwards
                    bool as_string = false;
                    while (!def_lex.hit_line) {
                        // shadowing...
                        size_t token_length = def_lex.token_end - def_lex.token_start;
                        const unsigned char* token_data = def_lex.token_start;

                        if (def_lex.token_type == TOKEN_HASH) {
                            // TODO(NeGate): Error message
                            if (as_string) abort();

                            as_string = true;
                            lexer_read(&def_lex);
                            continue;
                        }

                        if (def_lex.token_type == TOKEN_COMMA) {
                            // we case our idea was dumb :p
                            unsigned char* fallback = temp_expansion;

                            *temp_expansion++ = ',';
                            *temp_expansion++ = ' ';

                            lexer_read(&def_lex);

                            if (has_varargs &&
                                token_length == sizeof("__VA_ARGS__")-1 &&
                                memcmp(token_data, "__VA_ARGS__", sizeof("__VA_ARGS__")-1) == 0 &&
                                key_count == value_count) {
                                // we remove the comma since there's no varargs to chew
                                temp_expansion = fallback;
                            }

                            continue;
                        } else if (def_lex.token_type != TOKEN_IDENTIFIER) {
                            // TODO(NeGate): Error message
                            if (as_string) abort();

                            memcpy(temp_expansion, token_data, token_length);
                            temp_expansion += token_length;
                            *temp_expansion++ = ' ';

                            lexer_read(&def_lex);
                            continue;
                        }

                        if (has_varargs &&
                            token_length == sizeof("__VA_ARGS__")-1 &&
                            memcmp(token_data, "__VA_ARGS__", sizeof("__VA_ARGS__")-1) == 0) {
                            *temp_expansion++ = ' ';

                            // Just slap all the arguments that are after the 'key_count'
                            if (key_count != value_count) {
                                for (size_t i = key_count; i < value_count; i++) {
                                    const unsigned char* end   = value_ranges[i*2 + 1];
                                    const unsigned char* start = value_ranges[i*2 + 0];
                                    size_t count = end-start;

                                    // slap a comma between var args
                                    if (i != key_count) {
                                        *temp_expansion++ = ',';
                                        *temp_expansion++ = ' ';
                                    }

                                    for (size_t j = 0; j < count; j++) {
                                        if (start[j] == '\r' || start[j] == '\n') {
                                            *temp_expansion++ = ' ';
                                        } else {
                                            *temp_expansion++ = start[j];
                                        }
                                    }
                                    *temp_expansion++ = ' ';
                                }
                            } else {
                                // walk back any spaces to find a comma
                                // if we find one, we should delete it
                                unsigned char* p = temp_expansion - 1;
                                while (p != temp_expansion_start) {
                                    if (*p == ',') {
                                        *p = ' ';
                                        break;
                                    }
                                    else if (*p != ' ') break;

                                    p--;
                                }
                            }
                        } else {
                            int index = -1;
                            for (size_t i = 0; i < key_count; i++) {
                                size_t key_length = key_ranges[i*2 + 1] - key_ranges[i*2 + 0];
                                const unsigned char* key = key_ranges[i*2 + 0];

                                if (token_length == key_length &&
                                    memcmp(key, token_data, token_length) == 0) {
                                    index = i;
                                    break;
                                }
                            }

                            if (index >= 0) {
                                const unsigned char* end   = value_ranges[index*2 + 1];
                                const unsigned char* start = value_ranges[index*2 + 0];
                                size_t count = end-start;

                                // Removes any funky characters like " into \"
                                // when stringifying
                                if (as_string) {
                                    *temp_expansion++ = '\"';
                                    for (size_t i = 0; i < count; i++) {
                                        if (start[i] == '\r' || start[i] == '\n') {
                                            *temp_expansion++ = ' ';
                                        } else if (start[i] == '\"' && as_string) {
                                            if (i == 0 || (i > 0 && start[i - 1] != '\\')) {
                                                *temp_expansion++ = '\\';
                                                *temp_expansion++ = '\"';
                                            }
                                        } else {
                                            *temp_expansion++ = start[i];
                                        }
                                    }
                                    *temp_expansion++ = '\"';
                                    *temp_expansion++ = ' ';
                                } else {
                                    for (size_t i = 0; i < count; i++) {
                                        if (start[i] == '\r' || start[i] == '\n') {
                                            *temp_expansion++ = ' ';
                                        } else {
                                            *temp_expansion++ = start[i];
                                        }
                                    }
                                    *temp_expansion++ = ' ';
                                }

                                as_string = false;
                            } else {
                                // TODO(NeGate): Error message
                                if (as_string) *temp_expansion++ = '#';

                                for (size_t i = 0; i < token_length; i++) {
                                    if (token_data[i] == '\r' || token_data[i] == '\n') {
                                        *temp_expansion++ = ' ';
                                    } else {
                                        *temp_expansion++ = token_data[i];
                                    }
                                }
                                *temp_expansion++ = ' ';
                            }
                        }

                        lexer_read(&def_lex);
                    }

                    *temp_expansion++ = '\0';
                    trim_the_shtuffs(c, temp_expansion);

                    if (temp_expansion_start != temp_expansion) {
                        // expand and append
                        Lexer temp_lex = (Lexer){ l->filepath, temp_expansion_start, temp_expansion_start };
                        temp_lex.current_line = line_of_expansion;
                        lexer_read(&temp_lex);

                        *temp_expansion++ = '\0';

                        SourceLocIndex saved_macro_parent_loc = c->macro_source_line;
                        c->macro_source_line = macro_expansion_loc;

                        // NOTE(NeGate): We need to disable the current macro define
                        // so it doesn't recurse.
                        size_t saved_length = c->macro_bucket_keys_length[def_i];
                        c->macro_bucket_keys_length[def_i] = 0;

                        expand(c, s, &temp_lex, macro_expansion_loc);

                        c->macro_bucket_keys_length[def_i] = saved_length;
                        c->macro_source_line = saved_macro_parent_loc;
                    }
                }

                tls_restore(value_ranges);
            } else if (def.length) {
                // expand and append
                if (*args == '(' && l->token_type != '(') {
                    Token t = {
                        classify_ident(token_data, token_length),
                        macro_expansion_loc,
                        token_data,
                        token_data + token_length
                    };

                    arrput(s->tokens, t);
                } else {
                    Lexer temp_lex = (Lexer){ l->filepath, def.data, def.data };
                    temp_lex.current_line = l->current_line;
                    lexer_read(&temp_lex);

                    SourceLocIndex saved_macro_parent_loc = c->macro_source_line;
                    c->macro_source_line = macro_expansion_loc;

                    // NOTE(NeGate): We need to disable the current macro define
                    // so it doesn't recurse.
                    size_t saved_length = c->macro_bucket_keys_length[def_i];
                    c->macro_bucket_keys_length[def_i] = 0;

                    expand(c, s, &temp_lex, macro_expansion_loc);

                    c->macro_bucket_keys_length[def_i] = saved_length;
                    c->macro_source_line = saved_macro_parent_loc;
                }
            }
        } else {
            // Normal identifier
            assert(l->token_type == TOKEN_IDENTIFIER);

            Token t = {
                classify_ident(token_data, token_length),
                get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
                token_data,
                token_data + token_length
            };

            arrput(s->tokens, t);
            lexer_read(l);
        }
    }
}

static void expand(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    int depth = 0;

    while (!l->hit_line) {
        if (l->token_type == '(') {
            depth++;
        }

        if (l->token_type == TOKEN_DOUBLE_HASH) {
            SourceLocIndex loc = get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL);
            lexer_read(l);

            assert(arrlen(s->tokens) > 0);
            Token* last = &s->tokens[arrlen(s->tokens) - 1];

            expand_double_hash(c, s, last, l, loc);
        } else if (l->token_type != TOKEN_IDENTIFIER) {
            Token t = {
                l->token_type,
                get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
                l->token_start,
                l->token_end
            };

            arrput(s->tokens, t);
            lexer_read(l);
        } else {
            expand_ident(c, s, l, parent_loc);
        }

        if (l->token_type == ')') {
            if (depth == 0) break;
            depth--;
        }
    }
}

////////////////////////////////
// Macro expressions
////////////////////////////////
static intmax_t eval_l0(CPP_Context* restrict c, TokenStream* restrict s) {
    bool flip = false;
    while (tokens_get(s)->type == '!') {
        flip = !flip;
        tokens_next(s);
    }

    intmax_t val;
    Token* t = tokens_get(s);
    if (t->type == TOKEN_INTEGER) {
        IntSuffix suffix;
        val = parse_int(t->end - t->start, (const char*)t->start, &suffix);

        tokens_next(s);
    } else if (t->type == TOKEN_IDENTIFIER) {
        assert(!is_defined(c, t->start, t->end - t->start));

        val = 0;
        tokens_next(s);
    } else if (t->type == TOKEN_STRING_SINGLE_QUOTE) {
        int ch;
        intptr_t distance = parse_char(t->end - t->start, (const char*)t->start, &ch);
        if (distance < 0) {
            report(REPORT_ERROR, s, t->location, "could not parse char literal");
            abort();
        }

        val = ch;
        tokens_next(s);
    } else if (t->type == '(') {
        tokens_next(s);
        val = eval(c, s, NULL, t->location);

        if (tokens_get(s)->type != ')') {
            report_two_spots(REPORT_ERROR, s, t->location, tokens_get(s)->location,
                             "expected closing parenthesis for macro subexpression",
                             "open", "close?", NULL);
            abort();
        }
        tokens_next(s);
    } else {
        report(REPORT_ERROR, s, t->location, "could not parse expression");
        abort();
    }

    return flip ? !val : val;
}

static intmax_t eval_l5(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l0(c, s);

    while (tokens_get(s)->type == TOKEN_LEFT_SHIFT ||
           tokens_get(s)->type == TOKEN_RIGHT_SHIFT) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l0(c, s);
        if (t == TOKEN_LEFT_SHIFT) left = (left << right);
        else left = (uintmax_t)((uintmax_t)left >> (uintmax_t)right);
    }

    return left;
}

static intmax_t eval_l6(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l5(c, s);

    while (tokens_get(s)->type == '>' ||
           tokens_get(s)->type == '<' ||
           tokens_get(s)->type == TOKEN_GREATER_EQUAL ||
           tokens_get(s)->type == TOKEN_LESS_EQUAL) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l5(c, s);
        switch (t) {
            case '>': left = left > right; break;
            case '<': left = left < right; break;
            case TOKEN_GREATER_EQUAL: left = left >= right; break;
            case TOKEN_LESS_EQUAL: left = left <= right; break;
        }
    }

    return left;
}

static intmax_t eval_l7(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l6(c, s);

    while (tokens_get(s)->type == TOKEN_NOT_EQUAL ||
           tokens_get(s)->type == TOKEN_EQUALITY) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l6(c, s);
        if (t == TOKEN_EQUALITY) left = (left == right);
        else left = (left != right);
    }

    return left;
}

static intmax_t eval_l8(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l7(c, s);

    while (tokens_get(s)->type == '&') {
        tokens_next(s);

        intmax_t right = eval_l7(c, s);
        left = left & right;
    }

    return left;
}

static intmax_t eval_l9(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l8(c, s);

    while (tokens_get(s)->type == '^') {
        tokens_next(s);

        intmax_t right = eval_l8(c, s);
        left = left ^ right;
    }

    return left;
}

static intmax_t eval_l10(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l9(c, s);

    while (tokens_get(s)->type == '|') {
        tokens_next(s);

        intmax_t right = eval_l9(c, s);
        left = left | right;
    }

    return left;
}

static intmax_t eval_l11(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l10(c, s);

    while (tokens_get(s)->type == TOKEN_DOUBLE_AND) {
        tokens_next(s);

        intmax_t right = eval_l10(c, s);
        left = left && right;
    }

    return left;
}

static intmax_t eval_l12(CPP_Context* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l11(c, s);

    while (tokens_get(s)->type == TOKEN_DOUBLE_OR) {
        tokens_next(s);

        intmax_t right = eval_l11(c, s);
        left = left || right;
    }

    return left;
}

static intmax_t eval(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    // Expand
    if (l) {
        size_t old_tokens_length = arrlen(s->tokens);
        s->current = old_tokens_length;

        expand(c, s, l, SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0));
        assert(s->current != arrlen(s->tokens) && "Expected the macro expansion to add something");

        // Insert a null token at the end
        Token t = { 0, arrlen(s->locations) - 1, NULL, NULL };
        arrput(s->tokens, t);

        // Evaluate
        intmax_t result = eval_l12(c, s);

        arrsetlen(s->tokens, old_tokens_length);
        s->current = 0;
        return result;
    } else {
        return eval_l12(c, s);
    }
}
