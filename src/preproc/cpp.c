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

static void* gimme_the_shtuffs(CPP_Context* restrict c, size_t len);
static void trim_the_shtuffs(CPP_Context* restrict c, void* new_top);
static SourceLocIndex get_source_location(CPP_Context* restrict c, Lexer* restrict l, TokenStream* restrict s, SourceLocIndex parent_loc, SourceLocType loc_type);

static void expand(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
static void expand_ident(CPP_Context* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc);
//static void expand_double_hash(CPP_Context* restrict c, TokenStream* restrict s, Token* last, Lexer* restrict l, SourceLocIndex loc);

// Basically a mini-unity build that takes up just the CPP module
#include "cpp_symtab.h"
#include "cpp_expand.h"
#include "cpp_expr.h"

void cpp_init(CPP_Context* ctx) {
    size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
    size_t sz2 = sizeof(SourceLocIndex) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;

    *ctx = (CPP_Context){
        .macro_bucket_keys = malloc(sz),
        .macro_bucket_keys_length = malloc(sz),
        .macro_bucket_values_start = malloc(sz),
        .macro_bucket_values_end = malloc(sz),
        .macro_bucket_source_locs = malloc(sz2),

        .the_shtuffs = malloc(THE_SHTUFFS_SIZE),
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

    printf("\n// Macro defines active: %d\n", count);
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

    TokenStream s = {0};
    preprocess_file(ctx, &s, NULL, 0, directory, filepath, 1);

    Token t = {0, 0, NULL, NULL};
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
    assert(columns <= UINT16_MAX && length <= UINT16_MAX);

    SourceLoc loc = {
        .line = source_line,
        .columns = columns,
        .length = length};
    arrput(s->locations, loc);

    return SOURCE_LOC_SET_TYPE(loc_type, i);
}

static void cpp_push_scope(CPP_Context* restrict ctx, Lexer* restrict l, bool initial) {
    if (ctx->depth >= CPP_MAX_SCOPE_DEPTH - 1) {
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

static void preprocess_file(CPP_Context* restrict c, TokenStream* restrict s, CPP_FileEntry* parent_entry, SourceLocIndex include_loc, const char* directory, const char* filepath, int depth) {
    // hacky but i don't wanna wrap it in a timed_block
    uint64_t timer_start = timer_now();

    unsigned char* text = (unsigned char*)read_entire_file(filepath);
    CPP_FileEntry file_entry = {
        .parent = parent_entry,
        .include_loc = include_loc,
        .filepath = filepath,
        .content = text};
    big_array_put(c->files, file_entry);

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
                    l.token_start, l.token_end};
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
                } else if (lexer_match(&l, 6, "ifndef")) {
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
                        Token t = {0, arrlen(s->locations) - 1, NULL, NULL};
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
                        Token t = (Token){
                            TOKEN_KW_Pragma, loc, str, str + 7};
                        arrput(s->tokens, t);

                        str = gimme_the_shtuffs(c, sizeof("("));
                        str[0] = '(';
                        str[1] = 0;
                        t = (Token){
                            '(', loc, str, str + 1};
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
                                str, curr - 1};
                            arrput(s->tokens, t);
                        }

                        str = gimme_the_shtuffs(c, sizeof(")"));
                        str[0] = ')';
                        str[1] = 0;
                        t = (Token){
                            ')',
                            loc,
                            str, str + 1};
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
                l.token_end};

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
