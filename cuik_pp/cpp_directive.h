
// directive result type
typedef enum {
    // this is what happens when no directive matches
    DIRECTIVE_UNKNOWN,

    // this is what the directive functions will return
    DIRECTIVE_SUCCESS,
    DIRECTIVE_ERROR,
    DIRECTIVE_YIELD,
} DirectiveResult;

// opens a scope on if-group (#if, #ifdef, #ifndef), closes a
// scope on #endif and can leave scopes if we're on the root scope
// and hit a #else, #endif, or #elif
//
// Simple right :P
static DirectiveResult skip_directive_body(Lexer* restrict in) {
    int depth = 0;

    Token t = lexer_read(in);
    while (t.type) {
        if (t.type == '#') {
            unsigned char* savepoint = (unsigned char*) t.content.data;
            t = lexer_read(in);

            if (t.type == TOKEN_IDENTIFIER) {
                if (string_equals_cstr(&t.content, "if") ||
                    string_equals_cstr(&t.content, "ifdef") ||
                    string_equals_cstr(&t.content, "ifndef")) {
                    depth++;
                } else if (string_equals_cstr(&t.content, "elif") || string_equals_cstr(&t.content, "else")) {
                    // else/elif does both entering a scope and exiting one
                    if (depth == 0) {
                        in->current = savepoint;
                        return DIRECTIVE_SUCCESS;
                    }
                } else if (string_equals_cstr(&t.content, "endif")) {
                    if (depth == 0) {
                        // revert both the identifier and hash
                        in->current = savepoint;
                        return DIRECTIVE_SUCCESS;
                    }
                    depth--;
                }
            }
        } else {
            t = lexer_read(in);
        }
    }

    // TODO(NeGate): add error message about unmatched directives
    assert(0 && "TODO");
    return DIRECTIVE_ERROR;
}

static void warn_if_newline(Lexer* restrict in) {
    Token t;
    while (t = lexer_read(in), t.type && !t.hit_line) {
    }
    in->current = (unsigned char*) t.content.data;
}

static SourceRange get_pp_tokens_until_newline(Cuik_CPP* ctx, Lexer* in) {
    Token t = lexer_read(in);
    SourceLoc loc = t.location;
    while (!t.hit_line) {
        t = lexer_read(in);
    }
    return (SourceRange){ loc, get_end_location(&t) };
}

static DirectiveResult cpp__warning(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    SourceRange r = get_pp_tokens_until_newline(ctx, in);
    diag_warn(&ctx->tokens, r, "TODO");
    // diag_warn(&ctx->tokens, r, "%!S", msg);
    return DIRECTIVE_SUCCESS;
}

static DirectiveResult cpp__error(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    SourceRange r = get_pp_tokens_until_newline(ctx, in);
    diag_warn(&ctx->tokens, r, "TODO");
    return DIRECTIVE_SUCCESS;
}

// 'define' IDENT '(' IDENT-LIST ')' PP-TOKENS NEWLINE
// 'define' IDENT                    PP-TOKENS NEWLINE
static DirectiveResult cpp__define(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    Token key = lexer_read(in);
    SourceLoc key_loc = key.location;

    if (key.type != TOKEN_IDENTIFIER) {
        SourceRange r = { key_loc, get_end_location(&key) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    // if there's a parenthesis directly after the identifier
    // it's a macro function... yes this is an purposeful off-by-one
    // it's mostly ok tho
    Token t = lexer_read(in);
    if (key.content.data[key.content.length] == '(') {
        t = lexer_read(in);

        int arg_count = 0;
        while (t.type != 0 && t.type != ')') {
            if (arg_count) {
                if (t.type != ',') {
                    diag_err(&ctx->tokens, get_token_range(&t), "expected comma");
                }

                t = lexer_read(in);
            }

            if (t.type != TOKEN_TRIPLE_DOT && t.type != TOKEN_IDENTIFIER) {
                SourceRange r = { t.location, get_end_location(&t) };
                diag_err(&ctx->tokens, r, "expected identifier");
                return DIRECTIVE_ERROR;
            } else {
                arg_count++;
            }

            t = lexer_read(in);
        }

        t = lexer_read(in);
    }

    SourceLoc loc = t.location;
    const unsigned char* start = t.content.data;

    #if 0 // USE_INTRIN && CUIK__IS_X64
    // SIMD whitespace skip
    const unsigned char* end = t.content.data;
    if (t.type && !t.hit_line) {
        end += t.content.length;

        for (;;) {
            if (*end == '\n') {
                if (end[-1] == '\\' || (end[-1] == '\r' && end[-2] == '\\')) {
                    end += 1;
                } else {
                    break;
                }
            }

            __m128i chars = _mm_loadu_si128((__m128i*) end);
            __m128i mask  = _mm_cmpeq_epi8(chars, _mm_set1_epi8('\n'));

            uint32_t bits = _mm_movemask_epi8(mask);
            if (bits == 0) {
                end += 16;
                continue;
            }

            int len = __builtin_ffs(bits);
            end += len-1;
        }
    }
    #else
    while (t.type && !t.hit_line) {
        t = lexer_read(in);
    }
    const unsigned char* end = t.content.data;
    #endif

    // push back before the newline
    in->current = (unsigned char*) end;

    // printf("%.*s -> %.*s\n", (int)key.content.length, key.content.data, (int) (end - start), start);

    cuikperf_region_start("insert", NULL);
    MacroDef* def = insert_symtab(ctx, key.content.length, (const char*) key.content.data);
    def->value = (String){ end - start, start };
    def->loc = loc;
    cuikperf_region_end();

    return DIRECTIVE_SUCCESS;
}

// 'if' EXPR NEWLINE GROUP[OPT]
static DirectiveResult cpp__if(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    SourceRange r = get_token_range(&ctx->directive_token);
    if (eval(ctx, in)) {
        if (!push_scope(ctx, r, true)) return DIRECTIVE_ERROR;
    } else {
        if (!push_scope(ctx, r, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'ifdef' IDENT NEWLINE GROUP[OPT]
static DirectiveResult cpp__ifdef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    Token t = lexer_read(in);
    SourceRange r = get_token_range(&t);
    if (t.type != TOKEN_IDENTIFIER) {
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    if (is_defined(ctx, t.content.data, t.content.length)) {
        if (!push_scope(ctx, r, true)) return DIRECTIVE_ERROR;
    } else {
        if (!push_scope(ctx, r, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'ifndef' IDENT NEWLINE GROUP[OPT]
static DirectiveResult cpp__ifndef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    Token t = lexer_read(in);
    SourceRange r = get_token_range(&t);
    if (t.type != TOKEN_IDENTIFIER) {
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    if (!is_defined(ctx, t.content.data, t.content.length)) {
        if (!push_scope(ctx, r, true)) return DIRECTIVE_ERROR;

        // if we don't skip the body then maybe just maybe it's a guard macro
        if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_IF) {
            slot->include_guard.status = INCLUDE_GUARD_LOOKING_FOR_ENDIF;
            slot->include_guard.define = t.content;
            slot->include_guard.if_depth = ctx->depth;
        }
    } else {
        if (!push_scope(ctx, r, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'elif' EXPR NEWLINE GROUP[OPT]
static DirectiveResult cpp__elif(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    int last_scope = ctx->depth - 1;

    // if it didn't evaluate any of the other options try to do this
    if (!ctx->scope_eval[last_scope].value && eval(ctx, in)) {
        ctx->scope_eval[last_scope].value = true;
    } else {
        skip_directive_body(in);
    }

    // we should be one a different line now
    return DIRECTIVE_SUCCESS;
}

// 'else' NEWLINE GROUP[OPT]
static DirectiveResult cpp__else(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    // if it didn't evaluate any of the other options
    // do this
    int last_scope = ctx->depth - 1;
    if (!ctx->scope_eval[last_scope].value) {
        ctx->scope_eval[last_scope].value = true;
    } else {
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

static DirectiveResult cpp__endif(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    SourceRange r = get_token_range(&ctx->directive_token);
    if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_ENDIF && slot->include_guard.if_depth == ctx->depth) {
        if (!is_defined(ctx, slot->include_guard.define.data, slot->include_guard.define.length)) {
            // the ifndef's macro needs to stay defined or else the include guard doesn't make sense
            slot->include_guard.status = INCLUDE_GUARD_INVALID;
        } else {
            slot->include_guard.status = INCLUDE_GUARD_EXPECTING_NOTHING;
        }
    }

    warn_if_newline(in);
    return pop_scope(ctx, r) ? DIRECTIVE_SUCCESS : DIRECTIVE_ERROR;
}

static DirectiveResult cpp__undef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    Token key = lexer_read(in);
    if (key.type != TOKEN_IDENTIFIER) {
        SourceRange r = { key.location, get_end_location(&key) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    cuikpp_undef(ctx, key.content.length, (const char*) key.content.data);
    return DIRECTIVE_SUCCESS;
}

static char* parse_directive_path(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in, bool* is_lib_include) {
    int start = dyn_array_length(ctx->tokens.list.tokens);

    // place all the tokens on this line into the buffer to be expanded
    Token t;
    for (;;) {
        t = lexer_read(in);
        if (t.type == 0 || t.hit_line) { break; }
        push_token(ctx, t);

        size_t def_i;
        if (t.type == TOKEN_IDENTIFIER) {
            MacroDef* def = find_define(ctx, t.content.data, t.content.length);
            if (def != NULL) {
                int head = dyn_array_length(ctx->tokens.list.tokens);
                expand_identifier(ctx, in, NULL, head-1, head, 0, def, 0, NULL);
            }
        }
    }

    // revert to before the line
    int end = dyn_array_length(ctx->tokens.list.tokens);
    in->current = (unsigned char*) t.content.data;

    size_t len = 0;
    char* filename = tb_arena_alloc(&ctx->perm_arena, FILENAME_MAX);

    DynArray(Token) tokens = ctx->tokens.list.tokens;
    if (start == end) {
        diag_err(&ctx->tokens, get_token_range(&ctx->directive_token), "expected file path!");
        *filename = true;
        return NULL;
    }

    if (tokens[start].type == '<') {
        *is_lib_include = true;

        // Hacky but mostly works
        size_t i = start+1;
        for (; i < end; i++) {
            Token t = tokens[i];
            if (t.type == '>') { break; }

            if (len + t.content.length >= FILENAME_MAX) {
                diag_err(&ctx->tokens, get_token_range(&t), "filename too long!");
                return NULL;
            }

            memcpy(&filename[len], t.content.data, t.content.length);
            len += t.content.length;
        }

        // slap that null terminator on it like a boss bitch (i hate C strings)
        filename[len] = '\0';

        t = tokens[i++];
        if (t.type != '>') {
            diag_err(&ctx->tokens, get_token_range(&t), "expected '>' for #include");
            return NULL;
        }
    } else if (tokens[start].type == TOKEN_STRING_DOUBLE_QUOTE) {
        *is_lib_include = false;

        int i = start;
        do {
            size_t piece_len = tokens[i].content.length - 2;
            if (len+piece_len >= FILENAME_MAX) {
                diag_err(&ctx->tokens, get_token_range(&tokens[i]), "filename too long!");
                return NULL;
            }

            memcpy(filename+len, tokens[i].content.data + 1, piece_len);
            len += piece_len;
        } while (++i < end && tokens[i].type == TOKEN_STRING_DOUBLE_QUOTE);
        filename[len] = '\0';
    } else {
        diag_err(&ctx->tokens, get_token_range(&tokens[start]), "expected file path!");
        return NULL;
    }

    // throwaway the expanded filepath
    dyn_array_set_length(tokens, start);
    return filename;
}

static DirectiveResult cpp__include(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    bool is_lib_include;
    char* filename = parse_directive_path(ctx, slot, in, &is_lib_include);
    if (filename == NULL) {
        return DIRECTIVE_ERROR;
    }

    // find canonical filesystem path
    cuikperf_region_start("locate", NULL);

    Cuik_Path canonical;
    LocateResult l = locate_file(ctx, is_lib_include, slot->directory, filename, &canonical);
    if ((l & LOCATE_FOUND) == 0) {
        diag_err(&ctx->tokens, get_token_range(&ctx->directive_token), "couldn't find file: %s", filename);
        dyn_array_for(i, ctx->system_include_dirs) {
            Cuik_Path* p = ctx->system_include_dirs[i].path;
            diag_extra(&ctx->tokens, "also tried %s%s", p->data, filename);
        }
        return DIRECTIVE_ERROR;
    }
    cuikperf_region_end();

    // check if in include_once list
    ptrdiff_t search = nl_map_get_cstr(ctx->include_once, canonical.data);
    if (search >= 0) {
        IncludeGuardEntry* guard = &ctx->include_once[search].v;
        if (guard->name.length == 0 || is_defined(ctx, guard->name.data, guard->name.length) == guard->expected) {
            if (cuikperf_is_active()) {
                cuikperf_region_end();
            }
            return DIRECTIVE_YIELD;
        }
    }

    // printf("AAA %s\n", canonical.data);
    Cuik_Path* alloced_filepath = alloc_path(ctx, canonical.data);

    // insert incomplete new stack slot
    CPPStackSlot* restrict new_slot = &ctx->stack[ctx->stack_ptr++];
    *new_slot = (CPPStackSlot){
        .filepath = alloced_filepath,
        .directory = alloc_directory_path(ctx, canonical.data),
        .loc = ctx->directive_token.location
    };

    // read new file & lex
    #if CUIK__CPP_STATS
    uint64_t start_time = cuik_time_in_nanos();
    #endif

    Cuik_FileResult next_file;
    if (!ctx->fs(ctx->user_data, &canonical, &next_file, ctx->case_insensitive)) {
        diag_err(&ctx->tokens, get_token_range(&ctx->directive_token), "couldn't find file: %s", filename);
        return DIRECTIVE_ERROR;
    }

    #if CUIK__CPP_STATS
    ctx->total_io_time += (cuik_time_in_nanos() - start_time);
    ctx->total_files_read += 1;
    #endif

    // initialize the file & lexer in the stack new_slot
    new_slot->include_guard = (struct CPPIncludeGuard){ 0 };
    // initialize the lexer in the stack slot & record file entry
    new_slot->file_id = dyn_array_length(ctx->tokens.files);
    new_slot->lexer = (Lexer){
        .file_id = dyn_array_length(ctx->tokens.files),
        .start = (unsigned char*) next_file.data,
        .current = (unsigned char*) next_file.data,
    };
    compute_line_map(ctx, l & LOCATE_SYSTEM, ctx->stack_ptr - 1, new_slot->loc, alloced_filepath->data, next_file.data, next_file.length);

    if (cuikperf_is_active()) {
        cuikperf_region_end();
        cuikperf_region_start("preprocess", filename);
    }

    return DIRECTIVE_YIELD;
}

// 'pragma' PP-TOKENS[OPT] NEWLINE
static DirectiveResult cpp__pragma(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    TokenStream* restrict s = &ctx->tokens;

    Token t = lexer_read(in);
    SourceLoc loc = t.location;
    String pragma_type = t.content;

    if (string_equals_cstr(&pragma_type, "once")) {
        IncludeGuardEntry e = { 0 };
        nl_map_put_cstr(ctx->include_once, slot->filepath->data, e);

        // We gotta hit a line by now
        warn_if_newline(in);
    } else if (string_equals_cstr(&pragma_type, "message")) {
        t = lexer_read(in);

        int start = dyn_array_length(ctx->tokens.list.tokens);
        while (t.type && !t.hit_line) {
            push_token(ctx, t);
            t = lexer_read(in);
        }
        in->current = (unsigned char*) t.content.data;

        SourceRange r = get_token_range(&ctx->directive_token);
        Token t = quote_token_array(ctx, loc, start, dyn_array_length(ctx->tokens.list.tokens));
        diag_note(s, r, "%!S", t.content);
    } else {
        // convert to #pragma blah => _Pragma("blah")
        Token t2 = { TOKEN_KW_Pragma, .location = loc, .content = string_cstr("_Pragma") };
        push_token(ctx, t2);

        t2 = (Token){ '(', .location = loc, .content = string_cstr("(") };
        push_token(ctx, t2);

        int start = dyn_array_length(ctx->tokens.list.tokens);
        while (t.type && !t.hit_line) {
            push_token(ctx, t);
            t = lexer_read(in);
        }
        in->current = (unsigned char*) t.content.data;

        t = quote_token_array(ctx, loc, start, dyn_array_length(ctx->tokens.list.tokens));
        dyn_array_set_length(ctx->tokens.list.tokens, start);
        push_token(ctx, t);

        t2 = (Token){ ')', .location = loc, .content = string_cstr(")") };
        push_token(ctx, t2);
    }

    return DIRECTIVE_SUCCESS;
}

#if 0
static DirectiveResult cpp__embed(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    SourceRange loc = get_token_range(&in->tokens[in->current]);
    TokenStream* restrict s = &ctx->tokens;

    bool is_lib_include;
    char* filename = parse_directive_path(ctx, slot, in, &is_lib_include);
    if (filename == NULL) {
        return DIRECTIVE_ERROR;
    }

    // find canonical filesystem path
    Cuik_Path canonical;
    LocateResult l = locate_file(ctx, is_lib_include, slot->directory, filename, &canonical);
    if ((l & LOCATE_FOUND) == 0) {
        diag_err(&ctx->tokens, loc, "couldn't find file: %s", filename);
        return DIRECTIVE_ERROR;
    }

    char* alloced_filepath = tb_arena_alloc(&thread_arena, FILENAME_MAX + 16);
    size_t token_len = snprintf(alloced_filepath, FILENAME_MAX, "\"%s\"", canonical.data);

    // convert #embed path => _Embed(path)
    unsigned char* str = gimme_the_shtuffs_fill(ctx, "_Embed");
    Token t = (Token){ TOKEN_KW_Embed, false, false, loc.start, { 7, str } };
    dyn_array_put(s->list.tokens, t);

    str = gimme_the_shtuffs_fill(ctx, "(");
    t = (Token){ '(', false, false, loc.start, { 1, str } };
    dyn_array_put(s->list.tokens, t);

    Cuik_FileResult next_file;
    if (!ctx->fs(ctx->user_data, &canonical, &next_file, ctx->case_insensitive)) {
        fprintf(stderr, "\x1b[31merror\x1b[0m: file doesn't exist.\n");
        return DIRECTIVE_ERROR;
    }

    t = (Token){ TOKEN_MAGIC_EMBED_STRING, false, false, loc.start };
    t.content.length = next_file.length;
    t.content.data = (const unsigned char*) next_file.data;
    dyn_array_put(s->list.tokens, t);

    str = gimme_the_shtuffs_fill(ctx, ")");
    t = (Token){ ')', false, false, loc.start, { 1, str } };
    dyn_array_put(s->list.tokens, t);

    return DIRECTIVE_SUCCESS;
}

// passthrough all tokens raw
static DirectiveResult cpp__version(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    __debugbreak();
    return DIRECTIVE_SUCCESS;

    #if 0
    push_token(ctx, lexer_read(in));
    push_token(ctx, lexer_read(in));

    for (;;) {
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            return DIRECTIVE_SUCCESS;
        }

        dyn_array_put(s->list.tokens, t);
    }
    #endif
}

static DirectiveResult cpp__extension(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, Lexer* restrict in) {
    return cpp__version(ctx, slot, in);
}
#endif
