
// directive result type
typedef enum {
    // this is what happens when no directive matches
    DIRECTIVE_UNKNOWN,

    // this is what the directive functions will return
    DIRECTIVE_SUCCESS,
    DIRECTIVE_ERROR,
    DIRECTIVE_YIELD,
} DirectiveResult;

static String get_pp_tokens_until_newline(Cuik_CPP* ctx, TokenList* in);
static void warn_if_newline(TokenList* restrict in);
static void expect_no_newline(TokenList* restrict in);
static DirectiveResult skip_directive_body(TokenList* restrict in);

static DirectiveResult cpp__warning(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    SourceLoc loc = peek(in).location;
    String msg = get_pp_tokens_until_newline(ctx, in);

    SourceRange r = { loc, get_end_location(&in->tokens[in->current - 1]) };
    diag_warn(&ctx->tokens, r, "%!S", msg);
    return DIRECTIVE_SUCCESS;
}

static DirectiveResult cpp__error(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    SourceLoc loc = peek(in).location;
    String msg = get_pp_tokens_until_newline(ctx, in);

    SourceRange r = { loc, get_end_location(&in->tokens[in->current - 1]) };
    diag_err(&ctx->tokens, r, "%!S", msg);
    return DIRECTIVE_SUCCESS;
}

// passthrough all tokens raw
static DirectiveResult cpp__version(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    TokenStream* restrict s = &ctx->tokens;
    dyn_array_put(s->list.tokens, in->tokens[in->current - 2]);
    dyn_array_put(s->list.tokens, in->tokens[in->current - 1]);

    for (;;) {
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            return DIRECTIVE_SUCCESS;
        }

        dyn_array_put(s->list.tokens, t);
    }
}

static DirectiveResult cpp__extension(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    return cpp__version(ctx, slot, in, packet);
}

// 'pragma' PP-TOKENS[OPT] NEWLINE
static DirectiveResult cpp__pragma(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    TokenStream* restrict s = &ctx->tokens;
    SourceLoc loc = peek(in).location;
    String pragma_type = peek(in).content;

    if (string_equals_cstr(&pragma_type, "once")) {
        nl_strmap_put_cstr(ctx->include_once, (const char*) slot->filepath, 0);

        // We gotta hit a line by now
        consume(in);
        warn_if_newline(in);
    } else if (string_equals_cstr(&pragma_type, "message")) {
        consume(in);
        String msg = get_pp_tokens_until_newline(ctx, in);

        SourceRange r = { loc, get_end_location(&in->tokens[in->current - 1]) };
        diag_note(s, r, "%!S", msg);
    } else {
        // convert to #pragma blah => _Pragma("blah")
        unsigned char* str = gimme_the_shtuffs(ctx, sizeof("_Pragma"));
        memcpy(str, "_Pragma", sizeof("_Pragma"));
        Token t = { TOKEN_KW_Pragma, false, loc, { 7, str } };
        dyn_array_put(s->list.tokens, t);

        str = gimme_the_shtuffs(ctx, sizeof("("));
        str[0] = '(';
        str[1] = 0;
        t = (Token){ '(', false, loc, { 1, str } };
        dyn_array_put(s->list.tokens, t);

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

            t = (Token){ TOKEN_STRING_DOUBLE_QUOTE, false, loc, { (curr - str) - 1, str } };
            dyn_array_put(s->list.tokens, t);
        }

        str = gimme_the_shtuffs(ctx, sizeof(")"));
        str[0] = ')';
        str[1] = 0;
        t = (Token){ ')', false, loc, { 1, str } };
        dyn_array_put(s->list.tokens, t);
    }

    return DIRECTIVE_SUCCESS;
}

static DirectiveResult cpp__include(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    TokenStream* restrict s = &ctx->tokens;
    SourceLoc loc = peek(in).location;

    char* filename = gimme_the_shtuffs(ctx, MAX_PATH);
    bool is_lib_include = false;

    // Evaluate
    size_t savepoint = in->current;
    if (consume(in).type == '<') {
        is_lib_include = true;
        size_t len = 0;

        // Hacky but mostly works
        for (;;) {
            Token t = consume(in);
            if (t.type == '>') break;

            if (len + t.content.length > MAX_PATH) {
                generic_error(in, "filename too long!");
            }

            memcpy(&filename[len], t.content.data, t.content.length);
            len += t.content.length;
        }

        // slap that null terminator on it like a boss bitch
        filename[len] = '\0';

        if (peek(in).type != '>') {
            generic_error(in, "expected '>' for #include");
        }
    } else {
        in->current = savepoint;

        size_t a = push_expansion(ctx, &s->list, in);
        Token t = consume(&s->list);

        if (t.type == TOKEN_STRING_DOUBLE_QUOTE) {
            size_t len = t.content.length - 2;
            if (len > MAX_PATH) {
                generic_error(in, "filepath too long!");
                abort();
            }

            memcpy(filename, t.content.data + 1, len);
            filename[len] = '\0';
        } else {
            generic_error(in, "expected file path!");
        }

        pop_expansion(&s->list, a);
    }

    // insert incomplete new stack slot
    ctx->stack[ctx->stack_ptr++] = (CPPStackSlot){
        .filepath = filename,
        .loc = loc,
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
        sprintf_s(path, FILENAME_MAX, "%s%s", ctx->system_include_dirs[0].name, filename);
    }

    packet->tag = CUIKPP_PACKET_QUERY_FILE;
    packet->file.input_path = path;
    packet->file.length = 0;
    packet->file.data = NULL;
    return DIRECTIVE_YIELD;
}

// 'define' IDENT '(' IDENT-LIST ')' PP-TOKENS NEWLINE
// 'define' IDENT                    PP-TOKENS NEWLINE
static DirectiveResult cpp__define(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    SourceLoc key_loc = peek(in).location;
    Token key = consume(in);

    if (key.type != TOKEN_IDENTIFIER) {
        SourceRange r = { key_loc, get_end_location(&key) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    // Hash name
    if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_DEFINE) {
        // as long as the define matches the include guard we're good
        if (string_equals(&slot->include_guard.define, &key.content)) {
            slot->include_guard.status = INCLUDE_GUARD_LOOKING_FOR_ENDIF;
        } else {
            slot->include_guard.status = INCLUDE_GUARD_INVALID;
        }
    }

    size_t i = insert_symtab(ctx, key.content.length, (const char*) key.content.data);
    ctx->macros.keys[i] = key.content;

    // if there's a parenthesis directly after the identifier
    // it's a macro function... yes this is an purposeful off-by-one
    // it's mostly ok tho
    if (key.content.data[key.content.length] == '(') {
        consume(in);

        int arg_count = 0;
        while (!at_token_list_end(in)) {
            Token t = consume(in);
            if (t.type == 0 || t.type == ')') break;

            if (arg_count) {
                if (t.type != ',') {
                    diag_err(&ctx->tokens, get_token_range(&t), "expected comma");
                }

                t = consume(in);
            }

            if (t.type != TOKEN_TRIPLE_DOT && t.type != TOKEN_IDENTIFIER) {
                SourceRange r = { t.location, get_end_location(&t) };
                diag_err(&ctx->tokens, r, "expected identifier");
                return DIRECTIVE_ERROR;
            } else {
                arg_count++;
            }
        }
    }

    SourceLoc loc = peek(in).location;
    String value = get_pp_tokens_until_newline(ctx, in);
    ctx->macros.vals[i] = (MacroDef){ value, loc };
    return DIRECTIVE_SUCCESS;
}

// 'if' EXPR NEWLINE GROUP[OPT]
static DirectiveResult cpp__if(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    expect_no_newline(in);
    if (eval(ctx, in)) {
        if (!push_scope(ctx, in, true)) return DIRECTIVE_ERROR;
    } else {
        if (!push_scope(ctx, in, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'ifdef' IDENT NEWLINE GROUP[OPT]
static DirectiveResult cpp__ifdef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    Token t = consume(in);
    if (t.type != TOKEN_IDENTIFIER) {
        SourceRange r = { t.location, get_end_location(&t) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    if (is_defined(ctx, t.content.data, t.content.length)) {
        if (!push_scope(ctx, in, true)) return DIRECTIVE_ERROR;
    } else {
        if (!push_scope(ctx, in, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'ifndef' IDENT NEWLINE GROUP[OPT]
static DirectiveResult cpp__ifndef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    Token t = consume(in);
    if (t.type != TOKEN_IDENTIFIER) {
        SourceRange r = { t.location, get_end_location(&t) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    if (!is_defined(ctx, t.content.data, t.content.length)) {
        if (!push_scope(ctx, in, true)) return DIRECTIVE_ERROR;

        // if we don't skip the body then maybe just maybe it's a guard macro
        if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_IFNDEF) {
            slot->include_guard.status = INCLUDE_GUARD_LOOKING_FOR_DEFINE;
            slot->include_guard.define = t.content;
            slot->include_guard.if_depth = ctx->depth;
        }
    } else {
        if (!push_scope(ctx, in, false)) return DIRECTIVE_ERROR;
        skip_directive_body(in);
    }

    return DIRECTIVE_SUCCESS;
}

// 'elif' EXPR NEWLINE GROUP[OPT]
static DirectiveResult cpp__elif(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    expect_no_newline(in);
    int last_scope = ctx->depth - 1;

    // if it didn't evaluate any of the other options try to do this
    if (!ctx->scope_eval[last_scope].value && eval(ctx, in)) {
        ctx->scope_eval[last_scope].value = true;
    } else {
        skip_directive_body(in);
    }

    // we should be one a different line now
    warn_if_newline(in);
    return DIRECTIVE_SUCCESS;
}

// 'else' NEWLINE GROUP[OPT]
static DirectiveResult cpp__else(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
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

static DirectiveResult cpp__endif(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    if (slot->include_guard.status == INCLUDE_GUARD_LOOKING_FOR_ENDIF && slot->include_guard.if_depth == ctx->depth) {
        if (!is_defined(ctx, slot->include_guard.define.data, slot->include_guard.define.length)) {
            // the ifndef's macro needs to stay defined or else the include guard doesn't make sense
            slot->include_guard.status = INCLUDE_GUARD_INVALID;
        } else {
            slot->include_guard.status = INCLUDE_GUARD_EXPECTING_NOTHING;
        }
    }

    warn_if_newline(in);
    return pop_scope(ctx, in) ? DIRECTIVE_SUCCESS : DIRECTIVE_ERROR;
}

static DirectiveResult cpp__undef(Cuik_CPP* restrict ctx, CPPStackSlot* restrict slot, TokenList* restrict in, Cuikpp_Packet* restrict packet) {
    Token key = consume(in);
    if (key.type != TOKEN_IDENTIFIER) {
        SourceRange r = { key.location, get_end_location(&key) };
        diag_err(&ctx->tokens, r, "expected identifier");
        return DIRECTIVE_ERROR;
    }

    cuikpp_undef(ctx, key.content.length, (const char*) key.content.data);
    return DIRECTIVE_SUCCESS;
}

// opens a scope on if-group (#if, #ifdef, #ifndef), closes a
// scope on #endif and can leave scopes if we're on the root scope
// and hit a #else, #endif, or #elif
//
// Simple right :P
static DirectiveResult skip_directive_body(TokenList* restrict in) {
    int depth = 0;

    while (!at_token_list_end(in)) {
        Token t = consume(in);

        if (t.type == 0) {
            break;
        } else if (t.type == '#') {
            t = peek(in);

            if (t.type == TOKEN_IDENTIFIER) {
                if (memeq(t.content.data, t.content.length, "if", 2) ||
                    memeq(t.content.data, t.content.length, "ifdef", 5) ||
                    memeq(t.content.data, t.content.length, "ifndef", 6)) {
                    depth++;
                } else if (memeq(t.content.data, t.content.length, "elif", 4) ||
                    memeq(t.content.data, t.content.length, "else", 4)) {
                    // else/elif does both entering a scope and exiting one
                    if (depth == 0) {
                        in->current -= 1;
                        // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "SKIP END");
                        return DIRECTIVE_SUCCESS;
                    }
                } else if (memeq(t.content.data, t.content.length, "endif", 5)) {
                    if (depth == 0) {
                        // revert both the identifier and hash
                        in->current -= 1;
                        // report(REPORT_INFO, NULL, in, tokens_get_location_index(in), "SKIP END");
                        return DIRECTIVE_SUCCESS;
                    }
                    depth--;
                }
            }
        }
    }

    // TODO(NeGate): add error message about unmatched directives
    assert(0 && "TODO");
    return DIRECTIVE_ERROR;
}

static String get_pp_tokens_until_newline(Cuik_CPP* ctx, TokenList* in) {
    Token first = peek(in);
    if (first.hit_line) {
        return (String){ 0 };
    }

    String str = peek(in).content;
    bool is_str = (first.type == TOKEN_STRING_WIDE_SINGLE_QUOTE || first.type == TOKEN_STRING_WIDE_DOUBLE_QUOTE);

    for (;;) {
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            break;
        }

        str.length = &t.content.data[t.content.length] - str.data;
    }

    if (is_str) {
        str.data += 1;
        str.length -= 1;
    }

    return str;
}

static void warn_if_newline(TokenList* restrict in) {
    while (!at_token_list_end(in)) {
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            break;
        }
    }
}

static void expect_no_newline(TokenList* restrict in) {
    // preprocessor usually expects it's statements to be on the same line
    bool hit = peek(in).hit_line;

    // TODO(NeGate): make this a real error message
    assert(!hit);
}
