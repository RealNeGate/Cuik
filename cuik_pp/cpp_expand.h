
static ptrdiff_t find_arg(ArenaArray(MacroArg) args, String name) {
    if (args) {
        aarray_for(i, args) {
            if (string_equals(&args[i].key, &name)) {
                return i;
            }
        }
    }

    return -1;
}

static SourceLoc macroify_loc(SourceLoc loc, uint32_t parent_macro) {
    if (parent_macro == 0) {
        return loc;
    } else if ((loc.raw & SourceLoc_IsMacro) == 0) {
        uint32_t pos = loc.raw & ((1u << SourceLoc_FilePosBits) - 1);
        return encode_macro_loc(parent_macro, pos);
    } else {
        return encode_macro_loc(parent_macro, 0);
    }
}

static bool expand_builtin_idents(Cuik_CPP* restrict c, Token* t) {
    if (string_equals_cstr(&t->content, "__FILE__") || string_equals_cstr(&t->content, "L__FILE__")) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t->location);

        // filepath as a string
        unsigned char* output_path_start = tb_arena_alloc(&c->perm_arena, FILENAME_MAX + 4);
        unsigned char* output_path = output_path_start;

        bool is_wide = (t->content.data[0] == 'L');
        if (is_wide) *output_path++ = 'L';

        *output_path++ = '\"';
        {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* input_path = (const char*) r.file->filename;
            assert(strlen(input_path) < FILENAME_MAX && "__FILE__ too long?");

            while (*input_path) {
                if (*input_path == '\\') {
                    *output_path++ = '\\';
                    *output_path++ = '\\';
                    input_path++;
                } else {
                    *output_path++ = *input_path++;
                }
            }
        }

        *output_path++ = '\"';
        *output_path++ = '\0';

        t->type = is_wide ? TOKEN_STRING_WIDE_DOUBLE_QUOTE : TOKEN_STRING_DOUBLE_QUOTE;
        t->content = string_from_range(output_path_start, output_path - 1);
        return true;
    } else if (string_equals_cstr(&t->content, "__COUNTER__")) {
        // line number as a string
        unsigned char* out = tb_arena_alloc(&c->perm_arena, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", c->unique_counter);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else if (string_equals_cstr(&t->content, "__LINE__")) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t->location);

        // line number as a string
        unsigned char* out = tb_arena_alloc(&c->perm_arena, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", r.line);
        // trim_the_shtuffs(c, &out[length + 1]);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else {
        return false;
    }
}

static int DUMP_TIMER = 0;
static void dump_tokens(Cuik_CPP* restrict ctx, const char* tag, int start, int end, int depth) {
    assert(start <= end);

    #if 0
    printf("%6d ", DUMP_TIMER++);
    FOR_N(i, 0, depth) {
        printf("  ");
    }
    printf("TOKENS %-20s [%d, %d): ", tag, start, end);
    FOR_N(i, start, end) {
        Token t = ctx->tokens.list.tokens[i];
        if (t.expanded) {
            printf("\x1b[32m");
        }
        if (1 || t.has_space) {
            printf(" ");
        }
        printf("%.*s", (int)t.content.length, t.content.data);
        if (t.expanded) {
            printf("\x1b[0m");
        }
    }
    printf("\n");
    #endif
}

static Token quote_token_array(Cuik_CPP* restrict ctx, SourceLoc loc, int start, int end) {
    int last_pos = -1, str_len = 2;
    FOR_N(i, start, end) {
        Token* t = &ctx->tokens.list.tokens[i];

        uint32_t pos = t->location.raw & ((1u << SourceLoc_MacroOffsetBits) - 1);
        if (last_pos >= 0 && pos != last_pos) {
            str_len += 1;
        }

        // find any chars that need to be escaped
        FOR_N(j, 0, t->content.length) {
            if (t->content.data[j] == '\\' || t->content.data[j] == '\'' || t->content.data[j] == '\"') {
                str_len += 1;
            }
        }

        // insert some whitespace padding based on the line info
        str_len += t->content.length;
        last_pos = pos + t->content.length;
    }

    // at best we might double the string length from backslashes
    unsigned char* stringized = tb_arena_alloc(&ctx->perm_arena, str_len + 1);

    last_pos = -1, str_len = 0;
    stringized[str_len++] = '"';
    FOR_N(i, start, end) {
        Token* t = &ctx->tokens.list.tokens[i];

        uint32_t pos = t->location.raw & ((1u << SourceLoc_MacroOffsetBits) - 1);
        if (i != start && t->has_space) {
            stringized[str_len++] = ' ';
        }

        // find any chars that need to be escaped
        FOR_N(j, 0, t->content.length) {
            if (t->content.data[j] == '\\' || t->content.data[j] == '\'' || t->content.data[j] == '\"') {
                stringized[str_len++] = '\\';
            }

            stringized[str_len++] = t->content.data[j];
        }
        last_pos = pos + t->content.length;
    }
    stringized[str_len++] = '"';
    stringized[str_len] = 0;
    return (Token){
        .type = TOKEN_STRING_DOUBLE_QUOTE,
        .expanded = true,
        .location = loc,
        .content = { str_len, stringized },
    };
}

typedef struct InvokeElem {
    struct InvokeElem* prev;
    int curr;
    int end;
} InvokeElem;

typedef struct InvokeCursor {
    InvokeElem* elem;
    int pos;
} InvokeCursor;

static InvokeCursor advance(InvokeCursor c) {
    c.pos += 1;
    if (c.elem && c.pos == c.elem->end) {
        c.elem = c.elem->prev;
        c.pos = c.elem ? c.elem->curr : 0;
    }
    return c;
}

static bool invoke_eat(Cuik_CPP* restrict ctx, CPPStackSlot* slot, Token* out_t, InvokeCursor* c, TknType type) {
    InvokeCursor next = advance(*c);
    if (next.elem == NULL) {
        if (slot == NULL) {
            return false;
        }

        // read from lexer
        unsigned char* savepoint = cpp_lexer_pos(slot);
        *out_t = cpp_lexer_read(slot);
        if (out_t->type != type) {
            cpp_lexer_seek(slot, savepoint);
            return false;
        }
    } else {
        *out_t = ctx->tokens.list.tokens[c->pos];
        if (out_t->type != type) {
            return false;
        }
    }

    *c = next;
    return true;
}

static Token read_one(Cuik_CPP* restrict ctx, CPPStackSlot* slot, InvokeCursor* c) {
    if (c->elem == NULL) {
        // read from lexer
        return cpp_lexer_read(slot);
    } else {
        Token t = ctx->tokens.list.tokens[c->pos];
        *c = advance(*c);
        return t;
    }
}

// returns the size of the expanded region which goes at the very end of the token stream
static int expand_identifier(Cuik_CPP* restrict ctx, CPPStackSlot* slot, InvokeElem* parent, int read_head, uint32_t parent_macro, MacroDef* def, int depth, int* out_read_tail) {
    if (expand_builtin_idents(ctx, &ctx->tokens.list.tokens[read_head])) {
        return 1;
    }

    int end_token = dyn_array_length(ctx->tokens.list.tokens);
    InvokeElem invoke_elem = { parent, read_head, end_token };
    InvokeCursor cursor = { &invoke_elem, read_head };

    Token t = ctx->tokens.list.tokens[read_head];
    String macro_name = def->key;
    String def_str = def->value;
    SourceLoc def_site = def->loc;

    // printf("EXPAND %.*s\n", (int) macro_name.length, macro_name.data);

    // create macro invoke site
    uint32_t macro_id = dyn_array_length(ctx->tokens.invokes);
    dyn_array_put(ctx->tokens.invokes, (MacroInvoke){
                  .name      = t.content,
                  .depth     = parent_macro ? ctx->tokens.invokes[parent_macro].depth+1 : 1,
                  .parent    = parent_macro,
                  .def_site  = { def_site, { def_site.raw + def_str.length } },
                  .call_site = t.location,
                  });

    const unsigned char* param_str = def->key.data + def->key.length;
    Lexer def_lexer = {
        .start = (unsigned char*) def_str.data,
        .current = (unsigned char*) def_str.data,
    };

    TB_ArenaSavepoint sp = tb_arena_save(&ctx->tmp_arena);
    ArenaArray(MacroArg) args = NULL;
    bool has_varargs = false;

    int read_tail = read_head+1;
    ptrdiff_t dt = 0;
    if (*param_str == '(') {
        cursor = advance(cursor);

        // ignore this expansion if we're missing the opening paren
        Token arg_t;
        if (!invoke_eat(ctx, slot, &arg_t, &cursor, '(')) {
            if (out_read_tail) { *out_read_tail = read_tail; }
            return 1;
        }

        // Construct substitution table
        Lexer param_lexer = {
            .start = (unsigned char*) param_str + 1,
            .current = (unsigned char*) param_str + 1,
        };
        args = aarray_create(&ctx->tmp_arena, MacroArg, 8);

        ////////////////////////////////
        // Parse params
        ////////////////////////////////
        // [https://www.sigbus.info/n1570#6.10p1] This just handles parsing the # define param list
        //
        // After '# define identifier':
        //   lparen identifier-list opt )
        //   lparen ... )
        //   lparen identifier-list , ... )
        //
        // identifier-list:
        //   identifier
        //   identifier-list , identifier
        //
        bool has_varargs = false;
        Token param_t = lexer_read(&param_lexer);
        while (param_t.type && param_t.type != ')') {
            // expect comma
            if (aarray_length(args)) {
                if (param_t.type != TOKEN_COMMA) {
                    assert(0);
                }
                param_t = lexer_read(&param_lexer);
            }

            // arg name
            MacroArg a = { .key = param_t.content };
            if (param_t.type == TOKEN_TRIPLE_DOT) {
                a.key = (String){ sizeof("__VA_ARGS__")-1, (const unsigned char*) "__VA_ARGS__" };
                has_varargs = true;
            }
            aarray_push(args, a);
            param_t = lexer_read(&param_lexer);
        }
        assert(param_t.type == ')');

        ////////////////////////////////
        // Parse args
        ////////////////////////////////
        // scan the items in the top of the arg list right now
        int arg_head = read_head+1;

        int parens = -1;
        if (arg_head < end_token) {
            parens = 0;

            DynArray(Token) tokens = ctx->tokens.list.tokens;
            arg_t = tokens[arg_head++];
            do {
                if (arg_t.type == 0) { break; }
                if (arg_t.type == '(') { parens++; }
                if (arg_t.type == ')') { parens--; }

                if (parens == 0) {
                    break;
                }
                arg_t = tokens[arg_head++];
            } while (arg_head <= end_token);

            read_tail = arg_head;
        }

        // if the expanded array didn't contain the entire arg list, we'll fill in the remaining bits
        if (parens != 0) {
            if (parens < 0) {
                parens = 0;
            }

            for (;;) {
                if (arg_t.type == 0) { break; }
                if (arg_t.type == '(') { parens++; }
                if (arg_t.type == ')') { parens--; }

                push_token(ctx, arg_t);
                if (parens == 0) {
                    break;
                }

                arg_t = read_one(ctx, slot, &cursor);
            }

            dt += dyn_array_length(ctx->tokens.list.tokens) - read_tail;
            read_tail = dyn_array_length(ctx->tokens.list.tokens);
            end_token = read_tail;
        }

        dump_tokens(ctx, "With args", read_head, read_tail, depth);
        assert(parens == 0);

        // token array is stable at the moment, let's cache the pointer
        DynArray(Token) tokens = ctx->tokens.list.tokens;

        arg_head = read_head+2;
        arg_t = tokens[arg_head++];

        int arg_c = 0;
        while (arg_t.type && arg_t.type != ')') {
            // expect comma
            if (arg_c > 0) {
                if (arg_t.type != TOKEN_COMMA) {
                    assert(0);
                }
                arg_t = tokens[arg_head++];
            }

            MacroArg a = { .key = arg_t.content };
            args[arg_c].token_start = arg_head - 1;

            bool is_vararg = has_varargs ? arg_c == aarray_length(args)-1 : false;
            int parens = 0;
            for (;;) {
                if (arg_t.type == '(') { parens++; }
                if (arg_t.type == ')') {
                    if (parens == 0) { break; }
                    parens--;
                }
                if (arg_t.type == ',' && parens == 0 && !is_vararg) { break; }

                // convert token location into macro relative
                /*if ((arg_t.location.raw & SourceLoc_IsMacro) == 0) {
                uint32_t pos = arg_t.location.raw & ((1u << SourceLoc_FilePosBits) - 1);
                arg_t.location = encode_macro_loc(macro_id, pos);
                }*/
                arg_t = tokens[arg_head++];
            }
            assert(parens == 0);

            args[arg_c].token_end = arg_head - 1;
            arg_c += 1;
        }
        assert(arg_t.type == ')');
    }

    if (out_read_tail) {
        *out_read_tail = read_tail;
    }

    // special case, the macro is empty
    if (def_str.length == 0) {
        dyn_array_set_length(ctx->tokens.list.tokens, read_head);
        tb_arena_restore(&ctx->tmp_arena, sp);
        return 0;
    }

    // Subst & Stringize, because of argument prescan we don't expand tokens
    // which contribute to # or ##
    bool has_space = t.has_space;
    bool was_dhash = false;
    size_t start = dyn_array_length(ctx->tokens.list.tokens);
    for (;;) {
        Token def_t = lexer_read(&def_lexer);
        if (def_t.type == 0 || def_t.hit_line) {
            break;
        }

        if (has_space) {
            def_t.has_space = true;
            has_space = false;
        }


        if (def_t.type == TOKEN_HASH) {
            Token next_t = lexer_read(&def_lexer);
            if (next_t.type == TOKEN_IDENTIFIER) {
                def_t = next_t;

                // Stringize
                SourceLoc loc = encode_macro_loc(macro_id, def_t.content.data - def_lexer.start);
                SourceRange r = { loc, { loc.raw + def_t.content.length } };
                ptrdiff_t arg = find_arg(args, def_t.content);
                if (arg < 0) {
                    diag_err(&ctx->tokens, r, "cannot stringize unknown argument");
                    break;
                }

                bool has_space = def_t.has_space;
                def_t = quote_token_array(ctx, def_t.location, args[arg].token_start, args[arg].token_end);
                def_t.has_space = has_space;
            } else {
                // rollback and paste both the # and ##
                def_lexer.current = (unsigned char*) def_t.content.data + def_t.content.length;
            }
        } else if (def_t.type == TOKEN_IDENTIFIER) {
            ptrdiff_t arg = find_arg(args, def_t.content);

            if (arg >= 0) {
                dump_tokens(ctx, "Arg pre-expand", args[arg].token_start, args[arg].token_end, depth + 1);

                if (args[arg].token_start != args[arg].token_end) {
                    bool block = was_dhash;
                    if (!block) {
                        Lexer saved = def_lexer;
                        Token peek = lexer_read(&saved);
                        block = (peek.type == TOKEN_DOUBLE_HASH);
                    }

                    size_t first_subst_token = dyn_array_length(ctx->tokens.list.tokens);
                    if (block) {
                        FOR_N(i, args[arg].token_start, args[arg].token_end) {
                            Token t = ctx->tokens.list.tokens[i];
                            push_token(ctx, t);
                        }
                    } else {
                        // subst & expand arguments
                        FOR_N(i, args[arg].token_start, args[arg].token_end) {
                            Token t = ctx->tokens.list.tokens[i];
                            if (!t.expanded && t.type == TOKEN_IDENTIFIER) {
                                // if it failed to expand, it can't expand later during the rescan
                                MacroDef* kid_def = find_define(ctx, t.content.data, t.content.length);
                                if (kid_def != NULL) {
                                    int kid_i = dyn_array_length(ctx->tokens.list.tokens);
                                    push_token(ctx, t);

                                    InvokeElem nested = { parent, i + 1, args[arg].token_end };

                                    int kid_tail;
                                    int kid_count = expand_identifier(ctx, NULL, &nested, kid_i, macro_id, kid_def, depth + 2, &kid_tail);

                                    assert(kid_count >= 0);
                                    i += (kid_tail - kid_i) - 1;
                                    continue;
                                } else {
                                    t.expanded = true;
                                }
                            }
                            push_token(ctx, t);
                        }
                    }

                    // first token inherits the leading space from the def_t
                    if (first_subst_token < dyn_array_length(ctx->tokens.list.tokens)) {
                        ctx->tokens.list.tokens[first_subst_token].has_space = def_t.has_space;
                    }

                    dump_tokens(ctx, "Arg post-expand", first_subst_token, dyn_array_length(ctx->tokens.list.tokens), depth + 1);
                }
                continue;
            }
        }

        was_dhash = (def_t.type == TOKEN_DOUBLE_HASH);

        // convert token location into macro relative
        if ((def_t.location.raw & SourceLoc_IsMacro) == 0) {
            def_t.location = encode_macro_loc(macro_id, def_t.content.data - def_lexer.start);
        }

        push_token(ctx, def_t);
    }

    dump_tokens(ctx, "Post-subst", start, dyn_array_length(ctx->tokens.list.tokens), depth);

    // Concat tokens
    size_t end = dyn_array_length(ctx->tokens.list.tokens);
    size_t j = start;
    for (size_t i = start; i < end;) {
        Token* t = &ctx->tokens.list.tokens[i];
        if (!t->expanded && t->type == TOKEN_DOUBLE_HASH) {
            if (i+1 == end) {
                // concat against nothing, just kill the double hash
                break;
            } else if (j > start) {
                bool has_space = ctx->tokens.list.tokens[j-1].has_space;
                String a = ctx->tokens.list.tokens[j-1].content;
                String b = ctx->tokens.list.tokens[i+1].content;

                // Literally join the data
                unsigned char* out = tb_arena_alloc(&ctx->perm_arena, a.length + b.length + 16);
                memcpy(out, a.data, a.length);
                memcpy(out + a.length, b.data, b.length);
                memset(&out[a.length + b.length], 0, 16);

                // generate a new token and see what happens
                Lexer scratch = { 0, 0, out, out };
                Token joined = lexer_read(&scratch);
                joined.location = ctx->tokens.list.tokens[i].location;
                joined.has_space = has_space;

                if (joined.type == TOKEN_DOUBLE_HASH) {
                    joined.expanded = true;
                }

                // shrink the token list
                ctx->tokens.list.tokens[j-1] = joined;
                i += 2;
            } else {
                // We can join a with "nothing", just skip the double hash and leave the rest of the
                // tokens alone
                i += 1;
            }
        } else {
            if (i != j) {
                ctx->tokens.list.tokens[j] = ctx->tokens.list.tokens[i];
            }
            i += 1, j += 1;
        }
    }

    dyn_array_set_length(ctx->tokens.list.tokens, j);
    end = j;

    dump_tokens(ctx, "Pre-expand", start, end, depth);
    size_t hidden = hide_macro(ctx, def);
    size_t new_start = end;

    // Rescanning, will build up expansions at the end of the token buffer
    // and then insert them between [start, end)
    for (int i = start; i < end;) {
        Token t = ctx->tokens.list.tokens[i];
        push_token(ctx, t);

        size_t def_i;
        if (!t.expanded && t.type == TOKEN_IDENTIFIER) {
            MacroDef* kid_def = find_define(ctx, t.content.data, t.content.length);
            if (kid_def != NULL) {
                size_t kid_i = dyn_array_length(ctx->tokens.list.tokens) - 1;
                InvokeElem nested = { parent, i + 1, end }; // { parent, read_head, end_token };

                // push ident to the end, we'll replace it and then copy it down
                int kid_tail;
                int kid_count = expand_identifier(ctx, slot, &nested, kid_i, macro_id, kid_def, depth+1, &kid_tail);
                size_t top = dyn_array_length(ctx->tokens.list.tokens);

                // skip expanded tokens
                i += kid_tail - kid_i;
                dump_tokens(ctx, "Expand", new_start, top, depth);
                continue;
            } else if (string_equals(&macro_name, &t.content)) {
                t.expanded = true;
            }
        }
        i += 1;
    }
    start = new_start;
    end   = dyn_array_length(ctx->tokens.list.tokens);
    unhide_macro(ctx, def, hidden);

    dump_tokens(ctx, "Post-expand", start, end, depth);
    dump_tokens(ctx, "Copying into", read_head, read_tail, depth);
    assert(end >= start);

    // We're replacing the token at read_head
    size_t new_len = read_head + (end - start);
    FOR_N(i, 0, end - start) {
        ctx->tokens.list.tokens[read_head + i] = ctx->tokens.list.tokens[start + i];
    }
    dyn_array_set_length(ctx->tokens.list.tokens, new_len);

    dump_tokens(ctx, "Copied", read_head, dyn_array_length(ctx->tokens.list.tokens), depth);
    tb_arena_restore(&ctx->tmp_arena, sp);
    return end - start;
}
