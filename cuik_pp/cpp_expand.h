
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
        unsigned char* output_path_start = tb_arena_alloc(&c->tmp_arena, FILENAME_MAX + 4);
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
        unsigned char* out = tb_arena_alloc(&c->tmp_arena, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", c->unique_counter);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else if (string_equals_cstr(&t->content, "__LINE__")) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t->location);

        // line number as a string
        unsigned char* out = tb_arena_alloc(&c->tmp_arena, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", r.line);
        // trim_the_shtuffs(c, &out[length + 1]);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else {
        return false;
    }
}

static void dump_tokens(Cuik_CPP* restrict ctx, const char* tag, int start, int end, int depth) {
    /*FOR_N(i, 0, depth) {
        printf("  ");
    }
    printf("TOKENS %-20s [%d, %d): ", tag, start, end);
    FOR_N(i, start, end) {
        Token t = ctx->tokens.list.tokens[i];
        if (t.expanded) {
            printf("\x1b[32m");
        }
        printf("%.*s ", (int)t.content.length, t.content.data);
        if (t.expanded) {
            printf("\x1b[0m");
        }
    }
    printf("\n");*/
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
        if (last_pos >= 0 && pos != last_pos) {
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

static bool invoke_eat(Cuik_CPP* restrict ctx, Lexer* in, Token* out_t, InvokeCursor* c, TknType type) {
    InvokeCursor next = advance(*c);
    if (c->elem == NULL) {
        if (in == NULL) {
            return false;
        }

        // read from lexer
        unsigned char* savepoint = in->current;
        *out_t = lexer_read(in);
        if (out_t->type != type) {
            in->current = savepoint;
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

static Token read_one(Cuik_CPP* restrict ctx, Lexer* in, InvokeCursor* c) {
    if (c->elem == NULL) {
        // read from lexer
        return lexer_read(in);
    } else {
        Token t = ctx->tokens.list.tokens[c->pos];
        *c = advance(*c);
        return t;
    }
}

// returns the number of newly inserted tokens at the read_head
static int expand_identifier(Cuik_CPP* restrict ctx, Lexer* in, InvokeElem* parent, int read_head, int end_token, uint32_t parent_macro, MacroDef* def, int depth, int* out_read_tail) {
    if (expand_builtin_idents(ctx, &ctx->tokens.list.tokens[read_head])) {
        return 0;
    }

    assert(end_token <= dyn_array_length(ctx->tokens.list.tokens));
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
    ArenaArray(int) args_to_expand = NULL;
    bool has_varargs = false;

    int read_tail = read_head+1;
    ptrdiff_t dt = 0;
    if (*param_str == '(') {
        cursor = advance(cursor);

        // ignore this expansion if we're missing the opening paren
        Token arg_t;
        if (!invoke_eat(ctx, in, &arg_t, &cursor, '(')) {
            if (out_read_tail) { *out_read_tail = read_tail; }
            return 0;
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
        Token param_t = lexer_read(&param_lexer);
        while (param_t.type && param_t.type != ')') {
            // expect comma
            if (aarray_length(args)) {
                if (param_t.type != TOKEN_COMMA) {
                    __debugbreak();
                }
                param_t = lexer_read(&param_lexer);
            }

            // arg name
            MacroArg a = { .key = param_t.content };
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
            dump_tokens(ctx, "With args", read_head, read_tail, depth);
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

                arg_t = read_one(ctx, in, &cursor);
            }

            dt += dyn_array_length(ctx->tokens.list.tokens) - read_tail;
            read_tail = dyn_array_length(ctx->tokens.list.tokens);
            end_token = read_tail;
        }
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
                    __debugbreak();
                }
                arg_t = tokens[arg_head++];
            }

            MacroArg a = { .key = arg_t.content };
            args[arg_c].token_start = arg_head - 1;
            args[arg_c].val.data = arg_t.content.data;

            int parens = 0;
            for (;;) {
                if (arg_t.type == '(') { parens++; }
                if (arg_t.type == ')') {
                    if (parens == 0) { break; }
                    parens--;
                }
                if (arg_t.type == ',' && parens == 0) { break; }

                // convert token location into macro relative
                /*if ((arg_t.location.raw & SourceLoc_IsMacro) == 0) {
                    uint32_t pos = arg_t.location.raw & ((1u << SourceLoc_FilePosBits) - 1);
                    arg_t.location = encode_macro_loc(macro_id, pos);
                }*/
                arg_t = tokens[arg_head++];
            }
            assert(parens == 0);

            args[arg_c].val.length = (arg_t.content.data - args[arg_c].val.data);
            args[arg_c].token_end = arg_head - 1;
            arg_c += 1;
        }
        assert(arg_t.type == ')');

        args_to_expand = aarray_create(&ctx->tmp_arena, int, 8);
    }

    if (out_read_tail) {
        *out_read_tail = read_tail;
    }

    // special case, the macro is empty
    if (def_str.length == 0) {
        size_t start = dyn_array_length(ctx->tokens.list.tokens);
        if (start != read_tail) {
            memmove(&ctx->tokens.list.tokens[read_head], &ctx->tokens.list.tokens[read_tail], (start - read_tail) * sizeof(Token));
        }
        ptrdiff_t diff = -(read_tail - read_head);
        dyn_array_set_length(ctx->tokens.list.tokens, start + diff);
        tb_arena_restore(&ctx->tmp_arena, sp);
        return diff + dt;
    }

    // Subst & Stringize
    size_t start = dyn_array_length(ctx->tokens.list.tokens);
    for (;;) {
        Token def_t = lexer_read(&def_lexer);
        if (def_t.type == 0 || def_t.hit_line) {
            break;
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

                def_t = quote_token_array(ctx, def_t.location, args[arg].token_start, args[arg].token_end);
            } else {
                // rollback and paste both the # and ##
                def_lexer.current = (unsigned char*) def_t.content.data + def_t.content.length;
            }
        } else if (def_t.type == TOKEN_IDENTIFIER) {
            ptrdiff_t arg = find_arg(args, def_t.content);
            if (arg >= 0) {
                // subst, just paste all the tokens into the final stream
                aarray_push(args_to_expand, dyn_array_length(ctx->tokens.list.tokens));
                FOR_N(i, args[arg].token_start, args[arg].token_end) {
                    push_token(ctx, ctx->tokens.list.tokens[i]);
                }
                aarray_push(args_to_expand, dyn_array_length(ctx->tokens.list.tokens));
                continue;
            }
        }

        // convert token location into macro relative
        if ((def_t.location.raw & SourceLoc_IsMacro) == 0) {
            def_t.location = encode_macro_loc(macro_id, def_t.content.data - def_lexer.start);
        }

        push_token(ctx, def_t);
    }

    // Concat tokens
    size_t end = dyn_array_length(ctx->tokens.list.tokens);
    size_t j = start;
    for (size_t i = start; i < end;) {
        Token* t = &ctx->tokens.list.tokens[i];
        if (t->type == TOKEN_DOUBLE_HASH) {
            if (j > start && i+1 < end) {
                String a = ctx->tokens.list.tokens[j-1].content;
                String b = ctx->tokens.list.tokens[i+1].content;

                // Literally join the data
                unsigned char* out = tb_arena_alloc(&ctx->tmp_arena, a.length + b.length + 16);
                memcpy(out, a.data, a.length);
                memcpy(out + a.length, b.data, b.length);
                memset(&out[a.length + b.length], 0, 16);

                // generate a new token and see what happens
                Lexer scratch = { 0, out, out };
                Token joined = lexer_read(&scratch);
                joined.location = ctx->tokens.list.tokens[i].location;

                // shrink the token list
                ctx->tokens.list.tokens[j-1] = joined;
                i += 2;

                // the resulting token can be expanded
                if (args_to_expand) {
                    aarray_push(args_to_expand, j-1);
                    aarray_push(args_to_expand, j);
                }
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

    if (end != j) {
        dyn_array_set_length(ctx->tokens.list.tokens, j);
        end = j;
    }

    dump_tokens(ctx, "Pre-expand", start, end, depth);

    // Expand arguments
    if (args_to_expand) {
        int shift = 0;
        FOR_N(i, 0, aarray_length(args_to_expand)/2) {
            int local_start = shift+args_to_expand[i*2 + 0];
            int local_end   = shift+args_to_expand[i*2 + 1];
            for (int j = local_start; j < local_end;) {
                Token* t = &ctx->tokens.list.tokens[j];

                if (!t->expanded && t->type == TOKEN_IDENTIFIER) {
                    // if it failed to expand, it can't expand later during the rescan
                    MacroDef* kid_def = find_define(ctx, t->content.data, t->content.length);
                    if (kid_def != NULL) {
                        int old_top = dyn_array_length(ctx->tokens.list.tokens);

                        int kid_read_tail;
                        ptrdiff_t kid_dt = expand_identifier(ctx, NULL, NULL, j, local_end, macro_id, kid_def, depth+1, &kid_read_tail);
                        end += kid_dt;
                        local_end += kid_dt;
                        shift += kid_dt;
                        assert(end >= start);
                        assert(dyn_array_length(ctx->tokens.list.tokens) == old_top + kid_dt);

                        j = kid_read_tail + kid_dt;
                        continue;
                    } else {
                        t->expanded = true;
                    }
                }
                j += 1;
            }
        }
    }

    size_t hidden = hide_macro(ctx, def);

    // Rescanning
    for (int i = start; i < end;) {
        Token* t = &ctx->tokens.list.tokens[i];

        size_t def_i;
        if (!t->expanded && t->type == TOKEN_IDENTIFIER) {
            MacroDef* kid_def = find_define(ctx, t->content.data, t->content.length);
            if (kid_def != NULL) {
                int kid_read_tail;
                ptrdiff_t kid_dt = expand_identifier(ctx, in, &invoke_elem, i, end, macro_id, kid_def, depth+1, &kid_read_tail);
                end += kid_dt;
                assert(end >= start);

                i = kid_read_tail + kid_dt;
                continue;
            } else if (string_equals(&macro_name, &t->content)) {
                t->expanded = true;
            }
        }
        i += 1;
    }
    unhide_macro(ctx, def, hidden);

    dump_tokens(ctx, "Post-expand", start, end, depth);
    dump_tokens(ctx, "Copying into", read_head, read_tail, depth);
    assert(end >= start);

    // Fast path, replacing a single token with a single token
    ptrdiff_t len = dyn_array_length(ctx->tokens.list.tokens);
    ptrdiff_t diff = (end - start) - (read_tail - read_head);
    if ((end - start) == 1 && (read_tail - read_head) == 1) {
        ctx->tokens.list.tokens[read_head] = ctx->tokens.list.tokens[start];
    } else {
        // Replace the tokens at the read_head
        int old = read_tail - read_head;
        int new = end - start;
        if (old >= new) {
            // we're shrinking, just copy the end piece into the middle and shift everyone
            // down to fit.
            int leftover = old - new;
            memcpy(&ctx->tokens.list.tokens[read_head], &ctx->tokens.list.tokens[start], new * sizeof(Token));
            if (start != read_tail) {
                memmove(&ctx->tokens.list.tokens[read_head + new], &ctx->tokens.list.tokens[read_tail], (start - read_tail) * sizeof(Token));
            }
        } else {
            int extra = new - old;
            dyn_array_put_uninit(ctx->tokens.list.tokens, extra);

            // shift up and then insert the entries
            memmove(&ctx->tokens.list.tokens[read_tail + extra], &ctx->tokens.list.tokens[read_tail], (len - read_tail) * sizeof(Token));
            memmove(&ctx->tokens.list.tokens[read_head], &ctx->tokens.list.tokens[start + extra], new * sizeof(Token));
        }
    }

    dyn_array_set_length(ctx->tokens.list.tokens, start + diff);
    dump_tokens(ctx, "Copied", 0, dyn_array_length(ctx->tokens.list.tokens), depth);
    tb_arena_restore(&ctx->tmp_arena, sp);
    return dt + diff;
}
