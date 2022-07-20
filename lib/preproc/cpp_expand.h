static Lexer make_temporary_lexer(const unsigned char* start) {
    return (Lexer){"<temp>", start, start, 1};
}

static Token* get_last_token(TokenStream* restrict s) {
    assert(arrlen(s->tokens) > 0);
    return &s->tokens[arrlen(s->tokens) - 1];
}

static void expand_double_hash(Cuik_CPP* restrict c, TokenStream* restrict s, Token* last, Lexer* restrict l, SourceLocIndex loc) {
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
    Lexer tmp_lex = (Lexer){l->filepath, out_start, out_start};
    lexer_read(&tmp_lex);

    // make nice joined token
    if (tmp_lex.token_type == TOKEN_IDENTIFIER) {
        if (!is_defined(c, tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start)) {
            *last = (Token){
                classify_ident(tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start),
                loc,
                tmp_lex.token_start,
                tmp_lex.token_end,
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
            tmp_lex.token_end,
        };
    }
    lexer_read(&tmp_lex);

    // NOTE(NeGate): So you're not supposed to have multiple tokens
    // once you've concaternated but... eh
    if (tmp_lex.token_type) {
        *l = savepoint;
    }
}

static SourceLoc* try_for_nicer_loc(TokenStream* s, SourceLoc* loc) {
    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loc = &s->locations[SOURCE_LOC_GET_DATA(loc->line->parent)];
    }

    return loc;
}

static void expand_ident(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    size_t token_length = l->token_end - l->token_start;
    const unsigned char* token_data = l->token_start;

    if (lexer_match(l, 8, "__FILE__") || lexer_match(l, 9, "L__FILE__")) {
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[SOURCE_LOC_GET_DATA(parent_loc)]);

        // filepath as a string
        unsigned char* out_start = gimme_the_shtuffs(c, MAX_PATH + 4);
        unsigned char* out = out_start;

        bool is_wide = (token_data[0] == 'L');
        if (is_wide) *out++ = 'L';

        *out++ = '\"';
        {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*) loc->line->filepath;
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
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[SOURCE_LOC_GET_DATA(parent_loc)]);

        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", loc->line->line);

        trim_the_shtuffs(c, &out[length + 1]);
        Token t = {
            TOKEN_INTEGER,
            get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
            out,
            out + length};
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
            out, out + 1,
        };
        arrput(s->tokens, t);
    } else {
        size_t def_i;
        if (find_define(c, &def_i, token_data, token_length)) {
            int line_of_expansion = l->current_line;

            SourceLocIndex expanded_loc = get_source_location(c, l, s,
                parent_loc,
                SOURCE_LOC_MACRO);

            // Identify macro definition
            lexer_read(l);

            String def = string_from_range(c->macro_bucket_values_start[def_i],
                c->macro_bucket_values_end[def_i]);

            const unsigned char* args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];

            // Sometimes we have a layer of indirection when doing
            // preprocessor expansion:
            //   #define PEAR(X) X;
            //   #define APPLE PEAR
            //   APPLE(int a)
            while (def.length && *args != '(' && l->token_type == '(') {
                // expand and append
                Lexer temp_lex = (Lexer){l->filepath, def.data, def.data};
                lexer_read(&temp_lex);

                size_t token_length = temp_lex.token_end - temp_lex.token_start;
                const unsigned char* token_data = temp_lex.token_start;

                if (!find_define(c, &def_i, token_data, token_length)) break;

                def = string_from_range(c->macro_bucket_values_start[def_i],
                    c->macro_bucket_values_end[def_i]);

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

                    if (l->token_type == TOKEN_STRING_WIDE_SINGLE_QUOTE ||
                        l->token_type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                        start -= 1, end -= 1;
                    }

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

                    value_ranges[i * 2 + 0] = start;
                    value_ranges[i * 2 + 1] = end;

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
                        Lexer arg_lex = (Lexer){l->filepath, args, args};
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
                                key_ranges[i * 2 + 0] = arg_lex.token_start;
                                key_ranges[i * 2 + 1] = arg_lex.token_end;

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
                    // then expand the result one
                    // more time
                    ////////////////////////////////
                    unsigned char* temp_expansion_start = gimme_the_shtuffs(c, 4096);
                    unsigned char* temp_expansion = temp_expansion_start;

                    Lexer def_lex = (Lexer){l->filepath, def.data, def.data, line_of_expansion};
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

                            if (has_varargs && key_count == value_count) {
                                Lexer old = def_lex;

                                if (def_lex.token_type == TOKEN_DOUBLE_HASH) {
                                    lexer_read(&def_lex);

                                    if (lexer_match(&def_lex, sizeof("__VA_ARGS__")-1, "__VA_ARGS__")) {
                                        // we remove the comma since there's no varargs to chew
                                        temp_expansion = fallback;
                                    } else {
                                        // sad boy hours... revert
                                        def_lex = old;
                                    }
                                } else if (lexer_match(&def_lex, sizeof("__VA_ARGS__")-1, "__VA_ARGS__")) {
                                    // we remove the comma since there's no varargs to chew
                                    temp_expansion = fallback;
                                }
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
                            token_length == sizeof("__VA_ARGS__") - 1 &&
                            memcmp(token_data, "__VA_ARGS__", sizeof("__VA_ARGS__") - 1) == 0) {
                            *temp_expansion++ = ' ';

                            // Just slap all the arguments that are after the 'key_count'
                            if (key_count != value_count) {
                                for (size_t i = key_count; i < value_count; i++) {
                                    const unsigned char* end = value_ranges[i * 2 + 1];
                                    const unsigned char* start = value_ranges[i * 2 + 0];
                                    size_t count = end - start;

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
                                    } else if (*p != ' ')
                                        break;

                                    p--;
                                }
                            }
                        } else {
                            int index = -1;
                            for (size_t i = 0; i < key_count; i++) {
                                size_t key_length = key_ranges[i * 2 + 1] - key_ranges[i * 2 + 0];
                                const unsigned char* key = key_ranges[i * 2 + 0];

                                if (token_length == key_length &&
                                    memcmp(key, token_data, token_length) == 0) {
                                    index = i;
                                    break;
                                }
                            }

                            if (index >= 0) {
                                const unsigned char* end = value_ranges[index * 2 + 1];
                                const unsigned char* start = value_ranges[index * 2 + 0];
                                size_t count = end - start;

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
                        Lexer temp_lex = make_temporary_lexer(temp_expansion_start);
                        lexer_read(&temp_lex);

                        *temp_expansion++ = '\0';

                        // macro hide set
                        size_t hidden = hide_macro(c, def_i);
                        expand(c, s, &temp_lex, expanded_loc);
                        unhide_macro(c, def_i, hidden);
                    }
                }

                tls_restore(value_ranges);
            } else if (def.length) {
                // expand and append
                if (*args == '(' && l->token_type != '(') {
                    Token t = {
                        classify_ident(token_data, token_length),
                        expanded_loc,
                        token_data,
                        token_data + token_length,
                    };

                    arrput(s->tokens, t);
                } else {
                    Lexer temp_lex = make_temporary_lexer(def.data);
                    lexer_read(&temp_lex);

                    size_t hidden = hide_macro(c, def_i);
                    expand(c, s, &temp_lex, expanded_loc);
                    unhide_macro(c, def_i, hidden);
                }
            }
        } else {
            // Normal identifier
            assert(l->token_type == TOKEN_IDENTIFIER);

            Token t = {
                classify_ident(token_data, token_length),
                get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
                token_data,
                token_data + token_length,
            };

            arrput(s->tokens, t);
            lexer_read(l);
        }
    }
}

static void expand(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    int depth = 0;

    while (l->token_type && !l->hit_line) {
        if (l->token_type == '(') {
            depth++;
        }

        if (l->token_type == TOKEN_DOUBLE_HASH) {
            SourceLocIndex loc = get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL);
            lexer_read(l);

            Token* last = get_last_token(s);
            expand_double_hash(c, s, last, l, loc);
        } else if (l->token_type != TOKEN_IDENTIFIER) {
            Token t = {
                l->token_type,
                get_source_location(c, l, s, parent_loc, SOURCE_LOC_NORMAL),
                l->token_start,
                l->token_end,
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
