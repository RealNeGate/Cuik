static Lexer make_temporary_lexer(const unsigned char* start) {
    return (Lexer){"<temp>", start, start, 1};
}

static Token* get_last_token(TokenStream* restrict s) {
    assert(dyn_array_length(s->tokens) > 0);
    return &s->tokens[dyn_array_length(s->tokens) - 1];
}

static bool concat_token(TokenStream* restrict in, Token* last, unsigned char* out, size_t capacity, Token* out_token) {
    // double hash is in the middle
    Token* other = &in->tokens[in->current];

    size_t len1 = last->end - last->start;
    size_t len2 = other->end - other->start;
    if (len1 + len2 + 1 >= capacity) {
        fprintf(stderr, "Internal preprocessor error: couldn't concat tokens (too large %zu, limit: %zu)", len1 + len2, capacity - 1);
        abort();
    }

    memcpy(out, last->start, len1);
    memcpy(out + len1, other->start, len2);
    out[len1 + len2] = '\0';

    // generate a new token and see what happens
    Lexer l = { "", out, out, 1 };
    lexer_read(&l);

    *out_token = (Token){
        l.token_type, last->hit_line | other->hit_line, 0, l.token_start, l.token_end
    };

    // check if there's any more tokens
    lexer_read(&l);
    if (l.token_type != 0) {
        // they don't concat
        return false;
    }

    return true;
}

static void expand_double_hash(Cuik_CPP* restrict c, TokenStream* restrict s, Token* last, TokenStream* restrict in, SourceLocIndex loc) {
    unsigned char* concat_buffer = gimme_the_shtuffs(c, 256);
    // report(REPORT_INFO, NULL, s, loc, "CONCAT");

    Token t;
    if (concat_token(in, last, concat_buffer, 256, &t)) {
        if (t.type == TOKEN_IDENTIFIER) {
            if (!is_defined(c, t.start, t.end - t.start)) {
                *last = (Token){
                    classify_ident(t.start, t.end - t.start),
                    t.hit_line, loc, t.start, t.end,
                };
            } else {
                TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", concat_buffer, NULL);

                // remove top
                dyn_array_set_length(s->tokens, dyn_array_length(s->tokens) - 1);
                expand_ident(c, s, &temp_tokens, loc);

                free_token_stream(&temp_tokens);
            }
        } else {
            *last = t;
        }

        in->current += 1;
    }
}

static SourceLoc* try_for_nicer_loc(TokenStream* s, SourceLoc* loc) {
    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loc = &s->locations[loc->line->parent];
    }

    return loc;
}

static size_t match_parenthesis(Cuik_CPP* restrict c, TokenStream* restrict in) {
    int depth = 0;
    size_t old = in->current;

    for (;;) {
        TknType t = tokens_get(in)->type;

        if (t == 0) {
            break;
        } else if (t == '(') {
            depth++;
        } else if (t == ')') {
            if (depth == 0) {
                break;
            }
            depth--;
        }
        tokens_next(in);
    }
    expect(in, ')');

    size_t result = in->current - 1;
    in->current = old;
    return result;
}

// allocates a list of comma separated values (the rules for macro arguments)
// in the temporary storage
static String* convert_tokens_to_value_list_in_tls(Cuik_CPP* restrict c, TokenStream* restrict in, size_t end_token_index, int* out_value_count) {
    String* values = tls_save();
    int value_count = 0;

    while (!tokens_eof(in) && in->current != end_token_index) {
        tls_push(sizeof(String));
        int i = value_count++;

        int paren_depth = 0;
        const unsigned char* start = tokens_get(in)->start;
        const unsigned char* end = start;

        // we're incrementally building up the string in the "the shtuffs"
        size_t len = 0;
        unsigned char* str = gimme_the_shtuffs(c, 0);
        while (in->current != end_token_index) {
            TknType t = tokens_get(in)->type;
            if (t == 0) {
                break;
            } else if (t == '(') {
                paren_depth++;
            } else if (t == ')') {
                if (paren_depth == 0) {
                    tokens_next(in);
                    break;
                }

                paren_depth--;
            } else if (t == ',') {
                if (paren_depth == 0) {
                    break;
                }
            } else if (t == TOKEN_STRING_WIDE_DOUBLE_QUOTE || t == TOKEN_STRING_WIDE_SINGLE_QUOTE) {
                gimme_the_shtuffs(c, 1);
                str[len++] = 'L';
            }

            // append to string
            String src = string_from_range(tokens_get(in)->start, tokens_get(in)->end);
            gimme_the_shtuffs(c, src.length + 1);

            memcpy(&str[len], src.data, src.length);
            str[len + src.length] = ' ';
            len += src.length + 1;

            // advance
            tokens_next(in);
        }

        // null terminator
        if (len > 0) {
            str[len - 1] = 0;
            len--;
        }

        values[i].data = str;
        values[i].length = len;

        if (tokens_is(in, ',')) {
            tokens_next(in);
        }
    }

    *out_value_count = value_count;
    return values;
}

static void expand_ident(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc) {
    Token* t = tokens_get(in);
    bool hit_line = t->hit_line;
    size_t token_length = t->end - t->start;
    const unsigned char* token_data = t->start;

    if (tokens_match(in, 8, "__FILE__") || tokens_match(in, 9, "L__FILE__")) {
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[parent_loc]);

        // filepath as a string
        unsigned char* output_path_start = gimme_the_shtuffs(c, MAX_PATH + 4);
        unsigned char* output_path = output_path_start;

        bool is_wide = (token_data[0] == 'L');
        if (is_wide) *output_path++ = 'L';

        *output_path++ = '\"';
        {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* input_path = (const char*) loc->line->filepath;
            if (strlen(input_path) >= MAX_PATH) {
                generic_error(in, "preprocessor error: __FILE__ generated a file path that was too long\n");
                abort();
            }

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
        trim_the_shtuffs(c, output_path);

        Token t = {
            is_wide ? TOKEN_STRING_WIDE_DOUBLE_QUOTE : TOKEN_STRING_DOUBLE_QUOTE,
            get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL), hit_line,
            output_path_start, output_path - 1
        };
        dyn_array_put(s->tokens, t);
        tokens_next(in);
    } else if (tokens_match(in, 11, "__COUNTER__")) {
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[parent_loc]);

        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", c->unique_counter);

        trim_the_shtuffs(c, &out[length + 1]);
        Token t = {
            TOKEN_INTEGER, hit_line,
            get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL),
            out, out + length
        };
        dyn_array_put(s->tokens, t);
        tokens_next(in);
    } else if (tokens_match(in, 8, "__LINE__")) {
        SourceLoc* loc = try_for_nicer_loc(s, &s->locations[parent_loc]);

        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", loc->line->line);

        trim_the_shtuffs(c, &out[length + 1]);
        Token t = {
            TOKEN_INTEGER, hit_line,
            get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL),
            out, out + length
        };
        dyn_array_put(s->tokens, t);
        tokens_next(in);
    } else if (tokens_match(in, 7, "defined")) {
        tokens_next(in);

        const unsigned char* start = NULL;
        const unsigned char* end = NULL;

        // optional parenthesis
        if (tokens_is(in, '(')) {
            tokens_next(in);

            if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                generic_error(in, "expected identifier!");
            }

            Token* t = tokens_get(in);
            start = t->start;
            end = t->end;

            tokens_next(in);
            expect(in, ')');
        } else if (tokens_is(in, TOKEN_IDENTIFIER)) {
            Token* t = tokens_get(in);
            start = t->start;
            end = t->end;
            tokens_next(in);
        } else {
            generic_error(in, "expected identifier!");
        }

        bool found = is_defined(c, start, end - start);

        // we really just allocated a single byte just to store this lmao
        unsigned char* out = gimme_the_shtuffs(c, 2);
        out[0] = found ? '1' : '0';
        out[1] = '\0';

        //printf("Is '%.*s' defined? %s\n", (int)(end-start), start, found?"Yes":"No");
        Token t = {
            TOKEN_INTEGER, hit_line,
            get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL),
            out, out + 1,
        };
        dyn_array_put(s->tokens, t);
    } else {
        size_t def_i;
        if (find_define(c, &def_i, token_data, token_length)) {
            int line_of_expansion = tokens_get_location_line(in);

            SourceLocIndex expanded_loc = get_source_location(
                c, in, s, parent_loc, SOURCE_LOC_MACRO
            );
            s->locations[expanded_loc].expansion = c->macro_bucket_source_locs[def_i];

            // Identify macro definition
            tokens_next(in);

            String def = string_from_range(c->macro_bucket_values_start[def_i], c->macro_bucket_values_end[def_i]);
            const unsigned char* args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];

            // Some macros immediately alias others so this is supposed to avoid the
            // heavier costs... but it's broken rn
            while (def.length && *args != '(' && tokens_is(in, '(')) {
                // expand and append
                Lexer temp_lex = (Lexer){in->filepath, def.data, def.data};
                lexer_read(&temp_lex);

                size_t token_length = temp_lex.token_end - temp_lex.token_start;
                const unsigned char* token_data = temp_lex.token_start;

                if (def.length == token_length && !find_define(c, &def_i, token_data, token_length)) {
                    break;
                }

                def = string_from_range(
                    c->macro_bucket_values_start[def_i],
                    c->macro_bucket_values_end[def_i]
                );

                args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];
            }

            // function macro
            if (*args == '(' && tokens_is(in, '(')) {
                tokens_next(in);

                ////////////////////////////////
                // Parse the arguments
                ////////////////////////////////
                int value_count;
                String* values;
                #if 1
                {
                    size_t paren_end = match_parenthesis(c, in);

                    values = convert_tokens_to_value_list_in_tls(c, in, paren_end, &value_count);

                    in->current = paren_end;
                    tokens_next(in);
                }
                #else
                {
                    size_t paren_end = match_parenthesis(c, in);

                    // This expansion is temporary
                    size_t old_tokens_length = dyn_array_length(s->tokens);
                    s->current = old_tokens_length;

                    // hide macro then expand
                    size_t hidden = hide_macro(c, def_i);
                    expand(c, s, in, paren_end, false, expanded_loc);
                    unhide_macro(c, def_i, hidden);

                    // Insert a null token at the end
                    Token t = {0, true, dyn_array_length(s->locations) - 1, NULL, NULL};
                    dyn_array_put(s->tokens, t);

                    s->current = old_tokens_length;
                    values = convert_tokens_to_value_list_in_tls(c, s, &value_count);

                    // Restore stuff, note that token streams don't own the tokens
                    // so the values list is still alive
                    arrsetlen(s->tokens, old_tokens_length);

                    in->current = paren_end;
                    tokens_next(in);
                }
                #endif

                // We dont need to parse this part if it expands into nothing
                if (def.length) {
                    // Parse macro function arg names
                    int key_count = 0;
                    String* keys = tls_save();
                    bool has_varargs = false;

                    {
                        Lexer arg_lex = (Lexer){in->filepath, args, args};
                        lexer_read(&arg_lex);
                        expect_from_lexer(&arg_lex, '(');

                        while (arg_lex.token_type != ')') {
                            if (key_count) {
                                expect_from_lexer(&arg_lex, ',');
                            }

                            if (arg_lex.token_type == TOKEN_TRIPLE_DOT) {
                                has_varargs = true;
                                lexer_read(&arg_lex);
                                break;
                            } else if (arg_lex.token_type == TOKEN_IDENTIFIER) {
                                tls_push(sizeof(String));

                                int i = key_count++;
                                keys[i].data = arg_lex.token_start;
                                keys[i].length = arg_lex.token_end - arg_lex.token_start;

                                lexer_read(&arg_lex);
                            } else {
                                fprintf(stderr, "error %s:%d: expected identifier or triple-dot\n", arg_lex.filepath, arg_lex.current_line);
                                abort();
                            }
                        }

                        expect_from_lexer(&arg_lex, ')');
                    }

                    ////////////////////////////////
                    // Stream over the text hoping
                    // to replace some identifiers
                    // then expand the result one
                    // more time
                    ////////////////////////////////
                    unsigned char* temp_expansion_start = gimme_the_shtuffs(c, 4096);
                    unsigned char* temp_expansion = temp_expansion_start;

                    Lexer def_lex = (Lexer){in->filepath, def.data, def.data, line_of_expansion};
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
                                    // slap a comma between var args
                                    if (i != key_count) {
                                        *temp_expansion++ = ',';
                                        *temp_expansion++ = ' ';
                                    }

                                    String v = values[i];
                                    for (size_t j = 0; j < v.length; j++) {
                                        if (v.data[j] == '\r' || v.data[j] == '\n') {
                                            *temp_expansion++ = ' ';
                                        } else {
                                            *temp_expansion++ = v.data[j];
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
                                if (token_length == keys[i].length &&
                                    memcmp(keys[i].data, token_data, token_length) == 0) {
                                    index = i;
                                    break;
                                }
                            }

                            if (index >= 0) {
                                const unsigned char* start = values[index].data;
                                size_t count = values[index].length;

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
                        *temp_expansion++ = '\0';
                        TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", temp_expansion_start, NULL);
                        size_t old = dyn_array_length(s->tokens);

                        // macro hide set
                        size_t hidden = hide_macro(c, def_i);
                        expand(c, s, &temp_tokens, dyn_array_length(temp_tokens.tokens), true, expanded_loc);
                        unhide_macro(c, def_i, hidden);

                        free_token_stream(&temp_tokens);
                    }
                }

                // it's a stack and keys is after values so it'll get popped too
                // tls_restore(keys);
                tls_restore(values);
            } else if (def.length) {
                // expand and append
                if (*args == '(' && !tokens_is(in, '(')) {
                    Token t = {
                        classify_ident(token_data, token_length), hit_line,
                        expanded_loc, token_data, token_data + token_length,
                    };

                    dyn_array_put(s->tokens, t);
                } else {
                    TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", def.data, &def.data[def.length]);

                    size_t hidden = hide_macro(c, def_i);
                    expand(c, s, &temp_tokens, dyn_array_length(temp_tokens.tokens), true, expanded_loc);
                    unhide_macro(c, def_i, hidden);

                    free_token_stream(&temp_tokens);
                }
            }
        } else {
            // Normal identifier
            assert(tokens_is(in, TOKEN_IDENTIFIER));

            Token t = {
                classify_ident(token_data, token_length), hit_line,
                get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL),
                token_data, token_data + token_length,
            };

            dyn_array_put(s->tokens, t);
            tokens_next(in);
        }
    }
}

static void expand(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, size_t in_stream_end, bool exit_on_hit_line, SourceLocIndex parent_loc) {
    int depth = 0;

    while (!tokens_is(in, 0) && in->current < in_stream_end) {
        if (exit_on_hit_line && tokens_hit_line(in)) {
            break;
        }

        if (tokens_is(in, '(')) {
            depth++;
        }

        if (tokens_is(in, TOKEN_DOUBLE_HASH)) {
            SourceLocIndex loc = get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL);
            tokens_next(in);

            Token* last = get_last_token(s);
            expand_double_hash(c, s, last, in, loc);
        } else if (!tokens_is(in, TOKEN_IDENTIFIER)) {
            Token t = *tokens_get(in);
            t.location = get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL);

            dyn_array_put(s->tokens, t);
            tokens_next(in);
        } else {
            expand_ident(c, s, in, parent_loc);
        }

        if (tokens_is(in, ')')) {
            if (depth == 0) break;
            depth--;
        }
    }
}
