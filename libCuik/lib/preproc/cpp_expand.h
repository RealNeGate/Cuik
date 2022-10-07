static Lexer make_temporary_lexer(unsigned char* start) {
    return (Lexer){"<temp>", start, start, 1};
}

static Token* get_last_token(TokenStream* restrict s) {
    assert(dyn_array_length(s->tokens) > 0);
    return &s->tokens[dyn_array_length(s->tokens) - 1];
}

static bool concat_token(Cuik_CPP* restrict c, String a, String b, Token* out_token) {
    unsigned char* out = gimme_the_shtuffs(c, a.length + b.length + 1);
    memcpy(out, a.data, a.length);
    memcpy(out + a.length, b.data, b.length);
    out[a.length + b.length] = '\0';

    // generate a new token and see what happens
    Lexer l = { "", out, out, 1 };
    lexer_read(&l);

    *out_token = (Token){
        l.token_type, false, 0, l.token_start, l.token_end
    };

    // check if there's any more tokens
    lexer_read(&l);
    if (l.token_type != 0) {
        // they don't concat
        return false;
    }

    return true;
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

typedef struct {
    int key_count;
    String* keys;

    int value_count;
    String* values;

    bool has_varargs;
} MacroArgs;

static void parse_params(Cuik_CPP* restrict c, MacroArgs* args, Lexer* restrict param_lex) {
    args->key_count = 0;
    args->keys = tls_save();

    lexer_read(param_lex);
    expect_from_lexer(param_lex, '(');

    while (param_lex->token_type != ')') {
        if (args->key_count) {
            expect_from_lexer(param_lex, ',');
        }

        if (param_lex->token_type == TOKEN_TRIPLE_DOT) {
            args->has_varargs = true;
            lexer_read(param_lex);
            break;
        } else if (param_lex->token_type == TOKEN_IDENTIFIER) {
            tls_push(sizeof(String));

            args->keys[args->key_count++] = string_from_range(
                param_lex->token_start, param_lex->token_end
            );
            lexer_read(param_lex);
        } else {
            fprintf(stderr, "error %s:%d: expected identifier or triple-dot\n", param_lex->filepath, param_lex->current_line);
            abort();
        }
    }

    expect_from_lexer(param_lex, ')');
}

static void parse_args(Cuik_CPP* restrict c, MacroArgs* args, TokenStream* restrict in) {
    size_t paren_end = match_parenthesis(c, in);

    args->values = convert_tokens_to_value_list_in_tls(c, in, paren_end, &args->value_count);

    in->current = paren_end;
    tokens_next(in);
}

static ptrdiff_t find_arg(MacroArgs* restrict args, String name) {
    for (size_t i = 0; i < args->key_count; i++) {
        if (string_equals(&args->keys[i], &name)) {
            return i;
        }
    }

    return -1;
}

// parse function macros where def_lex is the lexer for the macro definition
// TODO(NeGate): redo the error messages here
static void subst(Cuik_CPP* restrict c, TokenStream* restrict s, String def_str, MacroArgs* restrict args, SourceLocIndex parent_loc, int depth) {
    TokenStream in = lex_line("<temp>", (unsigned char*) def_str.data);

    while (!tokens_eof(&in)) {
        if (tokens_is(&in, TOKEN_HASH)) {
            tokens_next(&in);

            if (!tokens_is(&in, TOKEN_IDENTIFIER)) {
                generic_error(&in, "expected identifier");
                abort();
            }

            // stringize arg
            ptrdiff_t arg_i = find_arg(args, get_token_as_string(&in));
            if (arg_i < 0) {
                generic_error(&in, "cannot stringize unknown argument");
                abort();
            }

            // at best we might double the string length from backslashes
            unsigned char* stringized = gimme_the_shtuffs(c, (args->values[arg_i].length * 2) + 3);
            size_t len = 0;

            stringized[len++] = '"';
            String str = args->values[arg_i];
            for (size_t i = 0; i < str.length; i++) {
                if (str.data[i] == '\\' || str.data[i] == '"' || str.data[i] == '\'') {
                    stringized[len++] = '\\';
                }

                stringized[len++] = str.data[i];
            }
            stringized[len++] = '"';
            stringized[len] = 0;

            Token t = {
                .type = TOKEN_STRING_DOUBLE_QUOTE,
                .location = get_source_location(c, &in, s, parent_loc, SOURCE_LOC_NORMAL),
                .start = stringized,
                .end = &stringized[len],
            };
            dyn_array_put(s->tokens, t);
            tokens_next(&in);
        } else if (tokens_is(&in, TOKEN_DOUBLE_HASH)) {
            tokens_next(&in);

            Token* last = get_last_token(s);
            String a = string_from_range(last->start, last->end);
            String b = get_token_as_string(&in);
            tokens_next(&in);

            ptrdiff_t b_i = find_arg(args, b);
            if (b_i >= 0) {
                b = args->values[b_i];
            }

            Token t;
            if (concat_token(c, a, b, &t)) {
                if (t.type == TOKEN_IDENTIFIER) {
                    t.type = classify_ident(t.start, t.end - t.start);
                    t.location = last->location;

                    if (!is_defined(c, t.start, t.end - t.start)) {
                        *last = t;
                    } else {
                        // remove top
                        dyn_array_set_length(s->tokens, dyn_array_length(s->tokens) - 1);

                        // just gonna replace the old tokens with the concat token
                        // at a point where it looks weird if you stare in a debugger
                        // but the code itself doesn't care.
                        in.current -= 1;
                        in.tokens[in.current] = t;

                        expand_ident(c, s, &in, parent_loc, depth + 1);
                    }
                } else {
                    *last = t;
                }
            }
        } else if (tokens_is(&in, TOKEN_IDENTIFIER)) {
            String str = get_token_as_string(&in);
            SourceLocIndex loc = get_source_location(c, &in, s, parent_loc, SOURCE_LOC_MACRO);
            size_t old = in.current;
            (void)old;

            if (str.data[0] == '_' && string_equals_cstr(&str, "__VA_ARGS__")) {
                size_t key_count = args->key_count, value_count = args->value_count;
                assert(key_count == value_count && "TODO");

                for (size_t i = key_count; i < value_count; i++) {
                    // slap a comma between var args
                    if (i != key_count) {
                        const unsigned char* str = (const unsigned char*) ",";
                        Token t = {
                            .type = TOKEN_STRING_WIDE_DOUBLE_QUOTE,
                            .location = loc,
                            .start = str,
                            .end = str + 1,
                        };
                        dyn_array_put(s->tokens, t);
                    }

                    String def = args->values[i];
                    TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", (uint8_t*) def.data, (uint8_t*) &def.data[def.length]);
                    expand(c, s, &temp_tokens, dyn_array_length(temp_tokens.tokens), false, loc);
                    free_token_stream(&temp_tokens);
                }
            } else {
                ptrdiff_t arg_i = find_arg(args, str);
                if (arg_i >= 0) {
                    // macro arguments must be expanded before they're placed
                    String def = args->values[arg_i];
                    TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", (uint8_t*) def.data, (uint8_t*) &def.data[def.length]);
                    expand(c, s, &temp_tokens, dyn_array_length(temp_tokens.tokens), false, loc);
                    free_token_stream(&temp_tokens);
                } else {
                    // Normal identifier
                    Token t = {
                        classify_ident(str.data, str.length), false, loc,
                        str.data, &str.data[str.length],
                    };
                    dyn_array_put(s->tokens, t);
                }
            }

            tokens_next(&in);
        } else {
            Token t = *tokens_get(&in);
            t.location = get_source_location(c, &in, s, parent_loc, SOURCE_LOC_NORMAL);
            dyn_array_put(s->tokens, t);
            tokens_next(&in);
        }
    }

    free_token_stream(&in);
}

static void expand_ident(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc, int depth) {
    Token* t = tokens_get(in);
    bool hit_line = t->hit_line;
    size_t token_length = t->end - t->start;
    const unsigned char* token_data = t->start;

    assert(tokens_is(in, TOKEN_IDENTIFIER));
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

            // function macro
            if (*args == '(' && tokens_is(in, '(')) {
                tokens_next(in);

                ////////////////////////////////
                // Parse the arguments
                ////////////////////////////////
                MacroArgs arglist = { 0 };
                parse_args(c, &arglist, in);

                // We dont need to parse this part if it expands into nothing
                if (def.length) {
                    Lexer args_lexer = (Lexer){in->filepath, (unsigned char*) args, (unsigned char*) args};
                    parse_params(c, &arglist, &args_lexer);

                    /*printf("FUNCTION MACRO: %.*s    %.*s\n", (int)token_length, token_data, (int)def.length, def.data);
                    for (size_t i = 0; i < arglist.value_count; i++) {
                        printf("  ['%.*s'] = '%.*s'\n", (int) arglist.keys[i].length, arglist.keys[i].data, (int) arglist.values[i].length, arglist.values[i].data);
                    }
                    printf("\n");*/

                    // macro hide set
                    size_t hidden = hide_macro(c, def_i);
                    // at the bottom layer we create a temporary token stream and then expand from
                    // there to the final token stream
                    TokenStream scratch = { 0 };
                    scratch.locations = dyn_array_create_with_initial_cap(SourceLoc, 32);
                    scratch.tokens = dyn_array_create_with_initial_cap(Token, 32);

                    subst(c, &scratch, def, &arglist, parent_loc, depth + 1);

                    Token t = {0, true, dyn_array_length(s->locations) - 1, NULL, NULL};
                    dyn_array_put(scratch.tokens, t);

                    expand(c, s, &scratch, dyn_array_length(scratch.tokens), false, parent_loc);

                    free_token_stream(&scratch);
                    unhide_macro(c, def_i, hidden);
                }

                // it's a stack and keys is after values so it'll get popped too
                // tls_restore(keys);
                tls_restore(arglist.values);
                return;
            } else if (def.length > 0) {
                // expand and append
                if (*args == '(' && !tokens_is(in, '(')) {
                    Token t = {
                        classify_ident(token_data, token_length), hit_line,
                        expanded_loc, token_data, token_data + token_length,
                    };

                    dyn_array_put(s->tokens, t);
                } else {
                    TokenStream temp_tokens = get_all_tokens_in_buffer("<temp>", (unsigned char*) def.data, (unsigned char*) &def.data[def.length]);

                    size_t hidden = hide_macro(c, def_i);
                    expand(c, s, &temp_tokens, dyn_array_length(temp_tokens.tokens), true, expanded_loc);
                    unhide_macro(c, def_i, hidden);

                    free_token_stream(&temp_tokens);
                }
            }
        } else {
            // Normal identifier
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

        if (!tokens_is(in, TOKEN_IDENTIFIER)) {
            Token t = *tokens_get(in);
            t.location = get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL);

            dyn_array_put(s->tokens, t);
            tokens_next(in);
        } else {
            expand_ident(c, s, in, parent_loc, 1);
        }

        if (tokens_is(in, ')')) {
            if (depth == 0) break;
            depth--;
        }
    }
}
