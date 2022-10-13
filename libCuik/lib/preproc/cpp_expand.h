static Token* get_last_token(TokenStream* restrict s) {
    assert(dyn_array_length(s->list.tokens) > 0);
    return &s->list.tokens[dyn_array_length(s->list.tokens) - 1];
}

static bool concat_token(Cuik_CPP* restrict c, String a, String b, Token* out_token) {
    return true;
}

typedef struct {
    int key_count;
    String* keys;

    int value_count;
    String* values;

    bool has_varargs;
} MacroArgs;

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
static void parse_params(Cuik_CPP* restrict c, MacroArgs* args, Lexer* restrict in) {
    args->key_count = 0;
    args->keys = tls_save();

    Token t = lexer_read(in);
    if (t.type != '(') {
        fprintf(stderr, "error: expected '('\n");
        goto error;
    }

    for (;;) {
        t = lexer_read(in);
        if (t.type == 0) goto error;
        if (t.type == ')') break;

        if (args->key_count) {
            if (t.type != ',') {
                fprintf(stderr, "error: expected comma\n");
                goto error;
            }

            t = lexer_read(in);
        }

        if (t.type == TOKEN_TRIPLE_DOT) {
            args->has_varargs = true;
            break;
        } else if (t.type == TOKEN_IDENTIFIER) {
            tls_push(sizeof(String));

            args->keys[args->key_count++] = t.content;
        } else {
            fprintf(stderr, "error: expected identifier or triple-dot\n");
            goto error;
        }
    }
    return;

    // TODO(NeGate): improve this
    error:
    abort();
}

static void parse_args(Cuik_CPP* restrict c, MacroArgs* restrict args, TokenList* restrict in) {
    size_t value_count = 0;
    String* values = tls_save();

    int paren_depth = 0;
    for (;;) {
        Token t = peek(in);
        if (t.type == 0) {
            in->current -= 1;
            break;
        }

        if (value_count) {
            if (t.type != ',') {
                fprintf(stderr, "error: expected comma\n");
                goto error;
            }

            t = consume(in);
        }

        // we're incrementally building up the string in the "the shtuffs"
        size_t len = 0;
        unsigned char* str = gimme_the_shtuffs(c, 0);
        while (!at_token_list_end(in)) {
            t = consume(in);

            if (t.type == 0) {
                in->current -= 1;
                break;
            } else if (t.type == '(') {
                paren_depth++;
            } else if (t.type == ',') {
                if (paren_depth == 0) {
                    in->current -= 1;
                    break;
                }
            } else if (t.type == ')') {
                if (paren_depth == 0) {
                    break;
                }

                paren_depth--;
            } else if (t.type == TOKEN_STRING_WIDE_DOUBLE_QUOTE || t.type == TOKEN_STRING_WIDE_SINGLE_QUOTE) {
                gimme_the_shtuffs(c, 1);
                str[len++] = 'L';
            }

            // append to string
            String src = t.content;
            gimme_the_shtuffs(c, src.length + 1);

            memcpy(&str[len], src.data, src.length);
            str[len + src.length] = ' ';
            len += src.length + 1;
        }

        // null terminator
        if (len > 0) {
            str[len - 1] = 0;
            len--;
        }

        tls_push(sizeof(String));
        values[value_count++] = (String){ len, str };

        if (t.type == ')' && paren_depth == 0) {
            break;
        }
    }

    args->values = values;
    args->value_count = value_count;
    return;

    // TODO(NeGate): improve this
    error:
    abort();
}

static ptrdiff_t find_arg(MacroArgs* restrict args, String name) {
    for (size_t i = 0; i < args->key_count; i++) {
        if (string_equals(&args->keys[i], &name)) {
            return i;
        }
    }

    return -1;
}

static TokenList convert_line_to_token_list(Cuik_CPP* restrict c, uint32_t macro_id, unsigned char* data) {
    Lexer l = {
        .start = data, .current = data,
    };

    TokenList list = { 0 };
    list.tokens = dyn_array_create_with_initial_cap(Token, 32);
    for (;;) {
        Token t = lexer_read(&l);
        if (t.type == 0 || t.hit_line) break;

        t.location = encode_macro_loc(macro_id, t.content.data - l.start);
        dyn_array_put(list.tokens, t);
    }

    dyn_array_put(list.tokens, (Token){ 0 });
    return list;
}

static void copy_tokens(Cuik_CPP* restrict c, TokenList* restrict out_tokens, Lexer* restrict in) {
    for (;;) {
        Token t = lexer_read(in);
        if (t.type == 0) break;

        dyn_array_put(out_tokens->tokens, t);
    }
}

// parse function macros where def_lex is the lexer for the macro definition
// TODO(NeGate): redo the error messages here
static bool subst(Cuik_CPP* restrict c, TokenList* out_tokens, uint8_t* def_str, MacroArgs* restrict args, uint32_t macro_id) {
    Lexer in = { 0, def_str, def_str };

    for (;;) {
        Token t = lexer_read(&in);
        if (t.type == 0 || t.hit_line) {
            return false;
        }

        // convert token location into macro relative
        t.location = encode_macro_loc(macro_id, t.content.data - in.start);

        if (t.type == TOKEN_HASH) {
            t = lexer_read(&in);
            if (t.type != TOKEN_IDENTIFIER) {
                // generic_error(&in, "expected identifier");
                abort();
            }

            // stringize arg
            ptrdiff_t arg_i = find_arg(args, t.content);
            if (arg_i < 0) {
                // generic_error(&in, "cannot stringize unknown argument");
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

            dyn_array_put(out_tokens->tokens, (Token){
                    .type = TOKEN_STRING_DOUBLE_QUOTE,
                    .location = t.location,
                    .content = { len, stringized },
                });
        } else if (t.type == TOKEN_DOUBLE_HASH) {
            Token* last = &out_tokens->tokens[dyn_array_length(out_tokens->tokens) - 1];
            String a = last->content;

            String b = lexer_read(&in).content;
            ptrdiff_t b_i = find_arg(args, b);
            if (b_i >= 0) b = args->values[b_i];

            // Literally join the data
            unsigned char* out = gimme_the_shtuffs(c, a.length + b.length + 16);
            memcpy(out, a.data, a.length);
            memcpy(out + a.length, b.data, b.length);
            memset(&out[a.length + b.length], 0, 16);

            // generate a new token and see what happens
            Lexer scratch = { .start = out, .current = out };
            Token joined = lexer_read(&scratch);

            // if they only form one token then process it
            if (lexer_read(&scratch).type == 0) {
                if (joined.type == TOKEN_IDENTIFIER) {
                    joined.type = classify_ident(joined.content.data, joined.content.length);
                    joined.location = t.location;

                    if (!is_defined(c, joined.content.data, joined.content.length)) {
                        *last = joined;
                    } else {
                        // remove top
                        dyn_array_pop(out_tokens);

                        // replace with expanded identifier
                        TokenList scratch = {
                            .tokens = dyn_array_create_with_initial_cap(Token, 2)
                        };
                        dyn_array_put(scratch.tokens, joined);
                        dyn_array_put(scratch.tokens, (Token){ 0 });

                        if (!expand_ident(c, out_tokens, &scratch, macro_id)) {
                            return false;
                        }

                        dyn_array_destroy(scratch.tokens);
                    }
                } else {
                    *last = joined;
                }
            }
        } else if (t.type == TOKEN_IDENTIFIER) {
            if (t.content.data[0] == '_' && string_equals_cstr(&t.content, "__VA_ARGS__")) {
                size_t key_count = args->key_count, value_count = args->value_count;
                assert(key_count == value_count && "TODO");

                for (size_t i = key_count; i < value_count; i++) {
                    // slap a comma between var args
                    if (i != key_count) {
                        dyn_array_put(out_tokens->tokens, (Token){
                                .type = ',',
                                .location = t.location,
                                .content = string_cstr(","),
                            });
                    }

                    TokenList scratch = convert_line_to_token_list(c, macro_id, (uint8_t*) args->values[i].data);
                    if (!expand(c, out_tokens, &scratch, macro_id)) {
                        return false;
                    }
                    dyn_array_destroy(scratch.tokens);
                }
            } else {
                ptrdiff_t arg_i = find_arg(args, t.content);
                if (arg_i >= 0) {
                    // macro arguments must be expanded before they're placed
                    TokenList scratch = convert_line_to_token_list(c, macro_id, (uint8_t*) args->values[arg_i].data);
                    if (!expand(c, out_tokens, &scratch, macro_id)) {
                        return false;
                    }
                    dyn_array_destroy(scratch.tokens);
                } else {
                    // Normal identifier
                    t.type = classify_ident(t.content.data, t.content.length);
                    dyn_array_put(out_tokens->tokens, t);
                }
            }
        } else {
            dyn_array_put(out_tokens->tokens, t);
        }
    }

    return true;
}

static bool expand_ident(Cuik_CPP* restrict c, TokenList* restrict out_tokens, TokenList* restrict in, uint32_t parent_macro) {
    Token t = consume(in);

    // can a loc come up in yo crib?
    SourceLoc og_loc = t.location;

    if (parent_macro != 0) {
        // convert token location into macro relative
        uint32_t macro_pos = c->tokens.invokes[parent_macro].call_site.raw;

        if ((t.location.raw & SourceLoc_IsMacro) == 0) {
            uint32_t pos = t.location.raw & ((1u << SourceLoc_FilePosBits) - 1);
            t.location = encode_macro_loc(parent_macro, pos - macro_pos);
        }
    }

    size_t token_length = t.content.length;
    const unsigned char* token_data = t.content.data;
    if (memeq(token_data, token_length, "__FILE__", 8) ||
        memeq(token_data, token_length, "L__FILE__", 9)) {
        Cuik_File* f = cuikpp_find_file(&c->tokens, t.location);
        if (f == NULL) {
            assert(0 && "cuikpp_find_file failed?");
        }

        // filepath as a string
        unsigned char* output_path_start = gimme_the_shtuffs(c, MAX_PATH + 4);
        unsigned char* output_path = output_path_start;

        bool is_wide = (token_data[0] == 'L');
        if (is_wide) *output_path++ = 'L';

        *output_path++ = '\"';
        {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* input_path = (const char*) f->filename;
            if (strlen(input_path) >= MAX_PATH) {
                // generic_error(in, "preprocessor error: __FILE__ generated a file path that was too long\n");
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

        t.type = is_wide ? TOKEN_STRING_WIDE_DOUBLE_QUOTE : TOKEN_STRING_DOUBLE_QUOTE;
        t.content = string_from_range(output_path_start, output_path - 1);
        dyn_array_put(out_tokens->tokens, t);
    } else if (memeq(token_data, token_length, "__COUNTER__", 11)) {
        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", c->unique_counter);
        trim_the_shtuffs(c, &out[length + 1]);

        t.type = TOKEN_INTEGER;
        t.content = (String){ length, out };
        dyn_array_put(out_tokens->tokens, t);
    } else if (memeq(token_data, token_length, "__LINE__", 8)) {
        ResolvedSourceLoc r;
        if (!cuikpp_find_location(&c->tokens, t.location, &r)) {
            assert(0 && "__LINE__ failed?");
        }

        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", r.line);
        trim_the_shtuffs(c, &out[length + 1]);

        t.type = TOKEN_INTEGER;
        t.content = (String){ length, out };
        dyn_array_put(out_tokens->tokens, t);
    } else {
        size_t def_i;
        if (find_define(c, &def_i, token_data, token_length)) {
            String def = string_from_range(c->macro_bucket_values_start[def_i], c->macro_bucket_values_end[def_i]);

            // create macro invoke site
            uint32_t macro_id = dyn_array_length(c->tokens.invokes);
            dyn_array_put(c->tokens.invokes, (MacroInvoke){
                    .name        = t.content,
                    .parent      = parent_macro,
                    .def_site    = c->macro_bucket_source_locs[def_i],
                    .call_site   = og_loc,
                    .def_length  = def.length,
                });

            const unsigned char* args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];

            // function macro
            Token paren_peek = peek(in);
            if (*args == '(' && paren_peek.type == '(') {
                consume(in);

                ////////////////////////////////
                // Parse the arguments
                ////////////////////////////////
                MacroArgs arglist = { 0 };
                parse_args(c, &arglist, in);

                // We dont need to parse this part if it expands into nothing
                if (def.length) {
                    Lexer args_lexer = { 0, (unsigned char*) args, (unsigned char*) args };
                    parse_params(c, &arglist, &args_lexer);

                    /*printf("FUNCTION MACRO: %.*s    %.*s\n", (int)token_length, token_data, (int)def.length, def.data);
                    for (size_t i = 0; i < arglist.value_count; i++) {
                        printf("  ['%.*s'] = '%.*s'\n", (int) arglist.keys[i].length, arglist.keys[i].data, (int) arglist.values[i].length, arglist.values[i].data);
                    }
                    printf("\n");*/

                    // macro hide set
                    size_t hidden = hide_macro(c, def_i);
                    size_t old = dyn_array_length(c->scratch_list.tokens);

                    TokenList scratch = {
                        .tokens = dyn_array_create_with_initial_cap(Token, 2)
                    };

                    // before expanding the child macros we need to substitute all
                    // the arguments in, handle stringizing and ## concaternation.
                    subst(c, &scratch, (uint8_t*) def.data, &arglist, macro_id);
                    dyn_array_put(scratch.tokens, (Token){ 0 });

                    // c->scratch_list.current = old;
                    // expand(c, out_tokens, &c->scratch_list, macro_id);
                    expand(c, out_tokens, &scratch, macro_id);

                    dyn_array_destroy(scratch.tokens);
                    unhide_macro(c, def_i, hidden);
                }

                // it's a stack and keys is after values so it'll get popped too
                // tls_restore(keys);
                tls_restore(arglist.values);
            } else if (def.length > 0) {
                // expand and append
                if (*args == '(' && paren_peek.type != '(') {
                    t.type = classify_ident(t.content.data, t.content.length);
                    dyn_array_put(out_tokens->tokens, t);
                } else {
                    size_t hidden = hide_macro(c, def_i);

                    TokenList list = convert_line_to_token_list(c, macro_id, (unsigned char*) def.data);
                    expand(c, out_tokens, &list, macro_id);
                    unhide_macro(c, def_i, hidden);

                    dyn_array_destroy(list.tokens);
                }
            }
        } else {
            // Normal identifier
            t.type = classify_ident(t.content.data, t.content.length);
            dyn_array_put(out_tokens->tokens, t);
        }
    }

    return true;
}

static bool expand(Cuik_CPP* restrict c, TokenList* out_tokens, TokenList* restrict in, uint32_t parent_macro) {
    int depth = 0;

    while (!at_token_list_end(in)) {
        size_t savepoint = in->current;
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            break;
        }

        depth += (t.type == '(');

        if (t.type != TOKEN_IDENTIFIER) {
            if (parent_macro != 0) {
                // convert token location into macro relative
                uint32_t macro_pos = c->tokens.invokes[parent_macro].call_site.raw;

                if ((t.location.raw & SourceLoc_IsMacro) == 0) {
                    uint32_t pos = t.location.raw & ((1u << SourceLoc_FilePosBits) - 1);
                    t.location = encode_macro_loc(parent_macro, pos);
                }
            }

            dyn_array_put(out_tokens->tokens, t);
        } else {
            in->current = savepoint;
            if (!expand_ident(c, out_tokens, in, parent_macro)) {
                return false;
            }
        }

        if (t.type == ')') {
            if (depth == 0) break;
            depth--;
        }
    }

    return (depth == 0);
}
