static void dump(TokenNode* head) {
    printf("%p: ", head);
    for (TokenNode* n = head; n; n = n->next) {
        printf("%.*s ", (int) n->t.content.length, n->t.content.data);
    }
    printf("\n");
}

static Token* get_last_token(TokenStream* restrict s) {
    assert(dyn_array_length(s->list.tokens) > 0);
    return &s->list.tokens[dyn_array_length(s->list.tokens) - 1];
}

static bool concat_token(Cuik_CPP* restrict c, String a, String b, Token* out_token) {
    return true;
}

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
static bool parse_params(Cuik_CPP* restrict c, MacroArgs* args, Lexer* restrict in) {
    args->key_count = 0;
    args->keys = tls_save();

    Token t = lexer_read(in);
    if (t.type != '(') {
        fprintf(stderr, "error: expected '('\n");
        return false;
    }

    for (;;) {
        t = lexer_read(in);
        if (t.type == 0) return false;
        if (t.type == ')') break;

        if (args->key_count) {
            if (t.type != ',') {
                fprintf(stderr, "error: expected comma\n");
                return false;
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
            return false;
        }
    }

    return true;
}

static bool parse_args(Cuik_CPP* restrict c, MacroArgs* restrict args, TokenArray* restrict in) {
    size_t value_count = 0;
    MacroArg* values = tls_save();

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
                return false;
            }

            t = consume(in);
        }

        // we're incrementally building up the string in the "the shtuffs"
        size_t len = 0;
        unsigned char* str = gimme_the_shtuffs(c, 0);
        SourceRange loc = { .start = peek(in).location };
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
        loc.end = get_token_range(&in->tokens[in->current - 1]).end;

        // null terminator
        if (len > 0) {
            str[len - 1] = 0;
            len--;
        }

        tls_push(sizeof(MacroArg));
        values[value_count++] = (MacroArg){ .content = { len, str }, .loc = loc };

        if (t.type == ')' && paren_depth == 0) {
            break;
        }
    }

    args->values = values;
    args->value_count = value_count;
    return true;
}

// same as parse_args but with a tokenlist as input... we might want to merge this code somehow
static TokenNode* parse_args2(Cuik_CPP* restrict c, MacroArgs* restrict args, TokenNode* restrict curr) {
    size_t value_count = 0;
    MacroArg* values = tls_save();

    int paren_depth = 0;
    TokenNode* prev = NULL;
    while (curr) {
        Token t = curr->t;
        if (value_count) {
            if (t.type != ',') {
                fprintf(stderr, "error: expected comma\n");
                return false;
            }

            prev = curr, curr = curr->next;
            t = curr->t;
        }

        // we're incrementally building up the string in the "the shtuffs"
        size_t len = 0;
        unsigned char* str = gimme_the_shtuffs(c, 0);
        SourceRange loc = { .start = t.location };
        while (curr != NULL) {
            t = curr->t;

            if (t.type == '(') {
                paren_depth++;
            } else if (t.type == ',') {
                if (paren_depth == 0) {
                    break;
                }
            } else if (t.type == ')') {
                if (paren_depth == 0) {
                    prev = curr, curr = curr->next;
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

            // advance
            prev = curr, curr = curr->next;
        }
        loc.end = get_token_range(&prev->t).end;

        // null terminator
        if (len > 0) {
            str[len - 1] = 0;
            len--;
        }

        tls_push(sizeof(MacroArg));
        values[value_count++] = (MacroArg){ .content = { len, str }, .loc = loc };

        if (t.type == ')' && paren_depth == 0) {
            break;
        }
    }

    args->values = values;
    args->value_count = value_count;
    return curr;
}

static ptrdiff_t find_arg(MacroArgs* restrict args, String name) {
    for (size_t i = 0; i < args->key_count; i++) {
        if (string_equals(&args->keys[i], &name)) {
            return i;
        }
    }

    return -1;
}

static void append_to_list(TokenList* l, Token* t) {
    TokenNode* n = tls_push(sizeof(TokenNode));
    n->next = NULL;
    n->t = *t;

    if (l->head == NULL) {
        l->head = l->tail = n;
    } else {
        l->tail->next = n, l->tail = n;
    }
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

static TokenList into_list(Cuik_CPP* restrict c, uint8_t* def_str, uint32_t macro_id) {
    TokenList l = { def_str };
    Lexer in = { 0, def_str, def_str };

    for (;;) {
        Token t = lexer_read(&in);
        if (t.type == 0 || t.hit_line) break;

        t.location = macroify_loc(t.location, macro_id);
        append_to_list(&l, &t);
    }

    return l;
}

static TokenList line_into_list(TokenArray* restrict in) {
    TokenList l = { 0 };
    for (;;) {
        Token t = peek(in);
        if (t.type == 0 || t.hit_line) break;
        append_to_list(&l, &t);
        in->current += 1;
    }

    return l;
}

static bool expand_builtin_idents(Cuik_CPP* restrict c, Token* t) {
    size_t token_length = t->content.length;
    const unsigned char* token_data = t->content.data;

    if (memeq(token_data, token_length, "__FILE__", 8) ||
        memeq(token_data, token_length, "L__FILE__", 9)) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t->location);

        // filepath as a string
        unsigned char* output_path_start = gimme_the_shtuffs(c, FILENAME_MAX + 4);
        unsigned char* output_path = output_path_start;

        bool is_wide = (token_data[0] == 'L');
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
        trim_the_shtuffs(c, output_path);

        t->type = is_wide ? TOKEN_STRING_WIDE_DOUBLE_QUOTE : TOKEN_STRING_DOUBLE_QUOTE;
        t->content = string_from_range(output_path_start, output_path - 1);
        return true;
    } else if (memeq(token_data, token_length, "__COUNTER__", 11)) {
        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", c->unique_counter);
        trim_the_shtuffs(c, &out[length + 1]);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else if (memeq(token_data, token_length, "__LINE__", 8)) {
        ResolvedSourceLoc r = cuikpp_find_location(&c->tokens, t->location);

        // line number as a string
        unsigned char* out = gimme_the_shtuffs(c, 10);
        size_t length = sprintf_s((char*)out, 10, "%d", r.line);
        trim_the_shtuffs(c, &out[length + 1]);

        t->type = TOKEN_INTEGER;
        t->content = (String){ length, out };
        return true;
    } else {
        return false;
    }
}

static TokenNode* get_previous_link(TokenNode* head, TokenNode* curr) {
    // no previous
    if (head == curr) {
        return NULL;
    }

    while (head->next != curr) {
        head = head->next;
    }

    return head;
}

// parse function macros where def_lex is the lexer for the macro definition
// TODO(NeGate): redo the error messages here
#define NEXT_TOKEN() (curr = curr->next, curr->t)
static bool subst(Cuik_CPP* restrict c, TokenNode* head, const uint8_t* subst_start, MacroArgs* restrict args, uint32_t macro_id) {
    TokenNode *curr = head, *prev = NULL;
    while (curr != NULL) {
        Token t = curr->t;
        if (t.hit_line) {
            break;
        }

        if (t.type == TOKEN_HASH) {
            TokenNode* hash = curr;

            SourceLoc loc = encode_macro_loc(macro_id, t.content.data - subst_start);
            SourceRange r = { loc, { loc.raw + t.content.length } };
            if (curr->next->t.type != TOKEN_IDENTIFIER) {
                prev = curr, curr = curr->next;
                continue;
            }
            curr = curr->next;

            // stringize arg
            t = curr->t;
            ptrdiff_t arg_i = find_arg(args, t.content);
            if (arg_i < 0) {
                diag_err(&c->tokens, r, "cannot stringize unknown argument");
                return false;
            }

            // at best we might double the string length from backslashes
            unsigned char* stringized = gimme_the_shtuffs(c, (args->values[arg_i].content.length * 2) + 3);
            size_t len = 0;

            stringized[len++] = '"';
            String str = args->values[arg_i].content;
            for (size_t i = 0; i < str.length; i++) {
                if (str.data[i] == '\\' || str.data[i] == '"' || str.data[i] == '\'') {
                    stringized[len++] = '\\';
                }

                stringized[len++] = str.data[i];
            }
            stringized[len++] = '"';
            stringized[len] = 0;

            // FOO # identifier BAR
            // VVV
            // FOO NEWSTRING    BAR
            hash->next = curr->next;
            hash->t = (Token){
                .type = TOKEN_STRING_DOUBLE_QUOTE,
                .location = t.location,
                .content = { len, stringized },
            };
        } else if (t.type == TOKEN_DOUBLE_HASH) {
            String a = prev->t.content;

            TokenNode* savepoint = curr;
            String b = NEXT_TOKEN().content;
            ptrdiff_t b_i = find_arg(args, b);
            if (b_i >= 0) b = args->values[b_i].content;

            if (b.data[0] == '_' && string_equals_cstr(&b, "__VA_ARGS__")) {
                // if we just followed a comma and concat a __VA_ARGS__ which expands
                // to nothing, then we delete the comma
                size_t key_count = args->key_count, value_count = args->value_count;

                if (string_equals_cstr(&a, ",") && key_count == value_count) {
                    // delete comma
                    prev->t.type = 0;
                    prev->t.content = (String){ 0 };
                }

                prev->next = curr;
                continue;
            }

            // Literally join the data
            unsigned char* out = gimme_the_shtuffs(c, a.length + b.length + 16);
            memcpy(out, a.data, a.length);
            memcpy(out + a.length, b.data, b.length);
            memset(&out[a.length + b.length], 0, 16);

            // generate a new token and see what happens
            Lexer scratch = { .start = out, .current = out };
            Token joined = lexer_read(&scratch);

            // if they only form one token then process it
            {
                // FOO SOMETHING ## SOMETHING BAR
                // VVV
                // FOO JOINED                 BAR
                prev->t = joined;
                prev->next = curr->next;

                curr = prev, prev = get_previous_link(head, curr);
                curr->t.location = t.location;
            }

            // Copy over any of the extra tokens
            for (;;) {
                Token t = lexer_read(&scratch);
                if (t.type == 0) break;

                // dyn_array_put(out_tokens->tokens, t);
                assert(0 && "TODO");
            }

            prev = curr, curr = curr->next;
            continue;
        } else if (t.type == TOKEN_IDENTIFIER) {
            if (t.content.data[0] == '_' && string_equals_cstr(&t.content, "__VA_ARGS__")) {
                size_t key_count = args->key_count, value_count = args->value_count;
                if (key_count == value_count) {
                    curr->t.type = 0;
                    curr->t.content = (String){ 0 };
                    goto next;
                }

                TokenNode* old_next = curr->next;
                TokenNode* tail = NULL;

                TokenNode* old_curr = curr;
                for (size_t i = key_count; i < value_count; i++) {
                    // slap a comma between var args
                    if (i != key_count) {
                        Token t = {
                            .type = ',',
                            .location = t.location,
                            .content = string_cstr(","),
                        };

                        if (tail == NULL) {
                            curr->t = t, curr->next = NULL, tail = curr;
                        } else {
                            TokenNode* n = tls_push(sizeof(TokenNode));
                            n->next = NULL;
                            n->t = t;

                            tail->next = n, tail = n;
                        }
                    }

                    // __VA_ARGS__ is not technically a macro but because it needs to expand
                    uint32_t vaargs_macro = dyn_array_length(c->tokens.invokes);
                    dyn_array_put(c->tokens.invokes, (MacroInvoke){
                            .name      = t.content,
                            .parent    = macro_id,
                            .call_site = t.location,
                            .def_site  = args->values[i].loc,
                        });

                    String arg = args->values[i].content;
                    if (arg.length == 0) continue;

                    Lexer in = { 0, (uint8_t*) arg.data };
                    in.current = in.start;

                    TokenNode* before_arg = tail;
                    for (;;) {
                        Token t = lexer_read(&in);
                        if (t.type == 0 || t.hit_line) break;

                        t.location = encode_macro_loc(vaargs_macro, t.content.data - in.start);

                        if (tail == NULL) {
                            curr->t = t, curr->next = NULL, tail = curr;
                        } else {
                            TokenNode* n = tls_push(sizeof(TokenNode));
                            n->next = NULL;
                            n->t = t;

                            tail->next = n, tail = n;
                        }
                    }

                    TokenNode* first_token_in_arg = before_arg ? before_arg->next : curr;
                    expand(c, first_token_in_arg, vaargs_macro, NULL);

                    TokenNode* new_tail = first_token_in_arg;
                    while (new_tail->next) new_tail = new_tail->next;
                    tail = new_tail;
                }

                if (tail) {
                    tail->next = old_next, prev = tail;
                } else {
                    curr->t.type = 0;
                    curr->t.content = (String){ 0 };
                }
                curr = old_next;
                continue;
            } else {
                ptrdiff_t arg_i = find_arg(args, t.content);
                if (arg_i >= 0) {
                    String substitution = args->values[arg_i].content;
                    if (substitution.length == 0) {
                        curr->t.type = 0;
                        curr->t.content = (String){ 0 };
                        goto done;
                    }

                    TokenList list = into_list(c, (uint8_t*) substitution.data, macro_id);
                    if (list.head == NULL) {
                        curr->t.type = 0;
                        curr->t.content = (String){ 0 };
                        goto done;
                    }

                    TokenNode* old_next = curr->next;
                    expand(c, list.head, macro_id, NULL);

                    // replace *curr with newest expansion
                    TokenNode* new_tail = list.head;
                    while (new_tail->next) new_tail = new_tail->next;

                    if (new_tail == list.head) {
                        new_tail->next = old_next;
                        *curr = *list.head;
                    } else {
                        new_tail->next = old_next;
                        *curr = *list.head;
                        curr = new_tail;
                    }

                    done:;
                }
            }
        }

        next:
        prev = curr, curr = curr->next;
    }

    return true;
}
#undef NEXT_TOKEN

static void skip_nodes(TokenNode* head, TokenNode* tail) {
    // convert first one into zero
    head->t.type = 0;
    head->t.content = (String){ 0 };

    // skip the rest (since it's better than zeroing them all)
    head->next = tail;
}

static TokenNode* attach_to_list(TokenNode* head, TokenNode* end, TokenList list, const String* macro_name) {
    // attach to complete tokens list
    if (head == NULL) {
        head = list.head;
    }

    TokenNode* new_tail = list.head;
    while (new_tail->next) {
        if (new_tail->t.type == TOKEN_IDENTIFIER && string_equals(&new_tail->t.content, macro_name)) {
            new_tail->t.expanded = true;
        }

        new_tail = new_tail->next;
    }
    new_tail->next = end;
    *head = *list.head;

    return head;
}

// return.head is the start of the new output stream segment
// return.tail is the input stream's new position
static TokenList expand_ident(Cuik_CPP* restrict c, TokenArray* in, TokenNode* head, uint32_t parent_macro, TokenArray* rest) {
    // expansion:
    //   foo()                   #define foo bar(x, y, z)
    //   VVV
    //   substitution
    //   VVV
    //   bar(x, y, z)            #define bar(a,b,c) (a+b+c)
    //   VVV
    //   expansion
    //   VVV
    //   (x+y+z)
    Token t = in ? consume(in) : head->t;
    // t.location = macroify_loc(t.location, parent_macro);

    if (!in) {
        head->t.location = t.location;
    }

    // can a loc come up in yo crib?
    if (expand_builtin_idents(c, &t)) {
        head->t = t;
        return (TokenList){ .head = head, .tail = head->next };
    }

    TokenNode* end = head ? head->next : NULL;

    size_t def_i;
    if (!t.expanded && find_define(c, &def_i, t.content.data, t.content.length)) {
        String def = c->macros.vals[def_i].value;
        SourceLoc def_site = c->macros.vals[def_i].loc;

        // create macro invoke site
        uint32_t macro_id = dyn_array_length(c->tokens.invokes);
        dyn_array_put(c->tokens.invokes, (MacroInvoke){
                .name      = t.content,
                .parent    = parent_macro,
                .def_site  = { def_site, { def_site.raw + def.length } },
                .call_site = t.location,
            });

        const unsigned char* args = c->macros.keys[def_i].data + c->macros.keys[def_i].length;

        if (*args != '(') {
            // object-like macro:
            //   we dont need to parse this part
            //   if it expands into nothing
            if (def.length == 0) {
                if (head) {
                    head->t.type = 0;
                    head->t.content = (String){ 0 };
                }

                goto done;
            }

            // convert definition into token list
            TokenList list = into_list(c, (uint8_t*) def.data, macro_id);
            if (list.head == NULL) {
                if (head) {
                    head->t.type = 0;
                    head->t.content = (String){ 0 };
                }

                goto done;
            }

            // printf("OBJECT MACRO: %.*s    %.*s\n\n", (int)t.content.length, t.content.data, (int)def.length, def.data);
            // diag_note(&c->tokens, (SourceRange){ def_site, { def_site.raw + def.length } }, "%!S", t.content);

            #ifdef CPP_DBG
            int dbgmod = 0;
            for (size_t i = 0; i < 100; i++) {
                if (breakpoints[i] && t.content.length == strlen(breakpoints[i]) && memcmp(t.content.data, breakpoints[i], t.content.length) == 0) {
                    printf("Hit: %.*s\n", (int) t.content.length, t.content.data);
                    cppdbg__arglist = NULL;
                    dbgmod = cppdbg__break();
                    break;
                }
            }
            #endif /* CPP_DBG */

            // replace arguments and perform concats
            MacroArgs arglist = { 0 };
            size_t hidden = hide_macro(c, def_i);

            subst(c, list.head, list.start, &arglist, macro_id);

            #ifdef CPP_DBG
            if (dbgmod == 1) {
                printf("Substituion:\n");
                dump(list.head);
                dbgmod = cppdbg__break();
            }
            #endif /* CPP_DBG */

            // if the next token is a parenthesis, allow the expansion to see it
            if (end && end->t.type == '(') {
                TokenNode* curr = list.head;
                while (curr->next != NULL) curr = curr->next;

                curr->next = end;
                end = NULL;
            }

            // attach to complete tokens list
            expand(c, list.head, macro_id, in ? in : rest);
            unhide_macro(c, def_i, hidden);

            head = attach_to_list(head, end, list, &t.content);

            #ifdef CPP_DBG
            if (dbgmod == 1) {
                printf("Expansion:\n");
                dump(head);
                dbgmod = cppdbg__break();
            }
            #endif /* CPP_DBG */
        } else {
            TknType peeked = 0;
            if (in) {
                peeked = peek(in).type;
            } else if (head->next) {
                peeked = head->next->t.type;
            } else if (rest) {
                peeked = peek(rest).type;
                in = rest;
            }

            if (peeked == '(') {
                // expand function-like macro
                if (in) consume(in);

                ////////////////////////////////
                // Parse the arguments
                ////////////////////////////////
                MacroArgs arglist = { 0 };
                if (in) {
                    if (!parse_args(c, &arglist, in)) return (TokenList){ 0 };
                } else {
                    end = parse_args2(c, &arglist, head->next->next);
                }

                // We dont need to parse this part if it expands into nothing
                if (def.length == 0) {
                    if (head) skip_nodes(head, end);
                    goto done;
                }

                // convert definition into token list
                TokenList list = into_list(c, (uint8_t*) def.data, macro_id);
                if (list.head == NULL) {
                    if (head) skip_nodes(head, end);
                    goto done;
                }

                Lexer args_lexer = { 0, (unsigned char*) args, (unsigned char*) args };
                if (!parse_params(c, &arglist, &args_lexer)) goto done;

                /*printf("FUNCTION MACRO: %.*s    %.*s\n\n", (int)t.content.length, t.content.data, (int)def.length, def.data);
                for (size_t i = 0; i < arglist.key_count; i++) {
                    printf("  %.*s = %.*s\n", (int) arglist.keys[i].length, arglist.keys[i].data, (int) arglist.values[i].content.length, arglist.values[i].content.data);
                }
                printf("\n");*/

                #ifdef CPP_DBG
                int dbgmod = 0;
                for (size_t i = 0; i < 100; i++) {
                    if (breakpoints[i] && t.content.length == strlen(breakpoints[i]) && memcmp(t.content.data, breakpoints[i], t.content.length) == 0) {
                        printf("Hit: %.*s\n", (int) t.content.length, t.content.data);
                        cppdbg__arglist = &arglist;
                        dbgmod = cppdbg__break();
                        break;
                    }
                }
                #endif /* CPP_DBG */

                // replace arguments and perform concats
                subst(c, list.head, list.start, &arglist, macro_id);

                #ifdef CPP_DBG
                if (dbgmod == 1) {
                    printf("Substituion:\n");
                    dump(list.head);
                    dbgmod = cppdbg__break();
                }
                #endif /* CPP_DBG */

                size_t hidden = hide_macro(c, def_i);
                expand(c, list.head, macro_id, in ? in : rest);

                #ifdef CPP_DBG
                if (dbgmod == 1) {
                    printf("Expansion:\n");
                    dump(list.head);
                    dbgmod = cppdbg__break();
                }
                #endif /* CPP_DBG */

                unhide_macro(c, def_i, hidden);
                head = attach_to_list(head, end, list, &t.content);
            }
        }
    }

    if (head == NULL) {
        head = tls_push(sizeof(TokenNode));
        head->next = NULL;
        head->t = t;
    }

    done:
    return (TokenList){ .head = head, .tail = end };
}

static void expand(Cuik_CPP* restrict c, TokenNode* restrict head, uint32_t parent_macro, TokenArray* rest) {
    int depth = 0;

    TokenNode *curr = head;
    while (curr != NULL) {
        TokenNode* savepoint = curr;
        Token* restrict t = &curr->t;

        curr = curr->next;
        depth += (t->type == '(');

        if (t->type != TOKEN_IDENTIFIER) {
            if (parent_macro != 0) {
                // convert token location into macro relative
                if ((t->location.raw & SourceLoc_IsMacro) == 0) {
                    uint32_t pos = t->location.raw & ((1u << SourceLoc_FilePosBits) - 1);
                    t->location = encode_macro_loc(parent_macro, pos);
                } else {
                    t->location = encode_macro_loc(parent_macro, 0);
                }
            }
        } else {
            curr = expand_ident(c, NULL, savepoint, parent_macro, rest).tail;
        }

        if (t->type == ')') {
            if (depth == 0) break;
            depth--;
        }
    }
}
