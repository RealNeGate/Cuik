static Cuik_QualType parse_declspec2(Cuik_Parser* restrict parser, TokenStream* restrict s, Attribs* attr);

static bool expect_char(TokenStream* restrict s, char ch) {
    if (tokens_get(s)->type != ch) {
        diag_err(s, tokens_get_range(s), "expected '%c', got '%!S'", ch, tokens_get(s)->content);
        return false;
    } else {
        tokens_next(s);
        return true;
    }
}

static ParseResult parse_pragma(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    if (tokens_get(s)->type != TOKEN_KW_Pragma) {
        return NO_PARSE;
    }

    tokens_next(s);
    if (!expect_char(s, '(')) return PARSE_WIT_ERRORS;

    if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
        diag_err(s, tokens_get_range(s), "pragma declaration expects string literal");
        return PARSE_WIT_ERRORS;
    }

    // Slap it into a proper C string so we don't accidentally
    // walk off the end and go random places
    size_t len = (tokens_get(s)->content.length) - 1;
    unsigned char* out = tls_push(len);
    {
        const char* in = (const char*) tokens_get(s)->content.data;

        size_t out_i = 0, in_i = 1;
        while (in_i < len) {
            int ch;
            ptrdiff_t distance = parse_char(len - in_i, &in[in_i], &ch);
            if (distance < 0) {
                diag_err(s, tokens_get_range(s), "failed to handle string literal");
                break;
            }

            out[out_i++] = ch;
            in_i += distance;
        }

        assert(out_i <= len);
        out[out_i++] = '\0';
    }

    Lexer pragma_lex = { 0, out, out };
    String pragma_name = lexer_read(&pragma_lex).content;
    if (string_equals_cstr(&pragma_name, "comment")) {
        // https://learn.microsoft.com/en-us/cpp/preprocessor/comment-c-cpp?view=msvc-170
        //   'comment' '(' comment-type [ ',' "comment-string" ] ')'
        // supported comment types:
        //   lib - links library against final output
        Token t = lexer_read(&pragma_lex);
        if (t.type != '(') {
            diag_err(s, tokens_get_range(s), "expected (");
        }

        String comment_string = { 0 };
        String comment_type = lexer_read(&pragma_lex).content;

        t = lexer_read(&pragma_lex);
        if (t.type == ',') {
            t = lexer_read(&pragma_lex);
            if (t.type != TOKEN_STRING_DOUBLE_QUOTE) {
                diag_err(s, tokens_get_range(s), "expected string literal");
            }

            comment_string = t.content;
            comment_string.length -= 2;
            comment_string.data += 1;

            t = lexer_read(&pragma_lex);
        }

        if (t.type != ')') {
            diag_err(s, tokens_get_range(s), "expected )");
        }

        if (string_equals_cstr(&comment_type, "lib")) {
            if (comment_string.length == 0) {
                diag_err(s, tokens_get_range(s), "pragma comment lib expected lib name");
            }

            Cuik_ImportRequest* import = ARENA_ALLOC(&thread_arena, Cuik_ImportRequest);
            import->next = parser->import_libs;
            import->lib_name = atoms_put(comment_string.length, comment_string.data);
            parser->import_libs = import;
        } else {
            diag_err(s, tokens_get_range(s), "unknown pragma comment option");
        }
    }
    tokens_next(s);
    tls_restore(out);

    if (!expect_char(s, ')')) return PARSE_WIT_ERRORS;
    return PARSE_SUCCESS;
}

static ParseResult parse_static_assert(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    if (tokens_get(s)->type != TOKEN_KW_Static_assert) {
        return NO_PARSE;
    }

    tokens_next(s);
    if (!expect_char(s, '(')) return PARSE_WIT_ERRORS;

    TknType terminator;
    size_t current = skip_expression_in_parens(s, &terminator);
    dyn_array_put(parser->static_assertions, current);

    tokens_prev(s);
    if (tokens_get(s)->type == ',') {
        tokens_next(s);

        Token* t = tokens_get(s);
        if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
            diag_err(s, get_token_range(t), "expected string literal");
        }
        tokens_next(s);
    } else {
        if (parser->version < CUIK_VERSION_C23) {
            DiagFixit fixit = { tokens_get_range(s), 0, ", \"\"" };
            diag_err(s, tokens_get_range(s), "#static assertion without string literal requires compiling with C23 or higher", fixit);
        }
    }

    if (!expect_char(s, ')')) return PARSE_WIT_ERRORS;
    return PARSE_SUCCESS;
}

// decls ::= decl-spec (declarator (',' declarator)+)?
static ParseResult parse_decl(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    Cuik_Attribute* attribute_list = parse_attributes(s, NULL);
    SourceLoc loc = tokens_get_location(s);

    // must be a declaration since it's a top level statement
    Attribs attr = { 0 };
    Cuik_QualType type = parse_declspec2(parser, s, &attr);
    if (CUIK_QUAL_TYPE_IS_NULL(type)) {
        diag_err(s, (SourceRange){ loc, tokens_get_last_location(s) }, "could not parse base type.");
        type = cuik_uncanonical_type(parser->default_int);
    }

    // diag_note(s, (SourceRange){ loc, tokens_get_last_location(s) }, "Declspec");

    // normal variable lists
    // declarator (',' declarator )+ ';'
    while (true) {
        size_t start_decl_token = s->list.current;
        Decl decl = parse_declarator2(parser, s, type, false);
        if (decl.name == NULL) {
            diag_err(s, decl.loc, "Declaration has no name");
            break;
        }

        // diag_note(s, decl.loc, "Declarator: %s", decl.name);
        __debugbreak();
    }

    return NO_PARSE;
}

void cuikparse_make(Cuik_ParseVersion version, TokenStream* restrict s, const Cuik_Target* target) {
    assert(s != NULL);

    Cuik_Parser* parser = calloc(1, sizeof(Cuik_Parser));
    parser->version = version;
    parser->tokens = s;
    parser->target = target;
    parser->static_assertions = dyn_array_create(int);
    parser->types = init_type_table();

    // just a shorthand so it's faster to grab
    parser->default_int = (Cuik_Type*) &parser->target->signed_ints[CUIK_BUILTIN_INT];

    while (tokens_get(s)->type) {
        // skip any top level "null" statements
        while (tokens_get(s)->type == ';') tokens_next(s);

        if (parse_pragma(parser, s) != 0) continue;
        if (parse_static_assert(parser, s) != 0) continue;

        // since top level cannot have expressions we can assume that it's a declaration
        // even if we don't detect a known typename (since it can be inferred to be a typedef),
        // this allows us to skim the top level and do out-of-order declarations or generate
        // a summary of the symbol table.
        if (parse_decl(parser, s) != 0) continue;

        diag_err(s, tokens_get_range(s), "could not parse top level statement");
        tokens_next(s);

        // TODO(NeGate): Correctly parse pragmas instead of ignoring them.
        #if 0
        Cuik_Attribute* attribute_list = parse_attributes(tu, s, NULL);
        SourceLoc loc = tokens_get_location(s);

        // must be a declaration since it's a top level statement
        Attribs attr = { 0 };
        Cuik_QualType type = parse_declspec(tu, s, &attr);
        if (CUIK_QUAL_TYPE_IS_NULL(type)) {
            REPORT(ERROR, loc, "Could not parse base type.");
            type = cuik_uncanonical_type(&cuik__builtin_int);
        }

        if (attr.is_typedef) {
            // declarator (',' declarator)+ ';'
            while (true) {
                size_t start_decl_token = s->list.current;
                Decl decl = parse_declarator(tu, s, type, false, false);

                // we wanna avoid getting stuck in infinite loops so if we dont
                // do anything in an iteration then we want to exit with an error
                bool possibly_bad_decl = (s->list.current == start_decl_token);

                if (decl.name != NULL) {
                    // make typedef
                    Stmt* n = make_stmt(tu, s);
                    n->op = STMT_DECL;
                    n->loc = decl.loc;
                    n->attr_list = parse_attributes(tu, s, attribute_list);
                    n->decl = (struct StmtDecl){
                        .name = decl.name,
                        .type = decl.type,
                        .attrs = attr,
                    };

                    // typedefs can't be roots ngl
                    n->decl.attrs.is_root = false;
                    dyn_array_put(tu->top_level_stmts, n);

                    // check for collision
                    Symbol* search = find_global_symbol(tu, decl.name);
                    if (search != NULL) {
                        if (search->storage_class != STORAGE_TYPEDEF) {
                            abort();
                            /* report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, search->loc,
                                "typedef overrides previous declaration.",
                                "old", "new", NULL);*/
                        }

                        Cuik_Type* placeholder_space = cuik_canonical_type(search->type);
                        if (placeholder_space->kind != KIND_PLACEHOLDER && !type_equal(cuik_canonical_type(decl.type), placeholder_space)) {
                            /*report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, search->loc,
                                "typedef overrides previous declaration.",
                                "old", "new", NULL);*/
                            abort();
                        }

                        // replace placeholder with actual entry
                        memcpy(placeholder_space, cuik_canonical_type(decl.type), sizeof(Cuik_Type));
                        placeholder_space->also_known_as = decl.name;
                    } else {
                        // add new entry
                        Symbol sym = {
                            .name = decl.name,
                            .type = decl.type,
                            .loc = decl.loc,
                            .storage_class = STORAGE_TYPEDEF,
                        };
                        nl_strmap_put_cstr(tu->globals.symbols, decl.name, sym);
                    }
                }

                if (tokens_get(s)->type == 0) {
                    REPORT(ERROR, loc, "declaration list ended with EOF instead of semicolon.");
                    break;
                } else if (tokens_get(s)->type == '=') {
                    REPORT(ERROR, loc, "why did you just try that goofy shit wit me. You cannot assign a typedef.");
                    // error recovery
                } else if (tokens_get(s)->type == ';') {
                    tokens_next(s);
                    break;
                } else if (tokens_get(s)->type == ',') {
                    tokens_next(s);
                    continue;
                } else if (possibly_bad_decl) {
                    REPORT(ERROR, loc, "Bad declaration");
                    tokens_next(s);
                    break;
                }
            }
        } else {
            if (tokens_get(s)->type == ';') {
                Stmt* n = make_stmt(tu, s);
                n->op = STMT_GLOBAL_DECL;
                n->loc = tokens_get_range(s);
                n->attr_list = attribute_list;
                n->decl = (struct StmtDecl){
                    .name = NULL,
                    .type = type,
                    .attrs = attr,
                };
                n->decl.attrs.is_root = true;
                dyn_array_put(tu->top_level_stmts, n);

                tokens_next(s);
                continue;
            }

            // normal variable lists
            // declarator (',' declarator )+ ';'
            while (true) {
                size_t start_decl_token = s->list.current;

                Decl decl = parse_declarator(tu, s, type, false, false);
                if (decl.name == NULL) {
                    diag_err(&tu->tokens, decl.loc, "Declaration has no name");
                    break;
                }

                // we wanna avoid getting stuck in infinite loops so if we dont
                // do anything in an iteration then we want to exit with an error
                bool possibly_bad_decl = (s->list.current == start_decl_token);

                Stmt* n = make_stmt(tu, s);
                n->op = STMT_GLOBAL_DECL;
                n->loc = decl.loc;
                n->attr_list = attribute_list;
                n->decl = (struct StmtDecl){
                    .name = decl.name,
                    .type = decl.type,
                    .attrs = attr,
                };
                dyn_array_put(tu->top_level_stmts, n);

                Symbol* sym = find_global_symbol(tu, decl.name);
                Symbol* old_definition = sym;
                if (sym == NULL) {
                    // slap that bad boy into the symbol table
                    ptrdiff_t sym_index = nl_strmap_puti_cstr(tu->globals.symbols, decl.name);
                    sym = &tu->globals.symbols[sym_index];
                }

                *sym = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .loc = decl.loc,
                    .stmt = n,
                };

                if (cuik_canonical_type(decl.type)->kind == KIND_FUNC) {
                    sym->storage_class = (attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC);
                } else {
                    sym->storage_class = (attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL);
                }

                n->attr_list = parse_attributes(tu, s, n->attr_list);

                bool requires_terminator = true;
                if (tokens_get(s)->type == '=') {
                    tokens_next(s);

                    if (old_definition && old_definition->current != 0) {
                        /* report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, old_definition->stmt->loc,
                            "Cannot redefine global declaration",
                            NULL, NULL, "previous definition was:");*/
                        abort();
                    }

                    if (n->decl.attrs.is_inline) {
                        REPORT(ERROR, decl.loc, "Declaration cannot be inline");
                    }

                    n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;

                    if (tokens_get(s)->type == '{') {
                        sym->current = s->list.current;
                        sym->terminator = '}';

                        tokens_next(s);

                        int depth = 1;
                        while (depth) {
                            Token* t = tokens_get(s);

                            if (t->type == '\0') {
                                REPORT(ERROR, decl.loc, "Declaration ended in EOF");

                                // restore the token stream
                                s->list.current = sym->current + 1;
                                goto skip_declaration;
                            } else if (t->type == '{') {
                                depth++;
                            } else if (t->type == ';' && depth == 1) {
                                REPORT(ERROR, tokens_get_location(s), "Spurious semicolon");
                                goto skip_declaration;
                            } else if (t->type == '}') {
                                if (depth == 0) {
                                    REPORT(ERROR, decl.loc, "Unbalanced brackets");
                                    goto skip_declaration;
                                }

                                depth--;
                            }

                            tokens_next(s);
                        }
                    } else {
                        // '=' EXPRESSION ','
                        // '=' EXPRESSION ';'
                        sym->current = s->list.current;
                        sym->terminator = ';';

                        int depth = 1;
                        while (depth) {
                            Token* t = tokens_get(s);

                            if (t->type == '\0') {
                                REPORT(ERROR, decl.loc, "Declaration was never closed");

                                // restore the token stream
                                s->list.current = sym->current + 1;
                                goto skip_declaration;
                            } else if (t->type == '(') {
                                depth++;
                            } else if (t->type == ')') {
                                depth--;

                                if (depth == 0) {
                                    REPORT(ERROR, decl.loc, "Unbalanced parenthesis");

                                    s->list.current = sym->current + 1;
                                    goto skip_declaration;
                                }
                            } else if (t->type == ';' || t->type == ',') {
                                if (depth > 1 && t->type == ';') {
                                    REPORT(ERROR, decl.loc, "Declaration's expression has a weird semicolon");
                                    goto skip_declaration;
                                } else if (depth == 1) {
                                    sym->terminator = t->type;
                                    depth--;
                                }
                            }

                            tokens_next(s);
                        }

                        // we ate the terminator but the code right below it
                        // does need to know what it is...
                        tokens_prev(s);
                    }
                } else if (tokens_get(s)->type == '{') {
                    // function bodies dont end in semicolon or comma, it just terminates
                    // the declaration list
                    requires_terminator = false;

                    if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                        diag_err(&tu->tokens, decl.loc, "Somehow parsing a function body... on a non-function type?");
                    }

                    if (old_definition && old_definition->current != 0) {
                        /* report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, old_definition->stmt->loc,
                            "Cannot redefine function declaration",
                            NULL, NULL, "previous definition was:");*/
                        abort();
                    }

                    if (sym->name[0] == 'm' && strcmp(sym->name, "main") == 0) {
                        tu->entrypoint_status = CUIK_ENTRYPOINT_MAIN;
                    } else if (sym->name[0] == 'W' && strcmp(sym->name, "WinMain") == 0) {
                        tu->entrypoint_status = CUIK_ENTRYPOINT_WINMAIN;
                    }

                    //n->decl.type = decl.type;
                    //n->decl.attrs = attr;
                    n->decl.attrs.is_root = attr.is_tls || !(attr.is_static || attr.is_inline);

                    sym->terminator = '}';
                    sym->current = s->list.current;
                    tokens_next(s);

                    // we postpone parsing the function bodies
                    // balance some brackets: '{' SOMETHING '}'
                    int depth = 1;
                    while (depth) {
                        Token* t = tokens_get(s);

                        if (t->type == '\0') {
                            SourceLoc l = tokens_get_last_location(s);
                            report_fix(REPORT_ERROR, tu->errors, s, l, "}", "Function body ended in EOF");

                            s->list.current = sym->current + 1;
                            goto skip_declaration;
                        } else if (t->type == '{') {
                            depth++;
                        } else if (t->type == '}') {
                            depth--;
                        }

                        tokens_next(s);
                    }
                } else {
                    if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                        if (n->decl.attrs.is_inline) {
                            REPORT(ERROR, decl.loc, "Declaration cannot be inline");
                        }

                        n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;
                    }
                }

                if (!requires_terminator) {
                    // function bodies just end the declaration list
                    break;
                }

                if (tokens_get(s)->type == 0) {
                    REPORT(ERROR, loc, "declaration list ended with EOF instead of semicolon.");
                    break;
                } else if (tokens_get(s)->type == ';') {
                    tokens_next(s);
                    break;
                } else if (tokens_get(s)->type == ',') {
                    tokens_next(s);
                    continue;
                } else if (possibly_bad_decl) {
                    REPORT(ERROR, loc, "Bad declaration");
                    break;
                }
            }

            skip_declaration:;
        }
        #endif
    }
}
