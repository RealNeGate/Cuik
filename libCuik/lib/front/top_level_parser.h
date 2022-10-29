static Cuik_QualType parse_declspec2(Cuik_Parser* restrict parser, TokenStream* restrict s, Attribs* attr);

static Stmt* alloc_stmt(Cuik_Parser* parser) {
    Stmt* stmt = ARENA_ALLOC(&local_ast_arena, Stmt);
    memset(stmt, 0, sizeof(Stmt));
    return stmt;
}

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
    size_t starting_point = s->list.current;
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

    // [https://www.sigbus.info/n1570#6.7]
    // normal variable lists, we technically merge the function
    // declaration path into this which is wrong but not wrong enough.
    // it just means we can do `int foo, bar(void) {}`
    //
    // init-declarator-list:
    //   init-declarator (',' init-declarator )+ ';'
    //
    // init-declarator:
    //   declarator ('=' initializer)?
    while (!tokens_eof(s)) {
        size_t start_decl_token = s->list.current;
        Decl decl = parse_declarator2(parser, s, type, false);
        if (decl.name == NULL) {
            diag_warn(s, decl.loc, "Declaration has no name");
            tokens_next(s);
        }

        // Convert into statement
        Stmt* n = alloc_stmt(parser);
        n->op = STMT_GLOBAL_DECL;
        n->loc = decl.loc;
        n->decl = (struct StmtDecl){
            .name = decl.name,
            .type = decl.type,
            .attrs = attr,
        };
        dyn_array_put(parser->top_level_stmts, n);

        Symbol *old_def = NULL, *sym = NULL;
        if (decl.name != NULL) {
            // Check for duplicates
            old_def = sym = find_global_symbol(&parser->globals, decl.name);
            if (old_def == NULL) {
                ptrdiff_t sym_index = nl_strmap_puti_cstr(parser->globals.symbols, decl.name);
                sym = &parser->globals.symbols[sym_index];
            }

            StorageClass st_class;
            if (attr.is_typedef) {
                st_class = STORAGE_TYPEDEF;
            } else if (cuik_canonical_type(decl.type)->kind == KIND_FUNC) {
                st_class = (attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC);
            } else {
                st_class = (attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL);
            }

            if (old_def != NULL) {
                if (old_def->storage_class != st_class) {
                    diag_err(s, decl.loc, "declaration previously defined.");
                    diag_note(s, old_def->loc, "see here");
                }

                Cuik_Type* placeholder_space = cuik_canonical_type(old_def->type);
                if (placeholder_space->kind != KIND_PLACEHOLDER && !type_equal(cuik_canonical_type(decl.type), placeholder_space)) {
                    diag_err(s, decl.loc, "declaration incompatible with previous declaration");
                    diag_note(s, old_def->loc, "see here");
                }

                if (attr.is_typedef) {
                    // replace placeholder with actual entry
                    memcpy(placeholder_space, cuik_canonical_type(decl.type), sizeof(Cuik_Type));
                    placeholder_space->also_known_as = decl.name;
                }
            }

            *sym = (Symbol){
                .name = decl.name,
                .type = decl.type,
                .storage_class = st_class,
                .loc = decl.loc,
                .stmt = n,
            };
        }

        if (attr.is_typedef) {
            // typedef is just a special storage class
            diag_note(s, decl.loc, "Typedef: %s", decl.name);
        } else {
            n->attr_list = parse_attributes(s, n->attr_list);

            if (decl.name != NULL) {
                diag_note(s, decl.loc, "Declarator: %s", decl.name);

                // declaration endings
                ptrdiff_t expr_start, expr_end;
                if (tokens_get(s)->type == '=') {
                    // initializer:
                    //   assignment-expression
                    //   '{' initializer-list '}'
                    //
                    // since this is global scope we don't actually parse
                    // the expression, instead we can skim over it with
                    // brace matching.
                    tokens_next(s);

                    if (n->decl.attrs.is_inline) {
                        diag_err(s, decl.loc, "non-function declarations cannot be inline");
                    }
                    n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;

                    if (tokens_get(s)->type == '{') {
                        expr_start = skip_brackets(s, decl.loc, true, &expr_end);
                    } else {
                        expr_start = skip_expression_in_list(s, decl.loc, &expr_end);
                    }
                } else if (tokens_get(s)->type == '{') {
                    if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                        diag_err(s, decl.loc, "cannot add function body to non-function declaration");
                    }

                    n->decl.attrs.is_root = attr.is_tls || !(attr.is_static || attr.is_inline);
                    expr_start = skip_brackets(s, decl.loc, false, &expr_end);

                    if (expr_start >= 0) {
                        sym->token_start = expr_start;
                        sym->token_end = expr_end;

                        SourceRange r = { s->list.tokens[expr_start].location, get_token_range(&s->list.tokens[expr_end]).end };
                        diag_note(s, r, "Body");
                    }
                    break;
                } else {
                    expr_start = expr_end = -1;

                    if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                        if (n->decl.attrs.is_inline) {
                            diag_err(s, decl.loc, "non-function declaration cannot be inline");
                        }

                        n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;
                    }
                }

                if (expr_start >= 0) {
                    sym->token_start = expr_start;
                    sym->token_end = expr_end;

                    SourceRange r = { s->list.tokens[expr_start].location, get_token_range(&s->list.tokens[expr_end]).end };
                    diag_note(s, r, "Initializer");
                }
            }
        }

        if (tokens_get(s)->type == ';') {
            tokens_next(s);
            break;
        } else if (tokens_get(s)->type == ',') {
            tokens_next(s);
            continue;
        }
    }

    // we measure success here on forward progress
    if (starting_point == s->list.current) {
        tokens_next(s);
    }

    return PARSE_SUCCESS;
}

void cuikparse_make(Cuik_ParseVersion version, TokenStream* restrict s, const Cuik_Target* target) {
    assert(s != NULL);

    Cuik_Parser* restrict parser = calloc(1, sizeof(Cuik_Parser));
    parser->version = version;
    parser->tokens = s;
    parser->target = target;
    parser->static_assertions = dyn_array_create(int);
    parser->types = init_type_table();

    // just a shorthand so it's faster to grab
    parser->default_int = (Cuik_Type*) &parser->target->signed_ints[CUIK_BUILTIN_INT];
    parser->is_in_global_scope = true;
    parser->top_level_stmts = dyn_array_create(Stmt*);

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
        #endif
    }
}
