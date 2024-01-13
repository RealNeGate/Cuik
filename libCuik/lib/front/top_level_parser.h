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
            while (t.type == TOKEN_STRING_DOUBLE_QUOTE) {
                t = lexer_read(&pragma_lex);
            }
        }

        if (t.type != ')') {
            diag_err(s, tokens_get_range(s), "expected )");
        }

        if (string_equals_cstr(&comment_type, "linker")) {
            // TODO(NeGate): implement /linker, it just passes arguments to the linker
        } else if (string_equals_cstr(&comment_type, "lib")) {
            if (comment_string.length == 0) {
                diag_err(s, tokens_get_range(s), "pragma comment lib expected lib name");
            }

            Cuik_ImportRequest* import = TB_ARENA_ALLOC(parser->arena, Cuik_ImportRequest);
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
    Cuik_Attribute* attribute_list = parse_attributes(parser, s, NULL);
    SourceLoc loc = tokens_get_location(s);

    Attribs attr = { 0 };
    for (;;) {
        if (tokens_get(s)->type != TOKEN_KW_declspec &&
            tokens_get(s)->type != TOKEN_KW_Pragma) {
            break;
        }

        tokens_next(s);

        SourceLoc opening_loc = tokens_get_location(s);
        expect_char(s, '(');

        if (tokens_match(s, sizeof("noreturn")-1, "noreturn")) {
            attr.is_noret = true;
            tokens_next(s);

            expect_closing_paren(s, opening_loc);
        } else {
            // TODO(NeGate): Correctly parse declspec instead of
            // ignoring them.
            int depth = 1;
            while (depth) {
                if (tokens_get(s)->type == '(')
                    depth++;
                else if (tokens_get(s)->type == ')')
                    depth--;

                tokens_next(s);
            }
        }
    }

    // must be a declaration since it's a top level statement
    Cuik_QualType type = parse_declspec2(parser, s, &attr);
    if (CUIK_QUAL_TYPE_IS_NULL(type)) {
        diag_err(s, (SourceRange){ loc, tokens_get_last_location(s) }, "could not parse base type.");
        tokens_next(s);

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
    bool has_semicolon = true;
    while (!tokens_eof(s) && tokens_get(s)->type != ';') {
        size_t start_decl_token = s->list.current;
        Decl decl = parse_declarator2(parser, s, type, false);

        // Convert into statement
        Stmt* n = alloc_stmt(parser);
        n->op = STMT_GLOBAL_DECL;
        n->loc = decl.loc;
        n->decl = (struct StmtDecl){
            .name = decl.name,
            .type = decl.type,
            .attrs = attr,
            .local_ordinal = dyn_array_length(parser->top_level_stmts),
        };

        dyn_array_put(parser->top_level_stmts, n);

        Symbol *old_def = NULL, *sym = NULL;
        int ts = 0, te = 0;
        if (decl.name != NULL) {
            // Check for duplicates
            assert(!CUIK_QUAL_TYPE_IS_NULL(decl.type));
            old_def = sym = cuik_symtab_lookup(parser->symbols, decl.name);
            if (old_def == NULL) {
                if (attr.is_typedef) {
                    Cuik_Type* t = cuik_canonical_type(decl.type);
                    if (t->also_known_as != decl.name) {
                        // clone but preserve flags
                        decl.type = cuik_make_qual_type(
                            type_clone(&parser->types, t, decl.name),
                            cuik_get_quals(decl.type)
                        );
                    } else {
                        t->also_known_as = decl.name;
                    }
                }

                sym = CUIK_SYMTAB_PUT(parser->symbols, decl.name, Symbol);
                *sym = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .loc  = decl.loc,
                    .stmt = n
                };
            }

            if (strcmp(decl.name, "va_list") == 0) {
                parser->va_list = cuik_canonical_type(decl.type);
            }
        }

        bool has_body = false;
        if (!attr.is_typedef) {
            n->attr_list = parse_attributes(parser, s, n->attr_list);

            if (decl.name != NULL) {
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
                    has_body = true;
                } else if (tokens_get(s)->type == '{') {
                    if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                        diag_err(s, decl.loc, "cannot add function body to non-function declaration");
                    }

                    n->op = STMT_FUNC_DECL;
                    n->decl.attrs.is_root = attr.is_tls || !(attr.is_static || attr.is_inline);
                    expr_start = skip_brackets(s, decl.loc, false, &expr_end);
                    if (expr_start < 0) {
                        s->list.current = dyn_array_length(s->list.tokens) - 1;
                        return PARSE_WIT_ERRORS;
                    }

                    has_semicolon = false;
                    has_body = true;
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
                    // SourceRange r = { s->list.tokens[expr_start].location, get_token_range(&s->list.tokens[expr_end]).end };
                    // diag_note(s, r, "Initializer");
                }
            }
        }

        if (decl.name != NULL) {
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
                    diag_warn(s, decl.loc, "declaration previously defined.");
                    diag_note(s, old_def->loc, "see here");

                    st_class = old_def->storage_class;
                }

                Cuik_Type* placeholder_space = cuik_canonical_type(old_def->type);
                Cuik_Type* decl_type = cuik_canonical_type(decl.type);

                {
                    // mark potential conflict, we'll handle it once we have more context
                    TypeConflict* c = TB_ARENA_ALLOC(parser->arena, TypeConflict);
                    c->old_def = old_def;
                    c->new_def = sym;
                    // insert
                    c->next = parser->first_conflict;
                    parser->first_conflict = c;
                }

                /*if (placeholder_space->kind != KIND_PLACEHOLDER && !type_equal(decl_type, placeholder_space)) {
                    Cuik_Type *t1 = placeholder_space, *t2 = decl_type;
                    // only deref if both can
                    while (t1->kind == t2->kind && t1->kind == KIND_PTR) {
                        t1 = cuik_canonical_type(t1->ptr_to);
                        t2 = cuik_canonical_type(t2->ptr_to);
                    }

                    bool incompat = true;
                    if (t1->kind == t2->kind && (t1->kind == KIND_STRUCT || t2->kind == KIND_UNION)) {
                        // if the tag names match... it's all good
                        if (t1->record.name != NULL && t2->record.name && strcmp(t1->record.name, t2->record.name) == 0) {
                            incompat = false;

                            if (decl_type->size == 0 && placeholder_space->size != 0) {
                                decl.type = old_def->type;
                            }
                        }
                    }

                    if (incompat) {
                        diag_err(s, decl.loc, "declaration incompatible with previous declaration");
                        diag_note(s, old_def->loc, "see here");
                    }
                }*/

                if (attr.is_typedef) {
                    Cuik_Type* src = cuik_canonical_type(decl.type);
                    Cuik_Type* nominal = (src->kind == KIND_STRUCT || src->kind == KIND_UNION) ? src->record.nominal : NULL;

                    // replace placeholder with actual entry
                    *placeholder_space = *src;
                    placeholder_space->also_known_as = decl.name;

                    if (nominal != NULL) {
                        placeholder_space->record.nominal = nominal;
                    }
                }
            }

            if (sym->stmt != n && has_body) {
                sym->stmt = n;
            }

            sym->storage_class = st_class;
        }

        if (!has_semicolon) {
            // function body
            break;
        } else if (tokens_get(s)->type == ',') {
            tokens_next(s);
            continue;
        } else {
            break;
        }
    }

    if (has_semicolon) {
        expect_char(s, ';');
    }

    // we measure success here on forward progress
    if (starting_point == s->list.current) {
        tokens_next(s);
        return PARSE_WIT_ERRORS;
    }

    return PARSE_SUCCESS;
}

static void resolve_pending_exprs(Cuik_Parser* parser) {
    size_t pending_count = dyn_array_length(pending_exprs);
    for (size_t i = 0; i < pending_count; i++) {
        TokenStream mini_lex = parser->tokens;
        mini_lex.list.current = pending_exprs[i].start;

        if (pending_exprs[i].mode == PENDING_ALIGNAS) {
            Cuik_Type* type = pending_exprs[i].type;

            int align = 0;
            SourceLoc loc = tokens_get_location(&mini_lex);
            if (is_typename(parser, &mini_lex)) {
                Cuik_Type* new_align = cuik_canonical_type(parse_typename2(parser, &mini_lex));
                if (new_align == NULL || new_align->align) {
                    diag_err(&mini_lex, type->loc, "_Alignas cannot operate with incomplete");
                } else {
                    align = new_align->align;
                }
            } else {
                intmax_t new_align = parse_const_expr(parser, &mini_lex);
                if (new_align == 0) {
                    diag_err(&mini_lex, type->loc, "_Alignas cannot be applied with 0 alignment", new_align);
                } else if (new_align >= INT16_MAX) {
                    diag_err(&mini_lex, type->loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
                } else {
                    align = new_align;
                }
            }

            assert(align != 0);
            type->align = align;
        } else if (pending_exprs[i].mode == PENDING_BITWIDTH) {
            intmax_t result = parse_const_expr(parser, &mini_lex);
            *pending_exprs[i].dst = result;
        }
    }
}

static Cuik_Entrypoint check_for_entry(Cuik_Parser* parser) {
    Symbol* sym = cuik_symtab_lookup(parser->symbols, atoms_putc("WinMain"));
    if (sym != NULL && sym->storage_class == STORAGE_FUNC && sym->token_start != 0) {
        return CUIK_ENTRYPOINT_WINMAIN;
    }

    return CUIK_ENTRYPOINT_MAIN;
}

Cuik_ParseResult cuikparse_run(Cuik_Version version, TokenStream* restrict s, Cuik_Target* target, TB_Arena* restrict arena, bool only_code_index) {
    assert(s != NULL);

    tls_init();
    assert(target->pointer_byte_size == 8 && "other sized pointers aren't really supported yet");

    int r;
    Cuik_Parser parser = { 0 };
    parser.version = version;
    parser.tokens = *s;
    parser.target = target;
    parser.pointer_byte_size = target->pointer_byte_size;
    parser.static_assertions = dyn_array_create(int, 128);
    parser.types = init_type_table(target);
    parser.types.arena = parser.arena = arena;

    // just a shorthand so it's faster to grab
    parser.default_int = (Cuik_Type*) &target->signed_ints[CUIK_BUILTIN_INT];
    parser.is_in_global_scope = true;
    parser.top_level_stmts = dyn_array_create(Stmt*, 1024);
    parser.tokens.diag->parser = &parser;

    parser.symbols = cuik_symtab_create(NULL);
    parser.tags = cuik_symtab_create(&(Cuik_Type*){ NULL });

    if (parser.version == CUIK_VERSION_GLSL) {
        #define X(name) parser.glsl.name = atoms_putc(#name);
        #include "glsl_keywords.h"
    }

    if (pending_exprs) {
        dyn_array_clear(pending_exprs);
    } else {
        pending_exprs = dyn_array_create(PendingExpr, 1024);
    }

    // Phase 1: resolve all top level statements
    log_debug("%p: parse phase 1", &parser);
    CUIK_TIMED_BLOCK("phase 1") {
        while (!tokens_eof(s)) {
            // skip any top level "null" statements
            while (tokens_get(s)->type == ';') tokens_next(s);

            if (parse_pragma(&parser, s) != 0) continue;
            if (parse_static_assert(&parser, s) != 0) continue;

            // since top level cannot have expressions we can assume that it's a declaration
            // even if we don't detect a known typename (since it can be inferred to be a typedef),
            // this allows us to skim the top level and do out-of-order declarations or generate
            // a summary of the symbol table.
            if (parser.version != CUIK_VERSION_GLSL) {
                if (parse_decl(&parser, s) != 0) continue;
            } else {
                if (parse_decl_glsl(&parser, s) != 0) continue;
            }

            diag_err(s, tokens_get_range(s), "could not parse top level statement");
            tokens_next(s);
        }

        parser.is_in_global_scope = false;
    }
    THROW_IF_ERROR();

    // convert to translation unit
    parser.tu = cuik_malloc(sizeof(TranslationUnit));
    *parser.tu = (TranslationUnit){
        .filepath = s->filepath,
        .version = parser.version,
        .warnings = &DEFAULT_WARNINGS,
        .target = target,
        .tokens = *s,
        .arena = arena,
        .top_level_stmts = parser.top_level_stmts,
        .types = parser.types,
        .va_list = parser.va_list,
    };

    parser.tu->entrypoint_status = check_for_entry(&parser);

    if (only_code_index) {
        return (Cuik_ParseResult){ .tu = parser.tu, .imports = parser.import_libs };
    }

    // Phase 2: resolve top level types, layout records and
    // anything else so that we have a complete global symbol table
    log_debug("%p: parse phase 2", &parser);
    CUIK_TIMED_BLOCK("phase 2") {
        // check if any previous placeholders are still placeholders
        for (Cuik_Type* type = parser.first_placeholder; type != NULL; type = type->placeholder.next) {
            if (type->kind == KIND_PLACEHOLDER) {
                diag_err(s, type->loc, "you asked for a type %s but didn't declare it.", type->placeholder.name);
            }
        }

        if (cuikdg_error_count(s)) break;

        // parse all global declarations (we're walking the arena because it's
        // faster than walking the hash map, cache locality amirite)
        _Static_assert(sizeof(Symbol) == ((sizeof(Symbol) + 15ull) & ~15ull), "needs to be 16byte aligned to be walked in the arena easily");

        TB_ArenaChunk* base = parser.symbols->globals_arena->base;
        size_t chunk_size = parser.symbols->globals_arena->chunk_size;
        TB_ARENA_FOR(chunk, parser.symbols->globals_arena) {
            Symbol* syms = (Symbol*) &chunk->data[chunk == base ? sizeof(TB_Arena) : 0];
            Symbol* end_syms = (Symbol*) &chunk->data[chunk_size];
            if (chunk->next == NULL) {
                end_syms = (Symbol*) parser.symbols->globals_arena->watermark;
            }

            size_t count = end_syms - syms;
            for (size_t i = 0; i < count; i++) {
                Symbol* restrict sym = &syms[i];
                if (sym->token_start == 0 || (sym->storage_class != STORAGE_STATIC_VAR && sym->storage_class != STORAGE_GLOBAL)) {
                    continue;
                }

                // Spin up a mini parser here
                TokenStream mini_lex = *s;
                mini_lex.list.current = sym->token_start;

                // intitialize use list
                symbol_chain_start = NULL;

                if (tokens_get(&mini_lex)->type == '{') {
                    parse_initializer2(&parser, &mini_lex, CUIK_QUAL_TYPE_NULL);
                } else {
                    parse_assignment(&parser, &mini_lex);
                    if (mini_lex.list.current != sym->token_end) {
                        diag_err(&mini_lex, tokens_get_range(&mini_lex), "failed to parse expression");
                    }
                }

                sym->stmt->decl.initial = complete_expr(&parser);

                // finalize use list
                sym->stmt->decl.first_symbol = symbol_chain_start;
            }
        }

        if (cuikdg_error_count(s)) break;

        // do record layouts and shi
        resolve_pending_exprs(&parser);

        dyn_array_for(i, parser.types.tracked) {
            assert(parser.types.tracked[i]->align != -1);
            type_layout2(&parser, &parser.tokens, parser.types.tracked[i]);
        }

        quit_phase2:;
    }
    dyn_array_destroy(pending_exprs);
    dyn_array_destroy(parser.types.tracked);
    dyn_array_destroy(parser.static_assertions);
    THROW_IF_ERROR();

    log_debug("%p: parse phase 3", &parser);
    CUIK_TIMED_BLOCK("phase 3") {
        // we can't track types at this point, resolving that is over
        parser.tu->types.tracked = NULL;

        Cuik_Atom va_arg_fp = atoms_putc("__va_arg_fp");
        Cuik_Atom va_arg_gp = atoms_putc("__va_arg_gp");
        Cuik_Atom va_arg_mem = atoms_putc("__va_arg_mem");

        // TODO(NeGate): remember this code is stuff that can be made multithreaded, if we
        // care we can add that back in.
        TokenStream tokens = *s;
        CUIK_SYMTAB_FOR_GLOBALS(i, parser.symbols) {
            Symbol* sym = cuik_symtab_global_at(parser.symbols, i);

            // don't worry about normal globals, those have been taken care of...
            if (sym->token_start != 0 && (sym->storage_class == STORAGE_STATIC_FUNC || sym->storage_class == STORAGE_FUNC)) {
                // Spin up a mini parser here
                tokens.list.current = sym->token_start;

                // intitialize use list
                symbol_chain_start = NULL;

                Cuik_Atom name = sym->stmt->decl.name;
                if (name == va_arg_fp) parser.tu->sysv_abi.va_arg_fp = sym->stmt;
                else if (name == va_arg_gp) parser.tu->sysv_abi.va_arg_gp = sym->stmt;
                else if (name == va_arg_mem) parser.tu->sysv_abi.va_arg_mem = sym->stmt;

                // Some sanity checks in case a local symbol is acting funny.
                cuik_scope_open(parser.symbols), cuik_scope_open(parser.tags);
                parse_function(&parser, &tokens, sym->stmt);
                cuik_scope_close(parser.symbols), cuik_scope_close(parser.tags);

                // finalize use list
                sym->stmt->decl.first_symbol = symbol_chain_start;
            }
        }
    }
    cuik_symtab_destroy(parser.symbols);
    cuik_symtab_destroy(parser.tags);
    THROW_IF_ERROR();

    // output accumulated diagnostics, we used to pool them up in a nice
    // way... we'll get back to that eventually
    nl_map_for_str(i, parser.unresolved_symbols) {
        Diag_UnresolvedSymbol* loc = parser.unresolved_symbols[i].v;

        for (; loc != NULL; loc = loc->next) {
            diag_err(s, loc->loc, "unknown symbol: %s", loc->name);
        }

        /*
        cuikdg_tally_error(s);
        diag_header(s, DIAG_ERR, "could not resolve symbol: %s", loc->name);
        DiagWriter d = diag_writer(s);
        for (; loc != NULL; loc = loc->next) {
            if (!diag_writer_is_compatible(&d, loc->loc)) {
                // end line
                diag_writer_done(&d);
                d = diag_writer(s);
            }

            diag_writer_highlight(&d, loc->loc);
        }
        diag_writer_done(&d);*/
    }
    nl_map_free(parser.unresolved_symbols);
    THROW_IF_ERROR();

    parser.tokens.diag->parser = NULL;
    return (Cuik_ParseResult){ .tu = parser.tu, .imports = parser.import_libs };
}
#undef THROW_IF_ERROR
