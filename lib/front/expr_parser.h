////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
// This file is included into parser.h, it's parser of the parser module and is
// completely static
static Expr* parse_expr_l14(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr(TranslationUnit* tu, TokenStream* restrict s);

static Expr* parse_function_literal(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type) {
    // if the literal doesn't have a parameter list it will inherit from the `type`
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        // this might break on typeof because it's fucking weird...
        // TODO(NeGate): error messages... no attributes here
        Attribs attr = {0};
        type = parse_declspec(tu, s, &attr);
        type = parse_declarator(tu, s, type, true, true).type;

        expect(s, ')');
    }

    if (type->kind == KIND_PTR) {
        type = type->ptr_to;
    }

    if (type->kind != KIND_FUNC) {
        generic_error(s, "Function literal base type is not a function type");
    }

    // Because of the "not a lambda" nature of the function literals they count as
    // "scoped top level statements" which doesn't particularly change anything for
    // it but is interesting to think about internally
    Stmt* n = make_stmt(tu, s, STMT_FUNC_DECL, sizeof(struct StmtDecl));
    n->loc = 0;
    n->decl = (struct StmtDecl){
        .type = type,
        .name = NULL,
        .attrs = {
            .is_root = true, // to avoid it being vaporized
            .is_inline = true,
        },
        .initial_as_stmt = NULL,
    };
    arrput(tu->top_level_stmts, n);

    // Don't wanna share locals between literals and their parents, by the current
    // design of APIs with C callbacks this is aight because we already manually pass
    // user context, we just improve "locality" of these functions to their reference
    int old_start = local_symbol_start;
    local_symbol_start = local_symbol_count;
    {
        parse_function_definition(tu, s, n);
    }
    local_symbol_start = old_start;

    Expr* e = make_expr(tu);
    *e = (Expr){
        .op = EXPR_FUNCTION,
        .type = type,
        .func = {n}};
    return e;
}

// NOTE(NeGate): This function will push all nodes it makes onto the temporary
// storage where parse_initializer will move them into permanent storage.
static void parse_initializer_member(TranslationUnit* tu, TokenStream* restrict s) {
    // Parse designator, it's just chains of:
    // [const-expr]
    // .identifier
    InitNode* current = NULL;
    try_again: {
        if (tokens_get(s)->type == '[') {
            SourceLocIndex loc = tokens_get_location_index(s);
            tokens_next(s);

            int start = parse_const_expr(tu, s);
            if (start < 0) {
                // TODO(NeGate): Error messages
                generic_error(s, "Array initializer range is broken.");
            }

            // GNU-extension: array range initializer
            int count = 1;
            if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                tokens_next(s);

                count = parse_const_expr(tu, s) - start;
                if (count <= 1) {
                    // TODO(NeGate): Error messages
                    generic_error(s, "Array initializer range is broken.");
                }
            }
            expect(s, ']');

            current = (InitNode*)tls_push(sizeof(InitNode));
            *current = (InitNode){
                .mode = INIT_ARRAY,
                .loc = loc,
                .kids_count = 1,
                .start = start,
                .count = count,
            };
            goto try_again;
        }

        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            SourceLocIndex loc = tokens_get_location_index(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->end - t->start, t->start);
            tokens_next(s);

            current = (InitNode*)tls_push(sizeof(InitNode));
            *current = (InitNode){
                .mode = INIT_MEMBER,
                .loc = loc,
                .kids_count = 1,
                .member_name = name,
            };
            goto try_again;
        }
    }

    if (current != NULL) {
        expect(s, '=');
    } else {
        current = (InitNode*)tls_push(sizeof(InitNode));
        *current = (InitNode){ 0 };
    }

    // it can either be a normal expression
    // or a nested designated initializer
    if (tokens_get(s)->type == '{') {
        tokens_next(s);

        size_t local_count = 0;

        // don't expect one the first time
        bool expect_comma = false;
        while (tokens_get(s)->type != '}') {
            if (expect_comma) {
                expect(s, ',');

                // we allow for trailing commas like ballers do
                if (tokens_get(s)->type == '}') break;
            } else expect_comma = true;

            parse_initializer_member(tu, s);
            local_count += 1;
        }

        current->kids_count = local_count;
        expect(s, '}');
    } else {
        // parse without comma operator
        current->kids_count = 0;
        current->expr = parse_expr_l14(tu, s);
    }
}

static Expr* parse_initializer(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type) {
    SourceLocIndex loc = tokens_get_location_index(s);

    size_t count = 0;
    InitNode* start = tls_save();

    // don't expect one the first time
    bool expect_comma = false;
    while (tokens_get(s)->type != '}') {
        if (expect_comma) {
            expect(s, ',');

            if (tokens_get(s)->type == '}') break;
        } else
            expect_comma = true;

        parse_initializer_member(tu, s);
        count += 1;
    }

    size_t total_node_count = ((InitNode*)tls_save()) - start;
    expect(s, '}');

    InitNode* permanent_store = arena_alloc(&thread_arena, total_node_count * sizeof(InitNode), _Alignof(InitNode));
    memcpy(permanent_store, start, total_node_count * sizeof(InitNode));
    tls_restore(start);

    Expr* e = make_expr(tu);
    *e = (Expr){
        .op = EXPR_INITIALIZER,
        .start_loc = loc,
        .end_loc = tokens_get_last_location_index(s),
        .init = {type, count, permanent_store}};
    return e;
}

static Expr* parse_expr_l0(TranslationUnit* tu, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    if (t->type == '(') {
        SourceLocIndex start_loc = tokens_get_location_index(s);
        tokens_next(s);

        Expr* e = parse_expr(tu, s);

        expect_closing_paren(s, start_loc);

        e->has_parens = true;
        e->start_loc = start_loc;
        e->end_loc = tokens_get_last_location_index(s);
        return e;
    }

    Expr* e = make_expr(tu);
    SourceLocIndex start_loc = tokens_get_location_index(s);

    switch (t->type) {
        case TOKEN_IDENTIFIER: {
            const unsigned char* name = t->start;
            size_t length = t->end - t->start;

            if (length == sizeof("__va_arg") - 1 && memcmp(name, "__va_arg", length) == 0) {
                tokens_next(s);

                expect(s, '(');
                Expr* src = parse_expr_l14(tu, s);
                expect(s, ',');
                Cuik_Type* type = parse_typename(tu, s);
                expect(s, ')');

                tokens_prev(s);

                *e = (Expr){
                    .op = EXPR_VA_ARG,
                    .va_arg_ = {
                        type, src}};
                break;
            }

            Symbol* sym = find_local_symbol(s);
            if (sym != NULL) {
                if (sym->storage_class == STORAGE_PARAM) {
                    *e = (Expr){
                        .op = EXPR_PARAM,
                        .param_num = sym->param_num};
                } else if (sym->storage_class == STORAGE_ENUM) {
                    *e = (Expr){
                        .op = EXPR_ENUM,
                        .type = sym->type,
                        .enum_val = {&sym->type->enumerator.entries[sym->enum_value].value}};
                } else {
                    assert(sym->stmt != NULL);
                    *e = (Expr){
                        .op = EXPR_SYMBOL,
                        .symbol = sym->stmt};
                }
            } else {
                // We'll defer any global identifier resolution
                Token* t = tokens_get(s);
                Atom name = atoms_put(t->end - t->start, t->start);

                // check if it's builtin
                ptrdiff_t temp;
                ptrdiff_t builtin_search = shgeti_ts(tu->target.arch->builtin_func_map, name, temp);
                if (builtin_search >= 0) {
                    *e = (Expr){
                        .op = EXPR_BUILTIN_SYMBOL,
                        .builtin_sym = {name},
                    };
                } else {
                    Symbol* symbol_search = find_global_symbol((const char*)name);
                    if (symbol_search != NULL) {
                        if (symbol_search->storage_class == STORAGE_ENUM) {
                            *e = (Expr){
                                .op = EXPR_ENUM,
                                .type = symbol_search->type,
                                .enum_val = {&symbol_search->type->enumerator.entries[symbol_search->enum_value].value},
                            };
                        } else {
                            *e = (Expr){
                                .op = EXPR_SYMBOL,
                                .symbol = symbol_search->stmt,
                            };
                        }
                    } else {
                        REPORT(ERROR, start_loc, "could not resolve symbol: %s", name);

                        *e = (Expr){
                            .op = EXPR_UNKNOWN_SYMBOL,
                            .unknown_sym = name,
                        };
                    }
                }
            }

            // unknown symbols, symbols and enumerator entries participate in the
            // symbol chain, aka... don't append non-parameters :P
            if (e->op != EXPR_PARAM) {
                if (symbol_chain_current != NULL) {
                    symbol_chain_current->next_symbol_in_chain = e;
                    symbol_chain_current = e;
                } else {
                    symbol_chain_start = symbol_chain_current = e;
                }
            }
            break;
        }

        case TOKEN_FLOAT: {
            Token* t = tokens_get(s);
            bool is_float32 = t->end[-1] == 'f';
            double i = parse_float(t->end - t->start, (const char*)t->start);

            *e = (Expr){
                .op = is_float32 ? EXPR_FLOAT32 : EXPR_FLOAT64,
                .float_num = i};
            break;
        }

        case TOKEN_INTEGER: {
            Token* t = tokens_get(s);
            Cuik_IntSuffix suffix;
            uint64_t i = parse_int(t->end - t->start, (const char*)t->start, &suffix);

            *e = (Expr){
                .op = EXPR_INT,
                .int_num = {i, suffix}};
            break;
        }

        case TOKEN_STRING_SINGLE_QUOTE:
        case TOKEN_STRING_WIDE_SINGLE_QUOTE: {
            Token* t = tokens_get(s);

            int ch = 0;
            intptr_t distance = parse_char((t->end - t->start) - 2, (const char*)&t->start[1], &ch);
            if (distance < 0) abort();

            *e = (Expr){
                .op = t->type == TOKEN_STRING_SINGLE_QUOTE ? EXPR_CHAR : EXPR_WCHAR,
                .char_lit = ch,
            };
            break;
        }

        case TOKEN_STRING_DOUBLE_QUOTE:
        case TOKEN_STRING_WIDE_DOUBLE_QUOTE: {
            Token* t = tokens_get(s);
            bool is_wide = (tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE);

            *e = (Expr){
                .op = is_wide ? EXPR_WSTR : EXPR_STR,
                .str.start = t->start,
                .str.end = t->end};

            size_t saved_lexer_pos = s->current;
            tokens_next(s);

            if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                // Precompute length
                s->current = saved_lexer_pos;
                size_t total_len = (t->end - t->start);
                while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                    tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                    Token* segment = tokens_get(s);
                    total_len += (segment->end - segment->start) - 2;
                    tokens_next(s);
                }

                size_t curr = 0;
                char* buffer = arena_alloc(&thread_arena, total_len + 3, 4);

                buffer[curr++] = '\"';

                // Fill up the buffer
                s->current = saved_lexer_pos;
                while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                    tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                    Token* segment = tokens_get(s);

                    size_t len = segment->end - segment->start;
                    memcpy(&buffer[curr], segment->start + 1, len - 2);
                    curr += len - 2;

                    tokens_next(s);
                }

                buffer[curr++] = '\"';

                e->str.start = (const unsigned char*)buffer;
                e->str.end = (const unsigned char*)(buffer + curr);
            }

            tokens_prev(s);
            break;
        }

        case TOKEN_KW_Generic: {
            tokens_next(s);

            SourceLocIndex opening_loc = tokens_get_location_index(s);
            expect(s, '(');

            // controlling expression followed by a comma
            Expr* controlling_expr = parse_expr_l14(tu, s);

            *e = (Expr){
                .op = EXPR_GENERIC,
                .generic_ = {.controlling_expr = controlling_expr}};
            expect(s, ',');

            size_t entry_count = 0;
            C11GenericEntry* entries = tls_save();

            SourceLocIndex default_loc = 0;
            while (tokens_get(s)->type != ')') {
                if (tokens_get(s)->type == TOKEN_KW_default) {
                    if (default_loc) {
                        report_two_spots(REPORT_ERROR, s,
                            default_loc, tokens_get_location_index(s),
                            "multiple default cases on _Generic",
                            NULL, NULL, NULL);

                        // maybe do some error recovery
                        abort();
                    }

                    default_loc = tokens_get_location_index(s);
                    expect(s, ':');
                    Expr* expr = parse_expr_l14(tu, s);

                    // the default case is like a normal entry but without a type :p
                    tls_push(sizeof(C11GenericEntry));
                    entries[entry_count++] = (C11GenericEntry){
                        .key = NULL,
                        .value = expr};
                } else {
                    Cuik_Type* type = parse_typename(tu, s);
                    assert(type != 0 && "TODO: error recovery");

                    expect(s, ':');
                    Expr* expr = parse_expr_l14(tu, s);

                    tls_push(sizeof(C11GenericEntry));
                    entries[entry_count++] = (C11GenericEntry){
                        .key = type,
                        .value = expr};
                }

                // exit if it's not a comma
                if (tokens_get(s)->type != ',') break;
                tokens_next(s);
            }

            expect_closing_paren(s, opening_loc);

            // move it to a more permanent storage
            C11GenericEntry* dst = arena_alloc(&thread_arena, entry_count * sizeof(C11GenericEntry), _Alignof(C11GenericEntry));
            memcpy(dst, entries, entry_count * sizeof(C11GenericEntry));

            e->generic_.case_count = entry_count;
            e->generic_.cases = dst;

            tls_restore(entries);
            tokens_prev(s);
            break;
        }

        default:
        generic_error(s, "could not parse expression");
    }

    e->start_loc = start_loc;
    e->end_loc = tokens_get_location_index(s);
    tokens_next(s);
    return e;
}

static Expr* parse_expr_l1(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLocIndex start_loc = tokens_get_location_index(s);

    Expr* e = 0;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        if (is_typename(s)) {
            Cuik_Type* type = parse_typename(tu, s);
            expect(s, ')');

            if (tokens_get(s)->type == '{') {
                tokens_next(s);

                e = parse_initializer(tu, s, type);
            } else {
                Expr* base = parse_expr_l2(tu, s);
                e = make_expr(tu);

                *e = (Expr){
                    .op = EXPR_CAST,
                    .start_loc = start_loc,
                    .end_loc = start_loc,
                    .cast = {type, base}};
            }
        }

        if (!e) tokens_prev(s);
    }

    if (!e) e = parse_expr_l0(tu, s);

    // after any of the: [] () . ->
    // it'll restart and take a shot at matching another
    // piece of the expression.
    try_again : {
        if (tokens_get(s)->type == '[') {
            Expr* base = e;
            e = make_expr(tu);

            tokens_next(s);
            Expr* index = parse_expr(tu, s);
            expect(s, ']');

            SourceLocIndex end_loc = tokens_get_last_location_index(s);

            *e = (Expr){
                .op = EXPR_SUBSCRIPT,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .subscript = {base, index}};
            goto try_again;
        }

        // Pointer member access
        if (tokens_get(s)->type == TOKEN_ARROW) {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                generic_error(s, "Expected identifier after member access a.b");
            }

            SourceLocIndex end_loc = tokens_get_location_index(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->end - t->start, t->start);

            Expr* base = e;
            e = make_expr(tu);
            *e = (Expr){
                .op = EXPR_ARROW,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .dot_arrow = {.base = base, .name = name}};

            tokens_next(s);
            goto try_again;
        }

        // Member access
        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                generic_error(s, "Expected identifier after member access a.b");
            }

            SourceLocIndex end_loc = tokens_get_location_index(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->end - t->start, t->start);

            Expr* base = e;
            e = make_expr(tu);
            *e = (Expr){
                .op = EXPR_DOT,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .dot_arrow = {.base = base, .name = name}};

            tokens_next(s);
            goto try_again;
        }

        // Function call
        if (tokens_get(s)->type == '(') {
            tokens_next(s);

            Expr* target = e;
            e = make_expr(tu);

            size_t param_count = 0;
            void* params = tls_save();

            while (tokens_get(s)->type != ')') {
                if (param_count) {
                    expect(s, ',');
                }

                // NOTE(NeGate): This is a funny little work around because
                // i don't wanna parse the comma operator within the expression
                // i wanna parse it here so we just skip it.
                Expr* e = parse_expr_l14(tu, s);
                *((Expr**)tls_push(sizeof(Expr*))) = e;
                param_count++;
            }

            if (tokens_get(s)->type != ')') {
                generic_error(s, "Unclosed parameter list!");
            }
            tokens_next(s);

            SourceLocIndex end_loc = tokens_get_last_location_index(s);

            // Copy parameter refs into more permanent storage
            Expr** param_start = arena_alloc(&thread_arena, param_count * sizeof(Expr*), _Alignof(Expr*));
            memcpy(param_start, params, param_count * sizeof(Expr*));

            *e = (Expr){
                .op = EXPR_CALL,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .call = {target, param_count, param_start}};

            tls_restore(params);
            goto try_again;
        }

        // post fix, you can only put one and just after all the other operators
        // in this precendence.
        if (tokens_get(s)->type == TOKEN_INCREMENT || tokens_get(s)->type == TOKEN_DECREMENT) {
            bool is_inc = tokens_get(s)->type == TOKEN_INCREMENT;
            tokens_next(s);

            SourceLocIndex end_loc = tokens_get_last_location_index(s);

            Expr* src = e;
            e = make_expr(tu);
            *e = (Expr){
                .op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .unary_op.src = src};
        }

        return e;
    }
}

// deref* address& negate- sizeof _Alignof cast
static Expr* parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s) {
    // TODO(NeGate): Convert this code into a loop... please?
    // TODO(NeGate): just rewrite this in general...
    SourceLocIndex start_loc = tokens_get_location_index(s);

    if (tokens_get(s)->type == '*') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_DEREF,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == '!') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_LOGICAL_NOT,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == TOKEN_DOUBLE_EXCLAMATION) {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_CAST,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .cast = {&builtin_types[TYPE_BOOL], value}};
        return e;
    } else if (tokens_get(s)->type == '-') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_NEGATE,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == '~') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_NOT,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == '+') {
        tokens_next(s);
        return parse_expr_l2(tu, s);
    } else if (tokens_get(s)->type == TOKEN_INCREMENT) {
        tokens_next(s);
        Expr* value = parse_expr_l1(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_PRE_INC,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == TOKEN_DECREMENT) {
        tokens_next(s);
        Expr* value = parse_expr_l1(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_PRE_DEC,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else if (tokens_get(s)->type == TOKEN_KW_sizeof ||
        tokens_get(s)->type == TOKEN_KW_Alignof) {
        TknType operation_type = tokens_get(s)->type;
        tokens_next(s);

        bool has_paren = false;
        SourceLocIndex opening_loc = 0;
        if (tokens_get(s)->type == '(') {
            has_paren = true;

            opening_loc = tokens_get_location_index(s);
            tokens_next(s);
        }

        Expr* e = 0;
        if (is_typename(s)) {
            Cuik_Type* type = parse_typename(tu, s);

            if (has_paren) {
                expect_closing_paren(s, opening_loc);
            }

            // glorified backtracing on who own's the (
            // sizeof (int){ 0 } is a sizeof a compound list
            // not a sizeof(int) with a weird { 0 } laying around
            if (tokens_get(s)->type == '{') {
                tokens_next(s);

                e = parse_initializer(tu, s, type);
            } else {
                SourceLocIndex end_loc = tokens_get_last_location_index(s);

                e = make_expr(tu);
                *e = (Expr){
                    .op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF_T : EXPR_ALIGNOF_T,
                    .start_loc = start_loc,
                    .end_loc = end_loc,
                    .x_of_type = {type}};
            }
        } else {
            if (has_paren) tokens_prev(s);

            Expr* expr = parse_expr_l2(tu, s);
            SourceLocIndex end_loc = tokens_get_last_location_index(s);

            e = make_expr(tu);
            *e = (Expr){
                .op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF : EXPR_ALIGNOF,
                .start_loc = start_loc,
                .end_loc = end_loc,
                .x_of_expr = {expr}};

            /*if (has_paren) {
                expect_closing_paren(s, opening_loc);
            }*/
        }

        return e;
    } else if (tokens_get(s)->type == '&') {
        tokens_next(s);
        Expr* value = parse_expr_l1(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_ADDR,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .unary_op.src = value};
        return e;
    } else {
        return parse_expr_l1(tu, s);
    }
}

static int get_precendence(TknType ty) {
    switch (ty) {
        case TOKEN_TIMES:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
        return 100 - 3;

        case TOKEN_PLUS:
        case TOKEN_MINUS:
        return 100 - 4;

        case TOKEN_LEFT_SHIFT:
        case TOKEN_RIGHT_SHIFT:
        return 100 - 5;

        case TOKEN_GREATER_EQUAL:
        case TOKEN_LESS_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_LESS:
        return 100 - 6;

        case TOKEN_EQUALITY:
        case TOKEN_NOT_EQUAL:
        return 100 - 7;

        case TOKEN_AND:
        return 100 - 8;

        case TOKEN_XOR:
        return 100 - 9;

        case TOKEN_OR:
        return 100 - 10;

        case TOKEN_DOUBLE_AND:
        return 100 - 11;

        case TOKEN_DOUBLE_OR:
        return 100 - 12;

        // zero means it's not a binary operator
        default:
        return 0;
    }
}

static Expr* parse_expr_NEW(TranslationUnit* tu, TokenStream* restrict s, int min_prec) {
    // This precendence climber is always left associative
    SourceLocIndex start_loc = tokens_get_location_index(s);
    Expr* result = parse_expr_l2(tu, s);

    int prec;
    TknType binop;

    // It's kinda weird but you don't have to read it because you're a bitch anyways
    while (binop = tokens_get(s)->type,
        prec = get_precendence(binop),
        prec != 0 && prec >= min_prec) {
        tokens_next(s);

        Expr* e = make_expr(tu);
        Expr* rhs = parse_expr_NEW(tu, s, prec + 1);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        // Create binary operator
        *e = (Expr){
            .op = EXPR_NONE,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .bin_op = {result, rhs}};

        switch (binop) {
            case TOKEN_TIMES:
            e->op = EXPR_TIMES;
            break;
            case TOKEN_SLASH:
            e->op = EXPR_SLASH;
            break;
            case TOKEN_PERCENT:
            e->op = EXPR_PERCENT;
            break;
            case TOKEN_PLUS:
            e->op = EXPR_PLUS;
            break;
            case TOKEN_MINUS:
            e->op = EXPR_MINUS;
            break;
            case TOKEN_LEFT_SHIFT:
            e->op = EXPR_SHL;
            break;
            case TOKEN_RIGHT_SHIFT:
            e->op = EXPR_SHR;
            break;
            case TOKEN_GREATER_EQUAL:
            e->op = EXPR_CMPGE;
            break;
            case TOKEN_LESS_EQUAL:
            e->op = EXPR_CMPLE;
            break;
            case TOKEN_GREATER:
            e->op = EXPR_CMPGT;
            break;
            case TOKEN_LESS:
            e->op = EXPR_CMPLT;
            break;
            case TOKEN_EQUALITY:
            e->op = EXPR_CMPEQ;
            break;
            case TOKEN_NOT_EQUAL:
            e->op = EXPR_CMPNE;
            break;
            case TOKEN_AND:
            e->op = EXPR_AND;
            break;
            case TOKEN_XOR:
            e->op = EXPR_XOR;
            break;
            case TOKEN_OR:
            e->op = EXPR_OR;
            break;
            case TOKEN_DOUBLE_AND:
            e->op = EXPR_LOGICAL_AND;
            break;
            case TOKEN_DOUBLE_OR:
            e->op = EXPR_LOGICAL_OR;
            break;
            default:
            __builtin_unreachable();
        }

        result = e;
    }

    return result;
}

// ternary
static Expr* parse_expr_l13(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLocIndex start_loc = tokens_get_location_index(s);
    Expr* lhs = parse_expr_NEW(tu, s, 0);

    if (tokens_get(s)->type == '?') {
        tokens_next(s);

        Expr* mhs = parse_expr(tu, s);

        expect(s, ':');

        Expr* rhs = parse_expr_l13(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);
        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_TERNARY,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .ternary_op = {lhs, mhs, rhs}};

        return e;
    } else {
        return lhs;
    }
}

// = += -= *= /= %= <<= >>= &= ^= |=
//
// NOTE(NeGate): a=b=c is a=(b=c) not (a=b)=c
static Expr* parse_expr_l14(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLocIndex start_loc = tokens_get_location_index(s);
    Expr* lhs = parse_expr_l13(tu, s);

    if (tokens_get(s)->type == TOKEN_ASSIGN ||
        tokens_get(s)->type == TOKEN_PLUS_EQUAL ||
        tokens_get(s)->type == TOKEN_MINUS_EQUAL ||
        tokens_get(s)->type == TOKEN_TIMES_EQUAL ||
        tokens_get(s)->type == TOKEN_SLASH_EQUAL ||
        tokens_get(s)->type == TOKEN_PERCENT_EQUAL ||
        tokens_get(s)->type == TOKEN_AND_EQUAL ||
        tokens_get(s)->type == TOKEN_OR_EQUAL ||
        tokens_get(s)->type == TOKEN_XOR_EQUAL ||
        tokens_get(s)->type == TOKEN_LEFT_SHIFT_EQUAL ||
        tokens_get(s)->type == TOKEN_RIGHT_SHIFT_EQUAL) {
        Expr* e = make_expr(tu);

        ExprOp op;
        switch (tokens_get(s)->type) {
            case TOKEN_ASSIGN:
            op = EXPR_ASSIGN;
            break;
            case TOKEN_PLUS_EQUAL:
            op = EXPR_PLUS_ASSIGN;
            break;
            case TOKEN_MINUS_EQUAL:
            op = EXPR_MINUS_ASSIGN;
            break;
            case TOKEN_TIMES_EQUAL:
            op = EXPR_TIMES_ASSIGN;
            break;
            case TOKEN_SLASH_EQUAL:
            op = EXPR_SLASH_ASSIGN;
            break;
            case TOKEN_PERCENT_EQUAL:
            op = EXPR_PERCENT_ASSIGN;
            break;
            case TOKEN_AND_EQUAL:
            op = EXPR_AND_ASSIGN;
            break;
            case TOKEN_OR_EQUAL:
            op = EXPR_OR_ASSIGN;
            break;
            case TOKEN_XOR_EQUAL:
            op = EXPR_XOR_ASSIGN;
            break;
            case TOKEN_LEFT_SHIFT_EQUAL:
            op = EXPR_SHL_ASSIGN;
            break;
            case TOKEN_RIGHT_SHIFT_EQUAL:
            op = EXPR_SHR_ASSIGN;
            break;
            default:
            __builtin_unreachable();
        }
        tokens_next(s);
        Expr* rhs = parse_expr_l14(tu, s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        *e = (Expr){
            .op = op,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .bin_op = {lhs, rhs}};
        return e;
    } else {
        return lhs;
    }
}

static Expr* parse_expr_l15(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLocIndex start_loc = tokens_get_location_index(s);
    Expr* lhs = parse_expr_l14(tu, s);

    while (tokens_get(s)->type == TOKEN_COMMA) {
        Expr* e = make_expr(tu);
        ExprOp op = EXPR_COMMA;
        tokens_next(s);

        SourceLocIndex end_loc = tokens_get_last_location_index(s);

        Expr* rhs = parse_expr_l14(tu, s);
        *e = (Expr){
            .op = op,
            .start_loc = start_loc,
            .end_loc = end_loc,
            .bin_op = {lhs, rhs},
        };

        lhs = e;
    }

    return lhs;
}

static Expr* parse_expr(TranslationUnit* tu, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            generic_error(s, "pragma declaration expects string literal");
        }
        tokens_next(s);

        expect(s, ')');
    }

    return parse_expr_l15(tu, s);
}
