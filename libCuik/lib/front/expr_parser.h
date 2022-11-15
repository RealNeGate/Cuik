////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
// This file is included into parser.h, it's parser of the parser module and is
// completely static
static Expr* parse_expr_assign(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr(TranslationUnit* tu, TokenStream* restrict s);

static InitNode* make_init_node(TokenStream* restrict s, int mode) {
    InitNode* n = ARENA_ALLOC(&local_ast_arena, InitNode);
    *n = (InitNode){ .mode = mode };
    return n;
}

// new tail
static InitNode* append_to_init_list(TokenStream* restrict s, InitNode* parent, InitNode* tail, InitNode* elem) {
    if (tail != NULL) {
        tail->next = elem;
    } else {
        parent->kid = elem;
    }

    parent->kids_count += 1;
    return elem;
}

// initalizer-designator:
//   [const-expr]
//   .identifier
static InitNode* parse_initializer_member(TranslationUnit* tu, TokenStream* restrict s) {
    InitNode* current = make_init_node(s, INIT_NONE);
    InitNode* head = current;

    int depth = 0;
    for (;; depth++) {
        if (tokens_get(s)->type == '[')  {
            SourceLoc loc = tokens_get_location(s);
            tokens_next(s);

            intmax_t start = parse_const_expr(tu, s);
            if (start < 0) {
                // TODO(NeGate): Error messages
                diag_err(s, tokens_get_range(s), "array initializer range is broken.");
            }

            // GNU-extension: array range initializer
            intmax_t count = 1;
            if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                tokens_next(s);

                count = parse_const_expr(tu, s) - start;
                if (count <= 1) {
                    // TODO(NeGate): Error messages
                    diag_err(s, tokens_get_range(s), "array initializer range is broken.");
                }
            }
            expect_char(s, ']');

            current->mode = INIT_ARRAY, current->start = start, current->count = count;
            current->loc = (SourceRange){ loc, tokens_get_last_location(s) };
            // create new kid
            current->kid = make_init_node(s, INIT_NONE), current = current->kid;
            continue;
        }

        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            SourceLoc loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);
            tokens_next(s);

            current->mode = INIT_MEMBER, current->member_name = name;
            current->loc = (SourceRange){ loc, tokens_get_last_location(s) };
            // create new kid
            current->kid = make_init_node(s, INIT_NONE), current = current->kid;
            continue;
        }

        break;
    }

    if (depth > 0) {
        expect_char(s, '=');
    }

    // it can either be a normal expression
    // or a nested designated initializer
    SourceLoc loc = tokens_get_location(s);
    if (tokens_get(s)->type == '{') {
        tokens_next(s);

        // don't expect one the first time
        bool expect_comma = false;
        InitNode* tail = current->kid;
        while (tokens_get(s)->type != '}') {
            if (expect_comma) {
                expect_char(s, ',');

                // we allow for trailing commas like ballers do
                if (tokens_get(s)->type == '}') break;
            } else expect_comma = true;

            // attach to our linked list
            tail = append_to_init_list(s, current, tail, parse_initializer_member(tu, s));
        }
        expect_char(s, '}');
    } else {
        // parse without comma operator
        current->expr = parse_expr_assign(tu, s);
    }
    current->loc = (SourceRange){ loc, tokens_get_last_location(s) };

    return head;
}

static Expr* parse_initializer(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type) {
    SourceLoc loc = tokens_get_location(s);

    InitNode *root = ARENA_ALLOC(&local_ast_arena, InitNode), *tail = NULL;
    *root = (InitNode){ 0 };

    // don't expect one the first time
    bool expect_comma = false;
    while (tokens_get(s)->type != '}') {
        tail = append_to_init_list(s, root, tail, parse_initializer_member(tu, s));

        if (tokens_get(s)->type == ',') {
            tokens_next(s);
            continue;
        } else {
            break;
        }
    }
    expect_char(s, '}');

    Expr* e = make_expr(tu);
    *e = (Expr){
        .op = EXPR_INITIALIZER,
        .loc = { loc, tokens_get_last_location(s) },
        .init = { type, root },
    };
    return e;
}

static Expr* parse_expr_l0(TranslationUnit* tu, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    if (t->type == '(') {
        SourceLoc start_loc = tokens_get_location(s);
        tokens_next(s);

        Expr* e = parse_expr(tu, s);

        expect_closing_paren(s, start_loc);

        e->has_parens = true;
        e->loc.start = start_loc;
        e->loc.end = tokens_get_last_location(s);
        return e;
    }

    Expr* e = make_expr(tu);
    SourceLoc start_loc = tokens_get_location(s);

    switch (t->type) {
        case TOKEN_IDENTIFIER: {
            if (memeq(t->content.data, t->content.length, "__va_arg", sizeof("__va_arg") - 1)) {
                tokens_next(s);

                expect_char(s, '(');
                Expr* src = parse_expr_assign(tu, s);
                expect_char(s, ',');
                Cuik_QualType type = parse_typename(tu, s);
                expect_char(s, ')');

                tokens_prev(s);

                *e = (Expr){
                    .op = EXPR_VA_ARG,
                    .va_arg_ = { src, type },
                };
                break;
            }

            Symbol* sym = find_local_symbol(s);
            if (sym != NULL) {
                if (sym->storage_class == STORAGE_PARAM) {
                    *e = (Expr){
                        .op = EXPR_PARAM,
                        .param_num = sym->param_num
                    };
                } else if (sym->storage_class == STORAGE_ENUM) {
                    *e = (Expr){
                        .op = EXPR_ENUM,
                        .type = sym->type,
                        .enum_val = { &cuik_canonical_type(sym->type)->enumerator.entries[sym->enum_value] },
                    };
                } else {
                    assert(sym->stmt != NULL);
                    *e = (Expr){
                        .op = EXPR_SYMBOL,
                        .symbol = sym->stmt,
                    };
                }
            } else {
                // We'll defer any global identifier resolution
                Token* t = tokens_get(s);
                Atom name = atoms_put(t->content.length, t->content.data);

                // check if it's builtin
                ptrdiff_t builtin_search = nl_strmap_get_cstr(tu->target->builtin_func_map, name);
                if (builtin_search >= 0) {
                    *e = (Expr){
                        .op = EXPR_BUILTIN_SYMBOL,
                        .builtin_sym = { name },
                    };
                } else {
                    Symbol* symbol_search = find_global_symbol(&tu->globals, (const char*)name);
                    if (symbol_search != NULL) {
                        if (symbol_search->storage_class == STORAGE_ENUM) {
                            *e = (Expr){
                                .op = EXPR_ENUM,
                                .type = symbol_search->type,
                                .enum_val = { &cuik_canonical_type(symbol_search->type)->enumerator.entries[symbol_search->enum_value] },
                            };
                        } else {
                            *e = (Expr){
                                .op = EXPR_SYMBOL,
                                .symbol = symbol_search->stmt,
                            };
                        }
                    } else {
                        // diag_unresolved_symbol(tu, name, start_loc);
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
            if (e->op != EXPR_PARAM && e->op != EXPR_ENUM) {
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
            bool is_float32 = t->content.data[t->content.length - 1] == 'f';

            char* end;
            double f = strtod((const char*) t->content.data, &end);
            if (end != (const char*) &t->content.data[t->content.length]) {
                if (*end != 'l' && *end != 'L' && *end != 'f' && *end != 'd' && *end != 'F' && *end != 'D') {
                    REPORT(ERROR, t->location, "invalid float literal");
                }
            }

            *e = (Expr){
                .op = is_float32 ? EXPR_FLOAT32 : EXPR_FLOAT64,
                .float_num = f,
            };
            break;
        }

        case TOKEN_INTEGER: {
            Token* t = tokens_get(s);
            Cuik_IntSuffix suffix;
            uint64_t i = parse_int(t->content.length, (const char*) t->content.data, &suffix);

            *e = (Expr){
                .op = EXPR_INT,
                .int_num = {i, suffix},
            };
            break;
        }

        case TOKEN_STRING_SINGLE_QUOTE:
        case TOKEN_STRING_WIDE_SINGLE_QUOTE: {
            Token* t = tokens_get(s);

            int ch = 0;
            ptrdiff_t distance = parse_char(t->content.length - 2, (const char*) &t->content.data[1], &ch);
            if (distance < 0) {
                REPORT(ERROR, t->location, "invalid character literal");
            }

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
                .str.start = t->content.data,
                .str.end = &t->content.data[t->content.length],
            };

            size_t saved_lexer_pos = s->list.current;
            tokens_next(s);

            if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                // Precompute length
                s->list.current = saved_lexer_pos;
                size_t total_len = t->content.length;
                while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                    tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                    Token* segment = tokens_get(s);
                    total_len += segment->content.length - 2;
                    tokens_next(s);
                }

                size_t curr = 0;
                char* buffer = arena_alloc(&thread_arena, total_len + 3, 4);

                buffer[curr++] = '\"';

                // Fill up the buffer
                s->list.current = saved_lexer_pos;
                while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE ||
                    tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
                    Token* segment = tokens_get(s);

                    memcpy(&buffer[curr], segment->content.data + 1, segment->content.length - 2);
                    curr += segment->content.length - 2;

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

            SourceLoc opening_loc = tokens_get_location(s);
            expect(tu, s, '(');

            // controlling expression followed by a comma
            Expr* controlling_expr = parse_expr_assign(tu, s);

            *e = (Expr){
                .op = EXPR_GENERIC,
                .generic_ = {.controlling_expr = controlling_expr},
            };
            expect(tu, s, ',');

            size_t entry_count = 0;
            C11GenericEntry* entries = tls_save();

            SourceLoc default_loc = { 0 };
            while (tokens_get(s)->type != ')') {
                if (tokens_get(s)->type == TOKEN_KW_default) {
                    if (default_loc.raw != 0) {
                        report_two_spots(REPORT_ERROR, s,
                            default_loc, tokens_get_location(s),
                            "multiple default cases on _Generic",
                            NULL, NULL, NULL);

                        // maybe do some error recovery
                        abort();
                    }

                    default_loc = tokens_get_location(s);
                    expect(tu, s, ':');
                    Expr* expr = parse_expr_assign(tu, s);

                    // the default case is like a normal entry but without a type :p
                    tls_push(sizeof(C11GenericEntry));
                    entries[entry_count++] = (C11GenericEntry){
                        .key = CUIK_QUAL_TYPE_NULL,
                        .value = expr,
                    };
                } else {
                    Cuik_QualType type = parse_typename(tu, s);
                    assert(!CUIK_QUAL_TYPE_IS_NULL(type) && "TODO: error recovery");

                    expect(tu, s, ':');
                    Expr* expr = parse_expr_assign(tu, s);

                    tls_push(sizeof(C11GenericEntry));
                    entries[entry_count++] = (C11GenericEntry){
                        .key = type,
                        .value = expr,
                    };
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
        diag_err(s, tokens_get_range(s), "could not parse expression");
        break;
    }
    tokens_next(s);

    e->loc.start = start_loc;
    e->loc.end = tokens_get_last_location(s);
    return e;
}

static Expr* parse_expr_l1(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);

    Expr* e = NULL;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        if (is_typename(&tu->globals, s)) {
            Cuik_QualType type = parse_typename(tu, s);
            expect_closing_paren(s, start_loc);

            if (tokens_get(s)->type == '{') {
                tokens_next(s);

                e = parse_initializer(tu, s, type);
            } else {
                Expr* base = parse_expr_l2(tu, s);
                e = make_expr(tu);

                *e = (Expr){
                    .op = EXPR_CAST,
                    .loc = { start_loc, start_loc },
                    .cast = { base, type },
                };
            }
        } else {
            tokens_prev(s);
        }
    }

    start_loc = tokens_get_location(s);
    if (!e) e = parse_expr_l0(tu, s);

    // after any of the: [] () . ->
    // it'll restart and take a shot at matching another
    // piece of the expression.
    try_again: {
        if (tokens_get(s)->type == '[') {
            Expr* base = e;
            e = make_expr(tu);

            tokens_next(s);
            Expr* index = parse_expr(tu, s);
            expect(tu, s, ']');

            SourceLoc end_loc = tokens_get_last_location(s);

            *e = (Expr){
                .op = EXPR_SUBSCRIPT,
                .loc = { start_loc, end_loc },
                .subscript = {base, index},
            };
            goto try_again;
        }

        // Pointer member access
        if (tokens_get(s)->type == TOKEN_ARROW) {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                generic_error(tu, s, "Expected identifier after member access a.b");
            }

            SourceLoc end_loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Expr* base = e;
            e = make_expr(tu);
            *e = (Expr){
                .op = EXPR_ARROW,
                .loc = { start_loc, end_loc },
                .dot_arrow = {.base = base, .name = name},
            };

            tokens_next(s);
            goto try_again;
        }

        // Member access
        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                generic_error(tu, s, "Expected identifier after member access a.b");
            }

            SourceLoc end_loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Expr* base = e;
            e = make_expr(tu);
            *e = (Expr){
                .op = EXPR_DOT,
                .loc = { start_loc, end_loc },
                .dot_arrow = {.base = base, .name = name},
            };

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
                    expect(tu, s, ',');
                }

                // NOTE(NeGate): This is a funny little work around because
                // i don't wanna parse the comma operator within the expression
                // i wanna parse it here so we just skip it.
                Expr* e = parse_expr_assign(tu, s);
                *((Expr**)tls_push(sizeof(Expr*))) = e;
                param_count++;
            }

            if (tokens_get(s)->type != ')') {
                generic_error(tu, s, "Unclosed parameter list!");
            }
            tokens_next(s);

            SourceLoc end_loc = tokens_get_last_location(s);

            // Copy parameter refs into more permanent storage
            Expr** param_start = arena_alloc(&thread_arena, param_count * sizeof(Expr*), _Alignof(Expr*));
            memcpy(param_start, params, param_count * sizeof(Expr*));

            *e = (Expr){
                .op = EXPR_CALL,
                .loc = { start_loc, end_loc },
                .call = { target, param_count, param_start },
            };
            // diag_note(s, e->loc, "EXPR");

            tls_restore(params);
            goto try_again;
        }

        return e;
    }
}

// deref* address& negate- sizeof _Alignof cast
static Expr* parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s) {
    // TODO(NeGate): Convert this code into a loop... please?
    // TODO(NeGate): just rewrite this in general...
    SourceLoc start_loc = tokens_get_location(s);

    if (tokens_get(s)->type == '*') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_DEREF,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == '!') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_LOGICAL_NOT,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == TOKEN_DOUBLE_EXCLAMATION) {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_CAST,
            .loc = { start_loc, end_loc },
            .cast = { value, cuik_make_qual_type(&builtin_types[TYPE_BOOL], 0) },
        };
        return e;
    } else if (tokens_get(s)->type == '-') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_NEGATE,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == '~') {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_NOT,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == '+') {
        tokens_next(s);
        return parse_expr_l2(tu, s);
    } else if (tokens_get(s)->type == TOKEN_INCREMENT) {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_PRE_INC,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == TOKEN_DECREMENT) {
        tokens_next(s);
        Expr* value = parse_expr_l2(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_PRE_DEC,
            .loc = { start_loc, end_loc },
            .unary_op.src = value
        };
        return e;
    } else if (tokens_get(s)->type == TOKEN_KW_sizeof ||
        tokens_get(s)->type == TOKEN_KW_Alignof) {
        TknType operation_type = tokens_get(s)->type;
        tokens_next(s);

        bool has_paren = false;
        SourceLoc opening_loc = { 0 };
        if (tokens_get(s)->type == '(') {
            has_paren = true;

            opening_loc = tokens_get_location(s);
            tokens_next(s);
        }

        Expr* e = 0;
        if (is_typename(&tu->globals, s)) {
            Cuik_QualType type = parse_typename(tu, s);

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
                SourceLoc end_loc = tokens_get_last_location(s);

                e = make_expr(tu);
                *e = (Expr){
                    .op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF_T : EXPR_ALIGNOF_T,
                    .loc = { start_loc, end_loc },
                    .x_of_type = { type },
                };
            }
        } else {
            if (has_paren) tokens_prev(s);

            Expr* expr = parse_expr_l2(tu, s);
            SourceLoc end_loc = tokens_get_last_location(s);

            e = make_expr(tu);
            *e = (Expr){
                .op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF : EXPR_ALIGNOF,
                .loc = { start_loc, end_loc },
                .x_of_expr = {expr},
            };
        }

        return e;
    } else if (tokens_get(s)->type == '&') {
        tokens_next(s);
        Expr* value = parse_expr_l1(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);
        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_ADDR,
            .loc = { start_loc, end_loc },
            .unary_op.src = value,
        };
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
    SourceLoc start_loc = tokens_get_location(s);
    Expr* result = parse_expr_l2(tu, s);

    int prec;
    TknType binop;

    // It's kinda weird but you don't have to read it because you're a bitch anyways
    while (binop = tokens_get(s)->type, prec = get_precendence(binop), prec != 0 && prec >= min_prec) {
        tokens_next(s);

        Expr* e = make_expr(tu);
        Expr* rhs = parse_expr_NEW(tu, s, prec + 1);

        SourceLoc end_loc = tokens_get_last_location(s);

        // Create binary operator
        *e = (Expr){
            .op = EXPR_NONE,
            .loc = { start_loc, end_loc },
            .bin_op = {result, rhs},
        };

        switch (binop) {
            case TOKEN_TIMES:         e->op = EXPR_TIMES;       break;
            case TOKEN_SLASH:         e->op = EXPR_SLASH;       break;
            case TOKEN_PERCENT:       e->op = EXPR_PERCENT;     break;
            case TOKEN_PLUS:          e->op = EXPR_PLUS;        break;
            case TOKEN_MINUS:         e->op = EXPR_MINUS;       break;
            case TOKEN_LEFT_SHIFT:    e->op = EXPR_SHL;         break;
            case TOKEN_RIGHT_SHIFT:   e->op = EXPR_SHR;         break;
            case TOKEN_GREATER_EQUAL: e->op = EXPR_CMPGE;       break;
            case TOKEN_LESS_EQUAL:    e->op = EXPR_CMPLE;       break;
            case TOKEN_GREATER:       e->op = EXPR_CMPGT;       break;
            case TOKEN_LESS:          e->op = EXPR_CMPLT;       break;
            case TOKEN_EQUALITY:      e->op = EXPR_CMPEQ;       break;
            case TOKEN_NOT_EQUAL:     e->op = EXPR_CMPNE;       break;
            case TOKEN_AND:           e->op = EXPR_AND;         break;
            case TOKEN_XOR:           e->op = EXPR_XOR;         break;
            case TOKEN_OR:            e->op = EXPR_OR;          break;
            case TOKEN_DOUBLE_AND:    e->op = EXPR_LOGICAL_AND; break;
            case TOKEN_DOUBLE_OR:     e->op = EXPR_LOGICAL_OR;  break;
            default: __builtin_unreachable();
        }

        result = e;
    }

    return result;
}

// ternary
static Expr* parse_expr_ternary(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_expr_NEW(tu, s, 0);

    if (tokens_get(s)->type == '?') {
        tokens_next(s);

        Expr* mhs = parse_expr(tu, s);

        expect(tu, s, ':');

        Expr* rhs = parse_expr_ternary(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);
        Expr* e = make_expr(tu);
        *e = (Expr){
            .op = EXPR_TERNARY,
            .loc = { start_loc, end_loc },
            .ternary_op = {lhs, mhs, rhs},
        };

        return e;
    } else {
        return lhs;
    }
}

// = += -= *= /= %= <<= >>= &= ^= |=
//
// NOTE(NeGate): a=b=c is a=(b=c) not (a=b)=c
static Expr* parse_expr_assign(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_expr_ternary(tu, s);

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
            case TOKEN_ASSIGN:            op = EXPR_ASSIGN;          break;
            case TOKEN_PLUS_EQUAL:        op = EXPR_PLUS_ASSIGN;     break;
            case TOKEN_MINUS_EQUAL:       op = EXPR_MINUS_ASSIGN;    break;
            case TOKEN_TIMES_EQUAL:       op = EXPR_TIMES_ASSIGN;    break;
            case TOKEN_SLASH_EQUAL:       op = EXPR_SLASH_ASSIGN;    break;
            case TOKEN_PERCENT_EQUAL:     op = EXPR_PERCENT_ASSIGN;  break;
            case TOKEN_AND_EQUAL:         op = EXPR_AND_ASSIGN;      break;
            case TOKEN_OR_EQUAL:          op = EXPR_OR_ASSIGN;       break;
            case TOKEN_XOR_EQUAL:         op = EXPR_XOR_ASSIGN;      break;
            case TOKEN_LEFT_SHIFT_EQUAL:  op = EXPR_SHL_ASSIGN;      break;
            case TOKEN_RIGHT_SHIFT_EQUAL: op = EXPR_SHR_ASSIGN;      break;
            default: TODO();
        }
        tokens_next(s);
        Expr* rhs = parse_expr_assign(tu, s);

        SourceLoc end_loc = tokens_get_last_location(s);

        *e = (Expr){
            .op = op,
            .loc = { start_loc, end_loc },
            .bin_op = {lhs, rhs},
        };
        return e;
    } else {
        return lhs;
    }
}

static Expr* parse_expr_l15(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_expr_assign(tu, s);

    while (tokens_get(s)->type == TOKEN_COMMA) {
        Expr* e = make_expr(tu);
        ExprOp op = EXPR_COMMA;
        tokens_next(s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* rhs = parse_expr_assign(tu, s);
        *e = (Expr){
            .op = op,
            .loc = { start_loc, end_loc },
            .bin_op = {lhs, rhs},
        };

        lhs = e;
    }

    return lhs;
}

static Expr* parse_expr(TranslationUnit* tu, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            generic_error(tu, s, "pragma declaration expects string literal");
        }
        tokens_next(s);

        expect(tu, s, ')');
    }

    return parse_expr_l15(tu, s);
}
