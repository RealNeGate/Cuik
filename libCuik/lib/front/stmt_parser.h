static ParseResult parse_pragma(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Stmt* parse_stmt2(Cuik_Parser* parser, TokenStream* restrict s);

static Stmt* alloc_stmt(void) {
    Stmt* stmt = ARENA_ALLOC(&local_ast_arena, Stmt);
    memset(stmt, 0, sizeof(Stmt));
    return stmt;
}

// Doesn't handle it's own error recovery, once we return false the caller should try
// what it feels is necessary to get it to another parsable unit.
static bool parse_decl_or_expr2(Cuik_Parser* parser, TokenStream* restrict s, size_t* body_count) {
    SourceLoc loc = tokens_get_location(s);

    if (parse_pragma(parser, s) != 0) {
        return true;
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
        return true;
    } else if (is_typename(&parser->globals, s)) {
        Attribs attr = { 0 };
        Cuik_QualType type = parse_declspec2(parser, s, &attr);
        if (CUIK_QUAL_TYPE_IS_NULL(type)) {
            diag_err(s, (SourceRange){ loc, tokens_get_last_location(s) }, "could not parse base type.");
            tokens_next(s);

            type = cuik_uncanonical_type(parser->default_int);
        }

        while (!tokens_eof(s)) {
            size_t start_decl_token = s->list.current;
            Decl decl = parse_declarator2(parser, s, type, false);
            if (decl.name == NULL) {
                diag_warn(s, decl.loc, "Declaration has no name");
            }

            // Convert into statement
            Stmt* n = alloc_stmt();
            n->op = STMT_DECL;
            n->loc = decl.loc;
            n->decl = (struct StmtDecl){
                .name = decl.name,
                .type = decl.type,
                .attrs = attr,
            };
            *((Stmt**)tls_push(sizeof(Stmt*))) = n;
            *body_count += 1;

            if (decl.name != NULL) {
                if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
                    diag_err(s, decl.loc, "local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
                    return false;
                }

                local_symbols[local_symbol_count++] = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .storage_class = attr.is_typedef ? STORAGE_TYPEDEF : STORAGE_LOCAL,
                    .loc = decl.loc,
                    .stmt = n,
                };
            }

            Expr* e = NULL;
            if (tokens_get(s)->type == '=') {
                if (n->decl.attrs.is_inline) {
                    diag_err(s, decl.loc, "non-function declarations cannot be inline");
                }

                n->attr_list = parse_attributes(s, n->attr_list);
                tokens_next(s);

                if (tokens_get(s)->type == '{') {
                    e = parse_initializer2(parser, s, CUIK_QUAL_TYPE_NULL);
                } else {
                    e = parse_assignment(parser, s);
                }

                if (attr.is_typedef) {
                    diag_err(s, e->loc, "typedef cannot have initial expression");
                    e = NULL;
                }
            }
            n->decl.initial = e;

            if (tokens_get(s)->type == ';') {
                tokens_next(s);
                break;
            } else if (tokens_get(s)->type == ',') {
                tokens_next(s);
                continue;
            }
        }

        return true;
    } else {
        Stmt* n = alloc_stmt();
        size_t start_tkn = s->list.current; // used for error recovery
        Expr* expr = parse_expr_(parser, s);

        n->op = STMT_EXPR;
        n->loc = expr->loc;
        n->expr = (struct StmtExpr){ .expr = expr };

        *((Stmt**)tls_push(sizeof(Stmt*))) = n;
        *body_count += 1;

        if (start_tkn == s->list.current) {
            tokens_next(s);
        } else if (!expect_with_reason(s, ';', "expression")) {
            return PARSE_WIT_ERRORS;
        }
        return true;
    }
}

static void parse_compound_stmt2(Cuik_Parser* parser, TokenStream* restrict s, Stmt* node) {
    LOCAL_SCOPE {
        node->op = STMT_COMPOUND;
        node->loc.start = tokens_get_location(s);

        size_t kid_count = 0;
        Stmt** kids = tls_save();

        while (tokens_get(s)->type != '}') {
            if (tokens_get(s)->type == ';') {
                tokens_next(s);
            } else {
                Stmt* stmt = parse_stmt2(parser, s);
                if (stmt) {
                    tls_push(sizeof(Stmt*));
                    kids[kid_count++] = stmt;
                } else {
                    // this will push the decl or expression if it catches one
                    parse_decl_or_expr2(parser, s, &kid_count);
                }
            }
        }

        expect_char(s, '}');
        node->loc.end = tokens_get_last_location(s);

        Stmt** permanent_storage = arena_alloc(&thread_arena, kid_count * sizeof(Stmt*), _Alignof(Stmt*));
        memcpy(permanent_storage, kids, kid_count * sizeof(Stmt*));

        node->compound = (struct StmtCompound){
            .kids = permanent_storage,
            .kids_count = kid_count,
        };

        tls_restore(kids);
    }
}

static ParseResult parse_stmt_or_expr2(Cuik_Parser* parser, TokenStream* restrict s, Stmt** out_result) {
    int p = parse_pragma(parser, s);
    if (p != 0) {
        *out_result = NULL;
        return p;
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
        *out_result = NULL;
        return PARSE_SUCCESS;
    } else {
        Stmt* stmt = parse_stmt2(parser, s);
        if (stmt != NULL) {
            *out_result = stmt;
            return PARSE_SUCCESS;
        } else {
            Stmt* n = alloc_stmt();
            size_t start_tkn = s->list.current; // used for error recovery

            Expr* expr = parse_expr_(parser, s);
            n->op = STMT_EXPR;
            n->loc = expr->loc;
            n->expr = (struct StmtExpr){ .expr = expr };

            if (start_tkn == s->list.current) {
                tokens_next(s);
            } else if (!expect_with_reason(s, ';', "expression")) {
                return PARSE_WIT_ERRORS;
            }

            *out_result = n;
            return PARSE_SUCCESS;
        }
    }
}

// Doesn't handle declarators or expression-statements
static Stmt* parse_stmt2(Cuik_Parser* parser, TokenStream* restrict s) {
    // _Static_assert doesn't produce a statement, handle these first.
    // label declarations only produce a new statement if they haven't been used yet.
    SourceLoc start = tokens_get_location(s);
    while (tokens_get(s)->type == TOKEN_KW_Static_assert) {
        tokens_next(s);
        expect_char(s, '(');

        intmax_t condition = parse_const_expr2(parser, s);
        SourceLoc end = tokens_get_last_location(s);

        if (tokens_get(s)->type == ',') {
            tokens_next(s);

            Token* t = tokens_get(s);
            if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
                diag_err(s, get_token_range(t), "static assertion expects string literal");
            }
            tokens_next(s);

            if (condition == 0) {
                diag_err(s, (SourceRange){ start, end }, "static assertion failed! %.*s", (int) t->content.length, t->content.data);
            }
        } else {
            if (condition == 0) {
                diag_err(s, (SourceRange){ start, end }, "static assertion failed!");
            }
        }

        expect_char(s, ')');
        expect_char(s, ';');
    }

    Stmt* n = NULL;
    SourceLoc loc_start = tokens_get_location(s);
    TknType peek = tokens_get(s)->type;
    if (peek == '{') {
        tokens_next(s);

        n = alloc_stmt();
        parse_compound_stmt2(parser, s, n);
    } else if (peek == TOKEN_KW_return) {
        n = alloc_stmt();
        tokens_next(s);

        Expr* e = 0;
        if (tokens_get(s)->type != ';') {
            e = parse_expr_(parser, s);
        }

        n->op = STMT_RETURN;
        n->return_ = (struct StmtReturn){ .expr = e };

        expect_with_reason(s, ';', "return");
    } else if (peek == TOKEN_KW_if) {
        n = alloc_stmt();
        tokens_next(s);

        LOCAL_SCOPE {
            Expr* cond;
            {
                SourceLoc opening_loc = tokens_get_location(s);
                expect_char(s, '(');

                cond = parse_expr_(parser, s);

                expect_closing_paren(s, opening_loc);
            }

            Stmt* body;
            LOCAL_SCOPE {
                parse_stmt_or_expr2(parser, s, &body);
            }

            Stmt* next = 0;
            if (tokens_get(s)->type == TOKEN_KW_else) {
                tokens_next(s);

                LOCAL_SCOPE {
                    parse_stmt_or_expr2(parser, s, &next);
                }
            }

            n->op = STMT_IF;
            n->if_ = (struct StmtIf){
                .cond = cond,
                .body = body,
                .next = next,
            };
        }
    } else if (peek == TOKEN_KW_switch) {
        n = alloc_stmt();
        tokens_next(s);

        LOCAL_SCOPE {
            expect_char(s, '(');
            Expr* cond = parse_expr_(parser, s);
            expect_char(s, ')');

            n->op = STMT_SWITCH;
            n->switch_ = (struct StmtSwitch){ .condition = cond };

            // begin a new chain but keep the old one
            Stmt* old_switch = current_switch_or_case;
            current_switch_or_case = n;

            Stmt* old_breakable = current_breakable;
            current_breakable = n;
            LOCAL_SCOPE {
                parse_stmt_or_expr2(parser, s, &n->switch_.body);
            }
            current_breakable = old_breakable;
            current_switch_or_case = old_switch;
        }
    } else if (peek == TOKEN_KW_case) {
        // TODO(NeGate): error messages
        n = alloc_stmt();
        assert(current_switch_or_case);
        tokens_next(s);

        Stmt* top = n;
        top->op = STMT_CASE;

        intmax_t key = parse_const_expr2(parser, s);
        if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
            // GNU extension, case ranges
            tokens_next(s);
            intmax_t key_max = parse_const_expr2(parser, s);
            expect_with_reason(s, ':', "case");

            assert(key_max > key);
            n->case_.key = key;

            for (intmax_t i = key; i < key_max; i++) {
                Stmt* curr = alloc_stmt();
                curr->op = STMT_CASE;
                curr->case_ = (struct StmtCase){.key = i + 1};

                // Append to list
                n->case_.next = n->case_.body = curr;
                n = curr;
            }
        } else {
            expect_with_reason(s, ':', "case");

            n->case_ = (struct StmtCase){
                .key = key, .body = 0, .next = 0
            };
        }

        switch (current_switch_or_case->op) {
            case STMT_CASE: current_switch_or_case->case_.next = top; break;
            case STMT_DEFAULT: current_switch_or_case->default_.next = top; break;
            case STMT_SWITCH: current_switch_or_case->switch_.next = top; break;
            default: __builtin_unreachable();
        }
        current_switch_or_case = n;

        parse_stmt_or_expr2(parser, s, &n->case_.body);
    } else if (peek == TOKEN_KW_default) {
        // TODO(NeGate): error messages
        n = alloc_stmt();
        assert(current_switch_or_case);
        tokens_next(s);

        switch (current_switch_or_case->op) {
            case STMT_CASE: current_switch_or_case->case_.next = n; break;
            case STMT_DEFAULT: current_switch_or_case->default_.next = n; break;
            case STMT_SWITCH: current_switch_or_case->switch_.next = n; break;
            default: __builtin_unreachable();
        }
        current_switch_or_case = n;
        expect_with_reason(s, ':', "default");

        n->op = STMT_DEFAULT;
        n->default_ = (struct StmtDefault){
            .body = 0, .next = 0,
        };

        parse_stmt_or_expr2(parser, s, &n->default_.body);
    } else if (peek == TOKEN_KW_break) {
        // TODO(NeGate): error messages
        n = alloc_stmt();
        assert(current_breakable);

        tokens_next(s);
        expect_with_reason(s, ';', "break");

        n->op = STMT_BREAK;
        n->break_ = (struct StmtBreak){
            .target = current_breakable,
        };
    } else if (peek == TOKEN_KW_continue) {
        // TODO(NeGate): error messages
        n = alloc_stmt();
        assert(current_continuable);

        tokens_next(s);
        expect_with_reason(s, ';', "continue");

        n->op = STMT_CONTINUE;
        n->continue_ = (struct StmtContinue){
            .target = current_continuable,
        };
    } else if (peek == TOKEN_KW_while) {
        n = alloc_stmt();
        tokens_next(s);

        LOCAL_SCOPE {
            expect_char(s, '(');
            Expr* cond = parse_expr_(parser, s);
            expect_char(s, ')');

            // Push this as a breakable statement
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                parse_stmt_or_expr2(parser, s, &body);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            n->op = STMT_WHILE;
            n->while_ = (struct StmtWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_for) {
        n = alloc_stmt();
        tokens_next(s);

        LOCAL_SCOPE {
            expect_char(s, '(');

            // it's either nothing, a declaration, or an expression
            Stmt* first = NULL;
            if (tokens_get(s)->type == ';') {
                /* nothing */
                tokens_next(s);
            } else {
                // NOTE(NeGate): This is just a decl list or a single expression.
                first = alloc_stmt();
                first->op = STMT_COMPOUND;

                size_t kid_count = 0;
                Stmt** kids = tls_save();
                {
                    parse_decl_or_expr2(parser, s, &kid_count);
                }
                Stmt** permanent_storage = arena_alloc(&thread_arena, kid_count * sizeof(Stmt*), _Alignof(Stmt*));
                memcpy(permanent_storage, kids, kid_count * sizeof(Stmt*));

                first->compound = (struct StmtCompound){
                    .kids = permanent_storage,
                    .kids_count = kid_count,
                };
                tls_restore(kids);
            }

            Expr* cond = NULL;
            if (tokens_get(s)->type == ';') {
                /* nothing */
                tokens_next(s);
            } else {
                cond = parse_expr_(parser, s);
                expect_char(s, ';');
            }

            Expr* next = NULL;
            if (tokens_get(s)->type == ')') {
                /* nothing */
                tokens_next(s);
            } else {
                next = parse_expr_(parser, s);
                expect_char(s, ')');
            }

            // Push this as a breakable statement
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                parse_stmt_or_expr2(parser, s, &body);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            n->op = STMT_FOR;
            n->for_ = (struct StmtFor){
                .first = first,
                .cond = cond,
                .body = body,
                .next = next,
            };
        }
    } else if (peek == TOKEN_KW_do) {
        n = alloc_stmt();
        tokens_next(s);

        // Push this as a breakable statement
        LOCAL_SCOPE {
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                parse_stmt_or_expr2(parser, s, &body);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            if (tokens_get(s)->type != TOKEN_KW_while) {
                Token* t = tokens_get(s);

                diag_err(s, get_token_range(t), "expected 'while' got '%.*s'", (int)t->content.length, t->content.data);
            }
            tokens_next(s);

            expect_char(s, '(');

            Expr* cond = parse_expr_(parser, s);

            expect_char(s, ')');
            expect_char(s, ';');

            n->op = STMT_DO_WHILE;
            n->do_while = (struct StmtDoWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_goto) {
        n = alloc_stmt();
        tokens_next(s);

        // read label name
        Token* t = tokens_get(s);
        SourceRange loc = get_token_range(t);
        if (t->type != TOKEN_IDENTIFIER) {
            diag_err(s, loc, "expected identifier for goto target name");
            return n;
        }

        Atom name = atoms_put(t->content.length, t->content.data);

        // skip to the semicolon
        tokens_next(s);

        Expr* target = alloc_expr();
        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            *target = (Expr){
                .op = EXPR_SYMBOL,
                .loc = loc,
                .symbol = labels[search],
            };
        } else {
            // not defined yet, make a placeholder
            Stmt* label_decl = alloc_stmt();
            label_decl->op = STMT_LABEL;
            label_decl->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, label_decl);

            *target = (Expr){
                .op = EXPR_SYMBOL,
                .loc = loc,
                .symbol = label_decl,
            };
        }

        n->op = STMT_GOTO;
        n->goto_ = (struct StmtGoto){
            .target = target,
        };

        expect_with_reason(s, ';', "goto");
    } else if (peek == TOKEN_IDENTIFIER && tokens_peek(s)->type == TOKEN_COLON) {
        // label amirite
        // IDENTIFIER COLON STMT
        Token* t = tokens_get(s);
        Atom name = atoms_put(t->content.length, t->content.data);

        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            n = labels[search];
        } else {
            n = alloc_stmt();
            n->op = STMT_LABEL;
            n->loc = tokens_get_range(s);
            n->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, n);
        }

        n->label.placed = true;

        tokens_next(s);
        tokens_next(s);
    }

    if (n != NULL) {
        n->loc.start = loc_start;
        n->loc.end = tokens_get_last_location(s);
    }
    return n;
}

// function-definition:
//   declaration-specifiers declarator declaration-listOPT compound-statement
static bool parse_function(Cuik_Parser* parser, TokenStream* restrict s, Stmt* decl_node) {
    Cuik_Type* type = cuik_canonical_type(decl_node->decl.type);

    Param* param_list = type->func.param_list;
    size_t param_count = type->func.param_count;

    assert(local_symbol_start == local_symbol_count);
    if (param_count >= INT16_MAX) {
        diag_err(s, decl_node->loc, "Function parameter count cannot exceed %d (got %d)", param_count, MAX_LOCAL_SYMBOLS);
        return false;
    }

    for (size_t i = 0; i < param_count; i++) {
        Param* p = &param_list[i];
        if (p->name == NULL) continue;

        local_symbols[local_symbol_count++] = (Symbol){
            .name = p->name,
            .type = p->type,
            .storage_class = STORAGE_PARAM,
            .param_num = i
        };
    }

    // skip { for parse_compound_stmt
    tokens_next(s);
    Stmt* body;
    {
        cuik__sema_function_stmt = decl_node;

        body = alloc_stmt();
        parse_compound_stmt2(parser, s, body);

        cuik__sema_function_stmt = NULL;
    }

    decl_node->op = STMT_FUNC_DECL;
    decl_node->decl.initial_as_stmt = body;

    nl_strmap_free(labels);
    return true;
}
