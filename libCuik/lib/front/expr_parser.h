////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
// This file is included into parser.h, it's parser of the parser module and is
// completely static
static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);
static void parse_expr(Cuik_Parser* restrict parser, TokenStream* restrict s);
static void parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof);
static void parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof);
static void parse_assignment(Cuik_Parser* restrict parser, TokenStream* restrict s);
static void parse_initializer2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type);
static intmax_t parse_const_expr(Cuik_Parser* parser, TokenStream* restrict s);

static Cuik_Expr* parse_expr2(Cuik_Parser* restrict parser, TokenStream* restrict s);

typedef struct {
    char prec;
    char op;
} ExprInfo;

static ExprOp get_unary(TknType ty) {
    #define ON(k, op) case TOKEN_ ## k: return EXPR_ ## op ;
    switch (ty) {
        ON(TIMES,              DEREF);
        ON(EXCLAMATION,        LOGICAL_NOT);
        ON(MINUS,              NEGATE);
        ON(PLUS,               NONE);
        ON(TILDE,              NOT);
        ON(AND,                ADDR);
        ON(INCREMENT,          PRE_INC);
        ON(DECREMENT,          PRE_DEC);
        // zero means it's not a unary operator
        default: return 0;
    }
    #undef ON
}

static ExprInfo get_binop(TknType ty) {
    #define ON(k, prec, op) case TOKEN_ ## k: return (ExprInfo){ prec, EXPR_ ## op };
    switch (ty) {
        ON(TIMES,         11,  TIMES);
        ON(SLASH,         11,  SLASH);
        ON(PERCENT,       11,  PERCENT);
        ON(PLUS,          10,  PLUS);
        ON(MINUS,         10,  MINUS);
        ON(LEFT_SHIFT,    9,   SHL);
        ON(RIGHT_SHIFT,   9,   SHR);
        ON(GREATER_EQUAL, 8,   CMPGE);
        ON(LESS_EQUAL,    8,   CMPLE);
        ON(GREATER,       8,   CMPGT);
        ON(LESS,          8,   CMPLT);
        ON(EQUALITY,      7,   CMPEQ);
        ON(NOT_EQUAL,     7,   CMPNE);
        ON(AND,           6,   AND);
        ON(XOR,           5,   XOR);
        ON(OR,            4,   OR);
        ON(DOUBLE_AND,    3,   LOGICAL_AND);
        ON(DOUBLE_OR,     2,   LOGICAL_OR);
        // zero means it's not a binary operator
        default: return (ExprInfo){ 0 };
    }
    #undef ON
}

static Subexpr* push_expr(Cuik_Parser* parser) {
    if (parser->expr == NULL) {
        parser->expr = TB_ARENA_ALLOC(parser->arena, Cuik_Expr);
        *parser->expr = (Cuik_Expr){ .exprs = tls_push(0), .first_symbol = -1 };
    }

    return tls_push(sizeof(Subexpr));
}

static Subexpr* peek_expr(Cuik_Parser* parser) {
    Subexpr* end = tls_push(0);
    return &end[-1];
}

static Cuik_Expr* complete_expr(Cuik_Parser* parser) {
    Cuik_Expr* e = parser->expr;

    // move to more permanent storage
    size_t count = e->count = ((Subexpr*) tls_push(0)) - e->exprs;
    Subexpr* exprs = TB_ARENA_ARR_ALLOC(parser->arena, count, Subexpr);
    memcpy(exprs, e->exprs, count * sizeof(Subexpr));
    tls_restore(e->exprs);

    if (e->first_symbol >= 0) {
        parser->expr->next_in_chain = symbol_chain_start;
        symbol_chain_start = parser->expr;
    }

    e->exprs = exprs;
    parser->expr = NULL;
    return e;
}

static InitNode* make_init_node(Cuik_Parser* parser, TokenStream* restrict s, int mode) {
    InitNode* n = TB_ARENA_ALLOC(parser->arena, InitNode);
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
static InitNode* parse_initializer_member2(Cuik_Parser* parser, TokenStream* restrict s) {
    InitNode *current = NULL, *head = NULL;
    for (;;) {
        if (tokens_get(s)->type == '[')  {
            SourceLoc loc = tokens_get_location(s);
            tokens_next(s);

            intmax_t start = parse_const_expr(parser, s);
            if (start < 0) {
                // TODO(NeGate): Error messages
                diag_err(s, tokens_get_range(s), "array initializer range is broken.");
            }

            // GNU-extension: array range initializer
            intmax_t count = 1;
            if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                tokens_next(s);

                count = parse_const_expr(parser, s) - start;
                if (count <= 1) {
                    // TODO(NeGate): Error messages
                    diag_err(s, tokens_get_range(s), "array initializer range is broken.");
                }
            }
            expect_char(s, ']');

            if (current == NULL) {
                current = head = make_init_node(parser, s, INIT_ARRAY);
            } else {
                InitNode* n = make_init_node(parser, s, INIT_ARRAY);
                current->kid = n;
                current->kids_count++;
                current = n;
            }
            current->mode = INIT_ARRAY, current->start = start, current->count = count;
            current->loc = (SourceRange){ loc, tokens_get_last_location(s) };
            continue;
        }

        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            SourceLoc loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);
            tokens_next(s);

            if (current == NULL) {
                current = head = make_init_node(parser, s, INIT_MEMBER);
            } else {
                InitNode* n = make_init_node(parser, s, INIT_MEMBER);
                current->kid = n;
                current->kids_count++;
                current = n;
            }
            current->member_name = name;
            current->loc = (SourceRange){ loc, tokens_get_last_location(s) };
            continue;
        }

        break;
    }

    if (current == NULL) {
        current = head = make_init_node(parser, s, INIT_NONE);
    } else {
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
        while (!tokens_eof(s) && tokens_get(s)->type != '}') {
            if (expect_comma) {
                if (!expect_char(s, ',')) tokens_next(s);

                // we allow for trailing commas like ballers do
                if (tokens_get(s)->type == '}') break;
            } else expect_comma = true;

            // attach to our linked list
            tail = append_to_init_list(s, current, tail, parse_initializer_member2(parser, s));
        }

        expect_char(s, '}');
    } else {
        // parse without comma operator
        parse_assignment(parser, s);
        current->expr = complete_expr(parser);
    }
    current->loc = (SourceRange){ loc, tokens_get_last_location(s) };

    return head;
}

static void parse_initializer2(Cuik_Parser* parser, TokenStream* restrict s, Cuik_QualType type) {
    SourceLoc loc = tokens_get_location(s);
    expect_char(s, '{');

    InitNode *root = TB_ARENA_ALLOC(parser->arena, InitNode), *tail = NULL;
    *root = (InitNode){ 0 };

    // don't expect one the first time
    bool expect_comma = false;
    while (!tokens_eof(s) && tokens_get(s)->type != '}') {
        tail = append_to_init_list(s, root, tail, parse_initializer_member2(parser, s));

        if (tokens_get(s)->type == ',') {
            tokens_next(s);
            continue;
        } else {
            break;
        }
    }
    expect_char(s, '}');

    *push_expr(parser) = (Subexpr){
        .op = EXPR_INITIALIZER,
        .loc = { loc, tokens_get_last_location(s) },
        .init = { type, root },
    };
}

static void parse_string_literal(Cuik_Parser* parser, TokenStream* restrict s, Subexpr* e) {
    size_t saved_lexer_pos = s->list.current;
    size_t total_len = 0;
    while (!tokens_eof(s)) {
        Token* t = tokens_get(s);
        if (t->type == TOKEN_STRING_DOUBLE_QUOTE || t->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
            total_len += t->content.length - 2;
        } else if (string_equals_cstr(&t->content, "__func__")) {
            if (cuik__sema_function_stmt) total_len += strlen(cuik__sema_function_stmt->decl.name);
            else total_len += 3; // "???"
        } else {
            break;
        }

        tokens_next(s);
    }

    size_t curr = 0;
    char* buffer = tb_arena_alloc(parser->arena, total_len + 3);

    buffer[curr++] = '\"';

    // Fill up the buffer
    s->list.current = saved_lexer_pos;
    while (!tokens_eof(s)) {
        Token* t = tokens_get(s);
        if (t->type == TOKEN_STRING_DOUBLE_QUOTE || t->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
            memcpy(&buffer[curr], t->content.data + 1, t->content.length - 2);
            curr += t->content.length - 2;
        } else if (string_equals_cstr(&t->content, "__func__")) {
            if (cuik__sema_function_stmt) {
                size_t len = strlen(cuik__sema_function_stmt->decl.name);
                memcpy(&buffer[curr], cuik__sema_function_stmt->decl.name, len);
                curr += len;
            } else {
                memcpy(&buffer[curr], "???", 3);
                curr += 3;
            }
        } else {
            break;
        }

        tokens_next(s);
    }

    buffer[curr++] = '\"';

    e->str.start = (const unsigned char*)buffer;
    e->str.end = (const unsigned char*)(buffer + curr);
}

// primary-expression:
//   identifier
//   constant
//   string-literal
//   ( expression )
//   generic-selection
static void parse_primary_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    if (t->type == '(') {
        SourceLoc start_loc = tokens_get_location(s);
        tokens_next(s);

        parse_expr(parser, s);
        expect_closing_paren(s, start_loc);

        Subexpr* e = peek_expr(parser);
        e->has_parens = true;
        e->loc.start = start_loc;
        e->loc.end = tokens_get_last_location(s);
        return;
    }

    Subexpr* e = NULL;
    SourceLoc start_loc = tokens_get_location(s);

    switch (t->type) {
        case TOKEN_IDENTIFIER: {
            if (string_equals_cstr(&t->content, "__va_arg")) {
                tokens_next(s);

                expect_char(s, '(');
                parse_assignment(parser, s);
                expect_char(s, ',');
                Cuik_QualType type = parse_typename2(parser, s);
                expect_char(s, ')');

                tokens_prev(s);

                e = push_expr(parser);
                *e = (Subexpr){
                    .op = EXPR_VA_ARG,
                    .va_arg_ = { type },
                };
                break;
            } else if (!parser->is_in_global_scope && string_equals_cstr(&t->content, "__func__")) {
                tokens_next(s);
                Atom name = cuik__sema_function_stmt->decl.name;

                e = push_expr(parser);
                *e = (Subexpr){
                    .op = EXPR_STR,
                    .str.start = (const unsigned char*) name,
                    .str.end = (const unsigned char*) &name[strlen(name)],
                };
                break;
            }

            e = push_expr(parser);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Symbol* sym = NULL;
            ptrdiff_t builtin_search = nl_map_get_cstr(parser->target->builtin_func_map, name);
            if (builtin_search >= 0) {
                *e = (Subexpr){
                    .op = EXPR_BUILTIN_SYMBOL,
                    .builtin_sym = { name },
                };
            } else {
                sym = cuik_symtab_lookup(parser->symbols, name);
                if (sym != NULL) {
                    if (sym->storage_class == STORAGE_PARAM) {
                        *e = (Subexpr){
                            .op = EXPR_PARAM,
                            .param_num = sym->param_num
                        };
                    } else if (sym->storage_class == STORAGE_ENUM) {
                        *e = (Subexpr){
                            .op = EXPR_ENUM,
                            .enum_val = { sym->type, &cuik_canonical_type(sym->type)->enumerator.entries[sym->enum_value] },
                        };
                    } else {
                        assert(sym->stmt != NULL);
                        *e = (Subexpr){
                            .op = EXPR_SYMBOL,
                            .sym = { sym->stmt },
                        };
                    }
                } else {
                    diag_unresolved_symbol(parser, name, start_loc);

                    *e = (Subexpr){
                        .op = EXPR_UNKNOWN_SYMBOL,
                        .unknown_sym = { name },
                    };
                }
            }

            // only known symbols participate in the global collection phase,
            // and if it's an EXPR_SYMBOL then sym != NULL so we don't check
            // that here
            if (e->op == EXPR_SYMBOL && sym->storage_class != STORAGE_PARAM && sym->storage_class != STORAGE_ENUM && sym->storage_class != STORAGE_TYPEDEF && sym->storage_class != STORAGE_LOCAL) {
                // append to list inside of the expression
                ptrdiff_t i = e - parser->expr->exprs;
                e->sym.next_symbol = parser->expr->first_symbol;
                parser->expr->first_symbol = i;
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
                    diag_err(s, get_token_range(t), "invalid float literal");
                }
            }

            e = push_expr(parser);
            *e = (Subexpr){
                .op = is_float32 ? EXPR_FLOAT32 : EXPR_FLOAT64,
                .float_lit = f,
            };
            break;
        }

        case TOKEN_INTEGER: {
            Token* t = tokens_get(s);
            Cuik_IntSuffix suffix;
            uint64_t i = parse_int(t->content.length, (const char*) t->content.data, &suffix);

            e = push_expr(parser);
            *e = (Subexpr){
                .op = EXPR_INT,
                .int_lit = { i, suffix },
            };
            break;
        }

        case TOKEN_STRING_SINGLE_QUOTE:
        case TOKEN_STRING_WIDE_SINGLE_QUOTE: {
            Token* t = tokens_get(s);

            int ch = 0;
            ptrdiff_t distance = parse_char(t->content.length - 2, (const char*) &t->content.data[1], &ch);
            if (distance < 0) {
                diag_err(s, get_token_range(t), "invalid character literal");
            }

            e = push_expr(parser);
            *e = (Subexpr){
                .op = t->type == TOKEN_STRING_SINGLE_QUOTE ? EXPR_CHAR : EXPR_WCHAR,
                .char_lit = ch,
            };
            break;
        }

        case TOKEN_KW_Embed: {
            tokens_next(s);

            SourceLoc opening_loc = tokens_get_location(s);
            expect_char(s, '(');

            String content = tokens_get(s)->content;
            tokens_next(s);

            Cuik_QualType char_type = cuik_uncanonical_type(&parser->target->signed_ints[CUIK_BUILTIN_CHAR]);

            e = push_expr(parser);
            *e = (Subexpr){
                .op = EXPR_STR,
                .has_visited = true,
                .str = { content.data, content.data + content.length }
            };

            expect_closing_paren(s, opening_loc);
            tokens_prev(s);
            break;
        }

        case TOKEN_STRING_DOUBLE_QUOTE:
        case TOKEN_STRING_WIDE_DOUBLE_QUOTE: {
            bool is_wide = (tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE);

            e = push_expr(parser);
            *e = (Subexpr){
                .op = is_wide ? EXPR_WSTR : EXPR_STR,
            };

            parse_string_literal(parser, s, e);
            tokens_prev(s);
            break;
        }

        case TOKEN_KW_Generic: {
            tokens_next(s);

            SourceLoc opening_loc = tokens_get_location(s);
            expect_char(s, '(');

            // controlling expression followed by a comma
            parse_assignment(parser, s);

            e = push_expr(parser);
            *e = (Subexpr){ .op = EXPR_GENERIC };

            expect_char(s, ',');

            size_t entry_count = 0;
            C11GenericEntry* entries = tls_save();

            SourceRange default_loc = { 0 };
            while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                if (tokens_get(s)->type == TOKEN_KW_default) {
                    if (default_loc.start.raw != 0) {
                        diag_err(s, tokens_get_range(s), "multiple default cases on _Generic");
                        diag_note(s, default_loc, "see here");
                    }

                    default_loc = tokens_get_range(s);
                    expect_char(s, ':');

                    parse_assignment(parser, s);
                    Cuik_Expr* expr = complete_expr(parser);

                    // the default case is like a normal entry but without a type :p
                    tls_push(sizeof(C11GenericEntry));
                    entries[entry_count++] = (C11GenericEntry){
                        .key = CUIK_QUAL_TYPE_NULL,
                        .value = expr,
                    };
                } else {
                    Cuik_QualType type = parse_typename2(parser, s);
                    assert(!CUIK_QUAL_TYPE_IS_NULL(type) && "TODO: error recovery");
                    expect_char(s, ':');

                    parse_assignment(parser, s);
                    Cuik_Expr* expr = complete_expr(parser);

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
            C11GenericEntry* dst = tb_arena_alloc(parser->arena, entry_count * sizeof(C11GenericEntry));
            memcpy(dst, entries, entry_count * sizeof(C11GenericEntry));

            e->generic_.case_count = entry_count;
            e->generic_.cases = dst;

            tls_restore(entries);
            tokens_prev(s);
            break;
        }

        default:
        diag_err(s, tokens_get_range(s), "could not parse expression");

        e = push_expr(parser);
        *e = (Subexpr){ .op = EXPR_NONE };
        return;
    }
    tokens_next(s);

    e->loc.start = start_loc;
    e->loc.end = tokens_get_last_location(s);
}

static void parse_postfix(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
    SourceLoc start_loc = tokens_get_location(s);
    bool has_expr = false;

    // initializer list handling:
    //   '(' type-name ')' '{' initializer-list '}'
    //   '(' type-name ')' '{' initializer-list ',' '}'
    size_t fallback = s->list.current;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");
        if (!is_typename(parser, s)) {
            s->list.current = fallback;
            goto normal_path;
        }

        Cuik_QualType type = parse_typename2(parser, s);
        expect_closing_paren(s, start_loc);

        if (tokens_get(s)->type != '{') {
            if (in_sizeof) {
                // HACKY but it does get us to the 'sizeof' as opposed to the paren
                start_loc = s->list.tokens[s->list.current - 4].location;

                // resolve as sizeof (T)
                SourceLoc end_loc = tokens_get_last_location(s);
                *push_expr(parser) = (Subexpr){
                    .op = EXPR_SIZEOF_T,
                    .loc = { start_loc, end_loc },
                    .x_of_type = { type },
                };
                return;
            } else {
                s->list.current = fallback;
                has_expr = true;
                goto normal_path;
            }
        }

        parse_initializer2(parser, s, type);
        has_expr = true;
    }

    normal_path:
    start_loc = tokens_get_location(s);

    bool use_constructor = false;
    if (!has_expr) {
        if (parser->version == CUIK_VERSION_GLSL && is_typename(parser, s)) {
            Cuik_Type* type = parse_glsl_type(parser, s);
            SourceLoc end_loc = tokens_get_last_location(s);

            Subexpr* e = push_expr(parser);
            *e = (Subexpr){
                .op = EXPR_CONSTRUCTOR,
                .loc = { start_loc, end_loc },
                .constructor = { type },
            };

            if (tokens_get(s)->type != '(') {
                diag_err(s, e->loc, "Expected parenthesis after constructor name");
            }
        } else {
            parse_primary_expr(parser, s);
        }
    }

    // after any of the: [] () . ->
    // it'll restart and take a shot at matching another
    // piece of the expression.
    try_again: {
        if (tokens_get(s)->type == '[') {
            tokens_next(s);
            parse_expr(parser, s);
            expect_char(s, ']');

            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = EXPR_SUBSCRIPT,
                .loc = { start_loc, end_loc },
            };

            if (use_constructor) {
                diag_err(s, (SourceRange){ start_loc, end_loc }, "Cannot get element of type");
            }
            goto try_again;
        }

        // Pointer member access
        if (tokens_get(s)->type == TOKEN_ARROW) {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                diag_err(s, tokens_get_range(s), "Expected identifier after member access a.b");
            }

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);
            tokens_next(s);

            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = EXPR_ARROW,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .name = name },
            };

            if (use_constructor) {
                diag_err(s, (SourceRange){ start_loc, end_loc }, "Cannot get member of type");
            }
            goto try_again;
        }

        // Member access
        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                diag_err(s, tokens_get_range(s), "Expected identifier after member access a.b");
            }

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);
            tokens_next(s);

            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = EXPR_DOT,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .name = name },
            };

            if (use_constructor) {
                diag_err(s, (SourceRange){ start_loc, end_loc }, "Cannot get member of type");
            }
            goto try_again;
        }

        // Function call
        if (tokens_get(s)->type == '(') {
            SourceLoc open_loc = tokens_get_location(s);
            tokens_next(s);

            int param_count = 0;
            while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                if (param_count) {
                    if (tokens_get(s)->type != ',') {
                        break;
                    }

                    tokens_next(s);
                }

                parse_assignment(parser, s);
                param_count++;
            }

            expect_closing_paren(s, open_loc);
            SourceLoc end_loc = tokens_get_last_location(s);

            *push_expr(parser) = (Subexpr){
                .op = EXPR_CALL,
                .loc = { start_loc, end_loc },
                .call = { param_count },
            };
            goto try_again;
        }

        if (tokens_get(s)->type == TOKEN_INCREMENT || tokens_get(s)->type == TOKEN_DECREMENT) {
            bool is_inc = tokens_get(s)->type == TOKEN_INCREMENT;
            tokens_next(s);
            SourceLoc end_loc = tokens_get_last_location(s);

            *push_expr(parser) = (Subexpr){
                .op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
                .loc = { start_loc, end_loc },
            };

            if (use_constructor) {
                diag_err(s, (SourceRange){ start_loc, end_loc }, "Cannot increment or decrement type");
            }
            goto try_again;
        }

        return;
    }
}

// unary-expression:
//   ++ unary-expression
//   -- unary-expression
//   unary-operator cast-expression
//   sizeof unary-expression
//   sizeof ( type-name )
//   _Alignof ( type-name )
//   postfix-expression
//
// unary-operator: one of
//     & * + - ~ !
static void parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
    SourceLoc start_loc = tokens_get_location(s);
    TknType tkn = tokens_get(s)->type;

    if (tkn == TOKEN_KW_Alignof) {
        tokens_next(s);
        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");

        SourceLoc opening_loc = tokens_get_location(s);
        expect_char(s, '(');

        Cuik_QualType type = parse_typename2(parser, s);

        SourceLoc end_loc = tokens_get_last_location(s);
        expect_closing_paren(s, opening_loc);

        *push_expr(parser) = (Subexpr){
            .op = EXPR_ALIGNOF_T,
            .loc = { start_loc, end_loc },
            .x_of_type = { type },
        };
        return;
    } else if (tkn == TOKEN_KW_sizeof) {
        tokens_next(s);
        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");

        parse_unary(parser, s, true);
        if (peek_expr(parser)->op != EXPR_SIZEOF_T) {
            // convert expression into sizeof content
            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = EXPR_SIZEOF,
                .loc = { start_loc, end_loc },
            };
        }
    } else {
        ExprOp op = get_unary(tkn);

        if (op != EXPR_NONE) {
            tokens_next(s);
            parse_cast(parser, s, in_sizeof);

            if (op != EXPR_NONE) {
                SourceLoc end_loc = tokens_get_last_location(s);
                *push_expr(parser) = (Subexpr){
                    .op = op,
                    .loc = { start_loc, end_loc },
                };
            }
        } else {
            // skip unary +
            if (tkn == TOKEN_PLUS) tokens_next(s);

            parse_postfix(parser, s, in_sizeof);
        }
    }
}

static void parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
    SourceLoc start_loc = tokens_get_location(s);

    size_t fallback = s->list.current;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");
        if (!is_typename(parser, s)) {
            // this is not a cast
            s->list.current = fallback;
            goto normal_path;
        }

        Cuik_QualType type = parse_typename2(parser, s);
        expect_closing_paren(s, start_loc);

        if (tokens_get(s)->type == '{') {
            if (in_sizeof) {
                // resolve as sizeof (T)
                SourceLoc end_loc = tokens_get_last_location(s);
                *push_expr(parser) = (Subexpr){
                    .op = EXPR_SIZEOF_T,
                    .loc = { start_loc, end_loc },
                    .x_of_type = { type },
                };
                return;
            } else {
                // this is an initializer list not a normal cast
                s->list.current = fallback;
                goto normal_path;
            }
        }

        parse_cast(parser, s, false);

        SourceLoc end_loc = tokens_get_last_location(s);
        *push_expr(parser) = (Subexpr){
            .op = EXPR_CAST,
            .loc = { start_loc, start_loc },
            .cast = { type },
        };
        return;
    }

    normal_path:
    parse_unary(parser, s, false);
}

static void parse_binop(Cuik_Parser* restrict parser, TokenStream* restrict s, int min_prec) {
    Subexpr* start_of_expr = (Subexpr*) tls_push(0);

    // This precendence climber is always left associative
    SourceLoc start_loc = tokens_get_location(s);
    parse_cast(parser, s, false);

    ExprInfo binop;
    while (binop = get_binop(tokens_get(s)->type), binop.prec != 0 && binop.prec >= min_prec) {
        tokens_next(s);

        if (binop.op == EXPR_LOGICAL_AND || binop.op == EXPR_LOGICAL_OR) {
            // a = b || c
            //     ^
            //     we need to split from here to the logical or instead of just everything
            //     to the left of it.
            Cuik_Expr* hide = parser->expr;
            parser->expr = NULL;

            // complete expr between start_of_expr and now
            Cuik_Expr* left = NULL;
            {
                ptrdiff_t first_sym = -1;

                // relocate symbols
                ptrdiff_t start_i = start_of_expr - hide->exprs;

                // if it's part of the left expression, move it out
                // of the hidden expression.
                ptrdiff_t sym = hide->first_symbol;
                if (sym >= start_i) first_sym = sym - start_i;

                while (sym >= start_i) {
                    ptrdiff_t next = hide->exprs[sym].sym.next_symbol;

                    hide->exprs[sym].sym.next_symbol = next - start_i;
                    sym = hide->first_symbol = next;
                }

                // copy
                size_t count = (Subexpr*) tls_push(0) - start_of_expr;
                Subexpr* exprs = TB_ARENA_ARR_ALLOC(parser->arena, count, Subexpr);
                memcpy(exprs, start_of_expr, count * sizeof(Subexpr));
                tls_restore(start_of_expr);

                left = TB_ARENA_ALLOC(parser->arena, Cuik_Expr);
                *left = (Cuik_Expr){ .exprs = exprs, .count = count, .first_symbol = first_sym };

                if (left->first_symbol >= 0) {
                    left->next_in_chain = symbol_chain_start;
                    symbol_chain_start = left;
                }
            }

            parse_binop(parser, s, binop.prec + 1);
            Cuik_Expr* right = complete_expr(parser);

            // restore original expr stream
            parser->expr = hide;

            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = binop.op,
                .loc = { start_loc, end_loc },
                .logical_binop = { left, right }
            };
        } else {
            parse_binop(parser, s, binop.prec + 1);

            SourceLoc end_loc = tokens_get_last_location(s);
            *push_expr(parser) = (Subexpr){
                .op = binop.op,
                .loc = { start_loc, end_loc },
            };
        }
    }
}

// ternary
static void parse_ternary(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    parse_binop(parser, s, 0);

    if (tokens_get(s)->type == '?') {
        tokens_next(s);

        // ternaries are weird because we need to convert the left and right sides
        // into their own separate Cuik_Expr but we've already got stuff in progress
        // so we'll temporarily hide it.
        Cuik_Expr* hide = parser->expr;
        parser->expr = NULL;

        // left expression
        parse_expr(parser, s);
        Cuik_Expr* left = complete_expr(parser);

        expect_char(s, ':');

        // right expression
        parse_ternary(parser, s);
        Cuik_Expr* right = complete_expr(parser);

        // we can unhide the condition now
        parser->expr = hide;

        SourceLoc end_loc = tokens_get_last_location(s);
        *push_expr(parser) = (Subexpr){
            .op = EXPR_TERNARY,
            .loc = { start_loc, end_loc },
            .ternary = { left, right }
        };
    }
}

// = += -= *= /= %= <<= >>= &= ^= |=
//
// NOTE(NeGate): a=b=c is a=(b=c) not (a=b)=c
static void parse_assignment(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    parse_ternary(parser, s);

    ExprOp op = EXPR_NONE;
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
        default: break;
    }

    if (op == EXPR_NONE) {
        return;
    }

    tokens_next(s);
    parse_assignment(parser, s);

    SourceLoc end_loc = tokens_get_last_location(s);
    *push_expr(parser) = (Subexpr){
        .op = op,
        .loc = { start_loc, end_loc },
    };
}

static void parse_pragma_expr(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);

        if (expect_char(s, '(')) {
            if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
                diag_err(s, tokens_get_range(s), "pragma declaration expects string literal");
            }
            tokens_next(s);

            expect_char(s, ')');
        }
    }
}

static void parse_expr(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    parse_pragma_expr(parser, s);

    SourceLoc start_loc = tokens_get_location(s);
    parse_assignment(parser, s);

    while (tokens_get(s)->type == TOKEN_COMMA) {
        ExprOp op = EXPR_COMMA;
        tokens_next(s);

        SourceLoc end_loc = tokens_get_last_location(s);

        parse_assignment(parser, s);
        *push_expr(parser) = (Subexpr){
            .op = op,
            .loc = { start_loc, end_loc },
        };
    }

    parse_pragma_expr(parser, s);
}

static Cuik_Expr* parse_expr2(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    parse_expr(parser, s);
    return complete_expr(parser);
}

static intmax_t parse_const_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    parse_assignment(parser, s);
    Cuik_Expr* e = complete_expr(parser);

    Cuik_ConstVal value;
    if (!const_eval(parser, e, &value)) {
        // the const_eval_int will handle errors
        return 0;
    }

    if (value.tag != CUIK_CONST_INT) {
        diag_err(&parser->tokens, e->exprs[e->count - 1].loc, "Constant expression was not an integer");
        return 0;
    }

    return value.i;
}
