static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_expr_(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_assignment(Cuik_Parser* restrict parser, TokenStream* restrict s);

typedef struct {
    char prec;
    char op;
} ExprInfo;

static ExprOp get_unary(TknType ty) {
    #define ON(k, op) case TOKEN_ ## k: return EXPR_ ## op ;
    switch (ty) {
        ON(TIMES,              DEREF);
        ON(EXCLAMATION,        LOGICAL_NOT);
        ON(DOUBLE_EXCLAMATION, CAST); // converts to boolean
        ON(MINUS,              NEGATE);
        ON(PLUS,               PLUS); // this is a hack, it'll be treated as empty even tho it says EXPR_PLUS
        ON(TILDE,              NOT);
        ON(AND,                ADDR);
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

static Expr* alloc_expr(void) {
    return ARENA_ALLOC(&local_ast_arena, Expr);
}

// primary-expression:
//   identifier
//   constant
//   string-literal
//   ( expression )
//   generic-selection
static Expr* parse_primary_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    if (t->type == '(') {
        SourceLoc start_loc = tokens_get_location(s);
        tokens_next(s);

        Expr* e = parse_expr_(parser, s);
        expect_closing_paren(s, start_loc);

        e->has_parens = true;
        e->loc.start = start_loc;
        e->loc.end = tokens_get_last_location(s);
        return e;
    }

    Expr* e = alloc_expr();
    SourceLoc start_loc = tokens_get_location(s);

    switch (t->type) {
        case TOKEN_IDENTIFIER: {
            if (memeq(t->content.data, t->content.length, "__va_arg", sizeof("__va_arg") - 1)) {
                tokens_next(s);

                expect_char(s, '(');
                Expr* src = parse_assignment(parser, s);
                expect_char(s, ',');
                Cuik_QualType type = parse_typename2(parser, s);
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
                        .enum_val = { &cuik_canonical_type(sym->type)->enumerator.entries[sym->enum_value].value },
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
                ptrdiff_t builtin_search = nl_strmap_get_cstr(parser->target->builtin_func_map, name);
                if (builtin_search >= 0) {
                    *e = (Expr){
                        .op = EXPR_BUILTIN_SYMBOL,
                        .builtin_sym = { name },
                    };
                } else {
                    Symbol* symbol_search = find_global_symbol(&parser->globals, (const char*)name);
                    if (symbol_search != NULL) {
                        if (symbol_search->storage_class == STORAGE_ENUM) {
                            *e = (Expr){
                                .op = EXPR_ENUM,
                                .type = symbol_search->type,
                                .enum_val = { &cuik_canonical_type(symbol_search->type)->enumerator.entries[symbol_search->enum_value].value },
                            };
                        } else {
                            *e = (Expr){
                                .op = EXPR_SYMBOL,
                                .symbol = symbol_search->stmt,
                            };
                        }
                    } else {
                        //diag_unresolved_symbol(parser, name, start_loc);
                        diag_err(s, get_token_range(t), "could not resolve symbol: %s", name);

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
                    diag_err(s, get_token_range(t), "invalid float literal");
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
                .int_num = { i, suffix },
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
            expect_char(s, '(');

            // controlling expression followed by a comma
            Expr* controlling_expr = parse_assignment(parser, s);

            *e = (Expr){
                .op = EXPR_GENERIC,
                .generic_ = {.controlling_expr = controlling_expr},
            };
            expect_char(s, ',');

            size_t entry_count = 0;
            C11GenericEntry* entries = tls_save();

            SourceRange default_loc = { 0 };
            while (tokens_get(s)->type != ')') {
                if (tokens_get(s)->type == TOKEN_KW_default) {
                    if (default_loc.start.raw != 0) {
                        diag_err(s, tokens_get_range(s), "multiple default cases on _Generic");
                        diag_note(s, default_loc, "see here");
                    }

                    default_loc = tokens_get_range(s);
                    expect_char(s, ':');
                    Expr* expr = parse_assignment(parser, s);

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
                    Expr* expr = parse_assignment(parser, s);

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
        tokens_prev(s);
        break;
    }
    tokens_next(s);

    e->loc.start = start_loc;
    e->loc.end = tokens_get_last_location(s);
    return e;
}

static Expr* parse_postfix(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* e = NULL;

    // initializer list handling:
    //   '(' type-name ')' '{' initializer-list '}'
    //   '(' type-name ')' '{' initializer-list ',' '}'
    size_t fallback = s->list.current;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");
        if (!is_typename(&parser->globals, s)) {
            s->list.current = fallback;
            goto normal_path;
        }

        Cuik_QualType type = parse_typename2(parser, s);
        expect_closing_paren(s, start_loc);

        if (tokens_get(s)->type != '{') {
            s->list.current = fallback;
            goto normal_path;
        }

        tokens_next(s);
        __debugbreak();
        e = NULL; // parse_initializer2(parser, s, type);
    }

    normal_path:
    start_loc = tokens_get_location(s);
    if (e == NULL) {
        e = parse_primary_expr(parser, s);
    }

    // after any of the: [] () . ->
    // it'll restart and take a shot at matching another
    // piece of the expression.
    try_again: {
        if (tokens_get(s)->type == '[') {
            Expr* base = e;
            e = alloc_expr();

            tokens_next(s);
            Expr* index = parse_expr_(parser, s);
            expect_char(s, ']');

            SourceLoc end_loc = tokens_get_last_location(s);

            *e = (Expr){
                .op = EXPR_SUBSCRIPT,
                .loc = { start_loc, end_loc },
                .subscript = { base, index },
            };
            goto try_again;
        }

        // Pointer member access
        if (tokens_get(s)->type == TOKEN_ARROW) {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                diag_err(s, tokens_get_range(s), "Expected identifier after member access a.b");
            }

            SourceLoc end_loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Expr* base = e;
            e = alloc_expr();
            *e = (Expr){
                .op = EXPR_ARROW,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .base = base, .name = name },
            };

            tokens_next(s);
            goto try_again;
        }

        // Member access
        if (tokens_get(s)->type == '.') {
            tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                diag_err(s, tokens_get_range(s), "Expected identifier after member access a.b");
            }

            SourceLoc end_loc = tokens_get_location(s);

            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Expr* base = e;
            e = alloc_expr();
            *e = (Expr){
                .op = EXPR_DOT,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .base = base, .name = name },
            };

            tokens_next(s);
            goto try_again;
        }

        // Function call
        if (tokens_get(s)->type == '(') {
            tokens_next(s);

            Expr* target = e;
            e = alloc_expr();

            size_t param_count = 0;
            void* params = tls_save();

            while (tokens_get(s)->type != ')') {
                if (param_count) {
                    expect_char(s, ',');
                }

                Expr* e = parse_assignment(parser, s);
                *((Expr**)tls_push(sizeof(Expr*))) = e;
                param_count++;
            }

            if (tokens_get(s)->type != ')') {
                diag_err(s, tokens_get_range(s), "Unclosed parameter list!");
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

        if (tokens_get(s)->type == TOKEN_INCREMENT || tokens_get(s)->type == TOKEN_DECREMENT) {
            bool is_inc = tokens_get(s)->type == TOKEN_INCREMENT;
            tokens_next(s);
            SourceLoc end_loc = tokens_get_last_location(s);

            Expr* src = e;
            e = alloc_expr();
            *e = (Expr){
                .op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
                .loc = { start_loc, end_loc },
                .unary_op.src = e,
            };
            goto try_again;
        }

        return e;
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
static Expr* parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    // TODO(NeGate): Convert this code into a loop... please?
    // TODO(NeGate): just rewrite this in general...
    SourceLoc start_loc = tokens_get_location(s);
    TknType tkn = tokens_get(s)->type;

    if (tkn == TOKEN_KW_sizeof || tkn == TOKEN_KW_Alignof) {
        tokens_next(s);
        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");

        bool has_paren = false;
        SourceLoc opening_loc = { 0 };
        if (tokens_get(s)->type == '(') {
            has_paren = true;

            opening_loc = tokens_get_location(s);
            tokens_next(s);
        }

        size_t fallback = s->list.current;
        if (is_typename(&parser->globals, s)) {
            Cuik_QualType type = parse_typename2(parser, s);

            if (has_paren) {
                expect_closing_paren(s, opening_loc);
            }

            // glorified backtracing on who own's the (
            // sizeof (int){ 0 } is a sizeof a compound list
            // not a sizeof(int) with a weird { 0 } laying around
            if (tokens_get(s)->type == '{') {
                s->list.current = fallback;
                has_paren = false;
                goto normal_path;
            }

            SourceLoc end_loc = tokens_get_last_location(s);
            Expr* e = alloc_expr();
            *e = (Expr){
                .op = (tkn == TOKEN_KW_sizeof ? EXPR_SIZEOF_T : EXPR_ALIGNOF_T),
                .loc = { start_loc, end_loc },
                .x_of_type = { type },
            };
            return e;
        }

        normal_path:
        if (has_paren) {
            expect_closing_paren(s, opening_loc);
        }

        Expr* src = parse_unary(parser, s);
        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = alloc_expr();
        *e = (Expr){
            .op = (tkn == TOKEN_KW_sizeof ? EXPR_SIZEOF : EXPR_ALIGNOF),
            .loc = { start_loc, end_loc },
            .x_of_expr = { src },
        };
        return e;
    } else {
        ExprOp op = get_unary(tkn);

        if (op != EXPR_NONE) {
            tokens_next(s);
            Expr* value = parse_cast(parser, s);
            if (op == EXPR_PLUS) return value;

            SourceLoc end_loc = tokens_get_last_location(s);
            Expr* e = alloc_expr();

            if (op == EXPR_CAST) {
                // this is for !! which is converting to boolean
                *e = (Expr){
                    .op = EXPR_CAST,
                    .loc = { start_loc, end_loc },
                    .cast = { value, cuik_make_qual_type(&builtin_types[TYPE_BOOL], 0) },
                };
            } else {
                *e = (Expr){
                    .op = op,
                    .loc = { start_loc, end_loc },
                    .unary_op.src = value
                };
            }
            return e;
        } else {
            // either
            return parse_postfix(parser, s);
        }
    }
}

static Expr* parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);

    size_t fallback = s->list.current;
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");
        if (!is_typename(&parser->globals, s)) {
            // this is not a cast
            s->list.current = fallback;
            goto normal_path;
        }

        Cuik_QualType type = parse_typename2(parser, s);
        expect_closing_paren(s, start_loc);

        if (tokens_get(s)->type == '{') {
            // this is an initializer list not a normal cast
            s->list.current = fallback;
            goto normal_path;
        }

        Expr* base = parse_cast(parser, s);
        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = alloc_expr();
        *e = (Expr){
            .op = EXPR_CAST,
            .loc = { start_loc, start_loc },
            .cast = { base, type },
        };
        return e;
    }

    normal_path:
    return parse_unary(parser, s);
}

static Expr* parse_binop(Cuik_Parser* restrict parser, TokenStream* restrict s, int min_prec) {
    // This precendence climber is always left associative
    SourceLoc start_loc = tokens_get_location(s);
    Expr* result = parse_cast(parser, s);

    ExprInfo binop;
    while (binop = get_binop(tokens_get(s)->type), binop.prec != 0 && binop.prec >= min_prec) {
        tokens_next(s);

        Expr* e = alloc_expr();
        Expr* rhs = parse_binop(parser, s, binop.prec + 1);

        SourceLoc end_loc = tokens_get_last_location(s);
        *e = (Expr){
            .op = binop.op,
            .loc = { start_loc, end_loc },
            .bin_op = { result, rhs },
        };
        result = e;
    }

    return result;
}

// ternary
static Expr* parse_ternary(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_binop(parser, s, 0);

    if (tokens_get(s)->type == '?') {
        tokens_next(s);

        Expr* mhs = parse_expr_(parser, s);
        expect_char(s, ':');
        Expr* rhs = parse_ternary(parser, s);

        SourceLoc end_loc = tokens_get_last_location(s);
        Expr* e = alloc_expr();
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
static Expr* parse_assignment(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_ternary(parser, s);

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
        return lhs;
    }

    tokens_next(s);

    Expr* e = alloc_expr();
    Expr* rhs = parse_assignment(parser, s);

    SourceLoc end_loc = tokens_get_last_location(s);

    *e = (Expr){
        .op = op,
        .loc = { start_loc, end_loc },
        .bin_op = { lhs, rhs },
    };
    return e;

}

static Expr* parse_expr_(Cuik_Parser* restrict parser, TokenStream* restrict s) {
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

    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_assignment(parser, s);

    while (tokens_get(s)->type == TOKEN_COMMA) {
        Expr* e = alloc_expr();
        ExprOp op = EXPR_COMMA;
        tokens_next(s);

        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* rhs = parse_assignment(parser, s);
        *e = (Expr){
            .op = op,
            .loc = { start_loc, end_loc },
            .bin_op = { lhs, rhs },
        };

        lhs = e;
    }

    return lhs;
}

static intmax_t parse_const_expr2(Cuik_Parser* parser, TokenStream* restrict s) {
    Expr* folded = cuik__optimize_ast(parser->tu, parse_assignment(parser, s));
    if (folded->op != EXPR_INT) {
        diag_err(s, folded->loc, "could not parse expression as constant.");
        return 0;
    }

    return (intmax_t) folded->int_num.num;
}

