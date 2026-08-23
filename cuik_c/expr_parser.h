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
        *parser->expr = (Cuik_Expr){
            .first_symbol = -1,
            .exprs = aarray_create(parser->arena, Subexpr, 4),
        };
    }

    return aarray_grab(parser->expr->exprs);
}

static Atom as_atom(TokenStream* restrict s, const char* failure_reason) {
    Token* t = tokens_get(s);
    if (t->type != TOKEN_IDENTIFIER) {
        assert(failure_reason != NULL);
        diag_err(s, tokens_get_range(s), "Expected identifier for %s", failure_reason);
        return NULL;
    }

    tokens_next(s);
    return atoms_put(t->content.length, t->content.data);
}

static _Thread_local Atom C_va_arg;
static _Thread_local Atom C_func;

static bool atom_eq_cached(Atom src, Atom* cache, const char* exp) {
    if (*cache == NULL) {
        *cache = atoms_putc(exp);
    }
    return src == *cache;
}

// younger me was actually more scared of macros for the sake of shrinking
// code like this, i got old
#define E_PUSH(op, ...) push_expr2(parser, s, loc, op, (Subexpr){ __VA_ARGS__ })
#define E_ERR(...)      diag_err(s, aarray_top(parser->expr->exprs).loc, __VA_ARGS__)
static SourceRange push_expr2(Cuik_Parser* parser, TokenStream* restrict s, SourceLoc loc, ExprOp op, Subexpr se) {
    if (parser->expr == NULL) {
        parser->expr = TB_ARENA_ALLOC(parser->arena, Cuik_Expr);
        *parser->expr = (Cuik_Expr){
            .first_symbol = -1,
            .exprs = aarray_create(parser->arena, Subexpr, 4),
        };
    }

    // by the time we call this we've just advanced past whatever token
    assert(s->list.current >= 2);
    SourceLoc end_loc = get_end_location(&s->list.tokens[s->list.current - 2]);

    se.op  = op;
    se.loc = (SourceRange){ loc, end_loc };
    aarray_push(parser->expr->exprs, se);
    return se.loc;
}

static Subexpr* peek_expr(Cuik_Parser* parser) {
    assert(parser->expr != NULL);
    return &aarray_top(parser->expr->exprs);
}

static Cuik_Expr* complete_expr(Cuik_Parser* parser) {
    Cuik_Expr* e = parser->expr;
    if (e->first_symbol >= 0) {
        parser->expr->next_in_chain = symbol_chain_start;
        symbol_chain_start = parser->expr;
    }
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
        Cuik_Expr* hide = parser->expr;
        parser->expr = NULL;

        // parse without comma operator
        parse_assignment(parser, s);
        current->expr = complete_expr(parser);

        parser->expr = hide;
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
    E_PUSH(EXPR_INITIALIZER, .init = { type, root });
}

static Atom parse_string_literal(Cuik_Parser* parser, TokenStream* restrict s, bool is_wide) {
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
    unsigned char* buffer = tb_arena_alloc(parser->arena, total_len + 1);

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

    size_t out_i = 0, in_i = 0;
    if (is_wide) {
        wchar_t* out = tb_arena_alloc(parser->arena, total_len + 1);
        while (in_i < curr) {
            int ch;
            ptrdiff_t distance = parse_char(curr - in_i, (const char*) &buffer[in_i], &ch);
            if (distance <= 0) {
                abort(); // TODO: Error message
            }

            out[out_i++] = ch;
            in_i += distance;
        }
        return atoms_put(out_i*sizeof(wchar_t), (const unsigned char*) out);
    } else {
        // Since the input stream will always be ahead of the output stream, we can use the
        // same array and just mutate it
        while (in_i < curr) {
            int ch;
            ptrdiff_t distance = parse_char(curr - in_i, (const char*) &buffer[in_i], &ch);
            if (distance <= 0) {
                abort(); // TODO: Error message
            }

            buffer[out_i++] = ch;
            in_i += distance;
        }
        return atoms_put(out_i, buffer);
    }
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

        if (tokens_get(s)->type == '{') {
            diag_err(s, get_token_range(t), "GNU statement expressions are unsupported rn");
        } else {
            parse_expr(parser, s);
            expect_closing_paren(s, start_loc);

            Subexpr* e = peek_expr(parser);
            e->has_parens = true;
            e->loc.start = start_loc;
            e->loc.end = tokens_get_last_location(s);
        }
        return;
    }

    SourceLoc loc = tokens_get_location(s);
    switch (t->type) {
        case TOKEN_IDENTIFIER: {
            Atom name = as_atom(s, NULL);
            if (atom_eq_cached(name, &C_va_arg, "__va_arg")) {
                expect_char(s, '(');
                parse_assignment(parser, s);
                expect_char(s, ',');
                Cuik_QualType type = parse_typename2(parser, s);
                expect_char(s, ')');

                E_PUSH(EXPR_VA_ARG, .va_arg_ = { type });
                break;
            } else if (!parser->is_in_global_scope && atom_eq_cached(name, &C_func, "__func__")) {
                Atom name = cuik__sema_function_stmt->decl.name;
                E_PUSH(EXPR_STR, .str = name);
                break;
            }

            ptrdiff_t builtin_search = nl_map_get_cstr(parser->target->builtin_func_map, name);
            if (builtin_search >= 0) {
                E_PUSH(EXPR_BUILTIN_SYMBOL, .builtin_sym = { name });
            } else {
                Symbol* sym = cuik_symtab_lookup(parser->symbols, name);
                if (sym != NULL) {
                    if (sym->storage_class == STORAGE_PARAM) {
                        E_PUSH(EXPR_PARAM, .param_num = sym->param_num);
                    } else if (sym->storage_class == STORAGE_ENUM) {
                        EnumEntry* entry = &cuik_canonical_type(sym->type)->enumerator.entries[sym->enum_value];
                        E_PUSH(EXPR_ENUM, .enum_val = { sym->type, entry });
                    } else {
                        assert(sym->stmt != NULL);
                        E_PUSH(EXPR_SYMBOL, .sym = { sym->stmt });
                    }
                } else {
                    diag_unresolved_symbol(parser, name, loc);
                    E_PUSH(EXPR_UNKNOWN_SYMBOL, .unknown_sym = { name });
                }

                // only known symbols participate in the global collection phase,
                // and if it's an EXPR_SYMBOL then sym != NULL so we don't check
                // that here
                Subexpr* e = &aarray_top(parser->expr->exprs);
                if (e->op == EXPR_SYMBOL &&
                    sym->storage_class != STORAGE_PARAM &&
                    sym->storage_class != STORAGE_ENUM &&
                    sym->storage_class != STORAGE_TYPEDEF &&
                    sym->storage_class != STORAGE_LOCAL) {
                    // append to list inside of the expression
                    ptrdiff_t i = e - parser->expr->exprs;
                    e->sym.next_symbol = parser->expr->first_symbol;
                    parser->expr->first_symbol = i;
                }
            }
            break;
        }

        case TOKEN_FLOAT: {
            bool is_float32 = t->content.data[t->content.length - 1] == 'f';

            char* end;
            double f = strtod((const char*) t->content.data, &end);
            if (end != (const char*) &t->content.data[t->content.length]) {
                if (*end != 'l' && *end != 'L' && *end != 'f' && *end != 'd' && *end != 'F' && *end != 'D') {
                    diag_err(s, get_token_range(t), "invalid float literal");
                }
            }

            tokens_next(s);
            E_PUSH(is_float32 ? EXPR_FLOAT32 : EXPR_FLOAT64, .float_lit = f);
            break;
        }

        case TOKEN_INTEGER: {
            Cuik_IntSuffix suffix;
            uint64_t i = parse_int(t->content.length, (const char*) t->content.data, &suffix);

            if (i > UINT32_MAX) {
                suffix = INT_SUFFIX_LL;
            } else if (i > INT32_MAX) {
                suffix = INT_SUFFIX_U;
            }

            tokens_next(s);
            E_PUSH(EXPR_INT, .int_lit = { i, suffix });
            break;
        }

        case TOKEN_STRING_SINGLE_QUOTE:
        case TOKEN_STRING_WIDE_SINGLE_QUOTE: {
            int ch = 0;
            ptrdiff_t distance = parse_char(t->content.length - 2, (const char*) &t->content.data[1], &ch);
            if (distance < 0) {
                diag_err(s, get_token_range(t), "invalid character literal");
            }

            tokens_next(s);
            E_PUSH(t->type == TOKEN_STRING_SINGLE_QUOTE ? EXPR_CHAR : EXPR_WCHAR, .char_lit = ch);
            break;
        }

        case TOKEN_STRING_DOUBLE_QUOTE:
        case TOKEN_STRING_WIDE_DOUBLE_QUOTE: {
            bool is_wide = (tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE);
            Atom str = parse_string_literal(parser, s, is_wide);
            E_PUSH(is_wide ? EXPR_WSTR : EXPR_STR, .str = str);
            break;
        }

        case TOKEN_KW_Generic: {
            tokens_next(s);

            SourceLoc opening_loc = tokens_get_location(s);
            expect_char(s, '(');

            // controlling expression followed by a comma
            parse_assignment(parser, s);

            expect_char(s, ',');

            ArenaArray(C11GenericEntry) entries = aarray_create(parser->arena, C11GenericEntry, 4);
            SourceRange default_loc = { 0 };
            while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                Cuik_Expr* hide = parser->expr;
                parser->expr = NULL;

                Cuik_QualType type = CUIK_QUAL_TYPE_NULL;
                if (tokens_get(s)->type == TOKEN_KW_default) {
                    if (default_loc.start.raw != 0) {
                        diag_err(s, tokens_get_range(s), "multiple default cases on _Generic");
                        diag_note(s, default_loc, "see here");
                    }

                    default_loc = tokens_get_range(s);
                } else {
                    type = parse_typename2(parser, s);
                    assert(!CUIK_QUAL_TYPE_IS_NULL(type) && "TODO: error recovery");
                }
                expect_char(s, ':');

                parse_assignment(parser, s);
                Cuik_Expr* expr = complete_expr(parser);

                aarray_push(entries, (C11GenericEntry){ type, expr });
                parser->expr = hide;

                // exit if it's not a comma
                if (tokens_get(s)->type != ',') break;
                tokens_next(s);
            }
            E_PUSH(EXPR_GENERIC, .generic_ = { entries });

            expect_closing_paren(s, opening_loc);
            break;
        }

        default:
        diag_err(s, tokens_get_range(s), "could not parse expression");
        tokens_next(s);

        E_PUSH(EXPR_NONE);
        return;
    }
}

static void parse_postfix(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
    SourceLoc loc = tokens_get_location(s);
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
        expect_closing_paren(s, loc);

        if (tokens_get(s)->type != '{') {
            if (in_sizeof) {
                // HACKY but it does get us to the 'sizeof' as opposed to the paren
                loc = s->list.tokens[s->list.current - 4].location;
                // resolve as sizeof (T)
                E_PUSH(EXPR_SIZEOF_T, .x_of_type = { type });
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
    loc = tokens_get_location(s);

    bool use_constructor = false;
    if (!has_expr) {
        if (parser->version == CUIK_VERSION_GLSL && is_typename(parser, s)) {
            Cuik_Type* type   = parse_glsl_type(parser, s);
            SourceRange range = E_PUSH(EXPR_CONSTRUCTOR, .constructor = { type });

            if (tokens_get(s)->type != '(') {
                diag_err(s, range, "Expected parenthesis after constructor name");
            }
        } else {
            parse_primary_expr(parser, s);
        }
    }

    // [] () . -> ++ --
    for (;;) {
        SourceLoc op_loc = tokens_get_location(s);
        TknType type = tokens_post_inc(s)->type;
        switch (type) {
            case '[': {
                parse_expr(parser, s);
                expect_char(s, ']');

                E_PUSH(EXPR_SUBSCRIPT);
                if (use_constructor) {
                    E_ERR("Cannot get element of type");
                }
                break;
            }

            case '.':
            case TOKEN_ARROW: {
                ExprOp op = type == '.' ? EXPR_DOT : EXPR_ARROW;
                Atom name = as_atom(s, "member access a.b");
                E_PUSH(op, .dot_arrow = { .name = name });
                if (use_constructor) {
                    E_ERR("We can't know the base type for the member access since it's an untyped constructor");
                }
                break;
            }

            case '(': {
                int param_count = 0;
                while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                    if (param_count && !expect_char(s, ',')) {
                        break;
                    }
                    parse_assignment(parser, s);
                    param_count++;
                }
                expect_closing_paren(s, op_loc);
                E_PUSH(EXPR_CALL, .call = { param_count });
                break;
            }

            case TOKEN_INCREMENT:
            case TOKEN_DECREMENT: {
                E_PUSH(type == TOKEN_INCREMENT ? EXPR_POST_INC : EXPR_POST_DEC);
                if (use_constructor) {
                    E_ERR("Cannot increment or decrement an untyped constructor");
                }
                break;
            }

            default:
            tokens_prev(s);
            return;
        }
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
    ptrdiff_t start_i = parser->expr ? aarray_length(parser->expr->exprs) : 0;

    // This precendence climber is always left associative
    SourceLoc start_loc = tokens_get_location(s);
    parse_cast(parser, s, false);

    ExprInfo binop;
    while (binop = get_binop(tokens_get(s)->type), binop.prec != 0 && binop.prec >= min_prec) {
        tokens_next(s);

        if (binop.op == EXPR_LOGICAL_AND || binop.op == EXPR_LOGICAL_OR) {
            Cuik_Expr* hide = parser->expr;
            parser->expr = NULL;

            // complete expr between start_of_expr and now
            Cuik_Expr* left = NULL;
            {
                ptrdiff_t first_sym = -1;

                // if it's part of the left expression, move it out
                // of the hidden expression.
                ptrdiff_t sym = hide->first_symbol;
                if (sym >= start_i) first_sym = sym - start_i;

                while (sym >= start_i) {
                    ptrdiff_t next = hide->exprs[sym].sym.next_symbol;

                    hide->exprs[sym].sym.next_symbol = next - start_i;
                    sym = hide->first_symbol = next;
                }

                size_t count = aarray_length(hide->exprs) - start_i;
                ArenaArray(Subexpr) exprs = aarray_create(parser->arena, Subexpr, count);

                // migrate
                memcpy(&exprs[0], &hide->exprs[start_i], count * sizeof(Subexpr));
                aarray_set_length(hide->exprs, start_i);
                aarray_set_length(exprs, count);

                left  = TB_ARENA_ALLOC(parser->arena, Cuik_Expr);
                *left = (Cuik_Expr){ .exprs = exprs, .first_symbol = first_sym };

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

        // left expression
        parser->expr = NULL;
        parse_expr(parser, s);
        Cuik_Expr* left = complete_expr(parser);

        expect_char(s, ':');

        // right expression
        parser->expr = NULL;
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
    Cuik_Expr* old = parser->expr;
    parser->expr = NULL;

    parse_expr(parser, s);
    Cuik_Expr* e = complete_expr(parser);

    parser->expr = old;
    return e;
}

static intmax_t parse_const_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    Cuik_Expr* old = parser->expr;
    parser->expr = NULL;

    parse_assignment(parser, s);
    Cuik_Expr* e = complete_expr(parser);

    parser->expr = old;

    Cuik_ConstVal value;
    if (!const_eval(parser, &parser->tokens, e, &value)) {
        // the const_eval_int will handle errors
        return 0;
    }

    if (value.tag != CUIK_CONST_INT) {
        size_t count = aarray_length(e->exprs);
        diag_err(&parser->tokens, e->exprs[count - 1].loc, "Constant expression was not an integer");
        return 0;
    }

    return value.i;
}

#undef E_PUSH
#undef E_ERR
