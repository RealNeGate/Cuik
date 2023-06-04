////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
// This file is included into parser.h, it's parser of the parser module and is
// completely static
static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_expr(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof);
static Expr* parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof);
static Expr* parse_assignment(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Expr* parse_initializer2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type);
static intmax_t parse_const_expr(Cuik_Parser* parser, TokenStream* restrict s);

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
        ON(PLUS,               PLUS); // this is a hack, it'll be treated as empty even tho it says EXPR_PLUS
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

static Expr* alloc_expr(Cuik_Parser* parser) {
    return ARENA_ALLOC(parser->arena, Expr);
}

static InitNode* make_init_node(Cuik_Parser* parser, TokenStream* restrict s, int mode) {
    InitNode* n = ARENA_ALLOC(parser->arena, InitNode);
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
        current->expr = parse_assignment(parser, s);
    }
    current->loc = (SourceRange){ loc, tokens_get_last_location(s) };

    return head;
}

static Expr* parse_initializer2(Cuik_Parser* parser, TokenStream* restrict s, Cuik_QualType type) {
    SourceLoc loc = tokens_get_location(s);
    expect_char(s, '{');

    InitNode *root = ARENA_ALLOC(parser->arena, InitNode), *tail = NULL;
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

    Expr* e = alloc_expr(parser);
    *e = (Expr){
        .op = EXPR_INITIALIZER,
        .loc = { loc, tokens_get_last_location(s) },
        .init = { type, root },
    };
    return e;
}

static void parse_string_literal(Cuik_Parser* parser, TokenStream* restrict s, Expr* e) {
    size_t saved_lexer_pos = s->list.current;
    size_t total_len = 0;
    while (!tokens_eof(s)) {
        Token* t = tokens_get(s);
        if (t->type == TOKEN_STRING_DOUBLE_QUOTE || t->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
            total_len += t->content.length - 2;
        } else if (memeq(t->content.data, t->content.length, "__func__", sizeof("__func__") - 1)) {
            if (cuik__sema_function_stmt) total_len += strlen(cuik__sema_function_stmt->decl.name);
            else total_len += 3; // "???"
        } else {
            break;
        }

        tokens_next(s);
    }

    size_t curr = 0;
    char* buffer = arena_alloc(&thread_arena, total_len + 3, 4);

    buffer[curr++] = '\"';

    // Fill up the buffer
    s->list.current = saved_lexer_pos;
    while (!tokens_eof(s)) {
        Token* t = tokens_get(s);
        if (t->type == TOKEN_STRING_DOUBLE_QUOTE || t->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE) {
            memcpy(&buffer[curr], t->content.data + 1, t->content.length - 2);
            curr += t->content.length - 2;
        } else if (memeq(t->content.data, t->content.length, "__func__", sizeof("__func__") - 1)) {
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
static Expr* parse_primary_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    if (t->type == '(') {
        SourceLoc start_loc = tokens_get_location(s);
        tokens_next(s);

        Expr* e = parse_expr(parser, s);
        expect_closing_paren(s, start_loc);

        e->has_parens = true;
        e->loc.start = start_loc;
        e->loc.end = tokens_get_last_location(s);
        return e;
    }

    Expr* e = alloc_expr(parser);
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
            } else if (!parser->is_in_global_scope && memeq(t->content.data, t->content.length, "__func__", sizeof("__func__") - 1)) {
                tokens_next(s);

                Atom name = cuik__sema_function_stmt->decl.name;
                *e = (Expr){
                    .op = EXPR_STR,
                    .str.start = (const unsigned char*) name,
                    .str.end = (const unsigned char*) &name[strlen(name)],
                };
                break;
            }

            Symbol* sym = find_symbol(parser, s);
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
                Token* t = tokens_get(s);
                Atom name = atoms_put(t->content.length, t->content.data);

                // check if it's builtin
                ptrdiff_t builtin_search = nl_map_get_cstr(parser->target->builtin_func_map, name);
                if (builtin_search >= 0) {
                    *e = (Expr){
                        .op = EXPR_BUILTIN_SYMBOL,
                        .builtin_sym = { name },
                    };
                } else {
                    diag_unresolved_symbol(parser, name, start_loc);

                    *e = (Expr){
                        .op = EXPR_UNKNOWN_SYMBOL,
                        .unknown_sym = name,
                    };
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

        case TOKEN_KW_Embed: {
            tokens_next(s);

            SourceLoc opening_loc = tokens_get_location(s);
            expect_char(s, '(');

            String content = tokens_get(s)->content;
            tokens_next(s);

            Cuik_QualType char_type = cuik_uncanonical_type(&parser->target->signed_ints[CUIK_BUILTIN_CHAR]);
            *e = (Expr){
                .op = EXPR_STR,
                .type = cuik_uncanonical_type(cuik__new_array(&parser->types, char_type, content.length)),
                .has_visited = true,
                .str = {
                    content.data,
                    content.data + content.length,
                }
            };

            expect_closing_paren(s, opening_loc);
            tokens_prev(s);
            break;
        }

        case TOKEN_STRING_DOUBLE_QUOTE:
        case TOKEN_STRING_WIDE_DOUBLE_QUOTE: {
            bool is_wide = (tokens_get(s)->type == TOKEN_STRING_WIDE_DOUBLE_QUOTE);
            *e = (Expr){
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
            Expr* controlling_expr = parse_assignment(parser, s);

            *e = (Expr){
                .op = EXPR_GENERIC,
                .generic_ = {.controlling_expr = controlling_expr},
            };
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

static Expr* parse_postfix(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
    SourceLoc start_loc = tokens_get_location(s);
    Expr* e = NULL;

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
                // resolve as sizeof (T)
                SourceLoc end_loc = tokens_get_last_location(s);
                Expr* e = alloc_expr(parser);
                *e = (Expr){
                    .op = EXPR_SIZEOF_T,
                    .loc = { start_loc, end_loc },
                    .x_of_type = { type },
                };
                return e;
            } else {
                s->list.current = fallback;
                goto normal_path;
            }
        }

        e = parse_initializer2(parser, s, type);
    }

    normal_path:
    start_loc = tokens_get_location(s);

    bool use_constructor = false;
    if (e == NULL) {
        if (parser->version == CUIK_VERSION_GLSL && is_typename(parser, s)) {
            Cuik_Type* type = parse_glsl_type(parser, s);
            SourceLoc end_loc = tokens_get_last_location(s);

            e = alloc_expr(parser);
            *e = (Expr){
                .op = EXPR_CONSTRUCTOR,
                .loc = { start_loc, end_loc },
                .constructor = { type },
            };

            if (tokens_get(s)->type != '(') {
                diag_err(s, e->loc, "Expected parenthesis after constructor name");
            }
        } else {
            e = parse_primary_expr(parser, s);
        }
    }

    // after any of the: [] () . ->
    // it'll restart and take a shot at matching another
    // piece of the expression.
    try_again: {
        if (tokens_get(s)->type == '[') {
            Expr* base = e;
            e = alloc_expr(parser);

            tokens_next(s);
            Expr* index = parse_expr(parser, s);
            expect_char(s, ']');

            SourceLoc end_loc = tokens_get_last_location(s);
            *e = (Expr){
                .op = EXPR_SUBSCRIPT,
                .loc = { start_loc, end_loc },
                .subscript = { base, index },
            };

            if (use_constructor) {
                diag_err(s, e->loc, "Cannot get element of type");
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

            Expr* base = e;
            e = alloc_expr(parser);
            *e = (Expr){
                .op = EXPR_ARROW,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .base = base, .name = name },
            };

            if (use_constructor) {
                diag_err(s, e->loc, "Cannot get member of type");
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
            Expr* base = e;
            e = alloc_expr(parser);
            *e = (Expr){
                .op = EXPR_DOT,
                .loc = { start_loc, end_loc },
                .dot_arrow = { .base = base, .name = name },
            };

            if (use_constructor) {
                diag_err(s, e->loc, "Cannot get member of type");
            }
            goto try_again;
        }

        // Function call
        if (tokens_get(s)->type == '(') {
            SourceLoc open_loc = tokens_get_location(s);
            tokens_next(s);

            Expr* target = e;
            e = alloc_expr(parser);

            size_t param_count = 0;
            void* params = tls_save();

            while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                if (param_count) {
                    if (tokens_get(s)->type != ',') {
                        break;
                    }

                    tokens_next(s);
                }

                Expr* e = parse_assignment(parser, s);
                *((Expr**)tls_push(sizeof(Expr*))) = e;
                param_count++;
            }

            expect_closing_paren(s, open_loc);
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
            e = alloc_expr(parser);
            *e = (Expr){
                .op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
                .loc = { start_loc, end_loc },
                .unary_op.src = src,
            };

            if (use_constructor) {
                diag_err(s, e->loc, "Cannot increment or decrement type");
            }
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
static Expr* parse_unary(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
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

        Expr* e = alloc_expr(parser);
        *e = (Expr){
            .op = EXPR_ALIGNOF_T,
            .loc = { start_loc, end_loc },
            .x_of_type = { type },
        };
        return e;
    } else if (tkn == TOKEN_KW_sizeof) {
        tokens_next(s);
        assert(!parser->is_in_global_scope && "cannot resolve is_typename in global scope");

        Expr* src = parse_unary(parser, s, true);
        if (src->op != EXPR_SIZEOF_T) {
            // convert expression into sizeof content
            SourceLoc end_loc = tokens_get_last_location(s);
            Expr* e = alloc_expr(parser);
            *e = (Expr){
                .op = EXPR_SIZEOF,
                .loc = src->loc,
                .x_of_expr = { src },
            };
            return e;
        } else {
            return src;
        }
    } else {
        ExprOp op = get_unary(tkn);

        if (op != EXPR_NONE) {
            tokens_next(s);
            Expr* value = parse_cast(parser, s, in_sizeof);
            if (op == EXPR_PLUS) return value;

            SourceLoc end_loc = tokens_get_last_location(s);
            Expr* e = alloc_expr(parser);

            *e = (Expr){
                .op = op,
                .loc = { start_loc, end_loc },
                .unary_op.src = value
            };
            return e;
        } else {
            // either
            return parse_postfix(parser, s, in_sizeof);
        }
    }
}

static Expr* parse_cast(Cuik_Parser* restrict parser, TokenStream* restrict s, bool in_sizeof) {
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
                Expr* e = alloc_expr(parser);
                *e = (Expr){
                    .op = EXPR_SIZEOF_T,
                    .loc = { start_loc, end_loc },
                    .x_of_type = { type },
                };
                return e;
            } else {
                // this is an initializer list not a normal cast
                s->list.current = fallback;
                goto normal_path;
            }
        }

        Expr* base = parse_cast(parser, s, false);
        SourceLoc end_loc = tokens_get_last_location(s);

        Expr* e = alloc_expr(parser);
        *e = (Expr){
            .op = EXPR_CAST,
            .loc = { start_loc, start_loc },
            .cast = { base, type },
        };
        return e;
    }

    normal_path:
    return parse_unary(parser, s, false);
}

static Expr* parse_binop(Cuik_Parser* restrict parser, TokenStream* restrict s, int min_prec) {
    // This precendence climber is always left associative
    SourceLoc start_loc = tokens_get_location(s);
    Expr* result = parse_cast(parser, s, false);

    ExprInfo binop;
    while (binop = get_binop(tokens_get(s)->type), binop.prec != 0 && binop.prec >= min_prec) {
        tokens_next(s);

        Expr* e = alloc_expr(parser);
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

        Expr* mhs = parse_expr(parser, s);
        expect_char(s, ':');
        Expr* rhs = parse_ternary(parser, s);

        SourceLoc end_loc = tokens_get_last_location(s);
        Expr* e = alloc_expr(parser);
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

    Expr* e = alloc_expr(parser);
    Expr* rhs = parse_assignment(parser, s);

    SourceLoc end_loc = tokens_get_last_location(s);

    *e = (Expr){
        .op = op,
        .loc = { start_loc, end_loc },
        .bin_op = { lhs, rhs },
    };
    return e;
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

static Expr* parse_expr(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    parse_pragma_expr(parser, s);

    SourceLoc start_loc = tokens_get_location(s);
    Expr* lhs = parse_assignment(parser, s);

    while (tokens_get(s)->type == TOKEN_COMMA) {
        Expr* e = alloc_expr(parser);
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

    parse_pragma_expr(parser, s);
    return lhs;
}

static intmax_t parse_const_expr(Cuik_Parser* parser, TokenStream* restrict s) {
    Expr* folded = cuik__optimize_ast(parser, parser->tu, parse_assignment(parser, s));
    if (folded->op != EXPR_INT) {
        diag_err(s, folded->loc, "could not parse expression as constant.");
        return 0;
    }

    return (intmax_t) folded->int_num.num;
}
