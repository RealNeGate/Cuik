static Decl parse_declarator2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);
static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);

// [ ASSIGNMENT-EXPR ]
// ( ASSIGNMENT-EXPR )
//
// returns the lbrace
static ptrdiff_t skip_expression_in_braces(TokenStream* s, char open, char close) {
    // in the out of order case we defer expression parsing
    SourceLoc open_brace = tokens_get_location(s);
    tokens_next(s);

    size_t current = s->list.current;
    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            diag_err(s, get_token_range(t), "expression never terminated");
            return -1;
        } else if (t->type == open) {
            depth++;
        } else if (t->type == close) {
            if (depth == 0) {
                report_two_spots(REPORT_ERROR, s, open_brace, t->location, "unbalanced braces", "open", "close?", NULL);
                return -1;
            }

            depth--;
        }

        tokens_next(s);
    }

    tokens_prev(s);
    if (!expect_char(s, close)) return -1;
    return current;
}

// either ends with a comma or semicolon
static ptrdiff_t skip_expression_in_list(TokenStream* restrict s, SourceRange error_loc, ptrdiff_t* out_end) {
    // '=' EXPRESSION ','
    // '=' EXPRESSION ';'
    ptrdiff_t current = s->list.current;

    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            diag_err(s, error_loc, "Declaration was never closed");

            // restore the token stream
            s->list.current = current + 1;
            return -1;
        } else if (t->type == '(') {
            depth++;
        } else if (t->type == ')') {
            depth--;

            if (depth == 0) {
                diag_err(s, error_loc, "Unbalanced parenthesis");

                s->list.current = current + 1;
                return -1;
            }
        } else if (t->type == ';' || t->type == ',') {
            if (depth > 1 && t->type == ';') {
                diag_err(s, error_loc, "Declaration's expression has a weird semicolon");
                return -1;
            } else if (depth == 1) {
                depth--;
                break;
            }
        }

        tokens_next(s);
    }

    *out_end = s->list.current - 1;
    return current;
}

static ptrdiff_t skip_brackets(TokenStream* restrict s, SourceRange error_loc, bool no_semicolons, ptrdiff_t* out_end) {
    ptrdiff_t current = s->list.current;
    tokens_next(s);

    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            diag_err(s, error_loc, "brackets ended in EOF");

            // restore the token stream
            s->list.current = current + 1;
            return -1;
        } else if (t->type == '{') {
            depth++;
        } else if (t->type == ';' && depth == 1 && no_semicolons) {
            diag_err(s, tokens_get_range(s), "Spurious semicolon");
            return -1;
        } else if (t->type == '}') {
            if (depth == 0) {
                diag_err(s, error_loc, "Unbalanced brackets");
                return -1;
            }

            depth--;
        }

        tokens_next(s);
    }

    *out_end = s->list.current - 1;
    return current;
}

static Cuik_QualType parse_declspec2(Cuik_Parser* restrict parser, TokenStream* restrict s, Attribs* attr) {
    enum {
        VOID     = 1 << 0,
        BOOL     = 1 << 2,
        CHAR     = 1 << 4,
        SHORT    = 1 << 6,
        INT      = 1 << 8,
        LONG     = 1 << 10,
        FLOAT    = 1 << 12,
        DOUBLE   = 1 << 14,
        OTHER    = 1 << 16,
        SIGNED   = 1 << 17,
        UNSIGNED = 1 << 18,
    };

    const Cuik_Type* target_signed_ints = parser->target->signed_ints;
    const Cuik_Type* target_unsigned_ints = parser->target->unsigned_ints;

    int counter = 0;
    Cuik_Qualifiers quals = 0;
    const Cuik_Type* type = NULL;

    // _Alignas(N) or __declspec(align(N))
    // 0 means no forced alignment
    int forced_align = 0;
    PendingExpr* alignas_pending_expr = NULL;

    SourceRange loc = tokens_get_range(s);
    do {
        TknType tkn_type = tokens_get(s)->type;
        switch (tkn_type) {
            // type-specifier:
            case TOKEN_KW_void:   counter += VOID;  break;
            case TOKEN_KW_Bool:   counter += BOOL;  break;
            case TOKEN_KW_char:   counter += CHAR;  break;
            case TOKEN_KW_short:  counter += SHORT; break;
            case TOKEN_KW_int:    counter += INT;   break;
            case TOKEN_KW_long:   counter += LONG;  break;
            case TOKEN_KW_float:  counter += FLOAT; break;
            case TOKEN_KW_double: counter += DOUBLE;break;

            case TOKEN_KW_Complex: case TOKEN_KW_Imaginary:
            diag_err(s, loc, "Complex types are not supported in CuikC");
            break;

            case TOKEN_KW_unsigned: counter |= UNSIGNED; break;
            case TOKEN_KW_signed:   counter |= SIGNED;   break;

            // storage-class-specifier:
            case TOKEN_KW_Thread_local: attr->is_tls     = true; break;
            case TOKEN_KW_typedef:      attr->is_typedef = true; break;
            case TOKEN_KW_extern:       attr->is_extern  = true; break;
            case TOKEN_KW_static:       attr->is_static  = true; break;
            case TOKEN_KW_inline:       attr->is_inline  = true; break;
            case TOKEN_KW_auto:         /* lmao */               break;
            case TOKEN_KW_register:     /* lmao */               break;
            case TOKEN_KW_Noreturn:     /* lmao */               break;

            // Qualifiers
            case TOKEN_KW_const:    quals |= CUIK_QUAL_CONST;    break;
            case TOKEN_KW_volatile: quals |= CUIK_QUAL_VOLATILE; break;

            // TODO(NeGate): implement these eventually
            // currently i don't need them since the ABIs are the same on my platform (x64 windows)
            case TOKEN_KW_cdecl:   break;
            case TOKEN_KW_stdcall: break;

            case TOKEN_KW_Atomic: {
                tokens_next(s);
                if (tokens_get(s)->type == '(') {
                    SourceLoc opening_loc = tokens_get_location(s);
                    tokens_next(s);

                    Cuik_QualType t = parse_typename2(parser, s);
                    type = cuik_canonical_type(t);
                    quals |= cuik_get_quals(t);
                    quals |= CUIK_QUAL_ATOMIC;
                    counter += OTHER;

                    if (!expect_closing_paren(s, opening_loc)) return CUIK_QUAL_TYPE_NULL;
                } else {
                    // walk back, we didn't need to read that
                    quals |= CUIK_QUAL_ATOMIC;
                    tokens_prev(s);
                }
                break;
            }

            case TOKEN_KW_Typeof: {
                tokens_next(s);

                SourceLoc opening_loc = tokens_get_location(s);
                if (!expect_char(s, '(')) return CUIK_QUAL_TYPE_NULL;

                tokens_next(s);
                if (parser->is_in_global_scope) {
                    // _Typeof ( SOMETHING )
                    TknType terminator;
                    ptrdiff_t start = skip_expression_in_braces(s, '(', ')');
                    ptrdiff_t end = s->list.current;
                    if (start < 0) {
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    // Add to pending list
                    printf("MSG: Add _Typeof to pending list %zu\n", start);
                    __debugbreak();
                } else {
                    if (is_typename(&parser->globals, s)) {
                        type = cuik_canonical_type(parse_typename2(parser, s));
                    } else {
                        // we don't particularly resolve typeof for expressions immediately.
                        // instead we just wait until all symbols are resolved properly
                        Expr* src = NULL; // parse_expr(tu, s);
                        type = cuik__new_typeof(&parser->types, src);
                    }

                    if (!expect_closing_paren(s, opening_loc)) {
                        return CUIK_QUAL_TYPE_NULL;
                    }
                }
                break;
            }

            default: goto done;
        }

        switch (counter) {
            case 0: break; // not resolved yet
            case VOID: type = &cuik__builtin_void; break;
            case BOOL: type = &cuik__builtin_bool; break;

            case CHAR: case SIGNED + CHAR:
            type = &target_signed_ints[CUIK_BUILTIN_CHAR];
            break;

            case UNSIGNED + CHAR:
            type = &target_unsigned_ints[CUIK_BUILTIN_CHAR];
            break;

            case SHORT: case SHORT + INT: case SIGNED + SHORT: case SIGNED + SHORT + INT:
            type = &target_signed_ints[CUIK_BUILTIN_SHORT];
            break;

            case UNSIGNED + SHORT: case UNSIGNED + SHORT + INT:
            type = &target_unsigned_ints[CUIK_BUILTIN_SHORT];
            break;

            case INT: case SIGNED: case SIGNED + INT:
            type = &target_signed_ints[CUIK_BUILTIN_INT];
            break;

            case LONG: case LONG + INT: case SIGNED + LONG: case SIGNED + LONG + INT:
            type = &target_signed_ints[CUIK_BUILTIN_LONG];
            break;

            case UNSIGNED: case UNSIGNED + INT:
            type = &target_unsigned_ints[CUIK_BUILTIN_INT];
            break;

            case UNSIGNED + LONG: case UNSIGNED + LONG + INT:
            type = &target_unsigned_ints[CUIK_BUILTIN_LONG];
            break;

            case LONG + LONG: case LONG + LONG + INT: case SIGNED + LONG + LONG: case SIGNED + LONG + LONG + INT:
            type = &target_signed_ints[CUIK_BUILTIN_LLONG];
            break;

            case UNSIGNED + LONG + LONG: case UNSIGNED + LONG + LONG + INT:
            type = &target_unsigned_ints[CUIK_BUILTIN_LLONG];
            break;

            case FLOAT:
            type = &builtin_types[TYPE_FLOAT];
            break;

            case DOUBLE: case LONG + DOUBLE:
            type = &builtin_types[TYPE_DOUBLE];
            break;

            case OTHER:
            assert(type);
            break;

            default: {
                Token* last = &s->list.tokens[s->list.current];
                diag_err(s, (SourceRange){ loc.start, tokens_get_last_location(s) }, "unknown typename %!S", last->content);
                tokens_next(s);
                return CUIK_QUAL_TYPE_NULL;
            }
        }

        tokens_next(s);
    } while (true);

    done:
    loc = (SourceRange){ loc.start, tokens_get_last_location(s) };
    if (type == 0) {
        Token* last = &s->list.tokens[s->list.current];
        diag_err(s, loc, "unknown typename %!S", last->content);
        return CUIK_QUAL_TYPE_NULL;
    }

    if (forced_align && type->align != forced_align) {
        if (forced_align < type->align) {
            diag_err(s, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, type->align);
            return CUIK_QUAL_TYPE_NULL;
        }

        // clone it since we need to modify it
        Cuik_Type* new_type = cuik__new_blank_type(&parser->types);
        *new_type = *type;
        new_type->loc = loc;

        if (forced_align) {
            new_type->align = forced_align;
        } else if (alignas_pending_expr != NULL) {
            new_type->align = -1;
            alignas_pending_expr->dst = &new_type->align;
        }
        type = new_type;
    }

    return cuik_make_qual_type((Cuik_Type*) type, quals);
}

static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    // TODO(NeGate): Check if attributes are set, they shouldn't
    // be in this context.
    Attribs attr = { 0 };
    Cuik_QualType type = parse_declspec2(parser, s, &attr);
    return parse_declarator2(parser, s, type, true).type;
}

static Cuik_QualType parse_type_suffix2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type) {
    Token* t = tokens_get(s);
    if (t->type == '(') {
        // function call
        // void foo(int x)
        //         ^^^^^^^
        SourceLoc opening_loc = tokens_get_location(s);
        tokens_next(s);

        Cuik_Type* t = cuik__new_func(&parser->types);
        t->func.return_type = type;

        if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
            // this is required pre-C23 to say no parameters (empty parens meant undefined)
            tokens_next(s);
            tokens_next(s);

            t->func.param_list = 0;
            t->func.param_count = 0;
        } else {
            size_t param_count = 0;
            Param* params = tls_save();
            bool has_varargs = false;

            while (tokens_get(s)->type && tokens_get(s)->type != ')') {
                if (param_count) {
                    if (tokens_get(s)->type != ',') {
                        diag_err(s, tokens_get_range(s), "expected closing paren (or comma) after declaration name");
                    } else {
                        tokens_next(s);
                    }
                }

                if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                    tokens_next(s);
                    has_varargs = true;
                    break;
                }

                Attribs param_attr = { 0 };
                Cuik_QualType param_base_type = parse_declspec2(parser, s, &param_attr);
                if (CUIK_QUAL_TYPE_IS_NULL(param_base_type)) {
                    param_base_type = cuik_uncanonical_type(parser->default_int);
                    tokens_next(s);
                }

                Decl param_decl = parse_declarator2(parser, s, param_base_type, false);
                Cuik_QualType param_type = param_decl.type;

                // Handle parameter sugar
                Cuik_Type* param_type_canon = cuik_canonical_type(param_type);
                if (param_type_canon->kind == KIND_ARRAY) {
                    // Array parameters are desugared into pointers
                    param_type = cuik_uncanonical_type(cuik__new_pointer(&parser->types, param_type_canon->array_of));
                } else if (param_type_canon->kind == KIND_FUNC) {
                    // Function parameters are desugared into pointers
                    param_type = cuik_uncanonical_type(cuik__new_pointer(&parser->types, param_type));
                }

                // TODO(NeGate): Error check that no attribs are set
                tls_push(sizeof(Param));
                params[param_count++] = (Param){
                    .type = param_type,
                    .name = param_decl.name
                };
            }
            expect_closing_paren(s, opening_loc);

            // Allocate some more permanent storage
            Param* permanent_store = arena_alloc(&local_ast_arena, param_count * sizeof(Param), _Alignof(Param));
            memcpy(permanent_store, params, param_count * sizeof(Param));

            t->func.param_list = permanent_store;
            t->func.param_count = param_count;

            // Before C23 empty parameter lists mean undefined set of parameters
            // we're gonna stick with that for now...
            if (parser->version < CUIK_VERSION_C23) {
                t->func.has_varargs = (param_count == 0 || has_varargs);
            } else {
                t->func.has_varargs = has_varargs;
            }

            tls_restore(params);
        }

        type = cuik_uncanonical_type(t);
    } else if (t->type == '[') {
        // array
        // int bar[8 * 8]
        //        ^^^^^^^
        SourceLoc open_brace = tokens_get_location(s);
        tokens_next(s);

        Cuik_Type* t = NULL;
        if (parser->is_in_global_scope) {
            size_t current = 0;
            if (tokens_get(s)->type == ']') {
                tokens_next(s);
            } else if (tokens_get(s)->type == '*') {
                tokens_next(s);
                expect_char(s, ']');
            } else {
                current = skip_expression_in_braces(s, '[', ']');
            }

            // create placeholder array type
            t = cuik__new_array(&parser->types, parse_type_suffix2(parser, s, type), 0);
            t->loc = (SourceRange){ open_brace, tokens_get_last_location(s) };
            t->array_count_lexer_pos = current;
        } else {
            __debugbreak();
        }
        type = cuik_uncanonical_type(t);
    }

    return type;
}

// declarator:
//   pointerOPT direct-declarator
static Decl parse_declarator2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract) {
    assert(!CUIK_QUAL_TYPE_IS_NULL(type));
    SourceLoc start_loc = tokens_get_location(s);

    // pointer:
    //   * type-qualifier-listOPT
    //   * type-qualifier-listOPT pointer
    for (;;) {
        type = parse_ptr_qualifiers(s, type);

        if (tokens_get(s)->type == '*') {
            tokens_next(s);
            type = cuik_uncanonical_type(cuik__new_pointer(&parser->types, type));
        } else {
            break;
        }
    }

    // TODO(NeGate): implement proper MSVC declspec support
    skip_over_declspec(s);

    // direct-declarator:
    //   identifier
    //   ( declarator )
    //   direct-declarator [ type-qualifier-listopt assignment-expressionOPT ]
    //   direct-declarator [ static type-qualifier-listOPT assignment-expression ]
    //   direct-declarator [ type-qualifier-list static assignment-expression ]
    //   direct-declarator [ type-qualifier-listOPT * ]
    //   direct-declarator ( parameter-type-list )
    //   direct-declarator ( identifier-listOPT )
    Atom name = NULL;
    Token* t = tokens_get(s);

    // non-negative if there's a nested declarator
    ptrdiff_t nested_start = -1, nested_end = -1;
    if (!is_abstract && t->type == TOKEN_IDENTIFIER) {
        // simple name
        name = atoms_put(t->content.length, t->content.data);
        tokens_next(s);
    } else if (t->type == '(') {
        // int (*name)(void);
        //     ^^^^^^^
        //     S     E
        //
        // we skip over the nested declarator until the end of this function
        // in which case we return (this is after parsing the suffix) and we
        // apply the nested declarator.
        SourceLoc opening_loc = tokens_get_location(s);
        tokens_next(s);
        nested_start = s->list.current;

        // we pass a dummy type so we can skip over it
        parse_declarator2(parser, s, cuik_uncanonical_type(&builtin_types[TYPE_VOID]), is_abstract);
        expect_closing_paren(s, opening_loc);
    }

    // Handle suffixes like [] or ()
    type = parse_type_suffix2(parser, s, type);
    nested_end = s->list.current;

    if (nested_start >= 0) {
        assert(nested_end >= 0);
        s->list.current = nested_start;

        Decl nest = parse_declarator2(parser, s, type, is_abstract);
        name = nest.name;
        type = nest.type;

        s->list.current = nested_end;
    }

    // disambiguate
    #if 0
    bool is_nested_declarator = tokens_get(s)->type == '(';
    if (!out_of_order_mode && is_nested_declarator && is_abstract) {
        tokens_next(s);

        if (is_typename(&tu->globals, s)) {
            is_nested_declarator = false;
        }

        tokens_prev(s);
    }
    #endif

    SourceLoc end_loc = tokens_get_last_location(s);
    return (Decl){ type, name, { start_loc, end_loc } };
}
