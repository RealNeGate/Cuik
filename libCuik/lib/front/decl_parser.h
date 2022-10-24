////////////////////////////////
// TYPES
////////////////////////////////
static thread_local char temp_string0[1024];

static Cuik_Attribute* parse_attributes(TranslationUnit* restrict tu, TokenStream* restrict s, Cuik_Attribute* last) {
    for (;;) {
        if (tokens_peek_double_token(s, '[')) {
            // C23 attribute:
            //
            //   [[foo]]  [[foo::bar]]  [[foo(1, 3)]]
            Cuik_Attribute* a = ARENA_ALLOC(&local_ast_arena, Cuik_Attribute);
            a->prev = last;
            a->loc.start = tokens_get_location(s);

            // TODO(NeGate): we'll only handle the identifier case with no :: for now
            tokens_next(s), tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                REPORT(ERROR, tokens_get_location(s), "Expected an identifier");
            } else {
                Token* t = tokens_get(s);
                a->name = atoms_put(t->content.length, t->content.data);
                tokens_next(s);
            }
            a->loc.end = tokens_get_location(s);

            if (!tokens_peek_double_token(s, ']')) {
                REPORT(ERROR, tokens_get_location(s), "Expected closing ']]' for attribute");
            }
            tokens_next(s), tokens_next(s);

            last = a;
        } else if (tokens_get(s)->type == TOKEN_KW_attribute) {
            // TODO(NeGate): Correctly parse attributes instead of
            // ignoring them.
            tokens_next(s);
            expect(tu, s, '(');

            int depth = 1;
            while (depth) {
                if (tokens_get(s)->type == '(') {
                    depth++;
                } else if (tokens_get(s)->type == ')') {
                    depth--;
                }

                tokens_next(s);
            }
        } else {
            return last;
        }
    }
}

static bool skip_over_declspec(TranslationUnit* tu, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_declspec ||
        tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

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

        return true;
    }

    return false;
}

static Cuik_QualType parse_ptr_qualifiers(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type) {
    for (;;) {
        TknType t = tokens_get(s)->type;
        if (t == TOKEN_KW_Atomic) {
            type.raw |= CUIK_QUAL_ATOMIC;
            tokens_next(s);
        } else if (t == TOKEN_KW_restrict) {
            type.raw |= CUIK_QUAL_RESTRICT;
            tokens_next(s);
        } else if (t == TOKEN_KW_const) {
            type.raw |= CUIK_QUAL_CONST;
            tokens_next(s);
        } else if (t == TOKEN_KW_volatile) {
            type.raw |= CUIK_QUAL_CONST;
            tokens_next(s);
        } else if (t == TOKEN_KW_cdecl || t == TOKEN_KW_stdcall) {
            tokens_next(s);
        } else {
            break;
        }
    }

    return type;
}

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type, bool is_abstract, bool disabled_paren) {
    assert(!CUIK_QUAL_TYPE_IS_NULL(type));
    SourceLoc start_loc = tokens_get_location(s);

    // pointer:
    //   * type-qualifier-listOPT
    //   * type-qualifier-listOPT pointer
    for (;;) {
        type = parse_ptr_qualifiers(tu, s, cuik_uncanonical_type(new_pointer(tu, type)));

        if (tokens_get(s)->type != '*') break;
        tokens_next(s);
    }

    skip_over_declspec(tu, s);
    bool is_nested_declarator = tokens_get(s)->type == '(';

    // disambiguate
    if (!out_of_order_mode && is_nested_declarator && is_abstract) {
        tokens_next(s);

        if (is_typename(tu, s)) {
            is_nested_declarator = false;
        }

        tokens_prev(s);
    }

    if (is_nested_declarator) {
        // TODO(NeGate): I don't like this code...
        // it essentially just skips over the stuff in the
        // parenthesis to do the suffix then comes back
        // for the parenthesis after wards, restoring back to
        // the end of the declarator when it's done.
        //
        // should be right after the (
        SourceLoc opening_loc = tokens_get_location(s);

        tokens_next(s);
        size_t saved = s->list.current;

        // dummy_type just avoids problems where the type would be NULL and needs to be read
        // it's not gonna modify and rarely really reads from it
        Cuik_QualType dummy_type = cuik_uncanonical_type(&builtin_types[TYPE_VOID]);
        parse_declarator(tu, s, dummy_type, is_abstract, false);

        expect_closing_paren(tu, s, opening_loc);
        type = parse_type_suffix(tu, s, type, NULL);

        size_t saved_end = s->list.current;
        s->list.current = saved;

        Decl d = parse_declarator(tu, s, type, is_abstract, false);

        // inherit name
        // TODO(NeGate): I'm not sure if this is correct ngl
        if (!d.name) {
            Cuik_Type* t = cuik_canonical_type(d.type);

            if (t->kind == KIND_PTR) {
                t = cuik_canonical_type(cuik_canonical_type(d.type)->ptr_to);

                if (t->kind == KIND_FUNC) {
                    d.name = t->func.name;
                } else if (t->kind == KIND_STRUCT) {
                    d.name = t->record.name;
                } else if (t->kind == KIND_UNION) {
                    d.name = t->record.name;
                }
            }
        }

        s->list.current = saved_end;
        return d;
    }

    Atom name = NULL;
    Token* t = tokens_get(s);
    if (!is_abstract && t->type == TOKEN_IDENTIFIER) {
        name = atoms_put(t->content.length, t->content.data);
        tokens_next(s);
    }

    SourceLoc end_loc = tokens_get_last_location(s);
    type = parse_type_suffix(tu, s, type, name);
    return (Decl){ type, name, { start_loc, end_loc } };
}

static Cuik_QualType parse_typename(TranslationUnit* tu, TokenStream* restrict s) {
    // TODO(NeGate): Check if attributes are set, they shouldn't
    // be in this context.
    Attribs attr = {0};
    Cuik_QualType type = parse_declspec(tu, s, &attr);
    return parse_declarator(tu, s, type, true, false).type;
}

static Cuik_QualType parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type, Atom name) {
    assert(s->list.current > 0);
    SourceLoc loc = tokens_get_last_location(s);

    // type suffixes like array [] and function ()
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        Cuik_Type* t = new_func(tu);
        t->func.name = name;
        t->func.return_type = type;
        if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
            tokens_next(s);
            tokens_next(s);

            t->func.param_list = 0;
            t->func.param_count = 0;
            return cuik_uncanonical_type(t);
        }

        size_t param_count = 0;
        Param* params = tls_save();
        bool has_varargs = false;

        while (tokens_get(s)->type && tokens_get(s)->type != ')') {
            if (param_count) {
                if (tokens_get(s)->type != ',') {
                    diag_err(s, tokens_get_range(s), "expected closing paren (or comma) after declaration name");
                    abort();
                }

                tokens_next(s);
            }

            if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                tokens_next(s);

                has_varargs = true;
                break;
            }

            Attribs arg_attr = { 0 };
            Cuik_QualType arg_base_type = parse_declspec(tu, s, &arg_attr);
            if (CUIK_QUAL_TYPE_IS_NULL(arg_base_type)) {
                while (tokens_get(s)->type && tokens_get(s)->type != ')') {
                    tokens_next(s);
                }

                break;
            }

            Decl param_decl = parse_declarator(tu, s, arg_base_type, false, false);
            Cuik_QualType param_type = param_decl.type;

            Cuik_Type* param_type_canon = cuik_canonical_type(param_type);
            if (param_type_canon->kind == KIND_ARRAY) {
                // Array parameters are desugared into pointers
                param_type = cuik_uncanonical_type(new_pointer(tu, param_type_canon->array_of));
            } else if (param_type_canon->kind == KIND_FUNC) {
                // Function parameters are desugared into pointers
                param_type = cuik_uncanonical_type(new_pointer(tu, param_type));
            }

            // TODO(NeGate): Error check that no attribs are set
            tls_push(sizeof(Param));
            params[param_count++] = (Param){
                .type = param_type,
                .name = param_decl.name
            };
        }

        if (tokens_get(s)->type != ')') {
            generic_error(tu, s, "Unclosed parameter list!");
        }
        tokens_next(s);

        // Allocate some more permanent storage
        Param* permanent_store = arena_alloc(&tu->ast_arena, param_count * sizeof(Param), _Alignof(Param));
        memcpy(permanent_store, params, param_count * sizeof(Param));

        t->func.param_list = permanent_store;
        t->func.param_count = param_count;

        // Before C23 empty parameter lists mean undefined set of parameters
        // we're gonna stick with that for now...
        // TODO(NeGate): add a C version check for it
        t->func.has_varargs = param_count == 0 ? true : has_varargs;

        tls_restore(params);
        type = cuik_uncanonical_type(t);
    } else if (tokens_get(s)->type == '[') {
        if (out_of_order_mode) {
            // in the out of order case we defer expression parsing
            SourceLoc open_brace = tokens_get_location(s);
            tokens_next(s);

            size_t current = 0;
            if (tokens_get(s)->type == ']') {
                tokens_next(s);
            } else if (tokens_get(s)->type == '*') {
                tokens_next(s);
                expect(tu, s, ']');
            } else {
                current = s->list.current;

                int depth = 1;
                while (depth) {
                    Token* t = tokens_get(s);

                    if (t->type == '\0') {
                        REPORT(ERROR, t->location, "Array declaration ended in EOF");
                        abort();
                    } else if (t->type == '[') {
                        depth++;
                    } else if (t->type == ']') {
                        if (depth == 0) {
                            report_two_spots(REPORT_ERROR, tu->errors, s, open_brace, t->location,
                                "Unbalanced brackets", "open", "close?", NULL);
                            abort();
                        }

                        depth--;
                    }

                    tokens_next(s);
                }

                tokens_prev(s);
                expect(tu, s, ']');
            }

            Cuik_Type* t = new_array(tu, parse_type_suffix(tu, s, type, name), 0);
            // create placeholder array type
            t->loc = (SourceRange){ open_brace, tokens_get_last_location(s) };
            t->array_count_lexer_pos = current;
            type = cuik_uncanonical_type(t);
        } else {
            size_t depth = 0;
            size_t* counts = tls_save();

            // TODO(NeGate): read some array qualifiers
            do {
                tokens_next(s);

                long long count;
                if (tokens_get(s)->type == ']') {
                    count = 0;
                    tokens_next(s);
                } else if (tokens_get(s)->type == '*') {
                    count = 0;
                    tokens_next(s);
                    expect(tu, s, ']');
                } else {
                    count = parse_const_expr(tu, s);
                    expect(tu, s, ']');
                }

                tls_push(sizeof(size_t));
                counts[depth++] = count;
            } while (tokens_get(s)->type == '[');

            size_t expected_size = cuik_canonical_type(type)->size;
            while (depth--) {
                assert(cuik_canonical_type(type)->size == expected_size);

                uint64_t a = expected_size;
                uint64_t b = counts[depth];
                uint64_t result = a * b;

                // size checks
                if (result >= INT32_MAX) {
                    diag_err(s, cuik_canonical_type(type)->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
                    return CUIK_QUAL_TYPE_NULL;
                }

                type = cuik_uncanonical_type(new_array(tu, type, counts[depth]));
                expected_size = result;
            }

            tls_restore(counts);
        }
    }

    return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static Cuik_QualType parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr) {
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

    int counter = 0;
    Cuik_Qualifiers quals = 0;
    Cuik_Type* type = NULL;

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

            case TOKEN_KW_Complex:
            case TOKEN_KW_Imaginary:
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
            case TOKEN_KW_cdecl:   break;
            case TOKEN_KW_stdcall: break;

            case TOKEN_KW_Vector: {
                // _Vector '(' TYPENAME ',' CONST-EXPR ')'
                if (counter) goto done;
                tokens_next(s);

                SourceLoc opening_loc = tokens_get_location(s);
                expect(tu, s, '(');

                type = cuik_canonical_type(parse_typename(tu, s));
                if (!cuik_type_is_integer(type) && !cuik_type_is_float(type)) {
                    REPORT(ERROR, loc, "Only integers and floats can be used for _Vector types");
                    return CUIK_QUAL_TYPE_NULL;
                }

                expect(tu, s, ',');

                intmax_t count = parse_const_expr(tu, s);

                if (count <= 0) {
                    REPORT(ERROR, loc, "_Vector types must have a positive width");
                    return CUIK_QUAL_TYPE_NULL;
                }

                if (count == 1) {
                    REPORT(ERROR, loc, "It's not even a _Vector type... that's a scalar...");
                    return CUIK_QUAL_TYPE_NULL;
                }

                if (count > 64) {
                    REPORT(ERROR, loc, "_Vector type is too wide (%" PRIiMAX ", max is 64)", count);
                    return CUIK_QUAL_TYPE_NULL;
                }

                // only allow power of two widths
                if ((count & (count - 1)) != 0) {
                    REPORT(ERROR, loc, "_Vector types can only have power-of-two widths");
                    return CUIK_QUAL_TYPE_NULL;
                }

                type = new_vector(tu, cuik_uncanonical_type(type), count);
                counter += OTHER;

                expect_closing_paren(tu, s, opening_loc);
                tokens_prev(s);
                break;
            }
            case TOKEN_KW_Atomic: {
                tokens_next(s);
                if (tokens_get(s)->type == '(') {
                    SourceLoc opening_loc = tokens_get_location(s);
                    tokens_next(s);

                    Cuik_QualType t = parse_typename(tu, s);
                    type = cuik_canonical_type(t);
                    quals |= cuik_get_quals(t);
                    quals |= CUIK_QUAL_ATOMIC;

                    counter += OTHER;

                    SourceLoc closing_loc = tokens_get_location(s);
                    if (tokens_get(s)->type != ')') {
                        // report_two_spots(REPORT_ERROR, tu->errors, s, opening_loc, closing_loc, "expected closing parenthesis for _Atomic", "open", "close?", NULL);
                        __debugbreak();
                        tokens_next(s);
                        return CUIK_QUAL_TYPE_NULL;
                    }
                } else {
                    // walk back, we didn't need to read that
                    quals |= CUIK_QUAL_ATOMIC;
                    tokens_prev(s);
                }
                break;
            }

            case TOKEN_KW_Typeof: {
                tokens_next(s);
                if (tokens_get(s)->type != '(') {
                    REPORT(ERROR, loc, "expected opening parenthesis for _Typeof");
                    return CUIK_QUAL_TYPE_NULL;
                }
                tokens_next(s);

                if (out_of_order_mode) {
                    // _Typeof ( SOMETHING )
                    TknType terminator;
                    size_t current = skip_expression_in_parens(s, &terminator);

                    if (terminator == 0 || terminator == ',') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Typeof%s", terminator ? " (got EOF)" : "");
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    // Add to pending list
                    printf("MSG: Add _Typeof to pending list %zu ending at %c\n", current, terminator);
                    __builtin_trap();
                } else {
                    if (is_typename(tu, s)) {
                        type = cuik_canonical_type(parse_typename(tu, s));
                    } else {
                        // we don't particularly resolve typeof for expressions immediately.
                        // instead we just wait until all symbols are resolved properly
                        Expr* src = parse_expr(tu, s);
                        type = new_typeof(tu, src);
                    }

                    if (tokens_get(s)->type != ')') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Typeof");
                        return CUIK_QUAL_TYPE_NULL;
                    }
                }
                break;
            }

            case TOKEN_KW_Alignas: {
                if (alignas_pending_expr != NULL) {
                    REPORT(ERROR, loc, "cannot apply two _Alignas to one type");
                    return CUIK_QUAL_TYPE_NULL;
                }

                tokens_next(s);
                expect(tu, s, '(');

                if (out_of_order_mode) {
                    // _Alignas ( SOMETHING )
                    TknType terminator;
                    size_t current = skip_expression_in_parens(s, &terminator);

                    if (terminator == 0 || terminator == ',') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Alignas%s", terminator ? " (got EOF)" : "");
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    tokens_prev(s);

                    // Add to pending list
                    PendingExpr e = {PENDING_ALIGNAS, current, NULL};
                    dyn_array_put(pending_exprs, e);
                    alignas_pending_expr = &pending_exprs[dyn_array_length(pending_exprs) - 1];
                } else {
                    if (is_typename(tu, s)) {
                        Cuik_Type* new_align = cuik_canonical_type(parse_typename(tu, s));
                        if (new_align == NULL || new_align->align) {
                            REPORT(ERROR, loc, "_Alignas cannot operate with incomplete");
                        } else {
                            forced_align = new_align->align;
                        }
                    } else {
                        intmax_t new_align = parse_const_expr(tu, s);
                        if (new_align == 0) {
                            REPORT(ERROR, loc, "_Alignas cannot be applied with 0 alignment", new_align);
                        } else if (new_align >= INT16_MAX) {
                            REPORT(ERROR, loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
                        } else {
                            forced_align = new_align;
                        }
                    }

                    if (tokens_get(s)->type != ')') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Alignas");
                        return CUIK_QUAL_TYPE_NULL;
                    }
                }
                break;
            }

            case TOKEN_KW_declspec: {
                // TODO(NeGate): Correctly parse declspec instead of
                // ignoring them.
                tokens_next(s);
                expect(tu, s, '(');

                int depth = 1;
                while (depth) {
                    if (tokens_get(s)->type == '(')
                        depth++;
                    else if (tokens_get(s)->type == ')')
                        depth--;

                    tokens_next(s);
                }

                tokens_prev(s);
                break;
            }

            case TOKEN_KW_struct:
            case TOKEN_KW_union: {
                if (counter) goto done;
                SourceRange record_loc = tokens_get_range(s);
                tokens_next(s);

                bool is_union = tkn_type == TOKEN_KW_union;
                while (skip_over_declspec(tu, s)) {
                    // TODO(NeGate): printf("Don't forget about declspec\n");
                }

                Atom name = NULL;
                if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
                    record_loc = tokens_get_range(s);

                    Token* t = tokens_get(s);
                    name = atoms_put(t->content.length, t->content.data);

                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    type = name ? find_tag(tu, (char*)name) : 0;
                    if (type) {
                        // can't re-complete a struct
                        //assert(!type->is_incomplete);
                    } else {
                        type = new_record(tu, is_union);
                        type->is_incomplete = false;
                        type->record.name = name;

                        // can't forward decl unnamed records so we
                        // don't track it
                        if (name) {
                            if (out_of_order_mode) {
                                nl_strmap_put_cstr(tu->global_tags, name, type);
                            } else {
                                if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                    SourceLoc loc2 = tokens_get_location(s);
                                    REPORT(ERROR, loc2, "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                    abort();
                                }

                                local_tags[local_tag_count++] = (TagEntry){name, type};
                            }
                        }
                    }
                    type->loc = record_loc;
                    counter += OTHER;

                    size_t member_count = 0;
                    Member* members = tls_save();

                    while (tokens_get(s)->type != '}') {
                        if (skip_over_declspec(tu, s)) continue;

                        // in case we have unnamed declarators and we somewhere for them to point to
                        SourceRange default_loc = tokens_get_range(s);

                        Attribs member_attr = { 0 };
                        Cuik_QualType member_base_type = parse_declspec(tu, s, &member_attr);

                        // error recovery, if we couldn't parse the typename we skip the declaration
                        if (CUIK_QUAL_TYPE_IS_NULL(member_base_type)) {
                            while (tokens_get(s)->type != ';') tokens_next(s);

                            tokens_next(s);
                            continue;
                        }

                        // continues on commas, exists on semicolon
                        // int a,   *b,   c[3]    ;
                        //     ^    ^~    ^~~~    ^
                        //     one  two   three   DONE
                        do {
                            Decl decl = { 0 };
                            Cuik_QualType member_type = member_base_type;

                            // not all members have declarators for example
                            // char : 3; or struct { ... };
                            if (tokens_get(s)->type != ';' &&
                                tokens_get(s)->type != ':') {
                                decl = parse_declarator(tu, s, member_base_type, false, false);
                                member_type = decl.type;
                            } else {
                                decl.loc = default_loc;
                            }

                            // Append member
                            tls_push(sizeof(Member));
                            Member* member = &members[member_count++];

                            *member = (Member){
                                .loc = decl.loc,
                                .type = member_type,
                                .name = decl.name
                            };

                            if (tokens_get(s)->type == ':') {
                                if (is_union) {
                                    diag_warn(&tu->tokens, decl.loc, "Bitfield... unions... huh?!");
                                } else if (CUIK_QUAL_TYPE_HAS(member_type, CUIK_QUAL_ATOMIC)) {
                                    diag_err(&tu->tokens, decl.loc, "Cannot make bitfields using atomics");
                                }
                                tokens_next(s);

                                member->is_bitfield = true;
                                member->bit_offset = 0;
                                member->bit_width = parse_const_expr(tu, s);
                            }

                            // i just wanted to logically split this from the top stuff, this is a breather comment
                            if (tokens_get(s)->type == ',') {
                                tokens_next(s);
                                continue;
                            } else if (tokens_get(s)->type == ';') {
                                break;
                            }
                        } while (true);

                        expect(tu, s, ';');
                    }

                    if (tokens_get(s)->type != '}') {
                        diag_err(&tu->tokens, tokens_get_range(s), "Unclosed member list!");
                        tokens_next(s);
                    }

                    // put members into more permanent storage
                    Member* permanent_store = arena_alloc(&tu->ast_arena, member_count * sizeof(Member), _Alignof(Member));
                    memcpy(permanent_store, members, member_count * sizeof(Member));

                    type->align = 0;
                    type->size = 0;

                    type->record.kids = permanent_store;
                    type->record.kid_count = member_count;

                    if (!out_of_order_mode) {
                        type_layout(tu, type, true);
                    }

                    tls_restore(members);
                } else {
                    // TODO(NeGate): must be a forward decl, handle it
                    if (name == NULL) {
                        diag_err(&tu->tokens, record_loc, "Cannot have unnamed forward struct reference.");
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    type = find_tag(tu, (const char*)name);
                    if (type == NULL) {
                        type = new_record(tu, is_union);
                        type->loc = record_loc;
                        type->record.name = name;
                        type->is_incomplete = true;

                        if (out_of_order_mode) {
                            nl_strmap_put_cstr(tu->global_tags, name, type);
                        } else {
                            if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                SourceLoc loc2 = tokens_get_location(s);
                                REPORT(ERROR, loc2, "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                abort();
                            }

                            local_tags[local_tag_count++] = (TagEntry){name, type};
                        }
                    }
                    counter += OTHER;

                    // push back one because we push it forward one later but
                    // shouldn't
                    tokens_prev(s);
                }
                break;
            }

            case TOKEN_KW_enum: {
                if (counter) goto done;
                tokens_next(s);

                Token* t = tokens_get(s);
                Atom name = NULL;
                if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
                    name = atoms_put(t->content.length, t->content.data);
                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    type = name ? find_tag(tu, (char*)name) : 0;
                    if (type) {
                        // can't re-complete a enum
                        // TODO(NeGate): error messages
                        size_t count = type->enumerator.count;
                        if (count) {
                            generic_error(tu, s, "Cannot recomplete an enumerator");
                        }
                    } else {
                        type = new_enum(tu);
                        type->is_incomplete = true;
                        type->enumerator.name = name;

                        if (name) {
                            if (out_of_order_mode)
                                nl_strmap_put_cstr(tu->global_tags, name, type);
                            else {
                                if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                    SourceLoc loc2 = tokens_get_location(s);
                                    REPORT(ERROR, loc2, "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                    abort();
                                }

                                local_tags[local_tag_count++] = (TagEntry){name, type};
                            }
                        }
                    }

                    // starts at zero and after any entry it increments
                    // you can override it by using:
                    // identifier = int-const-expr
                    int cursor = 0;

                    size_t count = 0;
                    EnumEntry* start = tls_save();

                    // HACK HACK HACK this is probably going to create some weird issues later ngl
                    // we basically set the enumerator entries to the temporary stuff just so that
                    // the enum resolution doesn't break if we need to access stuff we're building
                    // for example:
                    //   enum { T, U = T + 1 }
                    type->enumerator.entries = start;
                    type->enumerator.count = 0;

                    while (tokens_get(s)->type != '}') {
                        // parse name
                        Token* t = tokens_get(s);
                        if (t->type != TOKEN_IDENTIFIER) {
                            generic_error(tu, s, "expected identifier for enum name entry.");
                        }

                        Atom name = atoms_put(t->content.length, t->content.data);
                        tokens_next(s);

                        int lexer_pos = 0;
                        if (tokens_get(s)->type == '=') {
                            tokens_next(s);

                            if (out_of_order_mode) {
                                TknType terminator;
                                lexer_pos = skip_expression_in_enum(s, &terminator);

                                if (terminator == 0) {
                                    diag_err(&tu->tokens, tokens_get_range(s), "expected comma or } (got EOF)");
                                    abort();
                                }
                            } else {
                                cursor = parse_const_expr(tu, s);
                            }
                        }

                        // Allocate into temporary buffer
                        tls_push(sizeof(EnumEntry));
                        start[count] = (EnumEntry){ name, lexer_pos, cursor };

                        Symbol sym = {
                            .name = name,
                            .type = cuik_uncanonical_type(type),
                            .loc = get_token_range(t),
                            .storage_class = STORAGE_ENUM,
                            .enum_value = count
                        };

                        if (out_of_order_mode) {
                            nl_strmap_put_cstr(tu->global_symbols, name, sym);
                        } else {
                            local_symbols[local_symbol_count++] = sym;
                            cursor += 1;
                        }

                        if (tokens_get(s)->type == ',') tokens_next(s);
                        count += 1;
                    }

                    if (tokens_get(s)->type != '}') {
                        generic_error(tu, s, "Unclosed enum list!");
                    }

                    // move to more permanent storage
                    EnumEntry* permanent_store = arena_alloc(&tu->ast_arena, count * sizeof(EnumEntry), _Alignof(EnumEntry));
                    memcpy(permanent_store, start, count * sizeof(EnumEntry));
                    tls_restore(start);

                    type->enumerator.entries = permanent_store;
                    type->enumerator.count = count;

                    if (out_of_order_mode) {
                        type->is_incomplete = true;
                        type->size = 0;
                        type->align = 0;
                    } else {
                        type_layout(tu, type, true);
                    }
                } else {
                    type = find_tag(tu, (char*)name);
                    if (!type) {
                        type = new_enum(tu);
                        type->record.name = name;
                        type->is_incomplete = true;

                        if (out_of_order_mode) {
                            nl_strmap_put_cstr(tu->global_tags, name, type);
                        } else {
                            if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                diag_err(&tu->tokens, tokens_get_range(s), "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                return CUIK_QUAL_TYPE_NULL;
                            }

                            local_tags[local_tag_count] = (TagEntry){name, type};
                        }
                    }

                    // push back one because we push it forward one later but
                    // shouldn't
                    tokens_prev(s);
                }

                counter += OTHER;
                break;
            }

            case TOKEN_IDENTIFIER: {
                if (counter) goto done;

                if (out_of_order_mode) {
                    Token* t = tokens_get(s);
                    Atom name = atoms_put(t->content.length, t->content.data);

                    // if the typename is already defined, then reuse that type index
                    Symbol* sym = find_global_symbol(tu, (const char*)name);
                    // if not, we assume this must be a typedef'd type and reserve space
                    if (sym != NULL) {
                        if (sym->storage_class != STORAGE_TYPEDEF) {
                            diag_err(&tu->tokens, tokens_get_range(s), "symbol '%s' is not a typedef.", name);
                            diag_note(&tu->tokens, sym->loc, "declared here");
                            return CUIK_QUAL_TYPE_NULL;
                        }

                        type = cuik_canonical_type(sym->type);
                        quals |= cuik_get_quals(sym->type);
                        counter += OTHER;
                    } else {
                        //printf("MSG: Add typename to pending list '%s'\n", name);

                        // add placeholder
                        type = new_blank_type(tu);
                        Symbol sym = {
                            .name = name,
                            .type = cuik_uncanonical_type(type),
                            .loc = get_token_range(t),
                            .storage_class = STORAGE_TYPEDEF,
                        };
                        type->loc = sym.loc;
                        type->placeholder.name = name;
                        counter += OTHER;

                        nl_strmap_put_cstr(tu->global_symbols, name, sym);
                    }

                    break;
                } else {
                    Symbol* sym = find_local_symbol(s);
                    if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
                        type = cuik_canonical_type(sym->type);
                        quals |= cuik_get_quals(sym->type);
                        counter += OTHER;
                        break;
                    }

                    Token* t = tokens_get(s);
                    Atom name = atoms_put(t->content.length, t->content.data);

                    sym = find_global_symbol(tu, (const char*)name);
                    if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
                        type = cuik_canonical_type(sym->type);
                        quals |= cuik_get_quals(sym->type);
                        counter += OTHER;
                        break;
                    }

                    // if not a typename, this isn't a typedecl
                    goto done;
                }
            }
            default:
            goto done;
        }

        switch (counter) {
            case 0:
            break; // not resolved yet
            case VOID:
            type = &builtin_types[TYPE_VOID];
            break;
            case BOOL:
            type = &builtin_types[TYPE_BOOL];
            break;
            case CHAR:
            case SIGNED + CHAR:
            type = &builtin_types[TYPE_CHAR];
            break;
            case UNSIGNED + CHAR:
            type = &builtin_types[TYPE_UCHAR];
            break;
            case SHORT:
            case SHORT + INT:
            case SIGNED + SHORT:
            case SIGNED + SHORT + INT:
            type = &builtin_types[TYPE_SHORT];
            break;
            case UNSIGNED + SHORT:
            case UNSIGNED + SHORT + INT:
            type = &builtin_types[TYPE_USHORT];
            break;
            case INT:
            case LONG:
            case LONG + INT:
            case SIGNED:
            type = &builtin_types[TYPE_INT];
            break;
            case SIGNED + INT:
            case SIGNED + LONG:
            case SIGNED + LONG + INT:
            type = &builtin_types[tu->is_windows_long ? TYPE_INT : TYPE_LONG];
            break;
            case UNSIGNED:
            case UNSIGNED + INT:
            type = &builtin_types[TYPE_UINT];
            break;
            case UNSIGNED + LONG:
            case UNSIGNED + LONG + INT:
            type = &builtin_types[tu->is_windows_long ? TYPE_UINT : TYPE_ULONG];
            break;
            case LONG + LONG:
            case LONG + LONG + INT:
            case SIGNED + LONG + LONG:
            case SIGNED + LONG + LONG + INT:
            type = &builtin_types[TYPE_LONG];
            break;
            case UNSIGNED + LONG + LONG:
            case UNSIGNED + LONG + LONG + INT:
            type = &builtin_types[TYPE_ULONG];
            break;
            case FLOAT:
            type = &builtin_types[TYPE_FLOAT];
            break;
            case DOUBLE:
            case LONG + DOUBLE:
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
            REPORT(ERROR, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, type->align);
            return CUIK_QUAL_TYPE_NULL;
        }

        // clone it since we need to modify it
        Cuik_Type* new_type = new_blank_type(tu);
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

    return cuik_make_qual_type(type, quals);
}

static bool is_typename(TranslationUnit* tu, TokenStream* restrict s) {
    Token* t = tokens_get(s);

    switch (t->type) {
        case TOKEN_KW_void:
        case TOKEN_KW_char:
        case TOKEN_KW_short:
        case TOKEN_KW_int:
        case TOKEN_KW_long:
        case TOKEN_KW_float:
        case TOKEN_KW_double:
        case TOKEN_KW_Bool:
        case TOKEN_KW_signed:
        case TOKEN_KW_unsigned:
        case TOKEN_KW_struct:
        case TOKEN_KW_union:
        case TOKEN_KW_enum:
        case TOKEN_KW_extern:
        case TOKEN_KW_static:
        case TOKEN_KW_typedef:
        case TOKEN_KW_inline:
        case TOKEN_KW_const:
        case TOKEN_KW_volatile:
        case TOKEN_KW_register:
        case TOKEN_KW_declspec:
        case TOKEN_KW_Thread_local:
        case TOKEN_KW_Alignas:
        case TOKEN_KW_Atomic:
        case TOKEN_KW_auto:
        case TOKEN_KW_Typeof:
        return true;

        case TOKEN_IDENTIFIER: {
            // good question...
            Token* t = tokens_get(s);
            Atom name = atoms_put(t->content.length, t->content.data);

            Symbol* loc = find_local_symbol(s);
            if (loc != NULL) {
                // if we find a normal symbol before the typedef, then we didn't match typename
                // and thus don't continue:
                //
                // typedef struct foo { int x; } foo;
                // void test() {
                //   foo *foo;
                //   foo->x = 0;
                //   ^^^
                //  this is referring to foo the variable not the typedef
                // }
                return (loc->storage_class == STORAGE_TYPEDEF);
            }

            Symbol* glob = find_global_symbol(tu, (const char*)name);
            if (glob != NULL && glob->storage_class == STORAGE_TYPEDEF) return true;

            return false;
        }

        default:
        return false;
    }
}
