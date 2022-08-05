////////////////////////////////
// TYPES
////////////////////////////////
static thread_local char temp_string0[1024];

static bool parse_attributes(TranslationUnit* restrict tu, TokenStream* restrict s, Stmt* restrict n) {
    if (tokens_get(s)->type == TOKEN_KW_attribute ||
        tokens_get(s)->type == TOKEN_KW_asm) {
        tokens_next(s);
        expect(tu, s, '(');

        // TODO(NeGate): Correctly parse attributes instead of
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

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type, bool is_abstract, bool disabled_paren) {
    assert(type != NULL);

    // handle calling convention
    // TODO(NeGate): Actually pass these to the AST
    parse_another_qualifier2: {
        switch (tokens_get(s)->type) {
            case TOKEN_KW_cdecl:
            case TOKEN_KW_stdcall:
            tokens_next(s);
            goto parse_another_qualifier2;
            default:
            break;
        }
    }

    // handle pointers
    while (tokens_get(s)->type == '*') {
        type = type ? new_pointer(tu, type) : 0;
        tokens_next(s);

        parse_another_qualifier : {
            switch (tokens_get(s)->type) {
                case TOKEN_KW_Atomic: {
                    type->is_atomic = true;
                    tokens_next(s);
                    goto parse_another_qualifier;
                }
                case TOKEN_KW_restrict: {
                    type->is_ptr_restrict = true;
                    tokens_next(s);
                    goto parse_another_qualifier;
                }

                case TOKEN_KW_const:
                case TOKEN_KW_volatile:
                case TOKEN_KW_cdecl:
                case TOKEN_KW_stdcall: {
                    tokens_next(s);
                    goto parse_another_qualifier;
                }

                default:
                break;
            }
        }
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
        SourceLocIndex opening_loc = tokens_get_location_index(s);

        tokens_next(s);
        size_t saved = s->current;

        // dummy_type just avoids problems where the type would be NULL and needs to be read
        // it's not gonna modify and rarely really reads from it
        Cuik_Type* dummy_type = &builtin_types[TYPE_VOID];
        parse_declarator(tu, s, dummy_type, is_abstract, false);

        expect_closing_paren(tu, s, opening_loc);
        type = parse_type_suffix(tu, s, type, NULL);

        size_t saved_end = s->current;
        s->current = saved;

        Decl d = parse_declarator(tu, s, type, is_abstract, false);

        // inherit name
        // TODO(NeGate): I'm not sure if this is correct ngl
        if (!d.name) {
            Cuik_Type* t = d.type;

            if (t->kind == KIND_PTR) {
                t = d.type->ptr_to;

                if (t->kind == KIND_FUNC) {
                    d.name = t->func.name;
                } else if (t->kind == KIND_STRUCT) {
                    d.name = t->record.name;
                } else if (t->kind == KIND_UNION) {
                    d.name = t->record.name;
                }
            }
        }

        s->current = saved_end;
        return d;
    }

    Atom name = NULL;
    Token* t = tokens_get(s);
    SourceLocIndex loc = tokens_get_location_index(s);
    if (!is_abstract && t->type == TOKEN_IDENTIFIER) {
        name = atoms_put(t->end - t->start, t->start);
        tokens_next(s);
    }

    type = parse_type_suffix(tu, s, type, name);
    return (Decl){type, name, loc};
}

static Cuik_Type* parse_typename(TranslationUnit* tu, TokenStream* restrict s) {
    // TODO(NeGate): Check if attributes are set, they shouldn't
    // be in this context.
    Attribs attr = {0};
    Cuik_Type* type = parse_declspec(tu, s, &attr);
    return parse_declarator(tu, s, type, true, false).type;
}

static Cuik_Type* parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type, Atom name) {
    assert(s->current > 0);
    SourceLocIndex loc = tokens_get_last_location_index(s);

    // type suffixes like array [] and function ()
    if (tokens_get(s)->type == '(') {
        tokens_next(s);

        Cuik_Type* return_type = type;

        type = new_func(tu);
        type->func.name = name;
        type->func.return_type = return_type;

        if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
            tokens_next(s);
            tokens_next(s);

            type->func.param_list = 0;
            type->func.param_count = 0;
            return type;
        }

        size_t param_count = 0;
        void* params = tls_save();
        bool has_varargs = false;

        while (tokens_get(s)->type && tokens_get(s)->type != ')') {
            if (param_count) {
                if (tokens_get(s)->type != ',') {
                    tokens_prev(s);
                    SourceLocIndex loc = tokens_get_last_location_index(s);

                    REPORT(ERROR, loc, "Expected a comma after this declaration name");
                    abort();
                }

                tokens_next(s);
            }

            if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
                tokens_next(s);

                has_varargs = true;
                break;
            }

            Attribs arg_attr = {0};
            Cuik_Type* arg_base_type = parse_declspec(tu, s, &arg_attr);
            if (arg_base_type == NULL) {
                while (tokens_get(s)->type && tokens_get(s)->type != ')') {
                    tokens_next(s);
                }

                break;
            }

            Decl param_decl = parse_declarator(tu, s, arg_base_type, false, false);
            Cuik_Type* param_type = param_decl.type;

            if (param_type->kind == KIND_ARRAY) {
                // Array parameters are desugared into pointers
                param_type = new_pointer(tu, param_type->array_of);
            } else if (param_type->kind == KIND_FUNC) {
                // Function parameters are desugared into pointers
                param_type = new_pointer(tu, param_type);
            }

            // TODO(NeGate): Error check that no attribs are set
            *((Param*)tls_push(sizeof(Param))) = (Param){
                .type = param_type,
                .name = param_decl.name};
            param_count++;
        }

        if (tokens_get(s)->type != ')') {
            generic_error(tu, s, "Unclosed parameter list!");
        }
        tokens_next(s);

        // Allocate some more permanent storage
        Param* permanent_store = arena_alloc(&tu->ast_arena, param_count * sizeof(Param), _Alignof(Param));
        memcpy(permanent_store, params, param_count * sizeof(Param));

        type->func.param_list = permanent_store;
        type->func.param_count = param_count;
        type->func.has_varargs = has_varargs;

        tls_restore(params);
    } else if (tokens_get(s)->type == '[') {
        if (out_of_order_mode) {
            // in the out of order case we defer expression parsing
            SourceLocIndex open_brace = tokens_get_location_index(s);
            tokens_next(s);

            size_t current = 0;
            if (tokens_get(s)->type == ']') {
                tokens_next(s);
            } else if (tokens_get(s)->type == '*') {
                tokens_next(s);
                expect(tu, s, ']');
            } else {
                current = s->current;

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

            type = parse_type_suffix(tu, s, type, name);

            // create placeholder array type
            type = new_array(tu, type, 0);
            type->loc = tokens_get_location_index(s);
            type->array_count_lexer_pos = current;
            return type;
        } else {
            size_t depth = 0;
            size_t* counts = tls_save();

            // TODO(NeGate): read some array qualifiers... then throw them away
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

            size_t expected_size = type->size;
            while (depth--) {
                assert(type->size == expected_size);

                uint64_t a = expected_size;
                uint64_t b = counts[depth];
                uint64_t result = a * b;

                // size checks
                if (result >= INT32_MAX) {
                    REPORT(ERROR, loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
                    abort();
                }

                type = new_array(tu, type, counts[depth]);
                expected_size = result;
            }

            tls_restore(counts);
        }
    }

    return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static Cuik_Type* parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr) {
    enum {
        VOID = 1 << 0,
        BOOL = 1 << 2,
        CHAR = 1 << 4,
        SHORT = 1 << 6,
        INT = 1 << 8,
        LONG = 1 << 10,
        FLOAT = 1 << 12,
        DOUBLE = 1 << 14,
        OTHER = 1 << 16,
        SIGNED = 1 << 17,
        UNSIGNED = 1 << 18,
    };

    int counter = 0;
    Cuik_Type* type = NULL;

    bool is_atomic = false;
    bool is_const = false;

    // _Alignas(N) or __declspec(align(N))
    // 0 means no forced alignment
    int forced_align = 0;
    PendingExpr* alignas_pending_expr = NULL;

    SourceLocIndex loc = tokens_get_location_index(s);
    do {
        TknType tkn_type = tokens_get(s)->type;
        switch (tkn_type) {
            case TOKEN_KW_void:
            counter += VOID;
            break;
            case TOKEN_KW_Bool:
            counter += BOOL;
            break;
            case TOKEN_KW_char:
            counter += CHAR;
            break;
            case TOKEN_KW_short:
            counter += SHORT;
            break;
            case TOKEN_KW_int:
            counter += INT;
            break;
            case TOKEN_KW_long:
            counter += LONG;
            break;
            case TOKEN_KW_float:
            counter += FLOAT;
            break;
            case TOKEN_KW_double:
            counter += DOUBLE;
            break;

            case TOKEN_KW_unsigned:
            counter |= UNSIGNED;
            break;
            case TOKEN_KW_signed:
            counter |= SIGNED;
            break;

            case TOKEN_KW_register: /* lmao */
            break;
            case TOKEN_KW_Noreturn: /* lmao */
            break;
            case TOKEN_KW_static:
            attr->is_static = true;
            break;
            case TOKEN_KW_typedef:
            attr->is_typedef = true;
            break;
            case TOKEN_KW_inline:
            attr->is_inline = true;
            break;
            case TOKEN_KW_extern:
            attr->is_extern = true;
            break;
            case TOKEN_KW_Thread_local:
            attr->is_tls = true;
            break;

            case TOKEN_KW_cdecl:
            break;
            case TOKEN_KW_stdcall:
            break;

            case TOKEN_KW_Complex:
            case TOKEN_KW_Imaginary: {
                REPORT(ERROR, loc, "Complex types are not supported in CuikC");
                break;
            }

            case TOKEN_KW_Vector: {
                // _Vector '(' TYPENAME ',' CONST-EXPR ')'
                if (counter) goto done;
                tokens_next(s);

                SourceLocIndex opening_loc = tokens_get_location_index(s);
                expect(tu, s, '(');

                type = parse_typename(tu, s);
                if (!(type->kind >= KIND_CHAR && type->kind <= KIND_LONG) &&
                    !(type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE)) {
                    REPORT(ERROR, loc, "Only integers and floats can be used for _Vector types");
                    return NULL;
                }

                expect(tu, s, ',');

                intmax_t count = parse_const_expr(tu, s);

                if (count <= 0) {
                    REPORT(ERROR, loc, "_Vector types must have a positive width");
                    return NULL;
                }

                if (count == 1) {
                    REPORT(ERROR, loc, "It's not even a _Vector type... that's a scalar...");
                    return NULL;
                }

                if (count > 64) {
                    REPORT(ERROR, loc, "_Vector type is too wide (%" PRIiMAX ", max is 64)", count);
                    return NULL;
                }

                // only allow power of two widths
                if ((count & (count - 1)) != 0) {
                    REPORT(ERROR, loc, "_Vector types can only have power-of-two widths");
                    return NULL;
                }

                type = new_vector(tu, type, count);
                counter += OTHER;

                expect_closing_paren(tu, s, opening_loc);
                tokens_prev(s);
                break;
            }
            case TOKEN_KW_Atomic: {
                tokens_next(s);
                if (tokens_get(s)->type == '(') {
                    SourceLocIndex opening_loc = tokens_get_location_index(s);
                    tokens_next(s);

                    type = parse_typename(tu, s);
                    counter += OTHER;
                    is_atomic = true;

                    SourceLocIndex closing_loc = tokens_get_location_index(s);
                    if (tokens_get(s)->type != ')') {
                        report_two_spots(REPORT_ERROR, tu->errors, s, opening_loc, closing_loc, "expected closing parenthesis for _Atomic", "open", "close?", NULL);
                        return NULL;
                    }
                } else {
                    // walk back, we didn't need to read that
                    is_atomic = true;
                    tokens_prev(s);
                }
                break;
            }
            case TOKEN_KW_const:
            is_const = true;
            break;
            case TOKEN_KW_volatile:
            break;
            case TOKEN_KW_auto:
            break;

            case TOKEN_KW_Typeof: {
                tokens_next(s);
                if (tokens_get(s)->type != '(') {
                    REPORT(ERROR, loc, "expected opening parenthesis for _Typeof");
                    return NULL;
                }
                tokens_next(s);

                if (out_of_order_mode) {
                    // _Typeof ( SOMETHING )
                    TknType terminator;
                    size_t current = skip_expression_in_parens(s, &terminator);

                    if (terminator == 0 || terminator == ',') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Typeof%s", terminator ? " (got EOF)" : "");
                        return NULL;
                    }

                    // Add to pending list
                    printf("MSG: Add _Typeof to pending list %zu ending at %c\n", current, terminator);
                    __builtin_trap();
                } else {
                    if (is_typename(tu, s)) {
                        type = parse_typename(tu, s);
                    } else {
                        // we don't particularly resolve typeof for expressions immediately.
                        // instead we just wait until all symbols are resolved properly
                        Expr* src = parse_expr(tu, s);
                        type = new_typeof(tu, src);
                    }

                    if (tokens_get(s)->type != ')') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Typeof");
                        return NULL;
                    }
                }
                break;
            }

            case TOKEN_KW_Alignas: {
                if (alignas_pending_expr != NULL) {
                    REPORT(ERROR, loc, "cannot apply two _Alignas to one type");
                    return NULL;
                }

                tokens_next(s);
                expect(tu, s, '(');

                if (out_of_order_mode) {
                    // _Alignas ( SOMETHING )
                    TknType terminator;
                    size_t current = skip_expression_in_parens(s, &terminator);

                    if (terminator == 0 || terminator == ',') {
                        REPORT(ERROR, loc, "expected closing parenthesis for _Alignas%s", terminator ? " (got EOF)" : "");
                        return NULL;
                    }

                    tokens_prev(s);

                    // Add to pending list
                    PendingExpr e = {PENDING_ALIGNAS, current, NULL};
                    arrput(pending_exprs, e);
                    alignas_pending_expr = &pending_exprs[arrlen(pending_exprs) - 1];
                } else {
                    if (is_typename(tu, s)) {
                        Cuik_Type* new_align = parse_typename(tu, s);
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
                        return NULL;
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
                SourceLocIndex record_loc = tokens_get_location_index(s);
                tokens_next(s);

                bool is_union = tkn_type == TOKEN_KW_union;

                while (skip_over_declspec(tu, s)) {
                    // TODO(NeGate): printf("Don't forget about declspec\n");
                }

                Atom name = NULL;
                if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
                    record_loc = tokens_get_location_index(s);

                    Token* t = tokens_get(s);
                    name = atoms_put(t->end - t->start, t->start);

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
                                    SourceLocIndex loc2 = tokens_get_location_index(s);
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
                        SourceLocIndex default_loc = tokens_get_location_index(s);

                        Attribs member_attr = {0};
                        Cuik_Type* member_base_type = parse_declspec(tu, s, &member_attr);

                        // error recovery, if we couldn't parse the typename we skip the declaration
                        if (member_base_type == 0) {
                            while (tokens_get(s)->type != ';') tokens_next(s);

                            tokens_next(s);
                            continue;
                        }

                        // continues on commas, exists on semicolon
                        // int a,   *b,  c[3]    ;
                        //     ^    ^~   ^~~~    ^
                        //     one  two  three   DONE
                        do {
                            Decl decl = {0};
                            Cuik_Type* member_type = member_base_type;

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
                                    REPORT(WARNING, decl.loc, "Bitfield... unions... huh?!");
                                } else if (member_type->is_atomic) {
                                    REPORT(ERROR, decl.loc, "Cannot make bitfields using atomics");
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
                            } else if (tokens_get(s)->type == ';') break;
                        } while (true);

                        expect(tu, s, ';');
                    }

                    if (tokens_get(s)->type != '}') {
                        generic_error(tu, s, "Unclosed member list!");
                    }

                    // put members into more permanent storage
                    Member* permanent_store = arena_alloc(&tu->ast_arena, member_count * sizeof(Member), _Alignof(Member));
                    memcpy(permanent_store, members, member_count * sizeof(Member));

                    type->align = 0;
                    type->size = 0;

                    type->record.kids = permanent_store;
                    type->record.kid_count = member_count;

                    if (!out_of_order_mode) {
                        type_layout(tu, type);
                    }

                    tls_restore(members);
                } else {
                    // TODO(NeGate): must be a forward decl, handle it
                    if (name == NULL) generic_error(tu, s, "Cannot have unnamed forward struct reference.");

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
                                SourceLocIndex loc2 = tokens_get_location_index(s);
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
                    name = atoms_put(t->end - t->start, t->start);
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
                                    SourceLocIndex loc2 = tokens_get_location_index(s);
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

                        Atom name = atoms_put(t->end - t->start, t->start);
                        tokens_next(s);

                        int lexer_pos = 0;
                        if (tokens_get(s)->type == '=') {
                            tokens_next(s);

                            if (out_of_order_mode) {
                                TknType terminator;
                                lexer_pos = skip_expression_in_enum(s, &terminator);

                                if (terminator == 0) {
                                    SourceLocIndex loc2 = tokens_get_location_index(s);
                                    REPORT(ERROR, loc2, "expected comma or } (got EOF)");
                                    abort();
                                }
                            } else {
                                cursor = parse_const_expr(tu, s);
                            }
                        }

                        // Allocate into temporary buffer
                        tls_push(sizeof(EnumEntry));
                        start[count] = (EnumEntry){name, lexer_pos, cursor};

                        Symbol sym = {
                            .name = name,
                            .type = type,
                            .loc = t->location,
                            .storage_class = STORAGE_ENUM,
                            .enum_value = count};

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
                        type_layout(tu, type);
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
                                SourceLocIndex loc2 = tokens_get_location_index(s);
                                REPORT(ERROR, loc2, "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                abort();
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
                    Atom name = atoms_put(t->end - t->start, t->start);

                    // if the typename is already defined, then reuse that type index
                    Symbol* sym = find_global_symbol(tu, (const char*)name);
                    // if not, we assume this must be a typedef'd type and reserve space
                    if (sym != NULL) {
                        if (sym->storage_class != STORAGE_TYPEDEF) {
                            sprintf_s(temp_string0, sizeof(temp_string0), "symbol '%s' is not a typedef", name);

                            SourceLocIndex loc2 = tokens_get_location_index(s);
                            report_two_spots(
                                REPORT_ERROR, tu->errors, s, loc2, sym->loc,
                                temp_string0, "use", "def", NULL
                            );
                            return NULL;
                        }

                        type = sym->type;
                        counter += OTHER;
                    } else {
                        //printf("MSG: Add typename to pending list '%s'\n", name);

                        // add placeholder
                        Symbol sym = {
                            .name = name,
                            .type = new_blank_type(tu),
                            .loc = t->location,
                            .storage_class = STORAGE_TYPEDEF,
                        };
                        type = sym.type;
                        type->loc = t->location;
                        type->placeholder.name = name;
                        counter += OTHER;

                        nl_strmap_put_cstr(tu->global_symbols, name, sym);
                    }

                    break;
                } else {
                    Symbol* sym = find_local_symbol(s);
                    if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
                        type = sym->type;
                        counter += OTHER;
                        break;
                    }

                    Token* t = tokens_get(s);
                    Atom name = atoms_put(t->end - t->start, t->start);

                    sym = find_global_symbol(tu, (const char*)name);
                    if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
                        type = sym->type;
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
            default:
            generic_error(tu, s, "invalid type");
            break;
        }

        tokens_next(s);
    } while (true);

    done:;
    if (type == 0) {
        REPORT(ERROR, loc, "unknown typename");
        return NULL;
    }

    if (is_atomic || is_const || (forced_align && type->align != forced_align)) {
        if (forced_align && forced_align < type->align) {
            REPORT(ERROR, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, type->align);
            return NULL;
        }

        int align = -1;
        if (forced_align) {
            align = forced_align;
        } else if (alignas_pending_expr != NULL) {
            alignas_pending_expr->dst = &type->align;
        }

        type = new_qualified_type(tu, type, align, is_atomic, is_const);
        type->loc = loc;
    }

    return type;
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
        case TOKEN_KW_declspec:
        case TOKEN_KW_Thread_local:
        case TOKEN_KW_Alignas:
        case TOKEN_KW_Atomic:
        case TOKEN_KW_auto:
        case TOKEN_KW_cdecl:
        case TOKEN_KW_stdcall:
        case TOKEN_KW_Typeof:
        return true;

        case TOKEN_IDENTIFIER: {
            // good question...
            Token* t = tokens_get(s);
            Atom name = atoms_put(t->end - t->start, t->start);

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
