////////////////////////////////
// TYPES
////////////////////////////////
static Decl parse_declarator2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);
static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);

static Cuik_Attribute* parse_attributes(TokenStream* restrict s, Cuik_Attribute* last) {
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
                diag_err(s, tokens_get_range(s), "expected an identifier");
            } else {
                Token* t = tokens_get(s);
                a->name = atoms_put(t->content.length, t->content.data);
                tokens_next(s);
            }
            a->loc.end = tokens_get_location(s);

            if (!tokens_peek_double_token(s, ']')) {
                diag_err(s, tokens_get_range(s), "expected closing ']]' for attribute");
            }
            tokens_next(s), tokens_next(s);

            last = a;
        } else if (tokens_get(s)->type == TOKEN_KW_attribute) {
            // TODO(NeGate): Correctly parse attributes instead of
            // ignoring them.
            tokens_next(s);
            expect_char(s, '(');

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

static bool skip_over_declspec(TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_declspec || tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect_char(s, '(');

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

static Cuik_QualType parse_ptr_qualifiers(TokenStream* restrict s, Cuik_QualType type) {
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
            type.raw |= CUIK_QUAL_VOLATILE;
            tokens_next(s);
        } else if (t == TOKEN_KW_cdecl || t == TOKEN_KW_stdcall) {
            tokens_next(s);
        } else {
            break;
        }
    }

    return type;
}

static bool is_typename(Cuik_GlobalSymbols* restrict syms, TokenStream* restrict s) {
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

            Symbol* glob = find_global_symbol(syms, (const char*)name);
            if (glob != NULL && glob->storage_class == STORAGE_TYPEDEF) return true;

            return false;
        }

        default:
        return false;
    }
}

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
    // EXPRESSION ','
    // EXPRESSION ';'
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

    *out_end = s->list.current;
    return current;
}

static ptrdiff_t skip_expression_in_enum2(TokenStream* restrict s, ptrdiff_t* out_end) {
    // EXPRESSION ','
    // EXPRESSION '}'
    ptrdiff_t current = s->list.current;

    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            diag_err(s, get_token_range(t), "Declaration was never closed");

            // restore the token stream
            s->list.current = current + 1;
            return -1;
        } else if (t->type == '(') {
            depth++;
        } else if (t->type == ')') {
            depth--;

            if (depth == 0) {
                diag_err(s, get_token_range(t), "Unbalanced parenthesis");

                s->list.current = current + 1;
                return -1;
            }
        } else if (t->type == '}' || t->type == ',') {
            if (depth == 1) {
                depth--;
                break;
            }
        }

        tokens_next(s);
    }

    *out_end = s->list.current;
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

    Cuik_Type* target_signed_ints = (Cuik_Type*) parser->target->signed_ints;
    Cuik_Type* target_unsigned_ints = (Cuik_Type*) parser->target->unsigned_ints;

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

            case TOKEN_KW_declspec: {
                // TODO(NeGate): Correctly parse declspec instead of ignoring them.
                tokens_next(s);
                expect_char(s, '(');

                int depth = 1;
                while (depth) {
                    if (tokens_get(s)->type == '(') {
                        depth++;
                    } else if (tokens_get(s)->type == ')') {
                        depth--;
                    }

                    tokens_next(s);
                }

                tokens_prev(s);
                break;
            }

            case TOKEN_KW_Vector: {
                // _Vector '(' TYPENAME ',' CONST-EXPR ')'
                if (counter) goto done;
                tokens_next(s);

                SourceLoc opening_loc = tokens_get_location(s);
                if (!expect_char(s, '(')) return CUIK_QUAL_TYPE_NULL;

                type = cuik_canonical_type(parse_typename2(parser, s));
                if (!cuik_type_is_integer(type) && !cuik_type_is_float(type)) {
                    diag_err(s, loc, "Only integers and floats can be used for _Vector types");
                    return CUIK_QUAL_TYPE_NULL;
                }

                if (!expect_char(s, ',')) return CUIK_QUAL_TYPE_NULL;

                intmax_t count = parse_const_expr2(parser, s);

                if (count <= 0) {
                    diag_err(s, loc, "_Vector types must have a positive width");
                    return CUIK_QUAL_TYPE_NULL;
                }

                if (count == 1) {
                    diag_err(s, loc, "It's not even a _Vector type... that's a scalar...");
                    return CUIK_QUAL_TYPE_NULL;
                }

                if (count > 64) {
                    diag_err(s, loc, "_Vector type is too wide (%" PRIiMAX ", max is 64)", count);
                    return CUIK_QUAL_TYPE_NULL;
                }

                // only allow power of two widths
                if ((count & (count - 1)) != 0) {
                    diag_err(s, loc, "_Vector types can only have power-of-two widths");
                    return CUIK_QUAL_TYPE_NULL;
                }

                type = cuik__new_vector(&parser->types, cuik_uncanonical_type(type), count);
                counter += OTHER;

                expect_closing_paren(s, opening_loc);
                tokens_prev(s);
                break;
            }

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
                    tokens_prev(s);
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
                    abort();
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

            // enum-specifier  ::= 'enum' identifier? '{' enumerator-list (',')? '}' | 'enum' identifier
            // enumerator-list ::= enumerator (',' enumerator)*
            // enumerator      ::= identifier | identifier '=' constant-expression
            case TOKEN_KW_enum: {
                if (counter) goto done;
                counter += OTHER;
                tokens_next(s);

                Atom name = NULL;
                if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
                    Token* t = tokens_get(s);
                    name = atoms_put(t->content.length, t->content.data);
                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    type = name ? find_tag(&parser->globals, (char*) name) : 0;
                    if (type) {
                        // can't re-complete a enum
                        size_t count = type->enumerator.count;
                        if (count) {
                            diag_err(s, tokens_get_range(s), "cannot recomplete an enumerator");
                            diag_note(s, type->loc, "see here");
                        }
                    } else {
                        type = cuik__new_enum(&parser->types);
                        type->is_complete = false;
                        type->enumerator.name = name;

                        if (name) {
                            if (parser->is_in_global_scope) {
                                nl_strmap_put_cstr(parser->globals.tags, name, type);
                            } else if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                diag_err(s, tokens_get_range(s), "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                            } else {
                                local_tags[local_tag_count++] = (TagEntry){ name, type };
                            }
                        }
                    }

                    // starts at zero and after any entry it increments
                    // you can override it by using:
                    //   identifier = int-const-expr
                    int cursor = 0;

                    size_t count = 0;
                    EnumEntry* start = tls_save();

                    // in global scope we delay resolving the enumerator values which is kinda problematic since
                    type->enumerator.entries = start;
                    type->enumerator.count = 0;

                    while (tokens_get(s)->type != '}') {
                        // parse name
                        Token* t = tokens_get(s);
                        if (t->type != TOKEN_IDENTIFIER) {
                            diag_err(s, tokens_get_range(s), "expected identifier for enum name entry.");
                        }

                        Atom name = atoms_put(t->content.length, t->content.data);
                        tokens_next(s);

                        int lexer_pos = 0;
                        if (tokens_get(s)->type == '=') {
                            tokens_next(s);

                            if (parser->is_in_global_scope) {
                                ptrdiff_t lexer_end;
                                lexer_pos = skip_expression_in_enum2(s, &lexer_end);

                                if (lexer_pos < 0) {
                                    diag_err(s, tokens_get_range(s), "expected comma or } (got EOF)");
                                    break;
                                }
                            } else {
                                cursor = parse_const_expr2(parser, s);
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

                        if (parser->is_in_global_scope) {
                            nl_strmap_put_cstr(parser->globals.symbols, name, sym);
                        } else {
                            local_symbols[local_symbol_count++] = sym;
                            cursor += 1;
                        }

                        count += 1;
                        if (tokens_get(s)->type == ',') {
                            tokens_next(s);
                            continue;
                        } else {
                            break;
                        }
                    }
                    expect_char(s, '}');
                    tokens_prev(s);

                    // move to more permanent storage
                    EnumEntry* permanent_store = arena_alloc(&local_ast_arena, count * sizeof(EnumEntry), _Alignof(EnumEntry));
                    memcpy(permanent_store, start, count * sizeof(EnumEntry));
                    tls_restore(start);

                    type->enumerator.entries = permanent_store;
                    type->enumerator.count = count;

                    if (parser->is_in_global_scope) {
                        type->is_complete = false;
                        type->size = 0;
                        type->align = 0;
                    } else {
                        type->is_complete = true;
                        type->size = type->align = 4;
                        type_layout2(parser, type, true);
                    }
                } else {
                    if (name == NULL) {
                        diag_err(s, tokens_get_range(s), "expected { after enum declaration");
                        break;
                    }

                    type = find_tag(&parser->globals, (char*)name);
                    if (!type) {
                        type = cuik__new_enum(&parser->types);
                        type->record.name = name;
                        type->is_complete = false;

                        if (parser->is_in_global_scope) {
                            nl_strmap_put_cstr(parser->globals.tags, name, type);
                        } else {
                            if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                diag_err(s, tokens_get_range(s), "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                return CUIK_QUAL_TYPE_NULL;
                            }

                            local_tags[local_tag_count] = (TagEntry){ name, type };
                        }
                    }

                    // push back one because we push it forward one later but shouldn't
                    tokens_prev(s);
                }
                break;
            }

            case TOKEN_KW_struct: case TOKEN_KW_union: {
                if (counter) goto done;
                counter += OTHER;

                SourceRange record_loc = tokens_get_range(s);
                bool is_union = tkn_type == TOKEN_KW_union;
                tokens_next(s);

                // TODO(NeGate): handle struct/union declspecs
                while (skip_over_declspec(s)) {}

                Atom name = NULL;
                if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
                    record_loc = tokens_get_range(s);

                    Token* t = tokens_get(s);
                    name = atoms_put(t->content.length, t->content.data);

                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    type = name ? find_tag(&parser->globals, (char*)name) : 0;
                    if (type) {
                        // can't re-complete a struct
                        if (type->is_complete) {
                            diag_warn(s, record_loc, "struct was declared somewhere else");
                            diag_note(s, type->loc, "see here");
                        }
                    } else {
                        type = cuik__new_record(&parser->types, is_union);
                        type->is_complete = false;
                        type->record.name = name;

                        // can't forward decl unnamed records so we don't track it
                        if (name) {
                            if (parser->is_in_global_scope) {
                                nl_strmap_put_cstr(parser->globals.tags, name, type);
                            } else {
                                if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                    diag_err(s, tokens_get_range(s), "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                                } else {
                                    local_tags[local_tag_count++] = (TagEntry){ name, type };
                                }
                            }
                        }
                    }

                    type->loc = record_loc;

                    size_t member_count = 0;
                    Member* members = tls_save();
                    while (tokens_get(s)->type != '}') {
                        if (skip_over_declspec(s)) continue;

                        // skip any random semicolons
                        if (tokens_get(s)->type == ';') {
                            tokens_next(s);
                            continue;
                        }

                        // in case we have unnamed declarators and we somewhere for them to point to
                        SourceRange default_loc = tokens_get_range(s);

                        Attribs member_attr = { 0 };
                        Cuik_QualType member_base_type = parse_declspec2(parser, s, &member_attr);

                        // error recovery
                        if (CUIK_QUAL_TYPE_IS_NULL(member_base_type)) {
                            member_base_type = cuik_uncanonical_type(parser->default_int);
                            tokens_next(s);
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
                            if (tokens_get(s)->type != ';' && tokens_get(s)->type != ':') {
                                decl = parse_declarator2(parser, s, member_base_type, false);
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
                                    diag_warn(s, decl.loc, "Bitfield... unions... huh?!");
                                } else if (CUIK_QUAL_TYPE_HAS(member_type, CUIK_QUAL_ATOMIC)) {
                                    diag_err(s, decl.loc, "Cannot make bitfields using atomics");
                                }
                                tokens_next(s);

                                // TODO(NeGate): implement new constant expression eval
                                member->is_bitfield = true;
                                member->bit_offset = 0;
                                member->bit_width = parse_const_expr2(parser, s);
                            }

                            // i just wanted to logically split this from the top stuff, this is a breather comment
                            if (tokens_get(s)->type == ',') {
                                tokens_next(s);
                                continue;
                            } else if (tokens_get(s)->type == ';') {
                                break;
                            }
                        } while (true);

                        expect_char(s, ';');
                    }
                    expect_char(s, '}');
                    tokens_prev(s);

                    // put members into more permanent storage
                    Member* permanent_store = arena_alloc(&local_ast_arena, member_count * sizeof(Member), _Alignof(Member));
                    memcpy(permanent_store, members, member_count * sizeof(Member));

                    type->align = 0;
                    type->size = 0;

                    type->record.kids = permanent_store;
                    type->record.kid_count = member_count;

                    if (!parser->is_in_global_scope) {
                        type_layout2(parser, type, true);
                    }

                    tls_restore(members);
                } else {
                    // refers to a complete version of the record (which may or may not be ready yet)
                    if (name == NULL) {
                        diag_err(s, record_loc, "Cannot have unnamed forward struct reference.");
                        tokens_next(s);
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    type = find_tag(&parser->globals, (const char*)name);
                    if (type == NULL) {
                        type = cuik__new_record(&parser->types, is_union);
                        type->loc = record_loc;
                        type->record.name = name;
                        type->is_complete = false;

                        if (parser->is_in_global_scope) {
                            nl_strmap_put_cstr(parser->globals.tags, name, type);
                        } else {
                            if (local_tag_count + 1 >= MAX_LOCAL_TAGS) {
                                diag_err(s, tokens_get_range(s), "too many tags in local scopes (%d)", MAX_LOCAL_TAGS);
                            } else {
                                local_tags[local_tag_count++] = (TagEntry){ name, type };
                            }
                        }
                    }

                    // push back one because we push it forward one later but
                    // shouldn't
                    tokens_prev(s);
                }

                break;
            }

            case TOKEN_IDENTIFIER: {
                if (counter) goto done;

                if (parser->is_in_global_scope) {
                    Token* t = tokens_get(s);
                    Atom name = atoms_put(t->content.length, t->content.data);

                    // if the typename is already defined, then reuse that type index
                    Symbol* old_def = find_global_symbol(&parser->globals, (const char*)name);
                    // if not, we assume this must be a typedef'd type and reserve space
                    if (old_def != NULL) {
                        if (old_def->storage_class != STORAGE_TYPEDEF) {
                            diag_err(s, tokens_get_range(s), "symbol '%s' is not a typedef.", name);
                            diag_note(s, old_def->loc, "declared here");
                            return CUIK_QUAL_TYPE_NULL;
                        }

                        type = cuik_canonical_type(old_def->type);
                        quals |= cuik_get_quals(old_def->type);
                        counter += OTHER;
                    } else {
                        // add placeholder
                        type = cuik__new_blank_type(&parser->types);
                        Symbol sym = {
                            .name = name,
                            .type = cuik_uncanonical_type(type),
                            .loc = get_token_range(t),
                            .storage_class = STORAGE_TYPEDEF,
                        };
                        type->loc = sym.loc;
                        type->placeholder.name = name;
                        counter += OTHER;

                        nl_strmap_put_cstr(parser->globals.symbols, name, sym);
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

                    sym = find_global_symbol(&parser->globals, (const char*)name);
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
        tokens_next(s);
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

            // while (skip_over_declspec(s)) {}

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
        tokens_next(s);
        SourceLoc open_brace = tokens_get_location(s);

        Cuik_Type* t = NULL;
        if (parser->is_in_global_scope) {
            size_t current = 0;
            if (tokens_get(s)->type == ']') {
                tokens_next(s);
            } else if (tokens_get(s)->type == '*') {
                tokens_next(s);
                expect_char(s, ']');
            } else {
                current = s->list.current;
                skip_expression_in_braces(s, '[', ']');
            }

            // create placeholder array type
            t = cuik__new_array(&parser->types, parse_type_suffix2(parser, s, type), 0);
            t->loc = (SourceRange){ open_brace, tokens_get_last_location(s) };
            t->array_count_lexer_pos = current;
        } else {
            size_t depth = 0;
            size_t* counts = tls_save();
            tokens_prev(s);

            do {
                tokens_next(s);

                long long count;
                if (tokens_get(s)->type == ']') {
                    count = 0;
                    tokens_next(s);
                } else if (tokens_get(s)->type == '*') {
                    count = 0;
                    tokens_next(s);
                    expect_char(s, ']');
                } else {
                    count = parse_const_expr2(parser, s);
                    expect_char(s, ']');
                }

                tls_push(sizeof(size_t));
                counts[depth++] = count;
            } while (!tokens_eof(s) && tokens_get(s)->type == '[');

            t = cuik_canonical_type(type);
            size_t expected_size = t->size;
            while (depth--) {
                assert(t->size == expected_size);

                uint64_t a = expected_size;
                uint64_t b = counts[depth];
                uint64_t result = a * b;

                // size checks
                if (result >= INT32_MAX) {
                    diag_err(s, t->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
                    continue;
                }

                t = cuik__new_array(&parser->types, cuik_uncanonical_type(t), counts[depth]);
                expected_size = result;
            }

            tls_restore(counts);
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
