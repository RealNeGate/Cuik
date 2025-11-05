////////////////////////////////
// TYPES
////////////////////////////////
static Decl parse_declarator2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);
static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Decl parse_declarator_glsl(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);

static Cuik_Attribute* parse_attributes(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_Attribute* last) {
    for (;;) {
        if (tokens_peek_double_token(s, '[')) {
            // C23 attribute:
            //
            //   [[foo]]  [[foo::bar]]  [[foo(1, 3)]]
            Cuik_Attribute* a = TB_ARENA_ALLOC(parser->arena, Cuik_Attribute);
            a->prev = last;
            a->loc.start = tokens_get_location(s);

            // TODO(NeGate): we'll only handle the identifier case with no :: for now
            tokens_next(s), tokens_next(s);
            if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
                diag_err(s, tokens_get_range(s), "expected an identifier");
            } else {
                a->name = tokens_get(s)->atom;
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
            expect_char(s, '(');

            Cuik_Attribute* a = TB_ARENA_ALLOC(parser->arena, Cuik_Attribute);
            a->prev = last;
            a->loc.start = tokens_get_location(s);

            Token* t = tokens_get(s);
            a->name = t->type == TOKEN_IDENTIFIER ? t->atom : NULL;

            int depth = 1;
            while (depth) {
                if (tokens_get(s)->type == '(') {
                    depth++;
                } else if (tokens_get(s)->type == ')') {
                    depth--;
                }

                tokens_next(s);
            }

            expect_char(s, ')');
            a->loc.end = tokens_get_location(s);
            last = a;
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

static bool is_typename(Cuik_Parser* restrict parser, TokenStream* restrict s) {
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

        // GLSL specific types, they'll only be recognized in GLSL contexts so
        // we don't need to version check
        case TOKEN_KW_vec2:
        case TOKEN_KW_vec3:
        case TOKEN_KW_vec4:
        case TOKEN_KW_dvec2:
        case TOKEN_KW_dvec3:
        case TOKEN_KW_dvec4:
        case TOKEN_KW_uvec2:
        case TOKEN_KW_uvec3:
        case TOKEN_KW_uvec4:
        case TOKEN_KW_ivec2:
        case TOKEN_KW_ivec3:
        case TOKEN_KW_ivec4:
        return true;

        case TOKEN_IDENTIFIER: {
            // good question...
            Symbol* loc = find_symbol(parser, s);
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
    bool noret = false;
    PendingExpr* alignas_pending_expr = NULL;

    // type-specifier:
    //   void _Bool char short int long float double
    // storage-class-specifier:
    //   _Thread_local typedef extern static inline auto register _Noreturn
    static struct {
        const char* name;
        int index;
        int cap;
    } simple_rules[] = {
        // primitive base types
        { "void",     BIN_BASE_TYPE },
        { "_Bool",    BIN_BASE_TYPE },
        { "char",     BIN_BASE_TYPE },
        { "int",      BIN_BASE_TYPE },
        { "float",    BIN_BASE_TYPE },
        { "double",   BIN_BASE_TYPE },
        // qualifiers
        { "unsigned", UNSIGNED      },
        { "signed",   SIGNED        },
        { "volatile", VOLATILE      },
        { "const",    CONST         },
        // counting qualifiers
        { "short",    SHORT         },
        { "long",     LONG          },
    };

    enum {
        BIN_BASE_TYPE,
        BIN_SHORT,
        BIN_LONG,
        BIN_MAX,
    };

    int bins[BIN_MAX] = { 0 };

    // Combine identifiers to make type specifier
    while (tokens_get(s)->type == TOKEN_IDENTIFIER) {
        Atom atom = tokens_get(s)->atom;

        if (0) {}
        else if (atom == atom_short)  { counter += SHORT; }
        else if (atom == atom_long)   { counter += LONG; }
        else if (atom == atom_char)   { counter += CHAR; }
        else if (atom == atom__Bool)  { counter += BOOL; }
        else if (atom == atom_int)    { counter += INT; }
        else if (atom == atom_long)   { counter += LONG; }
        else if (atom == atom_float)  { counter += FLOAT; }
        else if (atom == atom_double) { counter += DOUBLE; }
        else if (atom == atom__Complex || atom == atom__Imaginary) {
            diag_err(s, loc, "Complex types are not supported in CuikC");
        }
    }

    // FANCY OPS:
    //  __declspec
    //  enum
    //  struct/union
    //  _Typeof
    //  _Vector
    //  _Atomic

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
            case TOKEN_KW_Noreturn:     attr->is_noret = true;   break;

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

                if (tokens_match(s, atom_noreturn)) {
                    tokens_next(s);
                    expect_char(s, ')');
                } else {
                    int depth = 1;
                    while (depth) {
                        if (tokens_get(s)->type == '(') {
                            depth++;
                        } else if (tokens_get(s)->type == ')') {
                            depth--;
                        }

                        tokens_next(s);
                    }
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

                intmax_t count = parse_const_expr(parser, s);

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
                    if (is_typename(parser, s)) {
                        type = cuik_canonical_type(parse_typename2(parser, s));
                    } else {
                        // we don't particularly resolve typeof for expressions immediately.
                        // instead we just wait until all symbols are resolved properly
                        // Expr* src = NULL; // parse_expr(tu, s);
                        // type = cuik__new_typeof(&parser->types, src);
                        abort();
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
                    name = tokens_get(s)->atom;
                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    bool in_scope;
                    type = name ? CUIK_SYMTAB_LOOKUP2(parser->tags, name, &in_scope, Cuik_Type*) : 0;
                    if (type) {
                        // can't re-complete a enum
                        size_t count = type->enumerator.count;
                        if (count) {
                            if (in_scope) {
                                diag_err(s, tokens_get_range(s), "cannot recomplete an enumerator");
                                diag_note(s, type->loc, "see here");
                                break;
                            }

                            type = type_alloc(&parser->types, true);
                            *type = (Cuik_Type){ .kind = KIND_ENUM, .enumerator = { name } };

                            *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
                        }
                    } else {
                        type = type_alloc(&parser->types, true);
                        *type = (Cuik_Type){ .kind = KIND_ENUM, .enumerator = { name } };

                        if (name != NULL) {
                            *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
                        }
                    }

                    // starts at zero and after any entry it increments
                    // you can override it by using:
                    //   identifier = int-const-expr
                    int cursor = 0;

                    size_t count = 0;
                    EnumEntry* start = tls_save();

                    while (tokens_get(s)->type != '}') {
                        // parse name
                        Token* t = tokens_get(s);
                        if (t->type != TOKEN_IDENTIFIER) {
                            diag_err(s, tokens_get_range(s), "expected identifier for enum name entry.");
                        }

                        Atom name = t->atom;
                        tokens_next(s);

                        int lexer_pos = 0;
                        bool init = false;
                        if (tokens_get(s)->type == '=') {
                            tokens_next(s);
                            init = true;

                            if (parser->is_in_global_scope) {
                                ptrdiff_t lexer_end;
                                lexer_pos = skip_expression_in_enum2(s, &lexer_end);

                                if (lexer_pos < 0) {
                                    diag_err(s, tokens_get_range(s), "expected comma or } (got EOF)");
                                    break;
                                }
                            } else {
                                cursor = parse_const_expr(parser, s);
                            }
                        }

                        // Allocate into temporary buffer
                        tls_push(sizeof(EnumEntry));
                        start[count] = (EnumEntry){ name, init, lexer_pos, cursor };

                        Symbol sym = {
                            .name = name,
                            .type = cuik_uncanonical_type(type),
                            .loc = get_token_range(t),
                            .storage_class = STORAGE_ENUM,
                            .enum_value = count
                        };

                        // push symbol
                        *CUIK_SYMTAB_PUT(parser->symbols, name, Symbol) = sym;
                        if (!parser->is_in_global_scope) {
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
                    type->enumerator.count = count;
                    type->enumerator.entries = copy_out_temporary(parser->arena, start, count, sizeof(EnumEntry));

                    if (!parser->is_in_global_scope) {
                        type_layout2(parser, &parser->tokens, type);
                    }
                } else {
                    if (name == NULL) {
                        diag_err(s, tokens_get_range(s), "expected { after enum declaration");
                        break;
                    }

                    type = CUIK_SYMTAB_LOOKUP(parser->tags, name, Cuik_Type*);
                    if (type == NULL) {
                        type = type_alloc(&parser->types, true);
                        *type = (Cuik_Type){ .kind = KIND_ENUM, .enumerator = { name } };
                        *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
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
                    name = tokens_get(s)->atom;
                    tokens_next(s);
                }

                if (tokens_get(s)->type == '{') {
                    tokens_next(s);

                    bool in_scope;
                    Cuik_Type* old_type = name ? CUIK_SYMTAB_LOOKUP2(parser->tags, name, &in_scope, Cuik_Type*) : 0;

                    type = old_type;
                    if (type) {
                        // can't re-complete a enum
                        size_t count = type->enumerator.count;
                        if (count) {
                            if (in_scope) {
                                diag_warn(s, record_loc, "struct was declared somewhere else");
                                diag_note(s, type->loc, "see here %p", type);
                                break;
                            }

                            type = type_alloc(&parser->types, true);
                            *type = (Cuik_Type){
                                .kind = is_union ? KIND_UNION : KIND_STRUCT,
                                .loc = record_loc,
                                .record = { name }
                            };

                            *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
                        }
                    } else {
                        type = type_alloc(&parser->types, true);
                        *type = (Cuik_Type){
                            .kind = is_union ? KIND_UNION : KIND_STRUCT,
                            .loc = record_loc,
                            .record = { name }
                        };

                        // can't forward decl unnamed records so we don't track it
                        if (name != NULL) {
                            *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
                        }
                    }

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
                                member->bit_width = parse_const_expr(parser, s);
                            }

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
                    Member* permanent_store = copy_out_temporary(parser->arena, members, member_count, sizeof(Member));
                    type->record = (struct Cuik_TypeRecord){
                        .name = name, .kid_count = member_count, .kids = permanent_store, .nominal = type
                    };

                    if (!parser->is_in_global_scope) {
                        type_layout2(parser, &parser->tokens, type);
                    }
                } else {
                    // refers to a complete version of the record (which may or may not be ready yet)
                    if (name == NULL) {
                        diag_err(s, record_loc, "Cannot have unnamed forward struct reference.");
                        tokens_next(s);
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    type = CUIK_SYMTAB_LOOKUP(parser->tags, (Cuik_Atom) name, Cuik_Type*);
                    if (type == NULL) {
                        type = type_alloc(&parser->types, true);
                        *type = (Cuik_Type){
                            .kind = is_union ? KIND_UNION : KIND_STRUCT,
                            .loc = record_loc,
                            .record = { name, .nominal = type }
                        };

                        *CUIK_SYMTAB_PUT(parser->tags, name, Cuik_Type*) = type;
                    }

                    // push back one because we push it forward one later but
                    // shouldn't
                    tokens_prev(s);
                }

                break;
            }

            case TOKEN_IDENTIFIER: {
                if (counter) goto done;

                Token* t = tokens_get(s);
                Symbol* old_def = cuik_symtab_lookup(parser->symbols, t->atom);
                if (old_def != NULL) {
                    // if the typename is already defined, then reuse that type index
                    if (old_def->storage_class != STORAGE_TYPEDEF) {
                        diag_err(s, tokens_get_range(s), "symbol '%s' is not a typedef.", t->atom);
                        diag_note(s, old_def->loc, "declared here");
                        return CUIK_QUAL_TYPE_NULL;
                    }

                    type = cuik_canonical_type(old_def->type);
                    quals |= cuik_get_quals(old_def->type);
                    counter += OTHER;
                    break;
                } else {
                    if (parser->is_in_global_scope) {
                        // if not defined, we assume this must be a typedef'd type and reserve space
                        type = type_alloc(&parser->types, true);
                        *type = (Cuik_Type){
                            .kind = KIND_PLACEHOLDER,
                            .loc = get_token_range(t),
                            .placeholder = { t->atom },
                        };

                        // insert into placeholder list (we'll use this to check for unresolved types later)
                        type->placeholder.next = parser->first_placeholder;
                        parser->first_placeholder = type;

                        Symbol sym = {
                            .name = t->atom,
                            .type = cuik_uncanonical_type(type),
                            .loc = type->loc,
                            .storage_class = STORAGE_TYPEDEF,
                        };
                        counter += OTHER;

                        *CUIK_SYMTAB_PUT(parser->symbols, t->atom, Symbol) = sym;
                        break;
                    } else {
                        // if not a typename, this isn't a typedecl
                        goto done;
                    }
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
            type = &cuik__builtin_float;
            break;

            case DOUBLE: case LONG + DOUBLE:
            type = &cuik__builtin_double;
            break;

            case OTHER:
            assert(type);
            break;

            default: {
                Token* last = &s->list.tokens[s->list.current];
                diag_err(s, (SourceRange){ loc.start, tokens_get_last_location(s) }, "unknown typename %s", last->atom);
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
        diag_err(s, loc, "unknown typename %s", last->atom);
        tokens_next(s);
        return CUIK_QUAL_TYPE_NULL;
    }

    if (forced_align && type->align != forced_align) {
        if (forced_align < type->align) {
            diag_err(s, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, type->align);
            return CUIK_QUAL_TYPE_NULL;
        }

        // clone it since we need to modify it
        type = type_clone(&parser->types, type, type->also_known_as);
        type->loc = loc;
        type->align = alignas_pending_expr ? -1 : forced_align;

        if (type->align < 0) {
            alignas_pending_expr->dst = &type->align;
        }
    }

    return cuik_make_qual_type(type, quals);
}

static Cuik_QualType parse_typename2(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    if (parser->version != CUIK_VERSION_GLSL) {
        // TODO(NeGate): Check if attributes are set, they shouldn't
        // be in this context.
        Attribs attr = { 0 };
        Cuik_QualType type = parse_declspec2(parser, s, &attr);
        return parse_declarator2(parser, s, type, true).type;
    } else {
        Cuik_QualType type = cuik_uncanonical_type(parse_glsl_type(parser, s));
        return parse_declarator_glsl(parser, s, type, true).type;
    }
}

static Cuik_QualType parse_type_suffix2(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type) {
    Token* t = tokens_get(s);
    if (t->type == '(') {
        // function type
        // void foo(int x)
        //         ^^^^^^^
        SourceLoc opening_loc = tokens_get_location(s);
        tokens_next(s);

        if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
            // this is required pre-C23 to say no parameters (empty parens meant undefined)
            tokens_next(s);
            tokens_next(s);

            Cuik_Type* t = type_alloc(&parser->types, false);
            *t = (Cuik_Type){
                .kind  = KIND_FUNC,
                .size  = 1,
                .align = 1,
                .flags = CUIK_TYPE_FLAG_COMPLETE,
                .func = { .return_type = type },
            };
            type = cuik_uncanonical_type(t);
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
                    param_type = cuik_uncanonical_type(cuik__new_pointer(&parser->types, param_type_canon->array.of));
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

            // Before C23 empty parameter lists mean undefined set of parameters
            // we're gonna stick with that for now...
            if (parser->version < CUIK_VERSION_C23 && param_count == 0) {
                has_varargs = true;
            }

            Cuik_Type* t = type_alloc(&parser->types, false);
            *t = (Cuik_Type){
                .kind  = KIND_FUNC,
                .size  = 1,
                .align = 1,
                .flags = parser->is_in_global_scope ? 0 : CUIK_TYPE_FLAG_COMPLETE,
                .has_varargs = has_varargs,
                .func = {
                    .return_type = type,
                    .param_list = copy_out_temporary(parser->arena, params, param_count, sizeof(Param)),
                    .param_count = param_count,
                }
            };

            type = cuik_uncanonical_type(t);
        }
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
            t->array.count_lexer_pos = current;
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
                    count = parse_const_expr(parser, s);
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

static Decl parse_declarator_glsl(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_QualType type, bool is_abstract) {
    assert(!CUIK_QUAL_TYPE_IS_NULL(type));
    SourceLoc start_loc = tokens_get_location(s);

    Atom name = NULL;
    Token* t = tokens_get(s);
    if (!is_abstract && t->type == TOKEN_IDENTIFIER) {
        // simple name
        name = t->atom;
        tokens_next(s);
    }

    // Handle suffixes like [] or ()
    type = parse_type_suffix2(parser, s, type);

    SourceLoc end_loc = tokens_get_last_location(s);
    return (Decl){ type, name, { start_loc, end_loc } };
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
        name = t->atom;
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
        parse_declarator2(parser, s, cuik_uncanonical_type(&cuik__builtin_void), is_abstract);
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

    SourceLoc end_loc = tokens_get_last_location(s);
    return (Decl){ type, name, { start_loc, end_loc } };
}
