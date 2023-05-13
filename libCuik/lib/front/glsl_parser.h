static int parse_glsl_layout_attrib(TokenStream* restrict s, const SourceRange* r, Atom key, intmax_t value) {
    if (value < 0) {
        diag_err(s, *r, "layout '%s' cannot be negative or missing.", key);
        return -1;
    } else {
        return value;
    }
}

// this code might reference docs/glsl_grammar.txt
static Cuik_GlslQuals* parse_glsl_qualifiers(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_Qualifiers* quals) {
    Cuik_GlslQuals* glsl = ARENA_ALLOC(&local_ast_arena, Cuik_GlslQuals);

    for (;;) {
        TknType tkn_type = tokens_get(s)->type;
        switch (tkn_type) {
            // storage_qualifier
            case TOKEN_KW_in:      tokens_next(s); glsl->storage = CUIK_GLSL_STORAGE_IN; break;
            case TOKEN_KW_out:     tokens_next(s); glsl->storage = CUIK_GLSL_STORAGE_OUT; break;
            case TOKEN_KW_inout:   tokens_next(s); glsl->storage = CUIK_GLSL_STORAGE_INOUT; break;
            case TOKEN_KW_buffer:  tokens_next(s); glsl->storage = CUIK_GLSL_STORAGE_BUFFER; break;
            case TOKEN_KW_uniform: tokens_next(s); glsl->storage = CUIK_GLSL_STORAGE_UNIFORM; break;

            // interpolation_qualifier  ::= 'smooth' | 'flat' | 'noperspective'
            case TOKEN_KW_flat:          tokens_next(s); glsl->interp = CUIK_GLSL_FLAT; break;
            case TOKEN_KW_smooth:        tokens_next(s); glsl->interp = CUIK_GLSL_SMOOTH; break;
            case TOKEN_KW_noperspective: tokens_next(s); glsl->interp = CUIK_GLSL_NOPERSPECTIVE; break;

            case TOKEN_KW_const:    *quals |= CUIK_QUAL_CONST;    break;
            case TOKEN_KW_restrict: *quals |= CUIK_QUAL_RESTRICT; break;
            case TOKEN_KW_volatile: *quals |= CUIK_QUAL_VOLATILE; break;

            // layout_qualifier         ::= 'layout' '(' layout_qualifier_id_list ')'
            // layout_qualifier_id_list ::= layout_qualifier_id (',' layout_qualifier_id)*
            // layout_qualifier_id      ::= IDENTIFIER | IDENTIFIER '=' constant_expression | 'shared'
            case TOKEN_KW_layout: {
                tokens_next(s);

                SourceLoc opening_loc = tokens_get_location(s);
                if (!expect_char(s, '(')) goto done;

                while (!tokens_eof(s) && tokens_get(s)->type != ')') {
                    SourceLoc start = tokens_get_location(s);

                    Token* t = tokens_get(s);
                    Atom key = atoms_put(t->content.length, t->content.data);
                    tokens_next(s);

                    intmax_t value = -1;
                    if (tokens_get(s)->type == '=') {
                        tokens_next(s);
                        value = parse_const_expr(parser, s);
                    }

                    bool success = false;
                    SourceRange r = { start, tokens_get_last_location(s) };

                    #define X(name) if (key == parser->glsl.name) { glsl->name = parse_glsl_layout_attrib(s, &r, #name, value); success = (glsl->name >= 0); }
                    X(binding);
                    X(location);
                    X(offset);
                    #undef X

                    if (!success) {
                        diag_err(s, r, "layout '%s' does not match any options. https://www.khronos.org/opengl/wiki/Layout_Qualifier_(GLSL)", key);
                    }

                    if (tokens_get(s)->type == ',') {
                        tokens_next(s);
                        continue;
                    } else {
                        break;
                    }
                }

                expect_closing_paren(s, opening_loc);
                break;
            }

            default: goto done;
        }
    }

    done:
    return glsl;
}

static Cuik_Type* parse_glsl_type(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    Cuik_Type* int_type  = (Cuik_Type*) &parser->target->signed_ints[CUIK_BUILTIN_INT];
    Cuik_Type* uint_type = (Cuik_Type*) &parser->target->unsigned_ints[CUIK_BUILTIN_INT];

    Token* t = tokens_get(s);
    tokens_next(s);

    switch (t->type) {
        case TOKEN_KW_void:   return &cuik__builtin_void;
        case TOKEN_KW_Bool:   return &cuik__builtin_bool;
        case TOKEN_KW_uint:   return uint_type;
        case TOKEN_KW_int:    return int_type;
        case TOKEN_KW_float:  return &cuik__builtin_float;
        case TOKEN_KW_double: return &cuik__builtin_double;

        // vecn
        case TOKEN_KW_vec2:  return cuik__new_vector2(&parser->types, &cuik__builtin_float, 2);
        case TOKEN_KW_vec3:  return cuik__new_vector2(&parser->types, &cuik__builtin_float, 3);
        case TOKEN_KW_vec4:  return cuik__new_vector2(&parser->types, &cuik__builtin_float, 4);

        // dvecn
        case TOKEN_KW_dvec2: return cuik__new_vector2(&parser->types, &cuik__builtin_double, 2);
        case TOKEN_KW_dvec3: return cuik__new_vector2(&parser->types, &cuik__builtin_double, 3);
        case TOKEN_KW_dvec4: return cuik__new_vector2(&parser->types, &cuik__builtin_double, 4);

        // uvecn
        case TOKEN_KW_uvec2: return cuik__new_vector2(&parser->types, uint_type, 2);
        case TOKEN_KW_uvec3: return cuik__new_vector2(&parser->types, uint_type, 3);
        case TOKEN_KW_uvec4: return cuik__new_vector2(&parser->types, uint_type, 4);

        // ivecn
        case TOKEN_KW_ivec2: return cuik__new_vector2(&parser->types, int_type, 2);
        case TOKEN_KW_ivec3: return cuik__new_vector2(&parser->types, int_type, 3);
        case TOKEN_KW_ivec4: return cuik__new_vector2(&parser->types, int_type, 4);

        default:
        diag_err(s, get_token_range(t), "unknown type name. https://www.khronos.org/opengl/wiki/Data_Type_(GLSL)", t->content);
        return NULL;
    }
}

// fully_specified_type     ::= type_qualifier? type_specifier
// declarator               ::= IDENTIFIER array_specifier? ('=' initializer)?
// init_declarator_list     ::= fully_specified_type (declarator (',' declarator)*)?
static ParseResult parse_decl_glsl(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    size_t starting_point = s->list.current;
    SourceLoc loc = tokens_get_location(s);

    Cuik_Qualifiers quals = 0;
    Cuik_GlslQuals* glsl = parse_glsl_qualifiers(parser, s, &quals);

    Cuik_QualType type = cuik_make_qual_type(parse_glsl_type(parser, s), quals);
    if (CUIK_QUAL_TYPE_IS_NULL(type)) {
        diag_err(s, (SourceRange){ loc, tokens_get_last_location(s) }, "could not parse base type.");
        tokens_next(s);

        type = cuik_uncanonical_type(parser->default_int);
    }

    bool is_function = false;
    while (!tokens_eof(s) && tokens_get(s)->type != ';') {
        Decl decl = parse_declarator_glsl(parser, s, type, false);

        // Convert into statement
        Stmt* n = alloc_stmt();
        n->op = STMT_GLOBAL_DECL;
        n->loc = decl.loc;
        n->decl = (struct StmtDecl){
            .name = decl.name,
            .glsl_quals = glsl,
            .type = decl.type,
            .attrs = { .is_root = true }
        };

        dyn_array_put(parser->top_level_stmts, n);

        if (decl.name != NULL) {
            // Check for duplicates
            assert(!CUIK_QUAL_TYPE_IS_NULL(decl.type));

            StorageClass st_class;
            if (cuik_canonical_type(decl.type)->kind == KIND_FUNC) {
                st_class = STORAGE_FUNC;
            } else {
                st_class = STORAGE_GLOBAL;
            }

            Symbol* old_def = find_global_symbol(&parser->globals, decl.name);
            Symbol* sym = NULL;
            if (old_def == NULL) {
                ptrdiff_t sym_index = nl_strmap_puti_cstr(parser->globals.symbols, decl.name);
                sym = &parser->globals.symbols[sym_index];
                *sym = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .loc  = decl.loc,
                    .stmt = n,
                    .storage_class = st_class
                };
            } else {
                sym = old_def;
            }

            // it's a function
            ptrdiff_t expr_start, expr_end;
            if (tokens_get(s)->type == '{') {
                if (cuik_canonical_type(decl.type)->kind != KIND_FUNC) {
                    diag_err(s, decl.loc, "cannot add function body to non-function declaration");
                }

                n->op = STMT_FUNC_DECL;
                expr_start = skip_brackets(s, decl.loc, false, &expr_end);
                if (expr_start < 0) {
                    s->list.current = dyn_array_length(s->list.tokens) - 1;
                    return PARSE_WIT_ERRORS;
                }

                is_function = true;
            } else {
                expr_start = expr_end = -1;
            }

            if (expr_start >= 0) {
                sym->token_start = expr_start;
                sym->token_end = expr_end;
                // SourceRange r = { s->list.tokens[expr_start].location, get_token_range(&s->list.tokens[expr_end]).end };
                // diag_note(s, r, "Initializer");
            }
        }

        if (is_function) {
            // function body
            break;
        } else if (tokens_get(s)->type == ',') {
            tokens_next(s);
            continue;
        } else {
            break;
        }
    }

    if (!is_function) {
        expect_char(s, ';');
    }

    // we measure success here on forward progress
    if (starting_point == s->list.current) {
        tokens_next(s);
        return PARSE_WIT_ERRORS;
    }

    return PARSE_SUCCESS;
}
