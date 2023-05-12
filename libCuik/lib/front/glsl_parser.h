static int parse_glsl_layout_attrib(TokenStream* restrict s, const SourceRange* r, Atom key, intmax_t value) {
    if (value < 0) {
        diag_err(s, *r, "layout '%s' cannot be negative or missing.", key);
        return -1;
    } else {
        return value;
    }
}

// this code might reference docs/glsl_grammar.txt
static Cuik_GlslQuals* parse_glsl_qualifiers(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    Cuik_GlslQuals* glsl = ARENA_ALLOC(&local_ast_arena, Cuik_GlslQuals);

    for (;;) {
        TknType tkn_type = tokens_get(s)->type;
        switch (tkn_type) {
            // storage_qualifier
            case TOKEN_KW_in:    tokens_next(s); glsl->storage = CUIK_GLSL_QUALS_IN; break;
            case TOKEN_KW_out:   tokens_next(s); glsl->storage = CUIK_GLSL_QUALS_OUT; break;
            case TOKEN_KW_inout: tokens_next(s); glsl->storage = CUIK_GLSL_QUALS_INOUT; break;

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
                        diag_err(s, r, "layout '%s' does not match any options. (https://www.khronos.org/opengl/wiki/Layout_Qualifier_(GLSL))", key);
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

// declarator               ::= IDENTIFIER array_specifier? ('=' initializer)?
// init_declarator_list     ::= fully_specified_type (declarator (',' declarator)*)?
static ParseResult parse_decl_glsl(Cuik_Parser* restrict parser, TokenStream* restrict s) {
    SourceLoc loc = tokens_get_location(s);

    // fully_specified_type ::= type_qualifier? type_specifier
    Cuik_GlslQuals* glsl = parse_glsl_qualifiers(parser, s);

    Attribs attr = { 0 };
    Cuik_QualType type = parse_declspec2(parser, s, &attr);
    if (CUIK_QUAL_TYPE_IS_NULL(type)) {
        diag_err(s, (SourceRange){ loc, tokens_get_last_location(s) }, "could not parse base type.");
        tokens_next(s);

        type = cuik_uncanonical_type(parser->default_int);
    }

    __debugbreak();
    return PARSE_SUCCESS;
}
