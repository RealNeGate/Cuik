////////////////////////////////
// TYPES
////////////////////////////////
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
    if (tokens_get(s)->type == TOKEN_KW_declspec ||
        tokens_get(s)->type == TOKEN_KW_Pragma) {
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
