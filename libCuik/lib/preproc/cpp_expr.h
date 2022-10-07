static intmax_t eval_l13(Cuik_CPP* restrict c, TokenStream* restrict s);

// same as expand(...) except that it converts 'defined(MACRO)' and 'defined MACRO' into 0 or 1
static void expand_with_defined(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc) {
    int depth = 0;

    while (!tokens_is(in, 0) && !tokens_hit_line(in)) {
        if (tokens_is(in, TOKEN_IDENTIFIER)) {
            Token* t = tokens_get(in);
            if (strncmp((const char*) t->start, "defined", t->end - t->start) == 0) {
                tokens_next(in);

                String str = { 0 };
                if (tokens_is(in, '(')) {
                    tokens_next(in);

                    if (!tokens_is(in, TOKEN_IDENTIFIER)) {
                        generic_error(in, "expected identifier!");
                    }

                    Token* t = tokens_get(in);
                    str = string_from_range(t->start, t->end);

                    tokens_next(in);
                    expect(in, ')');
                } else if (tokens_is(in, TOKEN_IDENTIFIER)) {
                    Token* t = tokens_get(in);
                    str = string_from_range(t->start, t->end);
                    tokens_next(in);
                } else {
                    generic_error(in, "expected identifier for 'defined'!");
                }

                bool found = is_defined(c, str.data, str.length);

                // we really just allocated like two bytes just to store this lmao
                unsigned char* out = gimme_the_shtuffs(c, 2);
                out[0] = found ? '1' : '0';
                out[1] = '\0';

                Token t = {
                    TOKEN_INTEGER, false,
                    get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL),
                    out, out + 1,
                };
                dyn_array_put(s->tokens, t);
                continue;
            }
        }

        Token t = *tokens_get(in);
        t.location = get_source_location(c, in, s, parent_loc, SOURCE_LOC_NORMAL);
        dyn_array_put(s->tokens, t);
        tokens_next(in);
    }
}

static intmax_t eval(Cuik_CPP* restrict c, TokenStream* restrict s, TokenStream* restrict in, SourceLocIndex parent_loc) {
    // Expand
    if (in) {
        size_t old_tokens_length = dyn_array_length(s->tokens);
        s->current = old_tokens_length;
        {
            TokenStream scratch = { 0 };
            scratch.locations = dyn_array_create_with_initial_cap(SourceLoc, 32);
            scratch.tokens = dyn_array_create_with_initial_cap(Token, 32);

            // We need to get rid of the defined(MACRO) and defined MACRO before we
            // do real expansion or else it'll give us incorrect results
            expand_with_defined(c, &scratch, in, parent_loc);
            // This type of expansion is temporary
            expand(c, s, &scratch, dyn_array_length(scratch.tokens), true, parent_loc);
            free_token_stream(&scratch);
        }
        assert(s->current != dyn_array_length(s->tokens) && "Expected the macro expansion to add something");

        // Insert a null token at the end
        Token t = {0, true, dyn_array_length(s->locations) - 1, NULL, NULL};
        dyn_array_put(s->tokens, t);

        // Evaluate
        s->current = old_tokens_length;
        intmax_t result = eval_l13(c, s);

        // Restore stuff
        dyn_array_set_length(s->tokens, old_tokens_length);
        s->current = old_tokens_length;
        return result;
    } else {
        return eval_l13(c, s);
    }
}

static intmax_t eval_l0(Cuik_CPP* restrict c, TokenStream* restrict s) {
    bool flip = false;
    while (tokens_get(s)->type == '!') {
        flip = !flip;
        tokens_next(s);
    }

    intmax_t val;
    Token* t = tokens_get(s);
    if (t->type == TOKEN_INTEGER) {
        Cuik_IntSuffix suffix;
        val = parse_int(t->end - t->start, (const char*)t->start, &suffix);

        tokens_next(s);
    } else if (t->type == TOKEN_IDENTIFIER) {
        assert(!is_defined(c, t->start, t->end - t->start));
        val = 0;
        tokens_next(s);
    } else if (t->type == TOKEN_STRING_SINGLE_QUOTE) {
        int ch;
        ptrdiff_t distance = parse_char(t->end - t->start, (const char*)t->start, &ch);
        if (distance < 0) {
            report(REPORT_ERROR, NULL, s, t->location, "could not parse char literal");
            abort();
        }

        val = ch;
        tokens_next(s);
    } else if (t->type == '(') {
        tokens_next(s);
        val = eval(c, s, NULL, t->location);

        if (tokens_get(s)->type != ')') {
            report_two_spots(REPORT_ERROR, NULL, s, t->location, tokens_get(s)->location,
                "expected closing parenthesis for macro subexpression",
                "open", "close?", NULL);
            abort();
        }
        tokens_next(s);
    } else {
        report(REPORT_ERROR, NULL, s, t->location, "could not parse expression");
        abort();
    }

    return flip ? !val : val;
}

static intmax_t eval_l2(Cuik_CPP* restrict c, TokenStream* restrict s) {
    if (tokens_get(s)->type == '-') {
        return -eval_l0(c, s);
    } else if (tokens_get(s)->type == '+') {
        tokens_next(s);
        return eval_l0(c, s);
    } else {
        return eval_l0(c, s);
    }
}

static intmax_t eval_l4(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l2(c, s);

    while (tokens_get(s)->type == '+' ||
        tokens_get(s)->type == '-') {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l2(c, s);
        if (t == '+')
            left = (left + right);
        else
            left = (left - right);
    }

    return left;
}

static intmax_t eval_l5(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l4(c, s);

    while (tokens_get(s)->type == TOKEN_LEFT_SHIFT ||
        tokens_get(s)->type == TOKEN_RIGHT_SHIFT) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l4(c, s);
        if (t == TOKEN_LEFT_SHIFT)
            left = (left << right);
        else
            left = (uintmax_t)((uintmax_t)left >> (uintmax_t)right);
    }

    return left;
}

static intmax_t eval_l6(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l5(c, s);

    while (tokens_get(s)->type == '>' || tokens_get(s)->type == '<' || tokens_get(s)->type == TOKEN_GREATER_EQUAL || tokens_get(s)->type == TOKEN_LESS_EQUAL) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l5(c, s);
        switch (t) {
            case '>': left = left > right; break;
            case '<': left = left < right; break;
            case TOKEN_GREATER_EQUAL: left = left >= right; break;
            case TOKEN_LESS_EQUAL: left = left <= right; break;
        }
    }

    return left;
}

static intmax_t eval_l7(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l6(c, s);

    while (tokens_get(s)->type == TOKEN_NOT_EQUAL ||
        tokens_get(s)->type == TOKEN_EQUALITY) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l6(c, s);
        if (t == TOKEN_EQUALITY)
            left = (left == right);
        else
            left = (left != right);
    }

    return left;
}

static intmax_t eval_l8(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l7(c, s);

    while (tokens_get(s)->type == '&') {
        tokens_next(s);

        intmax_t right = eval_l7(c, s);
        left = left & right;
    }

    return left;
}

static intmax_t eval_l9(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l8(c, s);

    while (tokens_get(s)->type == '^') {
        tokens_next(s);

        intmax_t right = eval_l8(c, s);
        left = left ^ right;
    }

    return left;
}

static intmax_t eval_l10(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l9(c, s);

    while (tokens_get(s)->type == '|') {
        tokens_next(s);

        intmax_t right = eval_l9(c, s);
        left = left | right;
    }

    return left;
}

static intmax_t eval_l11(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l10(c, s);

    while (tokens_get(s)->type == TOKEN_DOUBLE_AND) {
        tokens_next(s);

        intmax_t right = eval_l10(c, s);
        left = left && right;
    }

    return left;
}

static intmax_t eval_l12(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l11(c, s);

    while (tokens_get(s)->type == TOKEN_DOUBLE_OR) {
        tokens_next(s);

        intmax_t right = eval_l11(c, s);
        left = left || right;
    }

    return left;
}

static intmax_t eval_l13(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t lhs = eval_l12(c, s);

    if (tokens_get(s)->type == '?') {
        tokens_next(s);

        intmax_t mhs = eval_l13(c, s);
        if (tokens_get(s)->type != ':') {
            report(REPORT_ERROR, NULL, s, tokens_get_location_index(s), "expected : for ternary");
            abort();
        }
        tokens_next(s);

        intmax_t rhs = eval_l13(c, s);
        return lhs ? mhs : rhs;
    }

    return lhs;
}
