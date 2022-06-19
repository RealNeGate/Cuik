static intmax_t eval_l12(Cuik_CPP* restrict c, TokenStream* restrict s);

static intmax_t eval(Cuik_CPP* restrict c, TokenStream* restrict s, Lexer* l, SourceLocIndex parent_loc) {
    // Expand
    if (l) {
        size_t old_tokens_length = arrlen(s->tokens);
        s->current = old_tokens_length;

        expand(c, s, l, SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0));
        assert(s->current != arrlen(s->tokens) && "Expected the macro expansion to add something");

        // Insert a null token at the end
        Token t = {0, arrlen(s->locations) - 1, NULL, NULL};
        arrput(s->tokens, t);

        // Evaluate
        intmax_t result = eval_l12(c, s);

        arrsetlen(s->tokens, old_tokens_length);
        s->current = 0;
        return result;
    } else {
        return eval_l12(c, s);
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
        IntSuffix suffix;
        val = parse_int(t->end - t->start, (const char*)t->start, &suffix);

        tokens_next(s);
    } else if (t->type == TOKEN_IDENTIFIER) {
        assert(!is_defined(c, t->start, t->end - t->start));

        val = 0;
        tokens_next(s);
    } else if (t->type == TOKEN_STRING_SINGLE_QUOTE) {
        int ch;
        intptr_t distance = parse_char(t->end - t->start, (const char*)t->start, &ch);
        if (distance < 0) {
            report(REPORT_ERROR, s, t->location, "could not parse char literal");
            abort();
        }

        val = ch;
        tokens_next(s);
    } else if (t->type == '(') {
        tokens_next(s);
        val = eval(c, s, NULL, t->location);

        if (tokens_get(s)->type != ')') {
            report_two_spots(REPORT_ERROR, s, t->location, tokens_get(s)->location,
                             "expected closing parenthesis for macro subexpression",
                             "open", "close?", NULL);
            abort();
        }
        tokens_next(s);
    } else {
        report(REPORT_ERROR, s, t->location, "could not parse expression");
        abort();
    }

    return flip ? !val : val;
}

static intmax_t eval_l5(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l0(c, s);

    while (tokens_get(s)->type == TOKEN_LEFT_SHIFT ||
           tokens_get(s)->type == TOKEN_RIGHT_SHIFT) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l0(c, s);
        if (t == TOKEN_LEFT_SHIFT)
            left = (left << right);
        else
            left = (uintmax_t)((uintmax_t)left >> (uintmax_t)right);
    }

    return left;
}

static intmax_t eval_l6(Cuik_CPP* restrict c, TokenStream* restrict s) {
    intmax_t left = eval_l5(c, s);

    while (tokens_get(s)->type == '>' ||
           tokens_get(s)->type == '<' ||
           tokens_get(s)->type == TOKEN_GREATER_EQUAL ||
           tokens_get(s)->type == TOKEN_LESS_EQUAL) {
        int t = tokens_get(s)->type;
        tokens_next(s);

        intmax_t right = eval_l5(c, s);
        switch (t) {
            case '>':
            left = left > right;
            break;
            case '<':
            left = left < right;
            break;
            case TOKEN_GREATER_EQUAL:
            left = left >= right;
            break;
            case TOKEN_LESS_EQUAL:
            left = left <= right;
            break;
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
