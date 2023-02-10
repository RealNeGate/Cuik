static intmax_t eval_ternary(Cuik_CPP* restrict c, TokenArray* restrict in);

// same as expand(...) except that it converts 'defined(MACRO)' and 'defined MACRO' into 0 or 1
static bool expand_with_defined(Cuik_CPP* restrict c, TokenArray* restrict out_tokens, TokenArray* restrict in, uint32_t macro_id) {
    int depth = 0;

    for (;;) {
        Token t = consume(in);
        if (t.type == 0 || t.hit_line) {
            in->current -= 1;
            break;
        }

        if (t.type == TOKEN_IDENTIFIER && memeq(t.content.data, t.content.length, "defined", 7)) {
            String str = { 0 };

            t = consume(in);
            if (t.type == '(') {
                t = consume(in);
                if (t.type != TOKEN_IDENTIFIER) {
                    // generic_error(in, "expected identifier!");
                    fprintf(stderr, "expected identifier\n");
                    return false;
                }

                str = t.content;

                t = consume(in);
                if (t.type != ')') {
                    fprintf(stderr, "expected ')'\n");
                    return false;
                }
            } else if (t.type == TOKEN_IDENTIFIER) {
                str = t.content;
            } else {
                // generic_error(in, "expected identifier for 'defined'!");
                fprintf(stderr, "expected identifier for 'defined'!\n");
                return false;
            }

            bool found = is_defined(c, str.data, str.length);

            // we really just allocated like two bytes just to store this lmao
            char* out = gimme_the_shtuffs(c, 2);
            out[0] = found ? '1' : '0';
            out[1] = '\0';

            t.type = TOKEN_INTEGER;
            t.content = string_cstr(out);
            dyn_array_put(out_tokens->tokens, t);
        } else {
            dyn_array_put(out_tokens->tokens, t);
        }
    }

    return true;
}

static intmax_t eval(Cuik_CPP* restrict c, TokenArray* restrict in) {
    // We need to get rid of the defined(MACRO) and defined MACRO before we
    // do real expansion or else it'll give us incorrect results
    __debugbreak();
    return 0;
    /*size_t old_scratch_length = dyn_array_length(c->scratch_list.tokens);
    if (!expand_with_defined(c, &c->scratch_list, in, 0)) {
        // TODO(NeGate): error messages
        abort();
    }
    dyn_array_put(c->scratch_list.tokens, (Token){ 0 });

    size_t old_tokens_length = dyn_array_length(c->tokens.list.tokens);

    // This expansion is temporary
    c->scratch_list.current = old_scratch_length;
    expand(c, &c->tokens.list, &c->scratch_list, 0);
    dyn_array_put(c->tokens.list.tokens, (Token){ 0 });

    // free pass 1 scratch tokens
    dyn_array_set_length(c->scratch_list.tokens, old_scratch_length);

    // Evaluate
    if (old_tokens_length == dyn_array_length(c->tokens.list.tokens)) {
        SourceLoc loc = c->tokens.list.tokens[old_tokens_length - 1].location;
        diag_err(&c->tokens, (SourceRange){ loc, loc }, "expected macro expression");
        return 0;
    }

    c->tokens.list.current = old_tokens_length;
    intmax_t result = eval_ternary(c, &c->tokens.list);

    // free pass 2 scratch tokens
    dyn_array_set_length(c->tokens.list.tokens, old_tokens_length);
    return result;*/
}

static intmax_t eval_unary(Cuik_CPP* restrict c, TokenArray* restrict in) {
    bool flip = false;
    while (peek(in).type == '!') {
        flip = !flip;
        in->current += 1;
    }

    intmax_t val;
    Token t = consume(in);
    if (t.type == TOKEN_INTEGER) {
        Cuik_IntSuffix suffix;
        val = parse_int(t.content.length, (const char*) t.content.data, &suffix);
    } else if (t.type == TOKEN_IDENTIFIER) {
        val = 0;
    } else if (t.type == TOKEN_STRING_SINGLE_QUOTE) {
        int ch;
        ptrdiff_t distance = parse_char(t.content.length, (const char*) t.content.data, &ch);
        if (distance < 0) {
            // report(REPORT_ERROR, NULL, s, t.location, "could not parse char literal");
            abort();
        }

        val = ch;
    } else if (t.type == '(') {
        val = eval_ternary(c, in);

        if (consume(in).type != ')') {
            /*report_two_spots(REPORT_ERROR, NULL, s, t.location, tokens_get(s)->location,
                "expected closing parenthesis for macro subexpression",
                "open", "close?", NULL);*/
            abort();
        }
    } else {
        SourceLoc loc = c->tokens.list.tokens[c->tokens.list.current - 1].location;
        diag_err(&c->tokens, (SourceRange){ loc, loc }, "could macro expression");
        abort();
    }

    return flip ? !val : val;
}

static intmax_t eval_l2(Cuik_CPP* restrict c, TokenArray* restrict in) {
    if (peek(in).type == '-') {
        consume(in);
        return -eval_unary(c, in);
    } else if (peek(in).type == '+') {
        consume(in);
        return eval_unary(c, in);
    } else {
        return eval_unary(c, in);
    }
}

static int get_precendence(TknType ty) {
    switch (ty) {
        case TOKEN_TIMES:
        case TOKEN_SLASH:
        case TOKEN_PERCENT:
        return 100 - 3;

        case TOKEN_PLUS:
        case TOKEN_MINUS:
        return 100 - 4;

        case TOKEN_LEFT_SHIFT:
        case TOKEN_RIGHT_SHIFT:
        return 100 - 5;

        case TOKEN_GREATER_EQUAL:
        case TOKEN_LESS_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_LESS:
        return 100 - 6;

        case TOKEN_EQUALITY:
        case TOKEN_NOT_EQUAL:
        return 100 - 7;

        case TOKEN_AND:
        return 100 - 8;

        case TOKEN_XOR:
        return 100 - 9;

        case TOKEN_OR:
        return 100 - 10;

        case TOKEN_DOUBLE_AND:
        return 100 - 11;

        case TOKEN_DOUBLE_OR:
        return 100 - 12;

        // zero means it's not a binary operator
        default:
        return 0;
    }
}

static intmax_t eval_binop(Cuik_CPP* restrict c, TokenArray* restrict in, int min_prec) {
    // This precendence climber is always left associative
    intmax_t result = eval_l2(c, in);

    int prec;
    TknType binop;

    // It's kinda weird but you don't have to read it because you're a bitch anyways
    while (!at_token_list_end(in)) {
        TknType binop = consume(in).type;
        int prec = get_precendence(binop);
        if (prec == 0 || prec < min_prec) {
            in->current -= 1;
            break;
        }

        intmax_t rhs = eval_binop(c, in, prec + 1);
        switch (binop) {
            case TOKEN_TIMES: result *= rhs; break;
            case TOKEN_SLASH: result /= rhs; break;
            case TOKEN_PERCENT: result %= rhs; break;
            case TOKEN_PLUS: result += rhs; break;
            case TOKEN_MINUS: result -= rhs; break;
            case TOKEN_RIGHT_SHIFT: result >>= rhs; break;
            case TOKEN_LEFT_SHIFT: result <<= rhs; break;
            case TOKEN_GREATER_EQUAL: result = (result >= rhs); break;
            case TOKEN_LESS_EQUAL: result = (result <= rhs); break;
            case TOKEN_GREATER: result = (result > rhs); break;
            case TOKEN_LESS: result = (result < rhs); break;
            case TOKEN_EQUALITY: result = (result == rhs); break;
            case TOKEN_NOT_EQUAL: result = (result != rhs); break;
            case TOKEN_AND: result = (result & rhs); break;
            case TOKEN_XOR: result = (result ^ rhs); break;
            case TOKEN_OR: result = (result | rhs); break;
            case TOKEN_DOUBLE_AND: result = (result && rhs); break;
            case TOKEN_DOUBLE_OR: result = (result || rhs); break;
            default: __builtin_unreachable();
        }
    }

    return result;
}

static intmax_t eval_ternary(Cuik_CPP* restrict c, TokenArray* restrict in) {
    intmax_t lhs = eval_binop(c, in, 0);

    if (peek(in).type == '?') {
        consume(in);

        intmax_t mhs = eval_ternary(c, in);
        if (peek(in).type != ':') {
            // report(REPORT_ERROR, NULL, s, tokens_get_location(s), "expected : for ternary");
            abort();
        }
        consume(in);

        intmax_t rhs = eval_ternary(c, in);
        return lhs ? mhs : rhs;
    }

    return lhs;
}
