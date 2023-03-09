static intmax_t eval_ternary(Cuik_CPP* restrict c, TokenList* restrict in);
static intmax_t eval_ternary_safe(Cuik_CPP* restrict c, TokenList* restrict in);

static _Thread_local jmp_buf eval__restore_point;

static intmax_t eval(Cuik_CPP* restrict c, TokenArray* restrict in) {
    void* savepoint = tls_save();

    TokenList l = line_into_list(in);
    if (l.head == NULL) {
        return 0;
    }

    // We need to get rid of the defined(MACRO) and defined MACRO before we
    // do real expansion or else it'll give us incorrect results
    for (TokenNode* n = l.head; n != NULL; n = n->next) {
        if (n->t.type != TOKEN_IDENTIFIER || !string_equals_cstr(&n->t.content, "defined")) {
            continue;
        }

        TokenNode* start = n;
        String str = { 0 };

        n = n->next;
        if (n->t.type == '(') {
            n = n->next;
            if (n->t.type != TOKEN_IDENTIFIER) {
                fprintf(stderr, "expected identifier\n");
                return 0;
            }

            str = n->t.content;

            n = n->next;
            if (n->t.type != ')') {
                fprintf(stderr, "expected ')'\n");
                return false;
            }
        } else if (n->t.type == TOKEN_IDENTIFIER) {
            str = n->t.content;
        } else {
            fprintf(stderr, "expected identifier for 'defined'!\n");
            return 0;
        }

        bool found = is_defined(c, str.data, str.length);

        start->t.type = TOKEN_INTEGER;
        start->t.content = string_cstr(found ? "1" : "0");
        start->next = n->next;
    }

    expand(c, l.head, 0, NULL);
    if (l.head->t.type == 0) {
        return 0;
    }

    // don't worry about the tail being correct, it doesn't matter here
    intmax_t result = eval_ternary_safe(c, &l);
    tls_restore(savepoint);

    return result;
}

static intmax_t eval_ternary_safe(Cuik_CPP* restrict c, TokenList* restrict in) {
    if (setjmp(eval__restore_point)) {
        return 0;
    }

    return eval_ternary(c, in);
}

// consume for lists
static Token grab_n_go(TokenList* restrict in) {
    TokenNode* n = in->head;
    return (in->head = in->head->next, n->t);
}

// some helpers to make TokenList act like a TokenArray
#define PEEK(in) (in->head->t)
#define CONSUME(in) grab_n_go(in)

static intmax_t eval_unary(Cuik_CPP* restrict c, TokenList* restrict in) {
    bool flip = false;
    while (PEEK(in).type == '!') {
        flip = !flip;
        in->head = in->head->next;
    }

    intmax_t val;
    Token t = CONSUME(in);
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
            longjmp(eval__restore_point, 1);
        }

        val = ch;
    } else if (t.type == '(') {
        val = eval_ternary(c, in);

        if (CONSUME(in).type != ')') {
            /*report_two_spots(REPORT_ERROR, NULL, s, t.location, tokens_get(s)->location,
                "expected closing parenthesis for macro subexpression",
                "open", "close?", NULL);*/
            longjmp(eval__restore_point, 1);
        }
    } else {
        SourceLoc loc = c->tokens.list.tokens[c->tokens.list.current].location;
        diag_err(&c->tokens, (SourceRange){ loc, loc }, "could macro expression");
        longjmp(eval__restore_point, 1);
    }

    return flip ? !val : val;
}

static intmax_t eval_l2(Cuik_CPP* restrict c, TokenList* restrict in) {
    if (PEEK(in).type == '-') {
        CONSUME(in);
        return -eval_unary(c, in);
    } else if (PEEK(in).type == '+') {
        CONSUME(in);
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

static intmax_t eval_binop(Cuik_CPP* restrict c, TokenList* restrict in, int min_prec) {
    // This precendence climber is always left associative
    intmax_t result = eval_l2(c, in);

    int prec;
    TknType binop;

    while (in->head) {
        TknType binop = PEEK(in).type;
        int prec = get_precendence(binop);
        if (prec == 0 || prec < min_prec) {
            break;
        }

        in->head = in->head->next;
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

static intmax_t eval_ternary(Cuik_CPP* restrict c, TokenList* restrict in) {
    intmax_t lhs = eval_binop(c, in, 0);

    if (in->head && in->head->t.type == '?') {
        CONSUME(in);

        intmax_t mhs = eval_ternary(c, in);
        if (PEEK(in).type != ':') {
            // report(REPORT_ERROR, NULL, s, tokens_get_location(s), "expected : for ternary");
            abort();
        }
        CONSUME(in);

        intmax_t rhs = eval_ternary(c, in);
        return lhs ? mhs : rhs;
    }

    return lhs;
}
