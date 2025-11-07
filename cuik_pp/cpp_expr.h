
static _Thread_local jmp_buf eval__restore_point;

typedef struct {
    DynArray(Token) tokens;
    int current;
} ExprParser;

static intmax_t eval_ternary(Cuik_CPP* restrict c, ExprParser* in);
static intmax_t eval_ternary_safe(Cuik_CPP* restrict c, ExprParser* in);

static intmax_t eval(Cuik_CPP* restrict ctx, Lexer* restrict in) {
    DynArray(Token) tokens = ctx->tokens.list.tokens;
    int start = dyn_array_length(tokens);

    // place all the tokens on this line into the buffer to be expanded
    Token t;
    for (;;) {
        t = lexer_read(in);
        if (t.type == 0 || t.hit_line) { break; }
        push_token(ctx, t);

        size_t def_i;
        if (t.type == TOKEN_IDENTIFIER) {
            SourceLoc loc = t.location;
            int head = dyn_array_length(tokens) - 1;

            if (string_equals_cstr(&t.content, "defined")) {
                bool paren = false;
                t = lexer_read(in);
                if (t.type == '(') {
                    paren = true;
                    t = lexer_read(in);
                }

                if (t.type != TOKEN_IDENTIFIER) {
                    diag_err(&ctx->tokens, get_token_range(&t), "expected identifier");
                    goto error;
                }

                // Replaced defined(MACRO) => 0/1
                bool found = is_defined(ctx, t.content.data, t.content.length);
                tokens[head] = (Token){ .type = TOKEN_INTEGER, .location = loc, .content = string_cstr(found ? "1" : "0") };

                if (paren) {
                    t = lexer_read(in);
                    if (t.type != ')') {
                        diag_err(&ctx->tokens, get_token_range(&t), "expected closing paren");
                        goto error;
                    }
                }
            } else {
                MacroDef* def = find_define(ctx, t.content.data, t.content.length);
                if (def != NULL) {
                    expand_identifier(ctx, in, NULL, head, head+1, 0, def, 0, NULL);
                }
            }

            // remaining identifiers are converted to 0
            FOR_N(i, head, dyn_array_length(tokens)) {
                if (tokens[i].type == TOKEN_IDENTIFIER) {
                    tokens[i].type = TOKEN_INTEGER;
                    tokens[i].content = string_cstr("0");
                }
            }
        }
    }
    in->current = (unsigned char*) t.content.data;

    // EOL token
    push_token(ctx, (Token){ 0 });

    ctx->tokens.list.tokens = tokens;
    ExprParser p = { tokens, start };
    intmax_t result = eval_ternary_safe(ctx, &p);
    dyn_array_set_length(tokens, start);
    return result;

    error:
    dyn_array_set_length(tokens, start);
    ctx->tokens.list.tokens = tokens;
    return 0;
}

static intmax_t eval_ternary_safe(Cuik_CPP* restrict c, ExprParser* in) {
    if (setjmp(eval__restore_point)) {
        return 0;
    }

    return eval_ternary(c, in);
}

// some helpers to make TokenList act like a TokenArray
#define PEEK(in) in->tokens[in->current]
#define CONSUME(in) in->tokens[in->current++]

static intmax_t eval_unary(Cuik_CPP* restrict c, ExprParser* in) {
    bool flip = false;
    while (PEEK(in).type == '!') {
        flip = !flip;
        in->current++;
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
        diag_err(&c->tokens, get_token_range(&t), "could not parse macro expression");
        longjmp(eval__restore_point, 1);
    }

    return flip ? !val : val;
}

static intmax_t eval_l2(Cuik_CPP* restrict c, ExprParser* in) {
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

static intmax_t eval_binop(Cuik_CPP* restrict c, ExprParser* in, int min_prec) {
    // This precendence climber is always left associative
    intmax_t result = eval_l2(c, in);

    int prec;
    TknType binop;

    while (PEEK(in).type != 0) {
        TknType binop = PEEK(in).type;
        int prec = get_precendence(binop);
        if (prec == 0 || prec < min_prec) {
            break;
        }

        in->current++;
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

static intmax_t eval_ternary(Cuik_CPP* restrict c, ExprParser* in) {
    intmax_t lhs = eval_binop(c, in, 0);

    if (PEEK(in).type == '?') {
        CONSUME(in);

        intmax_t mhs = eval_ternary(c, in);
        if (PEEK(in).type != ':') {
            // TODO(NeGate): better error here...
            diag_err(&c->tokens, get_token_range(&PEEK(in)), "expected : for ternary");
            abort();
        }
        CONSUME(in);

        intmax_t rhs = eval_ternary(c, in);
        return lhs ? mhs : rhs;
    }

    return lhs;
}
