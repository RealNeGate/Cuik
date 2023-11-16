
#define GET_CONST_INT(e) (e.i)
#define SET_CONST_INT(lhs, rhs) (lhs.tag = CUIK_CONST_INT, lhs.i = (rhs))

static bool const_eval(Cuik_Parser* restrict parser, Cuik_Expr* e, Cuik_ConstVal* out_val);

enum { CONST_ERROR = -2 };

// -1 for error
static ptrdiff_t const_eval_subexpr(Cuik_Parser* restrict parser, Cuik_QualType* types, Subexpr* exprs, ptrdiff_t i, Cuik_ConstVal* res) {
    assert(i >= 0);
    Subexpr* s = &exprs[i];

    switch (s->op) {
        case EXPR_CHAR: {
            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = s->char_lit };
            return i - 1;
        }
        case EXPR_INT: {
            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = s->int_lit.lit };
            return i - 1;
        }
        case EXPR_FLOAT32: {
            *res = (Cuik_ConstVal){ CUIK_CONST_FLOAT, .f = s->float_lit };
            return i - 1;
        }
        case EXPR_FLOAT64: {
            *res = (Cuik_ConstVal){ CUIK_CONST_FLOAT, .f = s->float_lit };
            return i - 1;
        }
        case EXPR_SIZEOF_T: {
            Cuik_Type* src = cuik_canonical_type(s->x_of_type.type);
            if (src->size == 0) {
                type_layout2(parser, &parser->tokens, src);

                if (src->size == 0) {
                    diag_err(&parser->tokens, s->loc, "Could not resolve type");
                    return -1;
                }
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = src->size };
            return i - 1;
        }

        case EXPR_ENUM: {
            if (s->enum_val.num->lexer_pos != 0) {
                type_layout2(parser, &parser->tokens, cuik_canonical_type(s->enum_val.type));
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = s->enum_val.num->value };
            return i - 1;
        }
        case EXPR_CAST: {
            Cuik_ConstVal src;

            i = const_eval_subexpr(parser, types, exprs, i - 1, &src);
            if (i == CONST_ERROR) return i;

            assert(src.tag == CUIK_CONST_INT);
            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = src.i };
            return i;
        }
        case EXPR_ADDR: {
            i -= 1;

            // find base (just skip arrow or dot)
            Subexpr* base = &exprs[i];
            if (base->op == EXPR_DOT || base->op == EXPR_ARROW) {
                base -= 1;
            }

            Cuik_Type* t = NULL;
            if (base->op == EXPR_CAST) {
                t = cuik_canonical_type(base->cast.type);
                base -= 1;
                if (base->op == EXPR_INT) {
                    // (T*) 0
                    *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = base->int_lit.lit };
                } else if (base->op == EXPR_SYMBOL) {
                    *res = (Cuik_ConstVal){ CUIK_CONST_ADDR, .s = { base - exprs, 0 } };
                } else {
                    diag_err(&parser->tokens, s->loc, "Cannot evaluate address as constant");
                    return CONST_ERROR;
                }
            }

            ptrdiff_t offset = 0;
            Subexpr* s = &exprs[i];
            if (s->op == EXPR_ARROW) {
                // &(a->b)         A -> &
                if (t == NULL) {
                    diag_err(&parser->tokens, s->loc, "Unknown type, cannot get member: %s", s->dot_arrow.name);
                    return CONST_ERROR;
                }

                if (cuik_type_can_deref(t)) {
                    t = cuik_canonical_type(t->ptr_to);
                } else {
                    diag_err(&parser->tokens, s->loc, "Expected pointer (or array) for arrow");
                    return CONST_ERROR;
                }

                // force this expression's type to resolve
                if (t->size == 0) {
                    type_layout2(parser, &parser->tokens, t);
                }

                uint32_t member_offset = 0;
                Member* member = sema_traverse_members(t, s->dot_arrow.name, &member_offset);
                if (member == NULL) {
                    diag_err(&parser->tokens, s->loc, "Unknown member: %s", s->dot_arrow.name);
                    return CONST_ERROR;
                }

                offset += member_offset;
                i -= 1;
            }

            if (res->tag == CUIK_CONST_ADDR) {
                res->s.offset += offset;
            } else {
                assert(res->tag == CUIK_CONST_INT);
                res->i += offset;
            }

            return (base - exprs) - 1;
        }
        case EXPR_SYMBOL: {
            Stmt* sym = s->sym.stmt;
            if (!cuik_type_implicit_ptr(cuik_canonical_type(sym->decl.type))) {
                diag_err(&parser->tokens, s->loc, "Cannot evaluate address as constant");
                return CONST_ERROR;
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_ADDR, .s = { i, 0 } };
            return i - 1;
        }
        default: break;
    }

    // try unary operators
    if (s->op == EXPR_NOT || s->op == EXPR_NEGATE) {
        Cuik_ConstVal src;

        i = const_eval_subexpr(parser, types, exprs, i - 1, &src);
        if (i == CONST_ERROR) return i;

        Cuik_Type* ty = types ? cuik_canonical_type(types[i]) : NULL;
        if (ty && cuik_type_is_float(ty)) {
            assert(s->op == EXPR_NEGATE && "expected negate");

            // implicit cast
            if (src.tag == CUIK_CONST_INT) { src.tag = CUIK_CONST_FLOAT; src.f = src.i; }

            *res = (Cuik_ConstVal){ CUIK_CONST_FLOAT, .f = -src.f };
            return i;
        } else {
            uint64_t result = ~src.i;
            if (s->op == EXPR_NEGATE) {
                result += 1;
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = result };
            return i;
        }
    }

    // try binary operators
    if (s->op >= EXPR_PLUS && s->op <= EXPR_CMPLT) {
        Cuik_ConstVal rhs, lhs;

        i = const_eval_subexpr(parser, types, exprs, i - 1, &rhs);
        if (i == CONST_ERROR) return i;
        i = const_eval_subexpr(parser, types, exprs, i, &lhs);
        if (i == CONST_ERROR) return i;

        Cuik_Type* ty = types ? cuik_canonical_type(types[i]) : NULL;
        if (ty && cuik_type_is_float(ty)) {
            if (lhs.tag == CUIK_CONST_INT) { lhs.tag = CUIK_CONST_FLOAT; lhs.f = lhs.i; }
            if (rhs.tag == CUIK_CONST_INT) { rhs.tag = CUIK_CONST_FLOAT; rhs.f = rhs.i; }

            double result = 0;
            switch (s->op) {
                // operators
                case EXPR_PLUS:    result = lhs.f +  rhs.f; break;
                case EXPR_MINUS:   result = lhs.f -  rhs.f; break;
                case EXPR_TIMES:   result = lhs.f *  rhs.f; break;
                case EXPR_SLASH:   result = lhs.f /  rhs.f; break;
                // comparisons
                case EXPR_CMPEQ:   result = lhs.f == rhs.f; break;
                case EXPR_CMPNE:   result = lhs.f != rhs.f; break;
                case EXPR_CMPGE:   result = lhs.f >= rhs.f; break;
                case EXPR_CMPLE:   result = lhs.f <= rhs.f; break;
                case EXPR_CMPGT:   result = lhs.f >  rhs.f; break;
                case EXPR_CMPLT:   result = lhs.f <  rhs.f; break;
                default: assert(0 && "todo");
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_FLOAT, .f = result };
            return i;
        } else {
            uint64_t result = 0;
            switch (s->op) {
                // operators
                case EXPR_PLUS:    result = lhs.i +  rhs.i; break;
                case EXPR_MINUS:   result = lhs.i -  rhs.i; break;
                case EXPR_TIMES:   result = lhs.i *  rhs.i; break;
                case EXPR_SLASH:   result = lhs.i /  rhs.i; break;
                case EXPR_PERCENT: result = lhs.i %  rhs.i; break;
                case EXPR_SHL:     result = lhs.i << rhs.i; break;
                case EXPR_SHR:     result = lhs.i >> rhs.i; break;
                case EXPR_AND:     result = lhs.i &  rhs.i; break;
                case EXPR_OR:      result = lhs.i |  rhs.i; break;
                // comparisons
                case EXPR_CMPEQ:   result = lhs.i == rhs.i; break;
                case EXPR_CMPNE:   result = lhs.i != rhs.i; break;
                case EXPR_CMPGE:   result = lhs.i >= rhs.i; break;
                case EXPR_CMPLE:   result = lhs.i <= rhs.i; break;
                case EXPR_CMPGT:   result = lhs.i >  rhs.i; break;
                case EXPR_CMPLT:   result = lhs.i <  rhs.i; break;
                default: assert(0 && "todo");
            }

            *res = (Cuik_ConstVal){ CUIK_CONST_INT, .i = result };
            return i;
        }
    }

    if (s->op == EXPR_TERNARY) {
        Cuik_ConstVal src;
        i = const_eval_subexpr(parser, types, exprs, i - 1, &src);
        if (i == CONST_ERROR) return i;

        if (src.tag != CUIK_CONST_INT) {
            diag_err(&parser->tokens, s->loc, "Ternary condition must be int");
            return CONST_ERROR;
        }

        Cuik_ConstVal v;
        if (!const_eval(parser, src.i ? s->ternary.left : s->ternary.right, &v)) {
            diag_err(&parser->tokens, s->loc, "Cannot fold ternary");
            return CONST_ERROR;
        }

        *res = v;
        return i;
    }

    diag_err(parser ? &parser->tokens : NULL, s->loc, "could not parse subexpression '%s' as constant.", cuik_get_expr_name(s));
    return CONST_ERROR;
}

static bool const_eval_addr_single(Cuik_Parser* restrict parser, Cuik_Expr* e, Subexpr* s, Cuik_ConstVal* args) {
    switch (s->op) {
        // try pointer arith
        case EXPR_PLUS: {
            if (args[1].tag == CUIK_CONST_ADDR) {
                SWAP(Cuik_ConstVal, args[0], args[1]);
            }

            if (args[1].tag == CUIK_CONST_ADDR) {
                diag_err(&parser->tokens, s->loc, "cannot add two pointers");
                return false;
            } else if (args[1].tag == CUIK_CONST_FLOAT) {
                diag_err(&parser->tokens, s->loc, "cannot offset float to pointer");
                return false;
            }

            args[0].s.offset += args[1].i;
            return true;
        }

        case EXPR_SIZEOF: {
            if (args[0].tag != CUIK_CONST_ADDR) {
                diag_err(&parser->tokens, s->loc, "cannot compute sizeof... sadge");
                return false;
            }

            Subexpr* s = &e->exprs[args[0].s.base];
            Stmt* sym = s->sym.stmt;
            if (cuik_canonical_type(sym->decl.type)->size == 0) {
                type_layout2(parser, &parser->tokens, cuik_canonical_type(sym->decl.type));

                // resolve declaration early
                sema_stmt(parser->tu, sym);

                if (cuik_canonical_type(sym->decl.type)->size == 0) {
                    diag_err(&parser->tokens, s->loc, "cannot compute size of symbol");
                    return false;
                }
            }

            args[0] = (Cuik_ConstVal){ CUIK_CONST_INT, .i = cuik_canonical_type(sym->decl.type)->size };
            return true;
        }

        case EXPR_SUBSCRIPT: {
            assert(0 && "todo");
            return true;
        }

        default: break;
    }

    diag_err(&parser->tokens, s->loc, "could not parse subexpression '%s' as constant.", cuik_get_expr_name(s));
    return false;
}

// does constant eval on integer values, if it ever fails it'll exit with false
static bool const_eval(Cuik_Parser* restrict parser, Cuik_Expr* e, Cuik_ConstVal* out_val) {
    return const_eval_subexpr(parser, e->types, e->exprs, e->count - 1, out_val) != CONST_ERROR;

    #if 0
    size_t top = 0;
    stack[128];

    Subexpr* exprs = e->exprs;
    bool has_symbols = false;

    size_t i = 0;
    for (; i < e->count; i++) {
        Subexpr* s = &exprs[i];

        // once we know this we can organize the top slice of the stack as the inputs
        int arity = cuik_get_expr_arity(s);
        top -= arity;
        Cuik_ConstVal* args = &stack[top];
        top += 1;

        // &((T*)0)->field
        if (s->op == EXPR_CAST) {
            Cuik_Type* t = cuik_canonical_type(s->cast.type);

            if (cuik_type_is_integer(t) && args[0].tag == CUIK_CONST_ADDR) {
                // cast constant pointer to int
                assert(args[0].s.base == UINT32_MAX);
                args[0].tag = CUIK_CONST_INT;
                args[0].i = args[0].s.offset;
                continue;
            } else if (t->kind == KIND_PTR || t->kind == KIND_ARRAY) {
                if (args[0].tag != CUIK_CONST_INT) {
                    diag_err(&parser->tokens, s->loc, "Constant cast can only accept integers here");
                    return false;
                }

                // walk an arrow+member chain until we resolve an address
                int64_t offset = args[0].i;
                i += 1;

                if (i < e->count && exprs[i].op == EXPR_ARROW) {
                    if (t->kind != KIND_PTR) {
                        diag_err(&parser->tokens, s->loc, "Expected pointer (or array) for arrow");
                    } else {
                        t = cuik_canonical_type(t->ptr_to);
                    }

                    // force this expression's type to resolve
                    if (t->size == 0) {
                        type_layout2(parser, &parser->tokens, t);
                    }

                    uint32_t member_offset = 0;
                    Member* member = sema_traverse_members(t, exprs[i].dot_arrow.name, &member_offset);
                    if (member == NULL) {
                        diag_err(&parser->tokens, s->loc, "Unknown member: %s", exprs[i].dot_arrow.name);
                        return false;
                    }

                    offset += member_offset;
                    i += 1;
                }

                if (i >= e->count || exprs[i].op != EXPR_ADDR) {
                    diag_err(&parser->tokens, exprs[i - 1].loc, "Cannot access members in constant expression");
                    return false;
                }

                args[0].tag = CUIK_CONST_ADDR;
                args[0].s.base = UINT32_MAX;
                args[0].s.offset = offset;
                continue;
            }
        }

        // symbols + address is allowed to push (the backend uses this)
        if (s->op == EXPR_SYMBOL && (s->sym.stmt->op == STMT_GLOBAL_DECL || s->sym.stmt->op == STMT_FUNC_DECL)) {
            if (cuik_type_implicit_ptr(cuik_canonical_type(s->sym.stmt->decl.type))) {
                args[0].tag = CUIK_CONST_ADDR;
                args[0].s.base = i;
                args[0].s.offset = 0;

                has_symbols = true;
                continue;
            } else if (exprs[i+1].op == EXPR_ADDR) {
                args[0].tag = CUIK_CONST_ADDR;
                args[0].s.base = i;
                args[0].s.offset = 0;
                i += 1;

                has_symbols = true;
                continue;
            } else {
                diag_err(&parser->tokens, s->loc, "Cannot evaluate symbol as constant");
                return false;
            }
        }

        // speed optimization because most folding ops don't care about
        // constant addresses. if we do have constant addresses we need
        // to handle pointer arithmatic and pointer difference.
        if (UNLIKELY(has_symbols)) {
            for (size_t j = 0; j < arity; j++) {
                if (UNLIKELY(args[j].tag != CUIK_CONST_ADDR)) continue;

                if (const_eval_addr_single(parser, e, s, args)) {
                    goto skip;
                } else {
                    return false;
                }
            }
        }

        switch (s->op) {
            case EXPR_FLOAT32: {
                args[0].tag = CUIK_CONST_FLOAT;
                args[0].f = s->float_lit;
                goto skip;
            }
            default: break;
        }

        // normie integers, these get all integer args
        // if any aren't we do some of the special cases.
        uint64_t result;
        if (!const_eval_int_single(parser, e, s, args, &result)) {
            return false;
        }

        args[0].tag = CUIK_CONST_INT;
        args[0].i = result;
        continue;

        skip:;
    }

    assert(top == 1);
    *out_val = stack[0];
    return true;
    #endif
}
