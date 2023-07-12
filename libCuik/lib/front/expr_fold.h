
#define GET_CONST_INT(e) (e.i)
#define SET_CONST_INT(lhs, rhs) (lhs.tag = CUIK_CONST_INT, lhs.i = (rhs))

static bool const_eval(Cuik_Parser* restrict parser, Cuik_Expr* e, Cuik_ConstVal* out_val);

static bool const_eval_int_single(Cuik_Parser* restrict parser, Cuik_Expr* e, Subexpr* s, Cuik_ConstVal* args, uint64_t* result) {
    switch (s->op) {
        case EXPR_CHAR: *result = s->char_lit; break;
        case EXPR_INT: *result = s->int_lit.lit; break;
        case EXPR_SIZEOF_T: {
            Cuik_Type* src = cuik_canonical_type(s->x_of_type.type);
            if (src->size == 0) {
                type_layout2(parser, &parser->tokens, src);

                if (src->size == 0) {
                    diag_err(&parser->tokens, s->loc, "Could not resolve type");
                    return false;
                }
            }

            *result = src->size;
            break;
        }

        case EXPR_ENUM: {
            if (s->enum_val.num->lexer_pos != 0) {
                type_layout2(parser, &parser->tokens, cuik_canonical_type(s->enum_val.type));
            }

            *result = s->enum_val.num->value;
            break;
        }

        case EXPR_CAST: {
            Cuik_Type* t = cuik_canonical_type(s->cast.type);

            if (!cuik_type_is_integer(t)) {
                diag_err(&parser->tokens, s->loc, "Cannot perform constant cast to %!T", cuik_canonical_type(s->cast.type));
                return false;
            }

            // TODO(NeGate): Perform masking
            *result = args[0].i;
            break;
        }

        // operators
        case EXPR_NEGATE:*result = -args[0].i;             break;
        case EXPR_PLUS:  *result = args[0].i + args[1].i;  break;
        case EXPR_MINUS: *result = args[0].i - args[1].i;  break;
        case EXPR_TIMES: *result = args[0].i * args[1].i;  break;
        case EXPR_SLASH: *result = args[0].i / args[1].i;  break;
        case EXPR_SHL:   *result = args[0].i << args[1].i; break;
        case EXPR_SHR:   *result = args[0].i >> args[1].i; break;
        case EXPR_AND:   *result = args[0].i & args[1].i;  break;
        case EXPR_OR:    *result = args[0].i | args[1].i;  break;

        // comparisons
        case EXPR_CMPEQ: *result = args[0].i == args[1].i; break;
        case EXPR_CMPNE: *result = args[0].i != args[1].i; break;
        case EXPR_CMPGE: *result = args[0].i >= args[1].i; break;
        case EXPR_CMPLE: *result = args[0].i <= args[1].i; break;
        case EXPR_CMPGT: *result = args[0].i > args[1].i;  break;
        case EXPR_CMPLT: *result = args[0].i < args[1].i;  break;

        // misc
        case EXPR_TERNARY: {
            Cuik_ConstVal v;
            if (!const_eval(parser, args[0].i ? s->ternary.left : s->ternary.right, &v)) {
                diag_err(&parser->tokens, s->loc, "Cannot fold ternary");
                return false;
            }

            if (v.tag != CUIK_CONST_INT) {
                diag_err(&parser->tokens, s->loc, "Ternary folded into non-integer");
                return false;
            }

            *result = v.i;
            break;
        }

        default:
        diag_err(parser ? &parser->tokens : NULL, s->loc, "could not parse subexpression '%s' as constant.", cuik_get_expr_name(s));
        return false;
    }

    return true;
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

        default: break;
    }

    diag_err(&parser->tokens, s->loc, "could not parse subexpression '%s' as constant.", cuik_get_expr_name(s));
    return false;
}

// does constant eval on integer values, if it ever fails it'll exit with false
static bool const_eval(Cuik_Parser* restrict parser, Cuik_Expr* e, Cuik_ConstVal* out_val) {
    size_t top = 0;
    Cuik_ConstVal stack[128];

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
        if (s->op == EXPR_CAST && exprs[i+1].op == EXPR_ARROW && exprs[i+2].op == EXPR_ADDR && i + 2 < e->count) {
            i += 2;
            continue;
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
}

#if 0
#include "parser.h"

static ConstValue const_eval_bin_op(ExprOp op, ConstValue a, ConstValue b);

#define unsigned_const(x) (ConstValue) { false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue) { true, .signed_value = (x) }

// Const eval probably needs some rework...
Cuik_QualType cuik__sema_expr(TranslationUnit* tu, Cuik_Expr* e);
Member* sema_traverse_members(Cuik_Type* record_type, Atom name, uint32_t* out_offset);

// I've forsaken god by doing this, im sorry... it's ugly because it has to be i swear...
typedef struct {
    Cuik_Type* type;
    uint32_t offset;
} WalkMemberReturn;

// just returns the byte offset it traveled in these DOTs and ARROWs.
// the type we pass in is that of whatever's at the end of the DOT and ARROW chain
static WalkMemberReturn walk_member_accesses(Cuik_Parser* restrict parser, const Expr* e, Cuik_Type* type) {
    const Expr* base_expr = e->dot_arrow.base;

    WalkMemberReturn base = { 0 };
    if (base_expr->op != EXPR_ARROW && base_expr->op != EXPR_DOT) {
        // use that base type we've been patiently keeping around
        base = (WalkMemberReturn){ type, 0 };
    } else {
        base = walk_member_accesses(parser, e->dot_arrow.base, type);
    }

    uint32_t relative = 0;
    Member* member = sema_traverse_members(base.type, e->dot_arrow.name, &relative);
    if (!member) abort();
    if (member->is_bitfield) abort();

    return (WalkMemberReturn){ cuik_canonical_type(member->type), base.offset + relative };
}

bool const_eval_try_offsetof_hack(Cuik_Parser* restrict parser, const Expr* e, uint64_t* out) {
    // hacky but handles things like:
    //   &(((T*)0)->apple)
    //   sizeof(((T*)0).apple)
    if (e->op == EXPR_DOT || e->op == EXPR_ARROW) {
        bool is_arrow = e->op == EXPR_ARROW;
        Expr* base_e = e->dot_arrow.base;

        while (base_e->op == EXPR_ARROW || base_e->op == EXPR_DOT) {
            // traverse any dot/arrow chains
            base_e = base_e->dot_arrow.base;
        }

        uint32_t offset = 0;
        Cuik_Type* record_type = NULL;
        Expr* arrow_base = base_e;
        if (arrow_base->op == EXPR_CAST) {
            record_type = cuik_canonical_type(arrow_base->cast.type);

            bool did_deref = false;
            while (!did_deref && record_type->kind == KIND_PTR) {
                record_type = cuik_canonical_type(record_type->ptr_to);
                did_deref = true;
            }

            if (is_arrow && !did_deref) {
                diag_err(&parser->tokens, arrow_base->loc, "Arrow cannot be applied to non-pointer type.");
                return false;
            }

            if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
                diag_err(&parser->tokens, arrow_base->loc, "Cannot do member access with non-aggregate type.");
                return false;
            }

            Expr* folded = cuik__optimize_ast(parser, parser->tu, arrow_base->cast.src);
            if (folded->op != EXPR_INT) {
                return false;
            }

            uintmax_t pointer_value = folded->int_num.num;
            assert(pointer_value < UINT32_MAX);

            offset += pointer_value;
        } else {
            return false;
        }

        offset += walk_member_accesses(parser, e, record_type).offset;
        *out = offset;
        return true;
    }

    return false;
}

static ConstValue gimme(const Expr* e) {
    assert(e->op == EXPR_INT);
    switch (e->int_num.suffix) {
        case INT_SUFFIX_NONE:
        case INT_SUFFIX_L:
        case INT_SUFFIX_LL:
        return signed_const(e->int_num.num);

        case INT_SUFFIX_U:
        case INT_SUFFIX_UL:
        case INT_SUFFIX_ULL:
        return unsigned_const(e->int_num.num);

        default:
        __builtin_unreachable();
    }
}

Expr* cuik__optimize_ast(Cuik_Parser* restrict parser, TranslationUnit* restrict tu, Expr* e) {
    switch (e->op) {
        case EXPR_ENUM: {
            if (e->enum_val.num->lexer_pos != 0) {
                type_layout2(parser, &parser->tokens, cuik_canonical_type(e->type));
            }
            int64_t v = e->enum_val.num->value;

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_NONE;
            e->int_num.num = v;
            break;
        }

        case EXPR_CAST: {
            Expr* src = cuik__optimize_ast(parser, tu, e->cast.src);

            if (src->op == EXPR_INT && cuik_type_is_integer(cuik_canonical_type(e->cast.type))) {
                src->type = e->cast.type;
                return src;
            }
            break;
        }

        case EXPR_CHAR:
        case EXPR_WCHAR: {
            int64_t v = e->char_lit;
            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_NONE;
            e->int_num.num = v;
            break;
        }

        case EXPR_TERNARY: {
            Expr* cond = cuik__optimize_ast(parser, tu, e->ternary_op.left);
            if (cond->op == EXPR_INT) {
                if (cond->int_num.num != 0) {
                    return cuik__optimize_ast(parser, tu, e->ternary_op.middle);
                } else {
                    return cuik__optimize_ast(parser, tu, e->ternary_op.right);
                }
            }

            break;
        }

        case EXPR_SIZEOF: {
            Cuik_Type* src = cuik_canonical_type(cuik__sema_expr(tu, e->x_of_expr.expr));
            if (src->size == 0) {
                type_layout2(parser, &parser->tokens, src);

                if (src->size == 0) {
                    diag_err(&parser->tokens, e->loc, "Could not resolve type of expression");
                }
            }

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->size;
            break;
        }
        case EXPR_SIZEOF_T: {
            Cuik_Type* src = cuik_canonical_type(e->x_of_type.type);
            if (src->size == 0) {
                type_layout2(parser, &parser->tokens, src);

                if (src->size == 0) {
                    diag_err(&parser->tokens, e->loc, "Could not resolve type");
                }
            }

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->size;
            break;
        }
        case EXPR_ALIGNOF_T: {
            Cuik_Type* src = cuik_canonical_type(e->x_of_type.type);
            if (src->size == 0) {
                type_layout2(parser, &parser->tokens, src);

                if (src->size == 0) {
                    diag_err(&parser->tokens, e->loc, "could not resolve type");
                }
            }

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->align;
            break;
        }
        case EXPR_NOT: {
            Expr* src = cuik__optimize_ast(parser, tu, e->unary_op.src);

            if (src->op == EXPR_INT) {
                e->op = EXPR_INT;
                e->int_num.suffix = src->int_num.suffix;
                e->int_num.num = ~src->int_num.num;
                return e;
            }
            break;
        }
        case EXPR_NEGATE: {
            // ~(N - 1) => -N
            Expr* src = cuik__optimize_ast(parser, tu, e->unary_op.src);
            if (src->op == EXPR_INT) {
                uint64_t x = src->int_num.num;

                e->op = EXPR_INT;
                e->int_num.suffix = INT_SUFFIX_ULL;
                e->int_num.num = (~x + 1);
            }

            break;
        }
        case EXPR_ADDR: {
            uint64_t dst;
            if (const_eval_try_offsetof_hack(parser, e->unary_op.src, &dst)) {
                e->op = EXPR_INT;
                e->int_num.suffix = INT_SUFFIX_ULL;
                e->int_num.num = dst;
            }
            break;
        }

        case EXPR_PLUS:
        case EXPR_MINUS:
        case EXPR_TIMES:
        case EXPR_SLASH:
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_SHL:
        case EXPR_SHR:
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGE:
        case EXPR_CMPLE:
        case EXPR_CMPGT:
        case EXPR_CMPLT: {
            Expr* a = cuik__optimize_ast(parser, tu, e->bin_op.right);

            if (a->op == EXPR_FLOAT32 || a->op == EXPR_FLOAT64) {
                Expr* b = cuik__optimize_ast(parser, tu, e->bin_op.left);

                if (b->op == EXPR_INT || b->op == EXPR_FLOAT32 || b->op == EXPR_FLOAT64) {
                    switch (e->op) {
                        case EXPR_SLASH: {
                            // inaccurate, we'll fix later
                            double af = a->float_num, bf = 0.0;

                            if (b->op) bf = b->int_num.num;
                            else bf = b->float_num;

                            e->op = EXPR_FLOAT32;
                            e->float_num = bf / af;
                            break;
                        }
                        default: break;
                    }
                }
            } else if (a->op == EXPR_INT) {
                ExprOp op = e->op;

                // TODO(NeGate): we can pull off a fancy tail recursion trick when we chain binary operators together
                Expr* b = cuik__optimize_ast(parser, tu, e->bin_op.left);
                if (b->op != EXPR_INT) {
                    break;
                }

                ConstValue c = const_eval_bin_op(op, gimme(b), gimme(a));

                e->op = EXPR_INT;
                e->int_num.suffix = c.is_signed ? INT_SUFFIX_LL : INT_SUFFIX_ULL;
                e->int_num.num = c.unsigned_value;
            }

            break;
        }

        // these don't get optimized
        default: break;
    }

    return e;
}

static ConstValue const_eval_bin_op(ExprOp op, ConstValue a, ConstValue b) {
    bool is_signed = a.is_signed | b.is_signed;

    switch (op) {
        case EXPR_PLUS:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value + b.unsigned_value};
        case EXPR_MINUS:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value - b.unsigned_value};
        case EXPR_TIMES:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value * b.unsigned_value};
        case EXPR_SLASH:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value / b.unsigned_value};
        case EXPR_AND:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value & b.unsigned_value};
        case EXPR_OR:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value | b.unsigned_value};

        case EXPR_SHL:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value << b.unsigned_value};
        case EXPR_SHR:
        return (ConstValue){is_signed, .unsigned_value = a.unsigned_value >> b.unsigned_value};

        case EXPR_CMPEQ:
        return unsigned_const(a.unsigned_value == b.unsigned_value);
        case EXPR_CMPNE:
        return unsigned_const(a.unsigned_value != b.unsigned_value);

        case EXPR_CMPGE:
        if (is_signed) return unsigned_const(a.signed_value >= b.signed_value);
        else return unsigned_const(a.unsigned_value >= b.unsigned_value);

        case EXPR_CMPLE:
        if (is_signed)
            return unsigned_const(a.signed_value <= b.signed_value);
        else
            return unsigned_const(a.unsigned_value <= b.unsigned_value);

        case EXPR_CMPGT:
        if (is_signed)
            return unsigned_const(a.signed_value > b.signed_value);
        else
            return unsigned_const(a.unsigned_value > b.unsigned_value);

        case EXPR_CMPLT:
        if (is_signed)
            return unsigned_const(a.signed_value < b.signed_value);
        else
            return unsigned_const(a.unsigned_value < b.unsigned_value);

        default:
        __builtin_unreachable();
    }
}
#endif

