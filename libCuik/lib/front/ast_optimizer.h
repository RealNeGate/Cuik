#include "parser.h"

static ConstValue const_eval_bin_op(ExprOp op, ConstValue a, ConstValue b);

#define unsigned_const(x) (ConstValue) { false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue) { true, .signed_value = (x) }

// Const eval probably needs some rework...
Cuik_QualType cuik__sema_expr(TranslationUnit* tu, Expr* e);
Cuik_QualType sema_guess_type(TranslationUnit* tu, Stmt* restrict s);
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

            Expr* folded = cuik__optimize_ast(parser, arrow_base->cast.src);
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

Expr* cuik__optimize_ast(Cuik_Parser* restrict parser, Expr* e) {
    switch (e->op) {
        case EXPR_ENUM: {
            if (e->enum_val.num->lexer_pos != 0) {
                type_layout2(parser, cuik_canonical_type(e->type), true);
            }
            int64_t v = e->enum_val.num->value;

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_NONE;
            e->int_num.num = v;
            break;
        }

        case EXPR_CAST: {
            Expr* src = cuik__optimize_ast(parser, e->cast.src);

            if (src->op == EXPR_INT) {
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
            Expr* cond = cuik__optimize_ast(parser, e->ternary_op.left);
            if (cond->op == EXPR_INT) {
                if (cond->int_num.num != 0) {
                    return cuik__optimize_ast(parser, e->ternary_op.middle);
                } else {
                    return cuik__optimize_ast(parser, e->ternary_op.right);
                }
            }

            break;
        }

        case EXPR_SIZEOF: {
            Cuik_Type* src = cuik_canonical_type(cuik__sema_expr(NULL, e->x_of_expr.expr));
            if (src->size == 0) {
                type_layout2(parser, src, true);

                if (src->size == 0) {
                    diag_err(&parser->tokens, e->loc, "Could not resolve type of expression");
                }
            }

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->size;
            break;
        }
        case EXPR_ALIGNOF: {
            Cuik_Type* src = cuik_canonical_type(cuik__sema_expr(NULL, e->x_of_expr.expr));
            if (src->size == 0) {
                type_layout2(parser, src, true);

                if (src->size == 0) {
                    diag_err(&parser->tokens, e->loc, "Could not resolve type of expression");
                }
            }

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->align;
            break;
        }
        case EXPR_SIZEOF_T: {
            Cuik_Type* src = cuik_canonical_type(e->x_of_type.type);
            if (src->size == 0) {
                type_layout2(parser, src, true);

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
                type_layout2(parser, src, true);

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
            Expr* src = cuik__optimize_ast(parser, e->unary_op.src);

            // ~(N - 1) => -N
            if (src->op == EXPR_INT) {
                if (!CUIK_QUAL_TYPE_IS_NULL(e->type) && cuik_type_is_integer(cuik_canonical_type(e->type))) {
                    e->op = EXPR_NEGATE;
                    src->int_num.num += 1;
                    return e;
                } else {
                    cuik__sema_expr(NULL, src);
                    uint64_t mask = UINT64_MAX >> (cuik_canonical_type(src->type)->size*8);

                    e->op = EXPR_INT;
                    e->int_num.suffix = src->int_num.suffix;
                    e->int_num.num = ~src->int_num.num & mask;
                    return e;
                }
            }
            break;
        }
        case EXPR_NEGATE: {
            Expr* src = cuik__optimize_ast(parser, e->unary_op.src);
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
            Expr* a = cuik__optimize_ast(parser, e->bin_op.right);

            if (a->op == EXPR_FLOAT32 || a->op == EXPR_FLOAT64) {
                Expr* b = cuik__optimize_ast(parser, e->bin_op.left);

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
                Expr* b = cuik__optimize_ast(parser, e->bin_op.left);
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
