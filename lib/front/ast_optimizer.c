#include "parser.h"

static ConstValue const_eval_bin_op(ExprOp op, ConstValue a, ConstValue b);

#define unsigned_const(x) (ConstValue) { false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue) { true, .signed_value = (x) }

// Const eval probably needs some rework...
Cuik_Type* sema_expr(TranslationUnit* tu, Expr* e);
Cuik_Type* sema_guess_type(TranslationUnit* tu, Stmt* restrict s);
Member* sema_traverse_members(TranslationUnit* tu, Cuik_Type* record_type, Atom name, uint32_t* out_offset);

// I've forsaken god by doing this, im sorry... it's ugly because it has to be i swear...
typedef struct {
    Cuik_Type* type;
    uint32_t offset;
} WalkMemberReturn;

// just returns the byte offset it traveled in these DOTs and ARROWs.
// the type we pass in is that of whatever's at the end of the DOT and ARROW chain
static WalkMemberReturn walk_member_accesses(TranslationUnit* tu, const Expr* e, Cuik_Type* type) {
    const Expr* base_expr = e->dot_arrow.base;

    WalkMemberReturn base = {0};
    if (base_expr->op != EXPR_ARROW && base_expr->op != EXPR_DOT) {
        // use that base type we've been patiently keeping around
        base = (WalkMemberReturn){type, 0};
    } else {
        base = walk_member_accesses(tu, e->dot_arrow.base, type);
    }

    uint32_t relative = 0;
    Member* member = sema_traverse_members(tu, base.type, e->dot_arrow.name, &relative);
    if (!member) abort();
    if (member->is_bitfield) abort();

    return (WalkMemberReturn){member->type, base.offset + relative};
}

bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out) {
    // hacky but handles things like:
    //   &(((T*)0)->apple)
    //   sizeof(((T*)0).apple)
    if (e->op == EXPR_DOT || e->op == EXPR_ARROW) {
        Expr* base_e = e->dot_arrow.base;

        while (base_e->op == EXPR_ARROW ||
            base_e->op == EXPR_DOT) {
            // traverse any dot/arrow chains
            base_e = base_e->dot_arrow.base;
        }

        uint32_t offset = 0;
        Cuik_Type* record_type = NULL;
        Expr* arrow_base = base_e;
        if (arrow_base->op == EXPR_CAST) {
            record_type = arrow_base->cast.type;

            // dereference
            if (record_type->kind == KIND_PTR) {
                record_type = record_type->ptr_to;
            } else {
                abort();
            }

            if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
                return false;
            }

            Expr* folded = cuik__optimize_ast(tu, arrow_base->cast.src);
            if (folded->op != EXPR_INT) {
                return false;
            }

            uintmax_t pointer_value = folded->int_num.num;
            assert(pointer_value < UINT32_MAX);

            offset += pointer_value;
        } else {
            return false;
        }

        offset += walk_member_accesses(tu, e, record_type).offset;
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

Expr* cuik__optimize_ast(TranslationUnit* tu, Expr* e) {
    switch (e->op) {
        case EXPR_ENUM: {
            int64_t v = *e->enum_val.num;

            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_NONE;
            e->int_num.num = v;
            break;
        }

        case EXPR_CAST: {
            Expr* src = cuik__optimize_ast(tu, e->cast.src);

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
            Expr* cond = cuik__optimize_ast(tu, e->ternary_op.left);
            if (cond->op == EXPR_INT) {
                if (cond->int_num.num != 0) {
                    return cuik__optimize_ast(tu, e->ternary_op.middle);
                } else {
                    return cuik__optimize_ast(tu, e->ternary_op.right);
                }
            }

            break;
        }

        case EXPR_SIZEOF: {
            Cuik_Type* src = sema_expr(tu, e->x_of_expr.expr);
            if (src->size == 0) {
                type_layout(tu, src);
            }

            assert(src->size && "Something went wrong...");
            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = src->size;
            break;
        }
        case EXPR_SIZEOF_T: {
            if (e->x_of_type.type->size == 0) {
                type_layout(tu, e->x_of_type.type);
            }

            size_t x = e->x_of_type.type->size;
            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = x;
            break;
        }
        case EXPR_ALIGNOF_T: {
            if (e->x_of_type.type->size == 0) {
                type_layout(tu, e->x_of_type.type);
            }

            size_t x = e->x_of_type.type->align;
            e->op = EXPR_INT;
            e->int_num.suffix = INT_SUFFIX_ULL;
            e->int_num.num = x;
            break;
        }
        case EXPR_NOT: {
            Expr* src = cuik__optimize_ast(tu, e->unary_op.src);

            // ~(N - 1) => -N
            if (src->op == EXPR_INT && e->type != NULL && TYPE_IS_INTEGER(e->type)) {
                e->op = EXPR_NEGATE;
                src->int_num.num += 1;
                return e;
            }

            break;
        }
        case EXPR_NEGATE: {
            Expr* src = cuik__optimize_ast(tu, e->unary_op.src);
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
            if (const_eval_try_offsetof_hack(tu, e->unary_op.src, &dst)) {
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
            Expr* a = cuik__optimize_ast(tu, e->bin_op.right);

            if (a->op == EXPR_FLOAT32 || a->op == EXPR_FLOAT64) {
                Expr* b = cuik__optimize_ast(tu, e->bin_op.left);

                if (b->op == EXPR_FLOAT32 || b->op == EXPR_FLOAT64) {
                    switch (e->op) {
                        case EXPR_SLASH: {
                            float af = a->float_num, bf = b->float_num;

                            e->op = EXPR_FLOAT32;
                            e->float_num = bf / af;
                            break;
                        }
                        default: break;
                    }
                }
            } else if (a->op == EXPR_INT) {
                ExprOp op = e->op;
                Expr* current = e->bin_op.left;
                if (current->op != op) {
                    Expr* b = cuik__optimize_ast(tu, current);

                    if (b->op == EXPR_INT) {
                        ConstValue c = const_eval_bin_op(op, gimme(b), gimme(a));

                        e->op = EXPR_INT;
                        e->int_num.suffix = c.is_signed ? INT_SUFFIX_LL : INT_SUFFIX_ULL;
                        e->int_num.num = c.unsigned_value;
                    }
                } else {
                    // try tail calling
                    ConstValue av = gimme(a);

                    do {
                        Expr* b = cuik__optimize_ast(tu, current->bin_op.right);
                        // we couldn't resolve it
                        if (b->op != EXPR_INT) break;

                        av = const_eval_bin_op(op, gimme(b), av);
                        current = current->bin_op.left;
                    } while (current->op == op);

                    Expr* b = cuik__optimize_ast(tu, current);
                    if (b->op == EXPR_INT) {
                        ConstValue c = const_eval_bin_op(op, gimme(b), av);

                        e->op = EXPR_INT;
                        e->int_num.suffix = c.is_signed ? INT_SUFFIX_LL : INT_SUFFIX_ULL;
                        e->int_num.num = c.unsigned_value;
                    }
                }
            }

            break;
        }

        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN: {
            e->bin_op.left = cuik__optimize_ast(tu, e->bin_op.left);
            e->bin_op.right = cuik__optimize_ast(tu, e->bin_op.right);
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
