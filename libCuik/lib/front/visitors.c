#include <front/sema.h>

#define SWITCH_ITER switch (it->index_++)
#define CASE_YIELD(k, v) case k: return (it->stmt = (v), true);
#define CASE_END(k) case k: return false;

#define SWITCH1(a)    SWITCH_ITER { CASE_YIELD(0, a); CASE_END(1); }
#define SWITCH2(a, b) SWITCH_ITER { CASE_YIELD(0, a); CASE_YIELD(1, b); CASE_END(2); }

bool cuik_next_stmt_kid(Cuik_StmtIter* it) {
    Stmt* restrict s = it->parent_;
    switch (s->op) {
        case STMT_NONE:
        case STMT_LABEL:
        case STMT_GOTO:
        case STMT_EXPR:
        case STMT_RETURN:
        case STMT_CONTINUE:
        case STMT_BREAK:
        case STMT_DECL:
        case STMT_FUNC_DECL:
        case STMT_GLOBAL_DECL:
        return false;

        case STMT_COMPOUND: {
            int i = it->index_++;

            Stmt** kids = s->compound.kids;
            if (i < s->compound.kids_count) {
                it->stmt = kids[i];
                return true;
            } else {
                return false;
            }
            break;
        }

        case STMT_IF:
        it->index_ += (it->index_ == 1 && s->if_.next == NULL);

        SWITCH2(s->if_.body, s->if_.next);
        break;

        case STMT_WHILE:
        // skip empty slots
        it->index_ += (it->index_ == 0 && s->while_.body == NULL);

        SWITCH1(s->while_.body);
        break;

        case STMT_DO_WHILE:
        // skip empty slots
        it->index_ += (it->index_ == 0 && s->do_while.body == NULL);

        SWITCH1(s->do_while.body);
        break;

        case STMT_FOR:
        // skip empty slots
        it->index_ += (it->index_ == 0 && s->for_.first == NULL);
        it->index_ += (it->index_ == 1 && s->for_.body == NULL);

        SWITCH2(s->for_.first, s->for_.body);
        break;

        case STMT_SWITCH: SWITCH1(s->switch_.body); break;
        case STMT_CASE: SWITCH1(s->case_.body); break;
        case STMT_DEFAULT: SWITCH1(s->default_.body); break;
    }

    #ifdef _DEBUG
    assert(0 && "StmtIter found unknown node");
    #else
    __builtin_unreachable();
    #endif
}

bool cuik_next_expr_kid(Cuik_ExprIter* it) {
    Expr* restrict e = it->parent_;
    switch (e->op) {
        case EXPR_UNKNOWN_SYMBOL:
        case EXPR_VA_ARG:
        case EXPR_INT:
        case EXPR_ENUM:
        case EXPR_FLOAT32:
        case EXPR_FLOAT64:
        case EXPR_CHAR:
        case EXPR_WCHAR:
        case EXPR_STR:
        case EXPR_WSTR:
        case EXPR_SIZEOF:
        case EXPR_ALIGNOF:
        case EXPR_SIZEOF_T:
        case EXPR_ALIGNOF_T:
        case EXPR_FUNCTION:
        case EXPR_SYMBOL:
        case EXPR_PARAM:
        return false;

        case EXPR_INITIALIZER:
        assert(0);
        return false;

        case EXPR_NOT:
        case EXPR_ADDR:
        case EXPR_DEREF:
        case EXPR_NEGATE:
        case EXPR_PRE_INC:
        case EXPR_PRE_DEC:
        case EXPR_POST_INC:
        case EXPR_POST_DEC:
        case EXPR_LOGICAL_NOT:
        case EXPR_CAST: {
            _Static_assert(offsetof(Expr, cast.src) == offsetof(Expr, unary_op.src), "these should be aliasing");
            _Static_assert(offsetof(Expr, va_arg_.src) == offsetof(Expr, unary_op.src), "these should be aliasing");

            switch (it->index_++) {
                case 0: it->expr = e->unary_op.src; return true;
                case 1: return false;
                default: __builtin_unreachable();
            }
            break;
        }

        case EXPR_GENERIC: {
            assert(e->generic_.case_count == 0);
            switch (it->index_++) {
                case 0: it->expr = e->generic_.controlling_expr; return true;
                case 1: return false;
                default: __builtin_unreachable();
            }
            break;
        }

        case EXPR_SUBSCRIPT:
        switch (it->index_++) {
            case 0: it->expr = e->subscript.base; return true;
            case 1: it->expr = e->subscript.index; return true;
            case 2: return false;
            default: __builtin_unreachable();
        }

        case EXPR_CALL: {
            int i = it->index_++;

            if (i == 0) {
                it->expr = e->call.target;
                return true;
            } else if ((i - 1) < e->call.param_count) {
                it->expr = e->call.param_start[i - 1];
                return true;
            } else {
                return false;
            }
        }

        case EXPR_TERNARY:
        switch (it->index_++) {
            case 0: it->expr = e->ternary_op.left; return true;
            case 1: it->expr = e->ternary_op.middle; return true;
            case 2: it->expr = e->ternary_op.middle; return true;
            case 3: return false;
            default: __builtin_unreachable();
        }

        case EXPR_DOT:
        case EXPR_ARROW:
        case EXPR_DOT_R:
        case EXPR_ARROW_R:
        switch (it->index_++) {
            case 0: it->expr = e->dot_arrow.base; return true;
            case 1: return false;
            default: __builtin_unreachable();
        }

        case EXPR_COMMA:
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF:
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
        case EXPR_PLUS:
        case EXPR_MINUS:
        case EXPR_TIMES:
        case EXPR_SLASH:
        case EXPR_PERCENT:
        case EXPR_AND:
        case EXPR_OR:
        case EXPR_XOR:
        case EXPR_SHL:
        case EXPR_SHR:
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE:
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN:
        switch (it->index_++) {
            case 0: it->expr = e->bin_op.left; return true;
            case 1: it->expr = e->bin_op.right; return true;
            case 2: return false;
            default: __builtin_unreachable();
        }

        default: assert(0); __builtin_unreachable();
    }
}
