#include "../front/sema.h"

#define SWITCH_ITER switch (it->index_++)
#define CASE_YIELD(k, v) case k: return (it->stmt = (v), true);
#define CASE_END(k) case k: return false;

#define SWITCH1(a)    SWITCH_ITER { CASE_YIELD(0, a); CASE_END(1); }
#define SWITCH2(a, b) SWITCH_ITER { CASE_YIELD(0, a); CASE_YIELD(1, b); CASE_END(2); }

int cuik_get_expr_arity(Subexpr* e) {
    switch (e->op) {
        case EXPR_INT:
        case EXPR_CHAR:
        case EXPR_WCHAR:
        case EXPR_STR:
        case EXPR_WSTR:
        case EXPR_ENUM:
        case EXPR_SYMBOL:
        case EXPR_BUILTIN_SYMBOL:
        case EXPR_UNKNOWN_SYMBOL:
        case EXPR_PARAM:
        case EXPR_FLOAT32:
        case EXPR_FLOAT64:
        case EXPR_INITIALIZER: // { }
        case EXPR_SIZEOF_T:    // sizeof(T)
        case EXPR_CONSTRUCTOR: // ty
        return 0;

        case EXPR_CALL:
        return e->call.param_count + 1;

        case EXPR_DEREF:       // *expr
        case EXPR_ADDR:        // &expr
        case EXPR_NEGATE:      // -expr
        case EXPR_NOT:         // ~expr
        case EXPR_LOGICAL_NOT: // !expr
        case EXPR_CAST:        // (ty) expr
        case EXPR_VA_ARG:      // va_arg(expr, ty)
        case EXPR_SWIZZLE:     // expr.xyzw
        case EXPR_SIZEOF:      // sizeof(expr)
        case EXPR_ALIGNOF_T:   // _Alignof(T)
        case EXPR_GENERIC:     // _Generic(expr, cases)
        case EXPR_DOT: case EXPR_DOT_R:
        case EXPR_ARROW: case EXPR_ARROW_R:
        case EXPR_PRE_INC: case EXPR_PRE_DEC:
        case EXPR_POST_INC: case EXPR_POST_DEC:
        return 1;

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
        case EXPR_ASSIGN:
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN:
        case EXPR_SUBSCRIPT:
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGE:
        case EXPR_CMPLE:
        case EXPR_CMPGT:
        case EXPR_CMPLT:
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF:
        case EXPR_COMMA:
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
        return 2;

        // ternary only keeps the condition here
        case EXPR_TERNARY:
        return 1;

        default:
        log_error("Missing case! %d", e->op);
        abort();
    }
}

CUIK_API const char* cuik_get_expr_name(Subexpr* e) {
    switch (e->op) {
        case EXPR_NONE:           return "None";

        case EXPR_INT:            return "Integer";
        case EXPR_ENUM:           return "Enum";
        case EXPR_FLOAT32:        return "Float32";
        case EXPR_FLOAT64:        return "Float64";

        case EXPR_WCHAR:          return "WideChar";
        case EXPR_CHAR:           return "Char";
        case EXPR_WSTR:           return "WideString";
        case EXPR_STR:            return "String";

        case EXPR_BUILTIN_SYMBOL: return "BuiltinSymbol";
        case EXPR_UNKNOWN_SYMBOL: return "UnknownSymbol";
        case EXPR_SYMBOL:         return "Symbol";
        case EXPR_CONSTRUCTOR:    return "Constructor";
        case EXPR_GENERIC:        return "Generic"; // C11's _Generic
        case EXPR_VA_ARG:         return "VaArg";

        case EXPR_INITIALIZER:    return "Initializer";

        case EXPR_CAST:           return "Cast";
        case EXPR_PARAM:          return "Param"; // special case of case EXPR_VAR
        case EXPR_ASSIGN:         return "Assign";
        case EXPR_PLUS_ASSIGN:    return "PlusAssign";
        case EXPR_MINUS_ASSIGN:   return "MinusAssign";
        case EXPR_TIMES_ASSIGN:   return "TimesAssign";
        case EXPR_SLASH_ASSIGN:   return "SlashAssign";
        case EXPR_PERCENT_ASSIGN: return "PercentAssign";
        case EXPR_AND_ASSIGN:     return "AndAssign";
        case EXPR_OR_ASSIGN:      return "OrAssign";
        case EXPR_XOR_ASSIGN:     return "XorAssign";
        case EXPR_SHL_ASSIGN:     return "ShiftLeftAssign";
        case EXPR_SHR_ASSIGN:     return "ShiftRightAssign";

        case EXPR_PLUS:           return "Plus";
        case EXPR_MINUS:          return "Minus";
        case EXPR_TIMES:          return "Times";
        case EXPR_SLASH:          return "Slash";
        case EXPR_PERCENT:        return "Percent";
        case EXPR_AND:            return "And";
        case EXPR_OR:             return "Or";
        case EXPR_XOR:            return "Xor";
        case EXPR_SHL:            return "ShiftLeft";
        case EXPR_SHR:            return "ShiftRight";

        case EXPR_CMPEQ:          return "Equal";
        case EXPR_CMPNE:          return "NotEqual";
        case EXPR_CMPGE:          return "GreaterEqual";
        case EXPR_CMPLE:          return "LesserEqual";
        case EXPR_CMPGT:          return "GreaterThan";
        case EXPR_CMPLT:          return "LesserThan";

        // these are resolved by semantics pass
        case EXPR_PTRADD:         return "PointerAdd";
        case EXPR_PTRSUB:         return "PointerSub";
        case EXPR_PTRDIFF:        return "PointerDiff";

        case EXPR_TERNARY:        return "Ternary";
        case EXPR_COMMA:          return "Comma";

        case EXPR_LOGICAL_NOT:    return "LogicalNot";
        case EXPR_LOGICAL_AND:    return "LogicalAnd";
        case EXPR_LOGICAL_OR:     return "LogicalOr";

        case EXPR_DEREF:          return "Deref";
        case EXPR_ADDR:           return "Addr";
        case EXPR_NEGATE:         return "Negate";
        case EXPR_NOT:            return "Not";
        case EXPR_SUBSCRIPT:      return "Subscript";
        case EXPR_DOT:            return "Dot";
        case EXPR_ARROW:          return "Arrow";
        case EXPR_DOT_R:          return "DotR";
        case EXPR_ARROW_R:        return "ArrowR";
        case EXPR_CALL:           return "Call";

        case EXPR_SWIZZLE:        return "Swizzle"; // GLSL stuff: return ""; .xyxy
        case EXPR_SIZEOF_T:       return "SizeofT";  // on type
        case EXPR_SIZEOF:         return "Sizeof";   // on expr
        case EXPR_ALIGNOF_T:      return "AlignofT"; // on type

        case EXPR_PRE_INC:        return "PreIncrement";
        case EXPR_PRE_DEC:        return "PreDecrement";
        case EXPR_POST_INC:       return "PostIncrement";
        case EXPR_POST_DEC:       return "PostDecrement";

        default:
        log_error("Missing case! %d", e->op);
        abort();
    }
}
