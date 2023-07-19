#if 0
// It's only *sorta* part of the semantics pass
#include "sema.h"

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];
static thread_local Stmt* function_stmt;
static thread_local bool barz[64];

void cuik_dump_expr(FILE* stream, Expr* e, int depth) {
    for (int i = 0; i < depth; i++) printf("  ");

    if (e->cast_type.raw != e->type.raw) {
        // qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
        qual_type_as_string(sizeof(temp_string1), temp_string1, e->cast_type);

        if (e->op != EXPR_CAST && e->cast_type.raw && cuik_canonical_type(e->cast_type)->kind != KIND_VOID) {
            // we don't wanna place implicit casts to void, it's weird
            printf("implicit-cast %s\n", temp_string1);
            depth++;

            for (int i = 0; i < depth; i++) printf("  ");
            printf("\n");
        }
    }

    switch (e->op) {
        case EXPR_CHAR:
        case EXPR_WCHAR: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "char '%s' %u\n", temp_string0, e->char_lit);
            break;
        }
        case EXPR_INT: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "int '%s' %llu\n", temp_string0, e->int_num.num);
            break;
        }
        case EXPR_COMMA:
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
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN:
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE:
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF: {
            static const char* names[] = {
                [EXPR_COMMA] = "Comma",

                [EXPR_PLUS] = "add",
                [EXPR_MINUS] = "sub",
                [EXPR_TIMES] = "mul",
                [EXPR_SLASH] = "div",
                [EXPR_PERCENT] = "mod",
                [EXPR_AND] = "bit-and",
                [EXPR_OR] = "bit-or",
                [EXPR_XOR] = "bit-xor",
                [EXPR_SHL] = "shl",
                [EXPR_SHR] = "shr",

                [EXPR_PLUS_ASSIGN] = "add-assign",
                [EXPR_MINUS_ASSIGN] = "sub-assign",
                [EXPR_ASSIGN] = "assign",
                [EXPR_TIMES_ASSIGN] = "mul-assign",
                [EXPR_PERCENT_ASSIGN] = "mod-assign",
                [EXPR_SLASH_ASSIGN] = "div-assign",
                [EXPR_AND_ASSIGN] = "bit-and-assign",
                [EXPR_OR_ASSIGN] = "bit-or-assign",
                [EXPR_XOR_ASSIGN] = "bit-xor-assign",
                [EXPR_SHL_ASSIGN] = "shl-assign",
                [EXPR_SHR_ASSIGN] = "shr-assign",

                [EXPR_CMPEQ] = "cmp-equal",
                [EXPR_CMPNE] = "cmp-not-equal",
                [EXPR_CMPGT] = "cmp-greater",
                [EXPR_CMPGE] = "cmp-greater-equal",
                [EXPR_CMPLT] = "cmp-lesser",
                [EXPR_CMPLE] = "cmp-lesser-equal",

                [EXPR_LOGICAL_AND] = "and",
                [EXPR_LOGICAL_OR] = "or",

                [EXPR_PTRADD] = "ptradd",
                [EXPR_PTRSUB] = "ptrsub",
                [EXPR_PTRDIFF] = "ptrdiff"
            };

            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "%s '%s'\n", names[e->op], temp_string0);

            cuik_dump_expr(stream, e->bin_op.left, depth + 1);
            cuik_dump_expr(stream, e->bin_op.right, depth + 1);
            break;
        }
        default:
        abort();
    }
}

static void print_barz(int depth, bool last_node) {
    if (last_node) {
        for (int i = 0; i < depth - 1; i++) {
            printf(barz[i] ? "| " : "  ");
        }
        printf("`-");
    } else {
        for (int i = 0; i < depth - 1; i++) {
            printf(barz[i] ? "| " : "  ");
        }
        printf("|-");
    }
    barz[depth - 1] = !last_node;
}

static void dump_expr(FILE* stream, Expr* restrict e, int depth, bool last_node) {
    print_barz(depth, last_node);

    if (e->cast_type.raw != e->type.raw) {
        qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
        qual_type_as_string(sizeof(temp_string1), temp_string1, e->cast_type);

        if (e->op != EXPR_CAST && e->cast_type.raw && cuik_canonical_type(e->cast_type)->kind != KIND_VOID) {
            // we don't wanna place implicit casts to void, it's weird
            fprintf(stream, "ImplicitCast '%s' -> '%s'\n", temp_string0, temp_string1);

            depth++;
            print_barz(depth, true);
        }
    }

    switch (e->op) {
        case EXPR_CHAR:
        case EXPR_WCHAR: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "CharLiteral %u '%s'\n", e->char_lit, temp_string0);
            break;
        }
        case EXPR_INT: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            if (cuik_type_is_signed(cuik_canonical_type(e->type))) {
                fprintf(stream, "IntegerLiteral %lld '%s'\n", e->int_num.num, temp_string0);
            } else {
                fprintf(stream, "IntegerLiteral %llu '%s'\n", e->int_num.num, temp_string0);
            }
            break;
        }
        case EXPR_ENUM: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "EnumLiteral %d '%s'\n", e->enum_val.num->value, temp_string0);
            break;
        }
        case EXPR_FLOAT32:
        case EXPR_FLOAT64: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "FloatLiteral %f '%s'\n", e->float_num, temp_string0);
            break;
        }
        case EXPR_SYMBOL: {
            Stmt* stmt = e->symbol;

            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            if (stmt->op == STMT_LABEL) {
                fprintf(stream, "LabelRef\n");
            } else {
                fprintf(stream, "Symbol %s '%s'\n", stmt->decl.name, temp_string0);
            }
            break;
        }
        case EXPR_BUILTIN_SYMBOL: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "BuiltinSymbol %s '%s'\n", e->builtin_sym.name, temp_string0);
            break;
        }
        case EXPR_PARAM: {
            int param_num = e->param_num;

            Cuik_Type* func_type = cuik_canonical_type(function_stmt->decl.type);
            Param* params = func_type->func.param_list;

            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Symbol %s '%s'\n", params[param_num].name, temp_string0);
            break;
        }
        case EXPR_WSTR: {
            // TODO(NeGate): Convert the string back into a C string literal so we don't cause any weird text printing
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            const wchar_t* start = (const wchar_t*)e->str.start;
            const wchar_t* end = (const wchar_t*)e->str.end;

            char* out_start = tls_push(1024);
            char* out = out_start;

            while (start != end) {
                // it's either printed as normal or \x00
                unsigned int ch = *start;
                bool is_normal = (*start >= 0x20 && *start < 0x7F);

                ptrdiff_t len = (out - out_start) + (is_normal ? 1 : 4);
                if (len >= 1024) break;

                if (is_normal) {
                    *out++ = ch;
                } else {
                    *out++ = '\\';
                    *out++ = 'x';
                    *out++ = "0123456789ABCDEF"[(ch >> 8) & 0xF];
                    *out++ = "0123456789ABCDEF"[ch & 0xF];
                }

                start++;
            }

            fprintf(stream, "StringLiteral \"%.*s\" '%s'\n", (int)(out - out_start), out_start, temp_string0);
            tls_restore(out_start);
            break;
        }
        case EXPR_STR: {
            // TODO(NeGate): Convert the string back into a C string literal so we don't cause any weird text printing
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            const unsigned char* start = e->str.start;
            const unsigned char* end = e->str.end;

            char* out_start = tls_push(1024);
            char* out = out_start;

            while (start != end) {
                // it's either printed as normal or \x00
                unsigned int ch = *start;
                bool is_normal = (*start >= 0x20 && *start < 0x7F);

                ptrdiff_t len = (out - out_start) + (is_normal ? 1 : 4);
                if (len >= 1024) break;

                if (is_normal) {
                    *out++ = ch;
                } else {
                    *out++ = '\\';
                    *out++ = 'x';
                    *out++ = "0123456789ABCDEF"[(ch >> 8) & 0xF];
                    *out++ = "0123456789ABCDEF"[ch & 0xF];
                }

                start++;
            }

            fprintf(stream, "StringLiteral \"%.*s\" '%s'\n", (int)(out - out_start), out_start, temp_string0);
            tls_restore(out_start);
            break;
        }
        case EXPR_INITIALIZER: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Initializer '%s'\n", temp_string0);
            break;
        }
        case EXPR_CALL: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "FunctionCall '%s'\n", temp_string0);

            Expr** args = e->call.param_start;
            int arg_count = e->call.param_count;

            dump_expr(stream, e->call.target, depth + 1, arg_count == 0);

            for (size_t i = 0; i < arg_count; i++) {
                dump_expr(stream, args[i], depth + 1, i == (arg_count - 1));
            }
            break;
        }
        case EXPR_TERNARY: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Ternary '%s'\n", temp_string0);

            dump_expr(stream, e->ternary_op.left, depth + 1, false);
            dump_expr(stream, e->ternary_op.middle, depth + 1, false);
            dump_expr(stream, e->ternary_op.right, depth + 1, true);
            break;
        }
        case EXPR_ARROW_R: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            char* name = (char*)e->dot_arrow.member->name;
            fprintf(stream, "Arrow %s '%s'\n", name ? name : "<unnamed>", temp_string0);

            dump_expr(stream, e->dot_arrow.base, depth + 1, true);
            break;
        }
        case EXPR_DOT_R: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            char* name = (char*)e->dot_arrow.member->name;
            fprintf(stream, "Dot %s '%s'\n", name ? name : "<unnamed>", temp_string0);

            dump_expr(stream, e->dot_arrow.base, depth + 1, true);
            break;
        }
        case EXPR_SUBSCRIPT: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Subscript '%s'\n", temp_string0);

            dump_expr(stream, e->subscript.base, depth + 1, false);
            dump_expr(stream, e->subscript.index, depth + 1, true);
            break;
        }
        case EXPR_DEREF: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Deref '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_GENERIC: {
            assert(e->generic_.case_count == 0);

            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Generic '%s'\n", temp_string0);

            dump_expr(stream, e->generic_.controlling_expr, depth + 1, true);
            break;
        }
        case EXPR_ADDR: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Addr '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_POST_INC: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "PostIncrement '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_POST_DEC: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "PostDecrement '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_PRE_INC: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "PreIncrement '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_PRE_DEC: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "PreDecrement '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_LOGICAL_NOT: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "LogicalNot '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_NOT: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "BinaryNot '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_NEGATE: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "Negate '%s'\n", temp_string0);

            dump_expr(stream, e->unary_op.src, depth + 1, true);
            break;
        }
        case EXPR_CAST: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            qual_type_as_string(sizeof(temp_string1), temp_string1, e->cast.type);

            fprintf(stream, "Cast '%s' -> '%s'\n", temp_string0, temp_string1);
            dump_expr(stream, e->cast.src, depth + 1, true);
            break;
        }
        case EXPR_VA_ARG: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->va_arg_.type);

            fprintf(stream, "VaArg '%s'\n", temp_string0);
            dump_expr(stream, e->va_arg_.src, depth + 1, true);
            break;
        }
        case EXPR_COMMA:
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
        case EXPR_PLUS_ASSIGN:
        case EXPR_MINUS_ASSIGN:
        case EXPR_ASSIGN:
        case EXPR_TIMES_ASSIGN:
        case EXPR_PERCENT_ASSIGN:
        case EXPR_SLASH_ASSIGN:
        case EXPR_AND_ASSIGN:
        case EXPR_OR_ASSIGN:
        case EXPR_XOR_ASSIGN:
        case EXPR_SHL_ASSIGN:
        case EXPR_SHR_ASSIGN:
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE:
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF: {
            static const char* names[] = {
                [EXPR_COMMA] = "Comma",

                [EXPR_PLUS] = "Plus",
                [EXPR_MINUS] = "Minus",
                [EXPR_TIMES] = "Times",
                [EXPR_SLASH] = "Slash",
                [EXPR_PERCENT] = "Percent",
                [EXPR_AND] = "And",
                [EXPR_OR] = "Or",
                [EXPR_XOR] = "Xor",
                [EXPR_SHL] = "ShiftLeft",
                [EXPR_SHR] = "ShiftRight",

                [EXPR_PLUS_ASSIGN] = "PlusAssign",
                [EXPR_MINUS_ASSIGN] = "MinusAssign",
                [EXPR_ASSIGN] = "Assign",
                [EXPR_TIMES_ASSIGN] = "TimesAssign",
                [EXPR_PERCENT_ASSIGN] = "PercentAssign",
                [EXPR_SLASH_ASSIGN] = "SlashAssign",
                [EXPR_AND_ASSIGN] = "AndAssign",
                [EXPR_OR_ASSIGN] = "OrAssign",
                [EXPR_XOR_ASSIGN] = "XorAssign",
                [EXPR_SHL_ASSIGN] = "ShiftLeftAssign",
                [EXPR_SHR_ASSIGN] = "ShiftRightAssign",

                [EXPR_CMPEQ] = "CompareEqual",
                [EXPR_CMPNE] = "CompareNotEqual",
                [EXPR_CMPGT] = "CompareGreater",
                [EXPR_CMPGE] = "CompareGreaterOrEqual",
                [EXPR_CMPLT] = "CompareLesser",
                [EXPR_CMPLE] = "CompareLesserOrEqual",

                [EXPR_LOGICAL_AND] = "LogicalAnd",
                [EXPR_LOGICAL_OR] = "LogicalOr",

                [EXPR_PTRADD] = "PointerAdd",
                [EXPR_PTRSUB] = "PointerSub",
                [EXPR_PTRDIFF] = "PointerDiff"
            };

            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);
            fprintf(stream, "%s '%s'\n", names[e->op], temp_string0);

            dump_expr(stream, e->bin_op.left, depth + 1, false);
            dump_expr(stream, e->bin_op.right, depth + 1, true);
            break;
        }
        case EXPR_CONSTRUCTOR: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            fprintf(stream, "Constructor '%s'\n", temp_string0);
            break;
        }
        case EXPR_SWIZZLE: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, e->type);

            fprintf(stream, "Swizzle '%s' [ ", temp_string0);
            for (int i = 0; i < e->swizzle.len; i++) {
                fprintf(stream, "%d ", e->swizzle.indices[i]);
            }
            fprintf(stream, "]\n");

            dump_expr(stream, e->swizzle.base, depth + 1, true);
            break;
        }
        default:
        abort();
    }

    barz[depth - 1] = false;
}

static void dump_stmt(FILE* stream, Stmt* restrict s, int depth, bool last_node) {
    if (s == NULL) return;
    print_barz(depth, last_node);

    switch (s->op) {
        case STMT_DECL:
        case STMT_GLOBAL_DECL: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, s->decl.type);

            if (s->decl.attrs.is_typedef) {
                fprintf(stream, "TypedefDecl %s '%s'\n", s->decl.name, temp_string0);
            } else {
                if (s->decl.name != NULL) {
                    fprintf(stream, "VarDecl %s '%s'", s->decl.name, temp_string0);

                    if (s->decl.glsl_quals != NULL) {
                        Cuik_GlslQuals* g = s->decl.glsl_quals;

                        fprintf(stream, " [ ");

                        switch (g->storage) {
                            case CUIK_GLSL_STORAGE_IN: fprintf(stream, "in "); break;
                            case CUIK_GLSL_STORAGE_OUT: fprintf(stream, "out "); break;
                            case CUIK_GLSL_STORAGE_INOUT: fprintf(stream, "inout "); break;
                            case CUIK_GLSL_STORAGE_BUFFER: fprintf(stream, "buffer "); break;
                            case CUIK_GLSL_STORAGE_UNIFORM: fprintf(stream, "uniform "); break;
                            default: break;
                        }

                        if (g->layout != CUIK_GLSL_LAYOUT_UNKNOWN) {
                            fprintf(stream, "%s ", g->layout == CUIK_GLSL_LAYOUT_140 ? "std140" : "std430");

                            if (g->binding >= 0)  fprintf(stream, "binding=%d ", g->binding);
                            if (g->location >= 0) fprintf(stream, "location=%d ", g->location);
                            if (g->offset >= 0) fprintf(stream, "offset=%d ", g->offset);
                        }

                        fprintf(stream, "]\n");
                    } else {
                        fprintf(stream, "\n");
                    }
                } else {
                    fprintf(stream, "UnnamedVarDecl '%s'\n", temp_string0);
                }

                if (s->decl.initial) {
                    dump_expr(stream, s->decl.initial, depth + 1, true);
                }
            }
            break;
        }
        case STMT_FUNC_DECL: {
            qual_type_as_string(sizeof(temp_string0), temp_string0, s->decl.type);
            fprintf(stream, "FunctionDecl %s '%s'\n", s->decl.name, temp_string0);

            Stmt* old_function_stmt = function_stmt;
            function_stmt = s;

            dump_stmt(stream, s->decl.initial_as_stmt, depth + 1, true);
            function_stmt = old_function_stmt;
            break;
        }
        case STMT_COMPOUND: {
            fprintf(stream, "Compound\n");

            Stmt** kids = s->compound.kids;
            size_t count = s->compound.kids_count;

            for (size_t i = 0; i < count; i++) {
                dump_stmt(stream, kids[i], depth + 1, i == (count - 1));
            }
            break;
        }
        case STMT_EXPR: {
            fprintf(stream, "Expr\n");
            dump_expr(stream, s->expr.expr, depth + 1, true);
            break;
        }
        case STMT_RETURN: {
            fprintf(stream, "Return\n");
            if (s->return_.expr) {
                dump_expr(stream, s->return_.expr, depth + 1, true);
            }
            break;
        }
        case STMT_BREAK: {
            fprintf(stream, "Break\n");
            break;
        }
        case STMT_CONTINUE: {
            fprintf(stream, "Continue\n");
            break;
        }
        case STMT_LABEL: {
            fprintf(stream, "Label %s\n", s->label.name);
            break;
        }
        case STMT_GOTO: {
            fprintf(stream, "Goto\n");
            dump_expr(stream, s->goto_.target, depth + 1, true);
            break;
        }
        case STMT_SWITCH: {
            fprintf(stream, "Switch\n");
            dump_expr(stream, s->switch_.condition, depth + 1, true);
            dump_stmt(stream, s->switch_.body, depth + 1, true);
            break;
        }
        case STMT_CASE: {
            if (s->case_.key != s->case_.key_max) {
                fprintf(stream, "Case %"PRId64" ... %"PRId64"\n", s->case_.key, s->case_.key_max);
            } else {
                fprintf(stream, "Case %"PRId64"\n", s->case_.key);
            }
            dump_stmt(stream, s->case_.body, depth + 1, true);
            break;
        }
        case STMT_DEFAULT: {
            fprintf(stream, "Default\n");
            dump_stmt(stream, s->default_.body, depth + 1, true);
            break;
        }
        case STMT_IF: {
            fprintf(stream, "If\n");
            dump_expr(stream, s->if_.cond, depth + 1, s->if_.body == 0);

            if (s->if_.body) {
                dump_stmt(stream, s->if_.body, depth + 1, s->if_.next == 0);

                if (s->if_.next) {
                    print_barz(depth, false);
                    fprintf(stream, "Else:\n");
                    dump_stmt(stream, s->if_.next, depth + 1, true);
                }
            }
            break;
        }
        case STMT_DO_WHILE: {
            fprintf(stream, "DoWhile\n");

            if (s->do_while.body) {
                dump_expr(stream, s->do_while.cond, depth + 1, false);
                dump_stmt(stream, s->do_while.body, depth + 1, true);
            } else {
                dump_expr(stream, s->do_while.cond, depth + 1, true);
            }
            break;
        }
        case STMT_WHILE: {
            fprintf(stream, "While\n");

            if (s->while_.body) {
                dump_expr(stream, s->while_.cond, depth + 1, false);
                dump_stmt(stream, s->while_.body, depth + 1, true);
            } else {
                dump_expr(stream, s->while_.cond, depth + 1, true);
            }
            break;
        }
        case STMT_FOR: {
            fprintf(stream, "For\n");
            if (s->for_.first) {
                print_barz(depth + 1, false);
                fprintf(stream, "Init:\n");

                Stmt* first = s->for_.first;
                if (first != NULL) {
                    if (first->op == STMT_COMPOUND) {
                        Stmt** kids = first->compound.kids;
                        size_t count = first->compound.kids_count;

                        for (size_t i = 0; i < count; i++) {
                            dump_stmt(stream, kids[i], depth + 2, i == (count - 1));
                        }
                    } else {
                        dump_stmt(stream, first, depth + 2, true);
                    }
                }
            }

            if (s->for_.cond) {
                print_barz(depth + 1, false);
                fprintf(stream, "Cond:\n");
                dump_expr(stream, s->for_.cond, depth + 2, true);
            }

            print_barz(depth + 1, s->for_.next == 0);
            fprintf(stream, "Body:\n");
            if (s->for_.body) {
                dump_stmt(stream, s->for_.body, depth + 2, true);
            }

            if (s->for_.next) {
                print_barz(depth + 1, true);
                fprintf(stream, "Next:\n");
                dump_expr(stream, s->for_.next, depth + 2, true);
            }
            break;
        }
        default:
        abort();
    }

    barz[depth - 1] = false;
}

void cuik_dump(FILE* stream, size_t count, Stmt** top_level, bool minimalist) {
    tls_init();
    fprintf(stream, "TranslationUnit\n");
    barz[0] = true;

    if (minimalist) {
        for (size_t i = 0; i < count; i++) {
            Stmt* stmt = top_level[i];
            if (!stmt->decl.attrs.is_used || stmt->decl.attrs.is_typedef) continue;

            bool is_last = (i == (count - 1));
            if (!is_last) {
                size_t j = i + 1;
                for (; j < count; j++) {
                    if (!stmt->decl.attrs.is_used || stmt->decl.attrs.is_typedef) break;
                }

                is_last = (j == (count - 1));
            }

            dump_stmt(stream, stmt, 1, is_last);
        }
    } else {
        for (size_t i = 0; i < count; i++) {
            dump_stmt(stream, top_level[i], 1, i == (count - 1));
        }
    }
}

void cuik_dump_translation_unit(FILE* stream, TranslationUnit* tu, bool minimalist) {
    cuik_dump(stream, dyn_array_length(tu->top_level_stmts), tu->top_level_stmts, minimalist);
}

void ast_dump_stats(TranslationUnit* tu, FILE* stream) {
    printf("\n~~~ AST STATS ~~~\n");
    printf("# Top level stmts: %zu\n", dyn_array_length(tu->top_level_stmts));
    // printf("# Arena:  %zu kB\n", arena_get_memory_usage(&tu->arena) / 1024);
}

void ast_dump_type(TranslationUnit* tu, Cuik_Type* ty, int depth, int offset) {
    for (int i = 0; i < depth; i++) printf("  ");

    if (ty->kind == KIND_STRUCT || ty->kind == KIND_UNION) {
        printf("%s %s {",
            ty->kind == KIND_STRUCT ? "struct" : "union",
            ty->record.name ? (char*)ty->record.name : "<unnamed>"
        );

        ResolvedSourceLoc r = cuikpp_find_location(&tu->tokens, ty->loc.start);
        printf("// %s:%d\n", r.file->filename, r.line);

        char temp_string0[1024];
        Member* kids = ty->record.kids;
        size_t kid_count = ty->record.kid_count;

        for (size_t i = 0; i < kid_count; i++) {
            Member* member = &kids[i];
            Cuik_Type* type = cuik_canonical_type(member->type);

            if (member->name == NULL || type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                ast_dump_type(tu, type, depth + 1, offset + member->offset);
            } else {
                for (int i = 0; i < depth; i++) printf("  ");
                printf("  ");

                qual_type_as_string(sizeof(temp_string0), temp_string0, member->type);

                int l = printf("%s %s", temp_string0, member->name);
                l += (depth + 1) * 2;

                for (int i = l; i < 42; i++) printf(" ");
                printf(" %d\n", offset + member->offset);
            }
        }

        for (int i = 0; i < depth; i++) printf("  ");
        printf("}\n");
    } else {
        printf("Could not represent type yet...\n");
    }

    if (depth == 0) {
        printf("STATS:\n  sizeof = %d\n  alignof = %d\n", ty->size, ty->align);
    }
}
#endif