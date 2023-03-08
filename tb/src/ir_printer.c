#include "tb_internal.h"
#include <stdarg.h>

TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf((FILE*)user_data, fmt, ap);
    va_end(ap);
}

#define P(...) callback(user_data, __VA_ARGS__)
static void tb_print_type(TB_DataType dt, TB_PrintCallback callback, void* user_data) {
    assert(dt.width < 8 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            if (dt.data == 0) P("void");
            else P("i%d", dt.data);
            break;
        }
        case TB_PTR: {
            if (dt.data == 0) P("ptr");
            else P("ptr%d", dt.data);
            break;
        }
        case TB_FLOAT: {
            if (dt.data == TB_FLT_32) P("f32");
            if (dt.data == TB_FLT_64) P("f64");
            break;
        }
        default: tb_todo();
    }
}

typedef struct {
    NL_Map(TB_Node*, int) ordinals;
    int count;
} TB_PrinterCtx;

// just a unique identifier for printing
#define NAME(n) get_name(ctx, n)
static int get_name(TB_PrinterCtx* ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->ordinals, n);
    assert(search >= 0);
    return ctx->ordinals[search].v;
}

TB_API const char* tb_node_get_name(TB_Node* n) {
    switch (n->type) {
        case TB_PARAM: return "param";
        case TB_LOCAL: return "local";

        case TB_POISON: return "poison";
        case TB_INTEGER_CONST: return "int";

        case TB_PASS: return "pass";
        case TB_PHI: return "phi";

        case TB_ZERO_EXT: return "zxt";
        case TB_SIGN_EXT: return "sxt";
        case TB_TRUNCATE: return "trunc";
        case TB_BITCAST: return "bitcast";
        case TB_GET_SYMBOL_ADDRESS: return "symbol";

        case TB_CMP_NE: return "cmp.ne";
        case TB_CMP_EQ: return "cmp.eq";
        case TB_CMP_ULT: return "cmp.ult";
        case TB_CMP_ULE: return "cmp.sle";
        case TB_CMP_SLT: return "cmp.slt";
        case TB_CMP_SLE: return "cmp.sle";
        case TB_CMP_FLT: return "cmp.lt";
        case TB_CMP_FLE: return "cmp.le";

        case TB_AND: return "and";
        case TB_OR: return "or";
        case TB_XOR: return "xor";
        case TB_ADD: return "add";
        case TB_SUB: return "sub";
        case TB_MUL: return "mul";
        case TB_UDIV: return "udiv";
        case TB_SDIV: return "sdiv";
        case TB_UMOD: return "umod";
        case TB_SMOD: return "smod";
        case TB_SHL: return "shl";
        case TB_SHR: return "shr";
        case TB_SAR: return "sar";
        case TB_NOT: return "not";

        case TB_LOAD: return "load";
        case TB_STORE: return "store";

        case TB_CALL: return "call";
        case TB_SCALL: return "syscall";
        case TB_BRANCH: return "br";
        case TB_RET: return "ret";

        default: return NULL;
    }
}

static void tb_print_node(TB_Function* f, TB_PrinterCtx* ctx, TB_PrintCallback callback, void* user_data, TB_Node* restrict n) {
    TB_NodeTypeEnum type = n->type;
    TB_DataType dt = n->dt;

    // check if non-void
    if ((dt.type == TB_INT && dt.data == 0) || n->type == TB_STORE) {
        P("  ");
    } else {
        P("  r%-8d = ", NAME(n));
        tb_print_type(dt, callback, user_data);
        P(".");
    }

    printf("%s ", tb_node_get_name(n));

    FOREACH_N(i, 0, n->input_count) {
        if (i) P(", ");

        if (n->inputs[i]) {
            P("r%d", NAME(n->inputs[i]));
        } else {
            P("_");
        }
    }

    switch (type) {
        case TB_INTEGER_CONST: {
            TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

            if (num->num_words == 1) {
                P("%"PRIu64, num->words[0]);
            } else {
                P("0x");
                FOREACH_N(i, 0, num->num_words) {
                    if (num) P("'");
                    P("%016"PRIx64, num->words[i]);
                }
            }
            break;
        }
        case TB_LOCAL: {
            TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
            P("@size %zu @align %zu", l->size, l->align);
            break;
        }
        case TB_GET_SYMBOL_ADDRESS: {
            TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n);
            P("%s", s->sym->name);
            break;
        }
        case TB_BRANCH: {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            if (br->count == 0) {
                P(" L%d", br->default_label);
            } else {
                P(" { ");
                FOREACH_N(i, 0, br->count) {
                    if (i) P(", ");
                    P("%llu: L%d", br->targets[i].key, br->targets[i].value);
                }
                P(" } else L%d", br->default_label);
            }
            break;
        }
        default: break;
    }

    for (TB_Attrib* attrib = n->first_attrib; attrib != NULL; attrib = attrib->next) {
        if (attrib->type == TB_ATTRIB_VARIABLE) {
            callback(user_data, ", var '%s'", attrib->var.name);
        } else {
            tb_todo();
        }
    }
}

TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data, bool display_nops) {
    P("%s:\n", f->super.name);
    TB_PrinterCtx ctx = { 0 };

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            int i = ctx.count++;
            nl_map_put(ctx.ordinals, n, i);
        }
    }

    TB_FOR_BASIC_BLOCK(bb, f) {
        if (f->bbs[bb].start == 0) continue;
        P("L%d:\n", bb);

        TB_FOR_NODE(n, f, bb) {
            if (!display_nops && n->type == TB_NULL) continue;

            tb_print_node(f, &ctx, callback, user_data, n);
            P("\n");
        }
    }
}
