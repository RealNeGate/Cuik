#include "tb_internal.h"
#include <stdarg.h>

TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf((FILE*)user_data, fmt, ap);
    va_end(ap);
}

TB_API const char* tb_node_get_name(TB_Node* n) {
    switch (n->type) {
        case TB_NULL: return "BAD";

        case TB_START:  return "start";
        case TB_RET:    return "ret";
        case TB_PROJ:   return "proj";
        case TB_REGION: return "region";

        case TB_LOCAL: return "local";

        case TB_VA_START: return "vastart";
        case TB_DEBUGBREAK: return "dbgbrk";

        case TB_POISON: return "poison";
        case TB_INTEGER_CONST: return "int";
        case TB_FLOAT32_CONST: return "float32";
        case TB_FLOAT64_CONST: return "float64";

        case TB_PHI: return "phi";

        case TB_ARRAY_ACCESS: return "array";
        case TB_MEMBER_ACCESS: return "member";

        case TB_PTR2INT: return "ptr2int";
        case TB_INT2PTR: return "int2ptr";

        case TB_MEMSET: return "memset";
        case TB_MEMCPY: return "memcpy";

        case TB_ZERO_EXT: return "zxt";
        case TB_SIGN_EXT: return "sxt";
        case TB_FLOAT_EXT: return "fpxt";
        case TB_TRUNCATE: return "trunc";
        case TB_BITCAST: return "bitcast";
        case TB_UINT2FLOAT: return "uint2float";
        case TB_INT2FLOAT: return "int2float";
        case TB_FLOAT2UINT: return "float2uint";
        case TB_FLOAT2INT: return "float2int";
        case TB_GET_SYMBOL_ADDRESS: return "symbol";

        case TB_CMP_NE: return "cmp.ne";
        case TB_CMP_EQ: return "cmp.eq";
        case TB_CMP_ULT: return "cmp.ult";
        case TB_CMP_ULE: return "cmp.ule";
        case TB_CMP_SLT: return "cmp.slt";
        case TB_CMP_SLE: return "cmp.sle";
        case TB_CMP_FLT: return "cmp.lt";
        case TB_CMP_FLE: return "cmp.le";

        case TB_NEG: return "neg";
        case TB_NOT: return "not";
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
        case TB_ROL: return "rol";
        case TB_ROR: return "ror";
        case TB_SAR: return "sar";

        case TB_FADD: return "fadd";
        case TB_FSUB: return "fsub";
        case TB_FMUL: return "fmul";
        case TB_FDIV: return "fdiv";

        case TB_MULPAIR: return "mulpair";
        case TB_LOAD: return "load";
        case TB_STORE: return "store";

        case TB_CALL: return "call";
        case TB_SCALL: return "syscall";
        case TB_BRANCH: return "br";

        default: tb_todo();return "(unknown)";
    }
}

typedef struct {
    NL_Map(TB_Node*, char) visited;
    NL_Map(TB_Node*, int) ordinals;
    int count;
} TB_PrinterCtx;

// returns true if it's the first time
static bool visit(TB_PrinterCtx* ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->visited, n);
    if (search < 0) {
        nl_map_put(ctx->visited, n, 0);
        return true;
    }

    return false;
}

// just a unique identifier for printing
#define P(...) callback(user_data, __VA_ARGS__)
#define NAME(n) get_name(ctx, n)
static int get_name(TB_PrinterCtx* ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->ordinals, n);
    if (search < 0) {
        nl_map_put(ctx->ordinals, n, ctx->count);
        return ctx->count++;
    }

    return ctx->ordinals[search].v;
}

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
        case TB_TUPLE: {
            P("tuple");
            break;
        }
        case TB_CONTROL: {
            P("control");
            break;
        }
        default: tb_todo();
    }
}

#if 0
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

    P("%s ", tb_node_get_name(n));

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
        case TB_MEMBER_ACCESS: {
            TB_NodeMember* m = TB_NODE_GET_EXTRA(n);
            P(" + %lld", m->offset);
            break;
        }
        case TB_ARRAY_ACCESS: {
            TB_NodeArray* a = TB_NODE_GET_EXTRA(n);
            P(" * %lld", a->stride);
            break;
        }
        case TB_GET_SYMBOL_ADDRESS: {
            TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n);
            P("%s", s->sym->name);
            break;
        }
        case TB_LINE_INFO: {
            TB_NodeLine* l = TB_NODE_GET_EXTRA(n);
            TB_Module* m = f->super.module;

            P("\"%s\" @ line %d", m->files[l->file].path, l->line);
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
#endif

static int tb_print_node(TB_Function* f, TB_PrinterCtx* ctx, TB_PrintCallback callback, void* user_data, TB_Node* restrict n) {
    int id = get_name(ctx, n);
    if (!visit(ctx, n)) {
        return id;
    }

    P("  r%p [label=\"%s ", n, tb_node_get_name(n));
    switch (n->type) {
        case TB_INTEGER_CONST: {
            TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

            if (num->num_words == 1) {
                P("%"PRIu64" ", num->words[0]);
            } else {
                P("0x");
                FOREACH_REVERSE_N(i, 0, num->num_words) {
                    if (num) P("'");
                    P("%016"PRIx64, num->words[i]);
                }
                P(" ");
            }

            tb_print_type(n->dt, callback, user_data);
            break;
        }

        case TB_MEMBER_ACCESS: {
            TB_NodeMember* m = TB_NODE_GET_EXTRA(n);
            P("%"PRId64" ", m->offset);
            break;
        }

        default:
        tb_print_type(n->dt, callback, user_data);
        break;
    }
    P("\"];\n");

    FOREACH_N(i, 0, n->input_count) {
        if (n->inputs[i]->type != TB_START && n->inputs[i]->type != TB_REGION) {
            tb_print_node(f, ctx, callback, user_data, n->inputs[i]);
        }

        P("  r%p -> r%p", n->inputs[i], n);
        if ((n->type == TB_PROJ && n->dt.type == TB_CONTROL) || (i == 0 && tb_has_effects(n))) {
            P(" [color=\"red\"]");
        }

        if (n->type == TB_PHI && i > 0) {
            P(" [label=\"%zu\"];\n", i - 1);
        } else if (n->type == TB_PROJ) {
            P(" [label=\"%d\"];\n", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
        } else {
            P("\n");
        }
    }

    return id;
}

TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data) {
    P("digraph %s {\n  rankdir=\"TB\"\n", f->super.name ? f->super.name : "unnamed");
    TB_PrinterCtx ctx = { 0 };
    TB_PostorderWalk order = tb_function_get_postorder(f);

    FOREACH_REVERSE_N(i, 0, order.count) {
        TB_Node* region = order.traversal[i];
        TB_Node* n = TB_NODE_GET_EXTRA_T(region, TB_NodeRegion)->end;

        // P("subgraph cluster_%p {\nstyle=filled;color=lightgrey;\n", n);
        do {
            tb_print_node(f, &ctx, callback, user_data, n);
            n = n->inputs[0];
        } while (n->type != TB_START && n->type != TB_REGION);

        tb_print_node(f, &ctx, callback, user_data, n);
        // P("}\n");
    }

    tb_function_free_postorder(&order);
    P("}\n\n");
}
