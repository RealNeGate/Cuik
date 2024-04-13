#include "tb_internal.h"
#include <stdarg.h>

TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf((FILE*)user_data, fmt, ap);
    va_end(ap);
}

const char* tb_node_get_name(TB_Node* n) {
    switch (n->type) {
        case TB_NULL: return "FREED";
        case TB_UNREACHABLE: return "unreachable";

        case TB_BRANCH_PROJ:   return "br_proj";

        case TB_ROOT:   return "root";
        case TB_RETURN: return "return";
        case TB_PROJ:   return "proj";
        case TB_REGION: return "region";
        case TB_NATURAL_LOOP: return "loop";
        case TB_AFFINE_LOOP: return "loop.affine";
        case TB_CALLGRAPH: return "callgraph";

        case TB_LOCAL: return "local";

        case TB_VA_START: return "vastart";
        case TB_DEBUGBREAK: return "dbgbrk";

        case TB_POISON: return "poison";
        case TB_DEAD: return "dead";
        case TB_ICONST: return "int";
        case TB_F32CONST: return "float32";
        case TB_F64CONST: return "float64";

        case TB_PHI: return "phi";
        case TB_SELECT: return "select";
        case TB_LOOKUP: return "lookup";

        case TB_PTR_OFFSET: return "ptr_offset";

        case TB_CYCLE_COUNTER: return "cyclecnt";
        case TB_SAFEPOINT_POLL: return "safepoint.poll";

        case TB_MEMSET: return "memset";
        case TB_MEMCPY: return "memcpy";
        case TB_SPLITMEM: return "split";
        case TB_MERGEMEM: return "merge";

        case TB_ZERO_EXT: return "zxt";
        case TB_SIGN_EXT: return "sxt";
        case TB_FLOAT_TRUNC: return "fptrunc";
        case TB_FLOAT_EXT: return "fpxt";
        case TB_TRUNCATE: return "trunc";
        case TB_BITCAST: return "bitcast";
        case TB_UINT2FLOAT: return "uint2float";
        case TB_TAG_INT2FLOAT: return "int2float";
        case TB_FLOAT2UINT: return "float2uint";
        case TB_FLOAT2INT: return "float2int";
        case TB_SYMBOL: return "symbol";

        case TB_CMP_NE: return "cmp.ne";
        case TB_CMP_EQ: return "cmp.eq";
        case TB_CMP_ULT: return "cmp.ult";
        case TB_CMP_ULE: return "cmp.ule";
        case TB_CMP_SLT: return "cmp.slt";
        case TB_CMP_SLE: return "cmp.sle";
        case TB_CMP_FLT: return "cmp.lt";
        case TB_CMP_FLE: return "cmp.le";

        case TB_ATOMIC_LOAD: return "atomic.load";
        case TB_ATOMIC_XCHG: return "atomic.xchg";
        case TB_ATOMIC_ADD: return "atomic.add";
        case TB_ATOMIC_SUB: return "atomic.sub";
        case TB_ATOMIC_AND: return "atomic.and";
        case TB_ATOMIC_XOR: return "atomic.xor";
        case TB_ATOMIC_OR: return "atomic.or";
        case TB_ATOMIC_CAS: return "atomic.cas";

        case TB_CLZ: return "clz";
        case TB_CTZ: return "ctz";
        case TB_NEG: return "neg";
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
        case TB_ADC: return "adc";

        case TB_FADD: return "fadd";
        case TB_FSUB: return "fsub";
        case TB_FMUL: return "fmul";
        case TB_FDIV: return "fdiv";
        case TB_FMAX: return "fmax";
        case TB_FMIN: return "fmin";

        case TB_MULPAIR:  return "mulpair";
        case TB_LOAD:     return "load";
        case TB_STORE:    return "store";
        case TB_READ:     return "read";
        case TB_WRITE:    return "write";

        case TB_CALL:     return "call";
        case TB_SYSCALL:  return "syscall";
        case TB_BRANCH:   return "branch";
        case TB_AFFINE_LATCH: return "affine_latch";
        case TB_NEVER_BRANCH: return "never_branch";
        case TB_TAILCALL: return "tailcall";

        case TB_MACH_MOVE:  return "mach_move";
        case TB_MACH_COPY:  return "mach_copy";
        case TB_MACH_PROJ:  return "mach_proj";
        case TB_MACH_SYMBOL:return "mach_symbol";
        case TB_MACH_FRAME_PTR: return "mach_frameptr";

        default: {
            int family = n->type / 0x100;
            assert(family >= 1 && family < TB_ARCH_MAX);
            return tb_codegen_families[family].node_name(n);
        }
    }
}

#define P(...) callback(user_data, __VA_ARGS__)
static void tb_print_type(TB_DataType dt, TB_PrintCallback callback, void* user_data) {
    switch (dt.type) {
        case TB_TAG_INT: {
            if (dt.data == 0) P("void");
            else P("i%d", dt.data);
            break;
        }
        case TB_TAG_PTR: {
            if (dt.data == 0) P("ptr");
            else P("ptr%d", dt.data);
            break;
        }
        case TB_TAG_F32: {
            P("f32");
            break;
        }
        case TB_TAG_F64: {
            P("f64");
            break;
        }
        case TB_TAG_TUPLE: {
            P("tuple");
            break;
        }
        case TB_TAG_MEMORY: {
            P("memory");
            break;
        }
        case TB_TAG_CONTROL: {
            P("control");
            break;
        }
        default: tb_todo();
    }
}

static void print_proj(TB_PrintCallback callback, void* user_data, TB_Node* n, TB_Node* proj, bool mach) {
    int index = TB_NODE_GET_EXTRA_T(proj, TB_NodeProj)->index;
    switch (n->type) {
        case TB_ROOT: {
            if (mach) {
                P("%c", 'A'+index);
            } else {
                if (index == 0) {
                    P("ctrl");
                } else if (index == 1) {
                    P("mem");
                } else if (index == 2) {
                    P("rpc");
                } else {
                    P("%c", 'a'+(index - 3));
                }
            }
            break;
        }

        case TB_NEVER_BRANCH: {
            P(index ? "never" : "taken");
            break;
        }

        case TB_BRANCH: {
            TB_NodeBranchProj* br = TB_NODE_GET_EXTRA(n);
            if (index == 0) {
                P("else");
            } else {
                P("on %"PRId64, br->key);
            }
            break;
        }

        case TB_TAILCALL:
        if (index == 0) {
            P("ctrl");
        } else if (index == 1) {
            P("mem");
        } else if (index == 2) {
            P("rpc");
        } else {
            P("val");
        }
        break;

        case TB_CALL:
        if (index == 0) {
            P("ctrl");
        } else if (index == 1) {
            P("mem");
        } else {
            P("val");
        }
        break;

        case TB_SPLITMEM:
        P("mem");
        break;

        default: tb_todo();
    }
}

static void print_graph_node(TB_Function* f, TB_PrintCallback callback, void* user_data, size_t bb, TB_Node* restrict n) {
    P("  r%u [label=\"{", n->gvn);

    bool ins = false;
    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        if (!ins) P("{");
        else P("|");

        P("<i%zu> %zu", i, i);
        ins = true;
    }

    if (ins) {
        P("}|");
    }

    if (n->dt.type == TB_TAG_TUPLE) {
        TB_Node* projs[128] = { 0 };
        int limit = 0;
        FOR_USERS(u, n) {
            if (is_proj(USERN(u))) {
                int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                if (limit < index+1) limit = index+1;
                projs[index] = USERN(u);
            }
        }

        P("%s|{", tb_node_get_name(n));
        int outs = 0;
        FOR_N(i, 0, limit) {
            if (projs[i] != NULL) {
                if (outs) P("|");
                P("<p%zu>", i);
                print_proj(callback, user_data, n, projs[i], false);
                outs++;
            }
        }
        P("}}\"]");
    } else {
        switch (n->type) {
            case TB_REGION: P("region"); break;

            case TB_LOAD: {
                P("ld.");
                tb_print_type(n->dt, callback, user_data);
                break;
            }
            case TB_STORE: {
                P("st.");
                tb_print_type(n->inputs[2]->dt, callback, user_data);
                break;
            }

            case TB_SYMBOL: {
                TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
                if (sym->name[0]) {
                    P("sym: %s", sym->name);
                } else {
                    P("sym: %p", sym);
                }
                break;
            }

            case TB_BITCAST: {
                P("bitcast ");
                tb_print_type(n->inputs[1]->dt, callback, user_data);
                P(" to ");
                tb_print_type(n->dt, callback, user_data);
                break;
            }

            case TB_ICONST: {
                TB_NodeInt* num = TB_NODE_GET_EXTRA(n);
                if (num->value < 0xFFFF) {
                    P("cst: %"PRId64, num->value);
                } else {
                    P("cst: %#0"PRIx64, num->value);
                }
                break;
            }

            default:
            P("%s", tb_node_get_name(n));
            break;
        }
        P("}\"]");
    }

    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        TB_Node* in = n->inputs[i];

        const char* color = "black";
        if (in->dt.type == TB_TAG_CONTROL) {
            color = "red";
        } else if (in->dt.type == TB_TAG_MEMORY) {
            color = "blue";
        }

        if (in->type == TB_MACH_PROJ) {
            int index = TB_NODE_GET_EXTRA_T(in, TB_NodeProj)->index;
            P("; r%u:m%d -> r%u:i%zu [color=%s]", in->inputs[0]->gvn, index, n->gvn, i, color);
        } else if (in->type == TB_PROJ) {
            int index = TB_NODE_GET_EXTRA_T(in, TB_NodeProj)->index;
            P("; r%u:p%d -> r%u:i%zu [color=%s]", in->inputs[0]->gvn, index, n->gvn, i, color);
        } else {
            P("; r%u -> r%u:i%zu [color=%s]", in->gvn, n->gvn, i, color);
        }
    }
    P("\n");
}

void tb_print_dot(TB_Function* f, TB_PrintCallback callback, void* user_data) {
    P("digraph %s {\n  node [ordering=in; shape=record];\n", f->super.name ? f->super.name : "unnamed");

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);
    worklist_push(&ws, f->root_node);

    for (size_t i = 0; i < dyn_array_length(ws.items); i++) {
        TB_Node* n = ws.items[i];
        FOR_USERS(u, n) { worklist_push(&ws, USERN(u)); }
        if (n->type != TB_PROJ && n->type != TB_MACH_PROJ) { print_graph_node(f, callback, user_data, i, n); }
    }

    worklist_free(&ws);

    P("}\n");
}
