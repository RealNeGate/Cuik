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
        case TB_DEAD: return "dead";

        case TB_START:  return "start";
        case TB_END:    return "end";
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
        case TB_SELECT: return "select";

        case TB_ARRAY_ACCESS: return "array";
        case TB_MEMBER_ACCESS: return "member";

        case TB_PTR2INT: return "ptr2int";
        case TB_INT2PTR: return "int2ptr";

        case TB_SAFEPOINT_POLL: return "safepoint";

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
        case TB_FMAX: return "fmax";
        case TB_FMIN: return "fmin";

        case TB_MULPAIR: return "mulpair";
        case TB_LOAD: return "load";
        case TB_STORE: return "store";
        case TB_MERGEMEM: return "merge";

        case TB_CALL: return "call";
        case TB_SYSCALL: return "syscall";
        case TB_BRANCH: return "branch";

        default: tb_todo();return "(unknown)";
    }
}

#define P(...) callback(user_data, __VA_ARGS__)
static void tb_print_type(TB_DataType dt, TB_PrintCallback callback, void* user_data) {
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
        case TB_MEMORY: {
            P("memory");
            break;
        }
        case TB_CONTROL: {
            P("control");
            break;
        }
        case TB_CONT: {
            P("cont");
            break;
        }
        default: tb_todo();
    }
}

static bool print_graph_node(TB_Function* f, NL_HashSet* visited, TB_PrintCallback callback, void* user_data, TB_Node* restrict n) {
    if (!nl_hashset_put(visited, n)) {
        return false;
    }

    bool is_effect = tb_has_effects(n);
    const char* fillcolor = is_effect ? "lightgrey" : "antiquewhite1";
    if (n->dt.type == TB_MEMORY) {
        fillcolor = "lightblue";
    }

    P("  r%p [style=\"filled\"; ordering=in; shape=box; fillcolor=%s; label=\"", n, fillcolor);
    P("%zu: %s", n->gvn, tb_node_get_name(n));
    P("\"];\n");

    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
        TB_Node* in = n->inputs[i];
        P("  r%p -> r%p\n", in, n);
        print_graph_node(f, visited, callback, user_data, in);
    }

    return true;
}

TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data) {
    P("digraph %s {\n  rankdir=TB\n", f->super.name ? f->super.name : "unnamed");

    NL_HashSet visited = nl_hashset_alloc(f->node_count);
    DynArray(TB_Node*) terminators = f->terminators;
    dyn_array_for(i, terminators) {
        print_graph_node(f, &visited, callback, user_data, terminators[i]);
    }
    nl_hashset_free(visited);

    P("}\n\n");
}
