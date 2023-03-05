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
    if (search >= 0) return ctx->ordinals[search].v;

    nl_map_put(ctx->ordinals, n, ctx->count);
    return ctx->count++;
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
    int name = NAME(n);
    if ((dt.type == TB_INT && dt.data == 0) || n->type == TB_STORE) {
        P("  ");
    } else {
        P("  r%-8d = ", name);
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

    #if 0
    TB_Reg i = n - f->nodes;
    TB_NodeTypeEnum type = n->type;
    TB_DataType dt = n->dt;

    switch (type) {
        case TB_NULL: callback(user_data, "  r%-8u = NOP", i); break;
        case TB_POISON: callback(user_data, "  r%-8u = POISON", i); break;
        case TB_DEBUGBREAK: callback(user_data, "  DEBUGBREAK"); break;
        case TB_INTEGER_CONST: {
            if (n->integer.num_words == 1) {
                callback(user_data, "  r%-8u = ", i);
                tb_print_type(dt, callback, user_data);
                callback(user_data, " %"PRIu64, n->integer.single_word);
            } else {
                callback(user_data, "  r%-8u = ", i);
                tb_print_type(dt, callback, user_data);
                callback(user_data, " 0x");
                FOREACH_REVERSE_N(i, 0, n->integer.num_words) {
                    callback(user_data, " %"PRIx64, n->integer.words[i]);
                }
            }
            break;
        }
        case TB_LINE_INFO: {
            callback(user_data, "  # line %s:%d", f->super.module->files[n->line_info.file].path, n->line_info.line);
            break;
        }
        case TB_FLOAT32_CONST: {
            callback(user_data, "  r%-8u = float ", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " %f", n->flt32.value);
            break;
        }
        case TB_FLOAT64_CONST: {
            callback(user_data, "  r%-8u = float ", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " %f", n->flt64.value);
            break;
        }
        case TB_MEMSET: {
            callback(user_data, "  memset r%d r%d r%d", n->mem_op.dst, n->mem_op.src, n->mem_op.size);
            break;
        }
        case TB_MEMCPY: {
            callback(user_data, "  memcpy r%d r%d r%d", n->mem_op.dst, n->mem_op.src, n->mem_op.size);
            break;
        }
        case TB_MEMBER_ACCESS: {
            callback(user_data, "  r%-8u = member", i);
            callback(user_data, " r%u, %d", n->member_access.base, n->member_access.offset);
            break;
        }
        case TB_ARRAY_ACCESS: {
            callback(user_data, "  r%-8u = array ", i);
            callback(user_data, "r%u, r%u, %d // ", n->array_access.base, n->array_access.index, n->array_access.stride);
            callback(user_data, "r%u + r%u*%d", n->array_access.base, n->array_access.index, n->array_access.stride);
            break;
        }
        case TB_ATOMIC_CMPXCHG:
        tb_assume(f->nodes[n->next].type == TB_ATOMIC_CMPXCHG2);
        callback(user_data, "  r%-8u = atomic.cmpxchg.", i);
        tb_print_type(dt, callback, user_data);
        callback(user_data, " %u, r%u, r%u", n->atomic.addr, n->atomic.src, f->nodes[n->next].atomic.src);
        break;
        case TB_ATOMIC_CMPXCHG2: break;
        case TB_ATOMIC_LOAD: {
            callback(user_data, "  r%-8u = atomic.load.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u, r%u", n->atomic.addr, n->atomic.src);
            break;
        }
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR: {
            callback(user_data, "  r%-8u = ", i);
            switch (type) {
                case TB_ATOMIC_XCHG: callback(user_data, "atomic.xchg."); break;
                case TB_ATOMIC_ADD: callback(user_data, "atomic.add."); break;
                case TB_ATOMIC_SUB: callback(user_data, "atomic.sub."); break;
                case TB_ATOMIC_AND: callback(user_data, "atomic.and."); break;
                case TB_ATOMIC_XOR: callback(user_data, "atomic.xor."); break;
                case TB_ATOMIC_OR: callback(user_data, "atomic.or."); break;
                default: tb_todo();
            }
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u, r%u", n->atomic.addr, n->atomic.src);
            break;
        }
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR: {
            callback(user_data, "  r%-8u = ", i);
            switch (type) {
                case TB_AND: callback(user_data, "and."); break;
                case TB_OR: callback(user_data, "or."); break;
                case TB_XOR: callback(user_data, "xor."); break;
                case TB_ADD: callback(user_data, "add."); break;
                case TB_SUB: callback(user_data, "sub."); break;
                case TB_MUL: callback(user_data, "mul."); break;
                case TB_UDIV: callback(user_data, "udiv."); break;
                case TB_SDIV: callback(user_data, "sdiv."); break;
                case TB_UMOD: callback(user_data, "umod."); break;
                case TB_SMOD: callback(user_data, "smod."); break;
                case TB_SHL: callback(user_data, "shl."); break;
                case TB_SHR: callback(user_data, "shr."); break;
                case TB_SAR: callback(user_data, "sar."); break;
                default: tb_todo();
            }
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u, r%u", n->i_arith.a, n->i_arith.b);

            if (n->i_arith.arith_behavior) {
                callback(user_data, " # ");
                if (n->i_arith.arith_behavior & TB_ARITHMATIC_NSW) {
                    callback(user_data, "nsw ");
                }

                if (n->i_arith.arith_behavior & TB_ARITHMATIC_NUW) {
                    callback(user_data, "nuw ");
                }
            }
            break;
        }
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV: {
            callback(user_data, "  r%-8u = ", i);
            switch (type) {
                case TB_FADD: callback(user_data, "fadd."); break;
                case TB_FSUB: callback(user_data, "fsub."); break;
                case TB_FMUL: callback(user_data, "fmul."); break;
                case TB_FDIV: callback(user_data, "fdiv."); break;
                default: tb_todo();
            }
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u, r%u", n->f_arith.a, n->f_arith.b);
            break;
        }
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        callback(user_data, "  r%-8u = ", i);
        switch (type) {
            case TB_CMP_NE: callback(user_data, "cmp.ne."); break;
            case TB_CMP_EQ: callback(user_data, "cmp.eq."); break;
            case TB_CMP_ULT: callback(user_data, "cmp.ult."); break;
            case TB_CMP_ULE: callback(user_data, "cmp.sle."); break;
            case TB_CMP_SLT: callback(user_data, "cmp.slt."); break;
            case TB_CMP_SLE: callback(user_data, "cmp.sle."); break;
            case TB_CMP_FLT: callback(user_data, "cmp.lt."); break;
            case TB_CMP_FLE: callback(user_data, "cmp.le."); break;
            default: tb_todo();
        }
        tb_print_type(n->cmp.dt, callback, user_data);
        callback(user_data, " r%u, r%u", n->cmp.a, n->cmp.b);
        break;
        case TB_BITCAST:
        case TB_NEG:
        case TB_NOT:
        case TB_VA_START:
        case TB_X86INTRIN_SQRT:
        case TB_X86INTRIN_RSQRT:
        case TB_FLOAT_EXT:
        case TB_ZERO_EXT:
        case TB_SIGN_EXT:
        case TB_INT2PTR:
        case TB_PTR2INT:
        case TB_FLOAT2INT:
        case TB_FLOAT2UINT:
        case TB_INT2FLOAT:
        case TB_UINT2FLOAT:
        case TB_TRUNCATE:
        callback(user_data, "  r%-8u = ", i);
        switch (type) {
            case TB_BITCAST: callback(user_data, "bitcast."); break;
            case TB_NEG: callback(user_data, "neg."); break;
            case TB_NOT: callback(user_data, "not."); break;
            case TB_VA_START: callback(user_data, "va.start."); break;
            case TB_X86INTRIN_SQRT: callback(user_data, "x86.sqrt."); break;
            case TB_X86INTRIN_RSQRT: callback(user_data, "x86.rsqrt."); break;
            case TB_FLOAT_EXT: callback(user_data, "fxt."); break;
            case TB_ZERO_EXT: callback(user_data, "zxt."); break;
            case TB_SIGN_EXT: callback(user_data, "sxt."); break;
            case TB_INT2PTR: callback(user_data, "int2ptr."); break;
            case TB_PTR2INT: callback(user_data, "ptr2int."); break;
            case TB_FLOAT2INT: callback(user_data, "float2int."); break;
            case TB_INT2FLOAT: callback(user_data, "int2float."); break;
            case TB_FLOAT2UINT: callback(user_data, "float2uint."); break;
            case TB_UINT2FLOAT: callback(user_data, "uint2float."); break;
            case TB_TRUNCATE: callback(user_data, "trunc."); break;
            default: tb_todo();
        }
        tb_print_type(dt, callback, user_data);
        callback(user_data, " r%u", n->unary.src);
        break;
        case TB_X86INTRIN_LDMXCSR:
        callback(user_data, "  r%-8u = ldmxcsr.", i);
        tb_print_type(dt, callback, user_data);
        callback(user_data, " r%u", n->unary.src);
        break;
        case TB_X86INTRIN_STMXCSR:
        callback(user_data, "  r%-8u = stmxcsr.", i);
        tb_print_type(dt, callback, user_data);
        break;
        case TB_LOCAL:
        callback(user_data, "  r%-8u = local %d (%d align)", i, n->local.size, n->local.alignment);
        break;
        case TB_ICALL: {
            callback(user_data, "  r%-8u = call.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " %s(", i, n->call.target->name);
            for (size_t j = n->call.param_start; j < n->call.param_end; j++) {
                if (j != n->call.param_start) callback(user_data, ", ");

                callback(user_data, "r%u", f->vla.data[j]);
            }
            callback(user_data, ")");
            break;
        }
        case TB_CALL: {
            callback(user_data, "  r%-8u = call.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " %s(", n->call.target->name);
            for (size_t j = n->call.param_start; j < n->call.param_end; j++) {
                if (j != n->call.param_start) callback(user_data, ", ");

                callback(user_data, "r%u", f->vla.data[j]);
            }
            callback(user_data, ")");
            break;
        }
        case TB_VCALL: {
            callback(user_data, "  r%-8u = call.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u(", n->vcall.target);
            for (size_t j = n->vcall.param_start; j < n->vcall.param_end; j++) {
                if (j != n->vcall.param_start) callback(user_data, ", ");

                callback(user_data, "r%u", f->vla.data[j]);
            }
            callback(user_data, ")");
            break;
        }
        case TB_SCALL: {
            callback(user_data, "  r%-8u = suscall.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " (r%u", n->scall.target);
            for (size_t j = n->scall.param_start; j < n->scall.param_end; j++) {
                callback(user_data, ", ");
                callback(user_data, "r%u", f->vla.data[j]);
            }
            callback(user_data, ")");
            break;
        }
        case TB_SWITCH: {
            callback(user_data, " switch.");
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u (\n", n->switch_.key);

            size_t entry_start = n->switch_.entries_start;
            size_t entry_count = (n->switch_.entries_end - n->switch_.entries_start) / 2;

            for (size_t j = 0; j < entry_count; j++) {
                TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];

                callback(user_data, "    %-8u -> L%d,\n", e->key, e->value);
            }
            callback(user_data, "    default -> L%d)", n->switch_.default_label);
            break;
        }
        case TB_GET_SYMBOL_ADDRESS: {
            callback(user_data, "  r%-8u = &%s", i, n->sym.value->name);
            break;
        }
        case TB_PARAM:
        callback(user_data, "  r%-8u = params.", i);
        tb_print_type(dt, callback, user_data);
        callback(user_data, " [%u]", n->param.id);
        break;
        case TB_PARAM_ADDR:
        callback(user_data, "  r%-8u = &params[%u]", i, f->nodes[n->param_addr.param].param.id);
        break;
        case TB_LOAD:
        callback(user_data, "  r%-8u = load.", i);
        tb_print_type(dt, callback, user_data);
        callback(user_data, " r%u (%d align)", n->load.address, n->load.alignment);
        break;
        case TB_STORE:
        callback(user_data, "  store.");
        tb_print_type(dt, callback, user_data);
        callback(user_data, " r%u, r%u (%d align)", n->store.address, n->store.value, n->store.alignment);
        break;
        case TB_GOTO: callback(user_data, "  goto L%d", n->goto_.label); break;
        case TB_IF:
        callback(user_data, "  if (r%u) L%d else L%d", n->if_.cond, n->if_.if_true, n->if_.if_false);
        break;
        case TB_PASS:
        callback(user_data, "  r%-8u = pass.", i);
        tb_print_type(dt, callback, user_data);
        callback(user_data, " r%u", n->pass);
        break;
        case TB_PHI1:
        case TB_PHI2:
        case TB_PHIN: {
            int count = tb_node_get_phi_width(f, i);
            TB_PhiInput* inputs = tb_node_get_phi_inputs(f, i);

            callback(user_data, "  r%-8u = phi.", i);
            tb_print_type(dt, callback, user_data);
            callback(user_data, " ");

            FOREACH_N(j, 0, count) {
                if (j) callback(user_data, ", ");

                callback(user_data, "L%d:r%u", inputs[j].label, inputs[j].val);
            }
            break;
        }
        case TB_RET:
        callback(user_data, "  ret");
        if (n->i_arith.a) {
            callback(user_data, ".");
            tb_print_type(dt, callback, user_data);
            callback(user_data, " r%u", n->i_arith.a);
        }
        break;
        case TB_TRAP:
        callback(user_data, "  trap");
        break;
        case TB_UNREACHABLE:
        callback(user_data, "  unreachable");
        break;
        default: tb_todo();
    }

    for (TB_Attrib* attrib = n->first_attrib; attrib != NULL; attrib = attrib->next) {
        if (attrib->type == TB_ATTRIB_VARIABLE) {
            callback(user_data, ", var '%s'", attrib->var.name);
        } else {
            tb_todo();
        }
    }
    #endif
}

TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data, bool display_nops) {
    P("%s:\n", f->super.name);
    TB_PrinterCtx ctx = { 0 };

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
