#ifdef TB_HAS_X64
#include "x64.h"
#include <tb_x64.h>
#include "x64_emitter.h"
#include "x64_disasm.c"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_XMM,
    REG_CLASS_FLAGS,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 8,
    BUNDLE_INST_MAX = 1,
};

#include "../codegen_impl.h"

enum {
    MODE_REG,
    MODE_LD, // reg <- mem
    MODE_ST, // mem <- reg
};

enum {
    OP_IMMEDIATE = 1,
    OP_INDEXED   = 2,
};

// node with X86MemOp (mov, add, and...) will have this layout of inputs:
//   [1] mem
//   [2] base (or first src)
//   [3] idx
//   [4] val (only if flags' HAS_IMMEDIATE is unset)
typedef struct {
    uint8_t mode  : 2;
    uint8_t scale : 2;
    uint8_t cond  : 4;
    uint8_t flags;

    TB_DataType extra_dt;

    int32_t disp;
    int32_t imm;
} X86MemOp;

typedef struct {
    TB_FunctionPrototype* proto;
    int param_count;
} X86Call;

static bool fits_into_uint8(TB_DataType dt, TB_Node* n) {
    TB_ASSERT(n->type == TB_ICONST);
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return i->value <= 255;
}

static bool fits_into_int32(TB_DataType dt, TB_Node* n) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (dt.type == TB_TAG_I64 || dt.type == TB_TAG_PTR) {
        bool sign = (i->value >> 31ull) & 1;
        uint64_t top = i->value >> 32ull;

        // if the sign matches the rest of the top bits, we can sign extend just fine
        if (top != (sign ? 0xFFFFFFFF : 0)) {
            return false;
        }
    }

    return true;
}

static bool fits_into_scale(TB_Node* n) {
    TB_ASSERT(n->type == TB_ICONST);
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return i->value <= 3;
}

static int32_t as_int32(TB_Node* n) {
    TB_ASSERT(n->type == TB_ICONST);
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return i->value;
}

static bool is_float_zero(TB_Node* n) {
    if (n->dt.type == TB_TAG_F32) {
        uint32_t imm = (Cvt_F32U32) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value }.i;
        return imm == 0;
    } else {
        uint64_t imm = (Cvt_F64U64) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value }.i;
        return imm == 0;
    }
}

static TB_Symbol* gimme_float_neg_zero(Ctx* ctx, TB_DataType dt) {
    if (dt.type == TB_TAG_F32) {
        uint32_t imm[4];
        FOR_N(i, 0, 4) { imm[i] = 0x80000000; }

        TB_Global* g = tb__small_data_intern(ctx->module, sizeof(imm), imm);
        return &g->super;
    } else {
        uint64_t imm[2];
        FOR_N(i, 0, 2) { imm[i] = 1ull << 63ull; }

        TB_Global* g = tb__small_data_intern(ctx->module, sizeof(imm), imm);
        return &g->super;
    }
}

static TB_Symbol* gimme_float_sym(Ctx* ctx, TB_Node* n) {
    size_t elem_size = 0;
    void* elem = 0;

    char x[64];
    size_t vec_size = 0;
    if (n->type == TB_VBROADCAST) {
        if (n->dt.type == TB_TAG_V128) {
            vec_size = 16;
        } else if (n->dt.type == TB_TAG_V256) {
            vec_size = 32;
        } else {
            tb_todo();
        }
        n = n->inputs[1];
    }

    if (n->type == TB_F32CONST) {
        elem_size = 4;
        elem = &TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value;
    } else {
        TB_ASSERT(n->type == TB_F64CONST);
        elem_size = 8;
        elem = &TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value;
    }

    if (vec_size == 0) {
        vec_size = elem_size;
    }

    FOR_N(i, 0, vec_size / elem_size) {
        memcpy(&x[i * elem_size], elem, elem_size);
    }

    TB_Global* g = tb__small_data_intern(ctx->module, vec_size, x);
    return &g->super;
}

static TB_Node* isel_va_start(Ctx* ctx, TB_Function* f, TB_Node* n);

#include "x64_gen.h"

static TB_Node* isel_va_start(Ctx* ctx, TB_Function* f, TB_Node* n) {
    TB_ASSERT(ctx->module->target_abi == TB_ABI_WIN64 && "How does va_start even work on SysV?");

    // on Win64 va_start just means whatever is one parameter away from
    // the parameter you give it (plus in Win64 the parameters in the stack
    // are 8bytes, no fanciness like in SysV):
    // void printf(const char* fmt, ...) {
    //     va_list args;
    //     va_start(args, fmt); // args = ((char*) &fmt) + 8;
    //     ...
    // }
    TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
    set_input(f, op, ctx->frame_ptr, 2);

    TB_FunctionPrototype* proto = ctx->f->prototype;
    X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
    op_extra->mode = MODE_LD;
    op_extra->disp = -(8 + proto->param_count*8);
    return op;
}

static bool can_gvn(TB_Node* n) {
    return true;
}

static uint32_t node_flags(TB_Node* n) {
    if (n->type == x86_jcc) {
        return NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH | NODE_MEMORY_IN;
    }

    return n->dt.type == TB_TAG_MEMORY ? (NODE_MEMORY_IN | NODE_MEMORY_OUT | NODE_PINNED) : NODE_MEMORY_IN;
}

/*
static uint32_t node_flags(TB_Node* n) {
    X86NodeType type = n->type;
    switch (type) {
        case x86_idiv: case x86_div:
        return NODE_MEMORY_IN;

        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub: case x86_xor:
        case x86_cmp: case x86_mov: case x86_test: case x86_imul: case x86_lea:
        case x86_shl: case x86_shr: case x86_sar: case x86_rol: case x86_ror:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor: case x86_ucomi:
        return n->dt.type == TB_TAG_MEMORY ? (NODE_MEMORY_IN | NODE_MEMORY_OUT | NODE_PINNED) : NODE_MEMORY_IN;

        case x86_call:
        case x86_static_call:
        return NODE_MEMORY_IN | NODE_MEMORY_OUT | NODE_PINNED;

        case x86_cmovcc:
        return NODE_MEMORY_IN;

        case x86_cmpjcc:
        case x86_testjcc:
        case x86_ucomijcc:
        case x86_AAAHHHH:
        return NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH | NODE_MEMORY_IN;

        default:
        return 0;
    }
}*/

static size_t extra_bytes(TB_Node* n) {
    return sizeof(X86MemOp);
}

static void print_extra(TB_Node* n) {
    static const char* modes[] = { "reg", "ld", "st" };

    X86MemOp* op = TB_NODE_GET_EXTRA(n);
    printf("scale=%d, disp=%d, mode=%s", 1u<<op->scale, op->disp, modes[op->mode]);
    if (op->flags & OP_IMMEDIATE) {
        printf(", imm=%d", op->imm);
    }
    printf(", cond=%s", COND_NAMES[op->cond]);
}

static void print_pretty_edge(Ctx* restrict ctx, TB_Node* n) {
    int vreg_id = ctx->vreg_map[n->gvn];
    if (0 && vreg_id > 0 && ctx->vregs && ctx->vregs[vreg_id].assigned >= 0) {
        /* VReg* v = &ctx->vregs[vreg_id];
        if (v->class == REG_CLASS_GPR) {
            printf("%s", GPR_NAMES[v->assigned]);
        } else if (v->class == REG_CLASS_XMM) {
            printf("XMM%d", v->assigned);
        } else if (v->class == REG_CLASS_STK) {
            printf("STACK%d", v->assigned);
        } else if (v->class == REG_CLASS_FLAGS) {
            printf("FLAGS");
        } */
        printf("V%d", vreg_id);
    } else {
        printf("%%%u", n->gvn);
    }
}

static void print_pretty(Ctx* restrict ctx, TB_Node* n) {
    if (n->type == TB_MACH_FRAME_PTR) {
        printf("  frameptr ");
        print_pretty_edge(ctx, n);
    } else if (n->type == TB_PROJ) {
        printf("  proj ");
        print_pretty_edge(ctx, n);
        printf(" = ");
        print_pretty_edge(ctx, n->inputs[0]);
        printf(", %d", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
    } else if (n->type == TB_MACH_PROJ) {
        printf("  proj ");
        print_pretty_edge(ctx, n);
        printf(" = ");
        print_pretty_edge(ctx, n->inputs[0]);
        printf(", %d ", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
        tb__print_regmask(TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->def);
    } else if (n->type == TB_MACH_TEMP) {
        printf("  temp ");
        print_pretty_edge(ctx, n);
        printf(" = ");
        tb__print_regmask(TB_NODE_GET_EXTRA_T(n, TB_NodeMachTemp)->def);
    } else if (n->type == TB_MACH_JUMP || n->type == x86_jcc) {
        int succ[2] = { -1, -1 };
        FOR_SUCC(s, n) {
            int index = TB_NODE_GET_EXTRA_T(s.succ, TB_NodeProj)->index;
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, s.succ);
            int b = succ_bb - ctx->cfg.blocks;
            if (ctx->cfg.blocks[b].fwd > 0) {
                while (b != ctx->cfg.blocks[b].fwd) {
                    b = ctx->cfg.blocks[b].fwd;
                }
            }
            succ[index] = b;
        }

        if (n->type == x86_jcc) {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf("  j%s ", COND_NAMES[op->cond]);
            print_pretty_edge(ctx, n->inputs[1]);
            printf(" .bb%d else .bb%d", succ[0], succ[1]);
        } else {
            printf("  jmp .bb%d", succ[0]);
        }
    } else if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);

        if (reg_mask_is_spill(cpy->def) && cpy->use->class != REG_CLASS_STK) {
            printf("  spill ");
            print_pretty_edge(ctx, n);
            printf(" = ");
            print_pretty_edge(ctx, n->inputs[1]);
            printf(": %s", reg_class_name(cpy->use->class));
        } else if (cpy->def->class != REG_CLASS_STK && reg_mask_is_spill(cpy->use)) {
            printf("  reload ");
            print_pretty_edge(ctx, n);
            printf(": %s = ", reg_class_name(cpy->def->class));
            print_pretty_edge(ctx, n->inputs[1]);
        } else {
            printf("  copy ");
            print_pretty_edge(ctx, n);
            printf(": ");
            tb__print_regmask(cpy->def);
            printf(" = ");
            print_pretty_edge(ctx, n->inputs[1]);
            printf(": ");
            tb__print_regmask(cpy->use);
        }
    } else if (n->type == TB_ICONST) {
        int bytes;
        switch (n->dt.type) {
            case TB_TAG_I8:  bytes = 1; break;
            case TB_TAG_I16: bytes = 2; break;
            case TB_TAG_I32: bytes = 4; break;
            case TB_TAG_I64: bytes = 8; break;
            case TB_TAG_PTR: bytes = 8; break;
            default: tb_todo();
        }
        printf("  mov%d ", bytes);
        print_pretty_edge(ctx, n);
        printf(" = %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value);
    } else if (n->type == TB_PHI) {
        printf("  ");
        print_pretty_edge(ctx, n);
        printf(" = phi(");
        FOR_N(i, 1, n->input_count) {
            if (i != 1) { printf(", "); }
            print_pretty_edge(ctx, n->inputs[i]);
        }
        printf(")");
    } else if (n->type == x86_vzero) {
        printf("  vzero ");
        print_pretty_edge(ctx, n);
    } else if (n->type == TB_VSHUFFLE) {
        printf("  vshuffle ");
        print_pretty_edge(ctx, n);
        printf(" = ");
        print_pretty_edge(ctx, n->inputs[1]);
        if (n->input_count >= 3) {
            printf(", ");
            print_pretty_edge(ctx, n->inputs[2]);
        }

        TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
        printf(", [");
        FOR_N(i, 0, shuf->width) {
            if (i) { printf(", "); }
            printf("%d", shuf->indices[i]);
        }
        printf("]");
    } else if (n->type >= TB_MACH_X86) {
        const char* name = tb_node_get_name(n->type);
        name += 4;

        TB_DataType dt = n->dt;
        X86MemOp* op = TB_NODE_GET_EXTRA(n);
        if (TB_IS_INT_OR_PTR(dt)) {
            int bytes;
            switch (dt.type) {
                case TB_TAG_I8:  bytes = 1; break;
                case TB_TAG_I16: bytes = 2; break;
                case TB_TAG_I32: bytes = 4; break;
                case TB_TAG_I64: bytes = 8; break;
                case TB_TAG_PTR: bytes = 8; break;
                default: tb_todo();
            }

            if (n->type == x86_movzx8 || n->type == x86_movzx16 ||
                n->type == x86_movsx8 || n->type == x86_movsx16 || n->type == x86_movsx32
            ) {
                printf("  %s_%d ", name, bytes);
            } else if (n->type == x86_cmovcc) {
                printf("  %s%s_%d ", name, COND_NAMES[op->cond], bytes);
            } else {
                printf("  %s%d ", name, bytes);
            }
        } else {
            printf("  %s ", name);
        }

        if (n->dt.type != TB_TAG_MEMORY) {
            print_pretty_edge(ctx, n);
            printf(" = ");
        }

        int rx_i = op->flags & OP_INDEXED ? 4 : 3;

        // if we're in load-form we print the RX first
        if (op->mode != MODE_ST && rx_i < n->input_count && n->inputs[rx_i]) {
            print_pretty_edge(ctx, n->inputs[rx_i]);
            printf(", ");
        }

        // print memory operand
        if (op->mode == MODE_LD || op->mode == MODE_ST) {
            printf("[");
            print_pretty_edge(ctx, n->inputs[2]);
            if (op->flags & OP_INDEXED) {
                printf(" + ");
                print_pretty_edge(ctx, n->inputs[3]);
                if (op->scale) {
                    printf("*%d", 1 << op->scale);
                }
            }
            if (op->disp) {
                printf(" + %"PRId32, op->disp);
            }
            printf("]");
        } else if (op->mode == MODE_REG) {
            print_pretty_edge(ctx, n->inputs[2]);
        }

        if (op->mode == MODE_ST && rx_i < n->input_count && n->inputs[rx_i]) {
            printf(", ");
            print_pretty_edge(ctx, n->inputs[rx_i]);
        }

        if (op->flags & OP_IMMEDIATE) {
            printf(", %"PRId32, op->imm);
        }

        int first = rx_i+1;
        FOR_N(i, first, n->input_count) {
            printf(", ");
            if (n->inputs[i]) {
                print_pretty_edge(ctx, n->inputs[i]);
            } else {
                printf("___");
            }
        }
    } else {
        // tb_print_dumb_node(NULL, n);

        printf("  %s ", tb_node_get_name(n->type));
        if (n->dt.type != TB_TAG_TUPLE && n->dt.type != TB_TAG_CONTROL) {
            print_pretty_edge(ctx, n);
            printf(" = ");
        }

        FOR_N(i, 1, n->input_count) {
            if (i != 1) { printf(", "); }
            print_pretty_edge(ctx, n->inputs[i]);
        }

        if (n->type == TB_MACH_SYMBOL) {
            TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
            if (sym->name[0]) {
                printf("%s", sym->name);
            } else {
                printf("sym%p", sym);
            }
        }
    }
}

static CallingConv CC_WIN64 = {
    .sp_class  = REG_CLASS_GPR, .sp_reg  = RSP, // stack pointer
    .fp_class  = REG_CLASS_GPR, .fp_reg  = RBP, // frame pointer
    .rpc_class = REG_CLASS_STK, .rpc_reg = 0,   // return PC

    .reg_saves[REG_CLASS_GPR] = (char[16]){
        // volatiles (caller saves)
        [RAX] = 'C', [RCX] = 'C', [RDX] = 'C', [R8] = 'C', [R9] = 'C', [R10] = 'C', [R11] = 'C',
        // non-volatile (callee saves)
        [RBX] = 'c', [RBP] = 'c', [RDI] = 'c', [RSI] = 'c', [R12 ... R15] = 'c',
    },

    .reg_saves[REG_CLASS_XMM] = (char[16]){
        [XMM0 ... XMM5]  = 'C',
        [XMM6 ... XMM15] = 'c',
    },

    .param_count = { [REG_CLASS_GPR] = 4, [REG_CLASS_XMM] = 4 },
    .params[REG_CLASS_GPR] = (uint8_t[]){ RCX,  RDX,  R8,   R9 },
    .params[REG_CLASS_XMM] = (uint8_t[]){ XMM0, XMM1, XMM2, XMM3 },

    .ret_count = { [REG_CLASS_GPR] = 2, [REG_CLASS_XMM] = 2 },
    .rets[REG_CLASS_GPR] = { RAX,  RDX  },
    .rets[REG_CLASS_XMM] = { XMM0, XMM1 },
};

static CallingConv CC_SYSV = {
    .sp_class  = REG_CLASS_GPR, .sp_reg  = RSP, // stack pointer
    .fp_class  = REG_CLASS_GPR, .fp_reg  = RBP, // frame pointer
    .rpc_class = REG_CLASS_STK, .rpc_reg = 0,   // return PC
    .flexible_param_alloc = true,

    .reg_saves[REG_CLASS_GPR] = (char[16]){
        // volatiles (caller saves)
        [RAX] = 'C', [RCX] = 'C', [RDX] = 'C', [RDI] = 'C', [RSI] = 'C', [R8] = 'C', [R9] = 'C', [R10] = 'C', [R11] = 'C',
        // non-volatile (callee saves)
        [RBX] = 'c', [RBP] = 'c', [R12 ... R15] = 'c',
    },

    .reg_saves[REG_CLASS_XMM] = (char[16]){
        [XMM0 ... XMM4]  = 'C',
        [XMM5 ... XMM15] = 'c',
    },

    .param_count = { [REG_CLASS_GPR] = 6, [REG_CLASS_XMM] = 4 },
    .params[REG_CLASS_GPR] = (uint8_t[]){ RDI,  RSI,  RDX,  RCX, R8, R9 },
    .params[REG_CLASS_XMM] = (uint8_t[]){ XMM0, XMM1, XMM2, XMM3 },

    .ret_count = { [REG_CLASS_GPR] = 2, [REG_CLASS_XMM] = 2 },
    .rets[REG_CLASS_GPR] = { RAX,  RDX  },
    .rets[REG_CLASS_XMM] = { XMM0, XMM1 },
};

static CallingConv CC_SYSCALL = {
    .reg_saves[REG_CLASS_GPR] = (char[16]){
        // volatiles (caller saves)
        [RAX] = 'C', [RCX] = 'C', [RDX] = 'C', [RDI] = 'C', [RSI] = 'C', [R8] = 'C', [R9] = 'C', [R10] = 'C', [R11] = 'C',
        // non-volatile (callee saves)
        [RBX] = 'c', [RBP] = 'c', [R12 ... R15] = 'c',
    },

    .reg_saves[REG_CLASS_XMM] = (char[16]){
        [XMM0 ... XMM4]  = 'C',
        [XMM5 ... XMM15] = 'c',
    },

    .param_count = { [REG_CLASS_GPR] = 6 },
    .params[REG_CLASS_GPR] = (uint8_t[]){ RDI, RSI, RDX, R10, R8, R9 },

    .ret_count = { [REG_CLASS_GPR] = 2 },
    .rets[REG_CLASS_GPR] = { RAX, RDX },
};

static TB_X86_DataType legalize_int(TB_DataType dt) {
    switch (dt.type) {
        case TB_TAG_BOOL: return TB_X86_BYTE;
        case TB_TAG_I8:   return TB_X86_BYTE;
        case TB_TAG_I16:  return TB_X86_WORD;
        case TB_TAG_I32:  return TB_X86_DWORD;
        case TB_TAG_I64:  return TB_X86_QWORD;
        case TB_TAG_PTR:  return TB_X86_QWORD;
        default: tb_todo();
    }
}

static TB_X86_DataType legalize_float(TB_DataType dt) {
    if (dt.type == TB_TAG_V128) {
        switch (dt.elem_or_addrspace) {
            case TB_TAG_I32: return TB_X86_PDWORD;
            case TB_TAG_F32: return TB_X86_F32x4;
            case TB_TAG_F64: return TB_X86_F64x2;
            default: tb_todo();
        }
    }

    TB_ASSERT(dt.type == TB_TAG_F32 || dt.type == TB_TAG_F64);
    return (dt.type == TB_TAG_F64 ? TB_X86_F64x1 : TB_X86_F32x1);
}

static TB_X86_DataType legalize(TB_DataType dt) {
    if (dt.type == TB_TAG_F32) {
        return TB_X86_F32x1;
    } else if (dt.type == TB_TAG_F64) {
        return TB_X86_F64x1;
    } else if (dt.type == TB_TAG_V128) {
        return TB_X86_F32x4;
    } else {
        return legalize_int(dt);
    }
}

static bool try_for_imm32_2(TB_DataType dt, uint64_t src, int32_t* out_x) {
    if (dt.type == TB_TAG_I64 || dt.type == TB_TAG_PTR) {
        bool sign = (src >> 31ull) & 1;
        uint64_t top = src >> 32ull;

        // if the sign matches the rest of the top bits, we can sign extend just fine
        if (top != (sign ? 0xFFFFFFFF : 0)) {
            return false;
        }
    }

    *out_x = src;
    return true;
}

static bool try_for_imm32(TB_DataType dt, TB_Node* n, int32_t* out_x) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (dt.type == TB_TAG_I64 || dt.type == TB_TAG_PTR) {
        bool sign = (i->value >> 31ull) & 1;
        uint64_t top = i->value >> 32ull;

        // if the sign matches the rest of the top bits, we can sign extend just fine
        if (top != (sign ? 0xFFFFFFFF : 0)) {
            return false;
        }
    }

    *out_x = i->value;
    return true;
}

// we do 0 instead of -1 because when we want it to
// alias with it's inputs as if there's a move before
// the op.
static int node_2addr(TB_Node* n) {
    switch (n->type) {
        case x86_add: case x86_or: case x86_and: case x86_sub: case x86_xor:
        case x86_vadd: case x86_vsub: case x86_vmul: case x86_vdiv: case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            if (op->mode == MODE_ST) return -1;
            if (op->flags & OP_IMMEDIATE) return 2;
            return n->input_count - 1;
            // return op->flags & OP_INDEXED ? 4 : 3;
        }

        case x86_cmovcc: case x86_adc:
        return 2;

        case x86_cmp: case x86_test:
        return 0;

        case x86_shl: case x86_shr: case x86_sar:
        case x86_rol: case x86_ror: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_ASSERT(op->mode == MODE_REG);
            return n->input_count - 1;
        }

        case TB_MACH_COPY:
        case TB_FLOAT_EXT:
        return 1;

        default:
        return -1;
    }
}

static bool node_remat(TB_Node* n) {
    return n->type == x86_lea || n->type == x86_cmp || n->type == x86_test || n->type == x86_ucomi || n->type == x86_bt;
}

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->calling_conv = abi == TB_ABI_SYSTEMV ? &CC_SYSV : &CC_WIN64;

    // currently only using 16 GPRs and 16 XMMs, AVX gives us
    // 32 YMMs (which double as XMMs) and later on APX will do
    // 32 GPRs.
    ctx->num_regs[REG_CLASS_GPR] = 16;
    ctx->num_regs[REG_CLASS_XMM] = 16;
    ctx->num_regs[REG_CLASS_FLAGS] = 1;

    uint16_t all_gprs = 0xFFFF & ~(1 << RSP);
    if (ctx->f->features.gen & TB_FEATURE_FRAME_PTR) {
        all_gprs &= ~(1 << RBP);
        ctx->stack_header = 16;
    } else {
        ctx->stack_header = 8;
    }

    ctx->all_mask[REG_CLASS_GPR]      = new_regmask(ctx->f, REG_CLASS_GPR, false, 0xFFFF);
    ctx->all_mask[REG_CLASS_XMM]      = new_regmask(ctx->f, REG_CLASS_XMM, false, 0xFFFF);
    ctx->all_mask[REG_CLASS_FLAGS]    = new_regmask(ctx->f, REG_CLASS_FLAGS, false, 1);

    ctx->normie_mask[REG_CLASS_GPR]   = new_regmask(ctx->f, REG_CLASS_GPR, false, all_gprs);
    ctx->normie_mask[REG_CLASS_XMM]   = new_regmask(ctx->f, REG_CLASS_XMM, false, 0xFFFF);
    ctx->normie_mask[REG_CLASS_FLAGS] = new_regmask(ctx->f, REG_CLASS_FLAGS, false, 1);

    ctx->mayspill_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, true, all_gprs);
    ctx->mayspill_mask[REG_CLASS_XMM] = new_regmask(ctx->f, REG_CLASS_XMM, true, 0xFFFF);
    ctx->mayspill_mask[REG_CLASS_FLAGS] = NULL;

    TB_FunctionPrototype* proto = ctx->f->prototype;
    TB_Node** params = ctx->f->params;
    TB_Node* root_ctrl = params[0];

    ctx->param_count = ctx->f->param_count;

    // walk the entry to find any parameter stack slots
    FOR_N(i, 0, ctx->f->param_count) {
        TB_Node* proj = params[3 + i];
        if (proj->user_count != 1 || USERI(proj->users) == 0) { continue; }
        TB_Node* store_op = USERN(proj->users);
        if (store_op->type != TB_STORE) { continue; }
        TB_Node* addr = store_op->inputs[2];
        if (addr->type != TB_LOCAL) { continue; }

        TB_NodeLocal* local = TB_NODE_GET_EXTRA(addr);
        local->stack_pos = -(8 + i*8);

        if (i >= 4 && ctx->calling_conv == &CC_WIN64) {
            // get rid of the store since it's already at that location
            TB_Node* prev = store_op->inputs[1];
            set_input(ctx->f, store_op, NULL, 1);
            subsume_node(ctx->f, store_op, prev);
        }
    }

    // allocate all non-param locals
    TB_Node* root = ctx->f->root_node;
    FOR_USERS(u, root) {
        TB_Node* n = USERN(u);
        if (n->type != TB_LOCAL) { continue; }
        TB_NodeLocal* local = TB_NODE_GET_EXTRA(n);
        if (local->stack_pos == 0) {
            // each stack slot is 8bytes
            local->stack_pos = ctx->num_spills*8;
            ctx->num_spills = align_up(ctx->num_spills + (local->size+7)/8, (local->align+7)/8);
        }

        if (local->type) {
            assert(local->name);
            TB_StackSlot s = {
                .name = local->name,
                .type = local->type,
                .storage = { local->stack_pos },
            };
            dyn_array_put(ctx->debug_stack_slots, s);
        }
    }

    ctx->num_regs[REG_CLASS_STK] = ctx->param_count + ctx->num_spills + 1;
}

static RegMask* normie_mask(Ctx* restrict ctx, TB_DataType dt) {
    return ctx->normie_mask[TB_IS_FLOAT_TYPE(dt) ? REG_CLASS_XMM : REG_CLASS_GPR];
}

// not TLS
static bool simple_symbol(TB_Node* n) {
    if (n->type != TB_SYMBOL) return false;

    TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
    if (sym->tag != TB_SYMBOL_GLOBAL) return true;

    TB_Global* g = (TB_Global*) sym;
    return (sym->module->sections[g->parent].flags & TB_MODULE_SECTION_TLS) == 0;
}

static bool is_tls_symbol(TB_Symbol* sym) {
    if (sym->tag == TB_SYMBOL_GLOBAL) {
        TB_Global* g = (TB_Global*) sym;
        return sym->module->sections[g->parent].flags & TB_MODULE_SECTION_TLS;
    } else {
        return false;
    }
}

static TB_Node* mach_symbol(Ctx* restrict ctx, TB_Function* f, TB_Symbol* s) {
    TB_Node* n = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_PTR, 1, sizeof(TB_NodeMachSymbol));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeMachSymbol, .sym = s);

    TB_Node* k = tb__gvn(f, n, sizeof(TB_NodeMachSymbol));
    worklist_test_n_set(ctx->walker_ws, k);
    return k;
}

static void node_add_tmps(Ctx* restrict ctx, TB_Node* n) {
    TB_Function* f = ctx->f;
    if (n->type >= x86_add && n->type <= x86_ror) {
        // integer ops all produce the FLAGS
        TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, TB_TYPE_I64, 1, sizeof(TB_NodeMachProj));
        set_input(f, proj, n, 0);
        TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = 0, .def = ctx->normie_mask[REG_CLASS_FLAGS]);
    }

    if (n->type == TB_PROJ && (n->inputs[0]->type == x86_idiv || n->inputs[0]->type == x86_div)) {
        TB_Node* tuple = n->inputs[0];
        int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;

        node_add_tmp(ctx, tuple, intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX));

        // add the remaining projection
        TB_Node* proj = tb_alloc_node(f, TB_PROJ, n->dt, 1, sizeof(TB_NodeMachProj));
        set_input(f, proj, tuple, 0);
        TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = !index);

        proj = tb_opt_gvn_node(f, proj);
    }
}

// which regs are clobbered by this node
static int node_constraint_kill(Ctx* restrict ctx, TB_Node* n, RegMask** kills) {
    TB_Function* f = ctx->f;

    int kill_count = 0;
    if (n->type == x86_call) {
        CallingConv* cc = ctx->calling_conv;

        // we need to know which regs we've used, since those won't be tmps
        RegMask** ins = tb_arena_alloc(&f->tmp_arena, n->input_count * sizeof(RegMask*));
        node_constraint(ctx, n, ins);

        X86MemOp* op = TB_NODE_GET_EXTRA(n);
        size_t base = op->flags & OP_INDEXED ? 4 : 3;
        uint64_t used[8] = { 0 };
        FOR_N(i, base, n->input_count) if (ins[i] != &TB_REG_EMPTY) {
            TB_ASSERT(ins[i]->count == 1);
            used[ins[i]->class] |= ins[i]->mask[0];
        }

        bool use_frame_ptr = ctx->f->features.gen & TB_FEATURE_FRAME_PTR;
        FOR_N(i, 1, ctx->num_classes) {
            const char* saves = cc->reg_saves[i];
            if (saves == NULL) { continue; }

            uint64_t clobbers = 0;
            FOR_N(j, 0, ctx->num_regs[i]) {
                if (saves[j] == 'C' &&
                    // if we're using the frame ptr, it should be treated as "no save"
                    (!use_frame_ptr || cc->fp_class != i || cc->fp_reg != j)
                ) {
                    clobbers |= 1ull << j;
                }
            }

            kills[kill_count++] = intern_regmask(ctx, i, false, clobbers & ~used[i]);
        }
        tb_arena_free(&f->tmp_arena, ins, n->input_count * sizeof(RegMask*));

        // Clobber FLAGS
        kills[kill_count++] = ctx->normie_mask[REG_CLASS_FLAGS];
        return kill_count;
    } else {
        return 0;
    }
}

static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins) {
    switch (n->type) {
        case TB_DEAD:
        case TB_REGION:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_TRAP:
        case TB_DEBUGBREAK:
        case TB_UNREACHABLE:
        case TB_AFFINE_LOOP:
        case TB_NATURAL_LOOP:
        case TB_CALLGRAPH:
        case TB_BLACKHOLE:
        case TB_DEBUG_LOCATION:
        if (ins) {
            FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
        }
        return &TB_REG_EMPTY;

        case TB_POISON: {
            if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) {
                return ctx->normie_mask[REG_CLASS_XMM];
            }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_LOCAL:
        case TB_BRANCH_PROJ:
        case TB_MACH_SYMBOL:
        return &TB_REG_EMPTY;

        case TB_MACH_FRAME_PTR:
        return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RSP);

        case TB_MACH_JIT_THREAD_PTR:
        return ctx->normie_mask[REG_CLASS_GPR];

        case TB_MACH_COPY: {
            TB_NodeMachCopy* move = TB_NODE_GET_EXTRA(n);
            if (ins) {
                ins[1] = move->use;
            }
            return move->def;
        }

        case TB_MACH_TEMP: {
            return TB_NODE_GET_EXTRA_T(n, TB_NodeMachTemp)->def;
        }

        case TB_MACH_PROJ: {
            return TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->def;
        }

        case TB_HARD_BARRIER:
        case TB_SAFEPOINT: {
            if (ins) {
                TB_NodeSafepoint* sp = TB_NODE_GET_EXTRA(n);
                ins[1] = &TB_REG_EMPTY;
                FOR_N(i, 2, n->input_count) {
                    TB_Node* in = n->inputs[i];
                    if (in->dt.type == TB_TAG_MEMORY) {
                        ins[i] = &TB_REG_EMPTY;
                    } else {
                        ins[i] = ctx->normie_mask[TB_IS_VECTOR_TYPE(in->dt) || TB_IS_FLOAT_TYPE(in->dt) ? REG_CLASS_XMM : REG_CLASS_GPR];
                    }
                }
            }
            return &TB_REG_EMPTY;
        }

        case TB_PHI: {
            if (n->dt.type == TB_TAG_MEMORY) {
                if (ins) {
                    FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
                }
                return &TB_REG_EMPTY;
            }

            RegMask* rm = TB_IS_VECTOR_TYPE(n->dt) || TB_IS_FLOAT_TYPE(n->dt) ? ctx->normie_mask[REG_CLASS_XMM] : ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                FOR_N(i, 1, n->input_count) { ins[i] = rm; }
            }
            return rm;
        }

        case TB_ICONST:
        return ctx->normie_mask[REG_CLASS_GPR];

        case x86_vzero:
        return ctx->normie_mask[REG_CLASS_XMM];

        case TB_VSHUFFLE:
        case TB_VBROADCAST:
        if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_XMM]; }
        return ctx->normie_mask[REG_CLASS_XMM];

        case TB_PROJ: {
            if (n->dt.type == TB_TAG_MEMORY || n->dt.type == TB_TAG_CONTROL) {
                return &TB_REG_EMPTY;
            }

            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == x86_call) {
                CallingConv* cc = ctx->calling_conv;
                int reg_class = TB_IS_VECTOR_TYPE(n->dt) || TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_XMM : REG_CLASS_GPR;

                TB_ASSERT(i >= 2 && i < 2 + cc->ret_count[reg_class]);
                return intern_regmask(ctx, reg_class, false, 1u << cc->rets[reg_class][i - 2]);
            } else if (n->inputs[0]->type == x86_idiv || n->inputs[0]->type == x86_div) {
                return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (i ? RDX : RAX));
            }

            tb_todo();
            return &TB_REG_EMPTY;
        }

        case x86_add: case x86_or:  case x86_and: case x86_sub:
        case x86_xor: case x86_mov: case x86_imul: case x86_lea:
        case x86_cmp: case x86_test: case x86_idiv: case x86_div:
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_cmovcc: case x86_bt: case x86_adc:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            if (ins) {
                ins[1] = &TB_REG_EMPTY;

                RegMask* rm = ctx->all_mask[REG_CLASS_GPR];
                FOR_N(i, 2, n->input_count) {
                    ins[i] = n->inputs[i] ? rm : &TB_REG_EMPTY;
                }

                if (n->inputs[2] && n->inputs[2]->type == TB_MACH_SYMBOL) {
                    ins[2] = &TB_REG_EMPTY;
                } else if (op->mode == MODE_REG && (op->flags & OP_INDEXED) == 0) {
                    // the memory operand can be a spill slot
                    // if we're not one already
                    ins[2] = ctx->mayspill_mask[REG_CLASS_GPR];
                }

                if (n->type == x86_cmovcc || n->type == x86_adc) {
                    ins[n->input_count - 1] = ctx->normie_mask[REG_CLASS_FLAGS];
                } else if (n->type == x86_idiv || n->type == x86_div) {
                    // LHS is in RAX
                    ins[n->input_count - 2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);

                    // high bits for LHS are in RDX
                    TB_ASSERT(n->inputs[n->input_count - 1]->type == TB_MACH_TEMP);
                    ins[n->input_count - 1] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
                }
            }

            if (op->mode == MODE_ST || n->type == x86_idiv || n->type == x86_div) {
                return &TB_REG_EMPTY;
            } else if (n->type == x86_cmp || n->type == x86_test || n->type == x86_bt) {
                return ctx->normie_mask[REG_CLASS_FLAGS];
            } else {
                return ctx->normie_mask[REG_CLASS_GPR];
            }
        }

        case x86_setcc:
        if (ins) {
            ins[1] = &TB_REG_EMPTY;
            ins[2] = ctx->normie_mask[REG_CLASS_FLAGS];
        }
        return ctx->normie_mask[REG_CLASS_GPR];

        case x86_shl: case x86_shr: case x86_rol: case x86_ror: case x86_sar:
        {
            if (ins) {
                ins[1] = &TB_REG_EMPTY;

                RegMask* rm = ctx->all_mask[REG_CLASS_GPR];
                X86MemOp* op = TB_NODE_GET_EXTRA(n);
                if ((op->flags & OP_IMMEDIATE) == 0) {
                    TB_ASSERT(op->mode == MODE_REG);
                    ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);

                    FOR_N(i, 3, n->input_count) {
                        ins[i] = rm;
                    }
                } else {
                    FOR_N(i, 2, n->input_count) {
                        ins[i] = rm;
                    }
                }
            }
            return ctx->all_mask[REG_CLASS_GPR];
        }

        case x86_vmov: case x86_vadd: case x86_vsub: case x86_vmul: case x86_vdiv:
        case x86_ucomi: case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            RegMask* xmm = ctx->normie_mask[REG_CLASS_XMM];
            if (ins) {
                ins[1] = &TB_REG_EMPTY;

                if (op->mode == MODE_LD || op->mode == MODE_ST) {
                    if (n->inputs[2]->type == TB_MACH_SYMBOL) {
                        ins[2] = &TB_REG_EMPTY;
                    } else {
                        // base & index are still GPRs
                        ins[2] = ctx->all_mask[REG_CLASS_GPR];
                        if (op->flags & OP_INDEXED) {
                            ins[3] = ctx->normie_mask[REG_CLASS_GPR];
                        }
                    }
                } else {
                    ins[2] = ctx->mayspill_mask[REG_CLASS_XMM];
                }

                if (!(n->type == x86_vmov && op->mode == MODE_LD)) {
                    ins[n->input_count - 1] = xmm;
                }
            }

            if (op->mode == MODE_ST) {
                return &TB_REG_EMPTY;
            } else if (n->type == x86_ucomi) {
                return ctx->normie_mask[REG_CLASS_FLAGS];
            } else {
                return ctx->normie_mask[REG_CLASS_XMM];
            }
        }

        case x86_jcc:
        {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_FLAGS];
            }
            return &TB_REG_EMPTY;
        }

        case TB_MEMSET:
        {
            if (ins) {
                ins[1] = &TB_REG_EMPTY;
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDI);
                ins[3] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);
                ins[4] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);
            }
            return &TB_REG_EMPTY;
        }

        case TB_MEMCPY:
        {
            if (ins) {
                ins[1] = &TB_REG_EMPTY;
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDI);
                ins[3] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RSI);
                ins[4] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);
            }
            return &TB_REG_EMPTY;
        }

        case TB_NEVER_BRANCH:
        return &TB_REG_EMPTY;

        case TB_RETURN:
        {
            if (ins) {
                static int ret_gprs[2] = { RAX, RDX };
                CallingConv* cc = ctx->calling_conv;

                ins[1] = &TB_REG_EMPTY; // memory
                ins[2] = intern_regmask2(ctx, cc->rpc_class, false, cc->rpc_reg);

                TB_FunctionPrototype* proto = ctx->f->prototype;
                FOR_N(i, 3, 3 + proto->return_count) {
                    TB_Node* in = n->inputs[i];
                    TB_DataType dt = in->dt;
                    int reg_class = TB_IS_VECTOR_TYPE(n->dt) || TB_IS_FLOAT_TYPE(dt) ? REG_CLASS_XMM : REG_CLASS_GPR;

                    TB_ASSERT(i >= cc->ret_count[reg_class]);
                    ins[i] = intern_regmask2(ctx, reg_class, false, cc->rets[reg_class][i-3]);
                }

                size_t k = 3 + proto->return_count;
                ins[k++] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RSP);

                bool use_frame_ptr = ctx->f->features.gen & TB_FEATURE_FRAME_PTR;
                FOR_N(i, 1, ctx->num_classes) {
                    const char* saves = cc->reg_saves[i];
                    if (saves == NULL) { continue; }

                    FOR_N(j, 0, ctx->num_regs[i]) {
                        if (saves[j] == 'c' &&
                            // if we're using the frame ptr, it should be treated as "no save"
                            (!use_frame_ptr || cc->fp_class != i || cc->fp_reg != j)
                        ) {
                            ins[k++] = intern_regmask(ctx, i, false, 1ull << j);
                        }
                    }
                }
            }
            return &TB_REG_EMPTY;
        }

        case x86_call:
        {
            if (ins) {
                CallingConv* cc = ctx->calling_conv;

                X86MemOp* op = TB_NODE_GET_EXTRA(n);
                ins[1] = &TB_REG_EMPTY;

                size_t base = 3;
                if (op->mode == MODE_REG) {
                    TB_ASSERT(n->inputs[2]->type == TB_MACH_SYMBOL);
                    ins[2] = &TB_REG_EMPTY;
                } else if (op->flags & OP_INDEXED) {
                    ins[2] = ctx->normie_mask[REG_CLASS_GPR];
                    ins[3] = ctx->normie_mask[REG_CLASS_GPR];
                    base += 1;
                } else {
                    ins[2] = ctx->normie_mask[REG_CLASS_GPR];
                }

                int used[8];
                FOR_N(i, 0, ctx->num_classes) {
                    used[i] = 0;
                }
                used[REG_CLASS_STK] = 1 + ctx->param_count;

                int param_count = 0;
                for (size_t i = base; i < n->input_count; i++) {
                    TB_Node* in = n->inputs[i];
                    if (in->type == TB_MACH_TEMP) {
                        ins[i] = TB_NODE_GET_EXTRA_T(in, TB_NodeMachTemp)->def;
                        continue;
                    }

                    // on win64 we always have the XMMs and GPRs used match the param_num
                    // so if XMM2 is used, it's always the 3rd parameter.
                    int j = i - base;
                    if (cc == &CC_WIN64) {
                        used[REG_CLASS_GPR] = used[REG_CLASS_XMM] = j;
                        used[REG_CLASS_STK] = 1 + ctx->param_count + j;
                    }

                    int reg_class = TB_IS_VECTOR_TYPE(in->dt) || TB_IS_FLOAT_TYPE(in->dt) ? REG_CLASS_XMM : REG_CLASS_GPR;
                    int reg_num = used[reg_class];

                    if (reg_num >= cc->param_count[reg_class]) {
                        ins[i] = intern_regmask2(ctx, REG_CLASS_STK, false, used[REG_CLASS_STK]);
                        used[REG_CLASS_STK] += 1;
                    } else {
                        ins[i] = intern_regmask2(ctx, reg_class, false, cc->params[reg_class][reg_num]);
                        used[reg_class] += 1;
                    }
                    param_count++;
                }

                if (param_count > ctx->call_usage) {
                    ctx->call_usage = param_count;
                    if (cc == &CC_WIN64 && ctx->call_usage > 0 && ctx->call_usage < 3) {
                        ctx->call_usage = 4;
                    }

                    ctx->num_regs[REG_CLASS_STK] = ctx->param_count + ctx->call_usage + ctx->num_spills + 1;
                }
            }

            // the tuple node doesn't itself produce the result
            return &TB_REG_EMPTY;
        }

        default:
        tb_todo();
        return &TB_REG_EMPTY;
    }
}

static int op_gpr_at(Ctx* ctx, TB_Node* n) { return op_reg_at(ctx, n, REG_CLASS_GPR); }
static int op_xmm_at(Ctx* ctx, TB_Node* n) { return op_reg_at(ctx, n, REG_CLASS_XMM); }

// stack_usage is how much we subtract when entering the function:
//
//   [rsp + stack_usage + stack_header + i*8] CALLER PARAM
//   [rsp + stack_usage + stack_header-8]     RPC
//   [rsp + stack_usage]                      RBP (optional)
//   [rsp + i*8]                              SPILLS/LOCALS
//   [rsp + i*8]                              CALLEE PARAM
//
static int stk_offset(Ctx* ctx, int reg, int width) {
    if (reg == 0) {
        // return address
        return ctx->stack_usage + (ctx->stack_header - 8);
    } else if (reg < ctx->param_count + 1) {
        // argument slots (reaching outside of our stack frame)
        return ctx->stack_usage + reg*8;
    } else if (reg < ctx->param_count + ctx->call_usage + 1) {
        // param passing slots
        return (reg - (1 + ctx->param_count))*8;
    } else {
        // normal spill slots
        int spill_num = reg - (1 + ctx->param_count + ctx->call_usage);
        return ctx->call_usage*8 + spill_num*8;
    }
}

static Val op_at(Ctx* ctx, TB_Node* n) {
    TB_ASSERT(ctx->vreg_map[n->gvn] > 0);
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    if (vreg->class == REG_CLASS_STK) {
        int width = 1;
        if (n->dt.type == TB_TAG_V128) {
            width = 2;
        }

        TB_ASSERT(vreg->assigned >= 0);
        return val_stack(stk_offset(ctx, vreg->assigned, width));
    } else {
        TB_ASSERT(vreg->assigned >= 0);
        return (Val) { .type = vreg->class == REG_CLASS_XMM ? VAL_XMM : VAL_GPR, .reg = vreg->assigned };
    }
}

static Val parse_cisc_operand(Ctx* restrict ctx, TB_Node* n, Val* rx, X86MemOp* op) {
    int j = 2;
    Val rm = { 0 };

    if (op->mode == MODE_LD || op->mode == MODE_ST) {
        rm.type  = VAL_MEM;
        rm.imm   = op->disp;
        rm.scale = op->scale;

        // parse base operand
        TB_Node* base = n->inputs[j++];
        if (base->type == TB_MACH_SYMBOL) {
            TB_ASSERT((op->flags & OP_INDEXED) == 0);
            rm.type = VAL_GLOBAL;
            rm.symbol = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeMachSymbol)->sym;
        } else if (base->type == TB_MACH_FRAME_PTR) {
            rm.reg = RSP;
            if (rm.imm < 0) {
                // "negative" stack offsets are actually talking about the top of the stack
                rm.imm = ctx->stack_usage + ctx->stack_header + (-rm.imm - 8);
            } else {
                rm.imm += ctx->call_usage*8;
            }
            // rm.imm += ctx->stack_usage - ctx->stack_header;
        } else {
            rm.reg = op_gpr_at(ctx, base);
        }

        if (op->flags & OP_INDEXED) {
            rm.index = op_gpr_at(ctx, n->inputs[j++]);
        } else {
            rm.index = -1;
        }
    } else {
        rm = op_at(ctx, n->inputs[j++]);
        TB_ASSERT(rm.type == VAL_GPR || rm.type == VAL_XMM || rm.type == VAL_MEM);
    }

    if (rx) {
        if (op->flags & OP_IMMEDIATE) {
            *rx = (Val){ VAL_IMM, .imm = op->imm };
        } else if (j < n->input_count && n->inputs[j]) {
            *rx = op_at(ctx, n->inputs[j++]);
        } else {
            *rx = (Val){ 0 };
        }
    }
    return rm;
}

static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle) {
    TB_ASSERT(bundle->count == 1);
    TB_Node* n = bundle->arr[0];
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];

    #if TB_OPTDEBUG_REGALLOC2
    if (e->has_comments) {
        enum { BUF_SIZE = 1024 };
        char buf[BUF_SIZE];
        int j = 0;

        j += snprintf(buf+j, BUF_SIZE-j, "%%%-3u: ", n->gvn);

        int dst = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;
        if (dst > 0) {
            j += snprintf(buf+j, BUF_SIZE-j, "V%-3d", dst);
        } else {
            j += snprintf(buf+j, BUF_SIZE-j, "____");
        }
        j += snprintf(buf+j, BUF_SIZE-j, " = %-14s (", tb_node_get_name(n->type));
        FOR_N(i, 0, n->input_count) {
            int src = n->inputs[i] && n->inputs[i]->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->inputs[i]->gvn] : 0;
            if (src > 0) {
                j += snprintf(buf+j, BUF_SIZE-j, " V%-3d", src);
            } else {
                j += snprintf(buf+j, BUF_SIZE-j, " ____");
            }
        }
        j += snprintf(buf+j, BUF_SIZE-j, " )");
        COMMENT("%.*s", j > 100 ? 100 : j, buf);
    }
    #endif

    switch (n->type) {
        // some ops don't do shit lmao
        case TB_PHI:
        case TB_DEAD:
        case TB_POISON:
        case TB_REGION:
        case TB_AFFINE_LOOP:
        case TB_NATURAL_LOOP:
        case TB_PROJ:
        case TB_BRANCH_PROJ:
        case TB_MACH_PROJ:
        case TB_LOCAL:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_MACH_TEMP:
        case TB_CALLGRAPH:
        case TB_MACH_SYMBOL:
        case TB_MACH_FRAME_PTR:
        case TB_HARD_BARRIER:
        case TB_UNREACHABLE:
        case TB_BLACKHOLE:
        break;

        case TB_SAFEPOINT:
        COMMENT("safepoint");
        EMIT1(e, 0x90);
        // TB_OPTDEBUG(REGALLOC2)(EMIT1(e, 0x90));
        break;

        case TB_MACH_JIT_THREAD_PTR: {
            GPR dst = op_gpr_at(ctx, n);
            __(MOV, TB_X86_QWORD, Vgpr(dst), Vgpr(RSP));
            __(AND, TB_X86_QWORD, Vgpr(dst), Vimm(-0x200000));
            break;
        }

        case TB_NEVER_BRANCH: {
            TB_Node* proj0 = USERN(proj_with_index(n, 0));
            TB_Node* succ_n = cfg_next_bb_after_cproj(proj0);
            TB_BasicBlock* succ = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
            __(JMP, TB_X86_QWORD, Vlbl(succ->fwd));
            break;
        }

        case TB_ICONST: {
            uint64_t x = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;
            uint32_t hi = x >> 32ull;

            TB_X86_DataType dt = legalize_int(n->dt);
            GPR dst = op_gpr_at(ctx, n);
            if (x == 0) {
                __(XOR, TB_X86_DWORD, Vgpr(dst), Vgpr(dst));
            } else if (hi == 0 || dt == TB_X86_QWORD) {
                // "movabs"
                EMIT1(e, rex(dt == TB_X86_QWORD, 0, dst, 0));
                EMIT1(e, 0xB8 + (dst & 0b111));
                if (dt != TB_X86_QWORD) {
                    EMIT4(e, x);
                } else {
                    EMIT8(e, x);
                }
            } else {
                __(MOV, dt, Vgpr(dst), Vimm(x));
            }
            break;
        }

        case TB_BSWAP: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);

            TB_X86_DataType dt = legalize(n->dt);
            if (!is_value_match(&dst, &src)) {
                __(MOV, dt, &dst, &src);
            }

            if (dt == TB_X86_WORD) {
                EMIT1(e, 0x66);
            }

            if (dt == TB_X86_QWORD || dst.reg >= 8) {
                uint8_t rex = 0x40 | (dt == TB_X86_QWORD ? 8 : 0) | (dst.reg >= 8 ? 1 : 0);
                EMIT1(e, rex);
            }

            EMIT1(e, 0x0F);
            EMIT1(e, 0xC8 + (dst.reg & 0x7));
            break;
        }

        case TB_MACH_COPY: {
            TB_X86_DataType dt = legalize(n->dt);

            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            if (!is_value_match(&dst, &src)) {
                #if !TB_OPTDEBUG_REGALLOC2
                if (dst.type == VAL_MEM && src.type != VAL_MEM) {
                    COMMENT("spill");
                } else if (dst.type != VAL_MEM && src.type == VAL_MEM) {
                    COMMENT("reload");
                }
                #endif

                if (dst.type == VAL_GPR && src.type == VAL_XMM) {
                    __(MOV_I2F, dt, &dst, &src);
                } else if (dst.type == VAL_XMM && src.type == VAL_GPR) {
                    TB_X86_DataType src_dt = legalize(n->inputs[1]->dt);
                    __(MOV_F2I, src_dt, &dst, &src);
                } else {
                    int op = dt < TB_X86_F32x1 ? MOV : FP_MOV;
                    __(op, dt, &dst, &src);
                }
            } else {
                TB_OPTDEBUG(REGALLOC2)(EMIT1(e, 0x90));
            }
            break;
        }

        case TB_CYCLE_COUNTER: {
            EMIT1(e, 0x0F); EMIT1(e, 0x31); // rdtsc
            __(SHL, TB_X86_QWORD, Vgpr(RDX), Vimm(32));
            __(OR,  TB_X86_QWORD, Vgpr(RAX), Vgpr(RDX));
            break;
        }

        // epilogue
        case TB_RETURN: {
            size_t pos = e->count;
            TB_FunctionPrototype* proto = ctx->f->prototype;

            int stack_usage = ctx->stack_usage;
            if (stack_usage > 8) {
                // add rsp, N
                if (stack_usage == (int8_t)stack_usage) {
                    EMIT1(e, rex(true, 0x00, RSP, 0));
                    EMIT1(e, 0x83);
                    EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x00, RSP));
                    EMIT1(e, (int8_t) stack_usage);
                } else {
                    EMIT1(e, rex(true, 0x00, RSP, 0));
                    EMIT1(e, 0x81);
                    EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x00, RSP));
                    EMIT4(e, stack_usage);
                }
            }

            // pop rbp (if we even used the frameptr)
            if ((ctx->f->features.gen & TB_FEATURE_FRAME_PTR) && stack_usage > 8) {
                EMIT1(e, 0x58 + RBP);
            }
            EMIT1(e, 0xC3);
            ctx->epilogue_length = e->count - pos;
            break;
        }

        #if 0
        case TB_FLOAT_EXT: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            TB_X86_DataType src_dt = legalize_float(n->inputs[1]->dt);
            __(FP_CVT, src_dt, &dst, &src);
            break;
        }

        case TB_FLOAT_TRUNC: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            __(FP_CVT, legalize_float(n->inputs[1]->dt), &dst, &src);
            break;
        }

        case x86_neg: {
            TB_X86_DataType dt = legalize_int(n->dt);
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            if (!is_value_match(&dst, &src)) {
                __(MOV, dt, &dst, &src);
            }
            __(NEG, dt, &dst);
            break;
        }

        case TB_UINT2FLOAT:
        case TB_INT2FLOAT: {
            TB_DataType src_dt = n->inputs[1]->dt;
            TB_ASSERT(TB_IS_INTEGER_TYPE(src_dt));

            // it's either 32bit or 64bit conversion
            //   CVTSI2SS r/m32, xmm1
            //   CVTSI2SD r/m64, xmm1
            bool is_64bit = src_dt.type == TB_TAG_I64;

            TB_X86_DataType dt = legalize_float(n->dt);
            Val dst = op_at(ctx, n);
            Val lhs = op_at(ctx, n->inputs[1]);
            __(is_64bit ? FP_CVT64 : FP_CVT32, dt, &dst, &lhs);

            // TODO(NeGate): that conversion from a 64bit unsigned number requires fixups we
            // don't do quite yet, go fiddle with godbolt later.
            break;
        }

        case TB_FLOAT2INT:
        case TB_FLOAT2UINT: {
            TB_X86_DataType dt = legalize_float(n->inputs[1]->dt);

            // it's either 32bit or 64bit conversion
            // F3 0F 2C /r            CVTTSS2SI xmm1, r/m32
            // F3 REX.W 0F 2C /r      CVTTSS2SI xmm1, r/m64
            // F2 0F 2C /r            CVTTSD2SI xmm1, r/m32
            // F2 REX.W 0F 2C /r      CVTTSD2SI xmm1, r/m64
            Val dst = op_at(ctx, n);
            Val lhs = op_at(ctx, n->inputs[1]);
            __(FP_CVTT, dt, &dst, &lhs);

            // TODO(NeGate): that conversion into a 64bit unsigned number requires fixups we
            // don't do quite yet, go fiddle with godbolt later.
            break;
        }
        #endif

        case x86_idiv: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            // cqo/cdq (sign extend RAX into RDX)
            if (n->dt.type == TB_TAG_I64) { EMIT1(e, 0x48); }
            EMIT1(e, 0x99);
            // idiv
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int(op->extra_dt);
            __(IDIV, dt, &rhs);
            break;
        }

        case x86_div: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            __(XOR, TB_X86_DWORD, Vgpr(RDX), Vgpr(RDX));
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int(op->extra_dt);
            __(DIV, dt, &rhs);
            break;
        }

        case x86_lea: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt = legalize_int(n->dt);

            Val rm  = parse_cisc_operand(ctx, n, NULL, op);
            Val dst = op_at(ctx, n);
            __(LEA, dt, &dst, &rm);
            break;
        }

        case x86_vmov:
        case x86_vadd:
        case x86_vsub:
        case x86_vmul:
        case x86_vdiv:
        case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt;
            if (n->dt.type == TB_TAG_MEMORY) {
                dt = legalize_float(op->extra_dt);
            } else {
                dt = legalize_float(n->dt);
            }

            int mov_op = FP_MOV;
            if (dt >= TB_X86_PBYTE && dt <= TB_X86_PQWORD) {
                mov_op = MOVDQU;
            }

            int op_type;
            switch (n->type) {
                case x86_vmov: op_type = mov_op; break;
                case x86_vadd: op_type = FP_ADD; break;
                case x86_vsub: op_type = FP_SUB; break;
                case x86_vmul: op_type = FP_MUL; break;
                case x86_vdiv: op_type = FP_DIV; break;
                case x86_vxor: op_type = FP_XOR; break;
                default: tb_todo();
            }

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            if (op->mode == MODE_ST) {
                __(op_type, dt, &rm, &rx);
            } else {
                Val dst = op_at(ctx, n);
                if (rx.type != VAL_NONE) {
                    TB_ASSERT(n->type != x86_lea);
                    if (!is_value_match(&dst, &rx)) {
                        __(mov_op, dt, &dst, &rx);
                    }
                }
                __(op_type, dt, &dst, &rm);
            }
            break;
        }

        case TB_VBROADCAST: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            TB_X86_DataType dt = legalize_float(n->dt);
            TB_ASSERT(dst.type == VAL_XMM);
            TB_ASSERT(src.type == VAL_XMM || src.type == VAL_MEM || src.type == VAL_GLOBAL);

            if (!is_value_match(&dst, &src)) {
                __(FP_MOV,  dt, &dst, &src);
            }
            __(FP_SHUF, dt, &dst, &src);
            EMIT1(e, 0);
            break;
        }

        case TB_VSHUFFLE: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            TB_X86_DataType dt = legalize_float(n->dt);
            TB_ASSERT(dst.type == VAL_XMM);
            TB_ASSERT(src.type == VAL_XMM || src.type == VAL_MEM || src.type == VAL_GLOBAL);

            uint8_t imm = 0;
            TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
            FOR_N(i, 0, shuf->width) {
                imm |= (shuf->indices[i] & 3) << (i*2);
            }

            if (!is_value_match(&dst, &src)) {
                __(FP_MOV,  dt, &dst, &src);
            }
            __(FP_SHUF, dt, &dst, &src);
            EMIT1(e, imm);
            break;
        }

        case x86_vzero: {
            Val dst = op_at(ctx, n);
            __(FP_XOR, TB_X86_F32x4, &dst, &dst); // xorps
            break;
        }

        case x86_setcc: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            Val dst = op_at(ctx, n);
            __(SETO+op->cond, TB_X86_BYTE, &dst);
            break;
        }

        case x86_add: case x86_or:  case x86_and:
        case x86_sub: case x86_xor: case x86_mov:
        case x86_shl: case x86_shr: case x86_rol:
        case x86_ror: case x86_sar: case x86_cmovcc:
        case x86_adc:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt;
            if (n->type == x86_test || n->type == x86_cmp || n->dt.type == TB_TAG_MEMORY) {
                dt = legalize_int(op->extra_dt);
            } else {
                dt = legalize_int(n->dt);
            }

            int op_type;
            switch (n->type) {
                case x86_mov: op_type = MOV; break;
                case x86_add: op_type = ADD; break;
                case x86_and: op_type = AND; break;
                case x86_sub: op_type = SUB; break;
                case x86_xor: op_type = XOR; break;
                case x86_or:  op_type = OR;  break;
                case x86_shl: op_type = SHL; break;
                case x86_shr: op_type = SHR; break;
                case x86_rol: op_type = ROL; break;
                case x86_ror: op_type = ROR; break;
                case x86_sar: op_type = SAR; break;
                case x86_cmovcc: op_type = CMOVO+op->cond; break;
                case x86_adc: op_type = ADC; break;
            }
            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            if (op->mode == MODE_ST) {
                __(op_type, dt, &rm, &rx);
            } else if (n->type == x86_cmovcc || (op->flags & OP_IMMEDIATE)) {
                Val dst = op_at(ctx, n);
                if (!is_value_match(&dst, &rm)) {
                    __(MOV, dt, &dst, &rm);
                }
                __(op_type, dt, &dst, &rx);
            } else {
                Val dst = op_at(ctx, n);
                if (rx.type != VAL_NONE) {
                    TB_ASSERT(n->type != x86_lea);
                    if (!is_value_match(&dst, &rx)) {
                        __(MOV, dt, &dst, &rx);
                    }
                }
                __(op_type, dt, &dst, &rm);
            }
            break;
        }

        case x86_movsx8:
        case x86_movzx8:
        case x86_movsx16:
        case x86_movzx16:
        case x86_movsx32: {
            static int ops[] = {
                MOVSXB, MOVZXB,
                MOVSXW, MOVZXW,
                MOVSXD,
            };

            TB_X86_DataType dt = legalize_int(n->inputs[2]->dt);
            if (n->type == x86_movzx8 || n->type == x86_movzx16) {
                dt = TB_X86_DWORD;
            } else if (n->type == x86_movsx32) {
                dt = TB_X86_QWORD;
            }

            Val dst = op_at(ctx, n);
            Val src = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));

            int op_type = ops[n->type - x86_movsx8];
            __(op_type, dt, &dst, &src);
            break;
        }

        case x86_cmp: case x86_test: case x86_ucomi: case x86_bt: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt = legalize(op->extra_dt);

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            int op_type = -1;
            switch (n->type) {
                case x86_cmp:   op_type = CMP;      break;
                case x86_test:  op_type = TEST;     break;
                case x86_ucomi: op_type = FP_UCOMI; break;
                case x86_bt:    op_type = BT;       break;
            }
            if (rx.type == VAL_IMM) {
                __(op_type, dt, &rm, &rx);
            } else {
                __(op_type, dt, &rx, &rm);
            }
            break;
        }

        case x86_jcc: {
            int succ[2] = { -1, -1 };
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_BRANCH_PROJ) {
                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                    TB_Node* succ_n = USERN(u);

                    TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
                    succ[index] = succ_bb->fwd;
                }
            }

            TB_NodeBranchProj* if_br = cfg_if_branch(n);
            TB_ASSERT(if_br != NULL);
            Cond cc = TB_NODE_GET_EXTRA_T(n, X86MemOp)->cond;
            if (ctx->fallthrough == succ[0]) {
                // if flipping avoids a jmp, do that
                cc ^= 1;
                SWAP(int, succ[0], succ[1]);
            }

            __(JO+cc, TB_X86_QWORD, Vlbl(succ[0]));
            if (ctx->fallthrough != succ[1]) {
                __(JMP, TB_X86_QWORD, Vlbl(succ[1]));
            }
            break;
        }

        case x86_call: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);

            // on SysV, AL stores the number of float params
            /*if (ctx->calling_conv == &CC_SYSV && op_extra->proto->has_varargs) {
                int float_params = 0;
                FOR_N(i, 3, n->input_count) {
                    if (n->inputs[i]->type == TB_MACH_TEMP) { break; }
                    if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) { float_params++; }
                }

                if (float_params == 0) {
                    __(XOR, TB_X86_DWORD, Vgpr(RAX), Vgpr(RAX));
                } else {
                    __(MOV, TB_X86_BYTE, Vgpr(RAX), Vimm(float_params));
                }
            }*/

            if (op->mode == MODE_REG) {
                TB_ASSERT(n->inputs[2]->type == TB_MACH_SYMBOL);
                TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeMachSymbol)->sym;

                // CALL rel32
                EMIT1(e, 0xE8);
                EMIT4(e, 0);
                tb_emit_symbol_patch(e->output, sym, e->count - 4);
            } else {
                Val target = parse_cisc_operand(ctx, n, NULL, op);
                __(CALL, TB_X86_QWORD, &target);
            }
            break;
        }

        case x86_imul: {
            TB_X86_DataType dt = legalize_int(n->dt);
            X86MemOp* op = TB_NODE_GET_EXTRA(n);

            Val dst = op_at(ctx, n);
            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            if (rx.type == VAL_IMM) {
                // hacky but the ternary multiply is just a 2op modrm like normal, except with an
                // extra immediate afterwards.
                __(IMUL3, dt, &dst, &rm);
                if (dt == TB_X86_WORD) { EMIT2(e, rx.imm); }
                else { EMIT4(e, rx.imm); }
            } else {
                if (!is_value_match(&dst, &rx)) {
                    __(MOV, dt, &dst, &rx);
                }
                __(IMUL, dt, &dst, &rm);
            }
            break;
        }

        case TB_MACH_JUMP: {
            TB_Node* succ_n = cfg_next_control(n);
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);

            EMIT1(e, 0xE9); EMIT4(e, 0);
            tb_emit_rel32(e, &e->labels[succ_bb->fwd], GET_CODE_POS(e) - 4, 0xFFFFFFFF, 0);
            break;
        }

        /*case x86_movsx8:
        case x86_movzx8:
        case x86_movsx16:
        case x86_movzx16:
        case x86_movsx32: {
            static int ops[] = {
                MOVSXB, MOVZXB,
                MOVSXW, MOVZXW,
                MOVSXD,
            };

            TB_X86_DataType dt = legalize_int(n->inputs[2]->dt);
            if (n->type == x86_movzx8 || n->type == x86_movzx16) {
                dt = TB_X86_DWORD;
            } else if (n->type == x86_movsx32) {
                dt = TB_X86_QWORD;
            }

            Val dst = op_at(ctx, n);
            Val src = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));

            int op_type = ops[n->type - x86_movsx8];
            __(op_type, dt, &dst, &src);
            break;
        }*/

        case TB_MEMSET: {
            EMIT1(e, 0xF3);
            EMIT1(e, 0xAA);
            break;
        }

        case TB_MEMCPY: {
            EMIT1(e, 0xF3);
            EMIT1(e, 0xA4);
            break;
        }

        case TB_TRAP: {
            EMIT1(e, 0x0F);
            EMIT1(e, 0x0B);
            break;
        }

        case TB_DEBUGBREAK: {
            EMIT1(e, 0xCC);
            break;
        }

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_PTROFF: {
            // tbl is for normal locking operations which don't care about the result,
            // fetch will need to worry about it which means slightly different codegen.
            const static int tbl[]       = { MOV,  ADD,  AND, XOR, OR, ADD  };
            const static int fetch_tbl[] = { XCHG, XADD, 0,   0,   0,  XADD };

            TB_Node* dproj = USERN(proj_with_index(n, 1));

            TB_NodeAtomic* a = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt = legalize_int(dproj->dt);

            Val addr = op_at(ctx, n->inputs[2]);
            Val src  = op_at(ctx, n->inputs[3]);
            int op = (dproj->users ? fetch_tbl : tbl)[n->type - TB_ATOMIC_XCHG];
            assert(op != 0); // unsupported op, we need to emulate it :(

            {
                assert(addr.type == VAL_GPR);
                addr.type = VAL_MEM;
                addr.index = -1;
            }

            if (dproj->users) {
                // this form needs to do exchanges
                Val dst = op_at(ctx, dproj);
                if (!is_value_match(&dst, &src)) {
                    __(MOV, dt, &dst, &src);
                }
                EMIT1(e, 0xF0);
                __(op, dt, &addr, &dst);
            } else {
                // this form can use normal ops with a LOCK
                EMIT1(e, 0xF0);
                __(op, dt, &addr, &src);
            }
            break;
        }

        case TB_SYSCALL: {
            __(SYSCALL, TB_X86_QWORD);
            break;
        }

        case TB_DEBUG_LOCATION: {
            TB_NodeDbgLoc* loc = TB_NODE_GET_EXTRA(n);
            TB_Location l = {
                .file = loc->file,
                .line = loc->line,
                .column = loc->column,
                .pos = e->count
            };
            dyn_array_put(ctx->locations, l);
            break;
        }

        default:
        tb_todo();
        break;
    }
}

static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b) {
    return false;
}

static uint64_t node_unit_mask(TB_Function* f, TB_Node* n) {
    if (n->type == x86_imul) {
        return 0b00000010;
    } else if (n->type == TB_VSHUFFLE) {
        return 0b00100000;
    }

    if (n->type >= TB_MACH_X86) {
        X86MemOp* op = TB_NODE_GET_EXTRA(n);

        if (op->mode == MODE_LD) {
            // if we're doing a load, we can only fit into ports 2 & 3
            return 0b00001100;
        } else if (op->mode == MODE_ST) {
            // stores go into port 4
            return 0b00010000;
        }

        // compares go into port 6
        return 0b00010000;
    }

    return 0b11101111;
}

static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) {
    if (n->type == x86_idiv || n->type == x86_div) {
        return 30;
    } else if (n->type == x86_imul) {
        return 3;
    } else if (n->type == x86_lea) {
        return 1;
    } else if (n->type == x86_test || n->type == x86_cmp) {
        return 1;
    }

    X86MemOp* op = TB_NODE_GET_EXTRA(n);
    int lat = 0;
    if (n->type == x86_vadd || n->type == x86_vmul) {
        lat = 4;
    } else if (n->type != x86_vmov && n->type != x86_mov) {
        lat = 1;
    }

    if (op->mode == MODE_LD) {
        // L1 hit is 4 cycles
        lat += 4;
    } else if (op->mode == MODE_ST) {
        lat += 1;
    }

    return lat;
}

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* root) {
    TB_FunctionPrototype* proto = ctx->f->prototype;

    size_t stack_usage = ctx->stack_header;
    if (ctx->num_spills != 0 || ctx->call_usage != 0) {
        stack_usage = (ctx->num_spills+ctx->call_usage) * 8;

        // Align stack usage to 16bytes + header to accommodate for the RIP being pushed
        // by CALL (and frameptr if applies)
        stack_usage = align_up(stack_usage, 16) + (16 - ctx->stack_header);
    }
    ctx->stack_usage = stack_usage;

    // save frame pointer (if applies)
    if ((ctx->f->features.gen & TB_FEATURE_FRAME_PTR) && stack_usage > 0) {
        EMIT1(e, 0x50 + RBP);

        // mov rbp, rsp
        EMIT1(e, rex(true, RSP, RBP, 0));
        EMIT1(e, 0x89);
        EMIT1(e, mod_rx_rm(MOD_DIRECT, RSP, RBP));
    }

    // inserts a chkstk call if we use too much stack
    if (ctx->calling_conv == &CC_WIN64 && ctx->f->super.module->chkstk_extern && stack_usage >= 4096) {
        TB_ASSERT(ctx->f->super.module->chkstk_extern);
        ctx->f->super.module->uses_chkstk++;

        Val sym = val_global(ctx->f->super.module->chkstk_extern, 0);
        Val rax = val_gpr(RAX);
        Val rsp = val_gpr(RSP);

        __(MOV,  TB_X86_DWORD, &rax, Vimm(stack_usage));
        __(CALL, TB_X86_QWORD, &sym);
        __(SUB,  TB_X86_QWORD, &rsp, &rax);
    } else if (stack_usage > 8) {
        if (stack_usage == (int8_t)stack_usage) {
            // sub rsp, stack_usage
            EMIT1(e, rex(true, 0x00, RSP, 0));
            EMIT1(e, 0x83);
            EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x05, RSP));
            EMIT1(e, stack_usage);
        } else {
            // sub rsp, stack_usage
            EMIT1(e, rex(true, 0x00, RSP, 0));
            EMIT1(e, 0x81);
            EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x05, RSP));
            EMIT4(e, stack_usage);
        }
    }

    // handle unknown parameters (if we have varargs)
    CallingConv* cc = ctx->calling_conv;
    if (ctx->calling_conv == &CC_WIN64 && proto->has_varargs) {
        // spill the rest of the parameters (they're all in the GPRs)
        size_t gpr_count = CC_WIN64.param_count[REG_CLASS_GPR];
        FOR_N(i, proto->param_count, gpr_count) {
            int dst_pos = i * 8;
            int param_gpr = CC_WIN64.params[REG_CLASS_GPR][i];

            __(MOV, TB_X86_QWORD, Vbase(RSP, stack_usage + ctx->stack_header + dst_pos), Vgpr(param_gpr));
        }
    }

    ctx->prologue_length = e->count;
}

static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb) {
    tb_resolve_rel32(e, &e->labels[bb], e->count, 0xFFFFFFFF, 0);
}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {
    // pad to 16bytes
    static const uint8_t nops[8][8] = {
        { 0x90 },
        { 0x66, 0x90 },
        { 0x0F, 0x1F, 0x00 },
        { 0x0F, 0x1F, 0x40, 0x00 },
        { 0x0F, 0x1F, 0x44, 0x00, 0x00 },
        { 0x66, 0x0F, 0x1F, 0x44, 0x00, 0x00 },
        { 0x0F, 0x1F, 0x80, 0x00, 0x00, 0x00, 0x00 },
        { 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 },
    };

    size_t pad = 16 - (ctx->emit.count & 15);
    if (pad < 16) {
        ctx->nop_pads = pad;

        uint8_t* dst = tb_cgemit_reserve(&ctx->emit, pad);
        tb_cgemit_commit(&ctx->emit, pad);

        if (pad > 8) {
            size_t rem = pad - 8;
            memset(dst, 0x66, rem);
            pad -= rem, dst += rem;
        }
        memcpy(dst, nops[pad - 1], pad);
    }
}

static void emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t stack_usage) {
    size_t patch_pos = e->count;
    UnwindInfo unwind = {
        .version = 1,
        .flags = 0, // UNWIND_FLAG_EHANDLER,
        .prolog_length = out_f->prologue_length,
        .code_count = 0,
    };
    tb_outs(e, sizeof(UnwindInfo), &unwind);

    size_t code_count = 0;
    if (stack_usage > 8) {
        UnwindCode codes[] = {
            // sub rsp, stack_usage
            { .code_offset = 4, .unwind_op = UNWIND_OP_ALLOC_SMALL, .op_info = (stack_usage / 8) - 1 },
        };
        tb_outs(e, sizeof(codes), codes);
        code_count += 1;
    }

    tb_patch1b(e, patch_pos + offsetof(UnwindInfo, code_count), code_count);
}

static void our_print_rip32(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos, int disp_pos, int32_t imm);

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void dump_stack_layout(Ctx* restrict ctx, TB_CGEmitter* e) {
    TB_FunctionPrototype* proto = ctx->f->prototype;
    size_t stack_usage = ctx->stack_usage;

    TB_OPTDEBUG(ANSI)(E("\x1b[32m"));
    E("// STACK OF '%s' (SPACE = %zu):\n", ctx->f->super.name, stack_usage);
    FOR_REV_N(i, 0, proto->param_count) {
        E("//  [SP + %3td] CALLER PARAM\n", stk_offset(ctx, 1+i, 1));
    }

    if (stack_usage > 8) {
        E("// ==============\n");
        E("//  [SP + %3zu] RPC\n", stk_offset(ctx, 0, 1));
        if (ctx->f->features.gen & TB_FEATURE_FRAME_PTR) {
            E("//  [SP + %3zu] saved RBP\n", stack_usage - 8);
        }
        FOR_REV_N(i, 0, ctx->num_spills) {
            E("//  [SP + %3td] STACK%zu\n", stk_offset(ctx, 1 + ctx->param_count + ctx->call_usage + i, 1), 1 + ctx->param_count + ctx->call_usage + i);
        }
        FOR_REV_N(i, 0, ctx->call_usage) {
            E("//  [SP + %3td] CALLEE ARG\n", stk_offset(ctx, 1 + ctx->param_count + i, 1));
        }
    }
    TB_OPTDEBUG(ANSI)(E("\x1b[0m"));
}
#undef E

static bool sym_handler(TB_Disasm* disasm, int inst_length, uint64_t field, int field_pos, int field_len, bool is_offset) {
    Disasm* d = disasm->ctx;
    size_t pos = disasm->in_curr;

    int64_t imm = field;
    if (d->patch && d->patch->pos == pos + (field_pos / 8)) {
        const TB_Symbol* target = d->patch->target;

        if (target->name[0] == 0) {
            tb_disasm_outf(disasm, "sym%p", target);
        } else {
            tb_disasm_outf(disasm, "%s", target->name);
        }

        if (imm > 0) {
            tb_disasm_outf(disasm, " + %"PRId64, imm);
        } else if (imm < 0) {
            tb_disasm_outf(disasm, " - %"PRId64, imm);
        }

        d->patch = d->patch->next;
        return true;
    } else if (is_offset) {
        uint32_t target = pos + inst_length + imm;
        int bb = tb_emit_get_label(d->emit, target);
        uint32_t landed = d->emit->labels[bb] & 0x7FFFFFFF;

        #if ASM_STYLE_PRINT_POS
        tb_disasm_outf(disasm, "BB%d", bb);
        #else
        tb_disasm_outf(disasm, ".bb%d", bb);
        #endif

        if (landed != target) {
            tb_disasm_outf(disasm, " + %d", (int)target - (int)landed);
        }
        return true;
    } else {
        return false;
    }
}

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    size_t r = 0;
    uint32_t src_section = out_f->section;

    for (TB_SymbolPatch* patch = out_f->first_patch; patch; patch = patch->next) {
        if (patch->target->tag == TB_SYMBOL_FUNCTION) {
            uint32_t dst_section = ((TB_Function*) patch->target)->output->section;

            // you can't do relocations across sections
            if (src_section == dst_section) {
                assert(patch->pos < out_f->code_size);

                // x64 thinks of relative addresses as being relative
                // to the end of the instruction or in this case just
                // 4 bytes ahead hence the +4.
                size_t actual_pos = out_f->code_pos + patch->pos + 4;

                uint32_t p = ((TB_Function*) patch->target)->output->code_pos - actual_pos;
                memcpy(&out_f->code[patch->pos], &p, sizeof(uint32_t));

                r += 1;
                patch->internal = true;
            }
        }
    }

    return out_f->patch_count - r;
}

int is_pack_op_supported(TB_Function* f, TB_DataType dt, TB_Node* n, int width) {
    // consider any ops which have no vector forms as 0
    int elem_bits = tb_data_type_bit_size(f->super.module, dt.type);
    switch (n->type) {
        case TB_LOAD:
        case TB_STORE:
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        case TB_FMIN:
        case TB_FMAX: {
            int bits = elem_bits * width;
            return bits == 128 || bits == 256 || bits == 512;
        }

        // some int multiplies are not possible
        // in vectors, but we'll write simple fallbacks
        // later on.
        // case TB_MUL:
        // break;

        default: return 0;
    }
}

int max_pack_width_for_op(TB_Function* f, TB_DataType dt, TB_Node* n) {
    static int limits[][7] = {
        // SSE
        { 16, 8,  4,  2, 0, 4,  2 },
        // AVX
        { 32, 16, 8,  4, 0, 8,  4 },
        // AVX512
        { 64, 32, 16, 8, 0, 16, 8 },
    };

    int i = 0;
    if (f->features.x64 & TB_FEATURE_X64_AVX) {
        i = 2;
    } else if (f->features.x64 & TB_FEATURE_X64_SSE2) {
        i = 1;
    }

    TB_ASSERT(dt.type >= 2);
    return limits[i][dt.type - 2];
}

ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .global_init = global_init,
    .can_gvn = can_gvn,
    .node_name = node_name,
    .print_extra = print_extra,
    .flags = node_flags,
    .extra_bytes = extra_bytes,
    .is_pack_op_supported = is_pack_op_supported,
    .max_pack_width_for_op = max_pack_width_for_op,
    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};
#endif
