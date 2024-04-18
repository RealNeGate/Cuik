#include "x64.h"
#include <tb_x64.h>
#include "x64_emitter.h"
#include "x64_disasm.c"

#ifdef TB_HAS_X64
enum {
    // register classes
    REG_CLASS_FLAGS = 1,
    REG_CLASS_GPR,
    REG_CLASS_XMM,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 1,
};

#include "../codegen_impl.h"

// node with X86MemOp (mov, add, and...) will have this layout of inputs:
//   [1] mem
//   [2] base (or first src)
//   [3] idx
//   [4] val
typedef struct {
    enum {
        MODE_REG,
        MODE_LD, // reg <- mem
        MODE_ST, // mem <- reg
    } mode;
    Scale scale;
    TB_DataType dt;
    int32_t disp;
    int32_t imm;
} X86MemOp;

typedef struct {
    int cc;
} X86Cmov;

typedef struct {
    TB_FunctionPrototype* proto;
    TB_Symbol* sym;
    uint32_t clobber_gpr;
    uint32_t clobber_xmm;
} X86Call;

// machine node types
typedef enum X86NodeType {
    x86_int3 = TB_MACH_X86,

    #define X(name) x86_ ## name,
    #include "x64_nodes.inc"
} X86NodeType;

static bool can_gvn(TB_Node* n) {
    return true;
}

static size_t extra_bytes(TB_Node* n) {
    X86NodeType type = n->type;
    switch (type) {
        case x86_int3:
        case x86_vzero:
        return 0;

        case x86_idiv: case x86_div:
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test: case x86_lea:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor: case x86_ucomi:
        case x86_addimm: case x86_orimm: case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        return sizeof(X86MemOp);

        case x86_call:
        case x86_static_call:
        return sizeof(X86Call);

        case x86_cmovcc:
        case x86_setcc:
        return sizeof(X86Cmov);

        default:
        tb_todo();
    }
}

static const char* node_name(TB_Node* n) {
    switch (n->type) {
        case x86_int3: return "int3";
        #define X(name) case x86_ ## name: return STR(x86_ ## name);
        #include "x64_nodes.inc"
        default: return NULL;
    }
}

static bool is_x86_mem_op(TB_Node* n) {
    switch (n->type) {
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test: case x86_lea:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        return true;

        default:
        return false;
    }
}

static void print_extra(TB_Node* n) {
    static const char* modes[] = { "reg", "ld", "st" };
    switch (n->type) {
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test: case x86_lea:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf(", scale=%d, disp=%d, mode=%s", 1<<op->scale, op->disp, modes[op->mode]);
            break;
        }

        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf(", scale=%d, disp=%d, mode=%s, imm=%d", 1<<op->scale, op->disp, modes[op->mode], op->imm);
            break;
        }
    }
}

static void print_dumb_extra(TB_Node* n) {
    static const char* modes[] = { "reg", "ld", "st" };
    switch (n->type) {
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test: case x86_lea:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf("scale=%d disp=%d mode=%s ", 1<<op->scale, op->disp, modes[op->mode]);
            break;
        }

        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf("scale=%d disp=%d mode=%s imm=%d ", 1<<op->scale, op->disp, modes[op->mode], op->imm);
            break;
        }
    }
}

typedef struct {
    int64_t min, max;
    bool if_chain;
} AuxBranch;

static const struct ParamDesc {
    int chkstk_limit;
    int gpr_count;
    int xmm_count;
    uint16_t caller_saved_xmms; // XMM0 - XMMwhatever
    uint16_t caller_saved_gprs; // bitfield

    GPR gprs[6];
} param_descs[] = {
    // win64
    { 4096,    4, 4, 6, WIN64_ABI_CALLER_SAVED,   { RCX, RDX, R8,  R9,  0,  0 } },
    // system v
    { INT_MAX, 6, 4, 5, SYSV_ABI_CALLER_SAVED,    { RDI, RSI, RDX, RCX, R8, R9 } },
    // syscall
    { INT_MAX, 6, 4, 5, SYSCALL_ABI_CALLER_SAVED, { RDI, RSI, RDX, R10, R8, R9 } },
};

enum {
    NO_RCX     = ~((1 << RCX)),
};

// *out_mask of 0 means no mask
static TB_X86_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_TAG_INT || dt.type == TB_TAG_PTR);
    if (dt.type == TB_TAG_PTR) return *out_mask = 0, TB_X86_QWORD;

    TB_X86_DataType t = TB_X86_NONE;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = TB_X86_BYTE;
    else if (dt.data <= 16) bits = 16, t = TB_X86_WORD;
    else if (dt.data <= 32) bits = 32, t = TB_X86_DWORD;
    else if (dt.data <= 64) bits = 64, t = TB_X86_QWORD;

    assert(bits != 0 && "TODO: large int support");
    assert(dt.data != 0);
    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

static TB_X86_DataType legalize_int2(TB_DataType dt) {
    uint64_t m;
    return legalize_int(dt, &m);
}

static TB_X86_DataType legalize_float(TB_DataType dt) {
    assert(dt.type == TB_TAG_F32 || dt.type == TB_TAG_F64);
    return (dt.type == TB_TAG_F64 ? TB_X86_F64x1 : TB_X86_F32x1);
}

static TB_X86_DataType legalize(TB_DataType dt) {
    if (dt.type == TB_TAG_F32) {
        return TB_X86_F32x1;
    } else if (dt.type == TB_TAG_F64) {
        return TB_X86_F64x1;
    } else {
        uint64_t m;
        return legalize_int(dt, &m);
    }
}

static bool fits_into_int32(uint64_t x) {
    uint32_t hi = x >> 32ull;
    return hi == 0 || hi == 0xFFFFFFFF;
}

static bool try_for_imm32(int bits, TB_Node* n, int32_t* out_x) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (bits > 32) {
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
        // ANY_GPR = OP(ANY_GPR, ANY_GPR)
        case x86_add: case x86_or:  case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_test:
        case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            return op->mode != MODE_ST ? 4 : 0;
        }

        case x86_mov:
        case x86_vmov:
        // ANY_GPR = OP(ANY_GPR, IMM)
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        case x86_movzx8: case x86_movzx16: case x86_movsx8: case x86_movsx16: case x86_movsx32:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            return op->mode == MODE_REG ? 2 : 0;
        }

        // ANY_GPR = OP(COND, shared: ANY_GPR, ANY_GPR)
        case x86_cmovcc:
        return 2;

        // ANY_GPR = OP(ANY_GPR, CL)
        case TB_SHL: case TB_SHR: case TB_ROL: case TB_ROR: case TB_SAR:
        return 1;

        case TB_ATOMIC_LOAD:
        return 0;

        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        return 3;

        case TB_MACH_COPY:
        case TB_MACH_MOVE:
        case TB_FLOAT_EXT:
        return 1;
    }

    return n->type >= TB_AND && n->type <= TB_CMP_FLE;
}

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->abi_index = abi == TB_ABI_SYSTEMV ? 1 : 0;
    ctx->has_flags = true;

    // currently only using 16 GPRs and 16 XMMs, AVX gives us
    // 32 YMMs (which double as XMMs) and later on APX will do
    // 32 GPRs.
    ctx->num_regs[REG_CLASS_FLAGS] = 1;
    ctx->num_regs[REG_CLASS_GPR] = 16;
    ctx->num_regs[REG_CLASS_XMM] = 16;

    uint16_t all_gprs = 0xFFFF & ~(1 << RSP);
    if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
        all_gprs &= ~(1 << RBP);
        ctx->stack_header = 16;
    } else {
        ctx->stack_header = 8;
    }

    ctx->normie_mask[REG_CLASS_FLAGS] = new_regmask(ctx->f, REG_CLASS_FLAGS, false, 1);
    ctx->normie_mask[REG_CLASS_GPR]   = new_regmask(ctx->f, REG_CLASS_GPR,   false, all_gprs);
    ctx->normie_mask[REG_CLASS_XMM]   = new_regmask(ctx->f, REG_CLASS_XMM,   false, 0xFFFF);

    // mark GPR callees (technically includes RSP but since it's
    // never conventionally allocated we should never run into issues)
    // ctx->callee_saved[REG_CLASS_GPR] = ~param_descs[ctx->abi_index].caller_saved_gprs;

    // mark XMM callees
    // ctx->callee_saved[REG_CLASS_XMM] = 0;
    /* FOR_N(i, param_descs[ctx->abi_index].caller_saved_xmms, 16) {
        ctx->callee_saved[REG_CLASS_XMM] |= (1ull << i);
    } */

    TB_FunctionPrototype* proto = ctx->f->prototype;
    TB_Node** params = ctx->f->params;
    TB_Node* root_ctrl = params[0];

    // walk the entry to find any parameter stack slots
    FOR_N(i, 0, ctx->f->param_count) {
        TB_Node* proj = params[3 + i];
        if (proj->user_count == 0 || single_use(proj) || USERI(proj->users) == 0) { continue; }
        TB_Node* store_op = USERN(proj->users);
        if (store_op->type != TB_STORE) { continue; }
        TB_Node* addr = store_op->inputs[2];
        if (addr->type != TB_LOCAL) { continue; }

        TB_NodeLocal* local = TB_NODE_GET_EXTRA(addr);
        local->stack_pos = ctx->stack_header + i*8;

        if (i >= 4 && ctx->abi_index == 0) {
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
        if (local->stack_pos < 0) { continue; }
        // each stack slot is 8bytes
        ctx->num_spills = align_up(ctx->num_spills + (local->size+7)/8, (local->align+7)/8);
        local->stack_pos = -(8 + ctx->num_spills*8);
    }
}

static RegMask* normie_mask(Ctx* restrict ctx, TB_DataType dt) {
    return ctx->normie_mask[TB_IS_FLOAT_TYPE(dt) ? REG_CLASS_XMM : REG_CLASS_GPR];
}

// returns true if it should split
static bool addr_split_heuristic(int arr_uses, int stride, int scale) {
    // doesn't matter if we do *1 *2 *4 *8, all
    // basically just an LEA. once we leave LEA
    // levels we need to do explicit ops with regs
    // which increases pressure.
    int cost = 0;
    if (stride != 1 << scale || scale >= 4) {
        cost = 3;
    } else {
        cost = 1;
    }

    return cost*arr_uses > 10;
}

// store(binop(load(a), b))
static bool can_folded_store(TB_Node* mem, TB_Node* addr, TB_Node* n) {
    if ((n->type >= TB_AND  && n->type <= TB_SUB) ||
        (n->type >= TB_FADD && n->type <= TB_FMAX)) {
        return
            n->inputs[1]->type == TB_LOAD &&
            n->inputs[1]->inputs[1] == mem &&
            n->inputs[1]->inputs[2] == addr &&
            single_use(n) &&
            single_use(n->inputs[1]);
    }

    return false;
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

static TB_Node* mach_symbol(TB_Function* f, TB_Symbol* s) {
    TB_Node* n = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_PTR, 1, sizeof(TB_NodeMachSymbol));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeMachSymbol, .sym = s);
    return tb__gvn(f, n, sizeof(TB_NodeMachSymbol));
}

static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n) {
    if (n->type == TB_PROJ) {
        return n;
    } else if (n->type == TB_ROOT) {
        assert(n->input_count == 2);
        assert(n->inputs[1]->type == TB_RETURN);
        TB_Node* ret = n->inputs[1];

        // add some callee-saved mach projections
        int j = 3 + f->prototype->param_count;

        uint32_t callee_saved_gpr = ~param_descs[ctx->abi_index].caller_saved_gprs;
        callee_saved_gpr &= ~(1u << RSP);
        if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
            callee_saved_gpr &= ~(1 << RBP);
        }

        FOR_N(i, 0, ctx->num_regs[REG_CLASS_GPR]) {
            if ((callee_saved_gpr >> i) & 1) {
                RegMask* rm = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << i);
                TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, TB_TYPE_I64, 1, sizeof(TB_NodeMachProj));
                TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = j++, .def = rm);

                set_input(f, proj, n, 0);
                add_input_late(f, ret, proj);
            }
        }

        FOR_N(i, param_descs[ctx->abi_index].caller_saved_xmms, ctx->num_regs[REG_CLASS_XMM]) {
            RegMask* rm = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << i);
            TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, TB_TYPE_F64, 1, sizeof(TB_NodeMachProj));
            TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = j++, .def = rm);

            set_input(f, proj, n, 0);
            add_input_late(f, ret, proj);
        }

        return n;
    } else if (n->type == TB_PHI) {
        if (TB_IS_SCALAR_TYPE(n->dt)) {
            RegMask* rm = ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_XMM : REG_CLASS_GPR];

            // just in case we have some recursive phis, RA should be able to fold it away later.
            // we have to be a bit hacky since we can't subsume the node with something that's
            // referencing it (we'll get a cycle we didn't want).
            TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = rm, .use = rm);

            subsume_node2(f, n, cpy);
            set_input(f, cpy, n, 1);

            // we just want some copies on the data edges which RA will coalesce, this way we
            // never leave SSA.
            FOR_N(i, 1, n->input_count) {
                TB_Node* in = n->inputs[i];
                assert(in->type != TB_MACH_MOVE);

                TB_Node* move = tb_alloc_node(f, TB_MACH_MOVE, in->dt, 2, 0);
                set_input(f, move, in, 1);
                set_input(f, n, move, i);
            }

            // we did the subsumes for it
            return n;
        } else {
            return n;
        }
    } else if (n->type == TB_F32CONST) {
        uint32_t imm = (Cvt_F32U32) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value }.i;

        if (imm == 0) {
            TB_Node* k = tb_alloc_node(f, x86_vzero, n->dt, 1, 0);
            set_input(f, k, f->root_node, 0);
            return k;
        } else {
            TB_Global* g = tb__small_data_intern(ctx->module, sizeof(float), &imm);
            TB_Node* base = mach_symbol(f, &g->super);

            TB_Node* op = tb_alloc_node(f, x86_vmov, TB_TYPE_F32, 5, sizeof(X86MemOp));
            X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
            op_extra->mode = MODE_LD;
            set_input(f, op, base, 2);
            return op;
        }
    } else if (n->type == TB_F64CONST) {
        uint64_t imm = (Cvt_F64U64) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value }.i;
        if (imm == 0) {
            TB_Node* k = tb_alloc_node(f, x86_vzero, n->dt, 1, 0);
            set_input(f, k, f->root_node, 0);
            return k;
        } else {
            TB_Global* g = tb__small_data_intern(ctx->module, sizeof(double), &imm);
            TB_Node* base = mach_symbol(f, &g->super);

            TB_Node* op = tb_alloc_node(f, x86_vmov, TB_TYPE_F64, 5, sizeof(X86MemOp));
            X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
            op_extra->mode = MODE_LD;
            set_input(f, op, base, 2);
            return op;
        }
    } else if (n->type == TB_BITCAST || n->type == TB_TRUNCATE) {
        TB_Node* in = n->inputs[1];
        RegMask* def_rm = ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt)  ? REG_CLASS_XMM : REG_CLASS_GPR];
        RegMask* use_rm = ctx->normie_mask[TB_IS_FLOAT_TYPE(in->dt) ? REG_CLASS_XMM : REG_CLASS_GPR];

        // mach copy actually just handles these sorts of things mostly
        TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, in, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = def_rm, .use = use_rm);
        return cpy;
    } else if (n->type == TB_ZERO_EXT) {
        TB_DataType src_dt = n->inputs[1]->dt;
        assert(src_dt.type == TB_TAG_INT);
        int src_bits = src_dt.data;

        // as long as any of these zero-extend to 32bits from a smaller size they're
        // capable of being a 64bit zero extend since 32bit ops will auto zero ext to 64bit.
        int op_type = -1;
        switch (src_bits) {
            case 8:  op_type = x86_movzx8;  break;
            case 16: op_type = x86_movzx16; break;
            case 32: op_type = x86_mov;     break;
        }

        if (op_type == x86_mov) {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            TB_Node* op = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, op, n->inputs[1], 1);
            TB_NODE_SET_EXTRA(op, TB_NodeMachCopy, .def = rm, .use = rm);
            return op;
        } else if (op_type >= 0) {
            TB_Node* op = tb_alloc_node(f, op_type, n->dt, 5, sizeof(X86MemOp));
            set_input(f, op, n->inputs[1], 2);
            return op;
        } else if (src_bits < 32) {
            // we can take advantange of the existing 64bit zero extension
            uint64_t mask = UINT64_MAX >> (64 - src_bits);
            TB_Node* op = tb_alloc_node(f, x86_andimm, n->dt, 4, sizeof(X86MemOp));
            set_input(f, op, n->inputs[1], 2);
            TB_NODE_SET_EXTRA(op, X86MemOp, .imm = mask);
            return op;
        } else {
            // uint64_t mask = UINT64_MAX >> (64 - src_bits);
            tb_todo();
        }
    } else if (n->type == TB_SIGN_EXT) {
        TB_DataType src_dt = n->inputs[1]->dt;
        assert(src_dt.type == TB_TAG_INT);
        int src_bits = src_dt.data;

        int op_type = -1;
        switch (src_bits) {
            case 8:  op_type = x86_movsx8;  break;
            case 16: op_type = x86_movsx16; break;
            case 32: op_type = x86_movsx32; break;
        }

        if (op_type >= 0) {
            TB_Node* op = tb_alloc_node(f, op_type, n->dt, 5, sizeof(X86MemOp));
            set_input(f, op, n->inputs[1], 2);
            return op;
        } else {
            // unconventional sizes do:
            //   SHL dst, x
            //   SAR dst, x (or SHR if zero ext)
            //
            // where x is 'reg_width - val_width'
            // int dst_bits = dt == TB_X86_TYPE_QWORD ? 64 : 32;
            // int ext = is_signed ? SAR : SHR;
            // Val imm = val_imm(dst_bits - bits_in_type);
            tb_todo();
        }
    } else if (n->type == TB_LOCAL) {
        // we don't directly ref the Local, this is the accessor op whenever we're
        // not folding into some other op nicely.
        TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;
        op_extra->disp = TB_NODE_GET_EXTRA_T(n, TB_NodeLocal)->stack_pos;

        subsume_node2(f, n, op);
        set_input(f, op, ctx->frame_ptr, 2);
        return n;
    } else if (n->type == TB_SYMBOL) {
        TB_Node* sym = mach_symbol(f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);

        TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;
        subsume_node2(f, n, op);
        set_input(f, op, sym, 2);
        return n;
    } else if (n->type >= TB_UDIV && n->type <= TB_SMOD) {
        // udiv, sdiv, umod, smod
        bool is_signed = (n->type == TB_SDIV || n->type == TB_SMOD);
        bool is_div    = (n->type == TB_UDIV || n->type == TB_SDIV);

        TB_Node* op = tb_alloc_node(f, is_signed ? x86_idiv : x86_div, TB_TYPE_TUPLE, 5, sizeof(X86MemOp));
        // dividend
        set_input(f, op, n->inputs[2], 2);
        // divisor (into RAX, and RDX gets zero'd)
        set_input(f, op, n->inputs[1], 4);

        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->dt = n->dt;

        TB_Node* rax = tb__make_proj(f, n->dt, op, 0); // quotient
        TB_Node* rdx = tb__make_proj(f, n->dt, op, 1); // remainder
        return is_div ? rax : rdx;
    } else if ((n->type >= TB_SHL && n->type <= TB_ROR) && n->inputs[2]->type == TB_ICONST) {
        const static int ops[] = { x86_shlimm, x86_shrimm, x86_sarimm, x86_rolimm, x86_rorimm };
        X86NodeType type = ops[n->type - TB_SHL];
        uint64_t imm = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;

        TB_Node* op = tb_alloc_node(f, type, n->dt, 3, sizeof(X86MemOp));
        set_input(f, op, n->inputs[1], 2);
        TB_NODE_SET_EXTRA(op, X86MemOp, .imm = imm & 63);
        return op;
    } else if (n->type == TB_SELECT) {
        TB_Node* op = tb_alloc_node(f, x86_cmovcc, n->dt, 5, sizeof(X86Cmov));

        Cond cc = E;
        TB_Node* cond = n->inputs[1];
        if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
            switch (cond->type) {
                case TB_CMP_EQ:  cc = E;  break;
                case TB_CMP_NE:  cc = NE; break;
                case TB_CMP_SLT: cc = L;  break;
                case TB_CMP_SLE: cc = LE; break;
                case TB_CMP_ULT: cc = B;  break;
                case TB_CMP_ULE: cc = BE; break;
                case TB_CMP_FLT: cc = B;  break;
                case TB_CMP_FLE: cc = BE; break;
                default: tb_unreachable();
            }

            set_input(f, op, cond->inputs[1], 1);
            set_input(f, op, cond->inputs[2], 2);
        } else {
            set_input(f, op, cond, 1);
        }
        set_input(f, op, n->inputs[2], 3);
        set_input(f, op, n->inputs[3], 4);
        TB_NODE_SET_EXTRA(op, X86Cmov, .cc = cc);
        return op;
    } else if (n->type == TB_CALL) {
        TB_Node* op = tb_alloc_node(f, x86_call, n->dt, n->input_count, sizeof(X86Call));
        set_input(f, op, n->inputs[0], 0); // ctrl
        set_input(f, op, n->inputs[1], 1); // mem
        X86Call* op_extra = TB_NODE_GET_EXTRA(op);

        // check for static call
        if (n->inputs[2]->type == TB_SYMBOL) {
            op->type = x86_static_call;
            op_extra->sym = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;
        } else {
            set_input(f, op, n->inputs[2], 2);
        }

        int num_params = n->input_count - 3;
        // win64 abi will alloc 4 param slots if you even use one
        if (ctx->abi_index == 0 && num_params > 0 && num_params < 4) { num_params = 4; }
        if (num_params > ctx->call_usage) { ctx->call_usage = num_params; }

        const struct ParamDesc* abi = &param_descs[ctx->abi_index];
        op_extra->clobber_gpr = abi->caller_saved_gprs;
        op_extra->clobber_xmm = ~0ull >> (64 - abi->caller_saved_xmms);

        int gprs_used = 0, xmms_used = 0;
        FOR_N(i, 3, n->input_count) {
            int param_num = i - 3;

            // on win64 we always have the XMMs and GPRs used match the param_num
            // so if XMM2 is used, it's always the 3rd parameter.
            if (ctx->abi_index == 0) { xmms_used = gprs_used = param_num; }

            if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) {
                if (xmms_used < abi->xmm_count) {
                    op_extra->clobber_xmm &= ~(1u << xmms_used);
                    xmms_used += 1;
                } else if (param_num + 1 > ctx->num_regs[REG_CLASS_STK]) {
                    ctx->num_regs[REG_CLASS_STK] = param_num + 1;
                }
            } else {
                assert(n->inputs[i]->dt.type == TB_TAG_INT || n->inputs[i]->dt.type == TB_TAG_PTR);
                if (gprs_used < abi->gpr_count) {
                    op_extra->clobber_gpr &= ~(1u << abi->gprs[gprs_used]);
                    gprs_used += 1;
                } else if (param_num + 1 > ctx->num_regs[REG_CLASS_STK]) {
                    ctx->num_regs[REG_CLASS_STK] = param_num + 1;
                }
            }

            set_input(f, op, n->inputs[i], i);
        }

        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            if (un->type != TB_PROJ) continue;
            int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
            if (index >= 2) {
                if (TB_IS_FLOAT_TYPE(un->dt))
                { op_extra->clobber_xmm &= ~(1u << (index - 2)); }
                else
                { op_extra->clobber_gpr &= ~(1u << (index == 2 ? RAX : RDX)); }
            }
        }

        return op;
    } else if (n->type == TB_VA_START) {
        assert(ctx->module->target_abi == TB_ABI_WIN64 && "How does va_start even work on SysV?");

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
        op_extra->disp = ctx->stack_header + proto->param_count*8;
        return op;
    } else if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        TB_Node* a = n->inputs[1];
        TB_Node* b = n->inputs[2];

        TB_Node* mach_cond;
        int flip = 0;
        if (TB_IS_FLOAT_TYPE(cmp_dt)) {
            mach_cond = tb_alloc_node(f, x86_ucomi, TB_TYPE_I8, 5, sizeof(X86MemOp));
            set_input(f, mach_cond, a, 4);
            set_input(f, mach_cond, b, 2);
        } else {
            if (a->type == TB_ICONST && b->type != TB_ICONST) {
                flip ^= 1;
                SWAP(TB_Node*, a, b);
            }

            mach_cond = tb_alloc_node(f, x86_cmp, TB_TYPE_I8, 5, sizeof(X86MemOp));

            X86MemOp* op_extra = TB_NODE_GET_EXTRA(mach_cond);
            op_extra->dt = cmp_dt;

            int32_t x;
            if ((cmp_dt.type == TB_TAG_INT || cmp_dt.type == TB_TAG_PTR) && try_for_imm32(cmp_dt.type == TB_TAG_PTR ? 64 : cmp_dt.data, b, &x)) {
                if (x == 0 && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
                    mach_cond->type = x86_test;
                    set_input(f, mach_cond, a, 4);
                } else {
                    mach_cond->type = x86_cmpimm;
                    op_extra->imm = x;
                }
            } else {
                set_input(f, mach_cond, b, 4);
            }
            set_input(f, mach_cond, a, 2);
        }

        Cond cc;
        switch (n->type) {
            case TB_CMP_EQ:  cc = E;  break;
            case TB_CMP_NE:  cc = NE; break;
            case TB_CMP_SLT: cc = L;  break;
            case TB_CMP_SLE: cc = LE; break;
            case TB_CMP_ULT: cc = B;  break;
            case TB_CMP_ULE: cc = BE; break;
            case TB_CMP_FLT: cc = B;  break;
            case TB_CMP_FLE: cc = BE; break;
            default: tb_unreachable();
        }

        TB_Node* op = tb_alloc_node(f, x86_setcc, n->dt, 2, sizeof(X86Cmov));
        set_input(f, op, mach_cond, 1);
        TB_NODE_SET_EXTRA(op, X86Cmov, .cc = cc);
        return op;
    } else if (n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH) {
        TB_Node* cond = n->inputs[1];
        TB_NodeBranchProj* if_br = cfg_if_branch(n);
        if (if_br) {
            // If-logic lowering, just generate a FLAGS and make
            // the branch compare on that instead.
            TB_Node* mach_cond = NULL;
            if (cond->type == x86_setcc) {
                set_input(f, n, cond->inputs[1], 1);
                if_br->key = TB_NODE_GET_EXTRA_T(cond, X86Cmov)->cc;
            } else if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
                TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cond, TB_NodeCompare)->cmp_dt;
                TB_Node* a = cond->inputs[1];
                TB_Node* b = cond->inputs[2];

                int flip = (if_br->key != 0);
                if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                    mach_cond = tb_alloc_node(f, x86_ucomi, TB_TYPE_I8, 5, sizeof(X86MemOp));
                    set_input(f, mach_cond, a, 4);
                    set_input(f, mach_cond, b, 2);
                } else {
                    if (a->type == TB_ICONST && b->type != TB_ICONST) {
                        flip ^= 1;
                        SWAP(TB_Node*, a, b);
                    }

                    mach_cond = tb_alloc_node(f, x86_cmp, TB_TYPE_I8, 5, sizeof(X86MemOp));

                    X86MemOp* op_extra = TB_NODE_GET_EXTRA(mach_cond);
                    op_extra->dt = cmp_dt;

                    int32_t x;
                    if ((cmp_dt.type == TB_TAG_INT || cmp_dt.type == TB_TAG_PTR) && try_for_imm32(cmp_dt.type == TB_TAG_PTR ? 64 : cmp_dt.data, b, &x)) {
                        if (x == 0 && (cond->type == TB_CMP_EQ || cond->type == TB_CMP_NE)) {
                            mach_cond->type = x86_test;
                            set_input(f, mach_cond, a, 4);
                        } else {
                            mach_cond->type = x86_cmpimm;
                            op_extra->imm = x;
                        }
                    } else {
                        set_input(f, mach_cond, b, 4);
                    }
                    set_input(f, mach_cond, a, 2);
                }

                Cond cc;
                switch (cond->type) {
                    case TB_CMP_EQ:  cc = E;  break;
                    case TB_CMP_NE:  cc = NE; break;
                    case TB_CMP_SLT: cc = L;  break;
                    case TB_CMP_SLE: cc = LE; break;
                    case TB_CMP_ULT: cc = B;  break;
                    case TB_CMP_ULE: cc = BE; break;
                    case TB_CMP_FLT: cc = B;  break;
                    case TB_CMP_FLE: cc = BE; break;
                    default: tb_unreachable();
                }
                if_br->key = cc ^ flip;
            } else if (if_br->key == 0) {
                mach_cond = tb_alloc_node(f, x86_test, TB_TYPE_I8, 5, sizeof(X86MemOp));
                TB_NODE_SET_EXTRA(mach_cond, X86MemOp, .dt = cond->dt);
                set_input(f, mach_cond, cond, 2);
                set_input(f, mach_cond, cond, 4);
                if_br->key = E;
            } else {
                mach_cond = tb_alloc_node(f, x86_cmpimm, TB_TYPE_I8, 5, sizeof(X86MemOp));
                TB_NODE_SET_EXTRA(mach_cond, X86MemOp, .dt = cond->dt, .imm = if_br->key);
                set_input(f, mach_cond, cond, 2);
                if_br->key = E;
            }
            set_input(f, n, mach_cond, 1);
        } else {
            tb_todo();
        }

        return n;
    }

    int32_t x;
    if (n->type == TB_MUL && try_for_imm32(n->dt.data, n->inputs[2], &x)) {
        TB_Node* op = tb_alloc_node(f, x86_imulimm, n->dt, 2, sizeof(X86MemOp));
        set_input(f, op, n->inputs[1], 1);
        TB_NODE_SET_EXTRA(op, X86MemOp, .imm = x);
        return op;
    }

    // any of these ops might be the starting point to complex addressing modes
    if ((n->type >= TB_AND && n->type <= TB_SUB)   ||
        (n->type >= TB_FADD && n->type <= TB_FMAX) ||
        n->type == TB_LOAD || n->type == TB_STORE  ||
        n->type == TB_PTR_OFFSET) {
        const static int ops[]  = { x86_and, x86_or, x86_xor, x86_add, x86_sub };
        const static int fops[] = { x86_vadd, x86_vsub, x86_vmul, x86_vdiv, x86_vmin, x86_vmax };

        // folded binop with immediate
        int32_t x;
        if (n->type >= TB_AND && n->type <= TB_SUB) {
            assert(n->dt.type == TB_TAG_INT);
            if (try_for_imm32(n->dt.data, n->inputs[2], &x)) {
                X86NodeType type = ops[n->type - TB_AND] + (x86_andimm - x86_and);

                TB_Node* op = tb_alloc_node(f, type, n->dt, 5, sizeof(X86MemOp));
                X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
                op_extra->imm = x;

                set_input(f, op, n->inputs[1], 2);
                return op;
            }
        }

        TB_Node* op = tb_alloc_node(f, x86_lea, n->dt, 5, sizeof(X86MemOp));
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;

        // folded load now
        if (n->type == TB_STORE) {
            op_extra->mode = MODE_ST;
            op_extra->dt = n->inputs[3]->dt;

            op->type = TB_IS_FLOAT_TYPE(n->inputs[3]->dt) ? x86_vmov : x86_mov;
            op->dt   = TB_TYPE_MEMORY;

            set_input(f, op, n->inputs[0], 0); // ctrl in
            set_input(f, op, n->inputs[1], 1); // mem in

            TB_Node* rhs = n->inputs[3];
            if (can_folded_store(n->inputs[1], n->inputs[2], n->inputs[3])) {
                TB_Node* binop = n->inputs[3];
                if (binop->type >= TB_AND && binop->type <= TB_SUB) {
                    op->type = ops[binop->type - TB_AND];
                } else {
                    assert(binop->type >= TB_FADD && binop->type <= TB_FMAX);
                    op->type = fops[binop->type - TB_FADD];
                }
                rhs = binop->inputs[2];
            }

            int32_t x;
            if (op->type >= x86_add && op->type <= x86_test &&
                try_for_imm32(rhs->dt.type == TB_TAG_PTR ? 64 : rhs->dt.data, rhs, &x)) {
                op_extra->imm = x;
                op->type += (x86_andimm - x86_and);
            } else {
                set_input(f, op, rhs, 4);
            }
            n = n->inputs[2];
        } else {
            // (add X (shl Y Z)) where Z is 1-3 => (x86_lea X Y :scale Z)
            if (n->type == TB_ADD && n->inputs[2]->type == TB_SHL &&
                n->inputs[2]->inputs[2]->type == TB_ICONST &&
                // LEA on x64 is allowed for 32bit and 64bit ops
                n->dt.type == TB_TAG_INT && (n->dt.data == 32 || n->dt.data == 64)
            ) {
                TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]->inputs[2]);
                if (i->value >= 1 && i->value <= 3) {
                    set_input(f, op, n->inputs[1],            2);
                    set_input(f, op, n->inputs[2]->inputs[1], 3);
                    op_extra->scale = i->value;
                    return op;
                }
            }

            // folded binop
            if (n->type >= TB_AND && n->type <= TB_SUB) {
                op_extra->mode = MODE_REG;
                op->type = ops[n->type - TB_AND];
                set_input(f, op, n->inputs[1], 4);
                n = n->inputs[2];
            } else if (n->type >= TB_FADD && n->type <= TB_FMAX) {
                op_extra->mode = MODE_REG;
                op->type = fops[n->type - TB_FADD];
                set_input(f, op, n->inputs[1], 4);
                n = n->inputs[2];
            }

            // folded load now
            if (n->type == TB_LOAD) {
                op_extra->mode = MODE_LD;
                if (op->type == x86_lea) {
                    op->type = TB_IS_FLOAT_TYPE(n->dt) ? x86_vmov : x86_mov;
                }

                set_input(f, op, n->inputs[0], 0); // ctrl in
                set_input(f, op, n->inputs[1], 1); // mem in
                n = n->inputs[2];
            }
        }

        // [... + disp]
        if (n->type == TB_PTR_OFFSET && n->inputs[2]->type == TB_ICONST) {
            int64_t disp = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
            if (disp == (int32_t) disp) {
                op_extra->disp = disp;
                n = n->inputs[1];
            }
        }

        if (n->type == TB_SYMBOL) {
            n = mach_symbol(f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);
        } else if (n->type == TB_PTR_OFFSET) {
            set_input(f, op, n->inputs[2], 3);
            if (n->inputs[2]->type == TB_SHL && n->inputs[2]->inputs[2]->type == TB_ICONST) {
                uint64_t scale = TB_NODE_GET_EXTRA_T(n->inputs[2]->inputs[2], TB_NodeInt)->value;

                // [... + index*scale] given scale is 1,2,4,8
                if (scale <= 3) {
                    set_input(f, op, n->inputs[2]->inputs[1], 3);
                    op_extra->scale = scale;
                }
            }
            n = n->inputs[1];
        }

        // sometimes introduced by other isel bits
        if (n->type == x86_lea && n->inputs[3] == NULL) {
            n = n->inputs[2];
            op_extra->disp += TB_NODE_GET_EXTRA_T(n, X86MemOp)->disp;
        }

        if (n->type == TB_SYMBOL && n->inputs[3] == NULL) {
            TB_Node* base = mach_symbol(f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);
            set_input(f, op, base, 2);
        } else if (n->type == TB_LOCAL) {
            set_input(f, op, ctx->frame_ptr, 2);
            op_extra->disp += TB_NODE_GET_EXTRA_T(n, TB_NodeLocal)->stack_pos;
        } else {
            set_input(f, op, n, 2);
        }
        return op;
    }

    return NULL;
}

static bool node_flags(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        // regions & misc nodes don't even generate ops
        case TB_PHI:
        case TB_PROJ:
        case TB_MACH_PROJ:
        case TB_BRANCH_PROJ:
        case TB_MACH_FRAME_PTR:
        case TB_MACH_SYMBOL:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_REGION:
        case TB_AFFINE_LOOP:
        case TB_NATURAL_LOOP:
        // moves don't affect FLAGS
        case x86_mov:
        case x86_movimm:
        case x86_movsx8:
        case x86_movzx8:
        case x86_movsx16:
        case x86_movzx16:
        case x86_movsx32:
        case TB_MACH_MOVE:
        case TB_MACH_COPY:
        case TB_NEVER_BRANCH:
        // actually uses flags, that's handled in node_constraint
        case TB_BRANCH:
        case TB_RETURN:
        case TB_AFFINE_LATCH:
        // actually produces FLAGS we care about
        case x86_ucomi:
        case x86_cmp: case x86_cmpimm:
        case x86_test: case x86_testimm:
        return false;

        case TB_ICONST: {
            uint64_t x = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;
            return x == 0;
        }

        default:
        return true;
    }
}

static int node_tmp_count(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        case x86_call: case x86_static_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);
            return tb_popcount(op_extra->clobber_gpr) + tb_popcount(op_extra->clobber_xmm);
        }

        case x86_idiv: case x86_div: // clobber RDX
        return 1;

        case TB_CYCLE_COUNTER: // clobber RDX & RAX
        return 2;

        default: return 0;
    }
}

static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins) {
    switch (n->type) {
        case TB_REGION:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_TRAP:
        case TB_DEBUGBREAK:
        case TB_AFFINE_LOOP:
        case TB_NATURAL_LOOP:
        case TB_CALLGRAPH:
        if (ins) {
            // region inputs are all control
            FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
        }
        return &TB_REG_EMPTY;

        case TB_CYCLE_COUNTER: {
            ins[1] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);
            ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
            return ins[1];
        }

        case TB_LOCAL:
        case TB_SYMBOL:
        case TB_BRANCH_PROJ:
        case TB_MACH_SYMBOL:
        case TB_MACH_FRAME_PTR:
        return &TB_REG_EMPTY;

        case TB_MACH_COPY: {
            TB_NodeMachCopy* move = TB_NODE_GET_EXTRA(n);
            if (ins) { ins[1] = move->use; }
            return move->def;
        }

        case TB_MACH_PROJ: {
            return TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->def;
        }

        case TB_MACH_MOVE: {
            RegMask* rm = ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_XMM : REG_CLASS_GPR];
            if (ins) { ins[1] = rm; }
            return rm;
        }

        case TB_PHI: {
            if (ins) {
                FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
            }

            if (n->dt.type == TB_TAG_MEMORY) return &TB_REG_EMPTY;
            if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) return ctx->normie_mask[REG_CLASS_XMM];
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_ICONST:
        return ctx->normie_mask[REG_CLASS_GPR];

        case TB_PROJ: {
            if (n->dt.type == TB_TAG_MEMORY || n->dt.type == TB_TAG_CONTROL) {
                return &TB_REG_EMPTY;
            }

            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == TB_ROOT) {
                const struct ParamDesc* params = &param_descs[ctx->abi_index];
                assert(i >= 2);
                if (i == 2) {
                    // RPC is inaccessible for now
                    return &TB_REG_EMPTY;
                } else if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) {
                    return intern_regmask(ctx, REG_CLASS_XMM, false, 1u << (i - 3));
                } else {
                    return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << params->gprs[i - 3]);
                }
            } else if (n->inputs[0]->type == x86_call || n->inputs[0]->type == x86_static_call) {
                assert(i == 2 || i == 3);
                if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) {
                    if (i >= 2) { return intern_regmask(ctx, REG_CLASS_XMM, false, 1u << (i - 2)); }
                } else {
                    if (i >= 2) { return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (i == 2 ? RAX : RDX)); }
                }
            } else if (n->inputs[0]->type == x86_idiv || n->inputs[0]->type == x86_div) {
                assert(i < 2);
                return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (i ? RDX : RAX));
            } else if (n->inputs[0]->type >= TB_ATOMIC_LOAD && n->inputs[0]->type <= TB_ATOMIC_OR) {
                return i == 1 && n->users ? ctx->normie_mask[REG_CLASS_GPR] : &TB_REG_EMPTY;
            } else {
                tb_todo();
                return &TB_REG_EMPTY;
            }
        }

        case x86_vzero:
        return ctx->normie_mask[REG_CLASS_XMM];

        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        case x86_ucomi:
        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_XMM];
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            if (ins) {
                ins[1] = &TB_REG_EMPTY;

                if (op->mode == MODE_ST || op->mode == MODE_LD) {
                    RegMask* gpr = ctx->normie_mask[REG_CLASS_GPR];
                    // these are addresses so GPRs
                    FOR_N(i, 2, 4) {
                        ins[i] = n->inputs[i] ? gpr : &TB_REG_EMPTY;
                    }
                    ins[4] = n->inputs[4] ? rm : &TB_REG_EMPTY;
                } else {
                    FOR_N(i, 2, n->input_count) {
                        ins[i] = n->inputs[i] ? rm : &TB_REG_EMPTY;
                    }
                }

                if (n->inputs[2] && (n->inputs[2]->type == TB_MACH_FRAME_PTR || n->inputs[2]->type == TB_MACH_SYMBOL)) {
                    ins[2] = &TB_REG_EMPTY;
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

        case TB_FLOAT_TRUNC:
        case TB_FLOAT_EXT: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_XMM]; }
            return ctx->normie_mask[REG_CLASS_XMM];
        }

        case TB_FLOAT2INT:
        case TB_FLOAT2UINT: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_GPR]; }
            return ctx->normie_mask[REG_CLASS_XMM];
        }

        case TB_TAG_INT2FLOAT:
        case TB_UINT2FLOAT: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_XMM]; }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case x86_lea:
        // ANY_GPR = OP(ANY_GPR)
        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        // ANY_GPR = OP(ANY_GPR, ANY_GPR)
        case x86_add: case x86_or:  case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test:
        // ANY_GPR = OP(ANY_GPR, IMM)
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[1] = &TB_REG_EMPTY;
                FOR_N(i, 2, n->input_count) {
                    ins[i] = n->inputs[i] ? rm : &TB_REG_EMPTY;
                }

                if (n->inputs[2] && (n->inputs[2]->type == TB_MACH_FRAME_PTR || n->inputs[2]->type == TB_MACH_SYMBOL)) {
                    ins[2] = &TB_REG_EMPTY;
                }
            }

            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            if (op->mode == MODE_ST) {
                return &TB_REG_EMPTY;
            } else if (n->type == x86_cmp || n->type == x86_cmpimm) {
                return ctx->normie_mask[REG_CLASS_FLAGS];
            } else {
                return ctx->normie_mask[REG_CLASS_GPR];
            }
        }

        case x86_div:
        case x86_idiv:
        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[1] = &TB_REG_EMPTY;
                // dividend operand (might be an address)
                ins[2] = n->inputs[2] ? rm : &TB_REG_EMPTY;
                ins[3] = n->inputs[3] ? rm : &TB_REG_EMPTY;

                assert(n->inputs[2]);
                if (n->inputs[2]->type == TB_MACH_FRAME_PTR || n->inputs[2]->type == TB_MACH_SYMBOL) {
                    ins[2] = &TB_REG_EMPTY;
                }

                // divisor (goes into RDX:RAX but we just fill RDX with a zero or sign extension)
                ins[4] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);
                ins[5] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
            }
            return &TB_REG_EMPTY;
        }

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR: {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[0] = ins[1] = &TB_REG_EMPTY;
                ins[2] = ins[3] = rm;
            }
            return &TB_REG_EMPTY;
        }

        case TB_MUL:
        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) { ins[1] = ins[2] = rm; }
            return rm;
        }

        case TB_SHL: case TB_SHR: case TB_ROL: case TB_ROR: case TB_SAR:
        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[1] = rm;
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);
            }
            return rm;
        }

        {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
                ins[1] = rm;
            }
            return rm;
        }

        case x86_setcc: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_FLAGS]; }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case x86_cmovcc:
        {
            int cc = TB_NODE_GET_EXTRA_T(n, X86Cmov)->cc;
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[1] = rm;
                ins[2] = n->inputs[2] ? rm : &TB_REG_EMPTY;
                ins[3] = rm;
                ins[4] = rm;
            }
            return rm;
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

        case TB_NEVER_BRANCH:
        return &TB_REG_EMPTY;

        case TB_BRANCH:
        case TB_AFFINE_LATCH:
        {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_FLAGS]; }
            return &TB_REG_EMPTY;
        }

        case TB_RETURN:
        {
            if (ins) {
                static int ret_gprs[2] = { RAX, RDX };

                ins[1] = &TB_REG_EMPTY; // mem
                ins[2] = &TB_REG_EMPTY; // rpc

                TB_FunctionPrototype* proto = ctx->f->prototype;
                assert(proto->return_count <= 2 && "At most 2 return values :(");

                FOR_N(i, 3, 3 + proto->return_count) {
                    TB_Node* in = n->inputs[i];
                    TB_DataType dt = in->dt;
                    if (TB_IS_FLOAT_TYPE(dt)) {
                        ins[i] = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << (i-3));
                    } else {
                        ins[i] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << ret_gprs[i-3]);
                    }
                }

                uint32_t callee_saved_gpr = ~param_descs[ctx->abi_index].caller_saved_gprs;
                callee_saved_gpr &= ~(1u << RSP);
                if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
                    callee_saved_gpr &= ~(1 << RBP);
                }

                size_t j = 3 + proto->return_count;
                FOR_N(i, 0, ctx->num_regs[REG_CLASS_GPR]) {
                    if ((callee_saved_gpr >> i) & 1) {
                        ins[j++] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << i);
                    }
                }

                FOR_N(i, param_descs[ctx->abi_index].caller_saved_xmms, ctx->num_regs[REG_CLASS_XMM]) {
                    ins[j++] = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << i);
                }
            }
            return &TB_REG_EMPTY;
        }

        case x86_static_call:
        case x86_call:
        {
            if (ins) {
                const struct ParamDesc* abi = &param_descs[ctx->abi_index];

                int abi_index = ctx->abi_index;
                int gprs_used = 0, xmms_used = 0;

                ins[1] = &TB_REG_EMPTY;
                ins[2] = n->type == x86_static_call ? &TB_REG_EMPTY : ctx->normie_mask[REG_CLASS_GPR];

                FOR_N(i, 3, n->input_count) {
                    int param_num = i - 3;

                    // on win64 we always have the XMMs and GPRs used match the param_num
                    // so if XMM2 is used, it's always the 3rd parameter.
                    if (abi_index == 0) { xmms_used = gprs_used = param_num; }

                    if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) {
                        if (xmms_used < abi->xmm_count) {
                            ins[i] = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << xmms_used);
                            xmms_used += 1;
                        } else {
                            tb_todo();
                        }
                    } else {
                        assert(n->inputs[i]->dt.type == TB_TAG_INT || n->inputs[i]->dt.type == TB_TAG_PTR);
                        if (gprs_used < abi->gpr_count) {
                            ins[i] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << abi->gprs[gprs_used]);
                            gprs_used += 1;
                        } else {
                            ins[i] = intern_regmask(ctx, REG_CLASS_STK, false, param_num);
                        }
                    }
                }

                size_t j = n->input_count;
                X86Call* op_extra = TB_NODE_GET_EXTRA(n);
                for (uint64_t bits = op_extra->clobber_gpr, k = 0; bits; bits >>= 1, k++) {
                    if (bits & 1) { ins[j++] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << k); }
                }

                for (uint64_t bits = op_extra->clobber_xmm, k = 0; bits; bits >>= 1, k++) {
                    if (bits & 1) { ins[j++] = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << k); }
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

static int op_reg_at(Ctx* ctx, TB_Node* n, int class) {
    assert(ctx->vreg_map[n->gvn] > 0);
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    assert(vreg->assigned >= 0);
    assert(vreg->class == class);
    return vreg->assigned;
}
static int op_gpr_at(Ctx* ctx, TB_Node* n) { return op_reg_at(ctx, n, REG_CLASS_GPR); }
static int op_xmm_at(Ctx* ctx, TB_Node* n) { return op_reg_at(ctx, n, REG_CLASS_XMM); }

static int stk_offset(Ctx* ctx, int reg) {
    if (reg >= STACK_BASE_REG_NAMES) {
        int pos = (reg-STACK_BASE_REG_NAMES)*8;
        return ctx->stack_usage - (8 + pos);
    } else {
        return reg*8;
    }
}

static bool is_flags_vreg(Ctx* ctx, TB_Node* n) {
    return ctx->vregs[ctx->vreg_map[n->gvn]].class == REG_CLASS_FLAGS;
}

static Val op_at(Ctx* ctx, TB_Node* n) {
    assert(ctx->vreg_map[n->gvn] > 0);
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    if (vreg->class == REG_CLASS_STK) {
        return val_stack(stk_offset(ctx, vreg->assigned));
    } else {
        assert(vreg->assigned >= 0);
        return (Val) { .type = vreg->class == REG_CLASS_XMM ? VAL_XMM : VAL_GPR, .reg = vreg->assigned };
    }
}

static void emit_goto(Ctx* ctx, TB_CGEmitter* e, MachineBB* succ) {
    if (ctx->fallthrough != succ->id) {
        EMIT1(e, 0xE9); EMIT4(e, 0);
        tb_emit_rel32(e, &e->labels[succ->id], GET_CODE_POS(e) - 4);
    }
}

static Val parse_cisc_operand(Ctx* restrict ctx, TB_Node* n, Val* rhs, X86MemOp* op) {
    if (rhs) {
        if (n->type >= x86_addimm && n->type <= x86_rorimm) {
            *rhs = (Val){ VAL_IMM, .imm = op->imm };
        } else if (n->inputs[4]) {
            *rhs = op_at(ctx, n->inputs[4]);
        } else {
            *rhs = (Val){ 0 };
        }
    }

    Val rm = { 0 };
    if (op->mode == MODE_LD || op->mode == MODE_ST) {
        rm.type  = VAL_MEM;
        rm.imm   = op->disp;
        rm.scale = op->scale;
        if (n->inputs[2]->type == TB_MACH_SYMBOL) {
            assert(n->inputs[3] == NULL); // RIP-relative can't have
            rm.type = VAL_GLOBAL;
            rm.symbol = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeMachSymbol)->sym;
        } else if (n->inputs[2]->type == TB_MACH_FRAME_PTR) {
            rm.reg = RSP;
            rm.imm += ctx->stack_usage;
        } else {
            rm.reg = op_gpr_at(ctx, n->inputs[2]);
        }
        if (n->inputs[3]) {
            rm.index = op_gpr_at(ctx, n->inputs[3]);
        } else {
            rm.index = -1;
        }
    } else {
        VReg* vreg = &ctx->vregs[ctx->vreg_map[n->inputs[2]->gvn]];
        assert(vreg->assigned >= 0);
        if (vreg->class == REG_CLASS_GPR) {
            rm.type = VAL_GPR;
        } else if (vreg->class == REG_CLASS_XMM) {
            rm.type = VAL_XMM;
        }
        rm.reg = vreg->assigned;
    }
    return rm;
}

static void node_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n, VReg* vreg) {
    switch (n->type) {
        // some ops don't do shit lmao
        case TB_PHI:
        case TB_REGION:
        case TB_AFFINE_LOOP:
        case TB_NATURAL_LOOP:
        case TB_PROJ:
        case TB_BRANCH_PROJ:
        case TB_MACH_PROJ:
        case TB_LOCAL:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_MACH_SYMBOL:
        case TB_MACH_FRAME_PTR:
        break;

        case TB_NEVER_BRANCH: {
            TB_Node* proj0 = USERN(proj_with_index(n, 0));
            TB_Node* succ_n = cfg_next_bb_after_cproj(proj0);
            int succ = node_to_bb(ctx, succ_n)->id;
            if (ctx->fallthrough != succ) {
                __(JMP, TB_X86_QWORD, Vlbl(succ));
            }
            break;
        }

        case TB_AFFINE_LATCH:
        case TB_BRANCH: {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            // the arena on the function should also be available at this time, we're
            // in the TB_Passes
            TB_Arena* arena = ctx->f->arena;
            TB_ArenaSavepoint sp = tb_arena_save(arena);
            int* succ = tb_arena_alloc(arena, br->succ_count * sizeof(int));

            // fill successors
            bool has_default = false;
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_BRANCH_PROJ) {
                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                    TB_Node* succ_n = cfg_next_bb_after_cproj(USERN(u));

                    if (index == 0) {
                        has_default = !cfg_is_unreachable(succ_n);
                    }

                    MachineBB* mbb = node_to_bb(ctx, succ_n);
                    succ[index] = mbb->id;
                }
            }

            TB_NodeBranchProj* if_br = cfg_if_branch(n);
            if (if_br) {
                Cond cc = if_br->key;
                if (ctx->fallthrough == succ[0]) {
                    // if flipping avoids a jmp, do that
                    cc ^= 1;
                    SWAP(int, succ[0], succ[1]);
                }

                __(JO+cc, TB_X86_QWORD, Vlbl(succ[0]));
                if (ctx->fallthrough != succ[1]) {
                    __(JMP, TB_X86_QWORD, Vlbl(succ[1]));
                }
            } else {
                tb_todo();
            }
            break;
        }

        case TB_SYMBOL: {
            TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;

            GPR dst = op_gpr_at(ctx, n);
            __(LEA, TB_X86_QWORD, Vgpr(dst), Vsym(sym, 0));
            break;
        }

        case TB_ICONST: {
            uint64_t x = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;
            uint32_t hi = x >> 32ull;

            TB_X86_DataType dt = legalize_int2(n->dt);
            GPR dst = op_gpr_at(ctx, n);
            if (x == 0) {
                if (dt != TB_X86_QWORD) { dt = TB_X86_DWORD; }
                __(XOR, dt, Vgpr(dst), Vgpr(dst));
            } else if (hi == 0 || dt == TB_X86_QWORD) {
                __(MOVABS, dt, Vgpr(dst), Vabs(x));
            } else {
                __(MOV, dt, Vgpr(dst), Vimm(x));
            }
            break;
        }

        case TB_MACH_MOVE:
        case TB_MACH_COPY: {
            if (is_flags_vreg(ctx, n)) {
                // FLAGS -> FLAGS is a nop since there's only one flags reg
                if (!is_flags_vreg(ctx, n->inputs[1])) {
                    COMMENT("reload FLAGS");

                    // GPR -> FLAGS
                    int reg = op_gpr_at(ctx, n->inputs[1]);
                    EMIT1(e, 0x48 + (reg >= 8)); // REX.W (optional B)
                    EMIT1(e, 0x50 + reg);        // pop r64
                    EMIT1(e, 0x9D);              // popf
                }
            } else if (is_flags_vreg(ctx, n->inputs[1])) {
                COMMENT("spilled FLAGS");

                // FLAGS -> GPR
                int reg = op_gpr_at(ctx, n);
                EMIT1(e, 0x9C);              // pushf
                EMIT1(e, 0x48 + (reg >= 8)); // REX.W (optional B)
                EMIT1(e, 0x58 + reg);        // pop r64
            } else {
                TB_X86_DataType dt = legalize(n->dt);

                Val dst = op_at(ctx, n);
                Val src = op_at(ctx, n->inputs[1]);
                if (!is_value_match(&dst, &src)) {
                    COMMENT("%%%u = copy(%%%u)", n->gvn, n->inputs[1]->gvn);

                    if (dst.type == VAL_GPR && src.type == VAL_XMM) {
                        __(MOV_I2F, dt, &dst, &src);
                    } else if (dst.type == VAL_XMM && src.type == VAL_GPR) {
                        TB_X86_DataType src_dt = legalize(n->inputs[1]->dt);
                        __(MOV_F2I, src_dt, &dst, &src);
                    } else {
                        int op = dt < TB_X86_F32x1 ? MOV : FP_MOV;
                        __(op, dt, &dst, &src);
                    }
                }
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
            if (stack_usage) {
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
            if ((ctx->features.gen & TB_FEATURE_FRAME_PTR) && stack_usage > 0) {
                EMIT1(e, 0x58 + RBP);
            }
            EMIT1(e, 0xC3);
            ctx->epilogue_length = e->count - pos;
            break;
        }

        case x86_ucomi: {
            Val b, a = parse_cisc_operand(ctx, n, &b, TB_NODE_GET_EXTRA(n));
            inst2sse(e, FP_UCOMI, &a, &b, legalize_float(n->inputs[4]->dt));
            break;
        }

        case x86_vzero: {
            Val dst = op_at(ctx, n);
            __(FP_XOR, TB_X86_F32x4, &dst, &dst); // xorps
            break;
        }

        case TB_FLOAT_EXT: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            TB_X86_DataType src_dt = legalize_float(n->inputs[1]->dt);
            __(FP_CVT, src_dt, &dst, &src);
            break;
        }

        case x86_idiv: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            // cqo/cdq (sign extend RAX into RDX)
            if (n->dt.data > 32) { EMIT1(e, 0x48); }
            EMIT1(e, 0x99);
            // idiv
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int2(op->dt);
            __(IDIV, dt, &rhs);
            break;
        }

        case x86_div: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            // xor edx, edx
            EMIT1(e, 0x31);
            EMIT1(e, 0xD2);
            // div
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int2(op->dt);
            __(DIV, dt, &rhs);
            break;
        }

        case TB_FLOAT_TRUNC: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            __(FP_CVT, legalize_float(n->inputs[1]->dt), &dst, &src);
            break;
        }

        case TB_UINT2FLOAT:
        case TB_TAG_INT2FLOAT: {
            TB_DataType src_dt = n->inputs[1]->dt;
            assert(src_dt.type == TB_TAG_INT);

            // it's either 32bit or 64bit conversion
            //   CVTSI2SS r/m32, xmm1
            //   CVTSI2SD r/m64, xmm1
            bool is_64bit = src_dt.data > 32;

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

        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        {
            const static int ops[] = {
                FP_MOV, FP_ADD, FP_MUL, FP_SUB, FP_DIV, FP_MIN, FP_MAX, FP_XOR, FP_UCOMI
            };

            TB_X86_DataType dt;
            if (n->dt.type == TB_TAG_MEMORY) {
                dt = legalize_float(n->inputs[4]->dt);
            } else {
                dt = legalize_float(n->dt);
            }

            X86MemOp* op = TB_NODE_GET_EXTRA(n);

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            int op_type = ops[n->type - x86_vmov];
            if (op->mode == MODE_ST) {
                __(op_type, dt, &rm, &rx);
            } else {
                Val dst = op_at(ctx, n);
                if (rx.type != VAL_NONE && !is_value_match(&dst, &rx)) {
                    __(FP_MOV, dt, &dst, &rx);
                }
                __(op_type, dt, &dst, &rm);
            }
            break;
        }

        case x86_lea:
        case x86_add: case x86_or:  case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test:
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            const static int ops[] = {
                // binop
                ADD, OR, AND, SUB, XOR, CMP, MOV, TEST,
                // binop with immediates
                ADD, OR, AND, SUB, XOR, CMP, MOV, TEST,
                // shifts
                SHL, SHR, SAR, ROL, ROR,
                // misc (except for imul because it's weird)
                LEA,
            };

            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt;
            if (n->type == x86_test || n->type == x86_cmp || n->dt.type == TB_TAG_MEMORY) {
                dt = legalize_int2(op->dt);
            } else if (n->type == x86_cmpimm) {
                dt = legalize_int2(n->inputs[2]->dt);
            } else {
                dt = legalize_int2(n->dt);
            }

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            int op_type = ops[n->type - x86_add];
            if (op_type == CMP || op_type == TEST) {
                __(op_type, dt, &rm, &rx);
            } else if (op->mode == MODE_ST) {
                __(op_type, dt, &rm, &rx);
            } else if (n->type >= x86_addimm && n->type <= x86_rorimm) {
                assert(n->type != x86_lea);
                Val dst = op_at(ctx, n);
                if (!is_value_match(&dst, &rm)) {
                    __(MOV, dt, &dst, &rm);
                }
                __(op_type, dt, &rm, &rx);
            } else {
                Val dst = op_at(ctx, n);
                if (rx.type != VAL_NONE) {
                    assert(n->type != x86_lea);
                    if (!is_value_match(&dst, &rx)) {
                        __(MOV, dt, &dst, &rx);
                    }
                }
                __(op_type, dt, &dst, &rm);
            }
            break;
        }

        case x86_imulimm: {
            TB_X86_DataType dt = legalize_int2(n->dt);
            X86MemOp* op = TB_NODE_GET_EXTRA(n);

            Val dst = op_at(ctx, n);
            Val lhs = op_at(ctx, n->inputs[1]);

            // hacky but the ternary multiply is just a 2op modrm like normal, except with an
            // extra immediate afterwards.
            __(IMUL3, dt, &dst, &lhs);
            if (dt == TB_X86_WORD) { EMIT2(e, op->imm); }
            else { EMIT4(e, op->imm); }
            break;
        }

        case TB_MUL: {
            TB_X86_DataType dt = legalize_int2(n->dt);

            Val dst  = op_at(ctx, n);
            Val lhs  = op_at(ctx, n->inputs[1]);
            Val rhs  = op_at(ctx, n->inputs[2]);

            if (!is_value_match(&dst, &lhs)) {
                __(MOV, dt, &dst, &lhs);
            }
            __(IMUL, dt, &dst, &rhs);
            break;
        }

        case x86_setcc: {
            int cc = TB_NODE_GET_EXTRA_T(n, X86Cmov)->cc;
            Val dst  = op_at(ctx, n);

            __(SETO + cc, TB_X86_BYTE, &dst);
            break;
        }

        case x86_cmovcc: {
            TB_X86_DataType dt = legalize_int2(n->dt);
            int cc = TB_NODE_GET_EXTRA_T(n, X86Cmov)->cc;

            Val dst  = op_at(ctx, n);
            Val lhs  = op_at(ctx, n->inputs[3]);
            if (!is_value_match(&dst, &lhs)) {
                __(MOV, dt, &dst, &lhs);
            }

            Val cmp1 = op_at(ctx, n->inputs[1]);
            if (n->inputs[2]) {
                Val cmp2 = op_at(ctx, n->inputs[2]);
                __(CMP, dt, &cmp1, &cmp2);
            } else {
                __(CMP, dt, &cmp1, Vimm(0));
            }

            Val rhs = op_at(ctx, n->inputs[4]);
            __(CMOVO+cc, dt, &dst, &rhs);
            break;
        }

        case TB_SHL:
        case TB_SHR:
        case TB_ROL:
        case TB_ROR:
        case TB_SAR: {
            TB_X86_DataType dt = legalize_int2(n->dt);

            Val dst = op_at(ctx, n);
            Val lhs = op_at(ctx, n->inputs[1]);
            if (!is_value_match(&dst, &lhs)) {
                __(MOV, dt, &dst, &lhs);
            }

            InstType op;
            switch (n->type) {
                case TB_SHL: op = SHL; break;
                case TB_SHR: op = SHR; break;
                case TB_ROL: op = ROL; break;
                case TB_ROR: op = ROR; break;
                case TB_SAR: op = SAR; break;
                default: tb_todo();
            }

            __(op, dt, &dst, Vgpr(RCX));
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

            TB_X86_DataType dt = legalize_int2(n->inputs[2]->dt);
            if (n->type == x86_movsx8 || n->type == x86_movsx16) {
                dt = TB_X86_DWORD;
            }

            Val dst = op_at(ctx, n);
            Val src = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));

            int op_type = ops[n->type - x86_movsx8];
            __(op_type, dt, &dst, &src);
            break;
        }

        case TB_MEMSET: {
            EMIT1(e, 0xF3);
            EMIT1(e, 0xAA);
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
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR: {
            // tbl is for normal locking operations which don't care about the result,
            // fetch will need to worry about it which means slightly different codegen.
            const static int tbl[]       = { MOV,  ADD,  SUB,  AND, XOR, OR };
            const static int fetch_tbl[] = { XCHG, XADD, XADD, 0,   0,   0  };

            TB_Node* dproj = USERN(proj_with_index(n, 1));

            TB_NodeAtomic* a = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt = legalize_int2(dproj->dt);

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

        case x86_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);
            Val target = op_at(ctx, n->inputs[2]);
            __(CALL, TB_X86_QWORD, &target);
            break;
        }

        case x86_static_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);

            // CALL rel32
            EMIT1(e, 0xE8);
            EMIT4(e, 0);
            tb_emit_symbol_patch(e->output, op_extra->sym, e->count - 4);
            break;
        }

        default:
        tb_todo();
        break;
    }
}

static bool flags_producer(TB_Node* n) {
    return n->type == x86_ucomi
        || n->type == x86_cmp || n->type == x86_cmpimm
        || n->type == x86_test || n->type == x86_testimm;
}

// don't care about functional units on x86
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n) { return 1; }
static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) {
    switch (n->type) {
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_movzx8: case x86_movzx16: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            return 2 + (op->mode == MODE_LD ? 3 : 0);
        }

        // doesn't atually load shit so it's cheaper than the other similar ops
        case x86_lea:
        return 2;

        // load/store ops should count as a bit slower
        case x86_add: case x86_or:  case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);

            if (end && end->type == TB_AFFINE_LATCH) {
                TB_Node* cond = end->inputs[1];
                if (flags_producer(cond) && cond->inputs[2] == n) {
                    return 0;
                }
            }

            int clk;
            switch (n->type) {
                case x86_vdiv:    clk = 11; break;
                case x86_imulimm: clk = 3;  break;
                default:          clk = 1;  break;
            }

            if (op->mode == MODE_LD) clk += 3;
            // every store op except for x86_mov will do both a ld(3 clks) + st(4 clks)
            if (op->mode == MODE_ST) clk += (n->type != x86_mov ? 7 : 4);
            return clk;
        }

        case TB_MEMSET: case TB_MEMCPY:
        return 20;

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        return 20;

        case TB_MACH_MOVE:
        return 0; // cheapest op so that it tries to schedule it later

        default: return 1;
    }
}

#if 0
#define OUT1(m) (dst->outs[0]->dt = n->dt, dst->outs[0]->mask = (m))
static void isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n) {
    switch (n->type) {
        // no inputs
        case TB_REGION:
        case TB_NATURAL_LOOP:
        case TB_AFFINE_LOOP:
        case TB_ROOT:
        case TB_TRAP:
        case TB_CALLGRAPH:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_UNREACHABLE:
        case TB_DEBUGBREAK:
        case TB_ICONST:
        case TB_F32CONST:
        case TB_F64CONST:
        case TB_POISON:
        break;

        case TB_SYMBOL: {
            TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
            if (is_tls_symbol(sym)) {
                // on windows we'll need one temporary, linux needs none
                if (ctx->abi_index == 0) {
                    dst->ins = tb_arena_alloc(tmp_arena, 1 * sizeof(TileInput));
                    dst->in_count = 1;
                    dst->ins[0].mask = ctx->normie_mask[REG_CLASS_GPR];
                    dst->ins[0].src = NULL;
                } else {
                    dst->ins = NULL;
                    dst->in_count = 0;
                }
            }
            break;
        }
        case TB_WRITE:
        case TB_STORE: {
            TileInput* ins = isel_addr(ctx, dst, n, n->inputs[2], 1);
            ins[0].src = get_interval(ctx, n->inputs[3], 0);
            ins[0].mask = normie_mask(ctx, n->inputs[3]->dt);
            break;
        }

        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD: {
            dst->ins = tb_arena_alloc(tmp_arena, 3 * sizeof(TileInput));
            dst->in_count = 3;
            dst->ins[0].mask = REGMASK(GPR, 1 << RAX);
            dst->ins[1].mask = ctx->normie_mask[REG_CLASS_GPR];
            dst->ins[2].mask = REGMASK(GPR, 1 << RDX);
            dst->ins[0].src = get_interval(ctx, n->inputs[1], 0);
            dst->ins[1].src = get_interval(ctx, n->inputs[2], 0);
            dst->ins[2].src = NULL;

            if (n->type == TB_UDIV || n->type == TB_SDIV) {
                OUT1(REGMASK(GPR, 1 << RAX));
            } else {
                OUT1(REGMASK(GPR, 1 << RDX));
            }
            break;
        }

        case TB_BRANCH: {
            TB_Node* cmp = n->inputs[1];
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            AuxBranch* aux = NULL;
            int ins = 1, tmps = 0;
            if (br->succ_count > 2) {
                // try for jump tables or if-chains
                //
                // check if there's at most only one space between entries
                int64_t last = br->keys[0].key;
                int64_t min = last, max = last;

                double dist_avg = 0;
                double inv_succ_count = 1.0 / (br->succ_count - 2);

                bool large_num = false;
                FOR_N(i, 2, br->succ_count) {
                    int64_t key = br->keys[i - 1].key;
                    if (!fits_into_int32(key)) {
                        large_num = true;
                    }

                    min = (min > key) ? key : min;
                    max = (max > key) ? max : key;

                    dist_avg += (key - last) * inv_succ_count;
                    last = key;
                }

                // if there's no default case we can skew heuristics around the lack of range check
                bool has_default = false;
                FOR_USERS(u, n) {
                    if (u->n->type != TB_PROJ) continue;
                    int index = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
                    if (index == 0) {
                        has_default = cfg_next_control(u->n)->type != TB_UNREACHABLE;
                        break;
                    }
                }

                int64_t range = (max - min) + 1;

                // if we do if-else chains we'll do 1+2c ops (c is the number of cases).
                int64_t if_chain_cost  = 1 + 2*range;
                // if we do jump table it's 6 ops + a table that's got [max-min] entries but cost
                // wise the issue is slots which are missed (go to fallthru).
                int64_t jmp_table_cost = has_default ? 6 : 4;
                jmp_table_cost += (range - (range / dist_avg));

                aux = tb_arena_alloc(tmp_arena, sizeof(AuxBranch));
                aux->min = min;
                aux->max = max;
                aux->if_chain = if_chain_cost < jmp_table_cost;

                if (aux->if_chain) {
                    // large numbers require a temporary to store the immediate
                    tmps += large_num;
                } else {
                    // we need tmp for the key (either offset or casted)
                    tmps += 3;
                }
            } else {
                if (cmp->type >= TB_CMP_EQ && cmp->type <= TB_CMP_FLE) {
                    TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n->inputs[1], TB_NodeCompare)->cmp_dt;
                    dst->flags |= TILE_FOLDED_CMP;

                    int32_t x;
                    if (!try_for_imm32(cmp_dt.type == TB_TAG_PTR ? 64 : cmp_dt.data, cmp->inputs[2], &x)) {
                        ins += 1;
                    } else {
                        dst->flags |= TILE_HAS_IMM;
                    }
                }
            }

            dst->ins = tb_arena_alloc(tmp_arena, (ins+tmps) * sizeof(TileInput));
            dst->in_count = ins+tmps;
            dst->aux = aux;

            if (dst->flags & TILE_FOLDED_CMP) {
                TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cmp, TB_NodeCompare)->cmp_dt;

                RegMask rm = normie_mask(ctx, cmp_dt);
                dst->ins[0].src = get_interval(ctx, cmp->inputs[1], 0);
                dst->ins[0].mask = rm;

                if ((dst->flags & TILE_HAS_IMM) == 0) {
                    dst->ins[1].src = get_interval(ctx, cmp->inputs[2], 0);
                    dst->ins[1].mask = rm;
                }
            } else {
                dst->ins[0].src = get_interval(ctx, cmp, 0);
                dst->ins[0].mask = normie_mask(ctx, cmp->dt);
            }

            FOR_N(i, ins, ins+tmps) {
                dst->ins[i].src = NULL;
                dst->ins[i].mask = ctx->normie_mask[REG_CLASS_GPR];
            }

            break;
        }

        case TB_SYSCALL: {
            const struct ParamDesc* abi = &param_descs[2];
            uint32_t caller_saved_gprs = abi->caller_saved_gprs;

            int param_count = n->input_count - 3;
            if (n->type == TB_TAILCALL) {
                caller_saved_gprs &= ~(1u << RAX);
            }

            FOR_N(i, 0, param_count > 4 ? 4 : param_count) {
                caller_saved_gprs &= ~(1u << abi->gprs[i]);
            }

            size_t clobber_count = tb_popcount(caller_saved_gprs);
            size_t input_count = (n->input_count - 2) + clobber_count;

            // SYSCALL
            TileInput* ins = dst->ins = tb_arena_alloc(tmp_arena, input_count * sizeof(TileInput));
            dst->in_count = input_count;

            ins[0].src = get_interval(ctx, n->inputs[2], 0);
            ins[0].mask = REGMASK(GPR, 1u << RAX);

            assert(param_count < abi->gpr_count);
            FOR_N(i, 0, param_count) {
                ins[i].src = get_interval(ctx, n->inputs[i + 3], 0);
                ins[i].mask = REGMASK(GPR, 1u << abi->gprs[i]);
            }

            int j = param_count;
            FOR_N(i, 0, ctx->num_regs[REG_CLASS_GPR]) {
                if (caller_saved_gprs & (1u << i)) {
                    ins[j].src = NULL;
                    ins[j].mask = REGMASK(GPR, 1u << i);
                    j++;
                }
            }
            break;
        }

        case TB_CALL:
        case TB_TAILCALL: {
            const struct ParamDesc* abi = &param_descs[ctx->abi_index];
            uint32_t caller_saved_gprs = abi->caller_saved_gprs;
            uint32_t caller_saved_xmms = ~0ull >> (64 - abi->caller_saved_xmms);

            int param_count = n->input_count - 3;
            if (ctx->num_regs[0] < param_count) {
                ctx->num_regs[0] = param_count;
                ctx->call_usage = param_count;
            }

            if (n->type == TB_TAILCALL) {
                caller_saved_gprs &= ~(1u << RAX);
            }

            FOR_N(i, 0, param_count > 4 ? 4 : param_count) {
                caller_saved_gprs &= ~(1u << abi->gprs[i]);
            }

            size_t clobber_count = tb_popcount(caller_saved_gprs);
            size_t input_start = n->inputs[2]->type == TB_SYMBOL ? 3 : 2;
            size_t input_count = (n->input_count - input_start) + clobber_count;

            TileInput* ins;
            if (n->inputs[2]->type == TB_SYMBOL) {
                // CALL symbol
                ins = dst->ins = tb_arena_alloc(tmp_arena, input_count * sizeof(TileInput));
                dst->in_count = input_count;
            } else {
                // CALL r/m
                ins = dst->ins = tb_arena_alloc(tmp_arena, input_count * sizeof(TileInput));
                dst->in_count = input_count;

                ins[0].src = get_interval(ctx, n->inputs[2], 0);
                if (n->type == TB_TAILCALL) {
                    ins[0].mask = REGMASK(GPR, 1u << RAX);
                } else {
                    ins[0].mask = ctx->normie_mask[REG_CLASS_GPR];
                }
                ins += 1;
            }

            FOR_N(i, 0, param_count) {
                ins[i].src = get_interval(ctx, n->inputs[i + 3], 0);

                if (i < abi->gpr_count) {
                    if (TB_IS_FLOAT_TYPE(n->inputs[i + 3]->dt)) {
                        ins[i].mask = REGMASK(XMM, 1u << i);
                    } else {
                        ins[i].mask = REGMASK(GPR, 1u << abi->gprs[i]);
                    }
                } else {
                    // stack slots go into [RSP + 8i]
                    ins[i].mask = REGMASK(STK, i);
                }
            }

            int j = param_count;
            FOR_N(i, 0, 16) {
                if (caller_saved_gprs & (1u << i)) {
                    ins[j].src = NULL;
                    ins[j].mask = REGMASK(GPR, 1u << i);
                    j++;
                }
            }

            assert(j == input_count - (n->inputs[2]->type != TB_SYMBOL));
            return;
        }

        default:
        tb_todo();
        break;
    }

    if (dst->out_count == 1) {
        dst->outs[0]->dt = n->dt;
        dst->outs[0]->mask = normie_mask(ctx, n->dt);
    } else if (dst->out_count != 0) {
        tb_todo();
    }
}

static Val op_at(Ctx* ctx, LiveInterval* l) {
    if (l->class == REG_CLASS_STK) {
        return val_stack(stk_offset(ctx, l->assigned));
    } else {
        assert(l->assigned >= 0);
        return (Val) { .type = l->class == REG_CLASS_XMM ? VAL_XMM : VAL_GPR, .reg = l->assigned };
    }
}

static GPR op_gpr_at(LiveInterval* l) {
    assert(l->class == REG_CLASS_GPR);
    return l->assigned;
}
#endif

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* root) {
    TB_FunctionPrototype* proto = ctx->f->prototype;

    size_t stack_usage = 0;
    if (ctx->num_spills != 0 || ctx->call_usage != 0) {
        stack_usage = (ctx->num_spills+ctx->call_usage) * 8;

        // Align stack usage to 16bytes + header to accommodate for the RIP being pushed
        // by CALL (and frameptr if applies)
        stack_usage = align_up(stack_usage + ctx->stack_header, 16) + (16 - ctx->stack_header);

        #if 0
        printf("FUNC %s:\n", ctx->f->super.name);
        FOR_N(i, 0, proto->param_count) {
            printf("  [FP + %2lld]           PARAM\n", ctx->stack_header + i*8);
        }
        printf("  [FP + %2d]           RPC\n", ctx->stack_header - 8);
        if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
            printf("  [FP + %2d]             saved RBP\n", 0);
        }
        printf("==============\n");
        FOR_N(i, 0, ctx->num_spills) {
            printf("  [FP - %2lld] [SP + %2lld] SPILL\n", 8 + i*8, stack_usage - (8 + i*8));
        }
        FOR_REV_N(i, 0, ctx->call_usage) {
            printf("            [SP + %2lld] CALLEE PARAM\n", i*8);
        }
        #endif
    }
    ctx->stack_usage = stack_usage;

    FOR_USERS(u, root) {
        TB_Node* n = USERN(u);
        if (n->type != TB_LOCAL) continue;
        TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
        if (l->type == NULL) continue;
        TB_StackSlot s = {
            .name = l->name,
            .type = l->type,
            .storage = { ctx->stack_usage + l->stack_pos },
        };
        dyn_array_put(ctx->debug_stack_slots, s);
    }

    // save frame pointer (if applies)
    if ((ctx->features.gen & TB_FEATURE_FRAME_PTR) && stack_usage > 0) {
        EMIT1(e, 0x50 + RBP);

        // mov rbp, rsp
        EMIT1(e, rex(true, RSP, RBP, 0));
        EMIT1(e, 0x89);
        EMIT1(e, mod_rx_rm(MOD_DIRECT, RSP, RBP));
    }

    // inserts a chkstk call if we use too much stack
    if (stack_usage >= param_descs[ctx->abi_index].chkstk_limit) {
        assert(ctx->f->super.module->chkstk_extern);
        ctx->f->super.module->uses_chkstk++;

        Val sym = val_global(ctx->f->super.module->chkstk_extern, 0);
        Val rax = val_gpr(RAX);
        Val rsp = val_gpr(RSP);

        __(MOV,  TB_X86_DWORD, &rax, Vimm(stack_usage));
        __(CALL, TB_X86_QWORD, &sym);
        __(SUB,  TB_X86_QWORD, &rsp, &rax);
    } else if (stack_usage) {
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
    if (proto->has_varargs) {
        const GPR* params = param_descs[ctx->abi_index].gprs;

        // spill the rest of the parameters (assumes they're all in the GPRs)
        size_t gpr_count = param_descs[ctx->abi_index].gpr_count;
        size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;

        FOR_N(i, proto->param_count, gpr_count) {
            int dst_pos = ctx->stack_header + (i * 8);
            __(MOV, TB_X86_QWORD, Vbase(RSP, stack_usage + dst_pos), Vgpr(params[i]));
        }
    }

    ctx->prologue_length = e->count;
}

static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb) {
    tb_resolve_rel32(e, &e->labels[bb], e->count);
}

#if 0
static Cond emit_cmp(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* cmp, Tile* t, int64_t falsey) {
    Val a = op_at(ctx, t->ins[0].src);
    if (t->flags & TILE_FOLDED_CMP) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cmp, TB_NodeCompare)->cmp_dt;
        assert(cmp->type >= TB_CMP_EQ && cmp->type <= TB_CMP_FLE);
        assert(falsey == 0 || falsey == 1);

        Cond cc;
        if (TB_IS_FLOAT_TYPE(cmp_dt)) {
            Val b = op_at(ctx, t->ins[1].src);
            inst2sse(e, FP_UCOMI, &a, &b, legalize_float(cmp_dt));

            switch (cmp->type) {
                case TB_CMP_EQ:  cc = E;  break;
                case TB_CMP_NE:  cc = NE; break;
                case TB_CMP_FLT: cc = B;  break;
                case TB_CMP_FLE: cc = BE; break;
                default: tb_unreachable();
            }
        } else {
            if (t->flags & TILE_HAS_IMM) {
                assert(cmp->inputs[2]->type == TB_ICONST);
                TB_NodeInt* i = TB_NODE_GET_EXTRA(cmp->inputs[2]);

                Val b = val_imm(i->value);
                inst2(e, CMP, &a, &b, legalize_int2(cmp_dt));
            } else {
                Val b = op_at(ctx, t->ins[1].src);
                inst2(e, CMP, &a, &b, legalize_int2(cmp_dt));
            }

            switch (cmp->type) {
                case TB_CMP_EQ:  cc = E;  break;
                case TB_CMP_NE:  cc = NE; break;
                case TB_CMP_SLT: cc = L;  break;
                case TB_CMP_SLE: cc = LE; break;
                case TB_CMP_ULT: cc = B;  break;
                case TB_CMP_ULE: cc = BE; break;
                default: tb_unreachable();
            }
        }

        if (falsey == 1) { cc ^= 1; }
        return cc;
    } else {
        if (falsey == 0) {
            inst2(e, TEST, &a, &a, legalize_int2(cmp->dt));
        } else {
            assert(fits_into_int32(falsey));
            Val imm = val_imm(falsey);
            inst2(e, CMP, &a, &imm, legalize_int2(cmp->dt));
        }
        return NE;
    }
}

static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_SPILL_MOVE) {
        Val dst = op_at(ctx, t->outs[0]);
        Val src = op_at(ctx, t->ins[0].src);
        if (!is_value_match(&dst, &src)) {
            COMMENT("move v%d -> v%d", t->outs[0]->id, t->ins[0].src->id);

            TB_DataType dt = t->spill_dt;
            if (TB_IS_FLOAT_TYPE(dt)) {
                inst2sse(e, FP_MOV, &dst, &src, legalize_float(dt));
            } else {
                inst2(e, MOV, &dst, &src, legalize_int2(dt));
            }
        } else {
            COMMENT("folded move v%d -> v%d", t->outs[0]->id, t->ins[0].src->id);
        }
    } else if (t->tag == TILE_GOTO) {
        MachineBB* mbb = node_to_bb(ctx, t->succ);
        if (ctx->fallthrough != mbb->id) {
            EMIT1(e, 0xE9); EMIT4(e, 0);
            tb_emit_rel32(e, &e->labels[mbb->id], GET_CODE_POS(e) - 4);
        }
    } else {
        TB_Node* n = t->n;
        switch (n->type) {
            // epilogue
            case TB_RETURN: {
                size_t pos = e->count;
                emit_epilogue(ctx, e, ctx->stack_usage);
                EMIT1(e, 0xC3);
                ctx->epilogue_length = e->count - pos;
                break;
            }
            case TB_TRUNCATE: {
                if (TB_IS_FLOAT_TYPE(n->dt)) {
                    Val dst = op_at(ctx, t->outs[0]);
                    Val lhs = op_at(ctx, t->ins[0].src);
                    inst2sse(e, FP_CVT, &dst, &lhs, legalize_float(n->inputs[1]->dt));
                } else {
                    TB_X86_DataType dt = legalize_int2(n->dt);

                    Val dst = op_at(ctx, t->outs[0]);
                    Val lhs = op_at(ctx, t->ins[0].src);
                    if (!is_value_match(&dst, &lhs)) {
                        inst2(e, MOV, &dst, &lhs, dt);
                    }
                }
                break;
            }
            case TB_FLOAT_EXT: {
                TB_X86_DataType src_dt = legalize_float(n->inputs[1]->dt);
                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);
                inst2sse(e, FP_CVT, &dst, &lhs, src_dt);
                break;
            }
            case TB_UINT2FLOAT:
            case TB_TAG_INT2FLOAT: {
                TB_DataType src_dt = n->inputs[1]->dt;
                assert(src_dt.type == TB_TAG_INT);

                // it's either 32bit or 64bit conversion
                //   CVTSI2SS r/m32, xmm1
                //   CVTSI2SD r/m64, xmm1
                bool is_64bit = src_dt.data > 32;

                TB_X86_DataType dt = legalize_float(n->dt);
                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);
                inst2sse(e, is_64bit ? FP_CVT64 : FP_CVT32, &dst, &lhs, dt);
                break;
            }

            case TB_FLOAT2INT:
            case TB_FLOAT2UINT: {
                TB_DataType src_dt = n->inputs[1]->dt;
                assert(src_dt.type == TB_FLOAT);

                // it's either 32bit or 64bit conversion
                // F3 0F 2C /r            CVTTSS2SI xmm1, r/m32
                // F3 REX.W 0F 2C /r      CVTTSS2SI xmm1, r/m64
                // F2 0F 2C /r            CVTTSD2SI xmm1, r/m32
                // F2 REX.W 0F 2C /r      CVTTSD2SI xmm1, r/m64
                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);
                inst2sse(e, FP_CVTT, &dst, &lhs, legalize_float(src_dt));
                break;
            }
            case TB_BITCAST: {
                TB_X86_DataType dst_dt = legalize_int2(n->dt);
                TB_X86_DataType src_dt = legalize_int2(n->inputs[1]->dt);

                Val dst = op_at(ctx, t->outs[0]);
                Val src = op_at(ctx, t->ins[0].src);

                if (dst_dt >= TB_X86_TYPE_BYTE && dst_dt <= TB_X86_TYPE_QWORD &&
                    src_dt >= TB_X86_TYPE_BYTE && src_dt <= TB_X86_TYPE_QWORD) {
                    if (dst_dt != src_dt || !is_value_match(&dst, &src)) {
                        inst2(e, MOV, &dst, &src, dst_dt);
                    }
                } else {
                    tb_todo();
                }
                break;
            }
            case TB_SYMBOL: {
                TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
                Val dst = op_at(ctx, t->outs[0]);

                assert(sym);
                if (is_tls_symbol(sym)) {
                    if (ctx->abi_index == 0) {
                        Val tmp = op_at(ctx, t->ins[0].src);
                        Val tls_index = val_global(ctx->module->tls_index_extern, 0);

                        // mov tmp, dword [_tls_index]
                        inst2(e, MOV, &tmp, &tls_index, TB_X86_TYPE_DWORD);
                        // mov dst, qword gs:[58h]
                        EMIT1(e, 0x65);
                        EMIT1(e, tmp.reg >= 8 ? 0x4C : 0x48);
                        EMIT1(e, 0x8B);
                        EMIT1(e, mod_rx_rm(MOD_INDIRECT, tmp.reg, RSP));
                        EMIT1(e, mod_rx_rm(SCALE_X1, RSP, RBP));
                        EMIT4(e, 0x58);
                        // mov dst, qword [dst+tmp*8]
                        Val mem = val_base_index_disp(dst.reg, tmp.reg, SCALE_X8, 0);
                        INST2(MOV, &dst, &mem, TB_X86_TYPE_QWORD);
                        // add dst, relocation
                        EMIT1(e, rex(true, 0, dst.reg, 0)), EMIT1(e, 0x81);
                        EMIT1(e, mod_rx_rm(MOD_DIRECT, 0, dst.reg));
                        EMIT4(e, 0);
                        tb_emit_symbol_patch(e->output, sym, e->count - 4);
                    } else {
                        tb_todo();
                    }
                } else {
                    Val src = val_global(sym, 0);
                    inst2(e, LEA, &dst, &src, TB_X86_TYPE_QWORD);
                }
                break;
            }
            case TB_NOT: {
                TB_X86_DataType dt = legalize_int2(n->dt);
                Val dst = op_at(ctx, t->outs[0]);
                Val src = op_at(ctx, t->ins[0].src);
                if (!is_value_match(&dst, &src)) {
                    inst2(e, MOV, &dst, &src, dt);
                }

                inst1(e, NOT, &dst, dt);
                break;
            }
            case TB_AND:
            case TB_OR:
            case TB_XOR:
            case TB_ADD:
            case TB_SUB: {
                const static InstType ops[] = { AND, OR, XOR, ADD, SUB };
                InstType op = ops[n->type - TB_AND];
                TB_X86_DataType dt = legalize_int2(n->dt);

                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);

                if (!is_value_match(&dst, &lhs)) {
                    // we'd rather do LEA addition than mov+add, but if it's add by itself it's fine
                    if (n->type == TB_ADD && (dt == TB_X86_TYPE_DWORD || dt == TB_X86_TYPE_QWORD)) {
                        if (t->flags & TILE_HAS_IMM) {
                            assert(n->inputs[2]->type == TB_ICONST);
                            TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);

                            // lea dst, [lhs + imm]
                            Val ea = val_base_disp(lhs.reg, i->value);
                            inst2(e, LEA, &dst, &ea, dt);
                            break;
                        }
                    }

                    inst2(e, MOV, &dst, &lhs, dt);
                }

                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);

                    Val rhs = val_imm(i->value);
                    inst2(e, op, &dst, &rhs, dt);
                } else {
                    Val rhs = op_at(ctx, t->ins[1].src);
                    inst2(e, op, &dst, &rhs, dt);
                }
                break;
            }
            case TB_MUL: {
                TB_X86_DataType dt = legalize_int2(n->dt);

                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);

                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);

                    inst2(e, IMUL3, &dst, &lhs, dt);
                    if (dt == TB_X86_TYPE_WORD) {
                        EMIT2(e, i->value);
                    } else {
                        EMIT4(e, i->value);
                    }
                } else {
                    if (!is_value_match(&dst, &lhs)) {
                        inst2(e, MOV, &dst, &lhs, dt);
                    }

                    Val rhs = op_at(ctx, t->ins[1].src);
                    inst2(e, IMUL, &dst, &rhs, dt);
                }
                break;
            }
            case TB_SHL:
            case TB_SHR:
            case TB_ROL:
            case TB_ROR:
            case TB_SAR: {
                TB_X86_DataType dt = legalize_int2(n->dt);

                Val dst = op_at(ctx, t->outs[0]);
                Val lhs = op_at(ctx, t->ins[0].src);
                if (!is_value_match(&dst, &lhs)) {
                    inst2(e, MOV, &dst, &lhs, dt);
                }

                InstType op;
                switch (n->type) {
                    case TB_SHL: op = SHL; break;
                    case TB_SHR: op = SHR; break;
                    case TB_ROL: op = ROL; break;
                    case TB_ROR: op = ROR; break;
                    case TB_SAR: op = SAR; break;
                    default: tb_todo();
                }

                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);

                    Val rhs = val_imm(i->value);
                    inst2(e, op, &dst, &rhs, dt);
                } else {
                    Val rcx = val_gpr(RCX);
                    inst2(e, op, &dst, &rcx, dt);
                }
                break;
            }
            case TB_UDIV:
            case TB_SDIV:
            case TB_UMOD:
            case TB_SMOD: {
                bool is_signed = (n->type == TB_SDIV || n->type == TB_SMOD);
                bool is_div    = (n->type == TB_UDIV || n->type == TB_SDIV);

                TB_DataType dt = n->dt;

                // if signed:
                //   cqo/cdq (sign extend RAX into RDX)
                // else:
                //   xor rdx, rdx
                if (is_signed) {
                    if (n->dt.data > 32) {
                        EMIT1(e, 0x48);
                    }
                    EMIT1(e, 0x99);
                } else {
                    Val rdx = val_gpr(RDX);
                    inst2(e, XOR, &rdx, &rdx, TB_X86_TYPE_DWORD);
                }

                break;
            }
            case TB_SYSCALL: {
                inst0(e, SYSCALL, TB_X86_TYPE_QWORD);
                break;
            }
            case TB_CALL:
            case TB_TAILCALL: {
                int op = CALL;
                if (n->type == TB_TAILCALL) {
                    op = JMP;
                    emit_epilogue(ctx, e, ctx->stack_usage);
                }

                if (n->inputs[2]->type == TB_SYMBOL) {
                    TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;

                    Val target = val_global(sym, 0);
                    inst1(e, op, &target, TB_X86_TYPE_QWORD);
                } else {
                    Val target = op_at(ctx, t->ins[0].src);
                    inst1(e, op, &target, TB_X86_TYPE_QWORD);
                }
                break;
            }
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE: {
                TB_X86_DataType dt = legalize_int2(n->dt);
                Val dst = op_at(ctx, t->outs[0]);

                Cond cc = emit_cmp(ctx, e, n, t, 0);
                inst1(e, SETO+(cc^1), &dst, dt);
                break;
            }
            case TB_SELECT: {
                TB_X86_DataType dt = legalize_int2(n->dt);
                Val dst = op_at(ctx, t->outs[0]);

                Cond cc = emit_cmp(ctx, e, n->inputs[1], t, 0);

                int ops = 1;
                if ((t->flags & TILE_HAS_IMM) == 0) {
                    ops += 1;
                }

                Val a = op_at(ctx, t->ins[ops+0].src);
                if (!is_value_match(&dst, &a)) {
                    inst2(e, MOV, &dst, &a, dt);
                }

                Val b = op_at(ctx, t->ins[ops+1].src);
                inst2(e, CMOVO+(cc^1), &dst, &b, dt);
                break;
            }
            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

                // the arena on the function should also be available at this time, we're
                // in the TB_Passes
                TB_Arena* arena = ctx->f->arena;
                TB_ArenaSavepoint sp = tb_arena_save(arena);
                int* succ = tb_arena_alloc(arena, br->succ_count * sizeof(int));

                // fill successors
                bool has_default = false;
                FOR_USERS(u, n) {
                    if (u->n->type == TB_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
                        TB_Node* succ_n = cfg_next_bb_after_cproj(u->n);

                        if (index == 0) {
                            has_default = !cfg_is_unreachable(succ_n);
                        }

                        MachineBB* mbb = node_to_bb(ctx, succ_n);
                        succ[index] = mbb->id;
                    }
                }

                TB_DataType dt = n->inputs[1]->dt;
                if (br->succ_count == 1) {
                    assert(0 && "degenerate branch? that's odd");
                } else if (br->succ_count == 2) {
                    Val naw = val_label(succ[1]);
                    Val yea = val_label(succ[0]);
                    Cond cc = emit_cmp(ctx, e, n->inputs[1], t, br->keys[0].key);

                    // if flipping avoids a jmp, do that
                    if (ctx->fallthrough == yea.label) {
                        x86_jcc(e, cc ^ 1, naw);
                    } else {
                        x86_jcc(e, cc, yea);
                        if (ctx->fallthrough != naw.label) {
                            x86_jmp(e, naw);
                        }
                    }
                } else {
                    AuxBranch* aux = t->aux;
                    TB_X86_DataType cmp_dt = legalize_int2(dt);
                    Val key = op_at(ctx, t->ins[0].src);

                    if (aux->if_chain) {
                        // Basic if-else chain
                        FOR_N(i, 1, br->succ_count) {
                            uint64_t curr_key = br->keys[i-1].key;

                            if (fits_into_int32(curr_key)) {
                                Val imm = val_imm(curr_key);
                                inst2(e, CMP, &key, &imm, cmp_dt);
                            } else {
                                Val tmp = op_at(ctx, t->ins[1].src);
                                Val imm = val_abs(curr_key);

                                inst2(e, MOV, &key, &imm, cmp_dt);
                                inst2(e, CMP, &key, &imm, cmp_dt);
                            }
                            x86_jcc(e, E, val_label(succ[i]));
                        }
                        x86_jmp(e, val_label(succ[0]));
                    } else {
                        int64_t min = aux->min;
                        int64_t max = aux->max;
                        int64_t range = (aux->max - aux->min) + 1;

                        // make a jump table with 4 byte relative pointers for each target
                        TB_Function* f = ctx->f;
                        TB_Global* jump_table = tb_global_create(f->super.module, -1, "jumptbl", NULL, TB_LINKAGE_PRIVATE);
                        tb_global_set_storage(f->super.module, tb_module_get_rdata(f->super.module), jump_table, range*4, 4, 1);

                        // generate patches for later
                        uint32_t* jump_entries = tb_global_add_region(f->super.module, jump_table, 0, range*4);

                        Set entries_set = set_create_in_arena(arena, range);
                        FOR_N(i, 1, br->succ_count) {
                            uint64_t key_idx = br->keys[i - 1].key - min;
                            assert(key_idx < range);

                            JumpTablePatch p;
                            p.pos = &jump_entries[key_idx];
                            p.target = succ[i];
                            dyn_array_put(ctx->jump_table_patches, p);
                            set_put(&entries_set, key_idx);
                        }

                        // handle default cases
                        FOR_N(i, 0, range) {
                            if (!set_get(&entries_set, i)) {
                                JumpTablePatch p;
                                p.pos = &jump_entries[i];
                                p.target = succ[0];
                                dyn_array_put(ctx->jump_table_patches, p);
                            }
                        }

                        /*int tmp = DEF(NULL, dt);
                        hint_reg(ctx, tmp, key);
                        if (dt.data >= 32) {
                            SUBMIT(inst_move(dt, tmp, key));
                        } else if (dt.data == 16) {
                            dt = TB_TYPE_I32;
                            SUBMIT(inst_op_rr(MOVZXW, dt, tmp, key));
                        } else if (dt.data == 8) {
                            dt = TB_TYPE_I32;
                            SUBMIT(inst_op_rr(MOVZXB, dt, tmp, key));
                        } else {
                            dt = TB_TYPE_I32;
                            uint64_t mask = tb__mask(dt.data);

                            SUBMIT(inst_move(dt, tmp, key));
                            SUBMIT(inst_op_rri(AND, dt, tmp, tmp, mask));
                        }*/

                        // copy key into temporary
                        {
                            Val tmp = op_at(ctx, t->ins[1].src);
                            inst2(e, MOV, &tmp, &key, TB_X86_TYPE_QWORD);
                            key = tmp;
                        }

                        int ins = 1;
                        Val target = op_at(ctx, t->ins[2].src);
                        Val table = op_at(ctx, t->ins[3].src);

                        // Simple range check:
                        //   if ((key - min) >= (max - min)) goto default
                        if (has_default) {
                            if (min != 0) {
                                Val imm = val_imm(min);
                                inst2(e, SUB, &key, &imm, cmp_dt);
                            }
                            // cmp key, range
                            Val imm = val_imm(range);
                            inst2(e, CMP, &key, &imm, cmp_dt);
                            // jnb fallthru
                            jcc(e, NB, succ[0]);
                        }
                        //   lea target, [rip + f]
                        Val fn_sym = val_global((TB_Symbol*) f, 0);
                        inst2(e, LEA, &target, &fn_sym, TB_X86_TYPE_QWORD);
                        //   lea table, [rip + JUMP_TABLE]
                        Val table_sym = val_global((TB_Symbol*) jump_table, 0);
                        inst2(e, LEA, &table, &table_sym, TB_X86_TYPE_QWORD);
                        //   movsxd table, [table + key*4]
                        Val addr = val_base_index_disp(table.reg, key.reg, SCALE_X4, 0);
                        inst2(e, MOVSXD, &table, &addr, TB_X86_TYPE_QWORD);
                        //   add target, table
                        inst2(e, ADD, &target, &table, TB_X86_TYPE_QWORD);
                        //   jmp target
                        __(jmp, target);
                    }
                }

                tb_arena_restore(arena, sp);
                break;
            }

            default: tb_todo();
        }
    }
}
#endif

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
    if (stack_usage > 0) {
        UnwindCode codes[] = {
            // sub rsp, stack_usage
            { .code_offset = 4, .unwind_op = UNWIND_OP_ALLOC_SMALL, .op_info = (stack_usage / 8) - 1 },
        };
        tb_outs(e, sizeof(codes), codes);
        code_count += 1;
    }

    tb_patch1b(e, patch_pos + offsetof(UnwindInfo, code_count), code_count);
}

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void our_print_rip32(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos, int disp_pos, int64_t imm);

// #define E(fmt, ...) printf(fmt, ## __VA_ARGS__)
static void our_print_memory_operand(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos) {
    uint8_t base = inst->regs & 0xFF;
    uint8_t index = (inst->regs >> 8) & 0xFF;

    if (inst->flags & TB_X86_INSTR_INDIRECT) {
        if ((inst->regs & 0xFFFF) == 0xFFFF) {
            E("%s [", tb_x86_type_name(inst->dt));
            our_print_rip32(e, d, inst, pos, inst->disp_pos, inst->disp);
            E("]");
            return;
        } else {
            E("%s [", tb_x86_type_name(inst->dt));
            if (base != 0xFF) {
                E("%s", tb_x86_reg_name(base, TB_X86_QWORD));
            }

            if (index != 0xFF) {
                E(" + %s*%d", tb_x86_reg_name(index, TB_X86_QWORD), 1 << inst->scale);
            }
        }

        if (inst->disp > 0) {
            E(" + %d", inst->disp);
        } else if (inst->disp < 0) {
            E(" - %d", -inst->disp);
        }
        E("]");
    } else if (base != 0xFF) {
        E("%s", tb_x86_reg_name(base, inst->dt));
    }
}

static void our_print_rip32(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos, int disp_pos, int64_t imm) {
    if (d->patch && d->patch->pos == pos + disp_pos) {
        const TB_Symbol* target = d->patch->target;

        if (target->name[0] == 0) {
            E("sym%p", target);
        } else {
            E("%s", target->name);
        }

        if (imm > 0) {
            E(" + %"PRId64, imm);
        } else if (imm < 0) {
            E(" - %"PRId64, imm);
        }

        d->patch = d->patch->next;
    } else {
        uint32_t target = pos + inst->length + imm;
        int bb = tb_emit_get_label(e, target);
        uint32_t landed = e->labels[bb] & 0x7FFFFFFF;

        if (landed != target) {
            E(".bb%d + %d", bb, (int)target - (int)landed);
        } else {
            E(".bb%d", bb);
        }
    }
}

static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    if (bb >= 0) {
        E(".bb%d:\n", bb);
    }

    while (pos < end) {
        while (d->loc != d->end && d->loc->pos == pos) {
            E("  // %s : line %d\n", d->loc->file->path, d->loc->line);
            d->loc++;
        }

        TB_X86_Inst inst;
        if (!tb_x86_disasm(&inst, end - pos, &e->data[pos])) {
            E("  ERROR\n");
            pos += 1; // skip ahead once... cry
            continue;
        }

        uint64_t line_start = e->total_asm;
        const char* mnemonic = tb_x86_mnemonic(&inst);
        E("  ");
        if (inst.flags & TB_X86_INSTR_REP) {
            E("rep ");
        }
        if (inst.flags & TB_X86_INSTR_LOCK) {
            E("lock ");
        }
        E("%s", mnemonic);
        if (inst.dt >= TB_X86_F32x1 && inst.dt <= TB_X86_F64x2) {
            static const char* strs[] = { "ss", "sd", "ps", "pd" };
            E("%s", strs[inst.dt - TB_X86_F32x1]);
        }
        E(" ");

        uint8_t rx = (inst.regs >> 16) & 0xFF;
        if (inst.flags & TB_X86_INSTR_DIRECTION) {
            if (rx != 255) {
                E("%s", tb_x86_reg_name(rx, inst.dt2));
                E(", ");
            }
            our_print_memory_operand(e, d, &inst, pos);
        } else {
            our_print_memory_operand(e, d, &inst, pos);
            if (rx != 255) {
                E(", ");
                E("%s", tb_x86_reg_name(rx, inst.dt2));
            }
        }

        if (inst.flags & TB_X86_INSTR_IMMEDIATE) {
            if ((inst.flags & TB_X86_INSTR_INDIRECT) || (inst.regs != 0xFFFFFF)) {
                E(", ");
            }

            if (inst.opcode == 0xE8 || inst.opcode == 0xE9 || inst.opcode == 0xEB || (inst.opcode >= 0x180 && inst.opcode <= 0x18F)) {
                our_print_rip32(e, d, &inst, pos, inst.length - 4, inst.imm);
            } else {
                E("%"PRId64, inst.imm);
            }
        }

        int offset = e->total_asm - line_start;
        if (d->comment && d->comment->pos == pos) {
            TB_OPTDEBUG(ANSI)(E("\x1b[32m"));
            E("%*s", 40 - offset, "// ");
            bool out_of_line = false;
            do {
                if (out_of_line) {
                    // tack on a newline
                    E("%*s  // ", offset, "");
                }

                E("%.*s\n", d->comment->line_len, d->comment->line);
                d->comment = d->comment->next;
                out_of_line = true;
            } while  (d->comment && d->comment->pos == pos);
            TB_OPTDEBUG(ANSI)(E("\x1b[0m"));
        } else {
            E("\n");
        }

        pos += inst.length;
    }
}
#undef E

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

ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .can_gvn = can_gvn,
    .node_name = node_name,
    .print_extra = print_extra,
    .print_dumb_extra = print_dumb_extra,
    .extra_bytes = extra_bytes,
    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,
    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
#else
ICodeGen tb__x64_codegen;
#endif
