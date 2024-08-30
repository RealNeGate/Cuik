#ifdef TB_HAS_X64
#include "x64.h"
#include <tb_x64.h>
#include "x64_emitter.h"
#include "x64_disasm.c"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_XMM,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 1,
    BUNDLE_INST_MAX = 1,
};

#include "../codegen_impl.h"

enum {
    MODE_REG,
    MODE_LD, // reg <- mem
    MODE_ST, // mem <- reg
};

// node with X86MemOp (mov, add, and...) will have this layout of inputs:
//   [1] mem
//   [2] base (or first src)
//   [3] idx
//   [4] val
typedef struct {
    uint8_t mode  : 2;
    uint8_t scale : 2;
    uint8_t cond  : 4;
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

uint32_t node_flags(TB_Node* n) {
    X86NodeType type = n->type;
    switch (type) {
        case x86_idiv: case x86_div:
        return NODE_MEMORY_IN;

        case x86_movzx8: case x86_movzx16:
        case x86_movsx8: case x86_movsx16: case x86_movsx32:
        case x86_add: case x86_or: case x86_and: case x86_sub:
        case x86_xor: case x86_cmp: case x86_mov: case x86_test: case x86_lea:
        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor: case x86_ucomi:
        case x86_addimm: case x86_orimm: case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        return n->dt.type == TB_TAG_MEMORY ? (NODE_MEMORY_IN | NODE_MEMORY_OUT | NODE_PINNED) : NODE_MEMORY_IN;

        case x86_call:
        case x86_static_call:
        return NODE_MEMORY_IN | NODE_MEMORY_OUT | NODE_PINNED;

        case x86_cmovcc:
        return NODE_MEMORY_IN;

        case x86_cmpjcc:
        case x86_cmpimmjcc:
        case x86_testjcc:
        case x86_testimmjcc:
        case x86_ucomijcc:
        case x86_AAAAAHHHH:
        return NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH | NODE_MEMORY_IN;

        default:
        return 0;
    }
}

static size_t extra_bytes(TB_Node* n) {
    X86NodeType type = n->type;
    switch (type) {
        case x86_int3:
        case x86_vzero:
        case x86_sxt_a2d:
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
        case x86_cmpjcc: case x86_cmpimmjcc:
        case x86_testjcc: case x86_testimmjcc:
        case x86_ucomijcc:
        return sizeof(X86MemOp);

        case x86_call:
        case x86_static_call:
        return sizeof(X86Call);

        case x86_cmovcc:
        return sizeof(X86Cmov);

        default:
        tb_todo();
    }
}

static const char* node_name(int n_type) {
    switch (n_type) {
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
            printf("scale=%d, disp=%d, mode=%s", 1u<<op->scale, op->disp, modes[op->mode]);
            break;
        }

        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm: case x86_imulimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf("scale=%d, disp=%d, mode=%s, imm=%d", 1u<<op->scale, op->disp, modes[op->mode], op->imm);
            break;
        }

        case x86_cmpjcc: case x86_cmpimmjcc:
        case x86_testjcc: case x86_testimmjcc:
        case x86_ucomijcc:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            printf("scale=%d, disp=%d, mode=%s, imm=%d", 1u<<op->scale, op->disp, modes[op->mode], op->imm);
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
        assert(dt.elem_or_addrspace == TB_TAG_F32 || dt.elem_or_addrspace == TB_TAG_F64);
        return dt.elem_or_addrspace == TB_TAG_F64 ? TB_X86_F64x2 : TB_X86_F32x4;
    }

    assert(dt.type == TB_TAG_F32 || dt.type == TB_TAG_F64);
    return (dt.type == TB_TAG_F64 ? TB_X86_F64x1 : TB_X86_F32x1);
}

static TB_X86_DataType legalize(TB_DataType dt) {
    if (dt.type == TB_TAG_F32) {
        return TB_X86_F32x1;
    } else if (dt.type == TB_TAG_F64) {
        return TB_X86_F64x1;
    } else {
        return legalize_int(dt);
    }
}

static bool fits_into_int32(uint64_t x) {
    uint32_t hi = x >> 32ull;
    return hi == 0 || hi == 0xFFFFFFFF;
}

static bool try_for_imm32(TB_DataType dt, TB_Node* n, int32_t* out_x) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (dt.type == TB_TAG_I64) {
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
            return op->mode == MODE_ST ? -1 : 4;
        }

        case x86_mov:
        case x86_vmov:
        // ANY_GPR = OP(ANY_GPR, IMM)
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm:
        case x86_shlimm: case x86_shrimm: case x86_sarimm: case x86_rolimm: case x86_rorimm:
        case x86_movzx8: case x86_movzx16: case x86_movsx8: case x86_movsx16: case x86_movsx32:
        {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            return op->mode == MODE_REG ? 2 : 0;
        }

        // ANY_GPR = OP(COND, shared: ANY_GPR, ANY_GPR)
        case x86_lea:
        case x86_cmovcc:
        return 2;

        // ANY_GPR = OP(ANY_GPR, ...)
        case TB_SHL: case TB_SHR: case TB_ROL: case TB_ROR: case TB_SAR:
        case TB_MUL: case x86_imulimm:
        return 1;

        case x86_div: case x86_idiv:
        return 4;

        case TB_ATOMIC_LOAD:
        return 0;

        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_PTROFF:
        return 3;

        case TB_MACH_COPY:
        case TB_MACH_MOVE:
        case TB_FLOAT_EXT:
        return 1;

        default:
        return n->type >= TB_AND && n->type <= TB_CMP_FLE ? 1 : -1;
    }
}

static bool node_remat(TB_Node* n) {
    return n->type == x86_lea;
}

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->abi_index = abi == TB_ABI_SYSTEMV ? 1 : 0;

    // currently only using 16 GPRs and 16 XMMs, AVX gives us
    // 32 YMMs (which double as XMMs) and later on APX will do
    // 32 GPRs.
    ctx->num_regs[REG_CLASS_GPR] = 16;
    ctx->num_regs[REG_CLASS_XMM] = 16;

    uint16_t all_gprs = 0xFFFF & ~(1 << RSP);
    if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
        all_gprs &= ~(1 << RBP);
        ctx->stack_header = 16;
    } else {
        ctx->stack_header = 8;
    }

    ctx->normie_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, all_gprs);
    ctx->normie_mask[REG_CLASS_XMM] = new_regmask(ctx->f, REG_CLASS_XMM, false, 0xFFFF);

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
        if (local->stack_pos >= 0) {
            // each stack slot is 8bytes
            ctx->num_spills = align_up(ctx->num_spills + (local->size+7)/8, (local->align+7)/8);
            local->stack_pos = -(8 + ctx->num_spills*8);
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

static bool same_mem_edge(TB_Node* ld_mem, TB_Node* st_mem) {
    /* if (ld_mem == st_mem) {
        return true;
    }

    // load might be above the split
    if (st_mem->type == TB_SPLITMEM) {
        return st_mem->inputs[1] == ld_mem;
    } else {
        return false;
    } */

    return ld_mem == st_mem;
}

// store(binop(load(a), b))
static bool can_folded_store(TB_Node* mem, TB_Node* addr, TB_Node* n) {
    if ((n->type >= TB_AND  && n->type <= TB_SUB) ||
        (n->type >= TB_FADD && n->type <= TB_FMAX)) {
        return
            n->inputs[1]->type == TB_LOAD &&
            same_mem_edge(n->inputs[1]->inputs[1], mem) &&
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

static TB_Node* mach_symbol(Ctx* restrict ctx, TB_Function* f, TB_Symbol* s) {
    TB_Node* n = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_PTR, 1, sizeof(TB_NodeMachSymbol));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeMachSymbol, .sym = s);

    TB_Node* k = tb__gvn(f, n, sizeof(TB_NodeMachSymbol));
    worklist_test_n_set(ctx->walker_ws, k);
    return k;
}

static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n) {
    if (n->type == TB_PROJ) {
        return n;
    } else if (n->type == TB_ROOT) {
        TB_Node* ret = n->inputs[1];
        if (ret->type == TB_RETURN) {
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
            TB_Node* base = mach_symbol(ctx, f, &g->super);

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
            TB_Node* base = mach_symbol(ctx, f, &g->super);

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
    } else if (n->type == TB_LOCAL) {
        // we don't directly ref the Local, this is the accessor op whenever we're
        // not folding into some other op nicely.
        TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;
        op_extra->disp = TB_NODE_GET_EXTRA_T(n, TB_NodeLocal)->stack_pos;
        op = tb__gvn(f, op, sizeof(X86MemOp));

        subsume_node2(f, n, op);
        set_input(f, op, ctx->frame_ptr, 2);
        return n;
    } else if (n->type == TB_SYMBOL) {
        TB_Node* sym = mach_symbol(ctx, f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);

        TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;
        op = tb__gvn(f, op, sizeof(X86MemOp));

        subsume_node2(f, n, op);
        set_input(f, op, sym, 2);
        return n;
    } else if (n->type >= TB_UDIV && n->type <= TB_SMOD) {
        // udiv, sdiv, umod, smod
        bool is_signed = (n->type == TB_SDIV || n->type == TB_SMOD);
        bool is_div    = (n->type == TB_UDIV || n->type == TB_SDIV);

        TB_Node* in_rdx;
        if (is_signed) {
            in_rdx = tb_alloc_node(f, x86_sxt_a2d, n->dt, 2, 0);
            set_input(f, in_rdx, n->inputs[1], 1);
        } else {
            // xor edx, edx
            in_rdx = tb_alloc_node(f, TB_ICONST, n->dt, 1, sizeof(TB_NodeInt));
            TB_NODE_SET_EXTRA(in_rdx, TB_NodeInt, .value = 0);
            in_rdx = tb__gvn(f, in_rdx, sizeof(TB_NodeInt));
        }

        TB_Node* op = tb_alloc_node(f, is_signed ? x86_idiv : x86_div, TB_TYPE_TUPLE, 6, sizeof(X86MemOp));
        // dividend
        set_input(f, op, n->inputs[2], 2);
        // divisor low (into RAX)
        set_input(f, op, n->inputs[1], 4);
        // divisor high (into RDX)
        set_input(f, op, in_rdx, 5);

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
        op_extra->clobber_xmm = abi->caller_saved_xmms;

        int gprs_used = 0, xmms_used = 0;
        FOR_N(i, 3, n->input_count) {
            int param_num = i - 3;

            // on win64 we always have the XMMs and GPRs used match the param_num
            // so if XMM2 is used, it's always the 3rd parameter.
            if (ctx->abi_index == 0) { xmms_used = gprs_used = param_num; }

            if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) {
                if (xmms_used < abi->xmm_count) {
                    xmms_used += 1;
                } else if (param_num + 1 > ctx->num_regs[REG_CLASS_STK]) {
                    ctx->num_regs[REG_CLASS_STK] = param_num + 1;
                }
            } else {
                TB_ASSERT(TB_IS_INTEGER_TYPE(n->inputs[i]->dt) || n->inputs[i]->dt.type == TB_TAG_PTR);
                if (gprs_used < abi->gpr_count) {
                    gprs_used += 1;
                } else if (param_num + 1 > ctx->num_regs[REG_CLASS_STK]) {
                    ctx->num_regs[REG_CLASS_STK] = param_num + 1;
                }
            }

            set_input(f, op, n->inputs[i], i);
        }

        return op;
    } else if (n->type == TB_MEMSET) {
        // for small memsets (16 <= size < 64), we just use XMMs
        if (n->inputs[3]->type == TB_ICONST && n->inputs[4]->type == TB_ICONST) {
            int val_byte = TB_NODE_GET_EXTRA_T(n->inputs[3], TB_NodeInt)->value;
            int size     = TB_NODE_GET_EXTRA_T(n->inputs[4], TB_NodeInt)->value;
            if (val_byte == 0 && (size == 16 || size == 32 || size == 48 || size == 64)) {
                TB_Node* mem = n->inputs[1];

                TB_Node* val;
                if (val_byte == 0) {
                    val = tb_alloc_node(f, x86_vzero, (TB_DataType){ { TB_TAG_V128, .elem_or_addrspace = TB_TAG_F32 } }, 1, sizeof(X86MemOp));
                    set_input(f, val, f->root_node, 0);
                } else {
                    tb_todo();
                }

                // sometimes introduced by other isel bits.
                TB_Node* addr = n->inputs[2];
                int32_t disp = 0;
                if (addr->type == x86_lea && addr->inputs[3] == NULL) {
                    disp = TB_NODE_GET_EXTRA_T(n, X86MemOp)->disp;
                    addr = addr->inputs[2];
                } else if (addr->type == TB_PTR_OFFSET && addr->inputs[2]->type == TB_ICONST) {
                    // [... + disp]
                    int64_t raw_disp = TB_NODE_GET_EXTRA_T(addr->inputs[2], TB_NodeInt)->value;
                    if (raw_disp == (int32_t) raw_disp) {
                        addr = addr->inputs[1];
                        disp = raw_disp;
                    }
                }

                if (addr->type == TB_LOCAL) {
                    disp += TB_NODE_GET_EXTRA_T(addr, TB_NodeLocal)->stack_pos;
                    addr = ctx->frame_ptr;
                }

                FOR_N(i, 0, size / 16) {
                    TB_Node* st = tb_alloc_node(f, x86_vmov, TB_TYPE_MEMORY, 5, sizeof(X86MemOp));
                    if (n->inputs[0]) {
                        set_input(f, st, n->inputs[0], 0);
                    }
                    set_input(f, st, mem,  1);
                    set_input(f, st, addr, 2);
                    set_input(f, st, val,  4);

                    X86MemOp* st_extra = TB_NODE_GET_EXTRA(st);
                    st_extra->mode = MODE_ST;
                    st_extra->disp = disp + i*16;

                    mem = st;
                }

                return mem;
            }
        }

        return n;
    } else if (n->type == TB_VA_START) {
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
        op_extra->disp = ctx->stack_header + proto->param_count*8;
        return op;
    } else if (n->type == TB_FRAME_PTR) {
        TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
        set_input(f, op, ctx->frame_ptr, 2);

        TB_FunctionPrototype* proto = ctx->f->prototype;
        X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
        op_extra->mode = MODE_LD;
        op_extra->disp = 0;
        return op;
    } else if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        TB_Node* a = n->inputs[1];
        TB_Node* b = n->inputs[2];

        TB_Node* mach_cmp;
        int flip = 0;
        X86MemOp* op_extra;
        if (TB_IS_FLOAT_TYPE(cmp_dt)) {
            mach_cmp = tb_alloc_node(f, x86_ucomi, n->dt, 5, sizeof(X86MemOp));
            op_extra = TB_NODE_GET_EXTRA(mach_cmp);

            set_input(f, mach_cmp, a, 4);
            set_input(f, mach_cmp, b, 2);
        } else {
            if (a->type == TB_ICONST && b->type != TB_ICONST) {
                flip ^= 1;
                SWAP(TB_Node*, a, b);
            }

            mach_cmp = tb_alloc_node(f, x86_cmp, n->dt, 5, sizeof(X86MemOp));
            op_extra = TB_NODE_GET_EXTRA(mach_cmp);

            int32_t x;
            if ((TB_IS_INTEGER_TYPE(cmp_dt) || cmp_dt.type == TB_TAG_PTR) && try_for_imm32(cmp_dt, b, &x)) {
                if (x == 0 && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
                    mach_cmp->type = x86_test;
                    set_input(f, mach_cmp, a, 4);
                } else {
                    mach_cmp->type = x86_cmpimm;
                    op_extra->imm = x;
                }
            } else {
                set_input(f, mach_cmp, b, 4);
            }
            set_input(f, mach_cmp, a, 2);
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

        op_extra->dt = cmp_dt;
        op_extra->cond = cc;
        return mach_cmp;
    } else if (n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH) {
        TB_Node* cond = n->inputs[1];
        TB_NodeBranchProj* if_br = cfg_if_branch(n);
        if (if_br) {
            // If-logic lowering, just generate a FLAGS and make
            // the branch compare on that instead.
            TB_Node* mach_cond = NULL;
            if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
                TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cond, TB_NodeCompare)->cmp_dt;
                TB_Node* a = cond->inputs[1];
                TB_Node* b = cond->inputs[2];

                int cc = -1;
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

                // proj0 is the true case given key=0
                int flip = (if_br->key == 0);
                X86MemOp* op_extra;
                if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                    mach_cond = tb_alloc_node(f, x86_ucomijcc, TB_TYPE_TUPLE, 5, sizeof(X86MemOp));
                    op_extra = TB_NODE_GET_EXTRA(mach_cond);
                    set_input(f, mach_cond, a, 4);
                    set_input(f, mach_cond, b, 2);
                } else {
                    if (a->type == TB_ICONST && b->type != TB_ICONST) {
                        // swap operands so the compares get swapped (not inverted)
                        switch (cond->type) {
                            case TB_CMP_EQ:  cc = E;  break;
                            case TB_CMP_NE:  cc = NE; break;
                            case TB_CMP_SLT: cc = G;  break;
                            case TB_CMP_SLE: cc = GE; break;
                            case TB_CMP_ULT: cc = A;  break;
                            case TB_CMP_ULE: cc = NB; break;
                            case TB_CMP_FLT: cc = A;  break;
                            case TB_CMP_FLE: cc = NB; break;
                            default: tb_unreachable();
                        }
                        SWAP(TB_Node*, a, b);
                    }

                    mach_cond = tb_alloc_node(f, x86_cmpjcc, TB_TYPE_TUPLE, 5, sizeof(X86MemOp));
                    op_extra = TB_NODE_GET_EXTRA(mach_cond);

                    int32_t x;
                    if ((TB_IS_INTEGER_TYPE(cmp_dt) || cmp_dt.type == TB_TAG_PTR) && try_for_imm32(cmp_dt, b, &x)) {
                        if (x == 0 && (cond->type == TB_CMP_EQ || cond->type == TB_CMP_NE)) {
                            mach_cond->type = x86_testjcc;
                            set_input(f, mach_cond, a, 4);
                        } else {
                            mach_cond->type = x86_cmpimmjcc;
                            op_extra->imm = x;
                        }
                    } else {
                        set_input(f, mach_cond, b, 4);
                    }
                    set_input(f, mach_cond, a, 2);
                }

                op_extra->dt = cmp_dt;
                if_br->key = cc ^ flip;
            } else if (if_br->key == 0) {
                mach_cond = tb_alloc_node(f, x86_testjcc, TB_TYPE_TUPLE, 5, sizeof(X86MemOp));
                TB_NODE_SET_EXTRA(mach_cond, X86MemOp, .dt = cond->dt);
                set_input(f, mach_cond, cond, 2);
                set_input(f, mach_cond, cond, 4);
                if_br->key = E;
            } else {
                mach_cond = tb_alloc_node(f, x86_cmpimmjcc, TB_TYPE_TUPLE, 5, sizeof(X86MemOp));
                TB_NODE_SET_EXTRA(mach_cond, X86MemOp, .dt = cond->dt, .imm = if_br->key);
                set_input(f, mach_cond, cond, 2);
                if_br->key = E;
            }

            set_input(f, mach_cond, n->inputs[0], 0);
            return mach_cond;
        } else {
            n->type = x86_AAAAAHHHH;
            return n;
        }
    }

    // any of these ops might be the starting point to complex addressing modes
    if ((n->type >= TB_AND && n->type <= TB_SUB)   ||
        (n->type >= TB_FADD && n->type <= TB_FMAX) ||
        n->type == TB_LOAD || n->type == TB_STORE  ||
        n->type == TB_SIGN_EXT || n->type == TB_ZERO_EXT ||
        n->type == TB_PTR_OFFSET) {
        const static int ops[]  = { x86_and, x86_or, x86_xor, x86_add, x86_sub };
        const static int fops[] = { x86_vadd, x86_vsub, x86_vmul, x86_vdiv, x86_vmin, x86_vmax };

        int32_t x;
        if (n->type == TB_MUL && try_for_imm32(n->dt, n->inputs[2], &x)) {
            TB_Node* op = tb_alloc_node(f, x86_imulimm, n->dt, 2, sizeof(X86MemOp));
            set_input(f, op, n->inputs[1], 1);
            TB_NODE_SET_EXTRA(op, X86MemOp, .imm = x);
            return op;
        }

        // folded binop with immediate
        if (n->type >= TB_AND && n->type <= TB_SUB) {
            TB_ASSERT(TB_IS_INTEGER_TYPE(n->dt));
            if (try_for_imm32(n->dt, n->inputs[2], &x)) {
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
                try_for_imm32(rhs->dt, rhs, &x)) {
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
                (n->dt.type == TB_TAG_I32 || n->dt.type == TB_TAG_I64)
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
            } else if (n->type == TB_SIGN_EXT) {
                TB_DataType src_dt = n->inputs[1]->dt;

                op_extra->mode = MODE_REG;
                n = n->inputs[1];
                set_input(f, op, n, 2);

                switch (src_dt.type) {
                    case TB_TAG_I8:  op->type = x86_movsx8;  break;
                    case TB_TAG_I16: op->type = x86_movsx16; break;
                    case TB_TAG_I32: op->type = x86_movsx32; break;
                    default: tb_todo();
                }
            } else if (n->type == TB_ZERO_EXT) {
                TB_DataType src_dt = n->inputs[1]->dt;

                op_extra->mode = MODE_REG;
                n = n->inputs[1];
                set_input(f, op, n, 2);

                // as long as any of these zero-extend to 32bits from a smaller size they're
                // capable of being a 64bit zero extend since 32bit ops will auto zero ext to 64bit.
                int op_type = -1;
                switch (src_dt.type) {
                    case TB_TAG_BOOL:op->type = x86_movzx8;  break;
                    case TB_TAG_I8:  op->type = x86_movzx8;  break;
                    case TB_TAG_I16: op->type = x86_movzx16; break;
                    case TB_TAG_I32: {
                        RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];

                        assert(op->user_count == 0);
                        tb_kill_node(f, op);

                        // mach copy actually just handles these sorts of things mostly
                        TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                        set_input(f, cpy, n, 1);
                        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = rm, .use = rm);
                        return cpy;
                    }
                    default: tb_todo();
                }
            }

            if (n->type == TB_LOAD) {
                // folded load now
                op_extra->mode = MODE_LD;
                if (op->type == x86_lea) {
                    op->type = TB_IS_FLOAT_TYPE(n->dt) ? x86_vmov : x86_mov;
                }

                set_input(f, op, n->inputs[0], 0); // ctrl in
                set_input(f, op, n->inputs[1], 1); // mem in
                n = n->inputs[2];
            }
        }

        // (PtrOffset a (Add b c)) => [... + disp]
        // (PtrOffset a b)         => [... + disp]
        if (n->type == TB_PTR_OFFSET) {
            TB_Node* base = n->inputs[1];
            TB_Node* idx  = n->inputs[2];

            if (idx->type == TB_ICONST) {
                // [... + disp]
                int64_t disp = TB_NODE_GET_EXTRA_T(idx, TB_NodeInt)->value;
                if (disp == (int32_t) disp) {
                    op_extra->disp = disp;
                }
            } else {
                // [... + disp]
                if (idx->type == TB_ADD && idx->inputs[2]->type == TB_ICONST) {
                    int64_t disp = TB_NODE_GET_EXTRA_T(idx->inputs[2], TB_NodeInt)->value;
                    if (disp == (int32_t) disp) {
                        op_extra->disp = disp;
                        idx = idx->inputs[1];
                    }
                }

                if (idx && idx->type == TB_SHL && idx->inputs[2]->type == TB_ICONST) {
                    uint64_t scale = TB_NODE_GET_EXTRA_T(idx->inputs[2], TB_NodeInt)->value;

                    // [... + index*scale] given scale is 1,2,4,8
                    if (scale <= 3) {
                        idx = idx->inputs[1];
                        op_extra->scale = scale;
                    }
                }

                set_input(f, op, idx, 3);
            }
            n = base;
        }

        // sometimes introduced by other isel bits.
        if (n->type == x86_lea && n->inputs[3] == NULL) {
            op_extra->disp += TB_NODE_GET_EXTRA_T(n, X86MemOp)->disp;
            n = n->inputs[2];
        }

        if (n->type == TB_SYMBOL) {
            if (op->inputs[3] != NULL) {
                // ok, if we need to make a symbol but also need indexing math
                TB_Node* sym = mach_symbol(ctx, f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);

                TB_Node* op = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 5, sizeof(X86MemOp));
                X86MemOp* op_extra = TB_NODE_GET_EXTRA(op);
                op_extra->mode = MODE_LD;
                set_input(f, op, sym, 2);

                op = tb__gvn(f, op, sizeof(X86MemOp));
                worklist_test_n_set(ctx->walker_ws, op);

                n = op;
            } else {
                TB_Node* base = mach_symbol(ctx, f, TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym);
                set_input(f, op, base, 2);
            }
        }

        if (n->type == TB_LOCAL) {
            set_input(f, op, ctx->frame_ptr, 2);
            op_extra->disp += TB_NODE_GET_EXTRA_T(n, TB_NodeLocal)->stack_pos;
        } else {
            TB_ASSERT(n);
            TB_ASSERT(n != op);
            set_input(f, op, n, 2);
        }
        return op;
    }

    return NULL;
}

static int node_tmp_count(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        case x86_call: case x86_static_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);
            return tb_popcount(op_extra->clobber_gpr) + op_extra->clobber_xmm;
        }

        case TB_MEMSET:
        return 2;

        case TB_MEMCPY:
        return 3;

        case x86_idiv: case x86_div:
        case TB_CYCLE_COUNTER: // clobber RDX & RAX
        return 2;

        default: return 0;
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

        case TB_CYCLE_COUNTER: {
            ins[1] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);
            ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
            return ins[1];
        }

        case TB_LOCAL:
        case TB_BRANCH_PROJ:
        case TB_MACH_SYMBOL:
        case TB_MACH_FRAME_PTR:
        return &TB_REG_EMPTY;

        case TB_MACH_JIT_THREAD_PTR:
        return ctx->normie_mask[REG_CLASS_GPR];

        case TB_MACH_COPY: {
            TB_NodeMachCopy* move = TB_NODE_GET_EXTRA(n);
            if (ins) {
                ins[1] = move->use;
            }
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

        case TB_SAFEPOINT: {
            if (ins) {
                TB_NodeSafepoint* sp = TB_NODE_GET_EXTRA(n);
                ins[1] = &TB_REG_EMPTY;
                ins[2] = ctx->normie_mask[REG_CLASS_GPR];
                FOR_N(i, n->input_count - sp->saved_val_count, n->input_count) {
                    ins[i] = ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_XMM : REG_CLASS_GPR];
                }
            }
            return &TB_REG_EMPTY;
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
                } else {
                    int param_id = i - 3;
                    if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) {
                        if (param_id >= params->xmm_count) {
                            return intern_regmask(ctx, REG_CLASS_STK, false, param_id);
                        }

                        return intern_regmask(ctx, REG_CLASS_XMM, false, 1u << param_id);
                    } else {
                        if (param_id >= params->gpr_count) {
                            return intern_regmask(ctx, REG_CLASS_STK, false, param_id);
                        }

                        return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << params->gprs[param_id]);
                    }
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
            } else if (n->inputs[0]->type >= TB_ATOMIC_LOAD && n->inputs[0]->type <= TB_ATOMIC_PTROFF) {
                return i == 1 && n->users ? ctx->normie_mask[REG_CLASS_GPR] : &TB_REG_EMPTY;
            } else {
                tb_todo();
                return &TB_REG_EMPTY;
            }
        }

        case x86_vzero:
        return ctx->normie_mask[REG_CLASS_XMM];

        case x86_AAAAAHHHH:
        if (ins) {
            ins[1] = ctx->normie_mask[REG_CLASS_GPR];
        }
        return &TB_REG_EMPTY;

        case x86_cmpjcc:
        case x86_testjcc:
        case x86_cmpimmjcc:
        case x86_testimmjcc:
        case x86_ucomijcc: {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (n->type == x86_ucomijcc) {
                rm = ctx->normie_mask[REG_CLASS_XMM];
            }

            if (ins) {
                ins[1] = &TB_REG_EMPTY;
                FOR_N(i, 2, n->input_count) {
                    ins[i] = n->inputs[i] ? rm : &TB_REG_EMPTY;
                }

                if (n->inputs[2] && (n->inputs[2]->type == TB_MACH_FRAME_PTR || n->inputs[2]->type == TB_MACH_SYMBOL)) {
                    ins[2] = &TB_REG_EMPTY;
                }
            }
            return &TB_REG_EMPTY;
        }

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
                return ctx->normie_mask[REG_CLASS_GPR];
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

        case TB_NEG: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_GPR]; }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_INT2FLOAT:
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
        case x86_xorimm: case x86_cmpimm: case x86_movimm: case x86_testimm:
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
            } else {
                return ctx->normie_mask[REG_CLASS_GPR];
            }
        }

        case x86_sxt_a2d: {
            if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_GPR]; }
            return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
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
                ins[6] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RAX);
                ins[7] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDX);
            }
            return &TB_REG_EMPTY;
        }

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_PTROFF: {
            RegMask* rm = ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                ins[0] = ins[1] = &TB_REG_EMPTY;
                ins[2] = ins[3] = rm;
            }
            return &TB_REG_EMPTY;
        }

        case x86_imulimm:
        if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_GPR]; }
        return ctx->normie_mask[REG_CLASS_GPR];

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
                ins[5] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDI);
                ins[6] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);
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
                ins[5] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RDI);
                ins[6] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RSI);
                ins[7] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RCX);
            }
            return &TB_REG_EMPTY;
        }

        case TB_NEVER_BRANCH:
        return &TB_REG_EMPTY;

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

                int base_stack = ctx->param_count;
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
                            ins[i] = intern_regmask(ctx, REG_CLASS_STK, false, base_stack + param_num);
                        }
                    } else {
                        TB_ASSERT(TB_IS_INTEGER_TYPE(n->inputs[i]->dt) || n->inputs[i]->dt.type == TB_TAG_PTR);
                        if (gprs_used < abi->gpr_count) {
                            ins[i] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << abi->gprs[gprs_used]);
                            gprs_used += 1;
                        } else {
                            ins[i] = intern_regmask(ctx, REG_CLASS_STK, false, base_stack + param_num);
                        }
                    }
                }

                size_t j = n->input_count;
                X86Call* op_extra = TB_NODE_GET_EXTRA(n);
                for (uint64_t bits = op_extra->clobber_gpr, k = 0; bits; bits >>= 1, k++) {
                    if (bits & 1) { ins[j++] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << k); }
                }

                FOR_N(k, 0, op_extra->clobber_xmm) {
                    ins[j++] = intern_regmask(ctx, REG_CLASS_XMM, false, 1u << k);
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

static int stk_offset(Ctx* ctx, int reg) {
    if (reg >= STACK_BASE_REG_NAMES) {
        int pos = -(8 + ((reg + 1) - STACK_BASE_REG_NAMES) * 8);
        return ctx->stack_usage + pos;
    } else if (reg >= ctx->param_count) {
        // param passing slots
        return (reg - ctx->param_count)*8;
    } else {
        // argument slots
        return ctx->stack_header + reg*8;
    }
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

static Val parse_cisc_operand(Ctx* restrict ctx, TB_Node* n, Val* rhs, X86MemOp* op) {
    if (rhs) {
        if ((n->type >= x86_addimm && n->type <= x86_rorimm) || n->type == x86_imulimm || n->type == x86_cmpimmjcc || n->type == x86_testimmjcc) {
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

static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle) {
    TB_ASSERT(bundle->count == 1);
    TB_Node* n = bundle->arr[0];
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];

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
        case TB_CALLGRAPH:
        case TB_MACH_SYMBOL:
        case TB_MACH_FRAME_PTR:
        case TB_UNREACHABLE:
        break;

        case TB_SAFEPOINT: {
            GPR poll = op_gpr_at(ctx, n->inputs[2]);
            COMMENT("safepoint %p", TB_NODE_GET_EXTRA_T(n, TB_NodeSafepoint)->userdata);
            __(TEST, TB_X86_QWORD, Vbase(poll, 0), Vgpr(RAX));
            break;
        }

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
            if (ctx->fallthrough != succ->fwd) {
                __(JMP, TB_X86_QWORD, Vlbl(succ->fwd));
            }
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

        case TB_MACH_MOVE:
        case TB_MACH_COPY: {
            TB_X86_DataType dt = legalize(n->dt);

            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            if (!is_value_match(&dst, &src)) {
                if (dst.type == VAL_MEM && src.type != VAL_MEM) {
                    COMMENT("spill");
                } else if (dst.type != VAL_MEM && src.type == VAL_MEM) {
                    COMMENT("reload");
                }

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

        case x86_sxt_a2d: {
            // cqo/cdq (sign extend RAX into RDX)
            if (n->dt.type == TB_TAG_I64) { EMIT1(e, 0x48); }
            EMIT1(e, 0x99);
            break;
        }

        case x86_idiv: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            // idiv
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int(op->dt);
            __(IDIV, dt, &rhs);
            break;
        }

        case x86_div: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            // div
            Val rhs = parse_cisc_operand(ctx, n, NULL, TB_NODE_GET_EXTRA(n));
            TB_X86_DataType dt = legalize_int(op->dt);
            __(DIV, dt, &rhs);
            break;
        }

        case TB_FLOAT_TRUNC: {
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            __(FP_CVT, legalize_float(n->inputs[1]->dt), &dst, &src);
            break;
        }

        case TB_NEG: {
            TB_X86_DataType dt = legalize_int(n->dt);
            Val dst = op_at(ctx, n);
            Val src = op_at(ctx, n->inputs[1]);
            if (!is_value_match(&dst, &src)) {
                __(MOV, dt, &dst, &src);
            }
            __(NEG, dt, &dst, &dst);
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

        case x86_vmov: case x86_vadd: case x86_vmul: case x86_vsub:
        case x86_vmin: case x86_vmax: case x86_vdiv: case x86_vxor:
        {
            const static int ops[] = {
                FP_MOV, FP_ADD, FP_MUL, FP_SUB, FP_DIV, FP_MIN, FP_MAX, FP_XOR
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
        case x86_add: case x86_or:  case x86_and:
        case x86_sub: case x86_xor: case x86_mov:
        case x86_addimm: case x86_orimm:  case x86_andimm: case x86_subimm:
        case x86_xorimm: case x86_movimm: case x86_shlimm: case x86_shrimm:
        case x86_sarimm: case x86_rolimm: case x86_rorimm:
        {
            const static int ops[] = {
                // binop
                ADD, OR, AND, SUB, XOR, -1, MOV, TEST,
                // binop with immediates
                ADD, OR, AND, SUB, XOR, -1, MOV, TEST,
                // shifts
                SHL, SHR, SAR, ROL, ROR,
                // misc (except for imul because it's weird)
                LEA,
            };

            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt;
            if (n->type == x86_test || n->type == x86_cmp || n->dt.type == TB_TAG_MEMORY) {
                dt = legalize_int(op->dt);
            } else if (n->type == x86_cmpimm) {
                dt = legalize_int(n->inputs[2]->dt);
            } else {
                dt = legalize_int(n->dt);
            }

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            int op_type = ops[n->type - x86_add];
            if (op->mode == MODE_ST) {
                __(op_type, dt, &rm, &rx);
            } else if (n->type >= x86_addimm && n->type <= x86_rorimm) {
                assert(n->type != x86_lea);
                Val dst = op_at(ctx, n);
                if (!is_value_match(&dst, &rm)) {
                    __(MOV, dt, &dst, &rm);
                }
                __(op_type, dt, &dst, &rx);
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
            TB_X86_DataType dt = legalize_int(n->dt);
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
            TB_X86_DataType dt = legalize_int(n->dt);

            Val dst  = op_at(ctx, n);
            Val lhs  = op_at(ctx, n->inputs[1]);
            Val rhs  = op_at(ctx, n->inputs[2]);

            if (!is_value_match(&dst, &lhs)) {
                __(MOV, dt, &dst, &lhs);
            }
            __(IMUL, dt, &dst, &rhs);
            break;
        }

        case x86_AAAAAHHHH: {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            TB_Arena* arena = &ctx->f->arena;
            TB_ArenaSavepoint sp = tb_arena_save(arena);
            TB_Node** succ = tb_arena_alloc(arena, br->succ_count * sizeof(TB_Node*));

            // fill successors
            bool has_default = false;
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_BRANCH_PROJ) {
                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                    succ[index] = USERN(u);
                }
            }

            Val key = op_at(ctx, n->inputs[1]);
            FOR_N(i, 1, br->succ_count) {
                uint64_t imm = TB_NODE_GET_EXTRA_T(succ[i], TB_NodeBranchProj)->key;
                TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ[i]);

                __(CMP, TB_X86_QWORD, &key, Vimm(imm));
                __(JE, TB_X86_QWORD, Vlbl(succ_bb->fwd));
            }

            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ[0]);
            __(JMP, TB_X86_QWORD, Vlbl(succ_bb->fwd));
            break;
        }

        case x86_ucomi:
        case x86_ucomijcc:
        case x86_cmp:
        case x86_test:
        case x86_cmpimm:
        case x86_testimm:
        case x86_cmpjcc:
        case x86_testjcc:
        case x86_cmpimmjcc:
        case x86_testimmjcc: {
            X86MemOp* op = TB_NODE_GET_EXTRA(n);
            TB_X86_DataType dt = legalize(op->dt);

            int cc = op->cond;
            int op_type = -1;
            switch (n->type) {
                case x86_cmp:        op_type = CMP;    break;
                case x86_cmpimm:     op_type = CMP;    break;
                case x86_cmpjcc:     op_type = CMP;    break;
                case x86_cmpimmjcc:  op_type = CMP;    break;
                case x86_test:       op_type = TEST;   break;
                case x86_testimm:    op_type = TEST;   break;
                case x86_testjcc:    op_type = TEST;   break;
                case x86_testimmjcc: op_type = TEST;   break;
                case x86_ucomi:      op_type = FP_UCOMI; break;
                case x86_ucomijcc:   op_type = FP_UCOMI; break;
            }

            Val rx, rm = parse_cisc_operand(ctx, n, &rx, op);
            __(op_type, dt, &rm, &rx);
            if (n->type >= x86_cmpjcc && n->type <= x86_testimmjcc) {
                int succ_count = 0;
                FOR_USERS(u, n) {
                    if (cfg_is_cproj(USERN(u))) { succ_count++; }
                }

                // the arena on the function should also be available at this time, we're
                // in the TB_Passes
                TB_Arena* arena = &ctx->f->arena;
                TB_ArenaSavepoint sp = tb_arena_save(arena);
                int* succ = tb_arena_alloc(arena, succ_count * sizeof(int));

                // fill successors
                bool has_default = false;
                FOR_USERS(u, n) {
                    if (USERN(u)->type == TB_BRANCH_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                        TB_Node* succ_n = USERN(u);

                        if (index == 0) {
                            has_default = !cfg_is_unreachable(succ_n);
                        }

                        TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
                        succ[index] = succ_bb->fwd;

                        printf("RELOC BB%ld => BB%d\n", succ_bb - ctx->cfg.blocks, succ_bb->fwd);
                    }
                }

                TB_NodeBranchProj* if_br = cfg_if_branch(n);
                if (if_br) {
                    Cond cc = if_br->key;
                    if (ctx->fallthrough == succ[1]) {
                        // if flipping avoids a jmp, do that
                        cc ^= 1;
                        SWAP(int, succ[0], succ[1]);
                    }

                    __(JO+cc, TB_X86_QWORD, Vlbl(succ[1]));
                    if (ctx->fallthrough != succ[0]) {
                        __(JMP, TB_X86_QWORD, Vlbl(succ[0]));
                    }
                } else {
                    tb_todo();
                }
                tb_arena_restore(arena, sp);
            } else {
                Val dst = op_at(ctx, n);
                __(SETO + cc, TB_X86_BYTE, &dst);
            }
            break;
        }

        case TB_MACH_JUMP: {
            TB_Node* succ_n = cfg_next_control(n);
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
            if (ctx->fallthrough != succ_bb->fwd) {
                EMIT1(e, 0xE9); EMIT4(e, 0);
                tb_emit_rel32(e, &e->labels[succ_bb->fwd], GET_CODE_POS(e) - 4, 0xFFFFFFFF, 0);
                printf("RELOC BB%ld => BB%d\n", succ_bb - ctx->cfg.blocks, succ_bb->fwd);
            }
            break;
        }

        case x86_cmovcc: {
            TB_X86_DataType dt = legalize_int(n->dt);
            int cc = TB_NODE_GET_EXTRA_T(n, X86Cmov)->cc;

            Val dst = op_at(ctx, n);
            Val rhs = op_at(ctx, n->inputs[4]);
            if (!is_value_match(&dst, &rhs)) {
                __(MOV, dt, &dst, &rhs);
            }

            Val cmp1 = op_at(ctx, n->inputs[1]);
            if (n->inputs[2]) {
                Val cmp2 = op_at(ctx, n->inputs[2]);
                __(CMP, dt, &cmp1, &cmp2);
            } else {
                __(CMP, dt, &cmp1, Vimm(0));
            }

            Val lhs = op_at(ctx, n->inputs[3]);
            __(CMOVO+cc, dt, &dst, &lhs);
            break;
        }

        case TB_SHL:
        case TB_SHR:
        case TB_ROL:
        case TB_ROR:
        case TB_SAR: {
            TB_X86_DataType dt = legalize_int(n->dt);

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

            TB_X86_DataType dt = legalize_int(n->inputs[2]->dt);
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

        case x86_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);
            Val target = op_at(ctx, n->inputs[2]);
            __(CALL, TB_X86_QWORD, &target);
            break;
        }

        case x86_static_call: {
            X86Call* op_extra = TB_NODE_GET_EXTRA(n);

            // on SysV, AL stores the number of float params
            if (ctx->abi_index == 1) {
                int float_params = 0;
                FOR_N(i, 3, n->input_count) {
                    if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) {
                        float_params++;
                    }
                }

                if (float_params == 0) {
                    __(XOR, TB_X86_DWORD, Vgpr(RAX), Vgpr(RAX));
                } else {
                    __(MOV, TB_X86_BYTE, Vgpr(RAX), Vimm(float_params));
                }
            }

            // CALL rel32
            EMIT1(e, 0xE8);
            EMIT4(e, 0);
            tb_emit_symbol_patch(e->output, op_extra->sym, e->count - 4);
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

static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b) { return false; }

// don't care about functional units on x86
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n) { return 1; }
static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) {
    switch (n->type) {
        case TB_MACH_JIT_THREAD_PTR:
        return 2;

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
            if (end && end->type >= x86_cmpjcc && end->type <= x86_testimmjcc && end->inputs[2] == n) {
                return 0;
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
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_PTROFF:
        return 20;

        case TB_MACH_MOVE:
        return 0; // cheapest op so that it tries to schedule it later

        default: return 1;
    }
}

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
static void our_print_rip32(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos, int disp_pos, int32_t imm);

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

static void our_print_rip32(TB_CGEmitter* e, Disasm* restrict d, TB_X86_Inst* restrict inst, size_t pos, int disp_pos, int32_t imm) {
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

        #if ASM_STYLE_PRINT_POS
        E("BB%d", bb);
        #else
        E(".bb%d", bb);
        #endif

        if (landed != target) {
            E(" + %d # target=%#4x, disp=%d", bb, (int)target - (int)landed, target, imm);
        }
    }
}

static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    while (pos < end) {
        #if ASM_STYLE_PRINT_POS
        E("%-4x  ", pos);
        #else
        E("  ", pos);
        #endif

        while (d->loc != d->end && d->loc->pos == pos) {
            TB_OPTDEBUG(ANSI)(E("\x1b[32m"));
            E("// %s : line %d\n", d->loc->file->path, d->loc->line);
            TB_OPTDEBUG(ANSI)(E("\x1b[0m"));
            d->loc++;

            #if ASM_STYLE_PRINT_POS
            E("%-4x  ", pos);
            #endif
        }

        TB_X86_Inst inst;
        if (!tb_x86_disasm(&inst, end - pos, &e->data[pos])) {
            E("ERROR %#02x\n", e->data[pos]);
            pos += 1; // skip ahead once... cry
            continue;
        }

        uint64_t line_start = e->total_asm;
        const char* mnemonic = tb_x86_mnemonic(&inst);
        if (inst.flags & TB_X86_INSTR_REP) {
            E("rep ");
        }
        if (inst.flags & TB_X86_INSTR_LOCK) {
            E("lock ");
        }

        #if ASM_STYLE_PRINT_POS
        if (inst.dt >= TB_X86_F32x1 && inst.dt <= TB_X86_F64x2) {
            static const char* strs[] = { "ss", "sd", "ps", "pd" };
            E("%s%-5s ", mnemonic, strs[inst.dt - TB_X86_F32x1]);
        } else {
            E("%-8s ", mnemonic);
        }
        #else
        if (inst.dt >= TB_X86_F32x1 && inst.dt <= TB_X86_F64x2) {
            static const char* strs[] = { "ss", "sd", "ps", "pd" };
            E("%s%s ", mnemonic, strs[inst.dt - TB_X86_F32x1]);
        } else {
            E("%s ", mnemonic);
        }
        #endif

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
            E("      %*s", 40 - offset, "// ");
            bool out_of_line = false;
            do {
                if (out_of_line) {
                    // tack on a newline
                    E("      %*s// ", offset, "");
                }

                E("%.*s\n", d->comment->line_len, d->comment->line);
                d->comment = d->comment->next;
                out_of_line = true;
            } while (d->comment && d->comment->pos == pos);
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
    .flags = node_flags,
    .extra_bytes = extra_bytes,
    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};
#endif
