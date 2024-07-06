#ifdef TB_HAS_MIPS
#include "../tb_internal.h"
#include "../emitter.h"

typedef enum {
    ZR,                             // zero reg.
    AT,                             // reserved for assembler.
    V0, V1,                         // returns.
    A0, A1, A2, A3, A4, A5, A6, A7, // call params.
    T4, T5, T6, T7,                 // temporaries (volatile)
    S0, S1, S2, S3, S4, S5, S6, S7, // temporaries (non-volatile)
    T8, T9,                         // temporaries (volatile)
    K0, K1,                         // kernel regs.
    GP,                             // global ptr
    SP,                             // stack ptr
    FP,                             // frame ptr
    RA,                             // return addr
} GPR;

static const char* gpr_names[32] = {
    "zr",
    "at",
    "v0", "v1",
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
    "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9",
    "k0", "k1",
    "gp",
    "sp",
    "fp",
    "ra",
};

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_FPR,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 2,
    BUNDLE_INST_MAX = 2,
};

#include "../codegen_impl.h"

typedef struct {
    int32_t imm;
} MIPSImm;

// machine node types
typedef enum MIPSNodeType {
    mips_nop = TB_MACH_MIPS,

    #define R(name, op, funct, ty) mips_ ## name,
    #define I(name, op, ty)        mips_ ## name,
    #define J(name, op, ty)        mips_ ## name,
    #include "mips_nodes.inc"
} MIPSNodeType;

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    TB_Arch arch = ctx->module->target_arch;
    TB_ASSERT(arch == TB_ARCH_MIPS32 || arch == TB_ARCH_MIPS64);

    ctx->abi_index = arch == TB_ARCH_MIPS64;
    ctx->stack_slot_size = arch == TB_ARCH_MIPS64 ? 8 : 4;

    uint32_t not_tmps = (1 << ZR) | (1 << AT) | (1 << SP) | (1 << K0) | (1 << K1) | (1 << GP);

    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->num_regs[REG_CLASS_FPR] = 32;
    ctx->normie_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, UINT32_MAX & ~not_tmps);
    ctx->normie_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, UINT32_MAX);

    #if 0
    uint32_t volatile_gprs = (((UINT32_MAX >> 7) << 1) & ~(0xFF << S0)) | (1u << RA);
    ctx->callee_saved[REG_CLASS_GPR] = ~volatile_gprs;
    #endif
}

static bool can_gvn(TB_Node* n) {
    return true;
}

uint32_t node_flags(TB_Node* n) {
    MIPSNodeType type = n->type;
    switch (type) {
        case mips_ldc1:
        case mips_lb:
        case mips_lw:
        case mips_ld:
        return NODE_MEMORY_IN;

        case mips_sdc1:
        case mips_sb:
        case mips_sh:
        case mips_sw:
        return NODE_MEMORY_IN | NODE_MEMORY_OUT;

        case mips_beq:
        case mips_bne:
        case mips_bgtz:
        case mips_blez:
        return NODE_CTRL | NODE_TERMINATOR | NODE_FORK_CTRL | NODE_BRANCH;

        default:
        return 0;
    }
}

static bool has_delay_slot(TB_Node* n) {
    switch (n->type) {
        case mips_beq:
        case mips_bne:
        case mips_bgtz:
        case mips_blez:
        return true;

        case mips_ldc1:
        case mips_lb:
        case mips_lw:
        case mips_ld:
        return true;

        default:
        return false;
    }
}

static size_t extra_bytes(TB_Node* n) {
    switch (n->type) {
        case mips_nop: return 0;
        #define R(name, op, funct, ty) case mips_ ## name: return sizeof(ty);
        #define I(name, op, ty)        case mips_ ## name: return sizeof(ty);
        #define J(name, op, ty)        case mips_ ## name: return sizeof(ty);
        #include "mips_nodes.inc"
        default: return 0;
    }
}

static const char* node_name(int n_type) {
    switch (n_type) {
        case mips_nop: return "nop";
        #define R(name, op, funct, ty) case mips_ ## name: return #name;
        #define I(name, op, ty)        case mips_ ## name: return #name;
        #define J(name, op, ty)        case mips_ ## name: return #name;
        #include "mips_nodes.inc"
        default: return NULL;
    }
}

static void print_extra(TB_Node* n) {
    switch (n->type) {
        // Print offset for memory ops
        #define FOO_void(name)
        #define FOO_MIPSImm(name) case mips_ ## name:

        #define R(name, op, funct, ty) CONCAT(FOO_, ty)(name)
        #define I(name, op, ty)        CONCAT(FOO_, ty)(name)
        #define J(name, op, ty)        CONCAT(FOO_, ty)(name)
        #include "mips_nodes.inc"
        {
            MIPSImm* op = TB_NODE_GET_EXTRA(n);
            printf("imm=%"PRId32, op->imm);
            break;
        }

        default: break;
    }
}

static int node_tmp_count(Ctx* restrict ctx, TB_Node* n) {
    return 0;
}

static bool fits_into_int16(uint64_t x) {
    uint64_t hi = x >> 16ull;
    return hi == 0 || hi == 0xFFFFFFFFFFFF;
}

static bool try_for_imm16(TB_Node* n, int32_t* out_x) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    bool sign     = (i->value >> 15ull) & 1;
    uint64_t top  = i->value >> 16ull;

    // if the sign matches the rest of the top bits, we can sign extend just fine
    if (top != (sign ? 0xFFFFFFFF : 0)) {
        return false;
    }

    *out_x = i->value;
    return true;
}

static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n) {
    // integer ops need to be converted into 32bit or 64bit ops
    if (n->type >= TB_AND && n->type <= TB_ROR) {
        TB_Node* a = n->inputs[1];
        TB_Node* b = n->inputs[2];

        // below 32bit? zero extend up
        bool is_64bit = n->dt.type == TB_TAG_I64;

        int32_t rhs;
        if (try_for_imm16(b, &rhs)) {
            TB_Node* op = tb_alloc_node(f, mips_nop, is_64bit ? TB_TYPE_I64 : TB_TYPE_I32, 2, sizeof(MIPSImm));
            set_input(f, op, n->inputs[1], 1);

            if (n->type == TB_SUB) {
                rhs = -rhs;
            }

            switch (n->type) {
                case TB_ADD: op->type = is_64bit ? mips_daddiu : mips_addiu; break;
                case TB_SUB: op->type = is_64bit ? mips_daddiu : mips_addiu; break;
                case TB_SHL: op->type = is_64bit ? mips_dsll : mips_sll;     break;
                case TB_SHR: op->type = is_64bit ? mips_dsrl : mips_srl;     break;
                default: tb_todo();
            }
            TB_NODE_SET_EXTRA(op, MIPSImm, .imm = rhs);
            return op;
        }

        /*switch (n->type) {
            case TB_ADD: n->type = is_64bit ? mips_daddu : mips_addu; break;
            case TB_SUB: n->type = is_64bit ? mips_dsubu : mips_subu; break;
            case TB_SHL: n->type = is_64bit ? mips_dsllv : mips_slv;  break;
            case TB_SHR: n->type = is_64bit ? mips_dslrv : mips_srv;  break;
            default: tb_todo();
        }*/
        tb_todo();

        return n;
    }

    // float ops
    if (n->type >= TB_FADD && n->type <= TB_FDIV) {
        // a*b + c => madd
        if (n->type == TB_FADD && n->inputs[1]->type == TB_FMUL) {
            TB_Node* op = tb_alloc_node(f, n->dt.type == TB_TAG_F64 ? mips_maddd : mips_madds, n->dt, 4, 0);
            set_input(f, op, n->inputs[1]->inputs[1], 1);
            set_input(f, op, n->inputs[1]->inputs[2], 2);
            set_input(f, op, n->inputs[2],            3);
            return op;
        }

        switch (n->type) {
            case TB_FADD: n->type = mips_adds; break;
            default: tb_todo();
        }

        return n;
    }

    // it's just addition but with pointer types
    if (n->type == TB_PTR_OFFSET) {
        n->type = mips_daddu;
        return n;
    }

    // MIPS really only has offsets for loads and stores (in the base ISA, MIPS II has
    // indexed loads which we don't support)
    if (n->type == TB_LOAD || n->type == TB_STORE) {
        int32_t offset = 0;
        TB_Node* addr  = n->inputs[2];
        if (addr->type == TB_PTR_OFFSET && try_for_imm16(addr->inputs[2], &offset)) {
            addr = addr->inputs[1];
        }

        bool store = n->type == TB_STORE;
        TB_DataType dt = store ? n->inputs[3]->dt : n->dt;

        int op_type = 0;
        if (dt.type == TB_TAG_F64) { op_type = store ? mips_sdc1 : mips_ldc1; }
        else { tb_todo(); }

        TB_Node* op = tb_alloc_node(f, op_type, store ? TB_TYPE_MEMORY : dt, store ? 4 : 3, sizeof(MIPSImm));
        set_input(f, op, n->inputs[0], 0);
        set_input(f, op, n->inputs[1], 1);
        set_input(f, op, addr,         2);
        if (store) {
            set_input(f, op, n->inputs[3], 3);
        }
        TB_NODE_SET_EXTRA(op, MIPSImm, .imm = offset);
        return op;
    }

    if (n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH) {
        TB_Node* cond = n->inputs[1];
        TB_NodeBranchProj* if_br = cfg_if_branch(n);
        if (if_br) {
            TB_Node* a = cond->inputs[1];
            TB_Node* b = cond->inputs[2];
            int flip = (if_br->key != 0);

            if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
                TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cond, TB_NodeCompare)->cmp_dt;
                if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                    tb_todo();
                } else {
                    // we want immediates on the RHS
                    if (a->type == TB_ICONST && b->type != TB_ICONST) {
                        flip ^= 1;
                        SWAP(TB_Node*, a, b);
                    }

                    int32_t rhs;
                    if (!try_for_imm16(b, &rhs) || rhs != 0) {
                        b = NULL;
                    }

                    MIPSNodeType t;
                    switch (cond->type) {
                        case TB_CMP_EQ:  t = mips_beq;  break;
                        case TB_CMP_NE:  t = mips_bne;  break;
                        case TB_CMP_SLT: t = mips_bgtz, flip ^= 1; break;
                        case TB_CMP_SLE: t = mips_blez; break;
                        case TB_CMP_ULT: t = mips_bgtz, flip ^= 1; break;
                        case TB_CMP_ULE: t = mips_blez; break;
                        default: tb_unreachable();
                    }

                    TB_Node* op = tb_alloc_node(f, mips_beq, TB_TYPE_TUPLE, 3, 0);
                    set_input(f, op, n->inputs[0], 0);
                    set_input(f, op, a, 1);
                    set_input(f, op, b, 2);
                    if_br->key = flip;
                    return op;
                }
            } else {
                tb_todo();
            }
        }
    }

    return n;
}

static bool node_remat(TB_Node* n) { return false; }

// 3 address ops, we don't really need this
static int node_2addr(TB_Node* n) { return -1; }

// don't care about functional units on x86
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n) {
    switch (n->type) {
        case mips_beq:
        case mips_bne:
        case mips_bgtz:
        case mips_blez:
        return 2;

        case mips_ldc1:
        case mips_lb:
        case mips_lw:
        case mips_ld:
        return 3;

        default:
        return 1;
    }
}

static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b) {
    // load delay slots in MIPS1
    if (a->type == mips_ldc1 || a->type == mips_lb ||
        a->type == mips_lw || a->type == mips_ld) {
        return true;
    }

    // branch delay slots
    if (b->type == mips_beq || b->type == mips_bne ||
        b->type == mips_bgtz || b->type == mips_blez) {
        return true;
    }

    return false;
}

static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) {
    switch (n->type) {
        case mips_ldc1:
        case mips_lb:
        case mips_lw:
        case mips_ld:
        return 3;

        case mips_sdc1:
        case mips_sb:
        case mips_sh:
        case mips_sw:
        return 3;

        default:
        return 1;
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
        case TB_DEBUG_LOCATION:
        if (ins) {
            FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
        }
        return &TB_REG_EMPTY;

        case TB_ICONST:
        return ctx->normie_mask[REG_CLASS_GPR];

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
            RegMask* rm = ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_FPR : REG_CLASS_GPR];
            if (ins) { ins[1] = rm; }
            return rm;
        }

        case TB_PHI: {
            if (ins) {
                FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
            }

            if (n->dt.type == TB_TAG_MEMORY) return &TB_REG_EMPTY;
            if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) return ctx->normie_mask[REG_CLASS_FPR];
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_PROJ: {
            if (n->dt.type == TB_TAG_MEMORY || n->dt.type == TB_TAG_CONTROL) {
                return &TB_REG_EMPTY;
            }

            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == TB_ROOT) {
                assert(i >= 2);
                if (i == 2) {
                    // RPC is inaccessible for now
                    return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RA);
                } else {
                    int param_id = i - 3;

                    // Parameters past the first 8 go into the stack
                    if (param_id >= 8) {
                        return intern_regmask(ctx, REG_CLASS_STK, false, param_id);
                    }

                    // int/ptr params: a0...a7
                    // float params:   f12...f19
                    if (n->dt.type == TB_TAG_F32 || n->dt.type == TB_TAG_F64) {
                        return intern_regmask(ctx, REG_CLASS_FPR, false, 1u << (param_id + 12));
                    } else {
                        return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (param_id + A0));
                    }
                }
            } else {
                tb_todo();
                return &TB_REG_EMPTY;
            }
        }

        case mips_beq:
        case mips_bne:
        case mips_bgtz:
        case mips_blez:
        if (ins) {
            ins[1] = ctx->normie_mask[REG_CLASS_GPR];
            ins[2] = n->inputs[2] ? ctx->normie_mask[REG_CLASS_GPR] : &TB_REG_EMPTY;
        }
        return &TB_REG_EMPTY;

        case mips_dsll:
        case mips_daddiu:
        if (ins) { ins[1] = ctx->normie_mask[REG_CLASS_GPR]; }
        return ctx->normie_mask[REG_CLASS_GPR];

        case mips_ldc1:
        if (ins) {
            ins[1] = &TB_REG_EMPTY;
            ins[2] = ctx->normie_mask[REG_CLASS_GPR];
        }
        return ctx->normie_mask[REG_CLASS_FPR];

        case mips_sdc1:
        if (ins) {
            ins[1] = &TB_REG_EMPTY;
            ins[2] = ctx->normie_mask[REG_CLASS_GPR];
            ins[3] = ctx->normie_mask[REG_CLASS_FPR];
        }
        return &TB_REG_EMPTY;

        case mips_addu:
        case mips_daddu:
        if (ins) {
            ins[1] = ctx->normie_mask[REG_CLASS_GPR];
            ins[2] = ctx->normie_mask[REG_CLASS_GPR];
        }
        return ctx->normie_mask[REG_CLASS_GPR];

        case mips_maddd:
        if (ins) {
            ins[1] = ctx->normie_mask[REG_CLASS_FPR];
            ins[2] = ctx->normie_mask[REG_CLASS_FPR];
            ins[3] = ctx->normie_mask[REG_CLASS_FPR];
        }
        return ctx->normie_mask[REG_CLASS_FPR];

        case TB_RETURN: {
            if (ins) {
                ins[1] = &TB_REG_EMPTY; // mem
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << RA); // rpc

                TB_FunctionPrototype* proto = ctx->f->prototype;
                assert(proto->return_count <= 2 && "At most 2 return values :(");

                FOR_N(i, 0, proto->return_count) {
                    TB_Node* in = n->inputs[3 + i];
                    TB_DataType dt = in->dt;
                    if (TB_IS_FLOAT_TYPE(dt)) {
                        ins[i] = intern_regmask(ctx, REG_CLASS_FPR, false, 1u << i);
                    } else {
                        ins[i] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (V0 + i));
                    }
                }
            }
            return &TB_REG_EMPTY;
        }

        default:
        tb_todo();
        return NULL;
    }
}

enum {
    #define R(name, op, funct, t) name,
    #define I(name, op, t)        name,
    #define J(name, op, t)        name,
    #include "mips_nodes.inc"

    INST_MAX,
};

#define __(t, op, ...) t ## type(e, op, __VA_ARGS__)

static uint32_t insts[INST_MAX] = {
    #define R(name, op, funct, t) [name] = ((op<<26) | (funct)),
    #define I(name, op, t)        [name] = (op<<26),
    #define J(name, op, t)        [name] = (op<<26),
    #include "mips_nodes.inc"
};

static void nop(TB_CGEmitter* e) {
    EMIT4(e, 0);
}

static void jtype(TB_CGEmitter* e, int op, uint32_t imm) {
    EMIT4(e, insts[op] | (imm & 0x3FFFFFF));
}

static void rtype(TB_CGEmitter* e, int op, uint32_t rd, uint32_t rs, uint32_t rt, uint32_t shamt) {
    assert(op >= 0 && op < INST_MAX);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | (rd<<11) | (shamt<<6));
}

static void itype(TB_CGEmitter* e, int op, uint32_t rt, uint32_t rs, uint32_t imm) {
    assert(op >= 0 && op < INST_MAX);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | (imm&0xFFFF));
}

static void branch(TB_CGEmitter* e, int op, uint32_t rt, uint32_t rs, int id) {
    assert(op == beq);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16));
    tb_emit_rel32(e, &e->labels[id], GET_CODE_POS(e) - 4, 0xFFFF, 2);
}

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {
    // TODO(NeGate): optionally preserve the frame pointer
    TB_FunctionPrototype* proto = ctx->f->prototype;

    size_t stack_usage = (ctx->num_regs[0] + proto->param_count) * ctx->stack_slot_size;
    ctx->stack_usage = stack_usage;

    if (stack_usage > 0) {
        __(i, addi, SP, SP, -stack_usage);
    }

    ctx->prologue_length = ctx->emit.count;
}

static void emit_goto(Ctx* ctx, TB_CGEmitter* e, MachineBB* succ) {
    if (ctx->fallthrough != succ->fwd) {
        branch(e, beq, ZR, ZR, succ->fwd);
    }
}

static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb) {
    tb_resolve_rel32(e, &e->labels[bb], e->count, 0xFFFF, 2);
}

static int op_gpr_at(Ctx* ctx, TB_Node* n) { return op_reg_at(ctx, n, REG_CLASS_GPR); }
static int op_fr_at(Ctx* ctx, TB_Node* n)  { return op_reg_at(ctx, n, REG_CLASS_FPR);  }

static void node_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    switch (n->type) {
        // some ops don't do shit lmao
        case TB_PHI:
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
        break;

        case TB_ICONST: {
            GPR dst = op_gpr_at(ctx, n);
            uint64_t imm = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

            GPR curr = ZR;
            if (imm >> 16ull) {
                __(i, lui, dst, curr, imm >> 16ull);
                curr = dst;
            }
            __(i, ori, dst, curr, imm & 0xFFFF);
            break;
        }

        case TB_RETURN: {
            // return can run in parallel to the stack free
            __(r, jr, 0, RA, 0, 0);
            if (ctx->stack_usage > 0) {
                __(i, addi, SP, SP, ctx->stack_usage);
            } else {
                nop(e);
            }
            break;
        }

        case mips_ldc1: {
            int dst  = op_fr_at(ctx, n);
            GPR base = op_gpr_at(ctx, n->inputs[2]);
            int32_t off = TB_NODE_GET_EXTRA_T(n, MIPSImm)->imm;
            __(i, ldc1, base, dst, off);
            break;
        }

        case mips_sdc1: {
            GPR base = op_gpr_at(ctx, n->inputs[2]);
            int val = op_fr_at(ctx, n->inputs[3]);
            int32_t off = TB_NODE_GET_EXTRA_T(n, MIPSImm)->imm;
            __(i, sdc1, base, val, off);
            break;
        }

        case mips_dsll: {
            GPR dst = op_gpr_at(ctx, n);
            GPR src = op_gpr_at(ctx, n->inputs[1]);

            int32_t imm = TB_NODE_GET_EXTRA_T(n, MIPSImm)->imm;
            __(r, dsll, dst, ZR, src, imm);
            break;
        }

        case mips_daddu: {
            GPR dst = op_gpr_at(ctx, n);
            GPR lhs = op_gpr_at(ctx, n->inputs[1]);
            GPR rhs = op_gpr_at(ctx, n->inputs[2]);
            __(r, daddu, dst, lhs, rhs, 0);
            break;
        }

        case mips_daddiu: {
            GPR dst = op_gpr_at(ctx, n);
            GPR lhs = op_gpr_at(ctx, n->inputs[1]);
            int32_t imm = TB_NODE_GET_EXTRA_T(n, MIPSImm)->imm;
            __(i, daddiu, dst, lhs, imm);
            break;
        }

        case mips_maddd: {
            int dst = op_fr_at(ctx, n);
            int a   = op_fr_at(ctx, n->inputs[1]);
            int b   = op_fr_at(ctx, n->inputs[2]);
            int c   = op_fr_at(ctx, n->inputs[3]);
            __(r, maddd, dst, a, b, c);
            break;
        }

        case mips_beq:
        case mips_bne:
        case mips_bgtz:
        case mips_blez: {
            int succ_count = 0;
            FOR_USERS(u, n) {
                if (cfg_is_cproj(USERN(u))) { succ_count++; }
            }

            // the arena on the function should also be available at this time, we're
            // in the TB_Passes
            TB_Arena* arena = &ctx->f->arena;
            TB_ArenaSavepoint sp = tb_arena_save(arena);
            int* succ = tb_arena_alloc(arena, succ_count * sizeof(int));
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_BRANCH_PROJ) {
                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                    MachineBB* mbb = node_to_bb(ctx, USERN(u));
                    succ[index] = mbb->fwd;
                }
            }

            GPR lhs = op_gpr_at(ctx, n->inputs[1]);
            GPR rhs = n->inputs[2] ? op_gpr_at(ctx, n->inputs[2]) : ZR;

            TB_NodeBranchProj* if_br = cfg_if_branch(n);
            if (if_br) {
                int inst = if_br->key ? bne : beq;
                if (ctx->fallthrough == succ[0]) {
                    // if flipping avoids a jmp, do that
                    inst = (inst == beq) ? bne : beq;
                    SWAP(int, succ[0], succ[1]);
                }

                branch(e, beq, lhs, rhs, succ[0]);
                if (ctx->fallthrough != succ[1]) {
                    nop(e);
                    branch(e, beq, ZR, ZR, succ[1]);
                }
            } else {
                tb_todo();
            }
            tb_arena_restore(arena, sp);
            break;
        }

        case TB_MACH_JUMP: {
            MachineBB* succ = node_to_bb(ctx, cfg_next_control(n));
            if (ctx->fallthrough != succ->fwd) {
                branch(e, beq, ZR, ZR, succ->fwd);
            }
            break;
        }

        default: tb_todo();
    }
}

static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle) {
    TB_ASSERT(bundle->count == 1 || bundle->count == 2);
    if (bundle->count == 2) {
        // place whichever has a delay slot first, if both have it then we don't care
        TB_Node* a = bundle->arr[0];
        TB_Node* b = bundle->arr[1];
        if (has_delay_slot(b) && !has_delay_slot(a)) {
            SWAP(TB_Node*, a, b);
        }

        node_emit(ctx, e, a);
        node_emit(ctx, e, b);
        COMMENT("delay slot");
    } else {
        node_emit(ctx, e, bundle->arr[0]);

        if (has_delay_slot(bundle->arr[0])) {
            nop(e);
            COMMENT("delay slot");
        }
    }
}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {
}

typedef struct {
    const char* name;
    int op;
    enum { ITYPE, RTYPE, JTYPE } family;
} Op;

static Op decode(uint32_t inst) {
    #define R(name, op, funct, t) if ((inst >> 26) == op && (inst & 0b111111) == funct) { return (Op){ #name, name, RTYPE }; }
    #define I(name, op, t)        if ((inst >> 26) == op) { return (Op){ #name, name, ITYPE }; }
    #define J(name, op, t)        if ((inst >> 26) == op) { return (Op){ #name, name, JTYPE }; }
    #include "mips_nodes.inc"

    return (Op){ "error", RTYPE };
}

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    if (bb >= 0) {
        E(".bb%d:\n", bb);
    }

    while (pos < end) {
        while (d->loc != d->end && d->loc->pos == pos) {
            E("  // %s : line %d\n", d->loc->file->path, d->loc->line);
            d->loc++;
        }

        uint32_t inst;
        memcpy(&inst, &e->data[pos], sizeof(uint32_t));
        pos += 4;

        // special case
        uint64_t line_start = e->total_asm;
        if (inst == 0) {
            E("  nop");
        } else if ((inst >> 26) == 0 && (inst & 0b111111) == 0b100101 && ((inst >> 16) & 0b11111) == 0) {
            int a = (inst >> 11) & 0b11111;
            int c = (inst >> 21) & 0b11111;
            E("  move $%s, $%s", gpr_names[a], gpr_names[c]);
        } else if ((inst >> 26) == 0b001101 && ((inst >> 21) & 0b11111) == 0) {
            int a = (inst >> 16) & 0b11111;
            E("  lui $%s, %d", gpr_names[a], inst & 0xFFFF);
        } else {
            // figure out what instruction type it is (everything else is trivial from there)
            Op op = decode(inst);
            E("  %s ", op.name);
            switch (op.family) {
                case RTYPE: {
                    if (op.op == madds || op.op == maddd) {
                        int fd = (inst >> 6)  & 0b11111;
                        int fs = (inst >> 11) & 0b11111;
                        int ft = (inst >> 16) & 0b11111;
                        int fr = (inst >> 21) & 0b11111;
                        E("$f%d, $f%d, $f%d, $f%d", fd, fr, fs, ft);
                    } else {
                        int shamt = (inst >> 6) & 0b11111;
                        int a = (inst >> 11) & 0b11111;
                        int b = (inst >> 16) & 0b11111;
                        int c = (inst >> 21) & 0b11111;
                        E("$%s, $%s", gpr_names[a], gpr_names[b]);
                        if (shamt) {
                            E(", %d", shamt);
                        } else {
                            E(", $%s", gpr_names[c]);
                        }
                    }
                    break;
                }

                case ITYPE: {
                    int b = (inst >> 16) & 0b11111;
                    int c = (inst >> 21) & 0b11111;
                    if (op.op >= lb && op.op <= sdc1) {
                        if (op.op == ldc1 || op.op == sdc1) {
                            E("$f%d, $%s(%d)", c, gpr_names[b], tb__sxt(inst & 0xFFFF, 16, 64));
                        } else {
                            E("$%s, $%s(%d)", gpr_names[c], gpr_names[b], tb__sxt(inst & 0xFFFF, 16, 64));
                        }
                    } else {
                        E("$%s, $%s", gpr_names[b], gpr_names[c]);

                        int32_t imm = tb__sxt(inst & 0xFFFF, 16, 64);
                        if (d->patch && d->patch->pos == pos - 4) {
                            const TB_Symbol* target = d->patch->target;
                            d->patch = d->patch->next;

                            if (target->name[0] == 0) {
                                E("sym%p", target);
                            } else {
                                E("%s", target->name);
                            }

                            if (imm) {
                                E(" + %d", imm*4);
                            }
                        } else if (op.op >= beq && op.op <= bgtz) {
                            uint32_t target = pos + imm*4;
                            int bb = tb_emit_get_label(e, target);
                            uint32_t landed = e->labels[bb] & 0x7FFFFFFF;

                            if (landed != target) {
                                E(", .bb%d + %d", bb, (int)target - (int)landed);
                            } else {
                                E(", .bb%d", bb);
                            }
                        } else {
                            E(", %d", imm);
                        }
                    }
                    break;
                }

                case JTYPE: {
                    int32_t disp = (inst & 0x3FFFFFF) * 2;
                    if (d->patch && d->patch->pos == pos - 4) {
                        const TB_Symbol* target = d->patch->target;
                        d->patch = d->patch->next;

                        if (target->name[0] == 0) {
                            E("sym%p", target);
                        } else {
                            E("%s", target->name);
                        }

                        if (disp) {
                            E(" + %"PRIu64, disp);
                        }
                    } else {
                        E("%"PRIu64, disp);
                    }
                    break;
                }

                default: tb_todo();
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
    }
}
#undef E

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    return 0;
}

ICodeGen tb__mips32_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 32,
    .can_gvn = can_gvn,
    .node_name = node_name,
    .print_extra = print_extra,
    .flags = node_flags,
    .extra_bytes = extra_bytes,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};

ICodeGen tb__mips64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .can_gvn = can_gvn,
    .node_name = node_name,
    .print_extra = print_extra,
    .flags = node_flags,
    .extra_bytes = extra_bytes,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};
#endif

#if 0
// NOTE(NeGate): THIS IS VERY INCOMPLETE

static void tb_emit_rel16(TB_CGEmitter* restrict e, uint32_t* head, uint32_t pos) {
    uint32_t curr = *head;
    if (curr & 0x80000000) {
        // the label target is resolved, we need to do the relocation now
        uint32_t target = curr & 0x7FFFFFFF;
        int32_t rel = target - (pos + 4);
        PATCH2(e, pos+2, rel*4);
    } else {
        PATCH2(e, pos+2, curr);
        *head = pos;
    }
}

static void tb_resolve_rel16(TB_CGEmitter* restrict e, uint32_t* head, uint32_t target) {
    // walk previous relocations
    uint32_t curr = *head;
    while (curr != 0 && (curr & 0x80000000) == 0) {
        uint32_t next;
        memcpy(&next, &e->data[curr], 4);
        int32_t rel = target - (curr + 4);
        PATCH2(e, curr+2, rel*4);
        curr = next;
    }

    // store the target and mark it as resolved
    *head = 0x80000000 | target;
}

static void loadimm(TB_CGEmitter* e, int dst, uint32_t imm) {
    GPR curr = ZR;
    if (imm >> 16ull) {
        itype(e, lui, dst, curr, imm >> 16ull);
        curr = dst;
    }

    itype(e, ori, dst, curr, imm & 0xFFFF);
}

static int stk_offset(Ctx* ctx, int reg) {
    int pos = reg * ctx->stack_slot_size;
    if (reg >= ctx->num_regs[0]) {
        return (ctx->stack_usage - pos) + ctx->stack_slot_size;
    } else {
        return pos;
    }
}

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {
    // TODO(NeGate): optionally preserve the frame pointer
    TB_FunctionPrototype* proto = ctx->f->prototype;

    size_t stack_usage = (ctx->num_regs[0] + proto->param_count) * ctx->stack_slot_size;
    ctx->stack_usage = stack_usage;

    if (stack_usage > 0) {
        __(i, addi, SP, SP, -stack_usage);
    }

    FOR_N(i, 0, dyn_array_length(ctx->callee_spills)) {
        int pos = stk_offset(ctx, ctx->callee_spills[i].stk);
        int rc = ctx->callee_spills[i].class;
        assert(rc == REG_CLASS_GPR);

        GPR src = ctx->callee_spills[i].reg;
        __(i, sw, src, SP, stk_offset(ctx, pos));
    }

    ctx->prologue_length = ctx->emit.count;
}

static GPR gpr_at(LiveInterval* l) { assert(l->class == REG_CLASS_GPR); return l->assigned; }
static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_SPILL_MOVE) {
        if (t->outs[0]->class == REG_CLASS_STK && t->ins[0].src->class == REG_CLASS_GPR) {
            COMMENT("spill v%d -> v%d", t->outs[0]->id, t->ins[0].src->id);
            GPR src = gpr_at(t->ins[0].src);
            int dst = t->outs[0]->assigned;
            itype(e, sw, src, SP, stk_offset(ctx, dst));
        } else if (t->outs[0]->class == REG_CLASS_GPR && t->ins[0].src->class == REG_CLASS_STK) {
            COMMENT("reload v%d -> v%d", t->outs[0]->id, t->ins[0].src->id);
            GPR dst = gpr_at(t->outs[0]);
            int src = t->ins[0].src->assigned;
            itype(e, lw, dst, SP, stk_offset(ctx, src));
        } else {
            GPR dst = gpr_at(t->outs[0]);
            GPR src = gpr_at(t->ins[0].src);
            if (dst != src) {
                rtype(e, or, dst, src, 0, 0);
            }
        }
    } else if (t->tag == TILE_GOTO) {
        MachineBB* mbb = node_to_bb(ctx, t->succ);
        if (ctx->fallthrough != mbb->id) {
            branch(e, beq, ZR, ZR, mbb->id);
        }
    } else {
        TB_Node* n = t->n;
        switch (n->type) {
            // projections don't manage their own work, that's the
            // TUPLE node's job.
            case TB_PROJ:
            case TB_REGION:
            case TB_NATURAL_LOOP:
            case TB_AFFINE_LOOP:
            case TB_PHI:
            case TB_ROOT:
            case TB_POISON:
            case TB_UNREACHABLE:
            case TB_SPLITMEM:
            case TB_MERGEMEM:
            case TB_CALLGRAPH:
            break;

            case TB_RETURN: {
                // return can run in parallel to the stack free
                __(r, jr, 0, RA, 0, 0);
                if (ctx->stack_usage > 0) {
                    __(i, addi, SP, SP, ctx->stack_usage);
                } else {
                    nop(e);
                }
                break;
            }

            case TB_BRANCH: {
                GPR src = gpr_at(t->ins[0].src);
                __debugbreak();
                // branch(e, beq, src, ZR, 0);
                // branch(e, beq, src, ZR, mbb->id);
                break;
            }

            case TB_CALL: {
                if (n->inputs[2]->type == TB_SYMBOL) {
                    TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;

                    __(j, jal, 0);
                    tb_emit_symbol_patch(e->output, sym, e->count - 4);
                } else {
                    tb_todo();
                }
                nop(e); // TODO(NeGate): delay slots
                break;
            }

            case TB_LOAD: {
                AuxAddress* aux = t->aux;
                GPR dst  = gpr_at(t->outs[0]);
                int32_t offset = aux->offset;

                GPR base;
                if (aux->base->type == TB_LOCAL) {
                    offset += get_stack_slot(ctx, aux->base);
                    base = SP;
                } else {
                    base = gpr_at(t->ins[0].src);
                }
                itype(e, lw, dst, base, offset);
                break;
            }

            case TB_STORE: {
                AuxAddress* aux = t->aux;
                GPR src = gpr_at(t->ins[0].src);
                int32_t offset = aux->offset;

                GPR base;
                if (aux->base->type == TB_LOCAL) {
                    offset += get_stack_slot(ctx, aux->base);
                    base = SP;
                } else {
                    base = gpr_at(t->ins[1].src);
                }
                itype(e, sw, src, base, offset);
                break;
            }

            case TB_LOCAL: {
                GPR dst = gpr_at(t->outs[0]);
                int pos = get_stack_slot(ctx, n);
                itype(e, addi, dst, SP, pos);
                break;
            }

            case TB_ICONST: {
                GPR dst = gpr_at(t->outs[0]);
                uint64_t imm = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

                loadimm(e, dst, imm);
                if (t->out_count == 2) {
                    GPR dst2 = gpr_at(t->outs[1]);
                    loadimm(e, dst2, imm >> 32ull);
                }
                break;
            }

            case TB_ARRAY_ACCESS: {
                intptr_t offset = (intptr_t) t->aux;
                GPR dst  = gpr_at(t->outs[0]);
                GPR base = gpr_at(t->ins[0].src);
                GPR idx  = gpr_at(t->ins[1].src);
                int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;

                uint64_t log2 = tb_ffs(stride) - 1;
                if (UINT64_C(1) << log2 == stride) {
                    rtype(e, sll, dst, 0, idx, log2);
                } else {
                    tb_todo();
                }
                rtype(e, add, dst, base, offset, 0);
                break;
            }

            case TB_AND:
            case TB_OR:
            case TB_XOR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL: {
                const static int rops[] = { and,  or,  xor,  add,  sub,  mul };
                const static int iops[] = { andi, ori, xori, addi, addi, -1  };

                GPR dst = gpr_at(t->outs[0]);
                GPR lhs = gpr_at(t->ins[0].src);
                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    uint64_t i = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
                    if (n->type == TB_SUB) {
                        // no SUBI, we just negate and do ADDI
                        i = -i;
                    }

                    itype(e, iops[n->type - TB_AND], dst, lhs, i);
                } else {
                    GPR rhs = gpr_at(t->ins[1].src);
                    rtype(e, rops[n->type - TB_AND], dst, lhs, rhs, 0);
                }
                break;
            }

            case TB_ZERO_EXT:
            case TB_SIGN_EXT: {
                GPR dst = gpr_at(t->outs[0]);
                GPR src = gpr_at(t->ins[0].src);
                int op = n->type == TB_SIGN_EXT ? sra : srl;

                int32_t x = 64;
                int32_t y = x - bits_in_type(ctx, n->dt);
                rtype(e, sll, dst, 0, src, x);
                rtype(e, op,  dst, 0, src, y);
                break;
            }

            case TB_SAR:
            case TB_SHL:
            case TB_SHR: {
                const static int rops[] = { sllv, srlv, srav };
                const static int iops[] = { sll,  srl,  sra  };

                GPR dst = gpr_at(t->outs[0]);
                GPR lhs = gpr_at(t->ins[0].src);
                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    uint64_t x = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
                    rtype(e, iops[n->type - TB_SHL], dst, 0, lhs, x);
                } else {
                    GPR rhs = gpr_at(t->ins[1].src);
                    rtype(e, rops[n->type - TB_SHL], dst, lhs, rhs, 0);
                }
                break;
            }

            case TB_ROL: {
                GPR dst = gpr_at(t->outs[0]);
                GPR src = gpr_at(t->ins[0].src);
                GPR tmp = gpr_at(t->ins[1].src);

                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    uint64_t x = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;

                    int32_t y = 32 - x;
                    rtype(e, sll, tmp, 0, src, x);   //   sll     tmp,src,X
                    rtype(e, srl, dst, 0, src, y);   //   srl     dst,src,Y
                    rtype(e, or,  dst, dst, tmp, 0); //   or      dst,dst,tmp
                } else {
                    tb_todo();
                }
                break;
            }

            default: tb_todo();
        }
    }
}
#endif
