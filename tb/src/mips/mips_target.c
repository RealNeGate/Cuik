#include "../tb_internal.h"

#ifdef TB_HAS_MIPS
// NOTE(NeGate): THIS IS VERY INCOMPLETE
#include "../emitter.h"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_COUNT,
};

typedef enum {
    ZR,                             // zero reg.
    AT,                             // reserved for assembler.
    V0, V1,                         // returns.
    A0, A1, A2, A3,                 // call params.
    T0, T1, T2, T3, T4, T5, T6, T7, // temporaries (volatile)
    S0, S1, S2, S3, S4, S5, S6, S7, // temporaries (non-volatile)
    T8, T9,                         // temporaries (volatile)
    K0, K1,                         // kernel regs.
    GP,                             // global ptr
    SP,                             // stack ptr
    FP,                             // frame ptr
    RA,                             // return addr
} GPR;

#include "../codegen_impl.h"

static bool try_for_imm16(int bits, TB_Node* n, int32_t* out_x) {
    if (n->type != TB_INTEGER_CONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (bits > 16) {
        bool sign = (i->value >> 15ull) & 1;
        uint64_t top = i->value >> 16ull;

        // if the sign matches the rest of the top bits, we can sign extend just fine
        if (top != (sign ? 0xFFFFFFFF : 0)) {
            return false;
        }
    }

    *out_x = i->value;
    return true;
}

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->sched = greedy_scheduler;
    ctx->regalloc = tb__lsra;

    uint32_t not_tmps = (1 << ZR) | (1 << AT) | (1 << SP) | (1 << K0) | (1 << K1) | (1 << GP);

    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->normie_mask[REG_CLASS_GPR] = REGMASK(GPR, UINT32_MAX & ~not_tmps);
}

static RegMask isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n) {
    switch (n->type) {
        case TB_REGION:
        case TB_TRAP:
        case TB_CALLGRAPH:
        case TB_SPLITMEM:
        case TB_MERGEMEM:
        case TB_UNREACHABLE:
        case TB_DEBUGBREAK:
        return REGEMPTY;

        case TB_PROJ: {
            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == TB_ROOT) {
                if (i == 2) {
                    return REGMASK(GPR, 1u << RA); // RPC is the RA reg
                } else if (i >= 3) {
                    return REGMASK(GPR, 1u << ((i - 3) + A0));
                } else {
                    return REGEMPTY;
                }
            } else if (n->inputs[0]->type == TB_SPLITMEM) {
                return REGEMPTY;
            } else {
                tb_todo();
            }
            break;
        }

        case TB_ROOT: {
            int rets = (n->input_count - 4);

            dst->ins = tb_arena_alloc(tmp_arena, (1+rets) * sizeof(TileInput));
            dst->in_count = 1+rets;
            dst->ins[0].mask = REGMASK(GPR, 1u << RA);
            dst->ins[0].src = get_tile(ctx, n->inputs[2], true)->interval;

            assert(rets <= 2 && "At most 2 return values :(");
            if (rets >= 1) {
                dst->ins[1].mask = REGMASK(GPR, 1u << V0);
                dst->ins[1].src = get_tile(ctx, n->inputs[4], true)->interval;
            }

            if (rets >= 2) {
                dst->ins[2].mask = REGMASK(GPR, 1u << V1);
                dst->ins[2].src = get_tile(ctx, n->inputs[5], true)->interval;
            }
            return REGEMPTY;
        }

        case TB_INTEGER_CONST:
        return ctx->normie_mask[REG_CLASS_GPR];

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL: {
            tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[REG_CLASS_GPR]);
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[REG_CLASS_GPR]);
        return ctx->normie_mask[REG_CLASS_GPR];

        // no rotate ops, we'll emulate it:
        case TB_ROL:
        case TB_ROR: {
            dst->ins = tb_arena_alloc(tmp_arena, 2 * sizeof(TileInput));
            dst->in_count = 2;
            dst->ins[0].mask = ctx->normie_mask[REG_CLASS_GPR];
            dst->ins[1].mask = ctx->normie_mask[REG_CLASS_GPR];
            dst->ins[0].src = get_tile(ctx, n->inputs[1], true)->interval;
            dst->ins[1].src = NULL;
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        default: tb_todo();
    }
}

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {
    // TODO(NeGate): optionally preserve the frame pointer
    // TODO(NeGate): stack allocation
    ctx->prologue_length = ctx->emit.count;
}

#define RTYPE(op, funct) ((op<<26) | (funct))
#define ITYPE(op)        (op<<26)

enum {
    // R-types
    SLL, SRL, AND, ADD, ADDU, SUB, SUBU, OR, XOR, NOR, JR, MUL,

    // I-type
    ORI, XORI, LUI,

    INST_MAX,
};

static uint32_t insts[INST_MAX] = {
    // special
    [SLL]  = RTYPE(0b000000, 0b000000),
    [SRL]  = RTYPE(0b000000, 0b000010),
    [ADD]  = RTYPE(0b000000, 0b100000),
    [ADDU] = RTYPE(0b000000, 0b100001),
    [AND]  = RTYPE(0b000000, 0b100100),
    [OR]   = RTYPE(0b000000, 0b100101),
    [XOR]  = RTYPE(0b000000, 0b100110),
    [NOR]  = RTYPE(0b000000, 0b100111),
    [JR]   = RTYPE(0b000000, 0b001000),

    // special2
    [MUL] = RTYPE(0b011100, 0b000010),

    // i-types
    [ORI]  = ITYPE(0b001101),
    [XORI] = ITYPE(0b001110),
    [LUI]  = ITYPE(0b001111),
};

static void nop(TB_CGEmitter* e) {
    EMIT4(e, 0);
}

static void rtype(TB_CGEmitter* e, int op, uint32_t rd, uint32_t rs, uint32_t rt, uint32_t shamt) {
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | (rd<<11) | (shamt<<6));
}

static void itype(TB_CGEmitter* e, int op, uint32_t rt, uint32_t rs, uint32_t imm) {
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | imm);
}

static GPR gpr_at(LiveInterval* l) { assert(l->class == REG_CLASS_GPR); return l->assigned; }
static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_SPILL_MOVE) {
        GPR dst = gpr_at(t->interval);
        GPR src = gpr_at(t->ins[0].src);
        if (dst != src) {
            itype(e, ORI, dst, src, 0);
        }
    } else {
        TB_Node* n = t->n;
        switch (n->type) {
            // projections don't manage their own work, that's the
            // TUPLE node's job.
            case TB_PROJ:
            case TB_REGION:
            case TB_PHI:
            case TB_POISON:
            case TB_UNREACHABLE:
            case TB_SPLITMEM:
            case TB_MERGEMEM:
            case TB_CALLGRAPH:
            break;

            case TB_ROOT: {
                rtype(e, JR, 0, RA, 0, 0);
                nop(e); // TODO(NeGate): delay slots
                break;
            }

            case TB_INTEGER_CONST: {
                GPR dst = gpr_at(t->interval);
                uint64_t imm = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

                GPR curr = ZR;
                if (imm > UINT16_MAX) {
                    itype(e, LUI, dst, curr, (imm >> 16ull) & 0xFFFF);
                    curr = dst;
                }
                itype(e, ORI, dst, curr, imm & 0xFFFF);
                break;
            }

            case TB_AND:
            case TB_OR:
            case TB_XOR:
            case TB_ADD:
            case TB_SUB: {
                const static int ops[] = { AND, OR, XOR, ADD, SUB };
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);
                GPR rhs = gpr_at(t->ins[1].src);
                rtype(e, ops[n->type - TB_AND], dst, lhs, rhs, 0);
                break;
            }

            case TB_MUL: {
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);
                GPR rhs = gpr_at(t->ins[1].src);
                rtype(e, MUL, dst, lhs, rhs, 0);
                break;
            }

            case TB_ROL: {
                GPR dst = gpr_at(t->interval);
                GPR src = gpr_at(t->ins[0].src);
                GPR tmp = gpr_at(t->ins[1].src);

                int32_t x;
                if (try_for_imm32(n->dt.data, n->inputs[2], &x)) {
                    int32_t y = 32 - x;
                    rtype(e, SLL, tmp, 0, src, x);   //   sll     tmp,src,X
                    rtype(e, SRL, dst, 0, src, y);   //   srl     dst,src,Y
                    rtype(e, OR,  dst, dst, tmp, 0); //   or      dst,dst,tmp
                } else {
                    tb_todo();
                }
                break;
            }

            default: tb_todo();
        }
    }
}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {
}

static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {

}

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    return 0;
}

ICodeGen tb__mips64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
#endif
