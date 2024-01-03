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

static bool _2addr(TB_Node* n) { return false; }
static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->sched = greedy_scheduler;
    ctx->_2addr = _2addr;
    ctx->regalloc = tb__lsra;

    uint32_t all_tmps = ((1u << 18) - 1) << T0;
    uint32_t non_vol  = ((1u << 8) - 1)  << S0;

    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->normie_mask[REG_CLASS_GPR] = REGMASK(GPR, all_tmps);
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
            int rets = n->input_count - 4;
            TileInput* ins = tile_set_ins(ctx, dst, n, 4, n->input_count);

            assert(rets <= 2 && "At most 2 return values :(");
            FOREACH_N(i, 0, rets) {
                // TB_DataType dt = n->inputs[4+i]->dt;
                ins[i].mask = REGMASK(GPR, 1u << (V0 + i));
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
        case TB_ROL:
        case TB_ROR: {
            tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[REG_CLASS_GPR]);
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

static uint32_t rtype(uint32_t op, uint32_t rd, uint32_t rs, uint32_t rt, uint32_t shamt, uint32_t funct) {
    return (op<<26) | (rs<<21) | (rt<<16) | (rd<<11) | (shamt<<6) | funct;
}

enum {
    // R-type opcodes
    ADD  = 32,
    ADDU = 33,
    SUB  = 34,
    SUBU = 35,
    AND  = 36,
    OR   = 37,
    XOR  = 38,
};

static GPR gpr_at(LiveInterval* l) { assert(l->class == REG_CLASS_GPR); return l->assigned; }
static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_SPILL_MOVE) {
        GPR dst = gpr_at(t->interval);
        GPR src = gpr_at(t->ins[0].src);
        if (dst != src) {
            EMIT4(e, rtype(OR, dst, src, ZR, 0, 0));
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
