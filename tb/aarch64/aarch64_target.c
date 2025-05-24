#ifdef TB_HAS_AARCH64
#include "../emitter.h"
#include "../tb_internal.h"
#include "aarch64_emitter.h"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_FPR,
    REG_CLASS_FLAGS,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 1,
    BUNDLE_INST_MAX = 1,
};

// Brings in all the glue to our codegen functions
#include "../codegen_impl.h"

typedef struct {
    uint32_t imm;
} A64Op;

#include "a64_gen.h"

static CallingConv CC_A64PCS = {
    .sp_class  = REG_CLASS_GPR, .sp_reg  = SP, // stack pointer
    .fp_class  = REG_CLASS_GPR, .fp_reg  = FP, // frame pointer
    .rpc_class = REG_CLASS_GPR, .rpc_reg = LR, // return PC
    .flexible_param_alloc = true,
    
    .reg_saves[REG_CLASS_GPR] = (char[32]){
        // volatile/clobbered (caller saved) 'C'
        [ 0 ... 18] = 'C', [LR] = 'C',
        // non-volatile (callee saved) 'c'
        [19 ... 29] = 'c', [SP] = 'c',
    },
    
    // TODO calling convention needs float pairs (low and high 64bit)
    // or something that's caller and callee saved at the same time
    .reg_saves[REG_CLASS_FPR] = (char[32]){
        // volatile/clobbered (caller saved) 'C'
        [ 0 ...  7] = 'C',
        [16 ... 31] = 'C',
        // non-volatile (callee saved) 'c'
        [ 8 ... 15] = 'c',
    },

    .param_count = { [REG_CLASS_GPR] = 6, [REG_CLASS_FPR] = 4 },
    .params[REG_CLASS_GPR] = (uint8_t[]){ 0, 1, 2, 3, 4, 5, 6, 7 },
    .params[REG_CLASS_FPR] = (uint8_t[]){ 0, 1, 2, 3, 4, 5, 6, 7 },

    .ret_count = { [REG_CLASS_GPR] = 2, [REG_CLASS_FPR] = 2 },
    .rets[REG_CLASS_GPR] = { 0, 1 },
    .rets[REG_CLASS_FPR] = { 0, 1 },
};

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->calling_conv = &CC_A64PCS;

    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->num_regs[REG_CLASS_FPR] = 32;
    ctx->num_regs[REG_CLASS_FLAGS] = 1;

    uint32_t all_gprs = UINT32_MAX & ~(1u << SP);
    ctx->all_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, 0xFFFFFFFF);
    ctx->all_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, 0xFFFFFFFF);
    ctx->all_mask[REG_CLASS_FLAGS] = new_regmask(ctx->f, REG_CLASS_FLAGS, false, 1);

    ctx->normie_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, all_gprs);
    ctx->normie_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, 0xFFFFFFFF);
    ctx->normie_mask[REG_CLASS_FLAGS] = new_regmask(ctx->f, REG_CLASS_FLAGS, false, 1);

    ctx->mayspill_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, true, all_gprs);
    ctx->mayspill_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, true, 0xFFFFFFFF);
    ctx->mayspill_mask[REG_CLASS_FLAGS] = NULL;
}

static void print_extra(TB_Node* n) {
    A64Op* op = TB_NODE_GET_EXTRA(n);
    printf("imm=%d", op->imm);
}

static void node_add_tmps(Ctx* restrict ctx, TB_Node* n) {

}

static int node_constraint_kill(Ctx* restrict ctx, TB_Node* n, RegMask** kills) {
    return 0;
}

static void print_pretty(Ctx* restrict ctx, TB_Node* n) {
    tb_print_dumb_node(NULL, n);
}

static bool can_gvn(TB_Node* n) {
    return true;
}

static size_t extra_bytes(TB_Node* n) {
    return sizeof(A64Op);
}

static uint32_t node_flags(TB_Node* n) {
    return 0;
}

////////////////////////////////
// Instruction selection
////////////////////////////////
static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n) {
    /* patterns
    mandatory:
        (rol $lhs $rhs) = (ror $lhs (sub $size $rhs))
        (umod $lhs $rhs) = (sub $lhs (mul $rhs (udiv $lhs $rhs)))
        (smod $lhs $rhs) = (sub $lhs (mul $rhs (sdiv $lhs $rhs)))
        (fconst $a) = (iconst $a)
    optional:
        (<some sort of zero register optimisation>)
        (xor $lhs (not $rhs)) = (eon $lhs $rhs)
        (and $lhs (not $rhs)) = (andn $lhs $rhs)
        (or $lhs (not $rhs)) = (orn $lhs $rhs)
        (add $a (mul $b $c)) = (madd $b $c $a)
        (sub $a (mul $b $c)) = (msub $b $c $a)
    */
    switch (n->type) {
        case TB_ADD: {
            // TB_Node* rhs = n->inputs[2];
            // if (rhs->type == TB_ICONST) {
            //     tb_print_dumb_node(NULL, n);
            //     uint64_t val = TB_NODE_GET_EXTRA_T(rhs, TB_NodeInt)->value;
            //     bool lo12 = val == (val & 07777);
            //     bool hi12 = val == (val & 077770000);
            //     if (lo12 || hi12) {
            //         TB_Node* new = tb_alloc_node(f, A64_ADDIMM, n->dt, 2, sizeof(A64Imm12));
            //         set_input(f, new, n->inputs[1], 1);
            //         TB_NODE_SET_EXTRA(new, A64Imm12, .imm = val);
            //         return new;
            //     }
            // }
            return NULL;
        }

        case TB_ROL: {
            // (rol $lhs $rhs) = (ror $lhs (sub $size $rhs))
            tb_todo();
        }

        // There is no instruction for calculating the remainder.
        // You can do that manually by calculating
        // r = n − (n ÷ d) × d
        case TB_UMOD: {
            // (umod $lhs $rhs) = (msub (udiv $lhs $rhs) $rhs $lhs)
            tb_todo();
        }
        case TB_SMOD: {
            // (umod $lhs $rhs) = (msub (sdiv $lhs $rhs) $rhs $lhs)
            tb_todo();
        }

        default: return NULL;
    }
}

////////////////////////////////
// RA constraints
////////////////////////////////
static bool node_remat(TB_Node* n) {
    return false;
}

static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins) {
    // printf("CONSTRAINT %%%u, %s\n", n->gvn, tb_node_get_name(n->type));
    switch (n->type) {
        ////////////////////////////////
        // CONSTANTS
        ////////////////////////////////
        case TB_ICONST: {
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        ////////////////////////////////
        // PROJECTIONS
        ////////////////////////////////
        case TB_PROJ: {
            if (n->dt.type == TB_TAG_MEMORY || n->dt.type == TB_TAG_CONTROL) {
                return &TB_REG_EMPTY;
            }

            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == TB_ROOT) {
                if (i == 2) { // 2, RPC
                    return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << LR);
                } else { // >2, Params
                    TB_ASSERT(i < 3 + 4); //?? A64 ABI says 8 regs, but 4 for now
                    int mask = 1u << (i - 3); // starts at 3, so i-3
                    int type = TB_IS_FLOAT_TYPE(n->dt)
                        ? REG_CLASS_FPR
                        : REG_CLASS_GPR;
                    return intern_regmask(ctx, type, false, mask);
                }
            } else {
                tb_todo();
            }
        }

        case TB_MACH_FRAME_PTR:
        return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << SP);

        case TB_MACH_TEMP:
        return TB_NODE_GET_EXTRA_T(n, TB_NodeMachTemp)->def;

        case TB_MACH_PROJ:
        return TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->def;

        ////////////////////////////////
        // MISCELLANEOUS
        ////////////////////////////////
        // case TB_POISON:
        // case TB_INLINE_ASM:
        // case TB_CYCLE_COUNTER:
        // case TB_PREFETCH:

        ////////////////////////////////
        // CONTROL
        ////////////////////////////////
        case TB_RETURN: {
            if (ins) {
                CallingConv* cc = ctx->calling_conv;

                ins[1] = &TB_REG_EMPTY;
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << LR);

                TB_FunctionPrototype* proto = ctx->f->prototype;
                TB_ASSERT(proto->return_count <= 2 && "At most 2 return values :(");

                FOR_N(i, 0, proto->return_count) {
                    int mask = 1u << i;
                    int type = TB_IS_FLOAT_TYPE(n->inputs[3 + i]->dt)
                        ? REG_CLASS_FPR
                        : REG_CLASS_GPR;
                    ins[3 + i] = intern_regmask(ctx, type, false, mask);
                }

                size_t k = 3 + proto->return_count;
                ins[k++] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << SP);

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
        // case TB_PHI:
        // case TB_BRANCH:
        // case TB_AFFINE_LATCH:
        // case TB_NEVER_BRANCH:
        // case TB_DEBUGBREAK:
        // case TB_TRAP:
        // case TB_UNREACHABLE:

        ////////////////////////////////
        // CONTROL + MEMORY
        ////////////////////////////////
        // case TB_CALL:
        // case TB_SYSCALL:
        // case TB_TAILCALL:
        // case TB_SAFEPOINT:

        ////////////////////////////////
        // MEMORY
        ////////////////////////////////
        // case TB_LOAD:
        // case TB_STORE:
        // case TB_MEMCPY:
        // case TB_MEMSET:
        // case TB_READ:
        // case TB_WRITE:
        // case TB_ATOMIC_LOAD:
        // case TB_ATOMIC_XCHG:
        // case TB_ATOMIC_ADD:
        // case TB_ATOMIC_AND:
        // case TB_ATOMIC_XOR:
        // case TB_ATOMIC_OR:
        // case TB_ATOMIC_PTROFF:
        // case TB_ATOMIC_CAS:
        // case TB_LOOKUP:

        ////////////////////////////////
        // POINTERS
        ////////////////////////////////
        case TB_SYMBOL: {
            return ctx->normie_mask[REG_CLASS_GPR];
        }
        // case TB_PTR_OFFSET:

        // Conversions
        // case TB_TRUNCATE:
        // case TB_FLOAT_TRUNC:
        // case TB_FLOAT_EXT:
        // case TB_SIGN_EXT:
        // case TB_ZERO_EXT:
        case TB_UINT2FLOAT:
        case TB_INT2FLOAT: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_GPR];
            }
            return ctx->normie_mask[REG_CLASS_FPR];
        }
        case TB_FLOAT2UINT:
        case TB_FLOAT2INT: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_FPR];
            }
            return ctx->normie_mask[REG_CLASS_GPR];
        }
        // case TB_BITCAST:

        case TB_SELECT: {
            /* inputs
                1 condition
                2 truthy
                3 falsey
            */
            if (TB_IS_INTEGER_TYPE(n->dt)) {
                if (ins) {
                    // ins[1] = ;
                    ins[2] = ctx->normie_mask[REG_CLASS_GPR];
                    ins[3] = ctx->normie_mask[REG_CLASS_GPR];
                }
            }
            tb_todo();
        }

        ////////////////////////////////
        // Bitmagic
        ////////////////////////////////
        case TB_BSWAP:
        case TB_CLZ:
        case TB_CTZ:
        case TB_POPCNT: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_GPR];
            }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        case TB_FNEG: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_FPR];
            }
            return ctx->normie_mask[REG_CLASS_FPR];
        }

        ////////////////////////////////
        // Integer arithmetic
        ////////////////////////////////
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:

        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_ROL:
        case TB_ROR:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_GPR];
                ins[2] = ctx->normie_mask[REG_CLASS_GPR];
            }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        ////////////////////////////////
        // Float arithmetic
        ////////////////////////////////
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        case TB_FMIN:
        case TB_FMAX: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_FPR];
                ins[2] = ctx->normie_mask[REG_CLASS_FPR];
            }
            return ctx->normie_mask[REG_CLASS_FPR];
        }

        ////////////////////////////////
        // Comparisons
        ////////////////////////////////
        // case TB_CMP_EQ:
        // case TB_CMP_NE:
        // case TB_CMP_ULT:
        // case TB_CMP_ULE:
        // case TB_CMP_SLT:
        // case TB_CMP_SLE:
        // case TB_CMP_FLT:
        // case TB_CMP_FLE:

        // case TB_FRAME_PTR:

        ////////////////////////////////
        // variadic
        ////////////////////////////////
        // case TB_VA_START:

        ////////////////////////////////
        // general machine nodes
        ////////////////////////////////
        // case TB_MACH_MOVE:
        case TB_MACH_COPY: {
            TB_NodeMachCopy* move = TB_NODE_GET_EXTRA(n);
            if (ins) {
                ins[1] = move->use;
            }
            return move->def;
        }
        // case TB_MACH_JUMP:
        // case TB_MACH_JIT_THREAD_PTR:

        ////////////////////////////////
        // custom machine nodes
        ////////////////////////////////
        case a64_add_imm12: {
            if (ins) {
                ins[1] = ctx->normie_mask[REG_CLASS_GPR];
            }
            return ctx->normie_mask[REG_CLASS_GPR];
        }

        default:
            tb_todo();
            return &TB_REG_EMPTY;
    }
}

static int node_tmp_count(Ctx* restrict ctx, TB_Node* n) {
    return 0;
}

static int node_2addr(TB_Node* n) {
    return -1;
}

////////////////////////////////
// Scheduling
////////////////////////////////
// don't care about functional units on ARM
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n) { return 1; }
static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b) { return false; }

static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end) {
    return 1;
}

////////////////////////////////
// Code emitting
////////////////////////////////
static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {

}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {

}

static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle) {
    TB_ASSERT(bundle->count == 1);
    TB_Node* n = bundle->arr[0];
    // VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];

    #if TB_OPTDEBUG_REGALLOC2
    if (e->has_comments) {
        enum { BUF_SIZE = 1024 };
        char buf[BUF_SIZE];
        int j = 0;

        j += snprintf(buf+j, BUF_SIZE-j, "%%%-3u: ", n->gvn);

        int dst = ctx->vreg_map[n->gvn];
        if (dst > 0) {
            j += snprintf(buf+j, BUF_SIZE-j, "V%-3d", dst);
        } else {
            j += snprintf(buf+j, BUF_SIZE-j, "____");
        }
        j += snprintf(buf+j, BUF_SIZE-j, " = %-14s (", tb_node_get_name(n->type));
        FOR_N(i, 0, n->input_count) {
            int src = n->inputs[i] ? ctx->vreg_map[n->inputs[i]->gvn] : 0;
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

    // printf("EMIT %%%u\n", n->gvn);
    switch (n->type) {
        case TB_MACH_FRAME_PTR: break;

        ////////////////////////////////
        // CONSTANTS
        ////////////////////////////////
        case TB_ICONST: {
            int dst = op_reg_at(ctx, n, REG_CLASS_GPR);
            uint64_t val = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

            bool wide = n->dt.type == TB_TAG_I64;
            int shift = 0;
            MoveOp op = MOVZ;
            while (val) {
                uint16_t imm = val & MASK(16);
                if (imm != 0) {
                    dpimm_movewide(e, wide, op, dst, imm, shift);
                    op = MOVK;
                }
                val >>= 16, shift += 1;
            }
            break;
        }

        ////////////////////////////////
        // PROJECTIONS
        ////////////////////////////////
        case TB_PROJ: break;
        case TB_BRANCH_PROJ: break;
        case TB_MACH_PROJ: break;

        ////////////////////////////////
        // MISCELLANEOUS
        ////////////////////////////////
        case TB_POISON: break;
        // case TB_INLINE_ASM:
        // case TB_CYCLE_COUNTER:
        // case TB_PREFETCH:
        case TB_SYMBOL_TABLE: break;

        ////////////////////////////////
        // CONTROL
        ////////////////////////////////
        // case TB_ROOT:
        case TB_RETURN: {
            control_branch_reg(e, RET, LR);
            break;
        }
        case TB_REGION: break;
        case TB_NATURAL_LOOP: break;
        case TB_AFFINE_LOOP: break;
        case TB_PHI: break;
        // case TB_BRANCH:
        // case TB_AFFINE_LATCH:
        // case TB_NEVER_BRANCH:
        case TB_ENTRY_FORK: break;
        // case TB_DEBUGBREAK:
        // case TB_TRAP:
        case TB_UNREACHABLE: break;
        // case TB_DEAD:

        ////////////////////////////////
        // CONTROL + MEMORY
        ////////////////////////////////
        // case TB_CALL:           // (Control, Memory, Ptr, Data...) -> (Control, Memory, Data)
        // case TB_SYSCALL:        // (Control, Memory, Ptr, Data...) -> (Control, Memory, Data)
        // case TB_TAILCALL:       // (Control, Memory, RPC, Data, Data...) -> ()
        // case TB_DEBUG_LOCATION: // (Control, Memory) -> (Control, Memory)
        // case TB_SAFEPOINT:      // (Control, Memory, Node, Data...) -> (Control)
        case TB_CALLGRAPH: break;
        // case TB_DEBUG_SCOPES:   // (Parent, Control...)

        ////////////////////////////////
        // MEMORY
        ////////////////////////////////
        // case TB_SPLITMEM:    // (Memory) -> (Memory...)
        // case TB_MERGEMEM:    // (Split, Memory...) -> Memory
        // case TB_LOAD:        // (Control?, Memory, Ptr)      -> Data
        // case TB_STORE:       // (Control, Memory, Ptr, Data) -> Memory
        // case TB_MEMCPY:      // (Control, Memory, Ptr, Ptr, Size)  -> Memory
        // case TB_MEMSET:      // (Control, Memory, Ptr, Int8, Size) -> Memory
        // case TB_READ:        // (Control, Memory, Ptr)       -> (Memory, Data)
        // case TB_WRITE:       // (Control, Memory, Ptr, Data) -> (Memory, Data)
        // case TB_ATOMIC_LOAD:   // (Control, Memory, Ptr)        -> (Memory, Data)
        // case TB_ATOMIC_XCHG:   // (Control, Memory, Ptr, Data)  -> (Memory, Data)
        // case TB_ATOMIC_ADD:    // (Control, Memory, Ptr, Data)  -> (Memory, Data)
        // case TB_ATOMIC_AND:    // (Control, Memory, Ptr, Data)  -> (Memory, Data)
        // case TB_ATOMIC_XOR:    // (Control, Memory, Ptr, Data)  -> (Memory, Data)
        // case TB_ATOMIC_OR:     // (Control, Memory, Ptr, Data)  -> (Memory, Data)
        // case TB_ATOMIC_PTROFF: // (Control, Memory, Ptr, Ptr)   -> (Memory, Ptr)
        // case TB_ATOMIC_CAS:    // (Control, Memory, Data, Data) -> (Memory, Data, Bool)

        // case TB_LOOKUP:

        ////////////////////////////////
        // POINTERS
        ////////////////////////////////
        // case TB_LOCAL:         // () & (Int, Int) -> Ptr
        // case TB_SYMBOL:        // () & case TB_Symbol: -> Ptr
        // case TB_PTR_OFFSET:    // (Ptr, Int) -> Ptr

        // Conversions
        // case TB_TRUNCATE:
        // case TB_FLOAT_TRUNC:
        // case TB_FLOAT_EXT:
        // case TB_SIGN_EXT:
        // case TB_ZERO_EXT:
        case TB_UINT2FLOAT:
        case TB_FLOAT2UINT:
        case TB_INT2FLOAT:
        case TB_FLOAT2INT: {
            static_assert(TB_UINT2FLOAT + 1 == TB_FLOAT2UINT && TB_FLOAT2UINT + 1 == TB_INT2FLOAT && TB_INT2FLOAT + 1 == TB_FLOAT2INT, "enum values not consecutive");
            uint8_t  type1 = n->dt.type;
            uint8_t  type2 = n->inputs[1]->dt.type;
            bool      wide = type1 == TB_TAG_I64 || type2 == TB_TAG_I64;
            FPtype    type = type1 == TB_TAG_F64 || type2 == TB_TAG_F64 ? Double : Single;
            FPconvINTop op = (FPconvINTop[]){UCVTF, FCVTZU, SCVTF, FCVTZS}[n->type - TB_UINT2FLOAT];
            // float<>int direction is encoded as parity due to the enum order
            int class_lut[] = {REG_CLASS_GPR, REG_CLASS_FPR};
            int class = n->type - TB_UINT2FLOAT;
            int   dst = op_reg_at(ctx, n,            class_lut[(class + 1) & 1]);
            int   src = op_reg_at(ctx, n->inputs[1], class_lut[(class + 0) & 1]);
            simd_dp_float2int(e, wide, type, op, dst, src, false);
            break;
        }
        // case TB_BITCAST:

        ////////////////////////////////
        // Select
        ////////////////////////////////
        case TB_SELECT: {
            //!! negate says this is borked so im just gonna todo it
            tb_todo();
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[3], REG_CLASS_GPR);
            bool wide = n->dt.type == TB_TAG_I64;
            Condition cc = 0;
            TB_Node* cond = n->inputs[1];
            if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
                switch (cond->type) {
                    case TB_CMP_EQ:  cc = EQ; break;
                    case TB_CMP_NE:  cc = NE; break;
                    case TB_CMP_SLT: cc = LT; break;
                    case TB_CMP_SLE: cc = LE; break;
                    case TB_CMP_ULT: cc = CC; break;
                    case TB_CMP_ULE: cc = CS; break;
                    case TB_CMP_FLT: tb_todo(); break;
                    case TB_CMP_FLE: tb_todo(); break;
                    default: tb_unreachable();
                }
            } else {
                // emit cmp instruction
                tb_todo();
            }
            dpreg_condsel(e, wide, CSEL, cc, dst, lhs, rhs);
            break;
        }

        ////////////////////////////////
        // Bitmagic
        ////////////////////////////////
        case TB_BSWAP:
        case TB_CLZ:
        case TB_CTZ:
        case TB_POPCNT: {
            static_assert(TB_BSWAP + 1 == TB_CLZ && TB_CLZ + 1 == TB_CTZ && TB_CTZ + 1 == TB_POPCNT, "enum values not consecutive");
            bool wide = n->dt.type == TB_TAG_I64;
            DPR1op op = (DPR1op[4]){REV, CLZ, CTZ, CNT}[n->type - TB_BSWAP];
            int   dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int   src = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            dpreg_dp_1src(e, wide, op, dst, src);
            break;
        }

        ////////////////////////////////
        // Unary operations
        ////////////////////////////////
        case TB_FNEG: {
            int type = n->dt.type == TB_TAG_F32 ? Single : Double;
            int  dst = op_reg_at(ctx, n,            REG_CLASS_FPR);
            int  src = op_reg_at(ctx, n->inputs[1], REG_CLASS_FPR);
            simd_dp_floatdp1(e, type, FNEG, dst, src);
            break;
        }

        ////////////////////////////////
        // Integer arithmatic
        ////////////////////////////////
        case TB_AND:
        case TB_OR:
        case TB_XOR: {
            static_assert(TB_AND + 1 == TB_OR && TB_OR + 1 == TB_XOR, "enum values not consecutive");
            bool  wide = n->dt.type == TB_TAG_I64;
            LogicOp op = (LogicOp[]){AND, ORR, EOR}[n->type - TB_AND];
            int    dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int    lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int    rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            dpreg_log_shift(e, wide, op, dst, lhs, rhs, 0, 0);
            break;
        }

        case TB_ADD:
        case TB_SUB: {
            static_assert(TB_ADD + 1 == TB_SUB, "enum values not consecutive");
            bool wide = n->dt.type == TB_TAG_I64;
            AddOp  op = (AddOp[]){ADD, SUB}[n->type - TB_ADD];
            int   dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int   lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int   rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            dpreg_addsub_shift(e, wide, op, dst, lhs, rhs, 0, 0, false);
            break;
        }

        case TB_MUL: {
            bool wide = n->dt.type == TB_TAG_I64;
            int   dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int   lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int   rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            dpreg_dp_3src(e, wide, MADD, dst, lhs, rhs, ZR);
            break;
        }

        case TB_ROL: tb_unreachable(); break; // a64 doesn't have rotate left
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_ROR:
        case TB_UDIV:
        case TB_SDIV: {
            static_assert(TB_SHL + 1 == TB_SHR && TB_SHR + 1 == TB_SAR && TB_SAR + 1 == TB_ROL && TB_ROL + 1 == TB_ROR && TB_ROR + 1 == TB_UDIV && TB_UDIV + 1 == TB_SDIV, "enum values not consecutive");
            bool wide = n->dt.type == TB_TAG_I64;
            DPR2op op = (DPR2op[]){LSLV, LSRV, ASRV, RORV, RORV, UDIV, SDIV}[n->type - TB_SHL];
            int   dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int   lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int   rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            dpreg_dp_2src(e, wide, op, dst, lhs, rhs);
            break;
        }

        case TB_UMOD:
        case TB_SMOD: tb_unreachable(); break;

        ////////////////////////////////
        // Float arithmatic
        ////////////////////////////////
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        case TB_FMIN:
        case TB_FMAX: {
            static_assert(TB_FADD + 1 == TB_FSUB && TB_FSUB + 1 == TB_FMUL && TB_FMUL + 1 == TB_FDIV && TB_FDIV + 1 == TB_FMIN && TB_FMIN + 1 == TB_FMAX, "enum values not consecutive");
            FPtype type = n->dt.type == TB_TAG_F64 ? Double : Single;
            FPDP2op  op = (FPDP2op[]){FADD, FSUB, FMUL, FDIV, FMIN, FMAX}[n->type - TB_FADD];
            int     dst = op_reg_at(ctx, n,            REG_CLASS_FPR);
            int     lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_FPR);
            int     rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_FPR);
            simd_dp_floatdp2(e, type, op, dst, lhs, rhs);
            break;
        }

        ////////////////////////////////
        // Comparisons
        ////////////////////////////////
        // case TB_CMP_EQ:
        // case TB_CMP_NE:
        // case TB_CMP_ULT:
        // case TB_CMP_ULE:
        // case TB_CMP_SLT:
        // case TB_CMP_SLE:
        // case TB_CMP_FLT:
        // case TB_CMP_FLE:

        // case TB_FRAME_PTR:

        ////////////////////////////////
        // variadic
        ////////////////////////////////
        // case TB_VA_START:

        ////////////////////////////////
        // general machine nodes:
        ////////////////////////////////
        case TB_MACH_COPY: {
            TB_Node* dst = n;
            TB_Node* src = n->inputs[1];
            VReg* vdst = node_vreg(ctx, dst);
            VReg* vsrc = node_vreg(ctx, src);
            if (vdst->class == REG_CLASS_GPR && vsrc->class == REG_CLASS_GPR) {
                bool wide = true;
                dpreg_log_shift(e, wide, ORR, vdst->assigned, vsrc->assigned, ZR, 0, 0);
            } else
            if (vdst->class == REG_CLASS_FPR && vsrc->class == REG_CLASS_FPR) {
                FPtype type = src->dt.type == TB_TAG_F64 ? Double : Single;
                simd_dp_floatdp1(e, type, FMOV, vdst->assigned, vsrc->assigned);
            } else
            if (vdst->class == REG_CLASS_FPR && vsrc->class == REG_CLASS_GPR) {
                bool   wide = src->dt.type == TB_TAG_I64;
                FPtype type = dst->dt.type == TB_TAG_F64 ? Double : Single;
                simd_dp_float2int(e, wide, type, FMOV_I2F, vdst->assigned, vsrc->assigned, false);
            } else
            if (vdst->class == REG_CLASS_GPR && vsrc->class == REG_CLASS_FPR) {
                bool   wide = src->dt.type == TB_TAG_I64;
                FPtype type = dst->dt.type == TB_TAG_F64 ? Double : Single;
                simd_dp_float2int(e, wide, type, FMOV_F2I, vdst->assigned, vsrc->assigned, false);
            }
            break;
        }
        // case TB_MACH_JUMP:
        // case TB_MACH_FRAME_PTR:
        // case TB_MACH_JIT_THREAD_PTR:
        // case TB_MACH_SYMBOL:

        ////////////////////////////////
        // custom machine nodes
        ////////////////////////////////
        case a64_add_imm12: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            uint64_t imm = TB_NODE_GET_EXTRA_T(n, A64Op)->imm;
            int shift = 0;
            if (imm == (imm & 077770000)) {
                shift = 12;
                imm >>= shift;
            }

            bool wide = n->dt.type == TB_TAG_I64;
            dpimm_addsub_imm(e, wide, ADD, dst, lhs, imm, shift, false);
            break;
        }

        default: {
            tb_todo();
            break;
        }
    }
}

static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb) {

}

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    return 0;
}

////////////////////////////////
// Disassembly
////////////////////////////////
#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void print_gpr_sp(TB_CGEmitter* e, int i, bool is_64bit) {
    if (i == ZR) {
        E("sp");
    } else if (i == LR) {
        E("lr");
    } else {
        E("%c%d", is_64bit["wx"], i);
    }
}

static void print_gpr(TB_CGEmitter* e, int i, bool is_64bit) {
    if (i == ZR) {
        E("zr");
    } else if (i == LR) {
        E("lr");
    } else {
        E("%c%d", is_64bit["wx"], i);
    }
}

static void dump_stack_layout(Ctx* restrict ctx, TB_CGEmitter* e) {

}

static bool sym_handler(TB_Disasm* disasm, int inst_length, uint64_t field, int field_pos, int field_len, bool is_offset) {
    return false;
}

ICodeGen tb__aarch64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .can_gvn = can_gvn,
    .global_init = global_init,
    .node_name = node_name,
    .print_extra = print_extra,
    .flags = node_flags,
    .extra_bytes = extra_bytes,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};
#endif

/* condensed list of things to handle :)
    TB_NULL = 0,
    // CONSTANTS
    TB_ICONST, TB_F32CONST, TB_F64CONST,
    // PROJECTIONS
    TB_PROJ, TB_BRANCH_PROJ, TB_MACH_PROJ,
    // MISCELLANEOUS
    TB_POISON, TB_INLINE_ASM, TB_CYCLE_COUNTER, TB_PREFETCH, TB_SYMBOL_TABLE,
    // CONTROL
    TB_ROOT, TB_RETURN,
    TB_REGION, TB_NATURAL_LOOP, TB_AFFINE_LOOP,
    TB_PHI, TB_BRANCH, TB_AFFINE_LATCH, TB_NEVER_BRANCH, TB_ENTRY_FORK,
    TB_DEBUGBREAK, TB_TRAP, TB_UNREACHABLE, TB_DEAD,
    // CONTROL + MEMORY
    TB_CALL, TB_SYSCALL, TB_TAILCALL, TB_DEBUG_LOCATION, TB_SAFEPOINT, TB_CALLGRAPH, TB_DEBUG_SCOPES,
    // MEMORY
    TB_SPLITMEM, TB_MERGEMEM,
    TB_LOAD, TB_STORE,
    TB_MEMCPY, TB_MEMSET,
    TB_READ, TB_WRITE,
    TB_ATOMIC_LOAD, TB_ATOMIC_XCHG, TB_ATOMIC_ADD, TB_ATOMIC_AND, TB_ATOMIC_XOR, TB_ATOMIC_OR, TB_ATOMIC_PTROFF, TB_ATOMIC_CAS,
    // like a multi-way branch but without the control flow aspect, but for data.
    TB_LOOKUP,
    // POINTERS
    TB_LOCAL, TB_SYMBOL, TB_PTR_OFFSET,
    // Conversions
    TB_TRUNCATE, TB_FLOAT_TRUNC, TB_FLOAT_EXT, TB_SIGN_EXT, TB_ZERO_EXT, TB_UINT2FLOAT, TB_FLOAT2UINT, TB_INT2FLOAT, TB_FLOAT2INT, TB_BITCAST,
    // Select
    TB_SELECT,
    // Bitmagic
    TB_BSWAP, TB_CLZ, TB_CTZ, TB_POPCNT,
    // Unary operations
    TB_FNEG,
    // Integer arithmatic
    TB_AND, TB_OR, TB_XOR, TB_ADD, TB_SUB, TB_MUL,
    TB_SHL, TB_SHR, TB_SAR, TB_ROL, TB_ROR, TB_UDIV, TB_SDIV, TB_UMOD, TB_SMOD,
    // Float arithmatic
    TB_FADD, TB_FSUB, TB_FMUL, TB_FDIV, TB_FMIN, TB_FMAX,
    // Comparisons
    TB_CMP_EQ, TB_CMP_NE, TB_CMP_ULT, TB_CMP_ULE, TB_CMP_SLT, TB_CMP_SLE, TB_CMP_FLT, TB_CMP_FLE,
    TB_FRAME_PTR,
    // Special ops
    TB_ADC, TB_UDIVMOD, TB_SDIVMOD, TB_MULPAIR,
    // variadic
    TB_VA_START,
    // x86 intrinsics
    TB_X86INTRIN_LDMXCSR, TB_X86INTRIN_STMXCSR, TB_X86INTRIN_SQRT, TB_X86INTRIN_RSQRT,
    // general machine nodes:
    TB_MACH_MOVE, TB_MACH_COPY, TB_MACH_JUMP, TB_MACH_FRAME_PTR, TB_MACH_JIT_THREAD_PTR, TB_MACH_SYMBOL,
    // limit on generic nodes
    TB_NODE_TYPE_MAX,
    // each family of machine nodes gets 256 nodes
    // first machine op, we have some generic ops here:
    TB_MACH_X86  = TB_ARCH_X86_64  * 0x100,
    TB_MACH_A64  = TB_ARCH_AARCH64 * 0x100,
    TB_MACH_MIPS = TB_ARCH_MIPS32  * 0x100,
*/
