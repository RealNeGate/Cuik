#ifdef TB_HAS_MIPS64
#include "../emitter.h"
#include "../tb_internal.h"
#include "mips_codec.c"

// Brings in all the glue to our codegen functions
#include "mips_gen.h"

typedef struct {
    int32_t imm;
} MIPSImm;

// machine node types
typedef enum MIPSNodeType {
    #define R(name, op, funct, ty) mips_ ## name,
    #define I(name, op, ty)        mips_ ## name,
    #define J(name, op, ty)        mips_ ## name,
    #include "mips_insts.inc"
} MIPSNodeType;

static CallingConv CC_M64PCS = {
    .sp_class  = REG_CLASS_GPR, .sp_reg  = SP, // stack pointer
    .fp_class  = REG_CLASS_GPR, .fp_reg  = FP, // frame pointer
    .rpc_class = REG_CLASS_GPR, .rpc_reg = RA, // return PC
    .flexible_param_alloc = true,

    // volatile/clobbered (caller saved) 'C'
    // non-volatile (callee saved) 'c'
    .reg_saves[REG_CLASS_GPR] = (char[32]){
        // [0] = 'c',
        [1  ... 15] = 'C', // AT, V, A, T
        [16 ... 23] = 'c', // S
        [24 ... 25] = 'C', // T
        [26 ... 27] = 'c', // K
        // [29 ... 30] = 'c',
        [29 ... 31] = 'C',
    },

    // TODO calling convention needs float pairs (low and high 64bit)
    // or something that's caller and callee saved at the same time
    .reg_saves[REG_CLASS_FPR] = (char[32]){
        // volatile/clobbered (caller saved) 'C'
        [0 ...  19] = 'C',
        // non-volatile (callee saved) 'c'
        [20 ... 31] = 'c',
    },

    .param_count = { [REG_CLASS_GPR] = 8, [REG_CLASS_FPR] = 8 },
    .params[REG_CLASS_GPR] = (uint8_t[]){ A0, A1, A2, A3, A4, A5, A6, A7 },
    .params[REG_CLASS_FPR] = (uint8_t[]){ 0, 1, 2, 3, 4, 5, 6, 7 },

    .ret_count = { [REG_CLASS_GPR] = 2, [REG_CLASS_FPR] = 2 },
    .rets[REG_CLASS_GPR] = { V0, V1 },
    .rets[REG_CLASS_FPR] = { 0,  1 },
};

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    mach_dsl_init(ctx, abi);
    ctx->calling_conv = &CC_M64PCS;

    uint32_t not_tmps = (1 << ZR) | (1 << AT) | (1 << SP) | (1 << K0) | (1 << K1) | (1 << GP);
    ctx->all_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, 0xFFFFFFFF);
    ctx->all_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, 0xFFFFFFFF);

    ctx->normie_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, UINT32_MAX & ~not_tmps);
    ctx->normie_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, 0xFFFFFFFF);

    ctx->mayspill_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, true, UINT32_MAX & ~not_tmps);
    ctx->mayspill_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, true, 0xFFFFFFFF);

    uint32_t volatile_gprs = (((UINT32_MAX >> 7) << 1) & ~(0xFF << S0)) | (1u << RA);
    for (size_t i = 0; i < 32; i++) {
        printf("VOL[%zu]: %d\n", i, (volatile_gprs >> i) & 1);
    }
}

static void print_extra(OutStream* s, TB_Node* n) {
    // A64Op* op = TB_NODE_GET_EXTRA(n);
    // s_writef(s, "imm=%d", op->imm);
}

static void node_add_tmps(Ctx* restrict ctx, TB_Node* n) {

}

static int node_constraint_kill(Ctx* restrict ctx, TB_Node* n, RegMask** kills) {
    return 0;
}

static void print_pretty(Ctx* restrict ctx, TB_Node* n) {
    tb_print_dumb_node(NULL, n);
}

////////////////////////////////
// RA constraints
////////////////////////////////
static bool node_remat(TB_Node* n) {
    return false;
}

static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins) {

    switch (n->type) {
        ////////////////////////////////
        // UNNECESSARY
        ////////////////////////////////
        case TB_REGION:
        case TB_NATURAL_LOOP:
        case TB_AFFINE_LOOP:
        if (ins) {
            FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
        }
        return &TB_REG_EMPTY;

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
                    return intern_regmask2(ctx, REG_CLASS_GPR, false, RA);
                } else { // >2, Params
                    TB_ASSERT(i < 3 + 4); //?? A64 ABI says 8 regs, but 4 for now
                    int type = TB_IS_FLOAT_TYPE(n->dt)
                        ? REG_CLASS_FPR
                        : REG_CLASS_GPR;
                    // starts at 3, so i-3
                    return intern_regmask2(ctx, type, false, i - 3);
                }
            } else {
                if (n->inputs[0]->type == TB_CALL) {
                    return intern_regmask2(ctx, REG_CLASS_GPR, false, 0);
                }
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
        case TB_PHI: {
            if (n->dt.type == TB_TAG_MEMORY) {
                if (ins) {
                    FOR_N(i, 1, n->input_count) { ins[i] = &TB_REG_EMPTY; }
                }
                return &TB_REG_EMPTY;
            }

            RegMask* rm = TB_IS_VECTOR_TYPE(n->dt) || TB_IS_FLOAT_TYPE(n->dt) ? ctx->normie_mask[REG_CLASS_FPR] : ctx->normie_mask[REG_CLASS_GPR];
            if (ins) {
                FOR_N(i, 1, n->input_count) { ins[i] = rm; }
            }
            return rm;
        }

        case TB_RETURN: {
            if (ins) {
                CallingConv* cc = ctx->calling_conv;

                ins[1] = &TB_REG_EMPTY;
                ins[2] = intern_regmask2(ctx, REG_CLASS_GPR, false, RA);

                TB_FunctionPrototype* proto = ctx->f->prototype;
                TB_ASSERT(proto->return_count <= 2 && "At most 2 return values :(");

                FOR_N(i, 0, proto->return_count) {
                    int type = TB_IS_FLOAT_TYPE(n->inputs[3 + i]->dt)
                        ? REG_CLASS_FPR
                        : REG_CLASS_GPR;

                    int mask = 1u << ((type == REG_CLASS_GPR ? V0 : 0) + i);
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
        case TB_CALL: {
            // (Control, Memory, Ptr, Data...) -> (Control, Memory, Data)
            if (ins) {
                // control
                ins[1] = &TB_REG_EMPTY;
                // memory
                ins[2] = ctx->normie_mask[REG_CLASS_GPR];
                // ptr
                ins[3] = ctx->normie_mask[REG_CLASS_GPR];
                // data
                int ngrn = 0; // next gpr number
                int nsrn = 0; // next fpr and simd number
                int nprn = 0; // next scalable predicate number
                int nsaa = 0; //?? next stack number (? how does the link and frame pointer look)
                //?? stage B - pre-padding and extension of arguments
                //!! TODO: composite types
                // stage C - assignment of args to registers and stack
                FOR_N(i, 4, n->input_count) {
                    int type = n->inputs[i]->dt.type;
                    bool is_single = type == TB_TAG_F32;
                    bool is_double = type == TB_TAG_F64;
                    bool is_integral = type >= TB_TAG_BOOL && type <= TB_TAG_I64;
                    bool is_pointer = type == TB_TAG_PTR;
                    bool is_float = is_single || is_double;
                    // C.1 float or vector and nsrn < 8 then allocate stack reg
                    if (is_float && nsrn < 8) {
                        ins[i] = intern_regmask2(ctx, REG_CLASS_FPR, false, nsrn);
                        nsrn += 1;
                        continue;
                    }
                    // C.2 aggr and nsrn + size <= 8 then allocate size float regs
                    // C.3 aggr then nsrn = 8
                    // C.4 aggr, quad, vec then round stack to 1 if align <= 8 or 2 if align >= 16
                    //?? C.5 half, single then size = 8 (? stack reg already word sized)
                    //!! C.6 aggr, float, vec then allocate enough stack regs
                    if (is_float) {
                        ins[i] = intern_regmask2(ctx, REG_CLASS_STK, false, nsaa);
                        nsaa += 1;
                        continue;
                    }
                    // C.7 it's complicated (vectors)
                    // C.8 pure scalable type then memory & use pointer
                    // C.9 integral, pointer then allocate int reg
                    if ((is_integral || is_pointer) && ngrn < 8) {
                        ins[i] = intern_regmask2(ctx, REG_CLASS_GPR, false, ngrn);
                        ngrn += 1;
                        continue;
                    }
                    // C.10 alignment 16 then align ngrn up to even
                    // C.11 16 byte integral
                    // C.12 composite
                    // C.13
                    ngrn = 8;
                    // C.14 round nsaa to nearest multiple of max(8, input's alignment)
                    // but stack regs are word-sized
                    // C.15 composite
                    // C.16 if size < 8 then size = 8
                    // C.17
                    ins[i] = intern_regmask2(ctx, REG_CLASS_STK, false, nsaa);
                    nsaa += 1;
                    continue;

                    //OLD idk if good or bad
                    // if (TB_IS_FLOAT_TYPE(n->inputs[i]->dt)) {
                    //     if (nsrn < 8) {
                    //         ins[i] = intern_regmask2(ctx, REG_CLASS_FPR, false, nsrn++);
                    //     } else {
                    //         ins[i] = intern_regmask2(ctx, REG_CLASS_STK, false, nsaa++);
                    //     }
                    // } else {
                    //     if (ngrn < 8) {
                    //         ins[i] = intern_regmask2(ctx, REG_CLASS_GPR, false, ngrn++);
                    //     } else {
                    //         ins[i] = intern_regmask2(ctx, REG_CLASS_STK, false, nsaa++);
                    //     }
                    // }
                }
                //!! TODO: something about context and call usage maybe
            }
            return &TB_REG_EMPTY;
        }
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

static int node_latency(TB_Function* f, TB_Node* n, int i) {
    return 1;
}

////////////////////////////////
// Code emitting
////////////////////////////////
static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {

}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {

}

static bool node_peephole(Ctx* restrict ctx, TB_Node* n, TB_BasicBlock* bb, int i) {
    return false;
}

static void stub_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_CodeStub* stub) {

}

static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle) {
    TB_ASSERT(bundle->count == 1);
    TB_Node* n = bundle->arr[0];

    printf("TODO: ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
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
static void dump_stack_layout(Ctx* restrict ctx, TB_CGEmitter* e) {

}

static bool sym_handler(TB_Disasm* disasm, int inst_length, uint64_t field, int field_pos, int field_len, bool is_offset) {
    return false;
}

// SLP
int is_pack_op_supported(TB_Function* f, TB_DataType dt, TB_Node* n, int width) {
    return false;
}

int max_pack_width_for_op(TB_Function* f, TB_DataType dt, TB_Node* n) {
    return 1;
}

ICodeGen tb__mips64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .global_init = global_init,
    .print_extra = print_extra,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .compile_function   = compile_function,
};

#endif
