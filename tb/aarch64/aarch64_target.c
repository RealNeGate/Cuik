// #ifdef TB_HAS_AARCH64
#include "../emitter.h"
#include "../tb_internal.h"
#include "aarch64_emitter.h"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_FPR,
    REG_CLASS_COUNT,
};

enum {
    FUNCTIONAL_UNIT_COUNT = 1,
    BUNDLE_INST_MAX = 1,
};

// Brings in all the glue to our codegen functions
#include "../codegen_impl.h"

typedef enum A64NodeType {
    A64_ADDIMM = TB_MACH_A64,
} A64NodeType;

typedef struct {
    uint64_t imm;
} A64Imm12;

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->num_regs[REG_CLASS_FPR] = 32;

    uint32_t all_gprs = UINT32_MAX & ~(1u << SP);
    ctx->normie_mask[REG_CLASS_GPR] = new_regmask(ctx->f, REG_CLASS_GPR, false, all_gprs);
    ctx->normie_mask[REG_CLASS_FPR] = new_regmask(ctx->f, REG_CLASS_FPR, false, 0xFFFFFFFF);
}

static void print_extra(TB_Node* n) {

}

static const char* node_name(int type) {
    switch (type) {
        case A64_ADDIMM: return "add imm";
        default: return "???";
    }
}

static bool can_gvn(TB_Node* n) {
    return true;
}

static size_t extra_bytes(TB_Node* n) {
    switch (n->type) {
        case A64_ADDIMM: return sizeof(A64Imm12);

        default: tb_todo();
    }
}

static uint32_t node_flags(TB_Node* n) {
    return 0;
}

////////////////////////////////
// Instruction selection
////////////////////////////////
static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n) {
    /* patterns
        (xor -1 $a) = (not $a)
        (sub 0 $a) = (neg $a)
        (xor $a (not $b)) = (eon $a $b)
        (and $a (not $b)) = (andn $a $b)
        (or $a (not $b)) = (orn $a $b)
        (add $a (mul $b $c)) = (madd $b $c $a)
        (sub $a (mul $b $c)) = (msub $b $c $a)
        (neg (mul $a $b)) = (msub $a $b zr)
        (umod $a $b) = (sub $a (mul $b (udiv $a $b))) = (msub $b (udiv $a $b) $a)
    */
    switch (n->type) {
        case TB_ROOT: {
            TB_Node* ret = n->inputs[1];
            if (ret->type == TB_RETURN) {
                return NULL;
            }
            return NULL;
        }

        case TB_PHI: {
            tb_todo();
        }

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

        // There is no instruction for calculating the remainder.
        // You can do that manually by calculating
        // r = n − (n ÷ d) × d
        case TB_UMOD: {
            // (umod $a $b) = (msub (udiv $a $b) $b $a)
            tb_todo();
            break;
        }
        case TB_SMOD: {
            // (umod $a $b) = (msub (sdiv $a $b) $b $a)
            tb_todo();
            break;
        }

        default:
        return NULL;
    }
}

////////////////////////////////
// RA constraints
////////////////////////////////
static bool node_remat(TB_Node* n) {
    return false;
}

static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins) {
    printf("CONSTRAINT %%%u, %s\n", n->gvn, tb_node_get_name(n->type));
    switch (n->type) {
        ////////////////////////////////
        // CONSTANTS
        ////////////////////////////////
        case TB_ICONST: {
            return ctx->normie_mask[REG_CLASS_GPR];
        }
        case TB_F32CONST:
        case TB_F64CONST: {
            return ctx->normie_mask[REG_CLASS_FPR];
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
                    TB_ASSERT(i < 3+4); // idk bro
                    return intern_regmask(ctx, REG_CLASS_GPR, false, 1u << (i - 3));
                }
            } else {
                tb_todo();
            }
        }
        // case TB_MACH_PROJ:

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
                ins[1] = &TB_REG_EMPTY;
                ins[2] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << LR);

                TB_FunctionPrototype* proto = ctx->f->prototype;
                TB_ASSERT(proto->return_count <= 2 && "At most 2 return values :(");

                if (proto->return_count >= 1) {
                    ins[3] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << X0);
                }

                if (proto->return_count >= 2) {
                    ins[4] = intern_regmask(ctx, REG_CLASS_GPR, false, 1u << X1);
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
        // case TB_SYMBOL:
        // case TB_PTR_OFFSET:

        // Conversions
        // case TB_TRUNCATE:
        // case TB_FLOAT_TRUNC:
        // case TB_FLOAT_EXT:
        // case TB_SIGN_EXT:
        // case TB_ZERO_EXT:
        // case TB_UINT2FLOAT:
        // case TB_FLOAT2UINT:
        // case TB_INT2FLOAT:
        // case TB_FLOAT2INT:
        // case TB_BITCAST:

        case TB_SELECT: {
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
        // Special ops
        ////////////////////////////////
        // case TB_ADC:
        // case TB_UDIVMOD:
        // case TB_SDIVMOD:
        // case TB_MULPAIR:

        ////////////////////////////////
        // variadic
        ////////////////////////////////
        // case TB_VA_START:

        ////////////////////////////////
        // general machine nodes
        ////////////////////////////////
        // case TB_MACH_MOVE:
        // case TB_MACH_COPY:
        // case TB_MACH_JUMP:
        // case TB_MACH_JIT_THREAD_PTR:
        
        ////////////////////////////////
        // custom machine nodes
        ////////////////////////////////
        // case A64_ADDIMM: {
        //     if (ins) {
        //         ins[1] = ctx->normie_mask[REG_CLASS_GPR];
        //     }
        //     return ctx->normie_mask[REG_CLASS_GPR];
        // }

        default:
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

    printf("EMIT %%%u\n", n->gvn);
    switch (n->type) {
        ////////////////////////////////
        // CONSTANTS
        ////////////////////////////////
        case TB_ICONST: {
            int dst = op_reg_at(ctx, n, REG_CLASS_GPR);
            uint64_t val = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;
            
            bool is_64bit = n->dt.type == TB_TAG_I64;
            int shift = 0;
            MoveOp op = MOVZ;
            while (val) {
                if (val & 0xFFFF) {
                    emit_dpi_mov(e, op, dst, val & 0xFFFF, shift, is_64bit);
                    // emit_movimm(e, dst, val & 0xFFFF, shift / 16, is_64bit, first);
                    op = MOVK;
                }
                val >>= 16, shift += 1;
            }
            break;
        }
        // case TB_F32CONST:
        // case TB_F64CONST:

        ////////////////////////////////
        // PROJECTIONS
        ////////////////////////////////
        case TB_PROJ: {} break;
        // case TB_BRANCH_PROJ:
        // case TB_MACH_PROJ:

        ////////////////////////////////
        // MISCELLANEOUS
        ////////////////////////////////
        // case TB_POISON:
        // case TB_INLINE_ASM:
        // case TB_CYCLE_COUNTER:
        // case TB_PREFETCH:
        // case TB_SYMBOL_TABLE:

        ////////////////////////////////
        // CONTROL
        ////////////////////////////////
        // case TB_ROOT:
        case TB_RETURN: {
            emit_ret(e, LR);
        } break;
        case TB_REGION:
        case TB_NATURAL_LOOP:
        case TB_AFFINE_LOOP:
        case TB_PHI: {} break;
        // case TB_BRANCH:
        // case TB_AFFINE_LATCH:
        // case TB_NEVER_BRANCH:
        // case TB_ENTRY_FORK:
        // case TB_DEBUGBREAK:
        // case TB_TRAP:
        // case TB_UNREACHABLE:
        // case TB_DEAD:

        ////////////////////////////////
        // CONTROL + MEMORY
        ////////////////////////////////
        // case TB_CALL:           // (Control, Memory, Ptr, Data...) -> (Control, Memory, Data)
        // case TB_SYSCALL:        // (Control, Memory, Ptr, Data...) -> (Control, Memory, Data)
        // case TB_TAILCALL:       // (Control, Memory, RPC, Data, Data...) -> ()
        // case TB_DEBUG_LOCATION: // (Control, Memory) -> (Control, Memory)
        // case TB_SAFEPOINT:      // (Control, Memory, Node, Data...) -> (Control)
        case TB_CALLGRAPH: {} break;
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
        // case TB_UINT2FLOAT:
        // case TB_FLOAT2UINT:
        // case TB_INT2FLOAT:
        // case TB_FLOAT2INT:
        // case TB_BITCAST:

        ////////////////////////////////
        // Select
        ////////////////////////////////
        case TB_SELECT: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[3], REG_CLASS_GPR);
            bool is64bit = n->dt.type == TB_TAG_I64;
            Cond cc = 0;
            TB_Node* cond = n->inputs[1];
            if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
                switch (cond->type) {
                    case TB_CMP_EQ:  cc = EQ;  break;
                    case TB_CMP_NE:  cc = NE; break;
                    case TB_CMP_SLT: cc = LT;  break;
                    case TB_CMP_SLE: cc = LE; break;
                    case TB_CMP_ULT: cc = CC;  break;
                    case TB_CMP_ULE: cc = CS; break;
                    case TB_CMP_FLT: tb_todo();  break;
                    case TB_CMP_FLE: tb_todo(); break;
                    default: tb_unreachable();
                }
            } else {
                // emit cmp instruction
                tb_todo();
            }
            emit_cs(e, CSEL, dst, lhs, rhs, cc, is64bit);
        } break;

        ////////////////////////////////
        // Bitmagic
        ////////////////////////////////
        case TB_BSWAP: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_1(e, REV, dst, lhs, is_64bit);
        } break;
        case TB_CLZ: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_1(e, CLZ, dst, lhs, is_64bit);
        } break;
        case TB_CTZ: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_1(e, CTZ, dst, lhs, is_64bit);
        } break;
        case TB_POPCNT: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_1(e, CNT, dst, lhs, is_64bit);
        } break;

        ////////////////////////////////
        // Unary operations
        ////////////////////////////////
        // case TB_FNEG:

        ////////////////////////////////
        // Integer arithmatic
        ////////////////////////////////
        case TB_AND: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_logical(e, AND, dst, lhs, rhs, 0, 0, is_64bit);
        } break;
        case TB_OR: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_logical(e, ORR, dst, lhs, rhs, 0, 0, is_64bit);
        } break;
        case TB_XOR: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_logical(e, EOR, dst, lhs, rhs, 0, 0, is_64bit);
        } break;
        case TB_ADD: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_add(e, ADD, dst, lhs, rhs, 0, 0, false, is_64bit);
        } break;
        case TB_SUB: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_add(e, SUB, dst, lhs, rhs, 0, 0, false, is_64bit);
        } break;
        case TB_MUL: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_3(e, MADD, dst, lhs, rhs, ZR, is_64bit);
        } break;

        case TB_SHL: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, LSLV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_SHR: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, LSRV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_SAR: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, ASRV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_ROL: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            tb_todo(); //!! arm doesn't have rotate left
            emit_dpr_2(e, RORV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_ROR: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, RORV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_UDIV: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, UDIV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_SDIV: {
            int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
            int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
            int rhs = op_reg_at(ctx, n->inputs[2], REG_CLASS_GPR);
            bool is_64bit = n->dt.type == TB_TAG_I64;
            emit_dpr_2(e, SDIV, dst, lhs, rhs, is_64bit);
        } break;
        case TB_UMOD: {
            tb_unreachable();
        } break;
        case TB_SMOD: {
            tb_unreachable();
        } break;

        ////////////////////////////////
        // Float arithmatic
        ////////////////////////////////
        // case TB_FADD:
        // case TB_FSUB:
        // case TB_FMUL:
        // case TB_FDIV:
        // case TB_FMIN:
        // case TB_FMAX:

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
        // Special ops
        ////////////////////////////////
        // case TB_ADC:     // (Int, Int, Bool?) -> (Int, Bool)
        // case TB_UDIVMOD: // (Int, Int) -> (Int, Int)
        // case TB_SDIVMOD: // (Int, Int) -> (Int, Int)
        // case TB_MULPAIR:

        ////////////////////////////////
        // variadic
        ////////////////////////////////
        // case TB_VA_START:

        ////////////////////////////////
        // general machine nodes:
        ////////////////////////////////
        // case TB_MACH_MOVE:
        // case TB_MACH_COPY:
        // case TB_MACH_JUMP:
        // case TB_MACH_FRAME_PTR:
        // case TB_MACH_JIT_THREAD_PTR:
        // case TB_MACH_SYMBOL:

        ////////////////////////////////
        // custom machine nodes
        ////////////////////////////////
        // case A64_ADDIMM: {
        //     int dst = op_reg_at(ctx, n,            REG_CLASS_GPR);
        //     int lhs = op_reg_at(ctx, n->inputs[1], REG_CLASS_GPR);
        //     uint64_t imm = TB_NODE_GET_EXTRA_T(n, A64Imm12)->imm;
        //     int shift = 0;
        //     if (imm == (imm & 077770000)) {
        //         shift = 12;
        //         imm >>= shift;
        //     }

        //     bool is_64bit = n->dt.type == TB_TAG_I64;
        //     emit_dp_imm(e, ADD, dst, lhs, imm, shift, is_64bit);
        //     break;
        // }

        default: {
            tb_todo();
        } break;
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

static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    static const char* dp_strs[] = { "add", "adds", "sub", "subs" };
    static const char* sh_strs[] = { "lsl", "lsr", "asr", "ror" };

    while (pos < end) {
        uint32_t inst = *((uint32_t*) &e->data[pos]);
        pos += 4;

        bool is_64bit = inst >> 31u;
        uint32_t family = (inst >> 25u) & 0xF;
        switch (family) {
            // Data processing (immediate)
            case 0b1000:
            case 0b1001: {
                uint32_t op1 = (inst >> 23u) & 0b111;
                uint32_t opc = (inst >> 29u) & 0b11;

                if (op1 == 2) {
                    uint32_t imm  = (inst >> 10u) & 0xFFF;
                    uint32_t rn   = (inst >> 5u) & 0x1F;
                    uint32_t rd   = (inst >> 0u) & 0x1F;

                    E("  %s ", dp_strs[opc]);
                    print_gpr(e, rd, is_64bit);
                    E(", ");
                    print_gpr(e, rn, is_64bit);
                    E(", #%#x", imm);
                } else if (op1 == 5) {
                    // Move wide
                    if (opc == 1) {
                        E("  ERROR ");
                    } else {
                        const char* str = "n_zk";
                        uint32_t hw  = (inst >> 21u) & 3;
                        uint32_t imm = (inst >> 5u) & 0xFFFF;
                        uint32_t rn  = inst & 0x1F;

                        E("  mov%c ", str[opc]);
                        print_gpr(e, rn, is_64bit);
                        E(", #%#x", imm);
                        if (hw) {
                            E(", lsl #%d", hw*16);
                        }
                    }
                } else if (op1 == 6) {
                    // Bitfield
                    static const char* strs[] = { "sbfm", "bfm", "ubfm", "error" };
                    uint32_t immr = (inst >> 16u) & 0b111111;
                    uint32_t imms = (inst >> 10u) & 0b111111;
                    uint32_t rn   = (inst >> 5u) & 0x1F;
                    uint32_t rd   = (inst >> 0u) & 0x1F;

                    if (opc == 2 && immr == imms + 1) {
                        // lsl
                        int amt = (is_64bit ? 63 : 31) - imms;
                        E("  lsl ");
                        print_gpr(e, rd, is_64bit);
                        E(", #%#x", amt);
                    } else {
                        E("  %s ", strs[opc]);
                        print_gpr(e, rd, is_64bit);
                        E(", ");
                        print_gpr(e, rn, is_64bit);
                        E(", #%#x, #%#x", immr, imms);
                    }
                } else {
                    E("  DATA IMM ");
                }
                break;
            }

            // Data processing (register)
            case 0b0101:
            case 0b1101: {
                uint32_t sf  = (inst >> 31u) & 1;
                uint32_t op0 = (inst >> 30u) & 1;
                uint32_t op1 = (inst >> 28u) & 1;
                uint32_t op2 = (inst >> 21u) & 0xF;
                uint32_t rm  = (inst >> 16u) & 0x1F;
                uint32_t rn  = (inst >> 5u) & 0x1F;
                uint32_t rd  = (inst >> 0u) & 0x1F;

                const char* mnemonic = NULL;
                if (op1) { // op1=1
                    if (op2 == 0b0110) {

                    }

                    // Data processing (3 source)
                    uint32_t ra = (inst >> 10u) & 0x1F;

                    if ((inst >> 29u) & 3) {
                        E("ERROR");
                    } else {
                        uint32_t opc = (((inst >> 21u) & 7) << 1) | ((inst >> 15u) & 1);
                        switch (opc) {
                            case 0b0000: E("  madd "); break;
                            case 0b0001: E("  msub "); break;
                            default: tb_todo();
                        }
                    }
                    print_gpr(e, rd, is_64bit);
                    E(", ");
                    print_gpr(e, rn, is_64bit);
                    E(", ");
                    print_gpr(e, rm, is_64bit);
                    E(", ");
                    print_gpr(e, ra, is_64bit);

                    /*if (op0 == 0 && op2 == 0b0110) {
                        // uint32_t S = (inst >> 29u) & 1;
                        uint32_t opc  = (inst >> 10u) & 0x3F;

                        if (opc == 2) {
                            E("  udiv ");
                        } else if (opc == 3) {
                            E("  sdiv ");
                        } else {
                            E("ERROR ");
                        }
                        print_gpr(e, rd, is_64bit);
                        E(", ");
                        print_gpr(e, rn, is_64bit);
                        E(", ");
                        print_gpr(e, rm, is_64bit);
                    } else if (op2 >> 3) {
                    } else {
                        E("  DATA REG ");
                    }*/
                } else { // op1=0
                    uint32_t imm  = (inst >> 10u) & 0x3F;
                    uint32_t sh   = (inst >> 22u) & 3;

                    if (op2 < 0b1000) { // op2=0xxx
                        static const char* ops[] = {
                            // Logical (shifted register)
                            // n=0   n=1      opc
                            "and",  "bic", // 00
                            "orr",  "orn", // 01
                            "eor",  "eon", // 10
                            "ands", "bics" // 11
                        };

                        uint32_t opc = (((inst >> 29u) & 3) << 1) | ((inst >> 21) & 1);
                        mnemonic = ops[opc];
                    } else if (op2 & 1) { // op2=1xx1
                        // Add/subtract (extended register)
                        tb_todo();
                    } else { // op2=1xx0
                        // Add/subtract (shifted register)
                        static const char* ops[] = {
                            "add", "adds",
                            "sub", "subs"
                        };
                        uint32_t opc = (inst >> 29u) & 3;
                        mnemonic = ops[opc];
                    }

                    E("  %s ", mnemonic);
                    print_gpr(e, rd, is_64bit);
                    E(", ");
                    print_gpr(e, rn, is_64bit);
                    E(", ");
                    print_gpr(e, rm, is_64bit);
                    if (imm) {
                        E(", %s #%#x", sh_strs[sh], imm);
                    }
                }
                break;
            }

            // Branches, Exceptions, Syscalls
            case 0b1010:
            case 0b1011: {
                uint32_t op0 = (inst >> 29u) & 0b111;
                uint32_t op1 = (inst >> 12u) & 0b11111111111111;

                if (op0 == 0b110 && op1 >> 13u) {
                    // Unconditional
                    //
                    //    opc   op2    op3   op4
                    //   0010 11111 000000 00000  RET
                    uint32_t opc = (inst >> 21u) & 0xF;
                    uint32_t op2 = (inst >> 16u) & 0x1F;
                    uint32_t op3 = (inst >> 10u) & 0x1F;
                    uint32_t rn  = (inst >>  5u) & 0x1F;
                    uint32_t op4 = (inst >>  0u) & 0x1F;
                    if (opc == 2 && op2 == 0x1F && op3 == 0 && op4 == 0) {
                        if (rn == LR) { E("  ret"); }
                        else  { E("  ret v%d", rn); }
                    } else {
                        tb_todo();
                    }
                } else {
                    E("  BRANCH(op1=%#x) ", op1);
                }
                break;
            }

            default: tb_todo();
        }
        E("\n");
    }
}
#undef E

ICodeGen tb__aarch64_codegen = {
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
// #endif

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

#if 0
#ifdef TB_HAS_AARCH64
// NOTE(NeGate): THIS IS VERY INCOMPLETE
#include "../emitter.h"
#include "aarch64_emitter.h"

enum {
    // register classes
    REG_CLASS_GPR = 1,
    REG_CLASS_COUNT,
};

enum {
    ALL_GPRS = 0xFFFFFFFF & ~((1 << SP) | (1 << LR)),
};

#include "../codegen_impl.h"

enum {
    //   OP reg, imm
    TILE_HAS_IMM = 1
};

// true for 64bit
static bool legalize_int(TB_DataType dt) { return dt.type == TB_TAG_PTR || (dt.type == TB_TAG_INT && dt.data > 32); }

static bool try_for_imm12(int bits, TB_Node* n, int32_t* out_x) {
    if (n->type != TB_ICONST) {
        return false;
    }

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (bits > 12) {
        bool sign = (i->value >> 11ull) & 1;
        uint64_t top = i->value >> 12ull;

        // if the sign matches the rest of the top bits, we can sign extend just fine
        if (top != (sign ? 0xFFFFFFFFFFFFF : 0)) {
            return false;
        }
    }

    *out_x = i->value;
    return true;
}

static bool _2addr(TB_Node* n) { return false; }
static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->sched = greedy_scheduler;
    ctx->_2addr = _2addr;
    ctx->regalloc = tb__lsra;

    uint32_t all_gprs = UINT32_MAX & ~(1u << SP);
    if (ctx->features.gen & TB_FEATURE_FRAME_PTR) {
        all_gprs &= ~(1u << FP);
    }
    ctx->num_regs[1] = 32;
    ctx->normie_mask[1] = REGMASK(GPR, all_gprs);

    // x19 - x29 are callee saved
    ctx->callee_saved[0] = ((1u << 10u) - 1) << 19u;
}

static RegMask isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n) {
    switch (n->type) {
        case TB_ROOT: {
            TileInput* ins = tile_set_ins(ctx, dst, n, 4, n->input_count);
            int rets = n->input_count - 4;

            assert(rets <= 2 && "At most 2 return values :(");
            FOR_N(i, 0, rets) {
                ins[i].mask = REGMASK(GPR, 1 << i);
            }
            return REGEMPTY;
        }

        case TB_PROJ:
        int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
        if (n->inputs[0]->type == TB_ROOT) {
            return REGMASK(GPR, i >= 3 ? (1u << (i - 3)) : 0);
        } else if (n->inputs[0]->type == TB_CALL) {
            if (i == 2)      return REGMASK(GPR, 1 << X0);
            else if (i == 3) return REGMASK(GPR, 1 << X1);
            else return REGEMPTY;
        } else if (n->inputs[0]->type == TB_BRANCH) {
            return REGEMPTY;
        } else {
            tb_todo();
        }

        case TB_ARRAY_ACCESS:
        tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[1]);
        return ctx->normie_mask[1];

        case TB_ICONST:
        return ctx->normie_mask[1];

        case TB_MUL:
        case TB_UDIV:
        case TB_SDIV:
        case TB_XOR:
        tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[1]);
        return ctx->normie_mask[1];

        // binary ops
        case TB_SHL:
        case TB_SHR:
        case TB_ADD:
        case TB_SUB: {
            int32_t x;
            if (try_for_imm12(n->dt.data, n->inputs[2], &x)) {
                tile_broadcast_ins(ctx, dst, n, 1, 2, ctx->normie_mask[1]);
                dst->flags |= TILE_HAS_IMM;
            } else {
                tile_broadcast_ins(ctx, dst, n, 1, n->input_count, ctx->normie_mask[1]);
            }
            return ctx->normie_mask[1];
        }

        default:
        tb_todo();
    }
}

static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n) {
    // TODO(NeGate): optionally preserve the frame pointer
    // TODO(NeGate): stack allocation
    ctx->prologue_length = ctx->emit.count;
}

static GPR gpr_at(LiveInterval* l) { assert(l->class == REG_CLASS_GPR); return l->assigned; }
static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_SPILL_MOVE) {
        GPR dst = gpr_at(t->interval);
        GPR src = gpr_at(t->ins[0].src);
        if (dst != src) {
            emit_mov(e, dst, src, true);
        }
    } else {
        TB_Node* n = t->n;
        switch (n->type) {
            // projections don't manage their own work, that's the
            // TUPLE node's job.
            case TB_PROJ: break;

            case TB_ROOT: {
                emit_ret(e, LR);
                break;
            }

            case TB_ICONST: {
                bool is_64bit = false;
                GPR dst = gpr_at(t->interval);
                uint64_t imm = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

                int shift = 0; bool first = true;
                while (imm) {
                    if (imm & 0xFFFF) {
                        emit_movimm(e, dst, imm & 0xFFFF, shift / 16, is_64bit, first);
                        first = false;
                    }
                    imm >>= 16, shift += 16;
                }
                break;
            }

            case TB_SDIV: {
                bool is_64bit = legalize_int(n->dt);
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);
                GPR rhs = gpr_at(t->ins[1].src);
                emit_dp_r(e, SDIV, dst, lhs, rhs, 0, 0, is_64bit);
                break;
            }

            case TB_SHL:
            case TB_SHR:
            case TB_SAR: {
                bool is_64bit = legalize_int(n->dt);
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);
                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);

                    int amt = 31 - i->value;
                    emit_bitfield(e, UBFM, dst, lhs, amt + 1, amt, is_64bit);
                } else {
                    // GPR rhs = gpr_at(t->ins[1].src);
                    tb_todo();
                }
                break;
            }

            case TB_XOR:
            case TB_ADD:
            case TB_SUB: {
                const static int ops[] = { -1, -1, EOR, ADD, SUB };
                int op = ops[n->type - TB_AND];

                bool is_64bit = legalize_int(n->dt);
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);

                if (t->flags & TILE_HAS_IMM) {
                    assert(n->inputs[2]->type == TB_ICONST);
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);
                    emit_dp_imm(e, op, dst, lhs, i->value, 0, is_64bit);
                } else {
                    GPR rhs = gpr_at(t->ins[1].src);
                    emit_dp_r(e, op, dst, lhs, rhs, 0, 0, is_64bit);
                }
                break;
            }

            case TB_MUL: {
                GPR dst = gpr_at(t->interval);
                GPR lhs = gpr_at(t->ins[0].src);
                GPR rhs = gpr_at(t->ins[1].src);

                emit_dp3(e, MADD, dst, lhs, rhs, ZR, legalize_int(n->dt));
                break;
            }

            default: tb_todo();
        }
    }
}

static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e) {
}

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    return 0;
}

ICodeGen tb__aarch64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
#else
ICodeGen tb__aarch64_codegen;
#endif
#endif