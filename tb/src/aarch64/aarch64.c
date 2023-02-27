// NOTE(NeGate): THIS IS VERY INCOMPLETE
#include "../tb_internal.h"
#include "../codegen/emitter.h"

typedef struct Ctx Ctx;

enum {
    AARCH64_REG_CLASS_GPR
};

// Xn refers to the 64bit variants of the registers,
// usually the 32bit aliases are Wn (we don't have enums
// for them because it's not that important, they're equal)
typedef enum {
    X0,  X1,   X2,  X3,  X4,  X5,  X6,  X7,
    X8,  X9,  X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23,
    X24, X25, X26, X27, X28, X29, X30,

    // It's context specific because ARM lmao
    ZR = 0x1F, SP = 0x1F,

    // not a real gpr
    GPR_NONE = -1,
} GPR;

// refers to the data processing immediate operand.
// Aarch64 has a bunch of weird immediate fields so
// we might wanna rename this later.
typedef struct {
    uint16_t imm;
    uint8_t shift;
} Immediate;

typedef struct {
    enum {
        VAL_NONE,
        VAL_FLAGS,
        VAL_GPR,
        VAL_FPR,
        VAL_IMM,
        VAL_MEM,
        VAL_GLOBAL,
    } type : 8;
    bool is_spill : 1;
    bool is_ref : 1;
    TB_DataType dt;
    TB_Reg r;

    union {
        uint64_t num;

        int reg;
        GPR gpr;
        uint8_t fpr;
        struct {
            bool is_rvalue;
            uint8_t base;
            int32_t disp;
        } mem;
    };
} Val;

static void jmp(TB_CGEmitter* restrict e, int label);
static void ret_jmp(TB_CGEmitter* restrict e);

#define GAD_EXTRA_CTX {}
#define GAD_FN(name) aarch64_ ## name
#define GAD_NUM_REG_FAMILIES 2
#define GAD_MAKE_STACK_SLOT(ctx, f, r_, pos) (Val){ VAL_MEM, .r = (r_), .mem = { SP, (pos) } }
#define GAD_VAL Val
#include "../codegen/generic_addrdesc.h"
#include "aarch64_emitter.h"

static GAD_VAL aarch64_phi_alloc(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    return GAD_FN(regalloc)(ctx, f, r, AARCH64_REG_CLASS_GPR);
}

static GAD_VAL aarch64_cond_to_reg(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int cc) {
    tb_todo();
    return (GAD_VAL){ 0 };
}

static void aarch64_spill(Ctx* restrict ctx, TB_Function* f, GAD_VAL* dst_val, GAD_VAL* src_val, TB_Reg r) {
    tb_todo();
}

static void aarch64_move(Ctx* restrict ctx, TB_Function* f, TB_Reg dst, TB_Reg src) {
    tb_todo();
}

static void aarch64_store(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    tb_todo();
}

static void aarch64_call(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    tb_todo();
}

static void aarch64_misc_op(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    tb_todo();
}

static void aarch64_mem_op(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    tb_todo();
}

static GAD_VAL aarch64_eval(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];
    TB_NodeTypeEnum type = n->type;

    switch (type) {
        case TB_INTEGER_CONST: {
            assert(n->integer.num_words == 1);
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, AARCH64_REG_CLASS_GPR);

            uint64_t x = n->integer.single_word;
            emit_movz(&ctx->emit, dst.reg, x & 0xFFFF, 0, true);

            uint8_t shift = 16;
            x >>= 16;

            while (x & 0xFFFF) {
                emit_movk(&ctx->emit, dst.reg, x & 0xFFFF, shift, true);
                x >>= 16, shift += 16;
            }
            return dst;
        }

        case TB_ADD: {
            GAD_VAL a = ctx->values[n->i_arith.a];
            GAD_VAL b = ctx->values[n->i_arith.b];
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, AARCH64_REG_CLASS_GPR);

            bool is_64bit = n->dt.type == TB_PTR || (n->dt.type == TB_INT && n->dt.data == 64);
            emit_dp_r(&ctx->emit, ADD, dst.reg, a.reg, b.reg, 0, 0, is_64bit);
            return dst;
        }

        default:
        tb_todo();
        return (GAD_VAL){ 0 };
    }
}

static size_t aarch64_emit_call_patches(TB_Module* restrict m) {
    return 0;
}

static void aarch64_barrier(Ctx* restrict ctx, TB_Function* f, TB_Label bb, TB_Reg except, int split) {
    tb_todo();
}

static void aarch64_return(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    GAD_VAL* src = &ctx->values[n->ret.value];

    if (src->type != VAL_GPR || src->gpr != X0) {
        emit_mov(&ctx->emit, X0, src->gpr, true);
    }
}

static void aarch64_goto(Ctx* restrict ctx, TB_Label label) {
    // jmp(&ctx->emit, label);
    tb_todo();
}

static void aarch64_ret_jmp(Ctx* restrict ctx) {
    ctx->emit.ret_patches[ctx->emit.ret_patch_count++] = GET_CODE_POS(&ctx->emit);
    EMIT4(&ctx->emit, (0b101 << 25));
}

static void GAD_FN(resolve_params)(Ctx* restrict ctx, TB_Function* f, GAD_VAL* values) {
    const TB_FunctionPrototype* restrict proto = f->prototype;

    size_t param_count = proto->param_count;
    assert(param_count == 0);
    (void) param_count;
}

static void GAD_FN(branch_if)(Ctx* restrict ctx, TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false, TB_Reg fallthrough) {
    tb_todo();
}

static void GAD_FN(branch_jumptable)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, uint64_t min, uint64_t max, size_t entry_count, TB_SwitchEntry* entries, TB_Label default_label) {
    tb_todo();
}

static void GAD_FN(resolve_stack_slot)(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    tb_todo();
}

static size_t GAD_FN(resolve_stack_usage)(Ctx* restrict ctx, TB_Function* f, size_t stack_usage, size_t caller_usage) {
    // tb_todo();
    return 0;
}

static void GAD_FN(resolve_local_patches)(Ctx* restrict ctx, TB_Function* f) {
    // tb_todo();
}

static size_t aarch64_emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static size_t aarch64_emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    // RET
    memcpy(out, &(uint32_t){ 0xD65F03C0 }, 4);
    return 4;
}

static void aarch64_initial_reg_alloc(Ctx* restrict ctx) {
    ctx->callee_saved[0] = 0;

    // SP/ZR is the only reserved register
    ctx->free_regs[0] = set_create(32);
    set_put(&ctx->free_regs[0], SP);
    ctx->active[0] = 0, ctx->active_count += 1;
}

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            // round up bits to a byte
            bool is_big_int = dt.data > 64;
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

            return ((bits+7) / 8) << dt.width;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            return s << dt.width;
        }
        case TB_PTR: {
            return 8;
        }
        default: {
            tb_unreachable();
            return 0;
        }
    }
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__aarch64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .get_data_type_size  = aarch64_get_data_type_size,
    .emit_call_patches   = aarch64_emit_call_patches,
    .emit_prologue       = aarch64_emit_prologue,
    .emit_epilogue       = aarch64_emit_epilogue,

    .fast_path    = aarch64_compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
