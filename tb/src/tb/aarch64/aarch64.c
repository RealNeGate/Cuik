#if 0
#include "../tb_internal.h"
#include "../codegen/emitter.h"

typedef struct Ctx Ctx;

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
    bool is_spill;
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

#define GAD_REG_PRIORITIES { \
    { X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15 }, \
    { } \
}

#define GAD_EXTRA_CTX {}
#define GAD_FN(name) aarch64_ ## name
#define GAD_NUM_REG_FAMILIES 2
#define GAD_REGS_IN_FAMILY 32
#define GAD_MAKE_STACK_SLOT(ctx, f, r_, pos) (Val){ VAL_MEM, .r = (r_), .mem = { SP, (pos) } }
#define GAD_VAL Val
#include "../codegen/generic_addrdesc.h"
#include "aarch64_emitter.h"

#ifdef TB_COMPILE_TESTS
bool tb_aarch64_test_suite(void) {
    return true;
}
#endif /* TB_COMPILE_TESTS */

static void aarch64_goto(Ctx* restrict ctx, TB_Label label) {
    // jmp(&ctx->emit, label);
}

static void aarch64_ret_jmp(Ctx* restrict ctx) {
    // ret_jmp(&ctx->emit);
}

static size_t aarch64_emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static size_t aarch64_emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    // RET
    memcpy(out, &(uint32_t){ 0xD65F03C0 }, sizeof(uint32_t));
    return 4;
}

static void aarch64_initial_reg_alloc(Ctx* restrict ctx) {
    ctx->regs_available[0] = 30; // take out the stack pointer
    ctx->regs_available[1] = 32;
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
    // .emit_call_patches   = aarch64_emit_call_patches,
    .emit_prologue       = aarch64_emit_prologue,
    .emit_epilogue       = aarch64_emit_epilogue,

    .fast_path    = aarch64_compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
#endif