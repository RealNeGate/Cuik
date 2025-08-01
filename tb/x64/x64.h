#pragma once
#include "../emitter.h"
#include "../tb_internal.h"
#include <tb_x64.h>

static_assert(sizeof(float) == sizeof(uint32_t), "Float needs to be a 32-bit float!");
static_assert(sizeof(double) == sizeof(uint64_t), "Double needs to be a 64-bit float!");

typedef union {
    float f;
    uint32_t i;
} Cvt_F32U32;

typedef union {
    double f;
    uint64_t i;
} Cvt_F64U64;

typedef enum Cond {
    O, NO, B, NB, E, NE, BE, A,
    S, NS, P, NP, L, GE, LE, G
} Cond;

typedef enum GPR {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11, R12, R13, R14, R15,

    GPR_NONE = -1
} GPR;

typedef enum XMM {
    XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,

    XMM_NONE = -1
} XMM;

typedef enum {
    VAL_NONE, VAL_FLAGS, VAL_GPR, VAL_XMM, VAL_IMM, VAL_MEM, VAL_GLOBAL, VAL_LABEL
} ValType;

typedef enum {
    SCALE_X1, SCALE_X2, SCALE_X4, SCALE_X8
} Scale;

enum {
    MOD_INDIRECT        = 0, // [rax]
    MOD_INDIRECT_DISP8  = 1, // [rax + disp8]
    MOD_INDIRECT_DISP32 = 2, // [rax + disp32]
    MOD_DIRECT          = 3, // rax
};

typedef enum Inst2FPFlags {
    INST2FP_DOUBLE = (1u << 0),
    INST2FP_PACKED = (1u << 1)
} Inst2FPFlags;

typedef struct Val {
    int8_t type;

    // if VAL_MEM then this is the base
    int8_t reg;

    // used by VAL_MEM and VAL_GLOBAL
    int8_t index, scale;

    // for VAL_IMM, memory displacement or signed immediate
    int32_t imm;

    union {
        // for VAL_GLOBAL this is used as the base
        TB_Symbol* symbol;
        // for VAL_LABEL
        int label;
    };
} Val;

typedef enum InstType {
    #define X(a, ...) a,
    #include "x64_insts.inc"
} InstType;

// EXT variations use a 0F before the opcode
typedef enum {
    // Nullary
    INST_BYTE,
    INST_BYTE_EXT,
    // Unary
    INST_UNARY,
    INST_UNARY_EXT, // 0F
    // Binop
    INST_BINOP,
    INST_BINOP_PLUS, // +r
    INST_BINOP_EXT,  // 0F
    INST_BINOP_EXT2, // 0F (movzx, movsx)
    INST_BINOP_EXT3, // 66 (movd, movq)
    INST_BINOP_CL, // implicit CL, used by the shift ops

    // SSE
    INST_BINOP_SSE,
} InstCategory;

typedef struct InstDesc {
    const char* mnemonic;
    uint8_t cat;

    uint8_t op;

    // IMMEDIATES (or unary instructions)
    uint8_t op_i;
    uint8_t rx_i;
} InstDesc;

static const GPR WIN64_GPR_PARAMETERS[4] = { RCX, RDX, R8, R9 };
static const GPR SYSV_GPR_PARAMETERS[6] = { RDI, RSI, RDX, RCX, R8, R9 };

static const InstDesc inst_table[] = {
    #define X(a, b, c, ...) [a] = { .mnemonic = b, .cat = INST_ ## c, __VA_ARGS__ },
    #include "x64_insts.inc"
};

#define SYSCALL_ABI_CALLER_SAVED ((1u << RDI) | (1u << RSI) | (1u << RDX) | (1u << R10) | (1u << R8) | (1u << R9) | (1u << RAX) | (1u << R11))
#define SYSCALL_ABI_CALLEE_SAVED ~SYSCALL_ABI_CALLER_SAVED

static Val val_gpr(GPR g) {
    return (Val) { .type = VAL_GPR, .reg = g };
}

static Val val_xmm(XMM x) {
    return (Val) { .type = VAL_XMM, .reg = x };
}

static Val val_flags(Cond c) {
    return (Val) { .type = VAL_FLAGS, .reg = c };
}

static Val val_global(TB_Symbol* s, int disp) {
    return (Val) { .type = VAL_GLOBAL, .symbol = s, .imm = disp };
}

static Val val_stack(int s) {
    return (Val) { .type = VAL_MEM, .reg = RSP, .index = GPR_NONE, .scale = SCALE_X1, .imm = s };
}

static Val val_base_disp(GPR b, int d) {
    return (Val) {
        .type = VAL_MEM, .reg = b, .index = GPR_NONE, .scale = SCALE_X1, .imm = d
    };
}

static Val val_base_index_disp(GPR b, GPR i, Scale s, int d) {
    return (Val) {
        .type = VAL_MEM, .reg = b, .index = i, .scale = s, .imm = d
    };
}

static bool is_value_mem(const Val* v) {
    return v->type == VAL_MEM || v->type == VAL_GLOBAL;
}

static bool is_value_gpr(const Val* v, GPR g) {
    if (v->type != VAL_GPR) return false;

    return (v->reg == g);
}

static bool is_value_xmm(const Val* v, XMM x) {
    if (v->type != VAL_XMM) return false;

    return (v->reg == x);
}

static bool is_value_match(const Val* a, const Val* b) {
    if (a->type != b->type) return false;

    if (a->type == VAL_MEM) {
        return a->reg == b->reg && a->index == b->index && a->scale == b->scale && a->index == b->index;
    }

    return (a->type == VAL_GPR || a->type == VAL_XMM) ? a->reg == b->reg : false;
}

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static const char* XMM_NAMES[] = { "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7", "XMM8",  "XMM9", "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15" };
static const char* COND_NAMES[] = {
    "O", "NO", "B", "NB", "E", "NE", "BE", "A",
    "S", "NS", "P", "NP", "L", "GE", "LE", "G"
};
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

// operand shorthands
#define Vimm(x)    &(Val){ VAL_IMM, .imm = (x) }
#define Vgpr(x)    &(Val){ VAL_GPR, .reg = (x) }
#define Vvec(x)    &(Val){ VAL_XMM, .reg = (x) }
#define Vabs(x)    &(Val){ VAL_ABS, .abs = (x) }
#define Vlbl(x)    &(Val){ VAL_LABEL,  .label = (x) }
#define Vbase(b,d) &(Val){ VAL_MEM, .reg = (b), .index = GPR_NONE, .imm = (d) }
#define Vmem(b,i,s,d) &(Val){ VAL_MEM, .reg = (b), .index = (i), .scale = (s), .imm = (d) }
#define Vsym(b,d)  &(Val){ VAL_GLOBAL, .imm = (d), .symbol = (b) }

// shorthand macros
#define INST0(op, dt)             inst0(&ctx->emit, op, dt)
#define INST1(op, a, dt)          inst1(&ctx->emit, op, a, dt)
#define INST2(op, a, b, dt)       inst2(&ctx->emit, op, a, b, dt)
#define INST2SSE(op, a, b, dt)    inst2sse(&ctx->emit, op, a, b, dt)

#define GET_MACRO(_0, _1, _2, NAME, ...) NAME
#define __(mnemonic, dt, ...) GET_MACRO(_0, ##__VA_ARGS__, asm_inst2, asm_inst1, asm_inst0)(e, mnemonic, dt, ## __VA_ARGS__)

typedef enum {
    UNWIND_OP_PUSH_NONVOL = 0, /* info == register number */
    UNWIND_OP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
    UNWIND_OP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
    UNWIND_OP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
    UNWIND_OP_SAVE_NONVOL,     /* info == register number, offset in next slot */
    UNWIND_OP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
    UNWIND_OP_SAVE_XMM128 = 8, /* info == XMM reg number, offset in next slot */
    UNWIND_OP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
    UNWIND_OP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UnwindCodeOps;

typedef union {
    struct {
        uint8_t code_offset;
        uint8_t unwind_op : 4;
        uint8_t op_info   : 4;
    };
    uint16_t frame_offset;
} UnwindCode;

enum {
    UNWIND_FLAG_EHANDLER  = 0x01,
    UNWIND_FLAG_UHANDLER  = 0x02,
    UNWIND_FLAG_CHAININFO = 0x04,
};

typedef struct {
    uint8_t version : 3;
    uint8_t flags   : 5;
    uint8_t prolog_length;
    uint8_t code_count;
    uint8_t frame_register : 4;
    uint8_t frame_offset   : 4;
    UnwindCode code[]; // ((code_count + 1) & ~1) - 1
} UnwindInfo;

