#pragma once
#include "../codegen/emitter.h"
#include "../tb_internal.h"

#define TB_TEMP_REG INT_MAX

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

typedef enum {
    O, NO, B, NB, E, NE, BE, A,
    S, NS, P, NP, L, GE, LE, G
} Cond;

typedef enum {
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8,  R9,  R10, R11, R12, R13, R14, R15,

    GPR_NONE = -1
} GPR;

typedef enum {
    XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,

    XMM_NONE = -1
} XMM;

typedef enum {
    VAL_NONE, VAL_FLAGS, VAL_GPR, VAL_XMM, VAL_IMM, VAL_MEM, VAL_GLOBAL
} ValType;

typedef enum {
    SCALE_X1, SCALE_X2, SCALE_X4, SCALE_X8
} Scale;

typedef enum X86_DataType {
    X86_TYPE_NONE = 0,

    X86_TYPE_BYTE,    // 1
    X86_TYPE_WORD,    // 2
    X86_TYPE_DWORD,   // 4
    X86_TYPE_QWORD,   // 8

    X86_TYPE_PBYTE,   // int8 x 16 = 16
    X86_TYPE_PWORD,   // int16 x 8 = 16
    X86_TYPE_PDWORD,  // int32 x 4 = 16
    X86_TYPE_PQWORD,  // int64 x 2 = 16

    X86_TYPE_SSE_SS,  // float32 x 1 = 4
    X86_TYPE_SSE_SD,  // float64 x 1 = 8
    X86_TYPE_SSE_PS,  // float32 x 4 = 16
    X86_TYPE_SSE_PD,  // float64 x 2 = 16

    X86_TYPE_XMMWORD, // the generic idea of them
} X86_DataType;

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
    uint8_t type;
    TB_DataType dt;

    union {
        int reg;
        GPR gpr;
        XMM xmm;

        Cond cond;
        struct {
            bool is_rvalue;
            GPR base : 8;
            GPR index : 8;
            Scale scale : 8;
            int32_t disp;
        } mem;
        struct {
            // this should alias with mem.is_rvalue
            bool is_rvalue;
            const TB_Global* g;
            int16_t disp;
        } global;
        int32_t imm;
    };
} Val;
static_assert(offsetof(Val, reg) == offsetof(Val, gpr), "Val::reg and Val::gpr must alias!");
static_assert(offsetof(Val, reg) == offsetof(Val, xmm), "Val::reg and Val::xmm must alias!");
static_assert(offsetof(Val, global.is_rvalue) == offsetof(Val, mem.is_rvalue), "Val::mem.is_rvalue and Val::global.is_rvalue must alias!");

typedef enum Inst2Type {
    // Nullary
    RET,
    // Unary
    NOT, NEG, DIV, IDIV, CALL_RM,
    // Integer data processing
    ADD, OR, AND, SUB, XOR, CMP, MOV,
    // Cooler integer ops
    SHL, SHR, SAR,
    // Misc interger ops
    TEST, LEA, IMUL, XCHG, XADD,
    // casts
    MOVSXB, MOVSXW, MOVSXD, MOVZXB, MOVZXW,

    // SSE operations
    FP_MOV, FP_ADD, FP_SUB, FP_MUL, FP_DIV, FP_CMP, FP_UCOMI,
    FP_SQRT, FP_RSQRT, FP_AND, FP_OR, FP_XOR,
    FP_CVT, // cvtss2sd or cvtsd2ss
} InstType;

typedef enum ExtMode {
    // Normal
    EXT_NONE,

    // DEF instructions have a 0F prefix
    EXT_DEF,

    // same as DEF but for MOVZX and MOVSX
    // these are forced as always load.
    EXT_DEF2
} ExtMode;

typedef struct InstDesc {
    uint8_t op;

    // IMMEDIATES (or unary instructions)
    uint8_t op_i;
    uint8_t rx_i;

    ExtMode ext : 8;

    const char* mnemonic;
} InstDesc;

static const GPR WIN64_GPR_PARAMETERS[4] = { RCX, RDX, R8, R9 };
static const GPR SYSV_GPR_PARAMETERS[6] = { RDI, RSI, RDX, RCX, R8, R9 };

#define NULLARY_OP(name, op) [name] = { (op), .mnemonic = #name }
#define UNARY_OP(name, op, rx) [name] = { .op_i = (op), .rx_i = (rx), .mnemonic = #name }
#define UNARY_OP2(name, op, op_i, rx_i) [name] = { (op), (op_i), (rx_i), .mnemonic = #name }
#define BINARY_OP(name, op, op_i, rx_i) [name] = { (op), (op_i), (rx_i), .mnemonic = #name }
#define BINARY_OP2(name, op) [name] = { (op), .mnemonic = #name }
#define BINARY_OP_DEF(name, op) [name] = { (op), .ext = EXT_DEF, .mnemonic = #name }
#define BINARY_OP_DEF2(name, op) [name] = { (op), .ext = EXT_DEF2, .mnemonic = #name }
static const InstDesc inst_table[] = {
    // nullary
    NULLARY_OP(RET,        0xC3),
    // unary ops
    UNARY_OP(NOT,          0xF7, 0x02),
    UNARY_OP(NEG,          0xF7, 0x03),
    UNARY_OP(DIV,          0xF7, 0x06),
    UNARY_OP(IDIV,         0xF7, 0x07),
    UNARY_OP(CALL_RM,      0xFF, 0x02),
    // unary ops (the normal op is for CL use, imm is imm8)
    UNARY_OP2(SHL,         0xD2, 0xC0, 0x04),
    UNARY_OP2(SHR,         0xD2, 0xC0, 0x05),
    UNARY_OP2(SAR,         0xD2, 0xC0, 0x07),
    // regular integer binops
    BINARY_OP(ADD,         0x00, 0x80, 0x00),
    BINARY_OP(OR,          0x08, 0x80, 0x01),
    BINARY_OP(AND,         0x20, 0x80, 0x04),
    BINARY_OP(SUB,         0x28, 0x80, 0x05),
    BINARY_OP(XOR,         0x30, 0x80, 0x06),
    BINARY_OP(CMP,         0x38, 0x80, 0x07),
    BINARY_OP(MOV,         0x88, 0xC6, 0x00),
    BINARY_OP(TEST,        0x84, 0xF6, 0x00),
    // misc integer ops
    BINARY_OP2(XCHG,       0x86),
    BINARY_OP2(LEA,        0x8D),
    BINARY_OP_DEF(XADD,    0xC0),
    BINARY_OP_DEF(IMUL,    0xAF),
    BINARY_OP_DEF2(MOVSXB, 0xBE),
    BINARY_OP_DEF2(MOVSXW, 0xBF),
    BINARY_OP2(MOVSXD,     0x63),
    BINARY_OP_DEF2(MOVZXB, 0xB6),
    BINARY_OP_DEF2(MOVZXW, 0xB7),
    // SSE binops
    BINARY_OP2(FP_MOV,     0x10),
    BINARY_OP2(FP_ADD,     0x58),
    BINARY_OP2(FP_MUL,     0x59),
    BINARY_OP2(FP_SUB,     0x5C),
    BINARY_OP2(FP_DIV,     0x5E),
    BINARY_OP2(FP_CMP,     0xC2),
    BINARY_OP2(FP_UCOMI,   0x2E),
    BINARY_OP2(FP_CVT,     0x5A),
    BINARY_OP2(FP_SQRT,    0x51),
    BINARY_OP2(FP_RSQRT,   0x52),
    BINARY_OP2(FP_AND,     0x54),
    BINARY_OP2(FP_OR,      0x56),
    BINARY_OP2(FP_XOR,     0x57),
};
#undef NULLARY_OP
#undef UNARY_OP
#undef BINARY_OP
#undef BINARY_OP2
#undef BINARY_OP_DEF
#undef BINARY_OP_DEF2

// NOTE(NeGate): This is for Win64, we can handle SysV later
#define WIN64_ABI_CALLER_SAVED ((1u << RAX) | (1u << RCX) | (1u << RDX) | (1u << R8) | (1u << R9) | (1u << R10) | (1u << R11))
#define WIN64_ABI_CALLEE_SAVED ~WIN64_ABI_CALLER_SAVED

#define SYSV_ABI_CALLER_SAVED ((1u << RAX) | (1u << RDI) | (1u << RSI) | (1u << RCX) | (1u << RDX) | (1u << R8) | (1u << R9) | (1u << R10) | (1u << R11))
#define SYSV_ABI_CALLEE_SAVED ~SYSV_ABI_CALLER_SAVED

#define SYSCALL_ABI_CALLER_SAVED ((1u << RDI) | (1u << RSI) | (1u << RDX) | (1u << R10) | (1u << R8) | (1u << R9) | (1u << RAX) | (1u << R11))
#define SYSCALL_ABI_CALLEE_SAVED ~SYSCALL_ABI_CALLER_SAVED

// GPRs can only ever be scalar
inline static Val val_gpr(TB_DataType dt, GPR g) {
    return (Val) { .type = VAL_GPR, .dt = dt, .gpr = g };
}

inline static Val val_xmm(TB_DataType dt, XMM x) {
    return (Val) { .type = VAL_XMM, .dt = dt, .xmm = x };
}

inline static Val val_flags(Cond c) {
    return (Val) { .type = VAL_FLAGS, .dt = TB_TYPE_BOOL, .cond = c };
}

inline static Val val_global(const TB_Global* g) {
    return (Val) { .type = VAL_GLOBAL, .dt = TB_TYPE_PTR, .global.is_rvalue = false, .global.g = g };
}

inline static Val val_imm(TB_DataType dt, int32_t imm) {
    return (Val) { .type = VAL_IMM, .dt = dt, .imm = imm };
}

inline static Val val_stack(TB_DataType dt, int s) {
    return (Val) {
        .type = VAL_MEM,
        .dt = dt,
        .mem = { .base = RBP, .index = GPR_NONE, .scale = SCALE_X1, .disp = s }
    };
}

inline static Val val_base_disp(TB_DataType dt, GPR b, int d) {
    return (Val) {
        .type = VAL_MEM,
        .dt = dt,
        .mem = { .base = b, .index = GPR_NONE, .scale = SCALE_X1, .disp = d }
    };
}

inline static Val val_base_index(TB_DataType dt, GPR b, GPR i, Scale s) {
    return (Val) {
        .type = VAL_MEM,
        .dt = dt,
        .mem = { .base = b, .index = i, .scale = s }
    };
}

inline static Val val_base_index_disp(TB_DataType dt, GPR b, GPR i, Scale s, int d) {
    return (Val) {
        .type = VAL_MEM,
        .dt = dt,
        .mem = { .base = b, .index = i, .scale = s, .disp = d }
    };
}

inline static bool is_lvalue(const Val* v) {
    return (v->type == VAL_MEM || v->type == VAL_GLOBAL) && !v->mem.is_rvalue;
}

inline static bool is_rvalue(const Val* v) {
    return (v->type == VAL_MEM || v->type == VAL_GLOBAL) && v->mem.is_rvalue;
}

inline static bool is_value_mem(const Val* v) {
    return v->type == VAL_MEM || v->type == VAL_GLOBAL;
}

inline static bool is_value_gpr(const Val* v, GPR g) {
    if (v->type != VAL_GPR) return false;

    return (v->gpr == g);
}

inline static bool is_value_xmm(const Val* v, XMM x) {
    if (v->type != VAL_XMM) return false;

    return (v->xmm == x);
}

inline static bool is_value_match(const Val* a, const Val* b) {
    if (a->type != b->type) return false;

    if (a->type == VAL_GPR) return a->gpr == b->gpr;
    else return false;
}

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static const char* COND_NAMES[] = {
    "O", "NO", "B", "NB", "E", "NE", "BE", "A",
    "S", "NS", "P", "NP", "L", "GE", "LE", "G"
};
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

// shorthand macros
#define STACK_ALLOC(size, align) (ctx->stack_usage = align_up(ctx->stack_usage + (size), align), - ctx->stack_usage)
#define INST1(op, a, dt)          inst1(&ctx->emit, op, a, dt)
#define INST2(op, a, b, dt)       inst2(&ctx->emit, op, a, b, dt)
#define INST2SSE(op, a, b, flags) inst2sse(&ctx->emit, op, a, b, flags)
#define JCC(cc, label)            jcc(&ctx->emit, cc, label)
#define JMP(label)                jmp(&ctx->emit, label)
#define RET_JMP()                 ret_jmp(&ctx->emit)
