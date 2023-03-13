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
    VAL_NONE, VAL_FLAGS, VAL_GPR, VAL_XMM, VAL_IMM, VAL_MEM, VAL_GLOBAL, VAL_ABS, VAL_LABEL
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
    int8_t type;

    // if VAL_MEM then this is the base
    int8_t reg;

    // used by VAL_MEM and VAL_GLOBAL
    int8_t index, scale;

    // memory displacement, label or signed immediate
    int32_t imm;

    union {
        // for VAL_ABS this is used
        uint64_t abs;
        // for VAL_GLOBAL this is used as the base
        const TB_Symbol* symbol;
    };
} Val;

typedef enum Inst2Type {
    // Nullary
    RET, INT3,
    // Control flow
    JO, JNO, JB, JNB, JE, JNE, JBE, JA,
    JS, JNS, JP, JNP, JL, JGE, JLE, JG,
    // Unary
    NOT, NEG, DIV, IDIV, JMP, CALL,
    // Integer data processing
    ADD, OR, AND, SUB, XOR, CMP, MOV,
    // Cooler integer ops
    SHL, SHR, SAR,
    // Misc interger ops
    MOVABS, TEST, LEA, IMUL, XCHG, XADD,
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
    EXT_DEF2,

    // implicit CL, used by the shift ops
    EXT_CL,
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
#define BINARY_OP_CL(name, op, op_i, rx_i) [name] = { (op), (op_i), (rx_i), .ext = EXT_CL, .mnemonic = #name }
#define BINARY_OP_DEF(name, op) [name] = { (op), .ext = EXT_DEF, .mnemonic = #name }
#define BINARY_OP_DEF2(name, op) [name] = { (op), .ext = EXT_DEF2, .mnemonic = #name }
static const InstDesc inst_table[] = {
    // nullary
    NULLARY_OP(RET,        0xC3),
    NULLARY_OP(INT3,       0xCC),
    // unary ops
    UNARY_OP(NOT,                0xF7, 0x02),
    UNARY_OP(NEG,                0xF7, 0x03),
    UNARY_OP(DIV,                0xF7, 0x06),
    UNARY_OP(IDIV,               0xF7, 0x07),
    UNARY_OP2(CALL,        0xE8, 0xFF, 0x02),
    UNARY_OP2(JMP,         0xE9, 0xFF, 0x04),
    // these are used via label but don't have an opcode for it so
    // they resort to using op_i and rx_i as the first two bytes followed
    // by a REL32.
    UNARY_OP(JO,           0x0F, 0x80),
    UNARY_OP(JNO,          0x0F, 0x81),
    UNARY_OP(JB,           0x0F, 0x82),
    UNARY_OP(JNB,          0x0F, 0x83),
    UNARY_OP(JE,           0x0F, 0x84),
    UNARY_OP(JNE,          0x0F, 0x85),
    UNARY_OP(JBE,          0x0F, 0x86),
    UNARY_OP(JA,           0x0F, 0x87),
    UNARY_OP(JS,           0x0F, 0x88),
    UNARY_OP(JNS,          0x0F, 0x89),
    UNARY_OP(JP,           0x0F, 0x8A),
    UNARY_OP(JNP,          0x0F, 0x8B),
    UNARY_OP(JL,           0x0F, 0x8C),
    UNARY_OP(JGE,          0x0F, 0x8D),
    UNARY_OP(JLE,          0x0F, 0x8E),
    UNARY_OP(JG,           0x0F, 0x8F),
    // binary ops but they have an implicit CL on the righthand side
    BINARY_OP_CL(SHL,      0xD2, 0xC0, 0x04),
    BINARY_OP_CL(SHR,      0xD2, 0xC0, 0x05),
    BINARY_OP_CL(SAR,      0xD2, 0xC0, 0x07),
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
    BINARY_OP2(MOVABS,     0xB8),
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

inline static Val val_gpr(GPR g) {
    return (Val) { .type = VAL_GPR, .reg = g };
}

inline static Val val_xmm(XMM x) {
    return (Val) { .type = VAL_XMM, .reg = x };
}

inline static Val val_flags(Cond c) {
    return (Val) { .type = VAL_FLAGS, .reg = c };
}

inline static Val val_global(const TB_Symbol* s) {
    return (Val) { .type = VAL_GLOBAL, .symbol = s };
}

inline static Val val_imm(int32_t imm) {
    return (Val) { .type = VAL_IMM, .imm = imm };
}

inline static Val val_abs(uint64_t abs) {
    return (Val) { .type = VAL_ABS, .abs = abs };
}

inline static Val val_label(int32_t l) {
    return (Val) { .type = VAL_LABEL, .imm = l };
}

inline static Val val_stack(int s) {
    return (Val) {
        .type = VAL_MEM, .reg = RBP, .index = GPR_NONE, .scale = SCALE_X1, .imm = s
    };
}

inline static Val val_base_disp(GPR b, int d) {
    return (Val) {
        .type = VAL_MEM, .reg = b, .index = GPR_NONE, .scale = SCALE_X1, .imm = d
    };
}

inline static Val val_base_index_disp(GPR b, GPR i, Scale s, int d) {
    return (Val) {
        .type = VAL_MEM, .reg = b, .index = i, .scale = SCALE_X1, .imm = d
    };
}

inline static bool is_value_mem(const Val* v) {
    return v->type == VAL_MEM || v->type == VAL_GLOBAL;
}

inline static bool is_value_gpr(const Val* v, GPR g) {
    if (v->type != VAL_GPR) return false;

    return (v->reg == g);
}

inline static bool is_value_xmm(const Val* v, XMM x) {
    if (v->type != VAL_XMM) return false;

    return (v->reg == x);
}

inline static bool is_value_match(const Val* a, const Val* b) {
    if (a->type != b->type) return false;

    if (a->type == VAL_GPR) return a->reg == b->reg;
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
#define INST2SSE(op, a, b, dt)    inst2sse(&ctx->emit, op, a, b, dt)
