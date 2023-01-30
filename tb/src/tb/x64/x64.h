#pragma once
#include "../codegen/tree.h"
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
    bool is_spill;
    TB_DataType dt;
    TB_Reg r;

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
    // Integer data processing
    ADD, AND, OR, SUB, XOR, CMP, MOV,
    // weird ones
    TEST, LEA, IMUL, XCHG, XADD,
    // casts
    MOVSXB, MOVSXW, MOVSXD, MOVZXB, MOVZXW
} Inst2Type;

typedef enum Inst2FPType {
    FP_MOV, FP_ADD, FP_SUB, FP_MUL, FP_DIV, FP_CMP, FP_UCOMI,
    FP_SQRT, FP_RSQRT, FP_AND, FP_OR, FP_XOR,
    FP_CVT, // cvtss2sd or cvtsd2ss
} Inst2FPType;

typedef enum ExtMode {
    // Normal
    EXT_NONE,

    // DEF instructions have a 0F prefix
    EXT_DEF,

    // same as DEF but for MOVZX and MOVSX
    // these are forced as always load.
    EXT_DEF2
} ExtMode;

// Describes what general 2 operand instructions are like
typedef struct Inst2 {
    uint8_t op;

    // IMMEDIATES
    uint8_t op_i;
    uint8_t rx_i;

    ExtMode ext : 8;
} Inst2;

static const GPR WIN64_GPR_PARAMETERS[4] = { RCX, RDX, R8, R9 };
static const GPR SYSV_GPR_PARAMETERS[6] = { RDI, RSI, RDX, RCX, R8, R9 };

typedef enum Inst1 {
    // 0xF7
    NOT  = 0xF702,
    NEG  = 0xF703,
    DIV  = 0xF706,
    IDIV = 0xF707,

    // 0xFF
    CALL_RM = 0xFF02
} Inst1;

static const Inst2 inst2_tbl[] = {
    [ADD]  = { 0x00, 0x80, 0x00 },
    [AND]  = { 0x20, 0x80, 0x04 },
    [OR]   = { 0x08, 0x80, 0x01 },
    [SUB]  = { 0x28, 0x80, 0x05 },
    [XOR]  = { 0x30, 0x80, 0x06 },
    [CMP]  = { 0x38, 0x80, 0x07 },
    [MOV]  = { 0x88, 0xC6, 0x00 },
    [TEST] = { 0x84, 0xF6, 0x00 },

    [XCHG] = { 0x86 },
    [XADD] = { 0xC0, .ext = EXT_DEF },
    [LEA]  = { 0x8D },

    [IMUL] = { 0xAF, .ext = EXT_DEF },

    [MOVSXB] = { 0xBE, .ext = EXT_DEF2 },
    [MOVSXW] = { 0xBF, .ext = EXT_DEF2 },
    [MOVSXD] = { 0x63, .ext = EXT_NONE },

    [MOVZXB] = { 0xB6, .ext = EXT_DEF2 },
    [MOVZXW] = { 0xB7, .ext = EXT_DEF2 }
};

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

static int get_data_type_size(const TB_DataType dt);

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif
static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

// shorthand macros
#define STACK_ALLOC(size, align) (ctx->stack_usage = align_up(ctx->stack_usage + (size), align), - ctx->stack_usage)
#define INST1(op, a)              inst1(&ctx->emit, op, a)
#define INST2(op, a, b, dt)       inst2(&ctx->emit, op, a, b, dt)
#define INST2SSE(op, a, b, flags) inst2sse(&ctx->emit, op, a, b, flags)
#define JCC(cc, label)            jcc(&ctx->emit, cc, label)
#define JMP(label)                jmp(&ctx->emit, label)
#define RET_JMP()                 ret_jmp(&ctx->emit)
