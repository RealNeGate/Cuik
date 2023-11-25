
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

typedef enum {
    AND,
    ADD,
    ADDS,
    SUB,
    SUBS,
} DPOpcode;

enum {
    SHIFT_LSL,
    SHIFT_LSR,
    SHIFT_ASR,
    SHIFT_RESERVED,
};

typedef struct {
    uint16_t r, i;
} DPInst;

static const DPInst inst_table[] = {
    [AND]  = { .r = 0b00001010, .i = 0b00010010 },
    [ADD]  = { .r = 0b00001011, .i = 0b00010001 },
    [ADDS] = { .r = 0b00101011, .i = 0b00110001 },
    [SUB]  = { .r = 0b01010001, .i = 0b01010001 },
    [SUBS] = { .r = 0b01101011, .i = 0b01110001 },
};

// data processing instruction
//   OP dst, src, imm
//
// 0000 0000 0000 0000 0000 0000 0000 0000
// AOOO OOOO SSII IIII IIII IINN NNND DDDD
//
// A - set when we're doing the 64bit variant of the instruction
// O - is the opcode
// S - shift
// I - immediate
// N - source
// D - destination
static void emit_dp_imm(TB_CGEmitter* restrict e, DPOpcode op, GPR dst, GPR src, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = _64bit ? (1u << 31u) : 0;

    inst |= ((inst_table[op].i & 0xFF) << 24u);

    if (op == ADD || op == SUB) {
        if (shift == 12) inst |= (shift << 22u);
        else if (shift == 0) {}
        else {
            tb_panic("emit_dp_imm: shift amount cannot be %d (only 0 or 12)", shift);
        }
    }

    inst |= (imm & 0xFFF) << 10u;
    inst |= (src & 0b11111) << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}

// data processing instruction
//   OP dst, a, b
//
// 0000 0000 0000 0000 0000 0000 0000 0000
// AOOO OOOO SS_M MMMM IIII IINN NNND DDDD
// x101 1110 xx1m mmmm x000 01nn nnnd dddd  -  add Sd Sn Sm
static void emit_dp_r(TB_CGEmitter* restrict e, DPOpcode op, GPR dst, GPR a, GPR b, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = _64bit ? (1u << 31u) : 0;

    inst |= ((inst_table[op].r & 0x7F) << 24u);

    if (op == ADD || op == SUB) {
        if (shift == 12) inst |= (shift << 22u);
        else if (shift == 0) {}
        else {
            tb_panic("emit_dp_r: shift amount cannot be %d (only 0 or 12)", shift);
        }
    }

    inst |= (b & 0b11111) << 16u;
    inst |= (imm & 0xFFF) << 10u;
    inst |= (a & 0b11111) << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}

static void emit_mov(TB_CGEmitter* restrict e, uint8_t dst, uint8_t src, bool _64bit) {
    uint32_t inst = (_64bit ? (1 << 31u) : 0);

    inst |= (0b00101010 << 24u);
    inst |= (src & 0b11111) << 16u;
    inst |= (0b11111) << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}

static void emit_movz(TB_CGEmitter* restrict e, uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = (_64bit ? (1 << 31u) : 0);

    inst |= (0b010100101 << 23u);
    inst |= shift << 21u;
    inst |= imm << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}

static void emit_movk(TB_CGEmitter* restrict e, uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = (_64bit ? (1 << 31u) : 0);

    inst |= (0b011100101 << 23u);
    inst |= shift << 21u;
    inst |= imm << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}
