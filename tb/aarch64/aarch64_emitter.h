#include <stdint.h>
#include <sys/types.h>
#include "../emitter.h"

#define ENUM(N) enum N typedef N; enum N
#define STRUCT(N) struct N typedef N; struct N

// provides mask of N bits
#define BIT_MASK(N) ((1 << N) - 1)
// get N bits from value V at offset O
#define GET_BITS(V, N, O) ((V >> O) & BIT_MASK(N))
// set N bits at offset O from value V
#define SET_BITS(V, N, O) ((V & BIT_MASK(N)) << O)

// Xn refers to the 64bit variants of the registers,
// usually the 32bit aliases are Wn (we don't have enums
// for them because it's not that important, they're equal)
ENUM(GPR) {
    X0,  X1,   X2,  X3,  X4,  X5,  X6,  X7,
    X8,  X9,  X10, X11, X12, X13, X14, X15,
    X16, X17, X18, X19, X20, X21, X22, X23,
    X24, X25, X26, X27, X28, X29, X30,

    // frame pointer
    FP = 29,
    // link register is basically just the RPC
    LR = 30,
    // It's context specific because ARM lmao
    ZR = 31, SP = 31,

    // not a real gpr
    GPR_NONE = -1,
};

ENUM(Cond) {
    EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, NV,
};

ENUM(ShiftType) {
    LSL, LSR, ASR, ROR,
};

/* UDF
    0000 0000 0000 0000 iiii iiii iiii iiii
    i = imm16 immediate (ignored by hardware)
*/
static void emit_udf(TB_CGEmitter* restrict e, uint16_t imm) {
    EMIT4(e, imm);
}

/* move wide (immediate)
    bpp1 0010 1ssi iiii iiii iiii iiid dddd
    b = 32/64 bit
    p = opcode
    s = shift amount
    i = imm16
    d = destination register
*/
ENUM(MoveOp) {
    MOVN = 0b00, // move inverse immediate
    MOVZ = 0b10, // move zero-extended immediate
    MOVK = 0b11, // move immediate (keep other bits)
};
static void emit_dpi_mov(TB_CGEmitter* restrict e, MoveOp o, GPR d, uint16_t i, uint8_t s, bool is_64bit) {
    //                ___100101_______________________
    uint32_t inst = 0b00010010100000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(o,  2, 29);
    inst |= SET_BITS(s,  2, 21);
    inst |= SET_BITS(i, 16,  5);
    inst |= SET_BITS(d,  5,  0);
    EMIT4(e, inst);
}

/* logical (shifted register)
    bcc0 1010 ssum mmmm iiii iinn nnnd dddd
    b = 32/64 bit
    c = opcode and/or/eor/ands
    s = shift type lsl/lsr/asr/ror
    u = negate 2nd reg
    m = 2nd reg (shifted)
    i = imm6 shift amount
    n = 1st reg
    d = destination reg
*/
ENUM(LogicOp) {
    //       ccu
    AND  = 0b000, // d = n & m
    BIC  = 0b001, // d = n & ~m
    ORR  = 0b010, // d = n | m
    ORN  = 0b011, // d = n | ~m
    EOR  = 0b100, // d = n ^ m
    EON  = 0b101, // d = n ^ ~m
    ANDS = 0b110, // d = n & m (set flags)
    BICS = 0b111, // d = n & ~m (set flags)
};
static void emit_dpr_logical(TB_CGEmitter* restrict e, LogicOp o, GPR d, GPR n, GPR m, uint8_t i, ShiftType s, bool is_64bit) {
    uint32_t c = GET_BITS(o, 2, 1);
    uint32_t u = GET_BITS(o, 1, 0);
    //                ___01010________________________
    uint32_t inst = 0b00001010000000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(c, 2, 29);
    inst |= SET_BITS(s, 2, 22);
    inst |= SET_BITS(u, 1, 21);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(i, 5, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* conditional select
    bps1 1010 110m mmmm cccc qqnn nnnd dddd
    b = 32/64 bit
    p = op
    s = always 0, 1 is unallocated :)
    m = 2nd reg
    c = condition
    q = op2
    n = 1st reg
    d = destination reg
*/
ENUM(CondSelOp) {
    // opcode pqq
    CSEL  = 0b000, // d = c ? n : m
    CSINC = 0b001, // d = c ? n : ++m
    CSINV = 0b100, // d = c ? n : ~m
    CSNEG = 0b101, // d = c ? n : -m
};

static void emit_cs(TB_CGEmitter* restrict e, CondSelOp o, GPR d, GPR n, GPR m, Cond c, bool is_64bit) {
    uint32_t p = GET_BITS(o, 1, 2);
    uint32_t q = GET_BITS(o, 2, 0);
    //                ___11010110_____________________
    uint32_t inst = 0b00011010110000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(p, 1, 30);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(c, 4, 12);
    inst |= SET_BITS(q, 2, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* data processing (1 source)
    b1s1 1010 110p pppp qqqq qqnn nnnd dddd
    b = 32/64 bit
    s = i don't know but it's always 0 according to ISA_A64_xml_A_profile-2024-09.pdf
    p = opcode2
    q = opcode1
    n = 1st register
    d = destination register
*/
ENUM(DPR1op) {
    // op1    qqqqqq
    RBIT  = 0b000000, // reverse bits
    REV16 = 0b000001, // reverse each 2-byte halfwords
    REV   = 0b000010, // reverse bytes
    CLZ   = 0b000100, // count leading zeros
    CLS   = 0b000101, // count leading signs
    CTZ   = 0b000110, // count trailing zeros
    CNT   = 0b000111, // count set bits
    ABS   = 0b001000, // absolute of signed value
};
static void emit_dpr_1(TB_CGEmitter* restrict e, DPR1op q, GPR d, GPR n, bool is_64bit) {
    //                _1_11010110_____________________
    uint32_t inst = 0b01011010110000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    // p is always 0 for what we care about currently
    inst |= SET_BITS(q, 6, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* data processing (2 source)
    b0s1 1010 110m mmmm pppp ppnn nnnd dddd
    b = 32/64 bit
    s = always 0 apart from SUBPS (FEAT_MTE)
    m = 2nd register
    p = opcode
    n = 1st register
    d = destination register
*/
ENUM(DPR2op) {
    // op    pppppp
    UDIV = 0b000010, // d = n / m (unsigned)
    SDIV = 0b000011, // d = n / m (signed)
    LSLV = 0b001000, // d = n << m
    LSRV = 0b001001, // d = n >> m (zero)
    ASRV = 0b001010, // d = n >> m (sign)
    RORV = 0b001011, // rotate right
    SMAX = 0b011000, // d = n > m ? n : m (signed)
    UMAX = 0b011001, // d = n > m ? n : m (unsigned)
    SMIN = 0b011010, // d = n < m ? n : m (signed)
    UMIN = 0b011011, // d = n < m ? n : m (unsigned)
};
static void emit_dpr_2(TB_CGEmitter* restrict e, DPR2op p, GPR d, GPR n, GPR m, bool is_64bit) {
    //                _0_11010110_____________________
    uint32_t inst = 0b00011010110000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(p, 6, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/*  data processing (3 source)
    bpp1 1011 qqqm mmmm raaa aann nnnd dddd
    b = 32/64 bit
    p = opcode bits 5-4
    q = opcode bits 3-1
    m = 2nd register
    r = opcode bits 0
    a = 3rd register
    n = 1st register
    d = destination register
*/
ENUM(DPR3op) {
    // opcode  ppqqqr
    MADD   = 0b000000, // d = a + n * m
    MSUB   = 0b000001, // d = a - n * m
    SMADDL = 0b000010, // d = a + n * m (64bit + 32bit * 32bit) (signed)
    SMSUBL = 0b000011, // d = a - n * m (64bit - 32bit * 32bit) (signed)
    SMULH  = 0b000100, // d = (n * m) >> 64 (signed)
    UMADDL = 0b001010, // d = a + n * m (64bit + 32bit * 32bit) (unsigned)
    UMSUBL = 0b001011, // d = a - n * m (64bit - 32bit * 32bit) (unsigned)
    UMULH  = 0b001100, // d = (n * m) >> 64 (unsigned)
};
static void emit_dpr_3(TB_CGEmitter* restrict e, DPR3op o, GPR d, GPR n, GPR m, GPR a, bool is_64bit) {
    if (o != MADD && o != MSUB) is_64bit = true;
    if (o == SMULH || o == UMULH) a = 0b11111;
    uint32_t p = GET_BITS(o, 2, 4);
    uint32_t q = GET_BITS(o, 3, 1);
    uint32_t r = GET_BITS(o, 1, 0);
    //                ___11011________________________
    uint32_t inst = 0b00011011000000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(p, 2, 29);
    inst |= SET_BITS(q, 3, 21);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(r, 1, 15);
    inst |= SET_BITS(a, 6, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* add/sub (shifted register)
    buf0 1011 ss0m mmmm iiii iinn nnnd dddd
    b = 32/64 bit
    u = add/sub
    f = ignore/set flags
    s = shift type
    m = 2nd register
    i = shift amount
    n = 1st register
    d = destination register
*/
ENUM(DPRaddop) {
    //      u
    ADD = 0b0,
    SUB = 0b1,
};
static void emit_dpr_add(TB_CGEmitter* restrict e, DPRaddop o, GPR d, GPR n, GPR m, ShiftType s, uint8_t i, bool set_flags, bool is_64bit) {
    //                ___01011__0_____________________
    uint32_t inst = 0b00001011000000000000000000000000;
    inst |= is_64bit  ? (1 << 31) : 0;
    inst |= set_flags ? (1 << 29) : 0;
    inst |= SET_BITS(o, 1, 30);
    inst |= SET_BITS(s, 2, 22);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(i, 6, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

static void emit_dpi_add(TB_CGEmitter* restrict e, DPRaddop o, GPR d, GPR n, bool s, uint16_t i, bool is_64bit) {
    uint32_t inst = (0b100010 << 23u);
    inst |= is_64bit  ? (1 << 31) : 0;
    inst |= SET_BITS(o, 1,  30);
    inst |= SET_BITS(s, 1,  22);
    inst |= SET_BITS(i, 12, 10);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* conditional compare (register)
    bps1 1010 001m mmmm cccc 0qnn nnnr ffff
    b = 32/64 bit
    p = opcode 1, when 0 means compare A and ¬B, when 1 means compare A and B
    s = idk but always 1
    m = 2nd register
    c = condition
    q = opcode 2 always 0, unallocated :)
    n = 1st register
    r = opcode 3 always 0, unallocated :)
    f = flag literal

    ---1 1010 001m mmmm cccc 0-nn nnn- vvvv
    size         Rm     cond   Rn      nzcv
    abc1 1010 010- ---- ---- 0d-- ---e ----
    --0                       -      - unallocated :)
    -01                       0      0 ccmn (conditional compare negative)
    -11                       0      0 ccmp ()
*/
ENUM(CCop) {
    CCMN, // flags = c ? n <=> ~m : f
    CCMP, // flags = c ? n <=> m : f
};
static void emit_dpr_cc(TB_CGEmitter* restrict e, CCop o, GPR d, GPR n, GPR m, Cond c, uint8_t else_flags, bool is_64bit) {
    uint32_t s = 1;
    //                ___11010001_________0___________
    uint32_t inst = 0b00011010001000000000000000000000;
    inst |= is_64bit  ? (1 << 31) : 0;
    inst |= SET_BITS(o, 1, 30);
    inst |= SET_BITS(s, 1, 29);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(c, 4, 12);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}



ENUM(FPtype) {
    Single = 0b00,
    Double = 0b01,
    Half   = 0b11,
};

/* conversion between floating-point and integer
    b0s1 1110 tt1m mppp 0000 00nn nnnd dddd
    b = 32/64 bit
    s = always 0
    t = floating point type
    m = rmode (different meanings)
    p = opcode
    n = source register
    d = destination register
*/
ENUM(FPconvINTop) {
    FMOV_F2I = 0b110,
    FMOV_I2F = 0b111,
};
static void emit_fp_convint(TB_CGEmitter* restrict e, FPconvINTop p, FPtype t, GPR d, GPR n, bool top_half, bool is_64bit) {
    //                _0_11110__1_____000000__________
    uint32_t inst = 0b00011110001000000000000000000000;
    inst |= is_64bit ? (1 << 31) : 0;
    inst |= SET_BITS(t, 2, 22);
    inst |= top_half ? (1 << 19) : 0;
    inst |= SET_BITS(p, 3, 16);
    inst |= SET_BITS(n, 5, 5);
    inst |= SET_BITS(d, 5, 0);
    EMIT4(e, inst);
}

/* floating-point data-processing (1 source)
    m0s1 1110 tt1p pppp p100 00nn nnnd dddd
    m = always 0
    s = always 0
    t = floating-point type
    p = opcode
    n = source register
    d = destination register
*/
ENUM(FPDP1op) {
    //       oooooo
    FMOV = 0b000000,
    FNEG = 0b000010,
};
static void emit_fp_dp_1(TB_CGEmitter* restrict e, FPDP1op p, FPtype t, GPR d, GPR n) {
    //                _0_11110__1______10000__________
    uint32_t inst = 0b00011110001000000100000000000000;
    inst |= SET_BITS(t, 2, 22);
    inst |= SET_BITS(p, 6, 15);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* floating-point data-processing (2 source)
    a0s1 1110 tt1m mmmm pppp 10nn nnnd dddd
    a = always 0
    s = always 0
    t = floating point type
    m = 2nd source register
    p = opcode
    n = 1st source register
    d = destination register
*/
ENUM(FPDP2op) {
    FMUL   = 0b0000, // d = n * m
    FDIV   = 0b0001, // d = n / m
    FADD   = 0b0010, // d = n + m
    FSUB   = 0b0011, // d = n - m
    FMAX   = 0b0100, // d = n > m ? n : m (prefers NaN)
    FMIN   = 0b0101, // d = n < m ? n : m (prefers NaN)
    FMAXNM = 0b0110, // d = n > m ? n : m (prefers Number)
    FMINNM = 0b0111, // d = n < m ? n : m (prefers Number)
    FNMUL  = 0b1000, // d = -(n * m)
};
static void emit_fp_dp_2(TB_CGEmitter* restrict e, FPDP2op p, FPtype t, GPR d, GPR n, GPR m) {
    //                _0_11110__1_________10__________
    uint32_t inst = 0b00011110001000000000100000000000;
    inst |= SET_BITS(t, 2, 22);
    inst |= SET_BITS(m, 5, 16);
    inst |= SET_BITS(p, 4, 12);
    inst |= SET_BITS(n, 5,  5);
    inst |= SET_BITS(d, 5,  0);
    EMIT4(e, inst);
}

/* A64 tree -> TB_NodeTypeEnum
    > reserved
        - UDF
    > data processing (immediate)
        > pc rel addressing
            adr - TB_PTR_OFFSET
        > move wide
            movz - TB_ICONST
            movk - TB_ICONST
    > branches, exceptions, system
        > conditional branch (immediate)
            b.cond - TB_BRANCH
        > exception generation
            brk - TB_DEBUGBREAK/TB_TRAP
        > hints
            nop
        > unconditional branch (register)
            br  - TB_BRANCH
            blr - TB_CALL
            ret - TB_RETURN
        > unconditional branch (immediate)
            b - TB_BRANCH
            bl - TB_CALL
        > compare and branch (immediate)
    > data processing (register)
        > data processing (2 source)
            udiv - TB_UDIV
            sdiv - TB_SDIV
            lslv - TB_SHL
            lsrv - TB_SHR
            asrv - TB_SAR
            rorv - TB_ROR (+TB_ROL)
        > data processing (1 source)
            rev - TB_BSWAP
            clz - TB_CLZ
            ctz - TB_CTZ
            cnt - TB_POPCNT
        > logical (shifted register)
            and/bic/ands/bics - TB_AND
            orr/orn           - TB_OR
            eor/eon           - TB_XOR
        > add/sub (shifted register)
            add  - TB_ADD
            sub  - TB_SUB
        > conditional select
            csel - TB_SELECT
        > data processing (3 source)
            madd - TB_MUL
    > data processing -- scalar floating-point and advanced simd
        > conversion between floating-point and fixed-point
        > conversion between floating-point and integer
            fmov - TB_MACH_MOVE
        > floating-point data-processing (1 source)
            fneg - TB_FNEG
        > floating-point compare
        > floating-point immediate
        > floating-point conditional compare
        > floating-point data-processing (2 source)
            fmul - TB_FMUL
            fdiv - TB_FDIV
            fadd - TB_FADD
            fsub - TB_FSUB
            fmax - TB_FMAX
            fmin - TB_FMIN
        > floating-point conditional select
        > floating-point data-processing (3 source)

nodes to implement
// Conversions (?)
TB_TRUNCATE,
TB_FLOAT_TRUNC,
TB_FLOAT_EXT,
TB_SIGN_EXT,
TB_ZERO_EXT,
TB_UINT2FLOAT,
TB_FLOAT2UINT,
TB_INT2FLOAT,
TB_FLOAT2INT,
TB_BITCAST,

// Select
TB_SELECT, csel, fcsel

// Bitmagic
TB_BSWAP, rev
TB_CLZ, clz
TB_CTZ, ctz
TB_POPCNT, cnt

// Integer arithmatic
TB_AND, and
TB_OR, orr
TB_XOR, eor
TB_ADD, add
TB_SUB, sub
TB_MUL, (mul a b c) = (madd a b c zr)

TB_SHL, lslv
TB_SHR, lsrv
TB_SAR, asrv
TB_ROL, (? gonna have to be rorv but negative)
TB_ROR, rorv
TB_UDIV, udiv
TB_SDIV, sdiv
TB_UMOD, (? gonna have to do some funky math)
TB_SMOD, (https://devblogs.microsoft.com/oldnewthing/20220801-00/?p=106922)
    There is no instruction for calculating the remainder.
    You can do that manually by calculating
        r = n − (n ÷ d) × d
    This can be done by following up the division with an msub:
    ; unsigned remainder after division
    udiv    Rq, Rn, Rm          ; Rq = Rn ÷ Rm
    msub    Rr, Rq, Rm, Rn      ; Rr = Rn - Rq × Rm
                                ;    = Rn - (Rn ÷ Rm) × Rm
    ; signed remainder after division
    sdiv    Rq, Rn, Rm          ; Rq = Rn ÷ Rm
    msub    Rr, Rq, Rm, Rn      ; Rr = Rn - Rq × Rm
                                ;    = Rn - (Rn ÷ Rm) × Rm


// Float arithmatic
TB_FADD, fadd
TB_FSUB, fsub
TB_FMUL, fmul
TB_FDIV, fdiv
TB_FMIN, fmin
TB_FMAX, fmax

// Comparisons (?cmp)
TB_CMP_EQ,
TB_CMP_NE,
TB_CMP_ULT,
TB_CMP_ULE,
TB_CMP_SLT,
TB_CMP_SLE,
TB_CMP_FLT,
TB_CMP_FLE,

TB_FRAME_PTR,
*/

static void emit_ret(TB_CGEmitter* restrict e, GPR rn) {
    // 1101 0110 0101 1111 0000 00NN NNN0 0000
    //
    // 'ret rn' just does 'mov pc, rn', although in practice
    // we only pass the link reg to it.
    uint32_t inst = 0b11010110010111110000000000000000;
    inst |= (rn & 0b11111) << 5u;
    EMIT4(e, inst);
}

/* old stuff
// refers to the data processing immediate operand.
// Aarch64 has a bunch of weird immediate fields so
// we might wanna rename this later.
typedef struct {
    uint16_t imm;
    uint8_t shift;
} Immediate;

typedef enum {
    ADD,
    SUB,
    UDIV,
    SDIV,
} DPOpcode;

enum {
    SHIFT_LSL,
    SHIFT_LSR,
    SHIFT_ASR,
    SHIFT_RESERVED,
};

typedef struct {
    uint32_t r, i;
} DPInst;

// op0  30-29
// op1  28
// 101  27-25
// op2  24-21
// op3  15-10
#define DPR(op0, op1, op2, op3) ((op0 << 29u) | (op1 << 28) | (0b101 << 25) | (op2 << 21) | (op3 << 10))
// op   30
// 100  28-26
// op0  25-23
#define DPI(op, op0) ()

#define DP3(op0, op1, op2) ((op0 << 30u) | (op1 << 28u) | (0b101 << 25u) | (op2 << 21u))

static const DPInst inst_table[] = {
    //         register                     immediate
    [ADD]  = { DPR(0, 0, 0b1000, 0),        DPI(0, ) },
    [SUB]  = { DPR(2, 0, 0b1000, 0),        DPI(1, 0b010) },
    [EOR]  = { 0b01001010000 << 21u,         0 },

    [UDIV] = { DPR(0, 1, 0b0110, 0b000010) },
    [SDIV] = { DPR(0, 1, 0b0110, 0b000011) },
};

enum {
    UBFM = DPI(0, 0b110) | (0b10 << 29u),

    //                       op0
    //                       V
    MADD = 0b00011011000000000000000000000000,
    MSUB = 0b00011011000000001000000000000000,
};

static void emit_ret(TB_CGEmitter* restrict e, GPR rn) {
    // 1101 0110 0101 1111 0000 00NN NNN0 0000
    //
    // 'ret rn' just does 'mov pc, rn', although in practice
    // we only pass the link reg to it.
    uint32_t inst = 0b11010110010111110000000000000000;
    inst |= (rn & 0b11111) << 5u;
    EMIT4(e, inst);
}

// OP Rd, Rn, Rm, Ra
static void emit_dp3(TB_CGEmitter* restrict e, uint32_t inst, GPR d, GPR n, GPR m, GPR a, bool _64bit) {
    inst |= (_64bit ? (1u << 31u) : 0);
    inst |= (m & 0x1F) << 16u;
    inst |= (a & 0x1F) << 10u;
    inst |= (n & 0x1F) << 5u;
    inst |= (d & 0x1F) << 0u;
    EMIT4(e, inst);
}

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

// bitfield
static void emit_bitfield(TB_CGEmitter* restrict e, uint32_t op, GPR dst, GPR src, uint8_t immr, uint8_t imms, bool _64bit) {
    uint32_t inst = op | (_64bit ? (1u << 31u) : 0);
    inst |= (immr & 0b111111) << 16u;
    inst |= (imms & 0b111111) << 10u;
    inst |= (src  & 0b11111) << 5u;
    inst |= (dst  & 0b11111) << 0u;
    EMIT4(e, inst);
}

// data processing instruction
//   OP dst, a, b
//
// 0000 0000 0000 0000 0000 0000 0000 0000
// AOOO OOOO SS_M MMMM IIII IINN NNND DDDD
// x101 1110 xx1m mmmm x000 01nn nnnd dddd  -  add Sd Sn Sm
static void emit_dp_r(TB_CGEmitter* restrict e, DPOpcode op, GPR dst, GPR a, GPR b, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = inst_table[op].r | (_64bit ? (1u << 31u) : 0);

    if (op == ADD || op == SUB) {
        assert(shift == 0 || shift == 12);
        inst |= (1 << 22u);
    } else if (op == UBFM) {
        if (shift) inst |= (1 << 22u);
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

// clear means movz, else movk
static void emit_movimm(TB_CGEmitter* restrict e, uint8_t dst, uint16_t imm, uint8_t shift, bool _64bit, bool clear) {
    uint32_t inst = (_64bit ? (1 << 31u) : 0);

    if (clear) {
        inst |= (0b010100101 << 23u);
    } else {
        inst |= (0b011100101 << 23u);
    }
    inst |= shift << 21u;
    inst |= imm << 5u;
    inst |= (dst & 0b11111) << 0u;
    EMIT4(e, inst);
}
*/
