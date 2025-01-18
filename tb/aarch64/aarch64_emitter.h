// masks N lowest bits of V and shifts to offset O
#define SET_BITS(V, N, O) ((V & ((1 << N) - 1)) << O)

// Xn refers to the 64bit variants of the registers,
// usually the 32bit aliases are Wn (we don't have enums
// for them because it's not that important, they're equal)
typedef enum {
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
} GPR;

typedef enum {
    EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, NV,
} Cond;

typedef enum {
    LSL, LSR, ASR, ROR,
} ShiftType;

typedef enum {
    AND, BIC, ORR, ORN, EOR, EON, ANDS, BICS,
} LogicOp;

/* UDF
    0000 0000 0000 0000 iiii iiii iiii iiii
    i = imm16 immediate (ignored by hardware)
*/
static void emit_udf(TB_CGEmitter* restrict e, uint16_t imm) {
    EMIT4(e, imm);
}

/* logical (shifted register)
    bcc0 1010 ssnm mmmm iiii iinn nnnd dddd
    b = 32bit/64bit
    c = opcode and/or/xor/ands
    s = shift type lsl/lsr/asr/ror
    n = negate 2nd reg
    m = 2nd reg (shifted)
    i = imm6 shift amount
    n = 1st reg
    d = destination reg
*/
static void emit_dpr_logical(TB_CGEmitter* restrict e, LogicOp o, GPR d, GPR n, GPR m, uint8_t i, ShiftType s, bool is64bit) {
    uint32_t opc  = o >> 1;
    uint32_t N    = o >> 0;
    uint32_t inst = 0b01010 << 24;
    inst |= is64bit ? (1 << 31) : 0;
    inst |= SET_BITS(opc, 2, 29);
    inst |= SET_BITS(s,   2, 22);
    inst |= SET_BITS(N,   1, 21);
    inst |= SET_BITS(m,   5, 16);
    inst |= SET_BITS(i,   5, 10);
    inst |= SET_BITS(n,   5,  5);
    inst |= SET_BITS(d,   5,  0);
    EMIT4(e, inst);
}

/* conditional select
    bps1 1010 110m mmmm cccc qqnn nnnd dddd
    b = 32bit/64bit
    p = op
    s = 1 is unallocated :)
    m = 2nd reg
    c = condition
    q = op2
    n = 1st reg
    d = destination reg
*/
typedef enum {
    // opcode pqq
    CSEL  = 0b000,
    CSINC = 0b001,
    CSINV = 0b100,
    CSNEG = 0b101,
} CondSelOp;

static void emit_cs(TB_CGEmitter* restrict e, CondSelOp o, GPR d, GPR n, GPR m, Cond c, bool is64bit) {
    uint32_t op   = o >> 2;
    uint32_t op2  = o >> 0;
    uint32_t inst = 0b11010110 << 21;
    inst |= is64bit ? (1 << 31) : 0;
    inst |= SET_BITS(op,  1, 30);
    inst |= SET_BITS(m,   5, 16);
    inst |= SET_BITS(c,   4, 12);
    inst |= SET_BITS(op2, 2, 10);
    inst |= SET_BITS(n,   5,  5);
    inst |= SET_BITS(d,   5,  0);
    EMIT4(e, inst);
}

/*
    a--b bbb- ---- ---- ---- ---- ---- ----
    0  0000 reserved
    1  0000 sme
    -  0010 sve
    -  00-1 unallocated
    -  100- data processing (immediate)
    -  101- branches, exception generation, system
    -  -101 data processing (register)
    -  -111 data processing (scalar floating point, advanced simd)
    -  -1-0 loads and stores
    > reserved
         op0    op1
        0aa0 000b bbbb bbbb ---- ---- ---- ----
         00     0 0000 0000 UDF
        > UDF
            0000 0000 0000 0000 iiii iiii iiii iiii
            UDF #<imm>
    > sme
    > sve
    > unallocated
    > data processing (immediate)
    > branches, exception generation, system
    > data processing (register)
        -a-b 101c ccc- ---- dddd dd-- ---- ----
         - 0    0 ---       ---- -- logical (shifted register)
         1 1    0 110       ---- -- data processing (1 source)
         0 1    0 110       ---- -- data processing (2 source)
         - 1    1 ---       ---- -- data processing (3 source)
         - 0    1 --0       ---- -- add/sub (shifted register)
         - 1    0 010       ---- 0- conditional compare (register)
         - 1    0 010       ---- 1- conditional compare (immediate)
         - 1    0 100       ---- -- conditional select
        > logical (shifted register)
            ---0 1010 ss-m mmmm iiii iinn nnnd dddd
                         Rm     imm6   Rn    Rd
                      shift
            abb0 1010 --d- ---- ---- ---- ---- ----
            -00         0 and
            -00         1 bic (bitwise clear) literally just andn :)
            -01         - orr
            -10         - eor
            -11         - ands (sets flags)
        > data processing (1 source)
            -1-1 1010 110- ---- ---- --nn nnnd dddd
                         op2    op1    Rn    Rd
            a1b1 1010 110c cccc dddd dd-- ---- ----
            - 0          0 001- ---- -- unallocated :)
            - 0          0 01-- ---- -- unallocated :-)
            - 0          0 1--- ---- -- unallocated :--)
            - 0          1 ---- ---- -- unallocated :---)
            - 1          - ---- ---- -- unallocated :----)
            - 0          0 0000 0000 01 rbit (reverse bits)
            - 0          0 0000 0000 10 rev16 (reverse 2bytes)
            - 0          0 0000 0000 11 rev (reverse bytes)
            - 0          0 0000 0001 00 clz (count leading zeros)
            - 0          0 0000 0001 01 cls (count leading sign bits)
            - 0          0 0000 0001 10 ctz (count trailing zeros)
            - 0          0 0000 0001 11 cnt (count bits set)
            - 0          0 0000 0010 00 abs (absolute value)
        > data processing (2 source)
            -0-1 1010 110m mmmm ---- --nn nnnd dddd
            size         Rm     opcode Rn      Rd
            a0b1 1010 110- ---- dddd dd-- ---- ----
            - 0                 0000 10 udiv (unsigned divide)
            - 0                 0000 11 sdiv (signed divide)
            - 0                 0010 00 lslv (logical shift left variable)
            - 0                 0010 01 lsrv (logical shift right variable)
            - 0                 0010 10 asrv (arithmetic shift right variable)
            - 0                 0010 11 rorv (rotate right variable)
            - 0                 0110 00 smax (signed maximum)
            - 0                 0110 01 umax (unsigned maximum)
            - 0                 0110 10 smin (signed minimum)
            - 0                 0110 11 umin (unsigned minimum)
        > data processing (3 source)
            ---1 1011 ---m mmmm -aaa aann nnnd dddd
            size         Rm      Ra    Rn    Rd
            abb1 1001 ccc- ---- d--- ---- ---- ----
             nn                   undefined :)
            -00       000       0 madd
            -00       000       1 msub a pattern :)
            100       001       - smaddl/smsubl
            100       010       0 smulh
            100       101       - umaddl/umsubl
            100       110       0 umulh
        > add/sub (shifted register)
            ---0 1011 ss0m mmmm iiii iinn nnnd dddd
                         Rm     imm6   Rn    Rd
                      shift
            abc0 1011 --0- ---- ---- ---- ---- ----
            0-- 32bit
            1-- 64bit
            -0- add
            -1- sub
            --1 set flags
        > conditional compare (register)
            ---1 1010 001m mmmm cccc 0-nn nnn- vvvv
            size         Rm     cond   Rn      nzcv
            abc1 1010 010- ---- ---- 0d-- ---e ----
            --0                       -      - unallocated :)
            -01                       0      0 ccmn (conditional compare negative)
            -11                       0      0 ccmp ()
        > conditional compare (immediate)
            ---1 1010 001i iiii cccc 0-nn nnn- vvvv
            size         imm5   cond   Rn      nzcv
            abc1 1010 010- ---- ---- 0d-- ---e ----
            --0                       -      - unallocated :)
            -01                       0      0 ccmn (conditional compare negative)
            -11                       0      0 ccmp ()
        > conditional select
            ---1 1010 100m mmmm cccc --nn nnnd dddd
                         Rm     cond   Rn      Rd
            abc1 1010 010- ---- ---- dd-- ---- ----
            --1                      -- unallocated :)
            -00                      00 csel
            -00                      01 csinc (increment)
            -10                      00 csinv (invert)
            -10                      01 csneg (negate)
    > data processing (scalar floating point, advanced simd)
        aaaa 111b bccc cddd dddd dd-- ---- ----
    > loads and stores
        aaaa 1b0c cccc cccc cccc cc-- ---- ----
        --01  - 0 ---- ---- ---- -- load register (literal)
        > load register (literal)
            aa01 1b00 iiii iiii iiii iiii iiit tttt
                i immediate (offset)
                t target register
            00    0 ldr 32bit
            01    0 ldr 64bit
            11    0 prfm

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

// Select (?)
TB_SELECT,

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
TB_MUL, mul a, b, c = madd a, b, c, zr

TB_SHL, lslv
TB_SHR, lsrv
TB_SAR, asrv
TB_ROL, (? gonna have to be rorv but negative)
TB_ROR, rorv
TB_UDIV, udiv
TB_SDIV, sdiv
TB_UMOD, (? gonna have to do some funky math)
TB_SMOD, (https://devblogs.microsoft.com/oldnewthing/20220801-00/?p=106922)

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
#define DPI(op, op0) ((op << 30u) | (0b100 << 26u) | (op0 << 23u))

#define DP3(op0, op1, op2) ((op0 << 30u) | (op1 << 28u) | (0b101 << 25u) | (op2 << 21u))

static const DPInst inst_table[] = {
    //         register                     immediate
    [ADD]  = { DPR(0, 0, 0b1000, 0),        DPI(0, 0b010) },
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
static void emit_dp_imm(TB_CGEmitter* restrict e, DPOpcode op, GPR dst, GPR src, uint16_t imm, uint8_t shift, bool _64bit) {
    uint32_t inst = inst_table[op].i | (_64bit ? (1u << 31u) : 0);

    if (op == ADD || op == SUB) {
        assert(shift == 0 || shift == 12);
        inst |= (1 << 22u);
    } else if (op == UBFM) {
        inst |= (1 << 22u);
    }

    inst |= (imm & 0xFFF) << 10u;
    inst |= (src & 0x1F) << 5u;
    inst |= (dst & 0x1F) << 0u;
    EMIT4(e, inst);
}

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

