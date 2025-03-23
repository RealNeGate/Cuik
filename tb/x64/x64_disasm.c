#include <tb_x64.h>
#include <common.h> // __debugbreak

// this is used to parse ModRM and SIB
#define UNPACK_233(a, b, c, src) \
(a = (src >> 6), b = (src >> 3) & 7, c = (src & 7))

#define READ16(x)   (memcpy(&(x), &data[current], 2), current += 2, x)
#define READ32(x)   (memcpy(&(x), &data[current], 4), current += 4, x)
#define READ64(x)   (memcpy(&(x), &data[current], 8), current += 8, x)

#define READ16LE(x) READ16(x)
#define READ32LE(x) READ32(x)
#define READ64LE(x) READ64(x)

bool tb_x86_disasm(TB_X86_Inst* restrict inst, size_t length, const uint8_t* data) {
    size_t current = 0;
    uint8_t rex = 0;

    inst->dt = TB_X86_DWORD;

    // legacy prefixes
    uint8_t b;
    uint32_t flags = 0;
    while (true) {
        b = data[current++];
        switch (b) {
            case 0xF0: flags |= TB_X86_INSTR_LOCK;        break;
            case 0xF3: flags |= TB_X86_INSTR_REP;         break;
            case 0xF2: flags |= TB_X86_INSTR_REPNE;       break;
            case 0x66: inst->dt = TB_X86_WORD;            break;
            case 0x67: inst->dt = TB_X86_DWORD;           break;
            case 0x2E: inst->segment = TB_X86_SEGMENT_CS; break;
            case 0x36: inst->segment = TB_X86_SEGMENT_SS; break;
            case 0x3E: inst->segment = TB_X86_SEGMENT_DS; break;
            case 0x26: inst->segment = TB_X86_SEGMENT_ES; break;
            case 0x64: inst->segment = TB_X86_SEGMENT_FS; break;
            case 0x65: inst->segment = TB_X86_SEGMENT_GS; break;
            default: goto done_prefixing;
        }
    }

    // rex/vex
    done_prefixing:;
    if ((b & 0xF0) == 0x40) {
        rex = b;
        b = data[current++];
    }

    // opcode translation (from 1-3 bytes to 10bits):
    //   op       => 00________
    //   0F op    => 01________
    //   0F 38 op => 10________
    //   0F 3A op => 11________
    uint32_t op = 0;
    if (b == 0x0F) {
        b = data[current++];
        switch (b) {
            case 0x38: b = data[current++], op = 0x200 | b; break;
            case 0x3A: b = data[current++], op = 0x300 | b; break;
            default:   op = 0x100 | b; break;
        }
    } else {
        op |= b;
    }

    enum {
        // fancy x86 operand
        OP_MODRM  = 1,
        // r/m operand will appear on the left side by default, this will flip that
        OP_DIR    = 2,
        // immediate with a size is based on the operand size
        OP_IMM    = 4,
        // signed 8bit immediate
        OP_IMM8   = 8,
        // operand size is forcibly 8bit
        OP_8BIT   = 16,
        // operand size is forcibly 64bit
        OP_64BIT  = 32,
        // the bottom 3bits of the opcode are the base reg
        OP_PLUSR  = 64,
        // rx represents an extended piece of the opcode
        OP_FAKERX = 128,
        // no operands
        OP_0ARY   = 256,
        // 2 datatypes on the instruction
        OP_2DT    = 512,
        // rcx is the rx field
        OP_RCX    = 1024,
        // rax is the rm field
        OP_RAX    = 2048,
        // vector op
        OP_SSE    = 4096,
        OP_SCALAR = 8192,
    };

    #define NORMIE_BINOP(op) [op+0] = OP_MODRM | OP_8BIT, [op+1] = OP_MODRM, [op+2] = OP_MODRM | OP_DIR | OP_8BIT, [op+3] = OP_MODRM | OP_DIR, [op+4] = OP_RAX | OP_IMM8, [op+5] = OP_RAX | OP_IMM
    #define _0F(op)   [0x100+op]
    #define _0F2(a,b) [0x100+a ... 0x100+b]
    static const uint32_t op_map[1024] = {
        NORMIE_BINOP(0x00), // add
        NORMIE_BINOP(0x08), // or
        NORMIE_BINOP(0x10), // adc
        NORMIE_BINOP(0x20), // and
        NORMIE_BINOP(0x28), // sub
        NORMIE_BINOP(0x30), // xor
        NORMIE_BINOP(0x38), // cmp

        // mov
        [0x88] = OP_MODRM | OP_8BIT,
        [0x89] = OP_MODRM,
        [0x8A] = OP_MODRM | OP_DIR | OP_8BIT,
        [0x8B] = OP_MODRM | OP_DIR,

        // push r+
        // pop  r+
        [0x50 ... 0x5F] = OP_PLUSR | OP_64BIT,
        // movsxd reg, r/m
        [0x63]          = OP_MODRM | OP_DIR | OP_2DT,
        // imul reg, r/m, imm
        [0x69]          = OP_MODRM | OP_DIR | OP_IMM,
        // jcc rel8
        [0x70 ... 0x7F] = OP_IMM8,
        // OP r/m8, imm8
        [0x80]          = OP_MODRM | OP_IMM | OP_8BIT | OP_FAKERX,
        // OP r/m, imm32
        [0x81]          = OP_MODRM | OP_IMM | OP_FAKERX,
        // OP r/m, imm8
        [0x83]          = OP_MODRM | OP_IMM8 | OP_FAKERX,
        // test r/m, reg
        [0x84]          = OP_MODRM | OP_8BIT,
        [0x85]          = OP_MODRM,
        // lea reg, r/m
        [0x8D]          = OP_MODRM | OP_DIR,
        // nop
        [0x90]          = OP_0ARY,
        // cbw/cwde/cdqe
        [0x98]          = OP_0ARY,
        // cwd/cdq/cqo
        [0x99]          = OP_0ARY,
        // mov r+   imm
        [0xB0 ... 0xB7] = OP_PLUSR | OP_IMM | OP_8BIT,
        [0xB8 ... 0xBF] = OP_PLUSR | OP_IMM,
        // sar r/m, imm8
        // shl r/m, imm8
        // shr r/m, imm8
        [0xC0]          = OP_MODRM | OP_IMM8 | OP_FAKERX | OP_8BIT,
        [0xC1]          = OP_MODRM | OP_IMM8 | OP_FAKERX,
        // ret
        [0xC3]          = OP_0ARY,
        // mov r/m  imm
        [0xC6]          = OP_MODRM | OP_IMM | OP_8BIT | OP_FAKERX,
        [0xC7]          = OP_MODRM | OP_IMM | OP_FAKERX,
        // pushf
        [0x9C]          = OP_0ARY,
        // popf
        [0x9D]          = OP_0ARY,
        // movs
        [0xA4 ... 0xA5] = OP_0ARY,
        // stos
        [0xAA ... 0xAB] = OP_0ARY,
        // scas
        [0xAE ... 0xAF] = OP_0ARY,
        // int3
        [0xCC]          = OP_0ARY,
        // sar r/m, CL
        // shl r/m, CL
        // shr r/m, CL
        // rol r/m, CL
        // ror r/m, CL
        [0xD0]          = OP_MODRM | OP_FAKERX | OP_8BIT,
        [0xD1]          = OP_MODRM | OP_FAKERX,
        [0xD2]          = OP_MODRM | OP_RCX | OP_FAKERX | OP_8BIT,
        [0xD3]          = OP_MODRM | OP_RCX | OP_FAKERX,
        // call rel32
        [0xE8]          = OP_IMM,
        // jmp rel32
        [0xE9]          = OP_IMM,
        // jmp rel8
        [0xEB]          = OP_IMM8,
        // idiv r/m8
        [0xF6] = OP_MODRM | OP_FAKERX | OP_8BIT,
        [0xF7] = OP_MODRM | OP_FAKERX,
        // dec/call/jmp r/m
        [0xFE] = OP_MODRM | OP_FAKERX,
        [0xFF] = OP_MODRM | OP_FAKERX,

        ////////////////////////////////
        // ESCAPE OPS
        ////////////////////////////////
        // syscall
        _0F(0x05)        = OP_0ARY,
        // ud2
        _0F(0x0B)        = OP_0ARY,
        // SSE: movu
        _0F(0x10)        = OP_MODRM | OP_SSE | OP_DIR,
        _0F(0x11)        = OP_MODRM | OP_SSE,
        // nop r/m
        _0F(0x1F)        = OP_MODRM,
        // SSE: ucomi
        _0F(0x2E)        = OP_MODRM | OP_SSE | OP_DIR | OP_SCALAR,
        // shufps
        _0F(0xC6)        = OP_MODRM | OP_SSE | OP_DIR | OP_IMM8,
        // bt r/m, reg
        _0F(0xA3)        = OP_MODRM,
        // imul reg, r/m
        _0F(0xAF)        = OP_MODRM,
        // bswap r+
        _0F2(0xC8, 0xCF) = OP_PLUSR,
        // cmovcc reg, r/m
        _0F2(0x40, 0x4F) = OP_MODRM | OP_DIR,
        // SSE: add, mul, sub, min, div, max
        _0F2(0x51, 0x5F) = OP_MODRM | OP_DIR | OP_SSE,
        // movzx reg, r/m
        _0F(0xB6)        = OP_MODRM | OP_2DT | OP_DIR,
        _0F(0xB7)        = OP_MODRM | OP_2DT | OP_DIR,
        // movsx reg, r/m
        _0F(0xBE)        = OP_MODRM | OP_2DT | OP_DIR,
        _0F(0xBF)        = OP_MODRM | OP_2DT | OP_DIR,
        // jcc rel32
        _0F2(0x80, 0x8F) = OP_IMM,
        // setcc r/m
        _0F2(0x90, 0x9F) = OP_MODRM | OP_FAKERX,
    };
    #undef NORMIE_BINOP
    #undef _0F

    uint32_t props = op_map[op];
    if (props == 0) {
        return false;
    }

    uint32_t regs = 0;
    int32_t  disp = 0;
    uint8_t  scale = 0;

    // in the "default" "type" "system", REX.W is 64bit, certain ops
    // will mark they're 8bit and most will just be 32bit (with 16bit on ADDR16)
    if (props & OP_SSE) {
        if (props & OP_SCALAR) {
            if (0) {}
            else if (flags & TB_X86_INSTR_REPNE) { inst->dt = TB_X86_F64x1; } // sd     REPNE  OPCODE
            else if (flags & TB_X86_INSTR_REP)   { inst->dt = TB_X86_F32x1; } // ss     REP    OPCODE
            else if (inst->dt == TB_X86_WORD)    { inst->dt = TB_X86_F64x1; } // pd     DATA16 OPCODE
            else                                 { inst->dt = TB_X86_F32x1; } // ps     __     OPCODE
        } else {
            if (0) {}
            else if (flags & TB_X86_INSTR_REPNE) { inst->dt = TB_X86_F64x1; } // sd     REPNE  OPCODE
            else if (flags & TB_X86_INSTR_REP)   { inst->dt = TB_X86_F32x1; } // ss     REP    OPCODE
            else if (inst->dt == TB_X86_WORD)    { inst->dt = TB_X86_F64x2; } // pd     DATA16 OPCODE
            else                                 { inst->dt = TB_X86_F32x4; } // ps     __     OPCODE
        }

        flags &= ~(TB_X86_INSTR_REP | TB_X86_INSTR_REPNE);
    } else if (props & OP_64BIT) {
        inst->dt = TB_X86_QWORD;
    } else if (props & OP_8BIT) {
        inst->dt = TB_X86_BYTE;
    } else {
        if (rex & 8) inst->dt = TB_X86_QWORD;
    }

    if (props & OP_MODRM) {
        uint8_t mod, rx, rm, modrm = data[current++];
        UNPACK_233(mod, rx, rm, modrm);

        if (props & OP_FAKERX) {
            if (rx <= 1) {
                // TEST r/m, imm
                if (op == 0xF6)      { props |= OP_IMM8; }
                else if (op == 0xF7) { props |= OP_IMM; }
            }

            regs |= 0xFF0000; // no rx since it's reserved
            op  |= rx << 12;
        } else {
            // unpack rx
            regs |= ((rex&4 ? 8 : 0) | rx) << 16;
        }

        if (mod == MOD_DIRECT) {
            // unpack base
            regs |= (rex&1 ? 8 : 0) | rm;
        } else {
            flags |= TB_X86_INSTR_INDIRECT;
            if (rm == TB_X86_RSP) { // use SIB
                uint8_t index, base, sib = data[current++];
                UNPACK_233(scale, index, base, sib);

                if (base == TB_X86_RSP && index == TB_X86_RSP) {
                    // no index
                    regs |= 0xFF00;
                } else if (mod == 0 && base == TB_X86_RBP) {
                    // indirect disp32
                    regs |= 0xFFFF;
                }

                regs |= (rex&1 ? 8 : 0) | base;
                regs |= ((rex&2 ? 8 : 0) | index) << 8;
            } else {
                if (mod == MOD_INDIRECT && rm == TB_X86_RBP) {
                    // rip-relative
                    regs |= 0xFFFF;
                } else {
                    // base-only
                    regs |= 0xFF00;
                    regs |= (rex&1 ? 8 : 0) | rm;
                }
            }

            // unpack displacement
            inst->disp_pos = current;
            if (mod == MOD_INDIRECT_DISP8) {
                inst->disp = (int8_t) data[current++];
            } else if (mod == MOD_INDIRECT_DISP32 || (regs & 0xFFFF) == 0xFFFF) {
                memcpy(&inst->disp, &data[current], sizeof(disp));
                current += 4;
            } else {
                inst->disp = 0;
            }
        }

        if (props & OP_RCX) {
            regs &= ~0xFF0000;
            regs |= RCX << 16;
        }
    } else if (props & OP_RAX) {
        regs |= 0xFFFF00;
        regs |= RAX << 0;
    } else if (props & OP_PLUSR) {
        regs |= 0xFFFF00;
        regs |= (rex&1 ? 8 : 0) | (op & 0x7);

        // discard those reg bits in the opcode
        op &= ~0x7;
    } else {
        regs |= 0xFFFFFF;
    }

    if (props & OP_IMM8) {
        inst->imm = (int8_t) data[current++];
        flags |= TB_X86_INSTR_IMMEDIATE;
    } else if (props & OP_IMM) {
        assert(inst->dt >= TB_X86_BYTE && inst->dt <= TB_X86_QWORD);
        int size = 1 << (inst->dt - TB_X86_BYTE);
        if (size > 4 && (op < 0xB8 || op > 0xBF)) {
            // only operation with 8byte immediates is movabs (0xB8 ... 0xBF)
            size = 4;
        }

        uint64_t x;
        memcpy(&x, &data[current], size);
        current += size;

        inst->imm = tb__sxt(x, size*8, 64);
        flags |= TB_X86_INSTR_IMMEDIATE;
    }

    if (props & OP_DIR) {
        flags |= TB_X86_INSTR_DIRECTION;
    }

    if (props & OP_2DT) {
        if (op == 0x1B6 || op == 0x1B7 || op == 0x1BE || op == 0x1BF) {
            inst->dt2 = (op == 0x1B6 || op == 0x1BE) ? TB_X86_BYTE : TB_X86_WORD;
        } else {
            inst->dt2 = TB_X86_DWORD;
        }
    } else {
        inst->dt2 = inst->dt;
    }

    inst->opcode = op;
    inst->scale  = scale;
    inst->regs   = regs;
    inst->flags  = flags;
    inst->length = current;
    return true;
}

const char* tb_x86_mnemonic(TB_X86_Inst* inst) {
    if (inst->opcode == 0x99) {
        // cwd/cdq/cqo
        if (inst->dt == TB_X86_WORD)  return "cwd";
        if (inst->dt == TB_X86_DWORD) return "cdq";
        if (inst->dt == TB_X86_QWORD) return "cqo";
        return "??";
    } else if (inst->opcode == 0x98) {
        // cbw/cwde/cdqe
        if (inst->dt == TB_X86_WORD)  return "cbw";
        if (inst->dt == TB_X86_DWORD) return "cwde";
        if (inst->dt == TB_X86_QWORD) return "cdqe";
        return "??";
    }

    #define _0F(op)      0x100+op
    #define _0Fx(op, rx) 0x100+op+(rx<<12)
    #define _Rx(op, rx)  op+(rx<<12)
    #define NORMIE_BINOP(op) case 0x80 + (op<<12): case 0x81 + (op<<12): case 0x83 + (op<<12)
    switch (inst->opcode) {
        case _0F(0x0B): return "ud2";
        case _0F(0x31): return "rdtsc";
        case 0xCC: return "int3";
        case 0x9C: return "pushf";
        case 0x9D: return "popf";

        case _0Fx(0x18, 0): return "prefetchnta";
        case _0Fx(0x18, 1): return "prefetch0";
        case _0Fx(0x18, 2): return "prefetch1";
        case _0Fx(0x18, 3): return "prefetch2";

        case _0F(0xC8) ... _0F(0xCF): return "bswap";
        case _0F(0xC6): return "shuf";

        case 0x00 ... 0x05: NORMIE_BINOP(0): return "add";
        case 0x08 ... 0x0D: NORMIE_BINOP(1): return "or";
        case 0x10 ... 0x1D: NORMIE_BINOP(2): return "adc";
        case 0x20 ... 0x25: NORMIE_BINOP(4): return "and";
        case 0x28 ... 0x2D: NORMIE_BINOP(5): return "sub";
        case 0x30 ... 0x33: NORMIE_BINOP(6): return "xor";
        case 0x38 ... 0x3D: NORMIE_BINOP(7): return "cmp";
        case 0x88 ... 0x8B: case 0x00C6: case 0x00C7: return "mov";

        case 0xA4: case 0xA5: return "movs";
        case 0xAA: case 0xAB: return "stos";
        case 0xAE: case 0xAF: return "scas";

        case 0x00C0: case 0x00C1: case 0x00D0: case 0x00D1: case 0x00D2: case 0x00D3: return "rol";
        case 0x10C0: case 0x10C1: case 0x10D0: case 0x10D1: case 0x10D2: case 0x10D3: return "ror";
        case 0x40C0: case 0x40C1: case 0x40D0: case 0x40D1: case 0x40D2: case 0x40D3: return "shl";
        case 0x50C0: case 0x50C1: case 0x50D0: case 0x50D1: case 0x50D2: case 0x50D3: return "shr";
        case 0x70C0: case 0x70C1: case 0x70D0: case 0x70D1: case 0x70D2: case 0x70D3: return "sar";

        case 0x00F6: case 0x00F7: return "test";
        case 0x20F6: case 0x20F7: return "not";
        case 0x30F6: case 0x30F7: return "neg";
        case 0x60F6: case 0x60F7: return "div";
        case 0x70F6: case 0x70F7: return "idiv";

        case _0F(0x05): return "syscall";
        case 0x84: case 0x85: return "test";

        case _0F(0x10): case _0F(0x11): return "mov";
        case _0F(0x58): return "add";
        case _0F(0x59): return "mul";
        case _0F(0x5C): return "sub";
        case _0F(0x5D): return "min";
        case _0F(0x5E): return "div";
        case _0F(0x5F): return "max";
        case _0F(0xC2): return "cmp";
        case _0F(0x2A): return "cvtsi";
        case _0F(0x2C): return "cvtsi";
        case _0F(0x2E): return "ucomi";
        case _0F(0x51): return "sqrt";
        case _0F(0x52): return "rsqrt";
        case _0F(0x54): return "and";
        case _0F(0x56): return "or";
        case _0F(0x57): return "xor";

        case 0xB0 ... 0xBF: return "mov";
        case _0F(0xB6): case _0F(0xB7): return "movzx";
        case _0F(0xBE): case _0F(0xBF): return "movsx";

        case 0x8D: return "lea";
        case 0x90: return "nop";
        case 0xC3: return "ret";
        case 0x63: return "movsxd";
        case 0x50: return "push";
        case 0x58: return "pop";

        case 0x00FF: return "inc";
        case 0x10FF: return "dec";

        case 0xE8: case 0x20FF: case 0x30FF: return "call";
        case 0xEB: case 0xE9: case 0x40FF: case 0x50FF: return "jmp";

        case _0F(0x1F): return "nop";
        case _0F(0xA3): return "bt";
        case 0x68: return "push";
        case _0F(0xAF): case 0x69: case 0x6B: return "imul";

        case _0F(0x40): return "cmovo";
        case _0F(0x41): return "cmovno";
        case _0F(0x42): return "cmovb";
        case _0F(0x43): return "cmovnb";
        case _0F(0x44): return "cmove";
        case _0F(0x45): return "cmovne";
        case _0F(0x46): return "cmovbe";
        case _0F(0x47): return "cmova";
        case _0F(0x48): return "cmovs";
        case _0F(0x49): return "cmovns";
        case _0F(0x4A): return "cmovp";
        case _0F(0x4B): return "cmovnp";
        case _0F(0x4C): return "cmovl";
        case _0F(0x4D): return "cmovge";
        case _0F(0x4E): return "cmovle";
        case _0F(0x4F): return "cmovg";

        case _0F(0x90): return "seto";
        case _0F(0x91): return "setno";
        case _0F(0x92): return "setb";
        case _0F(0x93): return "setnb";
        case _0F(0x94): return "sete";
        case _0F(0x95): return "setne";
        case _0F(0x96): return "setbe";
        case _0F(0x97): return "seta";
        case _0F(0x98): return "sets";
        case _0F(0x99): return "setns";
        case _0F(0x9A): return "setp";
        case _0F(0x9B): return "setnp";
        case _0F(0x9C): return "setl";
        case _0F(0x9D): return "setge";
        case _0F(0x9E): return "setle";
        case _0F(0x9F): return "setg";

        case _0F(0x80): case 0x70: return "jo";
        case _0F(0x81): case 0x71: return "jno";
        case _0F(0x82): case 0x72: return "jb";
        case _0F(0x83): case 0x73: return "jnb";
        case _0F(0x84): case 0x74: return "je";
        case _0F(0x85): case 0x75: return "jne";
        case _0F(0x86): case 0x76: return "jbe";
        case _0F(0x87): case 0x77: return "ja";
        case _0F(0x88): case 0x78: return "js";
        case _0F(0x89): case 0x79: return "jns";
        case _0F(0x8A): case 0x7A: return "jp";
        case _0F(0x8B): case 0x7B: return "jnp";
        case _0F(0x8C): case 0x7C: return "jl";
        case _0F(0x8D): case 0x7D: return "jge";
        case _0F(0x8E): case 0x7E: return "jle";
        case _0F(0x8F): case 0x7F: return "jg";

        default: return "??";
    }
    #undef NORMIE_BINOP
    #undef _0F
}

const char* tb_x86_reg_name(int8_t reg, TB_X86_DataType dt) {
    static const char* X86__GPR_NAMES[4][16] = {
        { "al",  "cl",  "dl",  "bl",  "spl", "bpl", "sil", "dil", "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b" },
        { "ax",  "cx",  "dx",  "bx",  "sp",  "bp",  "si",  "di",  "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w" },
        { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d" },
        { "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8",  "r9",  "r10",  "r11",  "r12",  "r13",  "r14",  "r15" }
    };

    static const char* X86__HIGH_NAMES[] = {
        "ah", "ch", "dh", "bh"
    };

    if (dt >= TB_X86_BYTE && dt <= TB_X86_QWORD) {
        return X86__GPR_NAMES[dt - TB_X86_BYTE][reg];
    } else if (dt >= TB_X86_F32x1 && dt <= TB_X86_F64x2) {
        static const char* X86__XMM_NAMES[] = {
            "xmm0", "xmm1", "xmm2",  "xmm3",  "xmm4",  "xmm5",  "xmm6",  "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
        };

        return X86__XMM_NAMES[reg];
    } else {
        return "??";
    }
}

const char* tb_x86_type_name(TB_X86_DataType dt) {
    switch (dt) {
        case TB_X86_BYTE:  return "byte";
        case TB_X86_WORD:  return "word";
        case TB_X86_DWORD: return "dword";
        case TB_X86_QWORD: return "qword";
        case TB_X86_F32x1: return "dword";
        case TB_X86_F64x1: return "qword";
        case TB_X86_F32x4: return "xmmword";
        case TB_X86_F64x2: return "xmmword";

        default: return "??";
    }
}

static const char* x86_fmts[] = {
    " + %d",
    " - %d",
    "%"PRId64,

    // displacement
    // " + %#x",
    // " - %#x",
    // immediates
    // "%#"PRIx64,
};

static void print_memory_operand(TB_Disasm* disasm, TB_X86_Inst* restrict inst, TB_X86_DataType dt) {
    uint8_t base = inst->regs & 0xFF;
    uint8_t index = (inst->regs >> 8) & 0xFF;

    if (inst->flags & TB_X86_INSTR_INDIRECT) {
        if ((inst->regs & 0xFFFF) == 0xFFFF) {
            tb_disasm_outf(disasm, "%s [", tb_x86_type_name(dt));
            if (disasm->symbol_handler(disasm, inst->length, inst->disp, inst->disp_pos*8, 32, true)) {
                tb_disasm_outf(disasm, "]");
                return;
            }

            tb_disasm_outf(disasm, "[rip");
        } else {
            tb_disasm_outf(disasm, "%s [", tb_x86_type_name(dt));
            if (base != 0xFF) {
                tb_disasm_outf(disasm, "%s", tb_x86_reg_name(base, TB_X86_QWORD));
            }

            if (index != 0xFF) {
                tb_disasm_outf(disasm, " + %s*%d", tb_x86_reg_name(index, TB_X86_QWORD), 1 << inst->scale);
            }
        }

        if (inst->disp > 0) {
            tb_disasm_outf(disasm, x86_fmts[0], inst->disp);
        } else if (inst->disp < 0) {
            tb_disasm_outf(disasm, x86_fmts[1], -inst->disp);
        }
        tb_disasm_outf(disasm, "]");
    } else if (base != 0xFF) {
        tb_disasm_outf(disasm, "%s", tb_x86_reg_name(base, dt));
    }
}

size_t tb_x86_print_inst(TB_Disasm* disasm, TB_X86_Inst* inst, bool has_relocs) {
    size_t old = disasm->out_curr;
    if (inst->flags & TB_X86_INSTR_REP) {
        tb_disasm_outf(disasm, "rep ");
    }

    if (inst->flags & TB_X86_INSTR_LOCK) {
        tb_disasm_outf(disasm, "lock ");
    }

    const char* mnemonic = tb_x86_mnemonic(inst);
    if (inst->dt >= TB_X86_F32x1 && inst->dt <= TB_X86_F64x2) {
        static const char* strs[] = { "ss", "sd", "ps", "pd" };
        tb_disasm_outf(disasm, "%s", mnemonic);
        tb_disasm_outf(disasm, "%*s", -(8 - strlen(mnemonic)), strs[inst->dt - TB_X86_F32x1]);
    } else {
        tb_disasm_outf(disasm, "%-8s", mnemonic);
    }

    uint8_t rx = (inst->regs >> 16) & 0xFF;
    if (inst->flags & TB_X86_INSTR_DIRECTION) {
        if (rx != 255) {
            tb_disasm_outf(disasm, "%s", tb_x86_reg_name(rx, inst->dt));
            tb_disasm_outf(disasm, ", ");
        }
        print_memory_operand(disasm, inst, inst->dt2);
    } else {
        print_memory_operand(disasm, inst, inst->dt);
        if (rx != 255) {
            tb_disasm_outf(disasm, ", ");
            tb_disasm_outf(disasm, "%s", tb_x86_reg_name(rx, inst->dt2));
        }
    }

    if (inst->flags & TB_X86_INSTR_IMMEDIATE) {
        if (inst->regs != 0xFFFFFF) {
            tb_disasm_outf(disasm, ", ");
        }

        bool is_offset = inst->opcode == 0xE8
            || inst->opcode == 0xE9
            || inst->opcode == 0xEB
            || (inst->opcode >= 0x70 && inst->opcode <= 0x7F)
            || (inst->opcode >= 0x180 && inst->opcode <= 0x18F);

        if (!disasm->symbol_handler(disasm, inst->length, inst->imm, inst->length*8 - 32, 32, is_offset)) {
            tb_disasm_outf(disasm, x86_fmts[2], inst->imm);
        }
    }

    return disasm->out_curr - old;
}

