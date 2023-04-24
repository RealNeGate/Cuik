#include <tb_x64.h>

// this is used to parse ModRM and SIB
#define UNPACK_233(a, b, c, src) \
(a = (src >> 6) & 3, b = (src >> 3) & 7, c = (src & 7))

#define THROW()        return (TB_X86_Inst){ .type = -1, .length = current }
#define THROW_OBB(amt) if (current + (amt) >= length) THROW()

// INSTRUCTION ::= PREFIX* OPCODE[1-4] (MODRM SIB?)? IMMEDIATE? OFFSET?
TB_X86_Inst tb_x86_disasm(size_t length, const uint8_t data[length]) {
    TB_X86_Inst inst = { 69 };
    size_t current = 0;

    ////////////////////////////////
    // Parse prefixes
    ////////////////////////////////
    uint8_t rex = 0;     // 0x4X
    bool addr32 = false; // 0x67
    bool addr16 = false; // 0x66
    bool rep    = false; // 0xF3 these are both used
    bool repne  = false; // 0xF2 to define SSE types

    uint8_t op;
    while (true) {
        THROW_OBB(1);
        op = data[current++];

        if ((op & 0xF0) == 0x40) rex = op;
        else if (op == 0xF0) inst.flags |= TB_X86_INSTR_LOCK;
        else if (op == 0x66) addr16 = true;
        else if (op == 0x67) addr32 = true;
        else if (op == 0xF3) rep = true;
        else if (op == 0xF2) repne = true;
        else if (op == 0x2E) inst.segment = TB_X86_SEGMENT_CS;
        else if (op == 0x36) inst.segment = TB_X86_SEGMENT_SS;
        else if (op == 0x3E) inst.segment = TB_X86_SEGMENT_DS;
        else if (op == 0x26) inst.segment = TB_X86_SEGMENT_ES;
        else if (op == 0x64) inst.segment = TB_X86_SEGMENT_FS;
        else if (op == 0x65) inst.segment = TB_X86_SEGMENT_GS;
        else break;
    }

    ////////////////////////////////
    // Parse opcode
    ////////////////////////////////
    __debugbreak();

    THROW_OBB(1);
    op = data[current++];

    return inst;
}
