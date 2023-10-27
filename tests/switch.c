
const char* tb_x86_mnemonic(unsigned int opcode) {
    switch (opcode) {
        case 0x00 ... 0x03: return "add";
        case 0x08 ... 0x0B: return "or";
        case 0x20 ... 0x23: return "and";
        case 0x28 ... 0x2B: return "sub";
        case 0x30 ... 0x33: return "xor";
        case 0x38 ... 0x3B: return "cmp";
        case 0x88 ... 0x8B: return "mov";

        case 0xA4: case 0xA5: return "movs";
        case 0xAA: case 0xAB: return "stos";
        case 0xAE: case 0xAF: return "scas";

        case 0xC04: case 0xC14: return "shl";
        case 0xC05: case 0xC15: return "shr";
        case 0xC07: case 0xC17: return "sar";

        case 0x830: return "add";
        case 0x831: return "or";
        case 0x834: return "and";
        case 0x835: return "sub";
        case 0x836: return "xor";
        case 0x837: return "cmp";
        case 0xC70: return "mov";

        case 0xB8 ... 0xBF: return "mov";

        case 0x8D: return "lea";
        case 0x90: return "nop";
        case 0xC3: return "ret";
        case 0x63: return "movsxd";
        case 0x50: return "push";
        case 0x58: return "pop";

        case 0xE8: return "call";
        case 0xE9: return "jmp";

        case 0x0F1F: return "nop";
        case 0x0FAF: case 0x68: case 0x69: return "imul";

        case 0x0F80: case 0x70: return "jo";
        case 0x0F81: case 0x71: return "jno";
        case 0x0F82: case 0x72: return "jb";
        case 0x0F83: case 0x73: return "jnb";
        case 0x0F84: case 0x74: return "je";
        case 0x0F85: case 0x75: return "jne";
        case 0x0F86: case 0x76: return "jbe";
        case 0x0F87: case 0x77: return "ja";
        case 0x0F88: case 0x78: return "js";
        case 0x0F89: case 0x79: return "jns";
        case 0x0F8A: case 0x7A: return "jp";
        case 0x0F8B: case 0x7B: return "jnp";
        case 0x0F8C: case 0x7C: return "jl";
        case 0x0F8D: case 0x7D: return "jge";
        case 0x0F8E: case 0x7E: return "jle";
        case 0x0F8F: case 0x7F: return "jg";

        default:   return "???";
    }
}
