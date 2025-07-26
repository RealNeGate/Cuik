#include <common.h> // __debugbreak
#include <tb_a64.h>

// #include "../../meta/a64dfa.h"

bool tb_a64_disasm(TB_A64_Inst* restrict inst, size_t length, const uint8_t* data) {
    uint32_t bits;
    memcpy(&bits, data, 4);

    /* __debugbreak();
    uint16_t state = walk(bits);
    if (state >= COUNTOF(names) || names[state] == NULL) {
        printf("???");
        __debugbreak();
    }

    names[state](bits); */
    return false;
}

size_t tb_a64_print_inst(TB_Disasm* disasm, TB_A64_Inst* inst, bool has_relocs) {
    __debugbreak();
    return 0;
}



