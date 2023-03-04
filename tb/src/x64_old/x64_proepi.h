#include "../objects/win64eh.h"

void x64_emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t saved, uint64_t stack_usage) {
    size_t patch_pos = e->count;
    UnwindInfo unwind = {
        .version = 1,
        .flags = UNWIND_FLAG_EHANDLER,
        .prolog_length = out_f->prologue_length,
        .code_count = 0,
        .frame_register = RBP,
        .frame_offset = 0,
    };
    tb_outs(e, sizeof(UnwindInfo), &unwind);

    size_t code_count = 0;
    if (stack_usage == 8) {
        // no real prologue
    } else {
        UnwindCode codes[] = {
            // sub rsp, stack_usage
            { .code_offset = 8, .unwind_op = UNWIND_OP_ALLOC_SMALL, .op_info = (stack_usage / 8) - 1 },
            // mov rbp, rsp
            { .code_offset = 4, .unwind_op = UNWIND_OP_SET_FPREG, .op_info = 0 },
            // push rbp
            { .code_offset = 1, .unwind_op = UNWIND_OP_PUSH_NONVOL, .op_info = RBP },
        };
        tb_outs(e, sizeof(codes), codes);
        code_count += 3;
    }

    tb_patch1b(e, patch_pos + offsetof(UnwindInfo, code_count), code_count);
}

size_t x64_emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    // align the stack correctly
    if ((tb_popcount(saved & 0xFFFF) & 1) == 0) stack_usage += 8;
    // If the stack usage is zero we don't need a prologue
    if (stack_usage == 8) return 0;

    size_t used = 0;

    // push rbp
    out[used++] = 0x50 + RBP;

    // mov rbp, rsp
    out[used++] = rex(true, RSP, RBP, 0);
    out[used++] = 0x89;
    out[used++] = mod_rx_rm(MOD_DIRECT, RSP, RBP);

    // push rXX
    for (size_t i = 0; i < 16; i++)
        if (saved & (1ull << i)) {
        if (i < 8) {
            out[used++] = 0x50 + i;
        } else {
            out[used++] = 0x41;
            out[used++] = 0x50 + (i & 0b111);
        }
    }

    if (stack_usage == (int8_t)stack_usage) {
        // sub rsp, stack_usage
        out[used++] = rex(true, 0x00, RSP, 0);
        out[used++] = 0x83;
        out[used++] = mod_rx_rm(MOD_DIRECT, 0x05, RSP);
        out[used++] = stack_usage;
    } else {
        // sub rsp, stack_usage
        out[used++] = rex(true, 0x00, RSP, 0);
        out[used++] = 0x81;
        out[used++] = mod_rx_rm(MOD_DIRECT, 0x05, RSP);
        *((uint32_t*)&out[used]) = stack_usage;
        used += 4;
    }

    // save XMMs
    int tally = stack_usage & ~15u;
    for (size_t i = 0; i < 16; i++) {
        if (saved & (1ull << (i + 16))) {
            if (i >= 8) {
                out[used++] = rex(false, i, 0, 0);
            }

            // movaps [rbp - (A * 16)], xmmI
            out[used++] = 0x0F;
            out[used++] = 0x29;
            out[used++] = mod_rx_rm(MOD_INDIRECT_DISP32, i, RBP);

            *((uint32_t*)&out[used]) = -tally;
            used += 4;

            tally -= 16;
        }
    }

    return used;
}

size_t x64_emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    // align the stack correctly
    if ((tb_popcount(saved & 0xFFFF) & 1) == 0) stack_usage += 8;

    // if the stack isn't used then just return
    if (stack_usage == 8) {
        out[0] = 0xC3;
        return 1;
    }

    size_t used = 0;

    // reload XMMs
    int tally = stack_usage & ~15u;
    for (size_t i = 0; i < 16; i++) {
        if (saved & (1ull << (i + 16))) {
            if (i >= 8) { out[used++] = rex(false, i, 0, 0); }

            // movaps xmmI, [rsp + (A * 16)]
            out[used++] = 0x0F;
            out[used++] = 0x28;
            out[used++] = mod_rx_rm(MOD_INDIRECT_DISP32, i, RBP);

            *((uint32_t*)&out[used]) = -tally;
            used += 4;

            tally -= 16;
        }
    }

    // add rsp, N
    if (stack_usage == (int8_t)stack_usage) {
        out[used++] = rex(true, 0x00, RSP, 0);
        out[used++] = 0x83;
        out[used++] = mod_rx_rm(MOD_DIRECT, 0x00, RSP);
        out[used++] = (int8_t)stack_usage;
    } else {
        out[used++] = rex(true, 0x00, RSP, 0);
        out[used++] = 0x81;
        out[used++] = mod_rx_rm(MOD_DIRECT, 0x00, RSP);
        *((uint32_t*)&out[used]) = stack_usage;
        used += 4;
    }

    // pop gpr
    for (size_t i = 16; i--;)
        if (saved & (1ull << i)) {
        if (i < 8) {
            out[used++] = 0x58 + i;
        } else {
            out[used++] = 0x41;
            out[used++] = 0x58 + (i & 0b111);
        }
    }

    out[used++] = 0x58 + RBP;
    out[used++] = 0xC3;
    return used;
}
