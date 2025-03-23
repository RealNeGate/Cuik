#include <tb_x64.h>

size_t tb_x86_print_inst(TB_Disasm* disasm, TB_X86_Inst* inst, bool has_relocs);

bool tb_disasm_outf(TB_Disasm* disasm, const char* fmt, ...) {
    size_t cap = disasm->out_len - disasm->out_curr;

    va_list ap;
    va_start(ap, fmt);
    int len = vsnprintf(&disasm->out[disasm->out_curr], cap, fmt, ap);
    va_end(ap);

    if (len < 0 || len > cap) {
        return false;
    } else {
        disasm->out_curr += len;
        return true;
    }
}

ptrdiff_t tb_disasm_print(TB_Arch arch, TB_Disasm* disasm, bool has_relocs) {
    switch (arch) {
        #ifdef TB_HAS_X64
        case TB_ARCH_X86_64: {
            TB_X86_Inst inst;
            if (!tb_x86_disasm(&inst, disasm->in_len - disasm->in_curr, &disasm->in[disasm->in_curr])) {
                return -1;
            }

            size_t s = tb_x86_print_inst(disasm, &inst, has_relocs);
            TB_ASSERT(s != 0);

            disasm->in_curr += inst.length;
            return inst.length;
        }
        #endif

        default:
        tb_todo();
    }
}


