#include <tb_x64.h>

static void print_memory_operand(TB_X86_Inst* restrict inst) {
    uint8_t base = inst->regs & 0xFF;
    uint8_t index = (inst->regs >> 8) & 0xFF;

    if (inst->flags & TB_X86_INSTR_INDIRECT) {
        if ((inst->regs & 0xFFFF) == 0xFFFF) {
            printf("[rip");
        } else {
            printf("%s [", tb_x86_type_name(inst->dt));
            if (base != 0xFF) {
                printf("%s", tb_x86_reg_name(base, TB_X86_TYPE_QWORD));
            }

            if (index != 0xFF) {
                printf(" + %s*%d", tb_x86_reg_name(index, TB_X86_TYPE_QWORD), 1 << inst->scale);
            }
        }

        if (inst->disp > 0) {
            printf(" + %x", inst->disp);
        } else if (inst->disp < 0) {
            printf(" - %x", -inst->disp);
        }
        printf("]");
    } else if (base != 0xFF) {
        printf("%s", tb_x86_reg_name(base, inst->dt));
    }
}

ptrdiff_t tb_print_disassembly_inst(TB_Arch arch, size_t length, const void* ptr) {
    switch (arch) {
        #ifdef TB_HAS_X64
        case TB_ARCH_X86_64: {
            TB_X86_Inst inst;
            if (!tb_x86_disasm(&inst, length, ptr)) {
                return -1;
            }

            const char* mnemonic = tb_x86_mnemonic(&inst);
            printf("%s", mnemonic);
            if (inst.dt >= TB_X86_TYPE_SSE_SS && inst.dt <= TB_X86_TYPE_SSE_PD) {
                static const char* strs[] = { "ss", "sd", "ps", "pd" };
                printf("%s", strs[inst.dt - TB_X86_TYPE_SSE_SS]);
            }
            printf(" ");

            uint8_t rx = (inst.regs >> 16) & 0xFF;
            if (inst.flags & TB_X86_INSTR_DIRECTION) {
                print_memory_operand(&inst);
                if (rx != 255) {
                    printf(", ");
                    printf("%s", tb_x86_reg_name(rx, inst.dt));
                }
            } else {
                if (rx != 255) {
                    printf("%s", tb_x86_reg_name(rx, inst.dt));
                    printf(", ");
                }
                print_memory_operand(&inst);
            }

            if (inst.flags & TB_X86_INSTR_IMMEDIATE) {
                printf(", ");
                printf("%#"PRIx64, inst.imm);
            }

            printf("\n");
            return inst.length;
        }
        #endif

        default:
        tb_todo();
    }
}

