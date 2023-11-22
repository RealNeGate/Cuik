#include "x64.h"
#include <tb_x64.h>
#include "x64_emitter.h"
#include "x64_disasm.c"

enum {
    // register classes
    REG_CLASS_GPR,
    REG_CLASS_XMM,
    REG_CLASS_COUNT,
};

#include "../codegen_impl.h"

static const struct ParamDesc {
    int chkstk_limit;
    int gpr_count;
    int xmm_count;
    uint16_t caller_saved_xmms; // XMM0 - XMMwhatever
    uint16_t caller_saved_gprs; // bitfield

    GPR gprs[6];
} param_descs[] = {
    // win64
    { 4096,    4, 4, 6, WIN64_ABI_CALLER_SAVED,   { RCX, RDX, R8,  R9,  0,  0 } },
    // system v
    { INT_MAX, 6, 4, 5, SYSV_ABI_CALLER_SAVED,    { RDI, RSI, RDX, RCX, R8, R9 } },
    // syscall
    { INT_MAX, 6, 4, 5, SYSCALL_ABI_CALLER_SAVED, { RDI, RSI, RDX, R10, R8, R9 } },
};

enum {
    ALL_GPRS = 0xFFFF & ~((1 << RBP) | (1 << RSP)),
};

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->sched = greedy_scheduler;
    ctx->regalloc = tb__lsra;

    ctx->abi_index = abi == TB_ABI_SYSTEMV ? 1 : 0;

    // currently only using 16 GPRs and 16 XMMs, AVX gives us
    // 32 YMMs (which double as XMMs) and later on APX will do
    // 32 GPRs.
    ctx->num_regs[0] = 16;
    ctx->num_regs[1] = 16;
}

static void isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n) {
    switch (n->type) {
        // no fancy tiling, will just be emitted later into a register
        case TB_START:
        case TB_END:
        case TB_CALL:
        case TB_SYMBOL:
        case TB_PROJ:
        case TB_LOAD:
        break;

        default: tb_todo();
    }
}

static RegMask out_reg_mask(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        case TB_PROJ: {
            int i = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            if (n->inputs[0]->type == TB_START) {
                // function params are ABI crap
                const struct ParamDesc* params = &param_descs[ctx->abi_index];
                return REGMASK(GPR, i >= 3 ? (1u << params->gprs[i - 3]) : 0);
            } else if (n->inputs[0]->type == TB_CALL) {
                if (i == 2) return REGMASK(GPR, 1 << RAX);
                else if (i == 3) return REGMASK(GPR, 1 << RDX);
                else return REGEMPTY;
            } else {
                tb_todo();
            }
            break;
        }

        case TB_START:
        case TB_END:
        case TB_CALL:
        return REGEMPTY;

        case TB_SYMBOL:
        case TB_LOAD:
        return REGMASK(GPR, ALL_GPRS);

        default: tb_todo();
    }
}

static RegMask in_reg_mask(Ctx* restrict ctx, Tile* tile, TB_Node* n, int i) {
    switch (n->type) {
        case TB_END: {
            if (i == 3) return REGMASK(GPR, 1 << RAX);
            else if (i == 4) return REGMASK(GPR, 1 << RDX);
            else return REGEMPTY;
        }

        case TB_CALL: {
            if (i >= 3) {
                // function parameters
                const struct ParamDesc* params = &param_descs[ctx->abi_index];
                return REGMASK(GPR, 1u << params->gprs[i - 3]);
            } else {
                return REGEMPTY;
            }
        }

        default: tb_todo();
    }
}

static void emit_epilogue(Ctx* restrict ctx, TB_CGEmitter* e, int stack_usage) {
    // add rsp, N
    if (stack_usage > 0) {
        if (stack_usage == (int8_t)stack_usage) {
            EMIT1(&ctx->emit, rex(true, 0x00, RSP, 0));
            EMIT1(&ctx->emit, 0x83);
            EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x00, RSP));
            EMIT1(&ctx->emit, (int8_t) stack_usage);
        } else {
            EMIT1(&ctx->emit, rex(true, 0x00, RSP, 0));
            EMIT1(&ctx->emit, 0x81);
            EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x00, RSP));
            EMIT4(&ctx->emit, stack_usage);
        }
    }

    // pop rbp
    if (stack_usage > 0) {
        EMIT1(&ctx->emit, 0x58 + RBP);
    }
}

static Val val_at(Tile* t) {
    return val_gpr(t->assigned);
}

static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    if (t->tag == TILE_NORMAL || t->tag == TILE_GOTO) {
        TB_Node* n = t->n;
        switch (n->type) {
            // prologue
            case TB_START: {
                int stack_usage = ctx->stack_usage;

                // push rbp
                if (stack_usage > 0) {
                    EMIT1(e, 0x50 + RBP);

                    // mov rbp, rsp
                    EMIT1(e, rex(true, RSP, RBP, 0));
                    EMIT1(e, 0x89);
                    EMIT1(e, mod_rx_rm(MOD_DIRECT, RSP, RBP));
                }

                // inserts a chkstk call if we use too much stack
                if (stack_usage >= param_descs[ctx->abi_index].chkstk_limit) {
                    assert(ctx->f->super.module->chkstk_extern);
                    Val sym = val_global(ctx->f->super.module->chkstk_extern, 0);
                    Val imm = val_imm(stack_usage);
                    Val rax = val_gpr(RAX);
                    Val rsp = val_gpr(RSP);

                    inst2(e, MOV, &rax, &imm, TB_X86_TYPE_DWORD);
                    inst1(e, CALL, &sym, TB_X86_TYPE_QWORD);
                    inst2(e, SUB, &rsp, &rax, TB_X86_TYPE_QWORD);
                } else if (stack_usage > 0) {
                    if (stack_usage == (int8_t)stack_usage) {
                        // sub rsp, stack_usage
                        EMIT1(e, rex(true, 0x00, RSP, 0));
                        EMIT1(e, 0x83);
                        EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x05, RSP));
                        EMIT1(e, stack_usage);
                    } else {
                        // sub rsp, stack_usage
                        EMIT1(e, rex(true, 0x00, RSP, 0));
                        EMIT1(e, 0x81);
                        EMIT1(e, mod_rx_rm(MOD_DIRECT, 0x05, RSP));
                        EMIT4(e, stack_usage);
                    }
                }
                ctx->prologue_length = ctx->emit.count;
                break;
            }
            // epilogue
            case TB_END: {
                emit_epilogue(ctx, e, ctx->stack_usage);
                EMIT1(&ctx->emit, 0xC3);
                break;
            }
            // projections don't manage their own work, that's the
            // TUPLE node's job.
            case TB_PROJ: break;

            case TB_SYMBOL: {
                TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;

                Val dst = val_at(t);
                Val src = val_global(sym, 0);
                inst2(e, LEA, &dst, &src, TB_X86_TYPE_QWORD);
                break;
            }

            default: tb_todo();
        }

        // if we were a TILE_GOTO this is the point where we jump
        if (t->tag == TILE_GOTO) {
            __debugbreak();
        }
    } else {
        tb_todo();
    }
}

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    if (bb >= 0) {
        E(".bb%d:\n", bb);
    }

    while (pos < end) {
        while (d->loc != d->end && d->loc->pos == pos) {
            E("  // %s : line %d\n", d->loc->file->path, d->loc->line);
            d->loc++;
        }

        TB_X86_Inst inst;
        if (!tb_x86_disasm(&inst, end - pos, &e->data[pos])) {
            E("  ERROR\n");
            pos += 1; // skip ahead once... cry
            continue;
        }

        const char* mnemonic = tb_x86_mnemonic(&inst);
        E("  ");
        if (inst.flags & TB_X86_INSTR_REP) {
            E("rep ");
        }
        if (inst.flags & TB_X86_INSTR_LOCK) {
            E("lock ");
        }
        E("%s", mnemonic);
        if (inst.data_type >= TB_X86_TYPE_SSE_SS && inst.data_type <= TB_X86_TYPE_SSE_PD) {
            static const char* strs[] = { "ss", "sd", "ps", "pd" };
            E(strs[inst.data_type - TB_X86_TYPE_SSE_SS]);
        }
        E(" ");

        bool mem = true, imm = true;
        for (int i = 0; i < 4; i++) {
            if (inst.regs[i] == -1) {
                if (mem && (inst.flags & TB_X86_INSTR_USE_MEMOP)) {
                    if (i > 0) E(", ");

                    mem = false;

                    if (inst.flags & TB_X86_INSTR_USE_RIPMEM) {
                        bool is_label = inst.opcode == 0xE8 || inst.opcode == 0xE9
                            || (inst.opcode >= 0x70   && inst.opcode <= 0x7F)
                            || (inst.opcode >= 0x0F80 && inst.opcode <= 0x0F8F);

                        if (!is_label) E("[");

                        if (d->patch && d->patch->pos == pos + inst.length - 4) {
                            const TB_Symbol* target = d->patch->target;

                            if (target->name[0] == 0) {
                                E("sym%p", target);
                            } else {
                                E("%s", target->name);
                            }
                            d->patch = d->patch->next;
                        } else {
                            uint32_t target = pos + inst.length + inst.disp;
                            int bb = tb_emit_get_label(e, target);
                            uint32_t landed = e->labels[bb] & 0x7FFFFFFF;

                            if (landed != target) {
                                E(".bb%d + %d", bb, (int)target - (int)landed);
                            } else {
                                E(".bb%d", bb);
                            }
                        }

                        if (!is_label) E("]");
                    } else {
                        E("%s [", tb_x86_type_name(inst.data_type));
                        if (inst.base != 255) {
                            E("%s", tb_x86_reg_name(inst.base, TB_X86_TYPE_QWORD));
                        }

                        if (inst.index != 255) {
                            E(" + %s*%d", tb_x86_reg_name(inst.index, TB_X86_TYPE_QWORD), 1 << inst.scale);
                        }

                        if (inst.disp > 0) {
                            E(" + %d", inst.disp);
                        } else if (inst.disp < 0) {
                            E(" - %d", -inst.disp);
                        }

                        E("]");
                    }
                } else if (imm && (inst.flags & (TB_X86_INSTR_IMMEDIATE | TB_X86_INSTR_ABSOLUTE))) {
                    if (i > 0) E(", ");

                    imm = false;
                    if (inst.flags & TB_X86_INSTR_ABSOLUTE) {
                        E("%#llx", inst.abs);
                    } else {
                        E("%#x", inst.imm);
                    }
                } else {
                    break;
                }
            } else {
                if (i > 0) {
                    E(", ");

                    // special case for certain ops with two data types
                    if (inst.flags & TB_X86_INSTR_TWO_DATA_TYPES) {
                        E("%s", tb_x86_reg_name(inst.regs[i], inst.data_type2));
                        continue;
                    }
                }

                E("%s", tb_x86_reg_name(inst.regs[i], inst.data_type));
            }
        }
        E("\n");

        pos += inst.length;
    }
}
#undef E

ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = NULL,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
