
static uint8_t mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
    return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

static uint8_t rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
    return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}

static void emit_memory_operand(TB_CGEmitter* restrict e, uint8_t rx, const Val* restrict a) {
    // Operand encoding
    if (a->type == VAL_GPR || a->type == VAL_XMM) {
        EMIT1(e, mod_rx_rm(MOD_DIRECT, rx, a->gpr));
    } else if (a->type == VAL_MEM) {
        GPR base = a->mem.base;
        GPR index = a->mem.index;
        uint8_t scale = a->mem.scale;
        int32_t disp  = a->mem.disp;

        bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;

        // If it needs an index, it'll put RSP into the base slot
        // and write the real base into the SIB
        uint8_t mod = MOD_INDIRECT_DISP32;
        if (disp == 0 && (base & 7) != RBP) mod = MOD_INDIRECT;
        else if (disp == (int8_t)disp) mod = MOD_INDIRECT_DISP8;

        EMIT1(e, mod_rx_rm(mod, rx, needs_index ? RSP : base));
        if (needs_index) {
            EMIT1(e, mod_rx_rm(scale, (base & 7) == RSP ? RSP : index, base));
        }

        if (mod == MOD_INDIRECT_DISP8) {
            EMIT1(e, (int8_t)disp);
        } else if (mod == MOD_INDIRECT_DISP32) {
            EMIT4(e, disp);
        }
    } else if (a->type == VAL_GLOBAL) {
        EMIT1(e, ((rx & 7) << 3) | RBP);
        EMIT4(e, a->global.disp);

        tb_emit_symbol_patch(e->f->super.module, e->f, (TB_Symbol*) a->global.g, e->count - 4, false, s_local_thread_id);
    } else {
        tb_unreachable();
    }
}

// cannot generate patches with f being NULL
static void inst1(TB_CGEmitter* restrict e, Inst1 op, const Val* r) {
    if (r->type == VAL_GPR) {
        EMIT1(e, rex(true, 0x00, r->gpr, 0x00));
        EMIT1(e, (op >> 8) & 0xFF);
        EMIT1(e, mod_rx_rm(MOD_DIRECT, op & 0xFF, r->gpr));
    } else if (r->type == VAL_MEM) {
        GPR base = r->mem.base;
        GPR index = r->mem.index;
        uint8_t scale = r->mem.scale;
        int32_t disp  = r->mem.disp;

        bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;

        EMIT1(e, rex(true, 0x00, base, index != GPR_NONE ? index : 0));
        EMIT1(e, (op >> 8) & 0xFF);

        // If it needs an index, it'll put RSP into the base slot
        // and write the real base into the SIB
        uint8_t mod = MOD_INDIRECT_DISP32;
        if (disp == 0) mod = MOD_INDIRECT_DISP8;
        else if (disp == (int8_t)disp)
            mod = MOD_INDIRECT_DISP8;

        EMIT1(e, mod_rx_rm(mod, op & 0xFF, needs_index ? RSP : base));
        if (needs_index) {
            EMIT1(e, mod_rx_rm(scale, (base & 7) == RSP ? RSP : index, base));
        }

        if (mod == MOD_INDIRECT_DISP8) EMIT1(e, (int8_t)disp);
        else if (mod == MOD_INDIRECT_DISP32) EMIT4(e, (int32_t)disp);
    } else if (r->type == VAL_GLOBAL) {
        uint8_t rx = (op & 0xFF);

        EMIT1(e, 0x48); // rex.w
        EMIT1(e, (op >> 8) & 0xFF);
        EMIT1(e, ((rx & 7) << 3) | RBP);
        EMIT4(e, r->global.disp);

        tb_emit_symbol_patch(e->f->super.module, e->f, (TB_Symbol*) r->global.g, e->count - 4, false, s_local_thread_id);
    } else {
        tb_unreachable();
    }
}

static void inst2(TB_CGEmitter* restrict e, Inst2Type op, const Val* a, const Val* b, TB_DataType dt) {
    assert(op < (sizeof(inst2_tbl) / sizeof(inst2_tbl[0])));
    assert(dt.type == TB_INT || dt.type == TB_PTR);

    int bits_in_type = dt.type == TB_PTR ? 64 : dt.data;
    assert(bits_in_type == 8 || bits_in_type == 16 || bits_in_type == 32 || bits_in_type == 64);

    const Inst2* inst = &inst2_tbl[op];

    bool dir = b->type == VAL_MEM || b->type == VAL_GLOBAL;
    if (dir || inst->op == 0x63 || inst->op == 0xAF || inst->ext == EXT_DEF2) {
        tb_swap(const Val*, a, b);
    }

    // operand size
    uint8_t sz = (bits_in_type != 8);

    // uses an immediate value that works as
    // a sign extended 8 bit number
    bool short_imm = (bits_in_type != 8 && b->type == VAL_IMM && b->imm == (int8_t)b->imm && inst->op_i == 0x80);

    // All instructions that go through here are
    // based on the ModRxRm encoding so we do need
    // an RX and an RM (base, index, shift, disp)
    uint8_t base = 0;
    uint8_t rx = 0xFF;
    if (inst->ext == EXT_NONE || inst->ext == EXT_DEF || inst->ext == EXT_DEF2) {
        // the destination can only be a GPR, no direction flag
        bool is_gpr_only_dst = (inst->op & 1);
        bool dir_flag = (dir != is_gpr_only_dst);

        // Address size prefix
        if (bits_in_type == 16 && inst->ext != EXT_DEF2) {
            EMIT1(e, 0x66);
        }

        // RX
        if (b->type == VAL_GPR) rx = b->gpr;
        else if (b->type == VAL_IMM) rx = inst->rx_i;
        else tb_unreachable();

        // RM & REX
        bool is_64bit = (bits_in_type == 64);
        if (a->type == VAL_GPR) {
            base = a->gpr;

            if (base >= 8 || rx >= 8 || is_64bit) {
                EMIT1(e, rex(is_64bit, rx, base, 0));
            } else if (bits_in_type == 8 && (base >= 4 || rx >= 4)) {
                EMIT1(e, rex(false, rx, base, 0));
            }
        } else if (a->type == VAL_MEM) {
            base = a->mem.base;

            uint8_t rex_index = (a->mem.index != GPR_NONE ? a->mem.index : 0);
            if (base >= 8 || rx >= 8 || rex_index >= 8 || is_64bit) {
                EMIT1(e, rex(is_64bit, rx, base, rex_index));
            } else if (bits_in_type == 8 && (base >= 4 || rx >= 4 || rex_index >= 4)) {
                EMIT1(e, rex(false, rx, base, 0));
            }
        } else if (a->type == VAL_GLOBAL) {
            base = RBP;
            if (rx >= 8 || is_64bit) {
                EMIT1(e, rex(is_64bit, rx, base, 0));
            } else if (bits_in_type == 8) {
                EMIT1(e, rex(false, rx, base, 0));
            }
        } else tb_unreachable();

        // Opcode
        if (inst->ext == EXT_DEF || inst->ext == EXT_DEF2) {
            // DEF instructions can only be 32bit and 64bit... maybe?
            if (op != XADD) sz = 0;
            EMIT1(e, 0x0F);
        }

        if (b->type == VAL_IMM && inst->op_i == 0 && inst->rx_i == 0) {
            // No immediate version
            tb_unreachable();
        }

        // Immediates have a custom opcode
        uint8_t opcode = b->type == VAL_IMM ? inst->op_i : inst->op;
        if (short_imm) opcode |= 2;

        EMIT1(e, opcode | sz | (dir_flag ? 2 : 0));
    } else tb_unreachable();

    // We forgot a case!
    assert(rx != 0xFF);
    emit_memory_operand(e, rx, a);

    if (b->type == VAL_IMM) {
        if (bits_in_type <= 8 || short_imm) {
            if (a->type == VAL_GLOBAL) {
                RELOC4(e, e->count - 4, -1);
            }

            if (short_imm) {
                assert(b->imm == (int8_t)b->imm);
            }

            EMIT1(e, (int8_t)b->imm);
        } else if (bits_in_type <= 16) {
            if (a->type == VAL_GLOBAL) {
                RELOC4(e, e->count - 4, -2);
            }

            uint32_t imm = b->imm;
            assert((imm & 0xFFFF0000) == 0xFFFF0000 || (imm & 0xFFFF0000) == 0);

            EMIT2(e, imm);
        } else {
            if (a->type == VAL_GLOBAL) {
                RELOC4(e, e->count - 4, -4);
            }

            EMIT4(e, (int32_t)b->imm);
        }
    }
}

static void inst2sse(TB_CGEmitter* restrict e, Inst2FPType op, const Val* a, const Val* b, uint8_t flags) {
    const static uint8_t OPCODES[] = {
        [FP_MOV]   = 0x10,
        [FP_ADD]   = 0x58,
        [FP_MUL]   = 0x59,
        [FP_SUB]   = 0x5C,
        [FP_DIV]   = 0x5E,
        [FP_CMP]   = 0xC2,
        [FP_UCOMI] = 0x2E,
        [FP_CVT]   = 0x5A,
        [FP_SQRT]  = 0x51,
        [FP_RSQRT] = 0x52,
        [FP_AND]   = 0x54,
        [FP_OR]    = 0x56,
        [FP_XOR]   = 0x57
    };

    // most SSE instructions (that aren't mov__) are mem src only
    bool supports_mem_dst = (op == FP_MOV);
    bool dir = is_value_mem(a);

    if (supports_mem_dst && dir) {
        tb_swap(const Val*, a, b);
    }

    uint8_t rx = a->xmm;
    uint8_t base, index;
    if (b->type == VAL_MEM) {
        base  = b->mem.base;
        index = b->mem.index != GPR_NONE ? b->mem.index : 0;
    } else if (b->type == VAL_XMM) {
        base  = b->xmm;
        index = 0;
    } else if (b->type == VAL_GLOBAL) {
        base  = 0;
        index = 0;
    } else {
        tb_todo();
    }

    if ((flags & INST2FP_PACKED) == 0 && op != FP_UCOMI) {
        EMIT1(e, flags & INST2FP_DOUBLE ? 0xF2 : 0xF3);
    } else if (flags & INST2FP_DOUBLE) {
        // packed double
        EMIT1(e, 0x66);
    }

    if (rx >= 8 || base >= 8 || index >= 8) {
        EMIT1(e, rex(false, rx, base, index));
    }

    // extension prefix
    EMIT1(e, 0x0F);

    EMIT1(e, OPCODES[op] + (supports_mem_dst ? dir : 0));
    emit_memory_operand(e, rx, b);
}

static void jcc(TB_CGEmitter* restrict e, Cond cc, int label) {
    e->label_patches[e->label_patch_count++] = (LabelPatch) { .pos = GET_CODE_POS(e) + 2, .target_lbl = label };

    EMIT1(e, 0x0F);
    EMIT1(e, 0x80 + (uint8_t)cc);
    EMIT4(e, 0x0);
}

static void jmp(TB_CGEmitter* restrict e, int label) {
    e->label_patches[e->label_patch_count++] = (LabelPatch) { .pos = GET_CODE_POS(e) + 1, .target_lbl = label };

    EMIT1(e, 0xE9);
    EMIT4(e, 0x0);
}

// jmp .return
static void ret_jmp(TB_CGEmitter* restrict e) {
    e->ret_patches[e->ret_patch_count++] = GET_CODE_POS(e) + 1;

    EMIT1(e, 0xE9);
    EMIT4(e, 0x0);
}
