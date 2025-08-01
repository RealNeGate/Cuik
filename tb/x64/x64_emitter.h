
static uint8_t mod_rx_rm(uint8_t mod, uint8_t rx, uint8_t rm) {
    return ((mod & 3) << 6) | ((rx & 7) << 3) | (rm & 7);
}

static uint8_t rex(bool is_64bit, uint8_t rx, uint8_t base, uint8_t index) {
    return 0x40 | (is_64bit ? 8 : 0) | (base >> 3) | ((index >> 3) << 1) | ((rx >> 3) << 2);
}

// emits 0F if (inst->cat == a) is true
#define EXT_OP(a) ((inst->cat == a) ? EMIT1(e, 0x0F) : 0)

static void jmp(TB_CGEmitter* restrict e, int label) {
    EMIT1(e, 0xE9); EMIT4(e, 0);
    tb_emit_rel32(e, &e->labels[label], GET_CODE_POS(e) - 4, 0xFFFFFFFF, 0);
}

static void jcc(TB_CGEmitter* restrict e, Cond cc, int label) {
    EMIT1(e, 0x0F); EMIT1(e, 0x80 + cc); EMIT4(e, 0);
    tb_emit_rel32(e, &e->labels[label], GET_CODE_POS(e) - 4, 0xFFFFFFFF, 0);
}

static void emit_memory_operand(TB_CGEmitter* restrict e, uint8_t rx, const Val* a) {
    // Operand encoding
    if (a->type == VAL_GPR || a->type == VAL_XMM) {
        EMIT1(e, mod_rx_rm(MOD_DIRECT, rx, a->reg));
    } else if (a->type == VAL_MEM) {
        GPR base = a->reg, index = a->index;
        Scale scale = a->scale;
        int32_t disp  = a->imm;

        bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;

        // If it needs an index, it'll put RSP into the base slot
        // and write the real base into the SIB
        uint8_t mod = MOD_INDIRECT_DISP32;
        if (disp == 0 && (base & 7) != RBP) mod = MOD_INDIRECT;
        else if (disp == (int8_t)disp) mod = MOD_INDIRECT_DISP8;

        EMIT1(e, mod_rx_rm(mod, rx, needs_index ? RSP : base));
        if (needs_index) {
            EMIT1(e, mod_rx_rm(scale, index >= 0 ? index : RSP, base));
        }

        if (mod == MOD_INDIRECT_DISP8) {
            EMIT1(e, (int8_t)disp);
        } else if (mod == MOD_INDIRECT_DISP32) {
            EMIT4(e, disp);
        }
    } else if (a->type == VAL_GLOBAL) {
        EMIT1(e, ((rx & 7) << 3) | RBP);
        EMIT4(e, a->imm);

        tb_emit_symbol_patch(e->output, a->symbol, e->count - 4, TB_OBJECT_RELOC_REL32);
    } else {
        tb_unreachable();
    }
}

static void inst2(TB_CGEmitter* restrict e, InstType type, const Val* a, const Val* b, TB_X86_DataType dt) {
    assert(dt >= TB_X86_BYTE && dt <= TB_X86_QWORD);
    assert(type < COUNTOF(inst_table));

    const InstDesc* restrict inst = &inst_table[type];
    bool dir = b->type == VAL_MEM || b->type == VAL_GLOBAL;
    if (dir || inst->op == 0x63 || inst->op == 0x69 || inst->op == 0x6E || (type >= CMOVO && type <= CMOVG) || inst->op == 0xAF || inst->cat == INST_BINOP_EXT2) {
        SWAP(const Val*, a, b);
    }

    // operand size
    bool sz = (dt != TB_X86_BYTE);

    // uses an immediate value that works as
    // a sign extended 8 bit number
    bool short_imm = (sz && b->type == VAL_IMM && b->imm == (int8_t)b->imm && inst->op_i == 0x80);

    // the destination can only be a GPR, no direction flag
    bool is_gpr_only_dst = (inst->op & 1);
    bool dir_flag = (dir != is_gpr_only_dst) && inst->op != 0x69 && inst->op != 0x84;
    if (type >= CMOVO && type <= CMOVG) {
        dir_flag = false;
    }

    if (inst->cat != INST_BINOP_EXT3) {
        // Address size prefix
        if (dt == TB_X86_WORD && inst->cat != INST_BINOP_EXT2) {
            EMIT1(e, 0x66);
        }

        assert((b->type == VAL_GPR || b->type == VAL_IMM) && "secondary operand is invalid!");
    } else {
        // movd/movq add the ADDR16 prefix for reasons?
        EMIT1(e, 0x66);
    }

    // REX PREFIX
    //  0 1 0 0 W R X B
    //          ^ ^ ^ ^
    //          | | | 4th bit on base.
    //          | | 4th bit on index.
    //          | 4th bit on rx.
    //          is 64bit?
    uint8_t base = 0;
    uint8_t rex_prefix = 0x40 | (dt == TB_X86_QWORD ? 8 : 0);
    if (a->type == VAL_MEM || a->type == VAL_GPR) {
        base = a->reg;
    } else {
        base = RBP;
    }

    if (a->type == VAL_MEM && a->index != GPR_NONE) {
        rex_prefix |= ((a->index >> 3) << 1);
    }

    uint8_t rx = (b->type == VAL_GPR || b->type == VAL_XMM) ? b->reg : inst->rx_i;
    if (inst->cat == INST_BINOP_CL) {
        assert(b->type == VAL_IMM || (b->type == VAL_GPR && b->reg == RCX));

        dt = TB_X86_BYTE;
        rx = inst->rx_i;
    }

    rex_prefix |= (base >> 3);
    rex_prefix |= (rx >> 3) << 2;

    // if the REX stays as 0x40 then it's default and doesn't need
    // to be here.
    if (rex_prefix != 0x40 || dt == TB_X86_BYTE || type == MOVZXB) {
        EMIT1(e, rex_prefix);
    }

    if (inst->cat == INST_BINOP_EXT3) {
        EMIT1(e, 0x0F);
        EMIT1(e, inst->op);
    } else {
        // Opcode
        if (inst->cat == INST_BINOP_EXT || inst->cat == INST_BINOP_EXT2) {
            // DEF instructions can only be 32bit and 64bit... maybe?
            if (type != XADD) sz = 0;
            EMIT1(e, 0x0F);
        }

        // Immediates have a custom opcode
        TB_ASSERT((b->type != VAL_IMM || inst->op_i != 0 || inst->rx_i != 0) && "no immediate variant of instruction");
        uint8_t opcode = b->type == VAL_IMM ? inst->op_i : inst->op;

        // bottom bit usually means size, 0 for 8bit, 1 for everything else.
        opcode |= sz;

        // you can't actually be flipped in the immediates because it would mean
        // you're storing into an immediate so they reuse that direction bit for size.
        opcode |= dir_flag << 1;
        opcode |= short_imm << 1;
        EMIT1(e, opcode);
    }

    emit_memory_operand(e, rx, a);

    // BTW memory displacements go before immediates
    ptrdiff_t disp_patch = e->count;
    if (b->type == VAL_IMM) {
        if (dt == TB_X86_BYTE || short_imm) {
            if (short_imm) {
                assert(b->imm == (int8_t)b->imm);
            }

            EMIT1(e, (int8_t)b->imm);
        } else if (dt == TB_X86_WORD) {
            uint32_t imm = b->imm;
            assert((imm & 0xFFFF0000) == 0xFFFF0000 || (imm & 0xFFFF0000) == 0);
            EMIT2(e, imm);
        } else {
            EMIT4(e, (int32_t)b->imm);
        }
    }

    if (a->type == VAL_GLOBAL && disp_patch != e->count) {
        RELOC4(e, disp_patch - 4, disp_patch - e->count, 0xFFFFFFFF);
    }
}

static void inst2sse(TB_CGEmitter* restrict e, InstType type, const Val* a, const Val* b, TB_X86_DataType dt) {
    TB_ASSERT(type < COUNTOF(inst_table));
    const InstDesc* restrict inst = &inst_table[type];

    // most SSE instructions (that aren't mov__) are mem src only
    bool supports_mem_dst = (type == FP_MOV);
    bool dir = is_value_mem(a);

    bool packed    = (dt == TB_X86_F32x4 || dt == TB_X86_F64x2);
    bool is_double = (dt == TB_X86_F64x2 || dt == TB_X86_F64x1);

    if (type == MOV_I2F || type == MOV_F2I || (supports_mem_dst && dir)) {
        SWAP(const Val*, a, b);
    }

    uint8_t rx = a->reg;
    uint8_t base, index;
    if (b->type == VAL_MEM) {
        base  = b->reg;
        index = b->index != GPR_NONE ? b->index : 0;
    } else if (b->type == VAL_XMM || b->type == VAL_GPR) {
        base  = b->reg;
        index = 0;
    } else if (b->type == VAL_GLOBAL) {
        base  = 0;
        index = 0;
    } else {
        tb_todo();
    }

    bool r = (type == FP_CVT64);
    if (type == MOV_I2F || type == MOV_F2I) {
        EMIT1(e, 0x66);
        r = dt == TB_X86_F64x1;
    } else if (type != FP_XOR && type != FP_AND && type != FP_OR) {
        if (!packed && type != FP_UCOMI) {
            EMIT1(e, is_double ? 0xF2 : 0xF3);
        } else if (is_double) {
            // packed double
            EMIT1(e, 0x66);
        }
    }

    if (r || rx >= 8 || base >= 8 || index >= 8) {
        EMIT1(e, rex(r, rx, base, index));
    }

    // extension prefix
    EMIT1(e, 0x0F);
    EMIT1(e, inst->op + (supports_mem_dst ? dir : 0));
    emit_memory_operand(e, rx, b);
}

static void inst2sseint(TB_CGEmitter* restrict e, InstType type, const Val* a, const Val* b, TB_X86_DataType dt) {
    TB_ASSERT(type < COUNTOF(inst_table));
    const InstDesc* restrict inst = &inst_table[type];

    bool supports_mem_dst = (type == MOVDQU || type == MOVDQA);
    bool dir = is_value_mem(a);

    bool packed    = (dt == TB_X86_F32x4 || dt == TB_X86_F64x2);
    bool is_double = (dt == TB_X86_F64x2 || dt == TB_X86_F64x1);

    TB_ASSERT(!dir || supports_mem_dst);
    if (supports_mem_dst && dir) {
        SWAP(const Val*, a, b);
    }

    uint8_t rx = a->reg;
    uint8_t base, index;
    if (b->type == VAL_MEM) {
        base  = b->reg;
        index = b->index != GPR_NONE ? b->index : 0;
    } else if (b->type == VAL_XMM || b->type == VAL_GPR) {
        base  = b->reg;
        index = 0;
    } else if (b->type == VAL_GLOBAL) {
        base  = 0;
        index = 0;
    } else {
        tb_todo();
    }

    if (type == MOVDQA) {
        EMIT1(e, 0x66);
    } else if (type == MOVDQU) {
        EMIT1(e, 0xF3);
    }

    if (rx >= 8 || base >= 8 || index >= 8) {
        EMIT1(e, rex(false, rx, base, index));
    }

    // extension prefix
    EMIT1(e, 0x0F);
    EMIT1(e, inst->op + (dir ? 0x10 : 0x00));
    emit_memory_operand(e, rx, b);
}

static void asm_inst0(TB_CGEmitter* restrict e, InstType type, TB_X86_DataType dt) {
    TB_ASSERT(type < COUNTOF(inst_table));
    const InstDesc* restrict inst = &inst_table[type];

    if (dt == TB_X86_QWORD) {
        EMIT1(e, 0x48);
    }
    EXT_OP(INST_BYTE_EXT);

    if (inst->op) {
        EMIT1(e, inst->op);
    } else {
        EMIT1(e, inst->op_i);
        EMIT1(e, inst->rx_i);
    }
}

static void asm_inst1(TB_CGEmitter* e, int type, TB_X86_DataType dt, const Val* r) {
    TB_ASSERT(type < COUNTOF(inst_table));
    const InstDesc* restrict inst = &inst_table[type];

    bool is_rex = (dt == TB_X86_BYTE || dt == TB_X86_QWORD);
    bool is_rexw = dt == TB_X86_QWORD;

    uint8_t op = inst->op_i, rx = inst->rx_i;
    if (r->type == VAL_GPR) {
        if (is_rex || r->reg >= 8) {
            EMIT1(e, rex(is_rexw, 0x00, r->reg, 0x00));
        }
        EXT_OP(INST_UNARY_EXT);
        EMIT1(e, op ? op : inst->op);
        EMIT1(e, mod_rx_rm(MOD_DIRECT, rx, r->reg));
    } else if (r->type == VAL_MEM) {
        GPR base = r->reg, index = r->index;
        Scale scale = r->scale;
        int32_t disp  = r->imm;

        bool needs_index = (index != GPR_NONE) || (base & 7) == RSP;

        EMIT1(e, rex(is_rexw, 0x00, base, index != GPR_NONE ? index : 0));
        EXT_OP(INST_UNARY_EXT);
        EMIT1(e, op);

        // If it needs an index, it'll put RSP into the base slot
        // and write the real base into the SIB
        uint8_t mod = MOD_INDIRECT_DISP32;
        if (disp == 0) {
            mod = MOD_INDIRECT_DISP8;
        } else if (disp == (int8_t)disp) {
            mod = MOD_INDIRECT_DISP8;
        }

        EMIT1(e, mod_rx_rm(mod, rx, needs_index ? RSP : base));
        if (needs_index) {
            EMIT1(e, mod_rx_rm(scale, (base & 7) == RSP ? RSP : index, base));
        }

        if (mod == MOD_INDIRECT_DISP8) EMIT1(e, (int8_t)disp);
        else if (mod == MOD_INDIRECT_DISP32) EMIT4(e, (int32_t)disp);
    } else if (r->type == VAL_GLOBAL) {
        if (inst->op) {
            // this is a unary instruction with a REL32 variant
            EXT_OP(INST_UNARY_EXT);
            EMIT1(e, inst->op);
        } else {
            if (is_rex) EMIT1(e, is_rexw ? 0x48 : 0x40);
            EXT_OP(INST_UNARY_EXT);
            EMIT1(e, op);
            EMIT1(e, ((rx & 7) << 3) | RBP);
        }

        EMIT4(e, r->imm);
        tb_emit_symbol_patch(e->output, r->symbol, e->count - 4, TB_OBJECT_RELOC_REL32);
    } else if (r->type == VAL_LABEL) {
        EXT_OP(INST_UNARY_EXT);
        EMIT1(e, inst->op);
        EMIT4(e, 0);

        assert(r->label >= 0 && r->label < e->label_count);
        tb_emit_rel32(e, &e->labels[r->label], GET_CODE_POS(e) - 4, 0xFFFFFFFF, 0);
    } else {
        tb_unreachable();
    }
}

enum {
    VEX_0F   = 1,
    VEX_0F38 = 2,
    VEX_0F3A = 3,
};

enum {
    VEX_NONE,
    VEX_66,
    VEX_F3,
    VEX_F2,
};

static void emit_vex(TB_CGEmitter* e, bool is_64bit, uint8_t rx, uint8_t base, uint8_t index, uint8_t v, uint8_t m, uint8_t p) {
    // idk why but the VEX stuff is all complements
    v     = ~v & 0b1111;
    base  = ~(base >> 3) & 1;
    index = ~(index >> 3) & 1;
    rx    = ~(rx >> 3) & 1;

    // VEX3
    EMIT1(e, 0xC4);
    EMIT1(e, (base << 5) | (index << 6) | (rx << 7) | m);
    EMIT1(e, (is_64bit ? 128 : 0) | (v << 3) | p);
}

static void asm_inst2(TB_CGEmitter* e, int type, TB_X86_DataType dt, const Val* a, const Val* b) {
    if (dt >= TB_X86_PBYTE && dt <= TB_X86_PQWORD) {
        inst2sseint(e, type, a, b, dt);
    } else if (dt >= TB_X86_F32x1 && dt <= TB_X86_F64x2) {
        inst2sse(e, type, a, b, dt);
    } else {
        inst2(e, type, a, b, dt);
    }
}
