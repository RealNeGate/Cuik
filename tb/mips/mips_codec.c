#include <tb_mips.h>

////////////////////////////////
// Encode
////////////////////////////////
enum {
    #define R(name, op, funct, t) name,
    #define I(name, op, t)        name,
    #define J(name, op, t)        name,
    #include "mips_insts.inc"

    INST_MAX,
};

#define __(t, op, ...) t ## type(e, op, __VA_ARGS__)
static uint32_t insts[INST_MAX] = {
    #define R(name, op, funct, t) [name] = ((op<<26) | (funct)),
    #define I(name, op, t)        [name] = (op<<26),
    #define J(name, op, t)        [name] = (op<<26),
    #include "mips_insts.inc"
};

static void nop(TB_CGEmitter* e) {
    EMIT4(e, 0);
}

static void jtype(TB_CGEmitter* e, int op, uint32_t imm) {
    EMIT4(e, insts[op] | (imm & 0x3FFFFFF));
}

static void rtype(TB_CGEmitter* e, int op, uint32_t rd, uint32_t rs, uint32_t rt, uint32_t shamt) {
    assert(op >= 0 && op < INST_MAX);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | (rd<<11) | (shamt<<6));
}

static void itype(TB_CGEmitter* e, int op, uint32_t rt, uint32_t rs, uint32_t imm) {
    assert(op >= 0 && op < INST_MAX);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16) | (imm&0xFFFF));
}

static void branch(TB_CGEmitter* e, int op, uint32_t rt, uint32_t rs, int id) {
    assert(op == beq);
    EMIT4(e, insts[op] | (rs<<21) | (rt<<16));
    tb_emit_rel32(e, &e->labels[id], GET_CODE_POS(e) - 4, 0xFFFF, 2);
}

static const char* gpr_names[32] = {
    "zr",
    "at",
    "v0", "v1",
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
    "t4", "t5", "t6", "t7",
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
    "t8", "t9",
    "k0", "k1",
    "gp",
    "sp",
    "fp",
    "ra",
};

////////////////////////////////
// Decode
////////////////////////////////
typedef struct {
    const char* name;
    int op;
    enum { ITYPE, RTYPE, JTYPE } family;
} Op;

static Op decode(uint32_t inst) {
    #define R(name, op, funct, t) if ((inst >> 26) == op && (inst & 0b111111) == funct) { return (Op){ #name, name, RTYPE }; }
    #define I(name, op, t)        if ((inst >> 26) == op) { return (Op){ #name, name, ITYPE }; }
    #define J(name, op, t)        if ((inst >> 26) == op) { return (Op){ #name, name, JTYPE }; }
    #include "mips_insts.inc"

    return (Op){ "error", RTYPE };
}

#define E(fmt, ...) tb_disasm_outf(disasm, fmt, ## __VA_ARGS__)
size_t tb_mips_print_inst(TB_Disasm* disasm, uint32_t inst, bool has_relocs) {
    size_t old = disasm->out_curr;

    // special case
    if (inst == 0) {
        E("nop");
    } else if ((inst >> 26) == 0 && (inst & 0b111111) == 0b100101 && ((inst >> 16) & 0b11111) == 0) {
        int a = (inst >> 11) & 0b11111;
        int c = (inst >> 21) & 0b11111;
        E("move $%s, $%s", gpr_names[a], gpr_names[c]);
    } else if ((inst >> 26) == 0b001101 && ((inst >> 21) & 0b11111) == 0) {
        int a = (inst >> 16) & 0b11111;
        E("lui $%s, %d", gpr_names[a], inst & 0xFFFF);
    } else {
        // figure out what instruction type it is (everything else is trivial from there)
        Op op = decode(inst);
        E("%s ", op.name);
        switch (op.family) {
            case RTYPE: {
                if (op.op == madds || op.op == maddd) {
                    int fd = (inst >> 6)  & 0b11111;
                    int fs = (inst >> 11) & 0b11111;
                    int ft = (inst >> 16) & 0b11111;
                    int fr = (inst >> 21) & 0b11111;
                    E("$f%d, $f%d, $f%d, $f%d", fd, fr, fs, ft);
                } else {
                    int shamt = (inst >> 6) & 0b11111;
                    int a = (inst >> 11) & 0b11111;
                    int b = (inst >> 16) & 0b11111;
                    int c = (inst >> 21) & 0b11111;
                    E("$%s, $%s", gpr_names[a], gpr_names[b]);
                    if (shamt) {
                        E(", %d", shamt);
                    } else {
                        E(", $%s", gpr_names[c]);
                    }
                }
                break;
            }

            #if 0
            case ITYPE: {
                int b = (inst >> 16) & 0b11111;
                int c = (inst >> 21) & 0b11111;
                if (op.op >= lb && op.op <= sdc1) {
                    if (op.op == ldc1 || op.op == sdc1) {
                        E("$f%d, $%s(%d)", c, gpr_names[b], tb__sxt(inst & 0xFFFF, 16, 64));
                    } else {
                        E("$%s, $%s(%d)", gpr_names[c], gpr_names[b], tb__sxt(inst & 0xFFFF, 16, 64));
                    }
                } else {
                    E("$%s, $%s", gpr_names[b], gpr_names[c]);

                    int32_t imm = tb__sxt(inst & 0xFFFF, 16, 64);
                    if (d->patch && d->patch->pos == pos - 4) {
                        const TB_Symbol* target = d->patch->target;
                        d->patch = d->patch->next;

                        if (target->name[0] == 0) {
                            E("sym%p", target);
                        } else {
                            E("%s", target->name);
                        }

                        if (imm) {
                            E(" + %d", imm*4);
                        }
                    } else if (op.op >= beq && op.op <= bgtz) {
                        uint32_t target = pos + imm*4;
                        int bb = tb_emit_get_label(e, target);
                        uint32_t landed = e->labels[bb] & 0x7FFFFFFF;

                        if (landed != target) {
                            E(", .bb%d + %d", bb, (int)target - (int)landed);
                        } else {
                            E(", .bb%d", bb);
                        }
                    } else {
                        E(", %d", imm);
                    }
                }
                break;
            }

            case JTYPE: {
                int32_t disp = (inst & 0x3FFFFFF) * 2;
                if (d->patch && d->patch->pos == pos - 4) {
                    const TB_Symbol* target = d->patch->target;
                    d->patch = d->patch->next;

                    if (target->name[0] == 0) {
                        E("sym%p", target);
                    } else {
                        E("%s", target->name);
                    }

                    if (disp) {
                        E(" + %"PRIu64, disp);
                    }
                } else {
                    E("%"PRIu64, disp);
                }
                break;
            }
            #endif

            default: tb_todo();
        }
    }
    return disasm->out_curr - old;
}
#undef E

TB_API bool tb_mips_disasm(TB_MIPS_Inst* restrict inst, size_t length, const uint8_t* data) {
    if (length < 4) {
        return false;
    }

    memcpy(&inst->raw, data, sizeof(uint32_t));
    return true;
}

