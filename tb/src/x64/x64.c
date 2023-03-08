#include "../x64/x64.h"
#include "../x64/x64_emitter.h"

enum {
    CG_REGISTER_CLASSES = 2
};

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM
};

typedef enum X86_InstType {
    // mov    lea    add
    X86_FIRST_INST2    = 0,
    // movps    ucomiss
    X86_FIRST_INST2SSE = 256,
    // call [rcx]     div
    X86_FIRST_UNARY    = 0x1000,


    // magic instructions
    X86_INST_JMP = -1,
    X86_INST_JCC = -2,
    X86_INST_RET = -3,
    //   dst = COPY src
    X86_INST_COPY = -4,
    X86_INST_MOVE = -5,
    X86_INST_CALL_SYM = -6,
} X86_InstType;

// for memory operands imm[0] is two fields:
//   top 32bits is scale, bottom 32bits is displacement
typedef enum X86_OperandLayout {
    // label
    X86_OP_L,

    // integer unary
    X86_OP_R,
    X86_OP_M,
    X86_OP_I,

    // integer binary ops
    X86_OP_RR,
    X86_OP_RI,
    X86_OP_MI,
    X86_OP_RM,
    X86_OP_MR,
} X86_OperandLayout;

typedef struct Inst {
    X86_InstType type;
    X86_OperandLayout layout;
    TB_DataType data_type;

    // virtual registers (-1 means none, -2 and lower is VREGs, 0+ is normal registers)
    //
    //   regs[0] is a destination
    int regs[4];
    uint64_t imm[2];
} Inst;

#include "generic_cg.h"

#define SUBMIT(i) (seq->insts[seq->inst_count++] = (i))
static Inst inst_jcc(int target, Cond cc) {
    return (Inst){
        .type = X86_INST_JCC,
        .layout = X86_OP_L,
        .imm = { target, cc }
    };
}

static Inst inst_jmp(int target) {
    return (Inst){
        .type = X86_INST_JMP,
        .layout = X86_OP_L,
        .imm[0] = target
    };
}

static Inst inst_ret(TB_DataType dt, int src) {
    return (Inst){
        .type = X86_INST_RET,
        .layout = X86_OP_R,
        .data_type = dt,
        .regs = { -1, src }
    };
}

static Inst inst_move(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = X86_INST_MOVE,
        .layout = X86_OP_RR,
        .data_type = dt,
        .regs = { 0, lhs, rhs }
    };
}

static Inst inst_call_sym(TB_DataType dt, int dst, const TB_Symbol* sym) {
    return (Inst){
        .type = X86_INST_CALL_SYM,
        .layout = X86_OP_L,
        .data_type = dt,
        .regs = { dst },
        .imm[0] = (uintptr_t) sym,
    };
}

static Inst inst_copy(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = X86_INST_COPY,
        .layout = X86_OP_RR,
        .data_type = dt,
        .regs = { lhs, rhs }
    };
}

static Inst inst_rr(int op, TB_DataType dt, int dst, int lhs, int rhs) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_RR,
        .data_type = dt,
        .regs = { dst, lhs, rhs }
    };
}

static Inst inst_ri(int op, TB_DataType dt, int dst, int lhs, int32_t imm) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_RI,
        .data_type = dt,
        .regs = { dst, lhs },
        .imm[0] = imm,
    };
}

static Inst inst_r(int op, TB_DataType dt, int dst, int src) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_R,
        .data_type = dt,
        .regs = { dst, src },
    };
}

static Inst inst_i(int op, TB_DataType dt, int dst, int32_t imm) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_I,
        .data_type = dt,
        .regs = { dst },
        .imm[0] = imm,
    };
}

static Inst inst_m(int op, TB_DataType dt, int dst, int base, int index, Scale scale, int32_t disp) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_M,
        .data_type = dt,
        .regs = { dst, 0, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static Inst inst_mr(int op, TB_DataType dt, int base, int index, Scale scale, int32_t disp, int rhs) {
    return (Inst){
        .type = X86_FIRST_INST2 + op,
        .layout = X86_OP_MR,
        .data_type = dt,
        .regs = { -1, rhs, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static int classify_reg_class(TB_DataType dt) {
    return dt.type == TB_FLOAT ? REG_CLASS_XMM : REG_CLASS_GPR;
}

// *out_mask of 0 means no mask
static TB_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_INT || dt.type == TB_PTR);
    if (dt.type == TB_PTR) return *out_mask = 0, TB_TYPE_I64;

    TB_DataType t = TB_TYPE_VOID;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = TB_TYPE_I8;
    else if (dt.data <= 16) bits = 16, t = TB_TYPE_I16;
    else if (dt.data <= 32) bits = 32, t = TB_TYPE_I32;
    else if (dt.data <= 64) bits = 64, t = TB_TYPE_I64;

    assert(bits != 0 && "TODO: large int support");
    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

static void print_operand(Val* v) {
    switch (v->type) {
        case VAL_GPR: printf("%s", GPR_NAMES[v->reg]); break;
        case VAL_IMM: printf("%d", v->imm); break;
        case VAL_MEM: {
            printf("[%s + %d]", GPR_NAMES[v->mem.base], v->mem.disp);
            break;
        }
        default: tb_todo();
    }
}

static Val spill_to_stack_slot(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n, Val* src) {
    if (src && src->type == VAL_MEM && src->mem.base == RBP) {
        return *src;
    }

    Val dst;
    if (n->type == TB_PARAM) {
        int id = TB_NODE_GET_EXTRA_T(n, TB_NodeParam)->id;

        // TODO(NeGate): this is win64 specific... maybe?
        dst = val_stack(TB_TYPE_PTR, 16 + (id * 8));
    } else {
        // allocate new stack slot
        int pos = STACK_ALLOC(8, 8);
        dst = val_stack(TB_TYPE_PTR, pos);
        dst.mem.is_rvalue = true;
    }

    if (src != NULL) {
        INST2(!is_lvalue(src) ? MOV : LEA, &dst, src, n->dt);
        ASM {
            printf("  MOV ");
            print_operand(&dst);
            printf(", ");
            print_operand(src);

            printf(" \x1b[32m# spill r%d\x1b[0m\n", NAME(n));
        }
    }

    return dst;
}

static bool try_for_imm32(Ctx* restrict ctx, TB_Node* n, int32_t* out_x) {
    if (n->type == TB_INTEGER_CONST && try_tile(ctx, n)) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        if (i->num_words == 1 && fits_into_int32(i->words[0])) {
            *out_x = i->words[0];
            return true;
        }
    }

    return false;
}

static Inst isel_load(Ctx* restrict ctx, TB_Node* n, int dst) {
    if (n->inputs[0]->type == TB_LOCAL) {
        ptrdiff_t search = nl_map_get(ctx->stack_slots, n->inputs[0]);
        assert(search >= 0);

        return inst_m(MOV, n->dt, dst, RBP, GPR_NONE, SCALE_X1, ctx->stack_slots[search].v);
    }

    return inst_m(MOV, n->dt, dst, USE_VAL(n->inputs[0]), GPR_NONE, SCALE_X1, 0);
}

static Cond isel_cmp(Ctx* restrict ctx, Sequence* seq, TB_Node* n) {
    if (try_tile(ctx, n) && n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        assert(cmp_dt.width == 0 && "TODO: Implement vector compares");
        assert(!TB_IS_FLOAT_TYPE(cmp_dt) && "TODO");

        Cond cc = -1;
        bool invert = false;

        int32_t x;
        int lhs = ISEL(n->inputs[0]);
        if (try_for_imm32(ctx, n->inputs[1], &x)) {
            SUBMIT(inst_ri(CMP, cmp_dt, -1, lhs, x));
        } else {
            int rhs = ISEL(n->inputs[1]);
            SUBMIT(inst_rr(CMP, cmp_dt, -1, lhs, rhs));
        }

        switch (n->type) {
            case TB_CMP_EQ: cc = E; break;
            case TB_CMP_NE: cc = NE; break;
            case TB_CMP_SLT: cc = invert ? G : L; break;
            case TB_CMP_SLE: cc = invert ? GE : LE; break;
            case TB_CMP_ULT: cc = invert ? A : B; break;
            case TB_CMP_ULE: cc = invert ? NB : BE; break;
            default: tb_unreachable();
        }
        return cc;
    }

    int src = ISEL(n);
    SUBMIT(inst_rr(TEST, n->dt, -1, src, src));
    return NE;
}

static int isel(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n) {
    int current_val = GET_VAL(n);
    if (current_val >= 0) {
        return current_val;
    }

    try_tile(ctx, n);

    TB_NodeTypeEnum type = n->type;
    int dst = -1;

    switch (type) {
        case TB_PARAM: {
            int id = TB_NODE_GET_EXTRA_T(n, TB_NodeParam)->id;
            if (id >= 4) {
                // return val_stack(TB_TYPE_PTR, 16 + (id * 8));
                tb_todo();
            } else {
                // copy from parameter
                dst = DEF_HINTED(n, REG_CLASS_GPR, WIN64_GPR_PARAMETERS[id]);

                SUBMIT(inst_copy(n->dt, dst, WIN64_GPR_PARAMETERS[id]));
            }
            break;
        }
        case TB_LOCAL: {
            ptrdiff_t search = nl_map_get(ctx->stack_slots, n);
            if (search >= 0) {
                // get address
                dst = DEF(n, REG_CLASS_GPR);
                SUBMIT(inst_m(LEA, n->dt, dst, RBP, GPR_NONE, SCALE_X1, ctx->stack_slots[search].v));
            } else {
                TB_NodeLocal* local = TB_NODE_GET_EXTRA(n);

                int pos = STACK_ALLOC(local->size, local->align);
                nl_map_put(ctx->stack_slots, n, pos);
            }
            break;
        }

        case TB_INTEGER_CONST: {
            TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
            assert(i->num_words == 1);

            uint64_t x = i->words[0];
            assert(fits_into_int32(x));

            dst = DEF(n, REG_CLASS_GPR);
            SUBMIT(inst_i(MOV, n->dt, dst, x));
            break;
        }

        case TB_NEG:
        case TB_NOT: {
            dst = DEF(n, REG_CLASS_GPR);
            int src = ISEL(n->inputs[0]);

            SUBMIT(inst_r(type == TB_NOT ? NOT : NEG, n->dt, dst, src));
            break;
        }

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB: {
            const static Inst2Type ops[]  = { AND, OR, XOR, ADD, SUB };
            Inst2Type op = ops[type - TB_AND];

            dst = DEF(n, REG_CLASS_GPR);

            int32_t x;
            if (try_for_imm32(ctx, n->inputs[1], &x)) {
                int lhs = ISEL(n->inputs[0]);
                SUBMIT(inst_ri(op, n->dt, dst, lhs, x));
            } else {
                int lhs = ISEL(n->inputs[0]);
                int rhs = ISEL(n->inputs[1]);
                SUBMIT(inst_rr(op, n->dt, dst, lhs, rhs));
            }
            break;
        }

        case TB_RET: {
            // hint input to be RAX
            int src_vreg = isel(ctx, seq, n->inputs[0]);
            if (ctx->defs[src_vreg].hint < 0) {
                ctx->defs[src_vreg].hint = RAX;
            }

            SUBMIT(inst_ret(n->inputs[0]->dt, USE(src_vreg)));
            break;
        }

        case TB_LOAD: {
            dst = DEF(n, REG_CLASS_GPR);
            SUBMIT(isel_load(ctx, n, dst));
            break;
        }
        case TB_STORE: {
            int src = ISEL(n->inputs[1]);

            if (n->inputs[0]->type == TB_LOCAL) {
                ptrdiff_t search = nl_map_get(ctx->stack_slots, n->inputs[0]);
                assert(search >= 0);

                inst_mr(MOV, n->dt, RBP, GPR_NONE, SCALE_X1, ctx->stack_slots[search].v, src);
            } else {
                inst_mr(MOV, n->dt, USE_VAL(n->inputs[0]), GPR_NONE, SCALE_X1, 0, src);
            }
            break;
        }
        case TB_SIGN_EXT: {
            TB_Node* src = n->inputs[0];

            int bits_in_type = src->dt.type == TB_PTR ? 64 : src->dt.data;
            int op = MOV;
            switch (bits_in_type) {
                case 64: op = MOV; break;
                case 32: op = MOVSXD; break;
                case 16: op = MOVSXW; break;
                case 8:  op = MOVSXB; break;
                default: tb_todo();
            }

            dst = DEF(n, REG_CLASS_GPR);
            if (try_tile(ctx, src)) {
                // movsx dst, [src]
                Inst inst = isel_load(ctx, src, dst);
                inst.type = X86_FIRST_INST2 + op;
                SUBMIT(inst);
            } else {
                // movsx dst, src
                SUBMIT(inst_r(op, n->dt, dst, ISEL(n->inputs[0])));
            }
            break;
        }

        case TB_BRANCH: {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            if (br->count == 0) {
                if (ctx->fallthrough != br->default_label) {
                    SUBMIT(inst_jmp(br->default_label));
                }
            } else if (br->count == 1) {
                // if-like branch
                Cond cc = isel_cmp(ctx, seq, n->inputs[0]);

                SUBMIT(inst_jcc(br->targets[0].value, cc));
                if (ctx->fallthrough != br->default_label) {
                    SUBMIT(inst_jmp(br->default_label));
                }
            } else {
                tb_todo();
            }
            break;
        }

        case TB_CALL: {
            static const struct ParamDescriptor {
                int gpr_count;
                int xmm_count;
                uint16_t callee_saved_xmm_count; // XMM0 - XMMwhatever
                uint16_t caller_saved_gprs;      // bitfield

                GPR gprs[6];
            } param_descs[] = {
                // win64
                { 4, 4, 16, WIN64_ABI_CALLER_SAVED,  { RCX, RDX, R8, R9,   0,  0 } },
                // system v
                { 6, 4, 5, SYSV_ABI_CALLER_SAVED,    { RDI, RSI, RDX, RCX, R8, R9 } },
                // syscall
                { 6, 4, 5, SYSCALL_ABI_CALLER_SAVED, { RDI, RSI, RDX, R10, R8, R9 } },
            };

            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
            const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];
            if (type == TB_SCALL) {
                desc = &param_descs[2];
            }

            // generate clones of each parameter which live until the CALL instruction executes
            dst = DEF_HINTED(n, REG_CLASS_GPR, RAX);
            int fake_dst = DEF_FORCED(n, REG_CLASS_GPR, RAX, dst);

            FOREACH_N(i, 1, n->input_count) {
                TB_Node* param = n->inputs[i];
                TB_DataType param_dt = param->dt;

                if (TB_IS_FLOAT_TYPE(param_dt) || param_dt.width) {
                    tb_todo();
                } else {
                    int src = ISEL(param);

                    if (i - 1 < desc->gpr_count) {
                        int param_def = DEF_FORCED(param, REG_CLASS_GPR, desc->gprs[i - 1], fake_dst);

                        SUBMIT(inst_copy(param->dt, param_def, src));
                    } else {
                        tb_todo();
                    }
                }
            }

            if (type == TB_SCALL) {
                __debugbreak();
            } else {
                if (try_tile(ctx, n->inputs[0]) && n->inputs[0]->type == TB_GET_SYMBOL_ADDRESS) {
                    TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n->inputs[0]);
                    SUBMIT(inst_call_sym(n->dt, fake_dst, s->sym));
                } else {
                    __debugbreak();
                }
            }

            SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
            break;
        }

        case TB_NULL:
        case TB_PHI:
        break;

        default: tb_todo();
    }

    GET_VAL(n) = dst;
    return dst;
}

static void copy_value(Ctx* restrict ctx, Sequence* seq, int dst, int src, TB_DataType dt) {
    SUBMIT(inst_move(dt, dst, src));
}

static int8_t resolve_def(Ctx* restrict ctx, int x) {
    return ctx->defs[x].reg;
}

static int8_t resolve_use(Ctx* restrict ctx, int x) {
    if (x < -1) return ctx->defs[-x - 2].reg;
    return x;
}

static void inst2_print(Ctx* restrict ctx, Inst2Type op, Val* dst, Val* src, TB_DataType dt) {
    INST2(op, dst, src, dt);
    ASM {
        #define A(x) case x: printf("  " #x " "); break
        switch (op) {
            A(ADD);
            A(AND);
            A(OR);
            A(SUB);
            A(XOR);
            A(CMP);
            A(MOV);
            A(LEA);

            A(MOVSXB);
            A(MOVSXW);
            A(MOVSXD);
            A(MOVZXB);
            A(MOVZXW);
            default: tb_todo();
        }
        #undef A

        print_operand(dst);
        printf(", ");
        print_operand(src);
        printf("\n");
    }
}

static void inst1_print(Ctx* restrict ctx, int op, Val* src) {
    INST1(op, src);
    ASM {
        #define A(x) case x: printf("  " #x " "); break
        switch (op) {
            A(IDIV);
            A(DIV);
            A(NOT);
            A(NEG);
            default: tb_todo();
        }
        #undef A

        print_operand(src);
        printf("\n");
    }
}

static void emit_sequence(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n) {
    Val ops[4];
    FOREACH_N(i, 0, seq->inst_count) {
        Inst* restrict inst = &seq->insts[i];

        if (inst->type == X86_INST_JMP) {
            JMP(inst->imm[0]);
            ASM {
                printf("  JMP L%zu\n", inst->imm[0]);
            }
            continue;
        } else if (inst->type == X86_INST_JCC) {
            JCC(inst->imm[1], inst->imm[0]);
            ASM {
                printf("  J%s L%zu\n", COND_NAMES[inst->imm[1]], inst->imm[0]);
            }
            continue;
        } else if (inst->type == X86_INST_CALL_SYM) {
            const TB_Symbol* target = (const TB_Symbol*) (uintptr_t) inst->imm[0];

            tb_emit_symbol_patch(ctx->module, ctx->f, target, GET_CODE_POS(&ctx->emit) + 1, true);

            // CALL rel32
            EMIT1(&ctx->emit, 0xE8), EMIT4(&ctx->emit, 0x0);
            ASM {
                printf("  CALL %s\n", target->name);
            }
            continue;
        }

        bool has_def = false;
        if (inst->regs[0] >= 0) {
            ops[0] = val_gpr(TB_TYPE_I64, resolve_def(ctx, inst->regs[0]));
            has_def = true;
        }

        int8_t regs[4];
        regs[0] = 0;
        FOREACH_N(i, 1, 4) {
            regs[i] = resolve_use(ctx, inst->regs[i]);
        }

        // convert into normie operands
        int op_count = 0;
        switch (inst->layout) {
            case X86_OP_R: {
                ops[1] = val_gpr(TB_TYPE_I64, regs[1]);
                op_count = 2;
                break;
            }
            case X86_OP_I: {
                ops[1] = val_imm(TB_TYPE_I64, inst->imm[0]);
                op_count = 2;
                break;
            }
            case X86_OP_RR: {
                ops[1] = val_gpr(TB_TYPE_I64, regs[1]);
                ops[2] = val_gpr(TB_TYPE_I64, regs[2]);
                op_count = 3;
                break;
            }
            case X86_OP_RI: {
                ops[1] = val_gpr(TB_TYPE_I64, regs[1]);
                ops[2] = val_imm(TB_TYPE_I64, inst->imm[0]);
                op_count = 3;
                break;
            }
            case X86_OP_M: {
                Scale scale = (inst->imm[0] >> 32);
                int32_t disp = inst->imm[0] & 0xFFFFFFFF;

                ops[1] = val_base_index(TB_TYPE_I64, regs[2], regs[3], scale);
                ops[1].mem.disp = disp;
                op_count = 2;
                break;
            }
            default: tb_todo();
        }

        if (inst->type == X86_INST_MOVE) {
            if (!is_value_match(&ops[1], &ops[2])) {
                inst2_print(ctx, MOV, &ops[1], &ops[2], inst->data_type);
            }
        } else if (inst->type == X86_INST_COPY) {
            if (!is_value_match(&ops[0], &ops[1])) {
                inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
            }
        } else if (inst->type == X86_INST_RET) {
            // mov src into RAX
            ops[0] = val_gpr(TB_TYPE_I64, RAX);
            if (ops[1].reg != RAX) {
                inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
            }

            if (ctx->fallthrough != -1) {
                ret_jmp(&ctx->emit);
                ASM {
                    printf("  JMP .ret\n");
                }
            }
        } else if (inst->type >= X86_FIRST_UNARY) {
            if (!is_value_match(&ops[0], &ops[1])) {
                inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
            }

            ops[0].dt = inst->data_type;
            inst1_print(ctx, inst->type, &ops[0]);
        } else if (inst->type >= X86_FIRST_INST2) {
            if (!has_def) {
                inst2_print(ctx, (Inst2Type) inst->type, &ops[1], &ops[2], inst->data_type);
            } else if (op_count == 2) {
                inst2_print(ctx, (Inst2Type) inst->type, &ops[0], &ops[1], inst->data_type);
            } else {
                assert(op_count == 3);
                if (ops[0].reg != ops[1].reg) {
                    inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
                }
                inst2_print(ctx, (Inst2Type) inst->type, &ops[0], &ops[2], inst->data_type);
            }
        } else {
            tb_todo();
        }
    }

    if (n == NULL) {
        return;
    }
}

static void patch_local_labels(Ctx* restrict ctx) {
    FOREACH_N(i, 0, ctx->emit.ret_patch_count) {
        uint32_t pos = ctx->emit.ret_patches[i];
        PATCH4(&ctx->emit, pos, GET_CODE_POS(&ctx->emit) - (pos + 4));
    }

    FOREACH_N(i, 0, ctx->emit.label_patch_count) {
        uint32_t pos = ctx->emit.label_patches[i].pos;
        uint32_t target_lbl = ctx->emit.label_patches[i].target_lbl;

        PATCH4(&ctx->emit, pos, ctx->emit.labels[target_lbl] - (pos + 4));
    }
}

static size_t emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static size_t emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    out[0] = 0xC3;
    return 1;
}

static size_t emit_call_patches(TB_Module* restrict m) {
    size_t r = 0;
    FOREACH_N(i, 0, m->max_threads) {
        TB_SymbolPatch* patches = m->thread_info[i].symbol_patches;

        dyn_array_for(j, patches) {
            TB_SymbolPatch* patch = &patches[j];

            if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                TB_FunctionOutput* out_f = patch->source->output;
                assert(out_f && "Patch cannot be applied to function with no compiled output");

                // x64 thinks of relative addresses as being relative
                // to the end of the instruction or in this case just
                // 4 bytes ahead hence the +4.
                size_t actual_pos = out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                uint32_t p = ((TB_Function*) patch->target)->output->code_pos - actual_pos;
                memcpy(&out_f->code[out_f->prologue_length + patch->pos], &p, sizeof(uint32_t));
                r += 1;
            }
        }
    }

    return r;
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .emit_prologue      = emit_prologue,
    .emit_epilogue      = emit_epilogue,

    .fast_path = compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
