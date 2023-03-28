#include "../x64/x64.h"
#include "../x64/x64_emitter.h"
#include "../objects/win64eh.h"

enum {
    CG_REGISTER_CLASSES = 2
};

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM
};

typedef enum X86_InstType {
    //   dst = COPY src
    X86_INST_COPY = -3,
    X86_INST_MOVE = -4,
} X86_InstType;

// for memory operands imm[0] is two fields:
//   top 32bits is scale, bottom 32bits is displacement
typedef enum X86_OperandLayout {
    X86_OP_NONE,

    // label
    X86_OP_L,
    // global
    X86_OP_G,

    // integer unary
    X86_OP_R,
    X86_OP_M,
    X86_OP_I,
    X86_OP_A,

    // integer binary ops
    X86_OP_RR,
    X86_OP_RI,
    X86_OP_MI,
    X86_OP_RM,
    X86_OP_MR,
} X86_OperandLayout;

typedef struct Inst Inst;
struct Inst {
    Inst* next;

    InstType type;
    X86_OperandLayout layout;
    X86_DataType data_type;
    int time;

    // virtual registers (-1 means none, -2 and lower is VREGs, 0+ is normal registers)
    //
    //   regs[0] is a destination
    int regs[4];
    uint64_t imm[2];
};

typedef struct {
    TB_DataType dt;

    int old;
    int stack_pos;
} Reload;

#include "generic_cg.h"

// *out_mask of 0 means no mask
static X86_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_INT || dt.type == TB_PTR);
    if (dt.type == TB_PTR) return *out_mask = 0, X86_TYPE_QWORD;

    X86_DataType t = X86_TYPE_NONE;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = X86_TYPE_BYTE;
    else if (dt.data <= 16) bits = 16, t = X86_TYPE_WORD;
    else if (dt.data <= 32) bits = 32, t = X86_TYPE_DWORD;
    else if (dt.data <= 64) bits = 64, t = X86_TYPE_QWORD;

    assert(bits != 0 && "TODO: large int support");
    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

static X86_DataType legalize_int2(TB_DataType dt) {
    uint64_t m;
    return legalize_int(dt, &m);
}

static Inst inst_jcc(int target, Cond cc) {
    return (Inst){
        .type = JO + cc,
        .layout = X86_OP_L,
        .regs   = { -1 },
        .imm = { target }
    };
}

static Inst inst_setcc(Cond cc, int src) {
    return (Inst){
        .type = SETO + cc,
        .layout = X86_OP_R,
        .regs   = { -1, src },
        .imm = { 0 }
    };
}

static Inst inst_jmp(int target) {
    return (Inst){
        .type = JMP,
        .layout = X86_OP_L,
        .regs   = { -1 },
        .imm[0] = target
    };
}

static Inst inst_u(int op, TB_DataType dt) {
    return (Inst){
        .type = op,
        .layout = X86_OP_NONE,
        .data_type = legalize_int2(dt),
    };
}

static Inst inst_move(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = (int)X86_INST_MOVE,
        .layout = X86_OP_RR,
        .data_type = legalize_int2(dt),
        .regs = { -1, lhs, rhs }
    };
}

static Inst inst_nullary(int op) {
    return (Inst){
        .type = op,
        .layout = X86_OP_NONE,
        .data_type = X86_TYPE_NONE,
    };
}

static Inst inst_call(TB_DataType dt, int dst, const TB_Symbol* sym) {
    return (Inst){
        .type = CALL,
        .layout = X86_OP_G,
        .data_type = legalize_int2(dt),
        .regs = { dst },
        .imm[0] = (uintptr_t) sym,
    };
}

static Inst inst_g(int op, TB_DataType dt, int dst, const TB_Symbol* sym) {
    return (Inst){
        .type = op,
        .layout = X86_OP_G,
        .data_type = legalize_int2(dt),
        .regs = { dst },
        .imm[0] = (uintptr_t) sym,
    };
}

static Inst inst_copy(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = (int) X86_INST_COPY,
        .layout = X86_OP_RR,
        .data_type = legalize_int2(dt),
        .regs = { lhs, rhs }
    };
}

static Inst inst_rr(int op, TB_DataType dt, int dst, int lhs, int rhs) {
    assert(lhs != -1);
    return (Inst){
        .type = op,
        .layout = X86_OP_RR,
        .data_type = legalize_int2(dt),
        .regs = { dst, lhs, rhs }
    };
}

static Inst inst_ri(int op, TB_DataType dt, int dst, int lhs, int32_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_RI,
        .data_type = legalize_int2(dt),
        .regs = { dst, lhs },
        .imm[0] = imm,
    };
}

static Inst inst_r(int op, TB_DataType dt, int dst, int src) {
    return (Inst){
        .type = op,
        .layout = X86_OP_R,
        .data_type = legalize_int2(dt),
        .regs = { dst, src },
    };
}

static Inst inst_i(int op, TB_DataType dt, int dst, int32_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_I,
        .data_type = legalize_int2(dt),
        .regs = { dst },
        .imm[0] = imm,
    };
}

static Inst inst_i64(int op, TB_DataType dt, int dst, uint64_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_A,
        .data_type = legalize_int2(dt),
        .regs = { dst },
        .imm = { imm & 0xFFFFFFFF, imm >> 32ull }
    };
}

static Inst inst_m(int op, TB_DataType dt, int dst, int base, int index, Scale scale, int32_t disp) {
    return (Inst){
        .type = op,
        .layout = X86_OP_M,
        .data_type = legalize_int2(dt),
        .regs = { dst, 0, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static Inst inst_mr(int op, TB_DataType dt, int base, int index, Scale scale, int32_t disp, int rhs) {
    return (Inst){
        .type = op,
        .layout = X86_OP_MR,
        .data_type = legalize_int2(dt),
        .regs = { -1, rhs, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static int classify_reg_class(TB_DataType dt) {
    return dt.type == TB_FLOAT ? REG_CLASS_XMM : REG_CLASS_GPR;
}

static void print_operand(Val* v) {
    switch (v->type) {
        case VAL_GPR: {
            assert(v->reg >= 0 && v->reg < 16);
            printf("%s", GPR_NAMES[v->reg]);
            break;
        }
        case VAL_XMM: printf("XMM%d", v->reg); break;
        case VAL_IMM: printf("%d", v->imm); break;
        case VAL_ABS: printf("%#llx", v->abs); break;
        case VAL_MEM: {
            if (v->index == -1) {
                printf("[%s + %d]", GPR_NAMES[v->reg], v->imm);
            } else {
                printf("[%s + %s*%d + %d]", GPR_NAMES[v->reg], GPR_NAMES[v->index], 1u << v->scale, v->imm);
            }
            break;
        }
        case VAL_GLOBAL: {
            const TB_Symbol* target = v->symbol;
            printf("[%s + %d]", target->name, v->imm);
            break;
        }
        case VAL_LABEL: {
            if (v->imm == -1) printf(".ret_jmp");
            else printf("L%d", v->imm);
            break;
        }
        default: tb_todo();
    }
}

static bool try_for_imm8(Ctx* restrict ctx, TB_Node* n, int32_t* out_x) {
    if (n->type == TB_INTEGER_CONST) {
        try_tile(ctx, n);

        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        if (i->num_words == 1 && fits_into_int8(i->words[0])) {
            *out_x = i->words[0];
            return true;
        }
    }

    return false;
}

static bool try_for_imm32(Ctx* restrict ctx, TB_Node* n, int32_t* out_x) {
    if (n->type == TB_INTEGER_CONST) {
        try_tile(ctx, n);

        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        if (i->num_words == 1 && fits_into_int32(i->words[0])) {
            *out_x = i->words[0];
            return true;
        }
    }

    return false;
}

static Inst isel_array(Ctx* restrict ctx, TB_Node* n, int dst) {
    int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;
    TB_Node* base_n = n->inputs[0];
    TB_Node* index_n = n->inputs[1];

    if (index_n->type == TB_INTEGER_CONST) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(index_n);
        if (i->num_words == 1 && fits_into_int32(i->words[0] * stride)) {
            try_tile(ctx, index_n);

            int32_t disp = i->words[0] * stride;
            int base = ISEL(base_n);

            return inst_m(LEA, n->dt, dst, base, GPR_NONE, 0, disp);
        }
    }

    // we resolve the index then add the base
    //
    // if it's an LEA index*stride
    // then stride > 0, if not it's free
    // do think of it however
    int scaled_index = DEF(n, REG_CLASS_GPR);

    uint8_t stride_as_shift = 0;
    bool scaled_already = false;
    if (tb_is_power_of_two(stride)) {
        stride_as_shift = tb_ffs(stride) - 1;

        if (stride_as_shift > 3) {
            scaled_already = true;

            int index = ISEL(index_n);
            SUBMIT(inst_ri(SHL, n->dt, scaled_index, index, stride_as_shift));
        } else {
            scaled_index = ISEL(index_n);
        }
    } else {
        tb_todo();
    }

    int base = ISEL(base_n);
    if (!scaled_already) {
        return inst_m(LEA, n->dt, dst, base, scaled_index, stride_as_shift, 0);
    } else {
        return inst_rr(ADD, n->dt, dst, USE(scaled_index), base);
    }
}

static Inst isel_load(Ctx* restrict ctx, TB_Node* n, int dst) {
    if (n->inputs[0]->type == TB_ARRAY_ACCESS && try_tile(ctx, n->inputs[0])) {
        Inst inst = isel_array(ctx, n->inputs[0], dst);
        if (inst.type == LEA) {
            inst.type = MOV;
            return inst;
        } else {
            return inst_m(MOV, n->dt, dst, dst, GPR_NONE, SCALE_X1, 0);
        }
    } else if (n->inputs[0]->type == TB_LOCAL) {
        ptrdiff_t search = nl_map_get(ctx->stack_slots, n->inputs[0]);
        assert(search >= 0);

        return inst_m(MOV, n->dt, dst, RBP, GPR_NONE, SCALE_X1, ctx->stack_slots[search].v);
    }

    int base = ISEL(n->inputs[0]);
    return inst_m(MOV, n->dt, dst, base, GPR_NONE, SCALE_X1, 0);
}

static Cond isel_cmp(Ctx* restrict ctx, TB_Node* n) {
    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        assert(cmp_dt.width == 0 && "TODO: Implement vector compares");
        assert(!TB_IS_FLOAT_TYPE(cmp_dt) && "TODO");

        Cond cc = -1;
        bool invert = false;

        int32_t x;
        int lhs = ISEL(n->inputs[0]);
        if (try_for_imm32(ctx, n->inputs[1], &x)) {
            if (x == 0 && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
                SUBMIT(inst_rr(TEST, n->dt, -1, lhs, lhs));
            } else {
                SUBMIT(inst_ri(CMP, cmp_dt, -1, lhs, x));
            }
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

static int isel(Ctx* restrict ctx, TB_Node* n) {
    int current_val = GET_VAL(n);
    if (current_val >= 0) {
        return current_val;
    }

    try_tile(ctx, n);

    TB_NodeTypeEnum type = n->type;
    int dst = -1;

    switch (type) {
        case TB_LINE_INFO: break;

        case TB_PARAM: {
            int id = TB_NODE_GET_EXTRA_T(n, TB_NodeParam)->id;
            if (id >= 4) {
                // return val_stack(TB_TYPE_PTR, 16 + (id * 8));
                tb_todo();
            } else {
                // copy from parameter
                dst = DEF_HINTED(n, REG_CLASS_GPR, WIN64_GPR_PARAMETERS[id]);

                ctx->defs[dst].start = -100 + id;
                SUBMIT(inst_copy(n->dt, dst, WIN64_GPR_PARAMETERS[id]));
            }
            break;
        }

        case TB_LOCAL: {
            ptrdiff_t search = nl_map_get(ctx->stack_slots, n);
            if (ctx->in_fence) {
                // we don't need to resolve it if no one's using this
            } else if (search >= 0) {
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
        case TB_GET_SYMBOL_ADDRESS: {
            dst = DEF(n, REG_CLASS_GPR);

            TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n);
            SUBMIT(inst_g(LEA, n->dt, dst, s->sym));
            break;
        }

        case TB_INTEGER_CONST: {
            TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
            assert(i->num_words == 1);

            dst = DEF(n, REG_CLASS_GPR);

            uint64_t x = i->words[0];
            if (!fits_into_int32(x)) {
                // movabs reg, imm64
                SUBMIT(inst_i64(MOVABS, n->dt, dst, x));
            } else {
                SUBMIT(inst_i(MOV, n->dt, dst, x));
            }
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
            const static InstType ops[] = { AND, OR, XOR, ADD, SUB };
            InstType op = ops[type - TB_AND];

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
        case TB_MUL: {
            dst = DEF(n, REG_CLASS_GPR);

            int lhs = ISEL(n->inputs[0]);
            int rhs = ISEL(n->inputs[1]);
            SUBMIT(inst_rr(IMUL, n->dt, dst, lhs, rhs));
            break;
        }
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD: {
            bool is_signed = (type == TB_SDIV || type == TB_SMOD);
            bool is_div    = (type == TB_UDIV || type == TB_SDIV);

            int fake_dst = DEF(n, REG_CLASS_GPR);
            int rdx = DEF_FORCED(n, REG_CLASS_GPR, RDX, fake_dst);
            // if signed:
            //   cqo/cdq (sign extend RAX into RDX)
            // else:
            //   xor rdx, rdx
            if (is_signed) {
                SUBMIT(inst_u(CAST, n->dt));
            } else {
                SUBMIT(inst_i(MOV, n->dt, rdx, 0));
            }

            // TODO(NeGate): hint into RAX
            int lhs = ISEL(n->inputs[0]);
            int rhs = ISEL(n->inputs[1]);

            dst = DEF(n, REG_CLASS_GPR);

            // mov rax, lhs
            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, fake_dst);
            SUBMIT(inst_copy(n->dt, rax, lhs));

            SUBMIT(inst_r(is_signed ? IDIV : DIV, n->dt, -1, rhs));
            SUBMIT(inst_copy(n->dt, fake_dst, USE(is_div ? rax : rdx)));
            SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
            break;
        }
        case TB_SHL:
        case TB_SHR:
        case TB_SAR: {
            const static InstType ops[] = { SHL, SHR, SAR };
            InstType op = ops[type - TB_SHL];

            dst = DEF(n, REG_CLASS_GPR);

            int32_t x;
            if (try_for_imm8(ctx, n->inputs[1], &x)) {
                int lhs = ISEL(n->inputs[0]);
                SUBMIT(inst_ri(op, n->dt, dst, lhs, x));
            } else {
                int fake_dst = DEF(n, REG_CLASS_GPR);

                // the shift operations need their right hand side in CL (RCX's low 8bit)
                int lhs = ISEL(n->inputs[0]);
                int rhs = ISEL(n->inputs[1]); // TODO(NeGate): hint into RCX

                int cl = DEF_FORCED(n, REG_CLASS_GPR, RCX, fake_dst);
                SUBMIT(inst_copy(n->dt, cl, rhs));
                SUBMIT(inst_rr(op, n->dt, fake_dst, lhs, RCX));
                SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
            }
            break;
        }

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE: {
            dst = DEF(n, REG_CLASS_GPR);
            SUBMIT(inst_i(MOV, n->dt, dst, 0));

            // use SETcc to convert into integer
            Cond cc = isel_cmp(ctx, n);
            SUBMIT(inst_setcc(cc, USE(dst)));
            break;
        }

        case TB_VA_START: {
            assert(ctx->module->target_abi == TB_ABI_WIN64 && "How does va_start even work on SysV?");

            // on Win64 va_start just means whatever is one parameter away from
            // the parameter you give it (plus in Win64 the parameters in the stack
            // are 8bytes, no fanciness like in SysV):
            // void printf(const char* fmt, ...) {
            //     va_list args;
            //     va_start(args, fmt); // args = ((char*) &fmt) + 8;
            //     ...
            // }
            dst = DEF(n, REG_CLASS_GPR);
            int src = ISEL(n->inputs[0]);

            SUBMIT(inst_ri(ADD, n->dt, dst, src, 8));
            break;
        }
        case TB_ARRAY_ACCESS: {
            dst = DEF(n, REG_CLASS_GPR);

            Inst inst = isel_array(ctx, n, dst);
            SUBMIT(inst);
            break;
        }

        case TB_RET: {
            if (n->inputs[0] != NULL) {
                // hint input to be RAX
                int src_vreg = isel(ctx, n->inputs[0]);
                if (ctx->defs[src_vreg].hint < 0) {
                    ctx->defs[src_vreg].hint = RAX;
                }

                // we ain't gotta worry about regalloc here, we dippin
                int fake_dst = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
                SUBMIT(inst_copy(n->inputs[0]->dt, fake_dst, USE(src_vreg)));
            }

            if (ctx->fallthrough != -1) {
                SUBMIT(inst_jmp(-1));
            }
            break;
        }

        case TB_LOAD: {
            dst = DEF(n, REG_CLASS_GPR);

            Inst ld = isel_load(ctx, n, dst);
            SUBMIT(ld);
            break;
        }
        case TB_STORE: {
            int src = ISEL(n->inputs[1]);

            Inst inst;
            if (n->inputs[0]->type == TB_LOCAL) {
                ptrdiff_t search = nl_map_get(ctx->stack_slots, n->inputs[0]);
                assert(search >= 0);

                SUBMIT(inst_mr(MOV, n->dt, RBP, GPR_NONE, SCALE_X1, ctx->stack_slots[search].v, src));
            } else {
                int addr = ISEL(n->inputs[0]);
                SUBMIT(inst_mr(MOV, n->dt, addr, GPR_NONE, SCALE_X1, 0, src));
            }
            break;
        }

        case TB_INT2PTR:
        case TB_SIGN_EXT:
        case TB_ZERO_EXT: {
            TB_Node* src = n->inputs[0];

            TB_DataType src_dt = src->dt;
            bool sign_ext = (type == TB_SIGN_EXT);
            int bits_in_type = src_dt.type == TB_PTR ? 64 : src_dt.data;

            dst = DEF(n, REG_CLASS_GPR);

            TB_NodeInt* i = TB_NODE_GET_EXTRA(src);
            if (i->num_words == 1 && fits_into_int32(i->words[0])) {
                #define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
                try_tile(ctx, src);

                uint64_t src = i->words[0];
                uint64_t sign_bit = (src >> (bits_in_type - 1)) & 1;
                uint64_t mask = MASK_UPTO(64) & ~MASK_UPTO(bits_in_type);

                src = (src & ~mask) | (sign_bit ? mask : 0);
                if (!fits_into_int32(src)) {
                    // movabs reg, imm64
                    SUBMIT(inst_i64(MOVABS, n->dt, dst, src));
                } else {
                    SUBMIT(inst_i(MOV, n->dt, dst, src));
                }

                return dst;
                #undef MASK_UPTO
            }

            int op = MOV;
            switch (bits_in_type) {
                case 64: op = MOV; break;
                case 32: op = sign_ext ? MOVSXD : MOV; break;
                case 16: op = sign_ext ? MOVSXW : MOVZXW; break;
                case 8:  op = sign_ext ? MOVSXB : MOVZXB; break;
                default: tb_todo();
            }

            if (src->type == TB_LOAD && try_tile(ctx, src)) {
                Inst inst = isel_load(ctx, src, dst);
                inst.type = op;
                SUBMIT(inst);
            } else {
                int src = ISEL(n->inputs[0]);
                SUBMIT(inst_r(op, n->dt, dst, src));
            }
            break;
        }
        case TB_PTR2INT:
        case TB_TRUNCATE: {
            int src = ISEL(n->inputs[0]);
            dst = DEF(n, REG_CLASS_GPR);

            SUBMIT(inst_copy(n->dt, dst, src));
            break;
        }

        case TB_BRANCH: {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

            if (br->count == 0) {
                fence(ctx);

                if (ctx->fallthrough != br->default_label) {
                    SUBMIT(inst_jmp(br->default_label));
                }
            } else if (br->count == 1) {
                // if-like branch
                Cond cc = isel_cmp(ctx, n->inputs[0]);

                fence(ctx);

                SUBMIT(inst_jcc(br->targets[0].value, cc));
                if (ctx->fallthrough != br->default_label) {
                    SUBMIT(inst_jmp(br->default_label));
                }
            } else {
                int key = USE(ISEL(n->inputs[0]));

                fence(ctx);

                FOREACH_N(i, 0, br->count) {
                    SUBMIT(inst_i(CMP, n->dt, key, br->targets[i].key));
                    SUBMIT(inst_jcc(br->targets[i].value, E));
                }
                SUBMIT(inst_jmp(br->default_label));

                // switch-like branch
                // tb_todo();
            }
            break;
        }

        case TB_SCALL:
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
            int fake_dst = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
            uint32_t caller_saved_gprs = desc->caller_saved_gprs;

            FOREACH_N(i, 1, n->input_count) {
                TB_Node* param = n->inputs[i];
                TB_DataType param_dt = param->dt;

                if (TB_IS_FLOAT_TYPE(param_dt) || param_dt.width) {
                    tb_todo();
                } else {
                    int src = isel(ctx, param);

                    if (i - 1 < desc->gpr_count) {
                        // hint src into GPR
                        GPR target_gpr = desc->gprs[i - 1];
                        if (ctx->defs[src].hint < 0) {
                            ctx->defs[src].hint = target_gpr;
                        }

                        int param_def = DEF_FORCED(param, REG_CLASS_GPR, target_gpr, dst);
                        SUBMIT(inst_copy(param->dt, param_def, USE(src)));

                        caller_saved_gprs &= ~(1u << target_gpr);
                    } else {
                        SUBMIT(inst_mr(MOV, param->dt, RSP, GPR_NONE, SCALE_X1, (i - 1) * 8, USE(src)));
                    }
                }
            }

            size_t clobber_cap = tb_popcount(caller_saved_gprs);
            Clobbers* clobbers = arena_alloc(&tb__arena, sizeof(Clobbers) + (clobber_cap * sizeof(MachineReg)), _Alignof(Clobbers));

            // mark all the clobbers
            size_t clobber_count = 0;
            FOREACH_N(i, 0, 16) if (caller_saved_gprs & (1u << i)) {
                clobbers->_[clobber_count++] = (MachineReg){ REG_CLASS_GPR, i };
            }

            assert(clobber_count == clobber_cap);
            clobbers->count = clobber_count;
            ctx->defs[fake_dst].clobbers = clobbers;

            if (type == TB_SCALL) {
                int num = ISEL(n->inputs[0]);
                SUBMIT(inst_copy(n->dt, fake_dst, num));
                SUBMIT(inst_nullary(SYSCALL));
            } else {
                if (try_tile(ctx, n->inputs[0]) && n->inputs[0]->type == TB_GET_SYMBOL_ADDRESS) {
                    TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n->inputs[0]);
                    SUBMIT(inst_call(n->dt, -1, s->sym));
                } else {
                    __debugbreak();
                }
            }

            SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
            break;
        }

        case TB_PHI:
        dst = DEF(n, classify_reg_class(n->dt));
        break;

        // x86 intrinsics
        case TB_DEBUGBREAK: {
            SUBMIT(inst_nullary(INT3));
            break;
        }

        /*case TB_X86INTRIN_RDTSC: {
            SUBMIT(inst_nullary(RDTSC));
            break;
        }*/

        case TB_NULL: break;
        default: tb_todo();
    }

    GET_VAL(n) = dst;
    return dst;
}

static void abi_prepare(Ctx* restrict ctx, TB_Function* f) {
    bool has_param_slots = false;
    TB_FOR_NODE(n, f, 0) if (n->type == TB_STORE) {
        // handle parameter storage, the first few parameters have reserved space for them
        // in Win64.
        if (n->inputs[0]->type == TB_LOCAL && n->inputs[1]->type == TB_PARAM && get_meta(ctx, n->inputs[1])) {
            TB_NodeParam* p = TB_NODE_GET_EXTRA(n->inputs[1]);

            int pos = 16 + (p->id * 8);
            nl_map_put(ctx->stack_slots, n->inputs[0], pos);

            has_param_slots = true;
        }
    }

    const TB_FunctionPrototype* restrict proto = f->prototype;
    if (has_param_slots) {
        ctx->stack_usage += 16 + (proto->param_count * 8);
    }

    if (proto->has_varargs) {
        bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
        const GPR* parameter_gprs = is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;

        // spill the rest of the parameters (assumes they're all in the GPRs)
        size_t gpr_count = is_sysv ? 6 : 4;
        size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;

        FOREACH_N(i, 0, extra_param_count) {
            size_t param_num = proto->param_count + i;

            int dst_pos = 16 + (param_num * 8);
            GPR src = parameter_gprs[param_num];

            SUBMIT(inst_mr(MOV, TB_TYPE_I64, RBP, GPR_NONE, SCALE_X1, dst_pos, src));
        }

        ctx->stack_usage += (extra_param_count * 8);
    }
}

static void copy_value(Ctx* restrict ctx, TB_Node* phi, int dst, TB_Node* src, TB_DataType dt) {
    if (src->type == TB_ADD && try_tile(ctx, src)) {
        if (src->inputs[0] == phi && try_tile(ctx, src->inputs[1])) {
            int32_t x;
            if (try_for_imm32(ctx, src->inputs[1], &x)) {
                SUBMIT(inst_ri(ADD, dt, dst, USE(dst), x));
            } else {
                int other = ISEL(src->inputs[1]);
                SUBMIT(inst_rr(ADD, dt, dst, USE(dst), other));
            }
            return;
        }
    }

    int src_v = ISEL(src);
    SUBMIT(inst_move(dt, dst, src_v));
}

static void spill(Ctx* restrict ctx, Inst* basepoint, Reload* r) {
    // allocate stack slot
    if (r->stack_pos == 0) {
        r->stack_pos = STACK_ALLOC(8, 8);
    }

    // write out
    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);

    *new_inst = inst_mr(MOV, r->dt, RBP, GPR_NONE, SCALE_X1, r->stack_pos, USE(r->old));
    new_inst->time = basepoint->time + 1;
    new_inst->next = basepoint->next;
    basepoint->next = new_inst;
}

static void reload(Ctx* restrict ctx, Inst* basepoint, Reload* r) {
    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);

    *new_inst = inst_m(MOV, r->dt, r->old, RBP, GPR_NONE, SCALE_X1, r->stack_pos);
    new_inst->time = basepoint->time + 1;
    new_inst->next = basepoint->next;
    basepoint->next = new_inst;
}

static int8_t resolve_def(Ctx* restrict ctx, int x) {
    return ctx->defs[x].reg;
}

static int8_t resolve_use(Ctx* restrict ctx, int x) {
    if (x < -1) return ctx->defs[-x - 2].reg;
    return x;
}

static void inst2_print(Ctx* restrict ctx, InstType type, Val* dst, Val* src, X86_DataType dt) {
    ASM {
        printf("  %s ", inst_table[type].mnemonic);
        print_operand(dst);
        printf(", ");
        print_operand(src);
        printf("\n");
    }
    INST2(type, dst, src, dt);
}

static void inst1_print(Ctx* restrict ctx, int type, Val* src, X86_DataType dt) {
    ASM {
        printf("  %s ", inst_table[type].mnemonic);
        print_operand(src);
        printf("\n");
    }
    INST1(type, src, dt);
}

static void emit_code(Ctx* restrict ctx) {
    /*if (n->type == TB_LINE_INFO) {
        TB_NodeLine* l = TB_NODE_GET_EXTRA(n);
        f->lines[f->line_count++] = (TB_Line) {
            .file = l->file,
            .line = l->line,
            .pos = GET_CODE_POS(&ctx->emit)
        };
        continue;
    }*/

    Val ops[4];
    for (Inst* restrict inst = ctx->first; inst; inst = inst->next) {
        if (inst->type == INST_LABEL) {
            TB_Label bb = inst->imm[0];
            ctx->emit.labels[bb] = GET_CODE_POS(&ctx->emit);

            ASM printf("L%d:\n", bb);
            continue;
        }

        bool has_def = false;
        if (inst->regs[0] >= 0) {
            ops[0] = val_gpr(resolve_def(ctx, inst->regs[0]));
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
            case X86_OP_NONE: {
                op_count = 0;
                break;
            }
            case X86_OP_R: {
                ops[1] = val_gpr(regs[1]);
                op_count = has_def ? 2 : 1;
                break;
            }
            case X86_OP_I: {
                ops[1] = val_imm(inst->imm[0]);
                op_count = 2;
                break;
            }
            case X86_OP_A: {
                ops[1] = val_abs((inst->imm[1] << 32) | inst->imm[0]);
                op_count = 2;
                break;
            }
            case X86_OP_G: {
                ops[1] = val_global((TB_Symbol*) (uintptr_t) inst->imm[0]);
                op_count = has_def ? 2 : 1;
                break;
            }
            case X86_OP_L: {
                ops[1] = val_label(inst->imm[0]);
                op_count = 1;
                break;
            }
            case X86_OP_RR: {
                ops[1] = val_gpr(regs[1]);
                ops[2] = val_gpr(regs[2]);
                op_count = 3;
                break;
            }
            case X86_OP_RI: {
                ops[1] = val_gpr(regs[1]);
                ops[2] = val_imm(inst->imm[0]);
                op_count = 3;
                break;
            }
            case X86_OP_M: {
                Scale scale = (inst->imm[0] >> 32);
                int32_t disp = inst->imm[0] & 0xFFFFFFFF;

                ops[1] = val_base_index_disp(regs[2], regs[3], scale, disp);
                op_count = 2;
                break;
            }
            case X86_OP_MR: {
                Scale scale = (inst->imm[0] >> 32);
                int32_t disp = inst->imm[0] & 0xFFFFFFFF;

                ops[1] = val_base_index_disp(regs[2], regs[3], scale, disp);
                ops[2] = val_gpr(regs[1]);
                op_count = 3;
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
        } else if (op_count == 0) {
            INST0(inst->type, inst->data_type);
            printf("  %s\n", inst_table[inst->type].mnemonic);
        } else if (op_count == 1) {
            if (!has_def) {
                inst1_print(ctx, inst->type, &ops[1], inst->data_type);
            } else if (!is_value_match(&ops[0], &ops[1])) {
                inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
            } else {
                inst1_print(ctx, inst->type, &ops[0], inst->data_type);
            }
        } else if (op_count == 2) {
            if (inst->type != MOV || (inst->type == MOV && !is_value_match(&ops[0], &ops[1]))) {
                inst2_print(ctx, (InstType) inst->type, &ops[0], &ops[1], inst->data_type);
            }
        } else if (op_count == 3) {
            if (!has_def) {
                inst2_print(ctx, (InstType) inst->type, &ops[1], &ops[2], inst->data_type);
            } else {
                if (ops[0].type == VAL_GPR && ops[1].type == VAL_GPR && ops[0].reg != ops[1].reg) {
                    inst2_print(ctx, MOV, &ops[0], &ops[1], inst->data_type);
                }
                inst2_print(ctx, (InstType) inst->type, &ops[0], &ops[2], inst->data_type);
            }
        } else {
            tb_todo();
        }
    }
}

static void resolve_stack_usage(Ctx* restrict ctx, size_t caller_usage) {
    size_t usage = ctx->stack_usage + (caller_usage * 8);

    // Align stack usage to 16bytes and add 8 bytes for the return address
    ctx->stack_usage = align_up(usage, 16) + 8;
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

static void emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t saved, uint64_t stack_usage) {
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

static size_t emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
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

static size_t emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
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

static size_t emit_call_patches(TB_Module* restrict m) {
    size_t r = 0;
    TB_FOR_FUNCTIONS(f, m) {
        for (TB_SymbolPatch* patch = f->last_patch; patch; patch = patch->prev) {
            if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                // you can't do relocations across COMDAT sections
                if (&patch->source->super == patch->target || (!tb_symbol_is_comdat(&patch->source->super) && !tb_symbol_is_comdat(patch->target))) {
                    TB_FunctionOutput* out_f = patch->source->output;
                    assert(out_f && "Patch cannot be applied to function with no compiled output");

                    // x64 thinks of relative addresses as being relative
                    // to the end of the instruction or in this case just
                    // 4 bytes ahead hence the +4.
                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                    uint32_t p = ((TB_Function*) patch->target)->output->code_pos - actual_pos;
                    memcpy(&out_f->code[out_f->prologue_length + patch->pos], &p, sizeof(uint32_t));

                    r += 1;
                    patch->internal = true;
                }
            }
        }

        m->text.reloc_count += f->patch_count;
    }

    return r;
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,
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
