#include "../x64/x64.h"
#include "../x64/x64_emitter.h"
#include "../objects/win64eh.h"

#include "x64_disasm.c"

enum {
    CG_REGISTER_CLASSES = 2
};

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM
};

typedef int X86_InstType;

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

typedef enum {
    INST_LOCK   = 1,
    INST_REP    = 2,
    INST_REPNE  = 4,
} InstPrefix;

typedef struct Inst Inst;
struct Inst {
    Inst* next;

    // prefixes
    InstPrefix prefix : 16;
    InstType type : 16;

    X86_OperandLayout layout;
    TB_X86_DataType data_type;
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
static TB_X86_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_INT || dt.type == TB_PTR);
    if (dt.type == TB_PTR) return *out_mask = 0, TB_X86_TYPE_QWORD;

    TB_X86_DataType t = TB_X86_TYPE_NONE;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = TB_X86_TYPE_BYTE;
    else if (dt.data <= 16) bits = 16, t = TB_X86_TYPE_WORD;
    else if (dt.data <= 32) bits = 32, t = TB_X86_TYPE_DWORD;
    else if (dt.data <= 64) bits = 64, t = TB_X86_TYPE_QWORD;

    assert(bits != 0 && "TODO: large int support");
    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

static TB_X86_DataType legalize_int2(TB_DataType dt) {
    uint64_t m;
    return legalize_int(dt, &m);
}

static TB_X86_DataType legalize_float(TB_DataType dt) {
    assert(dt.type == TB_FLOAT);
    TB_X86_DataType t = (dt.data == TB_FLT_64 ? TB_X86_TYPE_SSE_SD : TB_X86_TYPE_SSE_SS);

    if (dt.data == TB_FLT_64) {
        assert(dt.width == 0 || dt.width == 1);
    } else if (dt.data == TB_FLT_32) {
        assert(dt.width == 0 || dt.width == 2);
    } else {
        tb_unreachable();
    }

    return t + (dt.width ? 2 : 0);
}

static TB_X86_DataType legalize(TB_DataType dt) {
    if (dt.type == TB_FLOAT) {
        return legalize_float(dt);
    } else {
        uint64_t m;
        return legalize_int(dt, &m);
    }
}

static bool wont_spill_around(int t) {
    return t == TEST || t == CMP || t == JMP || (t >= JO && t <= JG);
}

static Inst inst_jcc(TB_Node* target, Cond cc) {
    return (Inst){
        .type = JO + cc,
        .layout = X86_OP_L,
        .regs   = { -1 },
        .imm = { (uintptr_t) target }
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

static Inst inst_jmp(TB_Node* target) {
    return (Inst){
        .type = JMP,
        .layout = X86_OP_L,
        .regs   = { -1 },
        .imm[0] = (uintptr_t) target
    };
}

static Inst inst_u(int op, TB_DataType dt) {
    return (Inst){
        .type = op,
        .layout = X86_OP_NONE,
        .data_type = legalize(dt),
        .regs = { -1 },
    };
}

static Inst inst_use(int src) {
    return (Inst){
        .type = INST_USE,
        .layout = X86_OP_NONE,
        .data_type = TB_X86_TYPE_NONE,
        .regs = { src },
    };
}

static Inst inst_nullary(int op) {
    return (Inst){
        .type = op,
        .layout = X86_OP_NONE,
        .data_type = TB_X86_TYPE_NONE,
        .regs = { -1 },
    };
}

static Inst inst_call(TB_DataType dt, int dst, const TB_Symbol* sym) {
    return (Inst){
        .type = CALL,
        .layout = X86_OP_G,
        .data_type = legalize(dt),
        .regs = { dst },
        .imm[0] = (uintptr_t) sym,
    };
}

static Inst inst_g(int op, TB_DataType dt, int dst, const TB_Symbol* sym) {
    return (Inst){
        .type = op,
        .layout = X86_OP_G,
        .data_type = legalize(dt),
        .regs = { dst },
        .imm[0] = (uintptr_t) sym,
    };
}

static Inst inst_move(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = (int)INST_MOVE,
        .layout = X86_OP_RR,
        .data_type = legalize(dt),
        .regs = { -1, lhs, rhs }
    };
}

static Inst inst_copy(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = INST_COPY,
        .layout = X86_OP_RR,
        .data_type = legalize(dt),
        .regs = { lhs, rhs }
    };
}

static Inst inst_rr(int op, TB_DataType dt, int dst, int lhs, int rhs) {
    assert(lhs != -1);
    return (Inst){
        .type = op,
        .layout = X86_OP_RR,
        .data_type = legalize(dt),
        .regs = { dst, lhs, rhs }
    };
}

static Inst inst_ri(int op, TB_DataType dt, int dst, int lhs, int32_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_RI,
        .data_type = legalize(dt),
        .regs = { dst, lhs },
        .imm[0] = imm,
    };
}

static Inst inst_r(int op, TB_DataType dt, int dst, int src) {
    return (Inst){
        .type = op,
        .layout = X86_OP_R,
        .data_type = legalize(dt),
        .regs = { dst, src },
    };
}

static Inst inst_i(int op, TB_DataType dt, int dst, int32_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_I,
        .data_type = legalize(dt),
        .regs = { dst },
        .imm[0] = imm,
    };
}

static Inst inst_i64(int op, TB_DataType dt, int dst, uint64_t imm) {
    return (Inst){
        .type = op,
        .layout = X86_OP_A,
        .data_type = legalize(dt),
        .regs = { dst },
        .imm = { imm & 0xFFFFFFFF, imm >> 32ull }
    };
}

static Inst inst_m(int op, TB_DataType dt, int dst, int base, int index, Scale scale, int32_t disp) {
    return (Inst){
        .type = op,
        .layout = X86_OP_M,
        .data_type = legalize(dt),
        .regs = { dst, 0, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static Inst inst_mr(int op, TB_DataType dt, int base, int index, Scale scale, int32_t disp, int rhs) {
    return (Inst){
        .type = op,
        .layout = X86_OP_MR,
        .data_type = legalize(dt),
        .regs = { -1, rhs, base, index },
        .imm[0] = ((uint64_t) scale << 32u) | ((uint64_t) disp)
    };
}

static Inst inst_mi(int op, TB_DataType dt, int base, int index, Scale scale, int32_t disp, int32_t rhs) {
    return (Inst){
        .type = op,
        .layout = X86_OP_MI,
        .data_type = legalize(dt),
        .regs = { -1, -1, base, index },
        .imm = { ((uint64_t) scale << 32u) | ((uint64_t) disp), rhs },
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
        case VAL_ABS: printf("%#"PRIx64, v->abs); break;
        case VAL_MEM: {
            if (v->index == -1) {
                printf("[%s", GPR_NAMES[v->reg]);
            } else {
                printf("[%s + %s*%d", GPR_NAMES[v->reg], GPR_NAMES[v->index], 1u << v->scale);
            }

            if (v->imm != 0) {
                printf(" + %d", v->imm);
            }
            printf("]");
            break;
        }
        case VAL_GLOBAL: {
            const TB_Symbol* target = v->symbol;
            if (target->name == NULL) {
                if (v->imm == 0) {
                    printf("sym%p", target);
                } else {
                    printf("[sym%p + %d]", target, v->imm);
                }
            } else {
                if (v->imm == 0) {
                    printf("%s", target->name);
                } else {
                    printf("[%s + %d]", target->name, v->imm);
                }
            }
            break;
        }
        case VAL_LABEL: {
            if (v->target == NULL) printf(".ret");
            else printf("L%p", (TB_Node*) v->target);
            break;
        }
        default: tb_todo();
    }
}

static bool try_for_imm8(Ctx* restrict ctx, TB_Node* n, int32_t* out_x) {
    if (n->type == TB_INTEGER_CONST) {
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
        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        if (i->num_words == 1 && fits_into_int32(i->words[0])) {
            *out_x = i->words[0];
            return true;
        }
    }

    return false;
}

static int get_stack_slot(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->stack_slots, n);
    if (search >= 0) {
        return ctx->stack_slots[search].v;
    } else {
        TB_NodeLocal* local = TB_NODE_GET_EXTRA(n);

        int pos = STACK_ALLOC(local->size, local->align);
        nl_map_put(ctx->stack_slots, n, pos);

        add_debug_local(ctx, n, pos);
        return pos;
    }
}

static Inst isel_array(Ctx* restrict ctx, TB_Node* n, int dst) {
    int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;
    TB_Node* base_n = n->inputs[0];
    TB_Node* index_n = n->inputs[1];

    if (index_n->type == TB_INTEGER_CONST) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(index_n);
        if (i->num_words == 1 && fits_into_int32(i->words[0] * stride)) {
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
    if (stride == 1) {
        scaled_index = ISEL(index_n);
    } else if (tb_is_power_of_two(stride)) {
        stride_as_shift = tb_ffs(stride) - 1;

        if (stride_as_shift > 3) {
            scaled_already = true;

            int index = ISEL(index_n);
            SUBMIT(inst_ri(SHL, n->dt, scaled_index, index, stride_as_shift));
        } else {
            scaled_index = ISEL(index_n);
        }
    } else {
        int imm = DEF(n, REG_CLASS_GPR);
        SUBMIT(inst_i(MOV, n->dt, imm, stride));

        int index = ISEL(index_n);
        int mul_index = DEF(n, REG_CLASS_GPR);
        SUBMIT(inst_rr(IMUL, n->dt, mul_index, index, USE(imm)));

        scaled_index = USE(mul_index);
    }

    int base = ISEL(base_n);
    if (!scaled_already) {
        return inst_m(LEA, n->dt, dst, base, scaled_index, stride_as_shift, 0);
    } else {
        return inst_rr(ADD, n->dt, dst, USE(scaled_index), base);
    }
}

static Inst isel_load(Ctx* restrict ctx, TB_Node* n, int dst) {
    InstType i = n->dt.type == TB_FLOAT ? FP_MOV : MOV;
    TB_Node* addr = n->inputs[1];

    if (addr->type == TB_ARRAY_ACCESS) {
        Inst inst = isel_array(ctx, addr, dst);
        if (inst.type == LEA) {
            inst.type = i;
            inst.data_type = legalize(n->dt);
            return inst;
        } else {
            return inst_m(i, n->dt, dst, dst, GPR_NONE, SCALE_X1, 0);
        }
    } else if (addr->type == TB_LOCAL) {
        int pos = get_stack_slot(ctx, addr);
        return inst_m(i, n->dt, dst, RBP, GPR_NONE, SCALE_X1, pos);
    }

    int base = ISEL(addr);
    return inst_m(i, n->dt, dst, base, GPR_NONE, SCALE_X1, 0);
}

static Inst isel_store(Ctx* restrict ctx, TB_DataType dt, TB_Node* addr, int src) {
    InstType i = dt.type == TB_FLOAT ? FP_MOV : MOV;

    if (addr->type == TB_LOCAL) {
        int pos = get_stack_slot(ctx, addr);
        return inst_mr(i, dt, RBP, GPR_NONE, SCALE_X1, pos, src);
    } else if (addr->type == TB_MEMBER_ACCESS) {
        Inst inst = isel_store(ctx, dt, addr->inputs[0], src);
        inst.imm[0] += TB_NODE_GET_EXTRA_T(addr, TB_NodeMember)->offset;

        return inst;
    }

    int base = ISEL(addr);
    return inst_mr(i, dt, base, GPR_NONE, SCALE_X1, 0, src);
}

static Cond isel_cmp(Ctx* restrict ctx, TB_Node* n) {
    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        assert(cmp_dt.width == 0 && "TODO: Implement vector compares");

        Cond cc = -1;

        if (TB_IS_FLOAT_TYPE(cmp_dt)) {
            int lhs = ISEL(n->inputs[0]);
            int rhs = ISEL(n->inputs[1]);
            SUBMIT(inst_rr(FP_UCOMI, cmp_dt, -1, lhs, rhs));

            switch (n->type) {
                case TB_CMP_EQ:  cc = E; break;
                case TB_CMP_NE:  cc = NE; break;
                case TB_CMP_FLT: cc = B; break;
                case TB_CMP_FLE: cc = BE; break;
                default: tb_unreachable();
            }
            return cc;
        } else {
            bool invert = false;
            int32_t x;
            int lhs = ISEL(n->inputs[0]);
            if (try_for_imm32(ctx, n->inputs[1], &x)) {
                if (x == 0 && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
                    SUBMIT(inst_rr(TEST, cmp_dt, -1, lhs, lhs));
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
    }

    int src = ISEL(n);
    TB_DataType dt = n->dt;
    if (TB_IS_FLOAT_TYPE(dt)) {
        int tmp = DEF(n, REG_CLASS_XMM);

        Inst inst = {
            .type = FP_XOR,
            .layout = X86_OP_RR,
            .data_type = TB_X86_TYPE_SSE_PS,
            .regs = { tmp, USE(tmp), USE(tmp) }
        };
        SUBMIT(inst);
        SUBMIT(inst_rr(FP_UCOMI, dt, -1, src, tmp));
        return NE;
    } else {
        SUBMIT(inst_rr(TEST, dt, -1, src, src));
        return NE;
    }
}

static void finna_use_reg(Ctx* restrict ctx, int reg_class, int reg_num) {
    // mark register as to be saved
    if (reg_class == REG_CLASS_GPR) {
        bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
        ctx->regs_to_save |= (1u << reg_num) & (is_sysv ? SYSV_ABI_CALLEE_SAVED : WIN64_ABI_CALLEE_SAVED);
    }
}

static int isel(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->values, n);
    if (search >= 0) {
        return ctx->values[search].v;
    }

    TB_NodeTypeEnum type = n->type;
    int dst = -1;

    switch (type) {
        case TB_REGION: {
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

            // allocate all the region parameters
            FOREACH_N(i, 0, r->proj_count) {
                TB_Node* proj = r->projs[i];

                ptrdiff_t search = nl_map_get(ctx->values, proj);
                if (search < 0) {
                    int param = DEF(proj, classify_reg_class(proj->dt));
                    nl_map_put(ctx->values, proj, param);
                }
            }
            break;
        }
        case TB_START: {
            TB_NodeRegion* start = TB_NODE_GET_EXTRA(n);
            const TB_FunctionPrototype* restrict proto = ctx->f->prototype;
            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);

            // Handle known parameters
            FOREACH_N(i, 0, proto->param_count) {
                TB_Node* proj = start->projs[i];

                // copy from parameter
                int reg_class = (proj->dt.type == TB_FLOAT ? REG_CLASS_XMM : REG_CLASS_GPR);
                int v = -1;

                if (is_sysv) {
                    tb_todo();
                } else {
                    if (i < 4) {
                        int reg_num = (proj->dt.type == TB_FLOAT ? i : WIN64_GPR_PARAMETERS[i]);

                        v = DEF_HINTED(proj, reg_class, reg_num);
                        ctx->defs[v].start = -100 + i;
                        SUBMIT(inst_copy(proj->dt, v, reg_num));

                        nl_map_put(ctx->values, proj, v);
                    }
                }
            }

            // walk the entry to find any parameter stack slots
            bool has_param_slots = false;
            TB_Node* curr = start->end;
            while (curr->type != TB_START) {
                if (curr->type == TB_STORE) {
                    // handle parameter storage, the first few parameters
                    // have reserved space for them in Win64.
                    if (curr->inputs[1]->type == TB_LOCAL &&
                        curr->inputs[2]->type == TB_PROJ &&
                        curr->inputs[2]->inputs[0]->type == TB_START) {
                        TB_NodeProj* p = TB_NODE_GET_EXTRA(curr->inputs[2]);

                        int pos = 16 + (p->index * 8);
                        nl_map_put(ctx->stack_slots, curr->inputs[1], pos);

                        if (p->index >= 4 && ctx->target_abi == TB_ABI_WIN64) {
                            nl_map_put(ctx->values, curr, -1); // marks as visited (stores don't return so we can -1)
                        }

                        // add parameter to debug info
                        add_debug_local(ctx, curr->inputs[1], pos);

                        has_param_slots = true;
                    }
                }

                // previous in control
                curr = curr->inputs[0];
            }

            if (has_param_slots) {
                ctx->stack_usage += 16 + (proto->param_count * 8);
            } else {
                ctx->stack_usage += 16;
            }

            // Handle unknown parameters (if we have varargs)
            if (proto->has_varargs) {
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
            return 0;
        }

        case TB_LOCAL: {
            int pos = get_stack_slot(ctx, n);

            dst = DEF(n, REG_CLASS_GPR);
            SUBMIT(inst_m(LEA, n->dt, dst, RBP, GPR_NONE, SCALE_X1, pos));
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

            // mask off bits
            uint64_t bits_in_type = n->dt.type == TB_PTR ? 64 : n->dt.data;
            if (bits_in_type < 64) {
                x &= (1ull << bits_in_type) - 1;
            }

            if (!fits_into_int32(x)) {
                // movabs reg, imm64
                SUBMIT(inst_i64(MOVABS, n->dt, dst, x));
            } else {
                SUBMIT(inst_i(MOV, n->dt, dst, x));
            }
            break;
        }
        case TB_FLOAT32_CONST: {
            assert(n->dt.type == TB_FLOAT && n->dt.width == 0);
            uint32_t imm = (Cvt_F32U32) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value }.i;

            dst = DEF(n, REG_CLASS_XMM);
            if (imm == 0) {
                // xor
                Inst inst = {
                    .type = FP_XOR,
                    .layout = X86_OP_RR,
                    .data_type = TB_X86_TYPE_SSE_PS,
                    .regs = { dst, USE(dst), USE(dst) }
                };
                SUBMIT(inst);
            } else {
                TB_Module* mod = ctx->module;
                TB_Global* g = tb_global_create(mod, NULL, NULL, TB_LINKAGE_PRIVATE);
                tb_global_set_storage(mod, &mod->rdata, g, sizeof(float), sizeof(float), 1);

                char* buffer = tb_global_add_region(mod, g, 0, sizeof(float));
                memcpy(buffer, &imm, sizeof(float));

                SUBMIT(inst_g(FP_MOV, n->dt, dst, (TB_Symbol*) g));
            }
            break;
        }
        case TB_FLOAT64_CONST: {
            assert(n->dt.type == TB_FLOAT && n->dt.width == 0);
            uint64_t imm = (Cvt_F64U64){ .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value }.i;

            dst = DEF(n, REG_CLASS_XMM);
            if (imm == 0) {
                // xor
                Inst inst = {
                    .type = FP_XOR,
                    .layout = X86_OP_RR,
                    .data_type = TB_X86_TYPE_SSE_PS,
                    .regs = { dst, USE(dst), USE(dst) }
                };
                SUBMIT(inst);
            } else {
                TB_Module* mod = ctx->module;
                TB_Global* g = tb_global_create(mod, NULL, NULL, TB_LINKAGE_PRIVATE);
                tb_global_set_storage(mod, &mod->rdata, g, sizeof(double), sizeof(double), 1);

                char* buffer = tb_global_add_region(mod, g, 0, sizeof(double));
                memcpy(buffer, &imm, sizeof(double));

                SUBMIT(inst_g(FP_MOV, n->dt, dst, (TB_Symbol*) g));
            }
            break;
        }
        case TB_FLOAT_EXT: {
            dst = DEF(n, REG_CLASS_XMM);

            int src = ISEL(n->inputs[0]);
            SUBMIT(inst_r(FP_CVT, n->inputs[0]->dt, dst, src));
            break;
        }

        case TB_NEG:
        case TB_NOT: {
            assert(n->dt.type != TB_FLOAT);
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
        case TB_MULPAIR: {
            dst = DEF(n, REG_CLASS_GPR);

            int fake_dst = DEF(n, REG_CLASS_GPR);

            // mov rax, lhs
            int lhs = ISEL(n->inputs[0]);
            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, fake_dst);
            SUBMIT(inst_copy(n->dt, rax, lhs));

            int rhs = ISEL(n->inputs[1]);
            int rdx = DEF_FORCED(n, REG_CLASS_GPR, RDX, fake_dst);
            SUBMIT(inst_use(rdx));

            SUBMIT(inst_r(MUL, n->dt, -1, rhs));
            SUBMIT(inst_copy(n->dt, fake_dst, USE(rax))); // use fake_dst

            // returns into both lo and hi
            TB_NodeMulPair* p = TB_NODE_GET_EXTRA(n);

            int lo = DEF(n, REG_CLASS_GPR);
            int hi = DEF(n, REG_CLASS_GPR);
            nl_map_put(ctx->values, p->lo, lo);
            nl_map_put(ctx->values, p->hi, hi);

            SUBMIT(inst_copy(n->dt, lo, USE(rdx)));      // hi
            SUBMIT(inst_copy(n->dt, hi, USE(fake_dst))); // lo
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

            // TODO(NeGate): hint into RAX
            SUBMIT(inst_use(rdx));

            // mov rax, lhs
            int lhs = ISEL(n->inputs[0]);
            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, fake_dst);
            SUBMIT(inst_copy(n->dt, rax, lhs));

            int rhs = ISEL(n->inputs[1]);

            // if signed:
            //   cqo/cdq (sign extend RAX into RDX)
            // else:
            //   xor rdx, rdx
            if (is_signed) {
                SUBMIT(inst_u(CAST, n->dt));
            } else {
                SUBMIT(inst_i(MOV, n->dt, rdx, 0));
            }
            SUBMIT(inst_r(is_signed ? IDIV : DIV, n->dt, -1, rhs));
            SUBMIT(inst_copy(n->dt, fake_dst, USE(is_div ? rax : rdx)));

            dst = DEF(n, REG_CLASS_GPR);
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

        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV: {
            const static InstType ops[] = { FP_ADD, FP_SUB, FP_MUL, FP_DIV };
            dst = DEF(n, REG_CLASS_XMM);

            int lhs = ISEL(n->inputs[0]);
            int rhs = ISEL(n->inputs[1]);
            SUBMIT(inst_rr(ops[type - TB_FADD], n->dt, dst, lhs, rhs));
            break;
        }

        case TB_UINT2FLOAT:
        case TB_INT2FLOAT: {
            TB_DataType src_dt = n->inputs[0]->dt;
            assert(src_dt.type == TB_INT);

            // it's either 32bit or 64bit conversion
            //   CVTSI2SS r/m32, xmm1
            //   CVTSI2SD r/m64, xmm1
            dst = DEF(n, REG_CLASS_XMM);

            int src = ISEL(n->inputs[0]);
            SUBMIT(inst_r(FP_CVT, n->inputs[0]->dt, dst, src));
            break;
        }

        case TB_FLOAT2INT:
        case TB_FLOAT2UINT: {
            TB_DataType src_dt = n->inputs[0]->dt;
            assert(src_dt.type == TB_FLOAT);

            // it's either 32bit or 64bit conversion
            // F3 0F 2C /r            CVTTSS2SI xmm1, r/m32
            // F3 REX.W 0F 2C /r      CVTTSS2SI xmm1, r/m64
            // F2 0F 2C /r            CVTTSD2SI xmm1, r/m32
            // F2 REX.W 0F 2C /r      CVTTSD2SI xmm1, r/m64
            dst = DEF(n, REG_CLASS_GPR);

            int src = ISEL(n->inputs[0]);
            SUBMIT(inst_r(FP_CVTT, n->inputs[0]->dt, dst, src));
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
        case TB_MEMBER_ACCESS: {
            dst = DEF(n, REG_CLASS_GPR);
            int src = ISEL(n->inputs[0]);

            int64_t offset = TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset;
            SUBMIT(inst_ri(ADD, n->dt, dst, src, offset));
            break;
        }
        case TB_ARRAY_ACCESS: {
            dst = DEF(n, REG_CLASS_GPR);

            Inst inst = isel_array(ctx, n, dst);
            SUBMIT(inst);
            break;
        }

        case TB_RET: {
            if (n->input_count > 1) {
                assert(n->input_count <= 2 && "We don't support multiple returns here");

                // hint input to be RAX
                int src_vreg = isel(ctx, n->inputs[1]);
                hint(ctx, src_vreg, RAX);

                // we ain't gotta worry about regalloc here, we dippin
                int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
                SUBMIT(inst_copy(n->inputs[1]->dt, rax, USE(src_vreg)));
            }

            if (ctx->fallthrough != NULL) {
                SUBMIT(inst_jmp(NULL));
            }
            break;
        }

        case TB_LOAD: {
            dst = DEF(n, n->dt.type == TB_FLOAT ? REG_CLASS_XMM : REG_CLASS_GPR);

            Inst ld = isel_load(ctx, n, dst);
            SUBMIT(ld);
            break;
        }
        case TB_STORE: {
            // try load-assign-store pattern
            TB_Node* addr = n->inputs[1];
            TB_Node* src_n = n->inputs[2];

            /*if (!TB_NODE_GET_EXTRA_T(n, TB_NodeMemAccess)->is_volatile && src_n->type >= TB_AND && src_n->type <= TB_SUB) {
                const static InstType ops[] = { AND, OR, XOR, ADD, SUB };

                TB_Node* ld = src_n->inputs[0];
                if (ld->type == TB_LOAD && ld->inputs[1] == addr && !TB_NODE_GET_EXTRA_T(ld, TB_NodeMemAccess)->is_volatile) {
                    int32_t x;
                    if (try_for_imm32(ctx, src_n->inputs[1], &x)) {
                        int base = ISEL(addr);
                        SUBMIT(inst_mi(ops[src_n->type - TB_AND], n->dt, base, GPR_NONE, SCALE_X1, 0, x));
                    } else {
                        int src = ISEL(src_n->inputs[1]);
                        int base = ISEL(addr);
                        SUBMIT(inst_mr(ops[src_n->type - TB_AND], n->dt, base, GPR_NONE, SCALE_X1, 0, src));
                    }
                    break;
                }
            }*/

            int src = ISEL(n->inputs[2]);
            Inst st = isel_store(ctx, n->dt, n->inputs[1], src);
            SUBMIT(st);
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
            if (bits_in_type <= 8) op = sign_ext ? MOVSXB : MOVZXB;
            else if (bits_in_type <= 16) op = sign_ext ? MOVSXW : MOVZXW;
            else if (bits_in_type <= 32) op = sign_ext ? MOVSXD : MOV;
            else if (bits_in_type <= 64) op = MOV;
            else tb_todo();

            if (src->type == TB_LOAD) {
                Inst inst = isel_load(ctx, src, dst);
                inst.type = op;
                inst.data_type = legalize(n->dt);
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

            if (n->dt.type == TB_FLOAT) {
                dst = DEF(n, REG_CLASS_XMM);
                SUBMIT(inst_r(FP_CVT, n->inputs[0]->dt, dst, src));
            } else {
                dst = USE(src);

                // TODO(NeGate): verify this is a valid optimization
                // dst = DEF(n, REG_CLASS_GPR);
                // SUBMIT(inst_copy(n->dt, dst, src));
            }
            break;
        }

        case TB_BRANCH: {
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(tb_get_parent_region(n));
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
            TB_Node** succ = r->succ;

            if (r->succ_count == 1) {
                if (ctx->fallthrough != succ[0]) {
                    SUBMIT(inst_jmp(succ[0]));
                }
            } else if (r->succ_count == 2) {
                TB_DataType dt = n->inputs[1]->dt;

                // if-like branch
                if (br->keys[0] == 0) {
                    Cond cc = isel_cmp(ctx, n->inputs[1]);

                    // if flipping avoids a jmp, do that
                    TB_Node *f = succ[0], *t = succ[1];
                    if (ctx->fallthrough == f) {
                        SUBMIT(inst_jcc(t, cc ^ 1));
                    } else {
                        SUBMIT(inst_jcc(f, cc));

                        if (ctx->fallthrough != t) {
                            SUBMIT(inst_jmp(t));
                        }
                    }
                } else {
                    int key = USE(ISEL(n->inputs[1]));

                    SUBMIT(inst_i(CMP, dt, key, br->keys[0]));
                    SUBMIT(inst_jcc(succ[1], E));
                    SUBMIT(inst_jmp(succ[0]));
                }
            } else {
                TB_DataType dt = n->inputs[1]->dt;
                int key = USE(ISEL(n->inputs[1]));

                FOREACH_N(i, 1, r->succ_count) {
                    SUBMIT(inst_i(CMP, dt, key, br->keys[i-1]));
                    SUBMIT(inst_jcc(succ[i], E));
                }
                SUBMIT(inst_jmp(succ[0]));

                // switch-like branch
                // tb_todo();
            }
            break;
        }

        // the good thing is that safepoints don't need live ranges or anything
        case TB_SAFEPOINT: {
            TB_SafepointKey* key = &ctx->safepoints[TB_NODE_GET_EXTRA_T(n, TB_NodeSafepoint)->id];
            TB_Safepoint* restrict sp = malloc(sizeof(TB_Safepoint) + (n->input_count * sizeof(int32_t)));

            FOREACH_N(i, 0, n->input_count) {
                int src = ISEL(n->inputs[i]);
                SUBMIT(inst_use(src));
            }

            key->sp = sp;
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
                { 4, 4, 16, WIN64_ABI_CALLER_SAVED,  { RCX, RDX, R8,  R9,  0,  0 } },
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

            // system calls don't count, we track this for ABI
            // and stack allocation purposes.
            if (ctx->caller_usage < n->input_count) {
                ctx->caller_usage = n->input_count;
            }

            // generate clones of each parameter which live until the CALL instruction executes
            int fake_dst = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
            uint32_t caller_saved_gprs = desc->caller_saved_gprs;

            size_t xmms_used = 0;
            FOREACH_N(i, 2, n->input_count) {
                TB_Node* param = n->inputs[i];
                TB_DataType param_dt = param->dt;

                int src = isel(ctx, param);

                if (TB_IS_FLOAT_TYPE(param_dt) || param_dt.width) {
                    int xmm_id = is_sysv ? xmms_used++ : i - 2;
                    if (xmm_id < desc->gpr_count) {
                        // hint src into XMM
                        XMM target_xmm = xmm_id;
                        hint(ctx, src, target_xmm);

                        int param_def = DEF_FORCED(param, REG_CLASS_XMM, target_xmm, fake_dst);
                        SUBMIT(inst_copy(param->dt, param_def, USE(src)));
                    } else {
                        SUBMIT(inst_mr(FP_MOV, param->dt, RSP, GPR_NONE, SCALE_X1, (i - 2) * 8, USE(src)));
                    }
                } else {
                    if (i - 2 < desc->gpr_count) {
                        // hint src into GPR
                        GPR target_gpr = desc->gprs[i - 2];
                        hint(ctx, src, target_gpr);

                        int param_def = DEF_FORCED(param, REG_CLASS_GPR, target_gpr, fake_dst);
                        SUBMIT(inst_copy(param->dt, param_def, USE(src)));
                    } else {
                        SUBMIT(inst_mr(MOV, param->dt, RSP, GPR_NONE, SCALE_X1, (i - 2) * 8, USE(src)));
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

            TB_Node* target = n->inputs[1];
            if (type == TB_SCALL) {
                int num = ISEL(target);
                SUBMIT(inst_copy(n->dt, fake_dst, num));
                SUBMIT(inst_nullary(SYSCALL));
            } else {
                // the number of float parameters is written into AL
                if (is_sysv) {
                    int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, fake_dst);
                    SUBMIT(inst_i(MOV, TB_TYPE_I64, rax, xmms_used));
                }

                if (target->type == TB_GET_SYMBOL_ADDRESS) {
                    TB_NodeSymbol* s = TB_NODE_GET_EXTRA(target);
                    SUBMIT(inst_call(n->dt, fake_dst, s->sym));
                } else {
                    int call_target = ISEL(target);

                    SUBMIT(inst_r(CALL, n->dt, fake_dst, call_target));
                }
            }

            TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
            if (n->extra_count > sizeof(TB_NodeCall)) {
                // multiple returns must fill up the projections
                tb_todo();
            } else {
                dst = DEF_HINTED(n, REG_CLASS_GPR, RAX);
                SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
            }
            break;
        }

        case TB_MEMSET: {
            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
            int rdi = DEF_FORCED(n, REG_CLASS_GPR, RDI, rax);
            int rcx = DEF_FORCED(n, REG_CLASS_GPR, RCX, rax);

            // clobber inputs
            Clobbers* clobbers = arena_alloc(&tb__arena, sizeof(Clobbers) + (3 * sizeof(MachineReg)), _Alignof(Clobbers));
            clobbers->count = 3;
            clobbers->_[0] = (MachineReg){ REG_CLASS_GPR, RDI };
            clobbers->_[1] = (MachineReg){ REG_CLASS_GPR, RCX };
            clobbers->_[2] = (MachineReg){ REG_CLASS_GPR, RAX };
            ctx->defs[rax].clobbers = clobbers;

            // rep stosb
            //   mov rdi, ADDRESS
            int addr = isel(ctx, n->inputs[1]);
            hint(ctx, addr, RDI);
            SUBMIT(inst_copy(TB_TYPE_PTR, rdi, USE(addr)));
            //   mov rcx, SIZE
            int size = isel(ctx, n->inputs[3]);
            hint(ctx, size, RCX);
            SUBMIT(inst_copy(TB_TYPE_I64, rcx, USE(size)));
            //   mov rax, SRC
            int src = isel(ctx, n->inputs[2]);
            hint(ctx, src, RAX);
            SUBMIT(inst_copy(TB_TYPE_I8, rax, USE(src)));
            //   rep stosb
            Inst i = inst_nullary(STOSB);
            i.prefix |= INST_REP;
            SUBMIT(i);
            break;
        }
        case TB_MEMCPY: {
            int rsi = DEF_FORCED(n, REG_CLASS_GPR, RSI, -1);
            int rdi = DEF_FORCED(n, REG_CLASS_GPR, RDI, rsi);
            int rcx = DEF_FORCED(n, REG_CLASS_GPR, RCX, rsi);

            // clobber inputs
            Clobbers* clobbers = arena_alloc(&tb__arena, sizeof(Clobbers) + (3 * sizeof(MachineReg)), _Alignof(Clobbers));
            clobbers->count = 3;
            clobbers->_[0] = (MachineReg){ REG_CLASS_GPR, RDI };
            clobbers->_[1] = (MachineReg){ REG_CLASS_GPR, RCX };
            clobbers->_[2] = (MachineReg){ REG_CLASS_GPR, RSI };
            ctx->defs[rsi].clobbers = clobbers;

            // rep movsb
            //   mov rdi, ADDRESS
            int addr = isel(ctx, n->inputs[1]);
            hint(ctx, addr, RDI);
            SUBMIT(inst_copy(TB_TYPE_PTR, rdi, USE(addr)));
            //   mov rcx, SIZE
            int size = isel(ctx, n->inputs[3]);
            hint(ctx, size, RCX);
            SUBMIT(inst_copy(TB_TYPE_I64, rcx, USE(size)));
            //   mov rsi, SRC
            int src = isel(ctx, n->inputs[2]);
            hint(ctx, src, RSI);
            SUBMIT(inst_copy(TB_TYPE_PTR, rsi, USE(src)));
            //   rep movsb
            Inst i = inst_nullary(MOVSB);
            i.prefix |= INST_REP;
            SUBMIT(i);
            break;
        }

        // x86 intrinsics
        case TB_DEBUGBREAK: {
            SUBMIT(inst_nullary(INT3));
            break;
        }

        /*case TB_X86INTRIN_RDTSC: {
            SUBMIT(inst_nullary(RDTSC));
            break;
        }*/

        case TB_PROJ: {
            isel(ctx, n->inputs[0]);

            if (n->inputs[0]->type == TB_START) {
                int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;

                // past the first 4 parameters, it's all stack
                if (index >= 4 && ctx->target_abi == TB_ABI_WIN64) {
                    dst = DEF(n, classify_reg_class(n->dt));

                    InstType i = n->dt.type == TB_FLOAT ? FP_MOV : MOV;
                    SUBMIT(inst_m(i, n->dt, dst, RBP, GPR_NONE, SCALE_X1, 16 + (index * 8)));
                    break;
                }
            }

            ptrdiff_t search = nl_map_get(ctx->values, n);
            assert(search >= 0);

            dst = ctx->values[search].v;
            break;
        }

        case TB_NULL: break;
        default: tb_todo();
    }

    if (n->type != TB_LOCAL) {
        nl_map_put(ctx->values, n, dst);
    }
    return dst;
}

static void copy_value(Ctx* restrict ctx, TB_Node* phi, int dst, TB_Node* src, TB_DataType dt) {
    if (src->type == TB_ADD) {
        if (src->inputs[0] == phi) {
            int32_t x;
            if (try_for_imm32(ctx, src->inputs[1], &x)) {
                SUBMIT(inst_ri(ADD, dt, USE(dst), dst, x));
            } else {
                int other = ISEL(src->inputs[1]);
                SUBMIT(inst_rr(ADD, dt, USE(dst), dst, other));
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

    REG_ALLOC_LOG printf("  \x1b[32m#   spill D%d (rbp + %d)\x1b[0m\n", r->old, r->stack_pos);

    // write out
    InstType i = r->dt.type == TB_FLOAT ? FP_MOV : MOV;
    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);

    *new_inst = inst_mr(i, r->dt, RBP, GPR_NONE, SCALE_X1, r->stack_pos, USE(r->old));
    new_inst->time = basepoint->time + 1;
    new_inst->next = basepoint->next;
    basepoint->next = new_inst;
}

static void reload(Ctx* restrict ctx, Inst* basepoint, Reload* r, size_t op_index) {
    InstType i = r->dt.type == TB_FLOAT ? FP_MOV : MOV;

    /*Inst* next = basepoint->next;
    if (next->type == INST_COPY && op_index == 1) {
        REG_ALLOC_LOG printf("  \x1b[32m#   folded reload D%d (rbp + %d)\x1b[0m\n", r->old, r->stack_pos);

        int old_time = next->time;
        Inst* old_next = next->next;

        *next = inst_m(i, r->dt, next->regs[0], RBP, GPR_NONE, SCALE_X1, r->stack_pos);
        next->time = old_time;
        next->next = old_next;
        return;
    }*/

    REG_ALLOC_LOG printf("  \x1b[32m#   reload D%d (rbp + %d)\x1b[0m\n", r->old, r->stack_pos);

    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);

    *new_inst = inst_m(i, r->dt, r->old, RBP, GPR_NONE, SCALE_X1, r->stack_pos);
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

static void inst2_print(Ctx* restrict ctx, InstType type, Val* dst, Val* src, TB_X86_DataType dt) {
    ASM {
        printf("  %s", inst_table[type].mnemonic);
        if (dt >= TB_X86_TYPE_SSE_SS && dt <= TB_X86_TYPE_SSE_PD) {
            static const char suffixes[4][3] = { "ss", "sd", "ps", "pd" };
            printf("%s ", suffixes[dt - TB_X86_TYPE_SSE_SS]);
        } else {
            printf(" ");
        }
        print_operand(dst);
        printf(", ");
        print_operand(src);
        printf("\n");
    }

    if (dt >= TB_X86_TYPE_SSE_SS && dt <= TB_X86_TYPE_SSE_PD) {
        INST2SSE(type, dst, src, dt);
    } else {
        INST2(type, dst, src, dt);
    }
}

static void inst1_print(Ctx* restrict ctx, int type, Val* src, TB_X86_DataType dt) {
    ASM {
        printf("  %s ", inst_table[type].mnemonic);
        print_operand(src);
        printf("\n");
    }
    INST1(type, src, dt);
}

static void emit_code(Ctx* restrict ctx) {
    Val ops[4];
    for (Inst* restrict inst = ctx->first; inst; inst = inst->next) {
        if (inst->type == INST_LABEL) {
            TB_Node* bb = (TB_Node*) inst->imm[0];
            uint32_t pos = GET_CODE_POS(&ctx->emit);
            tb_resolve_rel32(&ctx->emit, &nl_map_get_checked(ctx->emit.labels, bb), pos);

            ASM {
                if (bb == ctx->f->start_node) {
                    printf("%s:\n", ctx->f->super.name);
                } else {
                    printf("L%p:\n", bb);
                }
            }
            continue;
        } else if (inst->type == INST_LINE) {
            TB_Function* f = ctx->f;
            TB_Line l = {
                .file = inst->imm[0],
                .line = inst->imm[1],
                .pos = GET_CODE_POS(&ctx->emit)
            };
            dyn_array_put(f->lines, l);
            continue;
        } else if (inst->type == INST_USE) {
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
                ops[1] = val_label((TB_Node*) inst->imm[0]);
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
            case X86_OP_MI: {
                Scale scale = (inst->imm[0] >> 32);
                int32_t disp = inst->imm[0] & 0xFFFFFFFF;

                ops[1] = val_base_index_disp(regs[2], regs[3], scale, disp);
                ops[2] = val_imm(inst->imm[1]);
                op_count = 3;
                break;
            }
            default: tb_todo();
        }

        bool is_fp = (inst->data_type >= TB_X86_TYPE_SSE_SS && inst->data_type <= TB_X86_TYPE_SSE_PD);
        if (is_fp) {
            FOREACH_N(j, 0, op_count) {
                if (ops[j].type == VAL_GPR) ops[j].type = VAL_XMM;
            }
        }

        // TODO(NeGate): this can potentially place the prefix too early
        if (inst->prefix & INST_REP) EMIT1(&ctx->emit, 0xF3);

        if (inst->type == INST_MOVE) {
            if (!is_value_match(&ops[1], &ops[2])) {
                inst2_print(ctx, is_fp ? FP_MOV : MOV, &ops[1], &ops[2], inst->data_type);
            }
        } else if (inst->type == INST_COPY) {
            if (!is_value_match(&ops[0], &ops[1])) {
                inst2_print(ctx, is_fp ? FP_MOV : MOV, &ops[0], &ops[1], inst->data_type);
            }
        } else if (op_count == 0) {
            INST0(inst->type, inst->data_type);
            ASM printf("  %s\n", inst_table[inst->type].mnemonic);
        } else if (op_count == 1) {
            if (!has_def) {
                inst1_print(ctx, inst->type, &ops[1], inst->data_type);
            } else {
                tb_todo();
            }
        } else if (op_count == 2) {
            if (inst->type == JMP || inst->type == CALL) {
                inst1_print(ctx, inst->type, &ops[1], inst->data_type);
            } else {
                // sometimes 2ary is a unary with a separated dst and src, or a binop
                if (inst_table[inst->type].cat <= INST_UNARY_EXT) {
                    if (!is_value_match(&ops[0], &ops[1])) {
                        inst2_print(ctx, is_fp ? FP_MOV : MOV, &ops[0], &ops[1], inst->data_type);
                    }
                    inst1_print(ctx, inst->type, &ops[0], inst->data_type);
                } else {
                    inst2_print(ctx, inst->type, &ops[0], &ops[1], inst->data_type);
                }
            }
        } else if (op_count == 3) {
            if (!has_def) {
                inst2_print(ctx, (InstType) inst->type, &ops[1], &ops[2], inst->data_type);
            } else {
                if (!is_value_match(&ops[0], &ops[1])) {
                    inst2_print(ctx, is_fp ? FP_MOV : MOV, &ops[0], &ops[1], inst->data_type);
                }
                inst2_print(ctx, (InstType) inst->type, &ops[0], &ops[2], inst->data_type);
            }
        } else {
            tb_todo();
        }
    }
}

static void resolve_stack_usage(Ctx* restrict ctx, size_t caller_usage) {
    if (ctx->target_abi == TB_ABI_WIN64 && caller_usage > 0 && caller_usage < 4) {
        caller_usage = 4;
    }

    size_t usage = ctx->stack_usage + (caller_usage * 8);

    // Align stack usage to 16bytes
    ctx->stack_usage = align_up(usage, 16);

    int callee_saves = tb_popcount(ctx->regs_to_save & 0xFFFF);
    if (ctx->stack_usage > 0) {
        callee_saves += 1; // RBP is pushed
    }

    if ((callee_saves % 2) == 0) {
        if (ctx->stack_usage) {
            ctx->stack_usage += 8;
        } else {
            ctx->regs_to_save |= (1ull << 63ull); // push dummy for alignment
        }
    }
}

static void patch_local_labels(Ctx* restrict ctx) {
    tb_resolve_rel32(&ctx->emit, &ctx->emit.return_label, GET_CODE_POS(&ctx->emit));
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
    size_t used = 0;

    // push rbp
    if (stack_usage > 0) {
        out[used++] = 0x50 + RBP;

        // mov rbp, rsp
        out[used++] = rex(true, RSP, RBP, 0);
        out[used++] = 0x89;
        out[used++] = mod_rx_rm(MOD_DIRECT, RSP, RBP);
    }

    // push rXX
    FOREACH_N(i, 0, 16) if (saved & (1ull << i)) {
        if (i < 8) {
            out[used++] = 0x50 + i;
        } else {
            out[used++] = 0x41;
            out[used++] = 0x50 + (i & 0b111);
        }
    }

    // push dummy reg
    if (saved & (1ull << 63ull)) {
        out[used++] = 0x50 + RAX; // PUSH RAX
    }

    if (stack_usage > 0) {
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
    if (stack_usage > 0) {
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
    }

    // pop dummy register, it doesn't matter which it is as long as it's caller saved
    if (saved & (1ull << 63ull)) {
        out[used++] = 0x58 + RCX; // POP RCX
    }

    // pop gpr
    FOREACH_REVERSE_N(i, 0, 16) if (saved & (1ull << i)) {
        if (i < 8) {
            out[used++] = 0x58 + i;
        } else {
            out[used++] = 0x41;
            out[used++] = 0x58 + (i & 0b111);
        }
    }

    if (stack_usage > 0) {
        out[used++] = 0x58 + RBP;
    }

    out[used++] = 0xC3;
    return used;
}

static size_t emit_call_patches(TB_Module* restrict m) {
    size_t r = 0;
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* func_out = f->output;

        for (TB_SymbolPatch* patch = func_out->last_patch; patch; patch = patch->prev) {
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

        m->text.reloc_count += func_out->patch_count;
    }

    return r;
}

ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,
    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .emit_prologue      = emit_prologue,
    .emit_epilogue      = emit_epilogue,
    .fast_path          = compile_function,
};
