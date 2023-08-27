#include "../x64/x64.h"
#include "../x64/x64_emitter.h"
#include "../objects/win64eh.h"

#include "x64_disasm.c"

enum {
    CG_REGISTER_CLASSES = 2
};

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM,

    FIRST_GPR = 0,
    FIRST_XMM = 16, // we're getting more GPRs in intel APX so this might change :)
};

typedef struct {
    TB_DataType dt;

    int old;
    int stack_pos;
} Reload;

static const struct ParamDescriptor {
    int gpr_count;
    int xmm_count;
    uint16_t caller_saved_xmms; // XMM0 - XMMwhatever
    uint16_t caller_saved_gprs; // bitfield

    GPR gprs[6];
} param_descs[] = {
    // win64
    { 4, 4, 6, WIN64_ABI_CALLER_SAVED,  { RCX, RDX, R8,  R9,  0,  0 } },
    // system v
    { 6, 4, 5, SYSV_ABI_CALLER_SAVED,    { RDI, RSI, RDX, RCX, R8, R9 } },
    // syscall
    { 6, 4, 5, SYSCALL_ABI_CALLER_SAVED, { RDI, RSI, RDX, R10, R8, R9 } },
};

#include "generic_cg.h"

// initialize register allocator state
static void init_regalloc(Ctx* restrict ctx) {
    // Generate intervals for physical registers
    FOREACH_N(i, 0, 16) {
        dyn_array_put(ctx->intervals, (LiveInterval){ .reg_class = REG_CLASS_GPR, .dt = TB_X86_TYPE_QWORD, .reg = i, .assigned = i, .hint = -1, .start = INT_MAX, .split_kid = -1 });
    }

    FOREACH_N(i, 0, 16) {
        dyn_array_put(ctx->intervals, (LiveInterval){ .reg_class = REG_CLASS_XMM, .dt = TB_X86_TYPE_XMMWORD, .reg = i, .assigned = i, .hint = -1, .start = INT_MAX, .split_kid = -1 });
    }
}

static void mark_callee_saved_constraints(Ctx* restrict ctx, uint64_t callee_saved[CG_REGISTER_CLASSES]) {
    bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
    const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];

    // don't include RBP and RSP, those are special cases
    uint32_t callee_saved_gprs = ~desc->caller_saved_gprs;
    callee_saved_gprs &= ~(1u << RBP);
    callee_saved_gprs &= ~(1u << RSP);
    callee_saved[0] = callee_saved_gprs;

    // mark XMM callees
    callee_saved[1] = 0;
    FOREACH_N(i, desc->caller_saved_xmms, 16) {
        callee_saved[1] |= (1ull << i);
    }
}

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

static int classify_reg_class(TB_DataType dt) {
    return dt.type == TB_FLOAT ? REG_CLASS_XMM : REG_CLASS_GPR;
}

static bool wont_spill_around(int t) {
    return t == INST_LABEL || t == TEST || t == CMP || t == JMP || (t >= JO && t <= JG);
}

static bool is_terminator(int t) {
    return t == INST_TERMINATOR || t == INT3 || t == UD2;
}

static bool try_for_imm32(Ctx* restrict ctx, TB_Node* n, int32_t* out_x) {
    uint64_t mask = UINT64_MAX;
    TB_Node* prev = n;
    if (n->type == TB_SIGN_EXT) {
        n = n->inputs[1];
    } else if (n->type == TB_TRUNCATE) {
        assert(n->dt.type == TB_INT);

        n = n->inputs[1];
        mask = n->dt.data == 64 ? UINT64_MAX : (1ull << n->dt.data) - 1;
    }

    if (n->type == TB_INTEGER_CONST) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        if (i->num_words == 1 && fits_into_int32(i->words[0] & mask)) {
            if (prev != n) use(ctx, prev);

            *out_x = i->words[0] & mask;
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

static Inst* inst_jmp(TB_Node* target) {
    Inst* i = alloc_inst(JMP, TB_TYPE_VOID, 0, 0, 0);
    i->flags = INST_NODE;
    i->n = target;
    return i;
}

static Inst* inst_jcc(TB_Node* target, Cond cc) {
    Inst* i = alloc_inst(JO + cc, TB_TYPE_VOID, 0, 0, 0);
    i->flags = INST_NODE;
    i->n = target;
    return i;
}

// generates an LEA for computing the address of n.
static Inst* isel_addr(Ctx* restrict ctx, TB_Node* n, int dst) {
    int64_t offset = 0;
    if (n->type == TB_VA_START) {
        assert(ctx->module->target_abi == TB_ABI_WIN64 && "How does va_start even work on SysV?");

        // on Win64 va_start just means whatever is one parameter away from
        // the parameter you give it (plus in Win64 the parameters in the stack
        // are 8bytes, no fanciness like in SysV):
        // void printf(const char* fmt, ...) {
        //     va_list args;
        //     va_start(args, fmt); // args = ((char*) &fmt) + 8;
        //     ...
        // }
        offset = 8;

        use(ctx, n);
        n = n->inputs[1];
    } else if (n->type == TB_MEMBER_ACCESS) {
        offset = TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset;

        use(ctx, n);
        n = n->inputs[1];
    }

    Scale scale = SCALE_X1;
    int index = -1;

    if (n->type == TB_ARRAY_ACCESS) {
        TB_Node* base = n->inputs[1];
        int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;

        use(ctx, n);
        n = n->inputs[2];

        int32_t x;
        if (n->type == TB_SHL && try_for_imm32(ctx, n->inputs[2], &x)) {
            use(ctx, n);
            use(ctx, n->inputs[2]);

            n = n->inputs[1];
            stride *= (1ull << x);
        }

        index = isel(ctx, n);

        // compute index
        if (stride == 1) {
            // no scaling required
        } else if (tb_is_power_of_two(stride)) {
            scale = tb_ffs(stride) - 1;

            if (scale > 3) {
                // we can't fit this into an LEA, might as well just do a shift
                SUBMIT(inst_op_rri(SHL, TB_TYPE_I64, dst, index, scale));
                index = dst, scale = 1;
            }
        } else {
            // needs a proper multiply (we may wanna invest in a few special patterns
            // for reducing simple multiplies into shifts)
            //
            //   a * 24 => (a * 8) * 3
            //                b    * 3 => b<<1 + b
            //
            // thus
            //
            //   LEA b,   [a * 8]
            //   LEA dst, [b * 2 + b]
            SUBMIT(inst_op_rri(IMUL, n->dt, dst, index, stride));
            index = dst;
        }

        n = base;
    }

    int base;
    if (n->type == TB_LOCAL) {
        use(ctx, n);
        offset += get_stack_slot(ctx, n);
        base = RBP;
    } else {
        base = isel(ctx, n);
    }

    // compute base
    if (index < 0 && offset == 0) {
        return inst_move(n->dt, dst, base);
    } else {
        return inst_op_rm(LEA, n->dt, dst, base, index, scale, offset);
    }
}

static Cond isel_cmp(Ctx* restrict ctx, TB_Node* n) {
    bool invert = false;
    if (n->type == TB_CMP_EQ && n->dt.type == TB_INT && n->dt.data == 1 && n->inputs[2]->type == TB_INTEGER_CONST) {
        TB_NodeInt* b = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt);
        if (b->num_words == 1 && b->words[0] == 0) {
            invert = true;
            n = n->inputs[1];
        }
    }

    int src;
    ptrdiff_t search = nl_map_get(ctx->values, n);
    if (search < 0) {
        if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
            TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            assert(cmp_dt.width == 0 && "TODO: Implement vector compares");

            Cond cc = -1;
            use(ctx, n);

            if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                int lhs = ISEL(n->inputs[1]);
                int rhs = ISEL(n->inputs[2]);
                SUBMIT(inst_op_rr_no_dst(FP_UCOMI, cmp_dt, lhs, rhs));

                switch (n->type) {
                    case TB_CMP_EQ:  cc = E; break;
                    case TB_CMP_NE:  cc = NE; break;
                    case TB_CMP_FLT: cc = B; break;
                    case TB_CMP_FLE: cc = BE; break;
                    default: tb_unreachable();
                }
            } else {
                bool invert = false;
                int32_t x;
                int lhs = ISEL(n->inputs[1]);
                if (try_for_imm32(ctx, n->inputs[2], &x)) {
                    use(ctx, n->inputs[2]);

                    if (x == 0 && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
                        SUBMIT(inst_op_rr_no_dst(TEST, cmp_dt, lhs, lhs));
                    } else {
                        SUBMIT(inst_op_ri(CMP, cmp_dt, lhs, x));
                    }
                } else {
                    int tmp = DEF(n, cmp_dt);
                    SUBMIT(inst_move(cmp_dt, tmp, lhs));

                    int rhs = ISEL(n->inputs[2]);
                    SUBMIT(inst_op_rr_no_dst(CMP, cmp_dt, tmp, rhs));
                }

                switch (n->type) {
                    case TB_CMP_EQ: cc = E; break;
                    case TB_CMP_NE: cc = NE; break;
                    case TB_CMP_SLT: cc = invert ? G  : L;  break;
                    case TB_CMP_SLE: cc = invert ? GE : LE; break;
                    case TB_CMP_ULT: cc = invert ? A  : B;  break;
                    case TB_CMP_ULE: cc = invert ? NB : BE; break;
                    default: tb_unreachable();
                }
            }

            return cc ^ invert;
        }
    }

    src = ISEL(n);

    TB_DataType dt = n->dt;
    if (TB_IS_FLOAT_TYPE(dt)) {
        int tmp = DEF(n, n->dt);
        SUBMIT(inst_op_zero(dt, tmp));
        SUBMIT(inst_op_rr_no_dst(FP_UCOMI, dt, src, tmp));
    } else {
        SUBMIT(inst_op_rr_no_dst(TEST, dt, src, src));
    }
    return NE ^ invert;
}

static int isel(Ctx* restrict ctx, TB_Node* n) {
    use(ctx, n);

    ptrdiff_t search = nl_map_get(ctx->values, n);
    if (search >= 0) {
        return ctx->values[search].v;
    }

    TB_NodeTypeEnum type = n->type;
    int dst = -1;
    bool forget = false;

    switch (type) {
        case TB_PHI: {
            dst = DEF(n, n->dt);
            break;
        }

        case TB_REGION: break;
        case TB_START: {
            TB_NodeRegion* start = TB_NODE_GET_EXTRA(n);
            const TB_FunctionPrototype* restrict proto = ctx->f->prototype;
            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);

            const GPR* gpr_params  = is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;
            size_t gpr_param_count = is_sysv ? COUNTOF(SYSV_GPR_PARAMETERS) : COUNTOF(WIN64_GPR_PARAMETERS);
            int xmm_param_count    = is_sysv ? 8 : 4;

            Inst* prev = ctx->last;

            int out_count = 0;
            RegIndex outs[16];

            // handle known parameters
            int used_gpr = 0, used_xmm = 0;
            TB_Node** params = ctx->f->params;
            FOREACH_N(i, 0, ctx->f->param_count) {
                TB_Node* proj = params[i];
                bool is_float = proj->dt.type == TB_FLOAT;

                // copy from parameter
                int reg_class = (is_float ? REG_CLASS_XMM : REG_CLASS_GPR);
                int v = -1;

                int id = is_float ? used_gpr : used_xmm;
                if (is_sysv) {
                    if (is_float) used_xmm += 1;
                    else used_gpr += 1;
                } else {
                    // win64 will expend the slot regardless of if it's used
                    used_gpr += 1;
                    used_xmm += 1;
                }

                int reg_limit = is_float ? xmm_param_count : gpr_param_count;
                if (id < reg_limit) {
                    int reg_num = is_float ? id : gpr_params[id];
                    int vreg = (is_float ? FIRST_XMM : 0) + reg_num;

                    v = DEF(proj, proj->dt);
                    hint_reg(ctx, v, vreg);
                    SUBMIT(inst_move(proj->dt, v, vreg));

                    outs[out_count++] = vreg;

                    nl_map_put(ctx->values, proj, v);
                }
            }

            // insert INST_ENTRY (this is where parameter register come from)
            Inst* entry_inst = alloc_inst(INST_ENTRY, TB_TYPE_I64, out_count, 0, 0);
            memcpy(entry_inst->operands, outs, out_count * sizeof(RegIndex));

            entry_inst->next = prev->next;
            prev->next = entry_inst;

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

                    SUBMIT(inst_op_mr(MOV, TB_TYPE_I64, RBP, GPR_NONE, SCALE_X1, dst_pos, src));
                }

                ctx->stack_usage += (extra_param_count * 8);
            }
            break;
        }

        case TB_INTEGER_CONST: {
            TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
            assert(i->num_words == 1);

            dst = DEF(n, n->dt);
            uint64_t x = i->words[0];

            // mask off bits
            uint64_t bits_in_type = n->dt.type == TB_PTR ? 64 : n->dt.data;
            if (bits_in_type < 64) {
                x &= (1ull << bits_in_type) - 1;
            }

            if (!fits_into_int32(x)) {
                // movabs reg, imm64
                SUBMIT(inst_op_abs(MOVABS, n->dt, dst, x));
            } else if (x == 0) {
                SUBMIT(inst_op_zero(n->dt, dst));
            } else {
                SUBMIT(inst_op_imm(MOV, n->dt, dst, x));
            }
            forget = true;
            break;
        }

        case TB_SELECT: {
            dst = DEF(n, n->dt);

            int lhs = isel(ctx, n->inputs[2]);
            int rhs = isel(ctx, n->inputs[3]);

            Cond cc = isel_cmp(ctx, n->inputs[1]);
            SUBMIT(inst_move(n->dt, dst, rhs));
            SUBMIT(inst_op_rr(CMOVO + cc, n->dt, dst, lhs));
            break;
        }

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB: {
            const static InstType ops[] = { AND, OR, XOR, ADD, SUB };
            InstType op = ops[type - TB_AND];

            dst = DEF(n, n->dt);

            int lhs = isel(ctx, n->inputs[1]);
            hint_reg(ctx, dst, lhs);

            int32_t x;
            if (try_for_imm32(ctx, n->inputs[2], &x)) {
                use(ctx, n->inputs[2]);

                SUBMIT(inst_move(n->dt, dst, lhs));
                SUBMIT(inst_op_rri(op, n->dt, dst, dst, x));
            } else {
                int rhs = isel(ctx, n->inputs[2]);

                SUBMIT(inst_move(n->dt, dst, lhs));
                SUBMIT(inst_op_rrr(op, n->dt, dst, dst, rhs));
            }
            break;
        }

        case TB_MUL: {
            dst = DEF(n, n->dt);
            int lhs = isel(ctx, n->inputs[1]);
            hint_reg(ctx, dst, lhs);

            int32_t x;
            if (try_for_imm32(ctx, n->inputs[2], &x)) {
                use(ctx, n->inputs[2]);

                SUBMIT(inst_move(n->dt, dst, lhs));
                SUBMIT(inst_op_rri(IMUL, n->dt, dst, dst, x));
            } else {
                int rhs = isel(ctx, n->inputs[2]);

                SUBMIT(inst_move(n->dt, dst, lhs));
                SUBMIT(inst_op_rrr(IMUL, n->dt, dst, dst, rhs));
            }
            break;
        }

        // bit shifts
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_ROL:
        case TB_ROR: {
            const static InstType ops[] = { SHL, SHR, SAR, ROL, ROR };
            InstType op = ops[type - TB_SHL];

            dst = DEF(n, n->dt);

            int lhs = isel(ctx, n->inputs[1]);
            hint_reg(ctx, dst, lhs);

            int32_t x;
            if (try_for_imm32(ctx, n->inputs[2], &x) && x == (int8_t)x) {
                use(ctx, n->inputs[2]);
                SUBMIT(inst_move(n->dt, dst, lhs));
                SUBMIT(inst_op_rri_tmp(op, n->dt, dst, dst, x, RCX));
                break;
            }

            // the shift operations need their right hand side in CL (RCX's low 8bit)
            int rhs = isel(ctx, n->inputs[2]);

            SUBMIT(inst_move(n->dt, dst, lhs));
            SUBMIT(inst_move(n->dt, RCX, rhs));
            SUBMIT(inst_op_rrr_tmp(op, n->dt, dst, dst, RCX, RCX));
            break;
        }

        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD: {
            bool is_signed = (type == TB_SDIV || type == TB_SMOD);
            bool is_div    = (type == TB_UDIV || type == TB_SDIV);

            TB_DataType dt = n->dt;
            assert(dt.type == TB_INT);

            int op = MOV;
            if (dt.data <= 8)       op = is_signed ? MOVSXB : MOVZXB;
            else if (dt.data <= 16) op = is_signed ? MOVSXW : MOVZXW;

            // division is scaled up to 32bit
            if (dt.data < 32) dt.data = 32;

            // mov rax, lhs
            int lhs = isel(ctx, n->inputs[1]);
            SUBMIT(inst_op_rr(op, dt, RAX, lhs));

            int rhs = isel(ctx, n->inputs[2]);
            if (n->dt.data < 32) {
                // add cast
                int new_rhs = DEF(n->inputs[2], TB_TYPE_I32);
                SUBMIT(inst_op_rr(op, TB_TYPE_I32, new_rhs, rhs));
                rhs = new_rhs;
            }

            // if signed:
            //   cqo/cdq (sign extend RAX into RDX)
            // else:
            //   xor rdx, rdx
            if (is_signed) {
                Inst* inst = alloc_inst(CAST, dt, 1, 1, 0);
                inst->operands[0] = RDX;
                inst->operands[1] = RAX;
                SUBMIT(inst);
            } else {
                SUBMIT(inst_op_zero(dt, RDX));
            }

            {
                Inst* inst = alloc_inst(is_signed ? IDIV : DIV, dt, 2, 3, 0);
                inst->operands[0] = is_div ? RAX : RDX;
                inst->operands[1] = is_div ? RDX : RAX;
                inst->operands[2] = rhs;
                inst->operands[3] = RDX;
                inst->operands[4] = RAX;
                SUBMIT(inst);
            }

            dst = DEF(n, n->dt);
            hint_reg(ctx, dst, is_div ? RAX : RDX);
            SUBMIT(inst_move(dt, dst, is_div ? RAX : RDX));
            break;
        }

        case TB_FLOAT32_CONST: {
            assert(n->dt.type == TB_FLOAT && n->dt.width == 0);
            uint32_t imm = (Cvt_F32U32) { .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value }.i;

            dst = DEF(n, n->dt);
            if (imm == 0) {
                SUBMIT(inst_op_zero(n->dt, dst));
            } else {
                TB_Global* g = tb__small_data_intern(ctx->module, sizeof(float), &imm);
                SUBMIT(inst_op_global(FP_MOV, n->dt, dst, (TB_Symbol*) g));
            }
            forget = true;
            break;
        }
        case TB_FLOAT64_CONST: {
            assert(n->dt.type == TB_FLOAT && n->dt.width == 0);
            uint64_t imm = (Cvt_F64U64){ .f = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value }.i;

            dst = DEF(n, n->dt);
            if (imm == 0) {
                SUBMIT(inst_op_zero(n->dt, dst));
            } else {
                TB_Global* g = tb__small_data_intern(ctx->module, sizeof(double), &imm);
                SUBMIT(inst_op_global(FP_MOV, n->dt, dst, (TB_Symbol*) g));
            }
            forget = true;
            break;
        }
        case TB_FLOAT_EXT: {
            dst = DEF(n, n->dt);

            int src = isel(ctx, n->inputs[1]);
            SUBMIT(inst_op_rr(FP_CVT, n->inputs[1]->dt, dst, src));
            break;
        }
        case TB_NEG:
        case TB_NOT: {
            dst = DEF(n, n->dt);

            if (n->dt.type != TB_FLOAT) {
                int src = ISEL(n->inputs[1]);

                SUBMIT(inst_op_rr(type == TB_NOT ? NOT : NEG, n->dt, dst, src));
            } else {
                if (type == TB_NEG) {
                    TB_Global* g = NULL;
                    if (n->dt.data == TB_FLT_32) {
                        uint32_t buffer[4] = { 1u << 31u, 1u << 31u, 1u << 31u, 1u << 31u };
                        g = tb__small_data_intern(ctx->module, 16, buffer);
                    } else if (n->dt.data == TB_FLT_64) {
                        uint64_t buffer[4] = { 1ull << 63ull, 1ull << 63ull };
                        g = tb__small_data_intern(ctx->module, 16, buffer);
                    } else {
                        tb_todo();
                    }

                    SUBMIT(inst_op_global(FP_XOR, n->dt, dst, (TB_Symbol*) g));
                } else {
                    tb_todo();
                }
            }
            break;
        }
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV: {
            const static InstType ops[] = { FP_ADD, FP_SUB, FP_MUL, FP_DIV };
            dst = DEF(n, n->dt);

            int lhs = isel(ctx, n->inputs[1]);
            hint_reg(ctx, dst, lhs);
            SUBMIT(inst_move(n->dt, dst, lhs));

            int rhs = isel(ctx, n->inputs[2]);
            SUBMIT(inst_op_rrr(ops[type - TB_FADD], n->dt, dst, dst, rhs));
            break;
        }
        case TB_UINT2FLOAT:
        case TB_INT2FLOAT: {
            TB_DataType src_dt = n->inputs[1]->dt;
            assert(src_dt.type == TB_INT);

            // it's either 32bit or 64bit conversion
            //   CVTSI2SS r/m32, xmm1
            //   CVTSI2SD r/m64, xmm1
            dst = DEF(n, n->dt);

            bool is_64bit = src_dt.data > 32;

            int src = isel(ctx, n->inputs[1]);
            SUBMIT(inst_op_rr(is_64bit ? FP_CVT64 : FP_CVT32, n->dt, dst, src));
            break;
        }

        case TB_FLOAT2INT:
        case TB_FLOAT2UINT: {
            TB_DataType src_dt = n->inputs[1]->dt;
            assert(src_dt.type == TB_FLOAT);

            // it's either 32bit or 64bit conversion
            // F3 0F 2C /r            CVTTSS2SI xmm1, r/m32
            // F3 REX.W 0F 2C /r      CVTTSS2SI xmm1, r/m64
            // F2 0F 2C /r            CVTTSD2SI xmm1, r/m32
            // F2 REX.W 0F 2C /r      CVTTSD2SI xmm1, r/m64
            dst = DEF(n, n->dt);

            int src = isel(ctx, n->inputs[1]);
            SUBMIT(inst_op_rr(FP_CVTT, src_dt, dst, src));
            break;
        }

        // pointer arithmatic
        case TB_LOCAL:
        dst = DEF(n, n->dt);
        SUBMIT(isel_addr(ctx, n, dst));
        forget = true;
        break;

        case TB_VA_START:
        case TB_MEMBER_ACCESS:
        case TB_ARRAY_ACCESS: {
            dst = DEF(n, n->dt);
            SUBMIT(isel_addr(ctx, n, dst));
            break;
        }

        // downcasting
        case TB_PTR2INT:
        case TB_TRUNCATE: {
            int src = isel(ctx, n->inputs[1]);

            dst = DEF(n, n->dt);
            if (n->dt.type == TB_FLOAT) {
                SUBMIT(inst_op_rr(FP_CVT, n->inputs[1]->dt, dst, src));
            } else {
                SUBMIT(inst_move(n->dt, dst, src));
            }
            break;
        }

        // upcasting
        case TB_INT2PTR:
        case TB_SIGN_EXT:
        case TB_ZERO_EXT: {
            TB_Node* src = n->inputs[1];

            TB_DataType src_dt = src->dt;
            bool sign_ext = (type == TB_SIGN_EXT);
            int bits_in_type = src_dt.type == TB_PTR ? 64 : src_dt.data;

            dst = DEF(n, n->dt);

            int32_t imm;
            if (try_for_imm32(ctx, src, &imm)) {
                #define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
                use(ctx, src);

                uint64_t src = imm;
                uint64_t sign_bit = (src >> (bits_in_type - 1)) & 1;
                uint64_t mask = MASK_UPTO(64) & ~MASK_UPTO(bits_in_type);

                src = (src & ~mask) | (sign_bit ? mask : 0);
                if (!fits_into_int32(src)) {
                    // movabs reg, imm64
                    SUBMIT(inst_op_abs(MOVABS, n->dt, dst, src));
                } else {
                    SUBMIT(inst_op_imm(MOV, n->dt, dst, src));
                }

                return dst;
                #undef MASK_UPTO
            }

            TB_DataType dt = n->dt;

            int op = MOV;
            if (bits_in_type <= 8) op = sign_ext ? MOVSXB : MOVZXB;
            else if (bits_in_type <= 16) op = sign_ext ? MOVSXW : MOVZXW;
            else if (bits_in_type <= 32) {
                if (sign_ext) op = MOVSXD;
                else dt = src_dt;
            } else if (bits_in_type <= 64) op = MOV;
            else tb_todo();

            /*if (src->type == TB_LOAD && nl_map_get_checked(ctx->uses, src) == 1) {
                use(ctx, src);

                Inst inst = isel_load(ctx, src, dst);
                inst.type = op;
                inst.data_type = legalize(dt);
                SUBMIT(inst);
            } else {*/
            int val = ISEL(src);
            SUBMIT(inst_op_rr(op, dt, dst, val));
            // }
            break;
        }

        // memory op
        case TB_MEMSET: {
            TB_DataType ptr_dt = TB_TYPE_I64;
            SUBMIT(inst_move(ptr_dt,     RDI, isel(ctx, n->inputs[1])));
            SUBMIT(inst_move(TB_TYPE_I8, RAX, isel(ctx, n->inputs[2])));
            SUBMIT(inst_move(ptr_dt,     RCX, isel(ctx, n->inputs[3])));

            Inst* i = alloc_inst(STOSB, TB_TYPE_VOID, 0, 3, 0);
            i->flags |= INST_REP;
            i->operands[0] = RDI;
            i->operands[1] = RAX;
            i->operands[2] = RCX;
            SUBMIT(i);
            break;
        }
        case TB_MEMCPY: {
            TB_DataType ptr_dt = TB_TYPE_I64;
            int rdi = isel(ctx, n->inputs[1]);
            int rsi = isel(ctx, n->inputs[2]);
            int rcx = isel(ctx, n->inputs[3]);

            hint_reg(ctx, rdi, RDI);
            hint_reg(ctx, rsi, RSI);
            hint_reg(ctx, rcx, RCX);
            SUBMIT(inst_move(ptr_dt, RDI, rdi));
            SUBMIT(inst_move(ptr_dt, RSI, rsi));
            SUBMIT(inst_move(ptr_dt, RCX, rcx));

            Inst* i = alloc_inst(MOVSB, TB_TYPE_VOID, 0, 3, 0);
            i->flags |= INST_REP;
            i->operands[0] = RDI;
            i->operands[1] = RSI;
            i->operands[2] = RCX;
            SUBMIT(i);
            break;
        }

        case TB_SYSCALL:
        case TB_CALL: {
            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
            const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];
            if (type == TB_SYSCALL) {
                desc = &param_descs[2];
            }

            TB_Node* ret_node = TB_NODE_GET_EXTRA_T(n, TB_NodeCall)->projs[1];
            if (!has_users(ctx, ret_node)) {
                ret_node = NULL;
            }

            TB_DataType ret_dt = ret_node ? ret_node->dt : TB_TYPE_VOID;

            int ret_val = -1;
            if (ret_node != NULL) {
                ret_val = DEF(ret_node, ret_dt);
            }

            // system calls don't count, we track this for ABI
            // and stack allocation purposes.
            if (ctx->caller_usage < n->input_count) {
                ctx->caller_usage = n->input_count;
            }

            uint32_t caller_saved_gprs = desc->caller_saved_gprs;
            uint32_t caller_saved_xmms = ~0ull >> (64 - desc->caller_saved_xmms);

            // parameter passing is separate from eval from regalloc reasons
            size_t in_count = 0;
            RegIndex ins[16];
            RegIndex param_srcs[16];

            size_t xmms_used = 0, gprs_used = 0;
            FOREACH_N(i, 2, n->input_count) {
                TB_Node* param = n->inputs[i];
                TB_DataType param_dt = param->dt;

                bool use_xmm = TB_IS_FLOAT_TYPE(param_dt) || param_dt.width;
                int reg = use_xmm ? xmms_used : gprs_used;
                if (is_sysv) {
                    if (use_xmm) {
                        xmms_used++;
                    } else {
                        gprs_used++;
                        caller_saved_gprs &= ~(1u << gprs_used);
                    }
                } else {
                    // win64 will always expend a register
                    xmms_used++;
                    gprs_used++;
                }

                // first few parameters are passed as inputs to the CALL instruction.
                // the rest are written into the stack at specific places.
                RegIndex src = isel(ctx, param);
                if (reg >= desc->gpr_count) {
                    SUBMIT(inst_op_mr(use_xmm ? FP_MOV : MOV, param->dt, RSP, GPR_NONE, SCALE_X1, reg * 8, src));
                } else {
                    int phys_reg = use_xmm ? reg : desc->gprs[reg];
                    int dst = (use_xmm ? FIRST_XMM : FIRST_GPR) + phys_reg;

                    hint_reg(ctx, src, dst);

                    param_srcs[in_count] = src;
                    ins[in_count] = dst;
                    in_count += 1;

                    if (use_xmm) {
                        caller_saved_xmms &= ~(1ull << phys_reg);
                    } else {
                        caller_saved_gprs &= ~(1ull << phys_reg);
                    }
                }
            }

            // compute the target (unless it's a symbol) before the
            // registers all need to be forcibly shuffled
            TB_Node* target = n->inputs[1];
            bool static_call = n->type == TB_CALL && target->type == TB_GET_SYMBOL_ADDRESS;

            int target_val = RSP; // placeholder really
            if (!static_call) {
                target_val = ISEL(target);
            }

            // perform last minute copies (this avoids keeping parameter registers alive for too long)
            FOREACH_N(i, 0, in_count) {
                SUBMIT(inst_move(n->inputs[2 + i]->dt, ins[i], param_srcs[i]));
            }

            // the number of float parameters is written into AL
            if (is_sysv) {
                SUBMIT(inst_op_imm(MOV, TB_TYPE_I8, RAX, xmms_used));
                ins[in_count++] = FIRST_GPR + RAX;
                caller_saved_gprs &= ~(1ull << RAX);
            }

            bool use_xmm_ret = TB_IS_FLOAT_TYPE(ret_dt) || ret_dt.width;
            if (ret_node != NULL) {
                if (use_xmm_ret) {
                    caller_saved_xmms &= ~(1ull << XMM0);
                } else {
                    caller_saved_gprs &= ~(1ull << RAX);
                }
            }

            // all these registers need to be spilled and reloaded if they're used across
            // the function call boundary... you might see why inlining could be nice to implement
            size_t clobber_count = tb_popcount(caller_saved_gprs) + tb_popcount(caller_saved_xmms);
            Inst* call_inst = alloc_inst(n->type == TB_SYSCALL ? SYSCALL : CALL, ret_dt, 1, 1 + in_count, clobber_count);

            // mark clobber list
            {
                RegIndex* clobbers = &call_inst->operands[call_inst->out_count + call_inst->in_count];
                FOREACH_N(i, 0, 16) if (caller_saved_gprs & (1u << i)) {
                    *clobbers++ = FIRST_GPR + i;
                }

                FOREACH_N(i, 0, 16) if (caller_saved_xmms & (1u << i)) {
                    *clobbers++ = FIRST_XMM + i;
                }
            }

            // return value (either XMM0 or RAX)
            call_inst->operands[0] = (use_xmm_ret ? FIRST_XMM : 0) + RAX;

            // write inputs
            RegIndex* dst_ins = &call_inst->operands[call_inst->out_count];
            if (static_call) {
                call_inst->flags |= INST_GLOBAL;
                call_inst->mem_slot = 1;
                call_inst->s = TB_NODE_GET_EXTRA_T(target, TB_NodeSymbol)->sym;
            }

            *dst_ins++ = target_val;
            memcpy(dst_ins, ins, in_count * sizeof(RegIndex));

            fence(ctx, n);
            SUBMIT(call_inst);

            // copy out return
            if (ret_node != NULL) {
                if (use_xmm_ret) {
                    hint_reg(ctx, ret_val, FIRST_XMM + XMM0);
                    SUBMIT(inst_move(ret_dt, ret_val, FIRST_XMM + XMM0));
                } else {
                    hint_reg(ctx, ret_val, RAX);
                    SUBMIT(inst_move(ret_dt, ret_val, RAX));
                }

                nl_map_put(ctx->values, ret_node, ret_val);
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
            dst = DEF(n, TB_TYPE_I32);
            SUBMIT(inst_op_zero(n->dt, dst));

            // use SETcc to convert into integer
            Cond cc = isel_cmp(ctx, n);
            SUBMIT(inst_op_r(SETO + cc, TB_TYPE_I8, dst));
            break;
        }

        case TB_BRANCH: {
            TB_Node* bb = tb_get_parent_region(n);
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
            TB_Node** succ = br->succ;

            if (br->succ_count == 2 && br->keys[0] == 0) {
                use(ctx, n->inputs[1]);

                // do fence without that extra use
                fence(ctx, n);
                fence_last(ctx, bb, n);

                // we redo it because the later code will be the one to
                // actually apply it
                fake_unuse(ctx, n->inputs[1]);
            } else {
                fence(ctx, n);
                fence_last(ctx, bb, n);
            }

            SUBMIT(alloc_inst(INST_TERMINATOR, TB_TYPE_VOID, 0, 0, 0));

            if (br->succ_count == 1) {
                if (ctx->fallthrough != succ[0]) {
                    SUBMIT(inst_jmp(succ[0]));
                }
            } else if (br->succ_count == 2) {
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
                    int key = isel(ctx, n->inputs[1]);
                    SUBMIT(inst_op_ri(CMP, dt, key, br->keys[0]));
                    SUBMIT(inst_jcc(succ[1], E));
                    SUBMIT(inst_jmp(succ[0]));
                }
            } else {
                TB_DataType dt = n->inputs[1]->dt;
                int key = isel(ctx, n->inputs[1]);

                // check if there's at most only one space between entries
                uint64_t last = br->keys[1];
                uint64_t min = last, max = last;

                bool use_jump_table = true;
                FOREACH_N(i, 2, br->succ_count) {
                    uint64_t key = br->keys[i - 1];
                    min = (min > key) ? key : min;
                    max = (max > key) ? max : key;

                    int64_t dist = key - last;
                    if (dist > 2) {
                        use_jump_table = false;
                        break;
                    }
                    last = key;
                }

                if (use_jump_table) {
                    // Simple range check
                    log_debug("Should do range check (%llu .. %llu)", min, max);
                }

                FOREACH_N(i, 1, br->succ_count) {
                    uint64_t curr_key = br->keys[i-1];

                    if (fits_into_int32(curr_key)) {
                        SUBMIT(inst_op_ri(CMP, dt, key, curr_key));
                    } else {
                        int tmp = DEF(n, dt);
                        SUBMIT(inst_op_abs(MOVABS, dt, tmp, curr_key));
                        SUBMIT(inst_op_rr(CMP, dt, key, tmp));
                    }
                    SUBMIT(inst_jcc(succ[i], E));
                }
                SUBMIT(inst_jmp(succ[0]));
            }
            break;
        }

        case TB_DEBUGBREAK:
        SUBMIT(alloc_inst(INT3, TB_TYPE_VOID, 0, 0, 0));
        break;

        case TB_UNREACHABLE:
        case TB_TRAP: {
            SUBMIT(alloc_inst(UD2, TB_TYPE_VOID, 0, 0, 0));
            break;
        }

        case TB_GET_SYMBOL_ADDRESS: {
            dst = DEF(n, n->dt);

            TB_NodeSymbol* s = TB_NODE_GET_EXTRA(n);
            SUBMIT(inst_op_global(LEA, n->dt, dst, s->sym));
            forget = true;
            break;
        }
        case TB_LOAD: {
            dst = DEF(n, n->dt);
            int mov_op = (TB_IS_FLOAT_TYPE(n->dt) || n->dt.width) ? FP_MOV : MOV;

            TB_Node* addr = n->inputs[1];

            Inst* ld_inst = isel_addr(ctx, addr, dst);
            ld_inst->type = mov_op;
            ld_inst->dt = legalize(n->dt);
            if ((ld_inst->flags & INST_MEM) == 0) {
                ld_inst->flags |= INST_MEM;
                ld_inst->mem_slot = 1;
            }

            SUBMIT(ld_inst);
            break;
        }
        case TB_STORE: {
            TB_DataType store_dt = n->inputs[2]->dt;
            int src = isel(ctx, n->inputs[2]);
            int mov_op = (TB_IS_FLOAT_TYPE(store_dt) || store_dt.width) ? FP_MOV : MOV;

            Inst* inst = NULL;
            TB_Node* addr = n->inputs[1];
            if (addr->type == TB_LOCAL) {
                use(ctx, addr);

                int pos = get_stack_slot(ctx, addr);
                inst = inst_op_mr(mov_op, store_dt, RBP, -1, SCALE_X1, pos, src);
            } else {
                int base = isel(ctx, addr);
                inst = inst_op_mr(mov_op, store_dt, base, -1, SCALE_X1, 0, src);
            }

            fence(ctx, n);
            SUBMIT(inst);
            break;
        }

        case TB_RET: {
            if (n->input_count > 1) {
                assert(n->input_count <= 2 && "We don't support multiple returns here");

                int src = isel(ctx, n->inputs[1]);

                // we don't really need a fence if we're about to exit
                // fence(ctx, n);

                // copy to return register
                TB_DataType dt = n->inputs[1]->dt;
                if (dt.type == TB_FLOAT) {
                    hint_reg(ctx, src, FIRST_XMM + XMM0);
                    tb_todo();
                } else {
                    hint_reg(ctx, src, RAX);
                    SUBMIT(inst_move(dt, RAX, src));
                }
            }

            if (ctx->fallthrough != NULL && !empty_bb(ctx->fallthrough)) {
                SUBMIT(inst_jmp(NULL));
            }
            break;
        }

        case TB_PROJ: {
            if (n->inputs[0]->type == TB_START) {
                int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;

                // past the first 4 parameters, it's all stack
                if ((ctx->target_abi == TB_ABI_WIN64   && index >= 4) ||
                    (ctx->target_abi == TB_ABI_SYSTEMV && index >= 6)) {
                    dst = DEF(n, n->dt);

                    InstType i = n->dt.type == TB_FLOAT ? FP_MOV : MOV;
                    SUBMIT(inst_op_rm(i, n->dt, dst, RBP, GPR_NONE, SCALE_X1, 16 + index*8));
                    forget = true;
                    break;
                }
            } else if (n->inputs[0]->type == TB_CALL || n->inputs[0]->type == TB_SYSCALL) {
                // do nothing, be happy
            } else {
                isel(ctx, n->inputs[0]);

                ptrdiff_t search = nl_map_get(ctx->values, n);
                assert(search >= 0);
                dst = ctx->values[search].v;
            }
            break;
        }

        default: tb_todo();
    }

    if (!forget) {
        nl_map_put(ctx->values, n, dst);
    }
    return dst;
}

#if 0
static int isel(Ctx* restrict ctx, TB_Node* n) {
    use(ctx, n);

    ptrdiff_t search = nl_map_get(ctx->values, n);
    if (search >= 0) {
        return ctx->values[search].v;
    }

    TB_NodeTypeEnum type = n->type;
    int dst = -1;
    bool forget = false;

    switch (type) {
        case TB_SELECT: {
            dst = DEF(n, REG_CLASS_GPR);

            int lhs = ISEL(n->inputs[2]);
            int rhs = ISEL(n->inputs[3]);

            Cond cc = isel_cmp(ctx, n->inputs[1]);
            SUBMIT(inst_rr(CMOVO + cc, n->dt, dst, rhs, lhs));
            break;
        }

        case TB_MULPAIR: {
            dst = DEF(n, REG_CLASS_GPR);

            int fake_dst = DEF(n, REG_CLASS_GPR);

            // mov rax, lhs
            int lhs = ISEL(n->inputs[1]);
            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, fake_dst);
            SUBMIT(inst_copy(n->dt, rax, lhs));

            int rhs = ISEL(n->inputs[2]);
            int rdx = DEF_FORCED(n, REG_CLASS_GPR, RDX, fake_dst);
            SUBMIT(inst_def(rdx));

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

            int rax = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
            int rdx = DEF_FORCED(n, REG_CLASS_GPR, RDX, -1);

            TB_DataType dt = n->dt;
            assert(dt.type == TB_INT);

            int op = INST_COPY;
            if (dt.data <= 8)       op = is_signed ? MOVSXB : MOVZXB;
            else if (dt.data <= 16) op = is_signed ? MOVSXW : MOVZXW;

            // division is scaled up to 32bit
            if (dt.data < 32) dt.data = 32;

            // mov rax, lhs
            int lhs = ISEL(n->inputs[1]);
            SUBMIT(inst_r(op, dt, rax, lhs));

            int rhs = ISEL(n->inputs[2]);
            if (n->dt.data < 32) {
                // add cast
                int new_rhs = DEF(n->inputs[2], REG_CLASS_GPR);
                SUBMIT(inst_r(op, TB_TYPE_I32, new_rhs, rhs));
                rhs = USE(new_rhs);
            }

            // if signed:
            //   cqo/cdq (sign extend RAX into RDX)
            // else:
            //   xor rdx, rdx
            if (is_signed) {
                SUBMIT(inst_def(rdx));
                SUBMIT(inst_u(CAST, dt));
            } else {
                SUBMIT(inst_i(MOV, dt, rdx, 0));
            }
            SUBMIT(inst_r(is_signed ? IDIV : DIV, dt, -1, rhs));
            SUBMIT(inst_use(rax));
            SUBMIT(inst_use(rdx));

            dst = DEF(n, REG_CLASS_GPR);
            SUBMIT(inst_copy(dt, dst, USE(is_div ? rax : rdx)));
            break;
        }
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_ROL:
        case TB_ROR: {
            const static InstType ops[] = { SHL, SHR, SAR, ROL, ROR };
            InstType op = ops[type - TB_SHL];

            dst = DEF(n, REG_CLASS_GPR);

            int32_t x;
            if (try_for_imm8(ctx, n->inputs[2], &x)) {
                use(ctx, n->inputs[2]);

                int lhs = ISEL(n->inputs[1]);
                SUBMIT(inst_ri(op, n->dt, dst, lhs, x));
            } else {
                // the shift operations need their right hand side in CL (RCX's low 8bit)
                int lhs = ISEL(n->inputs[1]);
                int rhs = ISEL(n->inputs[2]); // TODO(NeGate): hint into RCX

                int cl = DEF_FORCED(n, REG_CLASS_GPR, RCX, -1);
                SUBMIT(inst_copy(n->dt, cl, rhs));
                SUBMIT(inst_rr(op, n->dt, dst, lhs, USE(cl)));
            }
            break;
        }

        case TB_INT2PTR:
        case TB_SIGN_EXT:
        case TB_ZERO_EXT: {
            TB_Node* src = n->inputs[1];

            TB_DataType src_dt = src->dt;
            bool sign_ext = (type == TB_SIGN_EXT);
            int bits_in_type = src_dt.type == TB_PTR ? 64 : src_dt.data;

            dst = DEF(n, REG_CLASS_GPR);

            int32_t imm;
            if (try_for_imm32(ctx, src, &imm)) {
                #define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
                use(ctx, src);

                uint64_t src = imm;
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

            TB_DataType dt = n->dt;

            int op = MOV;
            if (bits_in_type <= 8) op = sign_ext ? MOVSXB : MOVZXB;
            else if (bits_in_type <= 16) op = sign_ext ? MOVSXW : MOVZXW;
            else if (bits_in_type <= 32) {
                if (sign_ext) op = MOVSXD;
                else dt = src_dt;
            } else if (bits_in_type <= 64) op = MOV;
            else tb_todo();

            if (src->type == TB_LOAD && nl_map_get_checked(ctx->uses, src) == 1) {
                use(ctx, src);

                Inst inst = isel_load(ctx, src, dst);
                inst.type = op;
                inst.data_type = legalize(dt);
                SUBMIT(inst);
            } else {
                int val = ISEL(src);
                SUBMIT(inst_r(op, dt, dst, val));
            }
            break;
        }
        case TB_PTR2INT:
        case TB_TRUNCATE: {
            int src = ISEL(n->inputs[1]);

            if (n->dt.type == TB_FLOAT) {
                dst = DEF(n, REG_CLASS_XMM);
                SUBMIT(inst_r(FP_CVT, n->inputs[1]->dt, dst, src));
            } else {
                dst = DEF(n, REG_CLASS_GPR);
                SUBMIT(inst_copy(n->dt, dst, src));
            }
            break;
        }

        case TB_UNREACHABLE:
        case TB_TRAP: {
            SUBMIT(inst_nullary(UD2));
            break;
        }

        case TB_BRANCH: {
            TB_Node* bb = tb_get_parent_region(n);
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
            TB_Node** succ = r->succ;

            if (r->succ_count == 2 && br->keys[0] == 0) {
                use(ctx, n->inputs[1]);

                // do fence without that extra use
                fence(ctx, n);
                fence_last(ctx, bb, n);

                // we redo it because the later code will be the one to
                // actually apply it
                fake_unuse(ctx, n->inputs[1]);
            } else {
                fence(ctx, n);
                fence_last(ctx, bb, n);
            }

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

                // check if there's at most only one space between entries
                uint64_t last = br->keys[1];
                uint64_t min = last, max = last;

                bool use_jump_table = true;
                FOREACH_N(i, 2, r->succ_count) {
                    uint64_t key = br->keys[i - 1];
                    min = (min > key) ? key : min;
                    max = (max > key) ? max : key;

                    int64_t dist = key - last;
                    if (dist > 2) {
                        use_jump_table = false;
                        break;
                    }
                    last = key;
                }

                if (use_jump_table) {
                    // Simple range check
                    log_debug("Should do range check (%llu .. %llu)", min, max);
                }

                FOREACH_N(i, 1, r->succ_count) {
                    uint64_t curr_key = br->keys[i-1];

                    if (fits_into_int32(curr_key)) {
                        SUBMIT(inst_ri(CMP, dt, -1, USE(key), curr_key));
                    } else {
                        int tmp = DEF(n, REG_CLASS_GPR);
                        SUBMIT(inst_i64(MOVABS, dt, tmp, curr_key));
                        SUBMIT(inst_rr(CMP, dt, -1, USE(key), USE(tmp)));
                    }
                    SUBMIT(inst_jcc(succ[i], E));
                }
                SUBMIT(inst_jmp(succ[0]));
            }
            break;
        }

        // the good thing is that safepoints don't need live ranges or anything
        case TB_SAFEPOINT: {
            TB_SafepointKey* key = &ctx->safepoints[TB_NODE_GET_EXTRA_T(n, TB_NodeSafepoint)->id];
            TB_Safepoint* restrict sp = malloc(sizeof(TB_Safepoint) + (n->input_count * sizeof(int32_t)));

            FOREACH_N(i, 1, n->input_count) {
                int src = ISEL(n->inputs[i]);
                SUBMIT(inst_use(src));
            }

            key->sp = sp;
            break;
        }

        case TB_SYSCALL:
        case TB_CALL: {
            static const struct ParamDescriptor {
                int gpr_count;
                int xmm_count;
                uint16_t caller_saved_xmms; // XMM0 - XMMwhatever
                uint16_t caller_saved_gprs; // bitfield

                GPR gprs[6];
            } param_descs[] = {
                // win64
                { 4, 4, 6, WIN64_ABI_CALLER_SAVED,  { RCX, RDX, R8,  R9,  0,  0 } },
                // system v
                { 6, 4, 5, SYSV_ABI_CALLER_SAVED,    { RDI, RSI, RDX, RCX, R8, R9 } },
                // syscall
                { 6, 4, 5, SYSCALL_ABI_CALLER_SAVED, { RDI, RSI, RDX, R10, R8, R9 } },
            };

            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
            const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];
            if (type == TB_SYSCALL) {
                desc = &param_descs[2];
            }

            // system calls don't count, we track this for ABI
            // and stack allocation purposes.
            if (ctx->caller_usage < n->input_count) {
                ctx->caller_usage = n->input_count;
            }

            // generate clones of each parameter which live until the CALL instruction executes
            int fake_dst;
            if (n->dt.type == TB_FLOAT) {
                fake_dst = DEF_FORCED(n, REG_CLASS_XMM, XMM0, -1);
            } else {
                fake_dst = DEF_FORCED(n, REG_CLASS_GPR, RAX, -1);
            }
            uint32_t caller_saved_gprs = desc->caller_saved_gprs;

            // parameter passing is separate from eval from regalloc reasons
            size_t xmms_used = 0;
            FOREACH_REVERSE_N(i, 2, n->input_count) {
                TB_Node* param = n->inputs[i];
                TB_DataType param_dt = param->dt;

                // signed 32bit immediates get love
                int32_t imm;
                if (try_for_imm32(ctx, param, &imm)) {
                    use(ctx, param);

                    if (i - 2 < desc->gpr_count) {
                        int param_def = DEF_FORCED(param, REG_CLASS_GPR, desc->gprs[i - 2], fake_dst);
                        SUBMIT(inst_i(MOV, param->dt, param_def, imm));
                    } else {
                        SUBMIT(inst_mi(MOV, param->dt, RSP, GPR_NONE, SCALE_X1, (i - 2) * 8, imm));
                    }
                    continue;
                }

                int src = isel(ctx, param);
                if (TB_IS_FLOAT_TYPE(param_dt) || param_dt.width) {
                    int xmm_id = is_sysv ? xmms_used++ : i - 2;
                    if (xmm_id == 0 && n->dt.type == TB_FLOAT) {
                        // xmm0 is in use, let's not make another forced GPR
                        SUBMIT(inst_copy(param->dt, fake_dst, USE(src)));
                    } else if (xmm_id < desc->gpr_count) {
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

            size_t clobber_cap = tb_popcount(caller_saved_gprs) + desc->caller_saved_xmms;
            Clobbers* clobbers = tb_arena_alloc(&tb__arena, sizeof(Clobbers) + (clobber_cap * sizeof(MachineReg)));

            // mark all the clobbers
            size_t clobber_count = 0;
            FOREACH_N(i, 0, 16) if (caller_saved_gprs & (1u << i)) {
                clobbers->_[clobber_count++] = (MachineReg){ REG_CLASS_GPR, i };
            }

            FOREACH_N(i, 0, desc->caller_saved_xmms) {
                clobbers->_[clobber_count++] = (MachineReg){ REG_CLASS_XMM, i };
            }

            assert(clobber_count == clobber_cap);
            clobbers->count = clobber_count;
            ctx->defs[fake_dst].clobbers = clobbers;

            TB_Node* target = n->inputs[1];
            if (type == TB_SYSCALL) {
                int num = ISEL(target);
                fence(ctx, n);

                SUBMIT(inst_copy(n->dt, fake_dst, num));
                SUBMIT(inst_nullary(SYSCALL));
            } else {
                fence(ctx, n);

                // the number of float parameters is written into AL
                if (is_sysv) {
                    SUBMIT(inst_i(MOV, TB_TYPE_I64, RAX, xmms_used));
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
                if (n->dt.type == TB_FLOAT) {
                    dst = DEF_HINTED(n, REG_CLASS_XMM, XMM0);
                    SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
                } else {
                    dst = DEF_HINTED(n, REG_CLASS_GPR, RAX);
                    SUBMIT(inst_copy(n->dt, dst, USE(fake_dst)));
                }
            }
            break;
        }

        case TB_MEMSET: {
            int rax = DEF_FORCED(n->inputs[2], REG_CLASS_GPR, RAX, -1);
            int rcx = DEF_FORCED(n->inputs[3], REG_CLASS_GPR, RCX, rax);
            int rdi = DEF_FORCED(n->inputs[1], REG_CLASS_GPR, RDI, rax);

            // clobber inputs
            Clobbers* clobbers = tb_arena_alloc(&tb__arena, sizeof(Clobbers) + (3 * sizeof(MachineReg)));
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
            SUBMIT(inst_use(rdi));
            SUBMIT(inst_use(rcx));
            SUBMIT(inst_use(rax));
            break;
        }
        case TB_MEMCPY: {
            int rsi = DEF_FORCED(n->inputs[2], REG_CLASS_GPR, RSI, -1);
            int rdi = DEF_FORCED(n->inputs[1], REG_CLASS_GPR, RDI, rsi);
            int rcx = DEF_FORCED(n->inputs[3], REG_CLASS_GPR, RCX, rsi);

            // clobber inputs
            Clobbers* clobbers = tb_arena_alloc(&tb__arena, sizeof(Clobbers) + (3 * sizeof(MachineReg)));
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
            SUBMIT(inst_use(rdi));
            SUBMIT(inst_use(rcx));
            SUBMIT(inst_use(rsi));
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
                    forget = true;
                    break;
                }
            }

            ptrdiff_t search = nl_map_get(ctx->values, n);
            assert(search >= 0);

            dst = ctx->values[search].v;
            break;
        }

        case TB_POISON: {
            dst = DEF(n, REG_CLASS_GPR);
            break;
        }

        case TB_NULL: break;
        default: tb_todo();
    }

    if (!forget) {
        nl_map_put(ctx->values, n, dst);
    }
    return dst;
}
#endif

static void print_operand(TB_CGEmitter* restrict e, Val* v, TB_X86_DataType dt) {
    static const char* type_names[] = {
        "ptr",
        "byte",    "word",    "dword",   "qword",
        "pbyte",   "pword",   "pdword",  "pqword",
        "xmmword", "xmmword", "xmmword", "xmmword",
        "xmmword"
    };

    switch (v->type) {
        case VAL_GPR: {
            assert(v->reg >= 0 && v->reg < 16);
            EMITA(e, "%s", GPR_NAMES[v->reg]);
            break;
        }
        case VAL_XMM: EMITA(e, "XMM%d", v->reg); break;
        case VAL_IMM: EMITA(e, "%d", v->imm); break;
        case VAL_ABS: EMITA(e, "%#"PRId64, v->abs); break;
        case VAL_MEM: {
            EMITA(e, "%s ", type_names[dt]);

            if (v->index == -1) {
                EMITA(e, "[%s", GPR_NAMES[v->reg]);
            } else {
                EMITA(e, "[%s + %s*%d", GPR_NAMES[v->reg], GPR_NAMES[v->index], 1u << v->scale);
            }

            if (v->imm != 0) {
                EMITA(e, " + %d", v->imm);
            }
            EMITA(e, "]");
            break;
        }
        case VAL_GLOBAL: {
            const TB_Symbol* target = v->symbol;
            if (*target->name == 0) {
                if (v->imm == 0) {
                    EMITA(e, "sym%p", target);
                } else {
                    EMITA(e, "[sym%p + %d]", target, v->imm);
                }
            } else {
                if (v->imm == 0) {
                    EMITA(e, "%s", target->name);
                } else {
                    EMITA(e, "[%s + %d]", target->name, v->imm);
                }
            }
            break;
        }
        case VAL_LABEL: {
            if (v->target == NULL) {
                EMITA(e, ".ret");
            } else {
                EMITA(e, "L%p", (TB_Node*) v->target);
            }
            break;
        }
        default: tb_todo();
    }
}

static void inst2_print(TB_CGEmitter* restrict e, InstType type, Val* dst, Val* src, TB_X86_DataType dt) {
    if (dt == TB_X86_TYPE_XMMWORD) {
        dt = TB_X86_TYPE_SSE_PD;
    }

    if (e->emit_asm) {
        EMITA(e, "  %s", inst_table[type].mnemonic);
        if (dt >= TB_X86_TYPE_SSE_SS && dt <= TB_X86_TYPE_SSE_PD) {
            static const char suffixes[4][3] = { "ss", "sd", "ps", "pd" };
            EMITA(e, "%s ", suffixes[dt - TB_X86_TYPE_SSE_SS]);
        } else {
            EMITA(e, " ");
        }
        print_operand(e, dst, dt);
        EMITA(e, ", ");
        print_operand(e, src, dt);
        EMITA(e, "\n");
    }

    if (dt >= TB_X86_TYPE_SSE_SS && dt <= TB_X86_TYPE_SSE_PD) {
        inst2sse(e, type, dst, src, dt);
    } else {
        inst2(e, type, dst, src, dt);
    }
}

static void inst1_print(TB_CGEmitter* restrict e, int type, Val* src, TB_X86_DataType dt) {
    if (e->emit_asm) {
        EMITA(e, "  %s ", inst_table[type].mnemonic);
        print_operand(e, src, dt);
        EMITA(e, "\n");
    }
    inst1(e, type, src, dt);
}

static int resolve_interval(Ctx* restrict ctx, Inst* inst, int i, Val* val) {
    LiveInterval* interval = &ctx->intervals[inst->operands[i]];

    if ((inst->flags & (INST_MEM | INST_GLOBAL)) && i == inst->mem_slot) {
        tb_assert(interval->spill <= 0, "cannot use spilled value for memory operand");
        if (inst->flags & INST_MEM) {
            *val = (Val){
                .type = VAL_MEM,
                .reg  = interval->assigned,
                .index = GPR_NONE,
                .scale = inst->scale,
                .imm = inst->disp,
            };

            if (inst->flags & INST_INDEXED) {
                interval = &ctx->intervals[inst->operands[i + 1]];
                tb_assert(interval->spill <= 0, "cannot use spilled value for memory operand");

                val->index = interval->assigned;
                return 2;
            } else {
                return 1;
            }
        } else {
            *val = val_global(inst->s);
            return 1;
        }
    }

    if (interval->spill > 0) {
        *val = (Val){
            .type = VAL_MEM,
            .reg = RBP,
            .index = GPR_NONE,
            .imm = -interval->spill,
        };
    } else {
        *val = (Val){
            .type = interval->reg_class == REG_CLASS_XMM ? VAL_XMM : VAL_GPR,
            .reg  = interval->assigned
        };
    }

    return 1;
}

static void emit_code(Ctx* restrict ctx, TB_FunctionOutput* restrict func_out) {
    TB_CGEmitter* e = &ctx->emit;

    // resolve stack usage
    {
        size_t caller_usage = ctx->caller_usage;
        if (ctx->target_abi == TB_ABI_WIN64 && caller_usage > 0 && caller_usage < 4) {
            caller_usage = 4;
        }

        size_t usage = ctx->stack_usage + (caller_usage * 8);

        // Align stack usage to 16bytes + 8 to accommodate for the RIP being pushed by CALL
        ctx->stack_usage = align_up(usage, 16);
    }

    // emit prologue
    func_out->prologue_length = emit_prologue(ctx);
    EMITA(&ctx->emit, ".body:\n");

    for (Inst* restrict inst = ctx->first; inst; inst = inst->next) {
        size_t in_base = inst->out_count;
        InstCategory cat = inst->type > 1024 ? INST_BINOP : inst_table[inst->type].cat;

        if (0) {
            EMITA(e, "  \x1b[32m# %s t=%d { outs:", inst->type < 1024 ? inst_table[inst->type].mnemonic : "???", inst->time);
            FOREACH_N(i, 0, inst->out_count) {
                EMITA(e, " v%d", inst->operands[i]);
            }
            EMITA(e, ", ins: ");
            FOREACH_N(i, inst->out_count, inst->out_count + inst->in_count) {
                EMITA(e, " v%d", inst->operands[i]);
            }
            EMITA(e, "}\x1b[0m\n");
        }

        if (inst->type == INST_ENTRY || inst->type == INST_TERMINATOR) {
            // does nothing
        } else if (inst->type == INST_LABEL) {
            TB_Node* bb = inst->n;
            uint32_t pos = GET_CODE_POS(&ctx->emit);
            tb_resolve_rel32(&ctx->emit, &nl_map_get_checked(ctx->emit.labels, bb), pos);

            if (bb != ctx->f->start_node) {
                EMITA(e, "L%p:\n", bb);
            }
        } else if (inst->type == INST_EPILOGUE) {
            // return label goes here
            EMITA(&ctx->emit, ".ret:\n");
            tb_resolve_rel32(&ctx->emit, &ctx->emit.return_label, GET_CODE_POS(&ctx->emit));
        } else if (inst->type == INST_LINE) {
            TB_Function* f = ctx->f;
            TB_Attrib* loc = inst->a;
            EMITA(e, "  \x1b[33m# loc %s %"PRIu64"\x1b[0m\n", loc->loc.file->path, loc->loc.line);

            TB_Location l = {
                .file = loc->loc.file,
                .line = loc->loc.line,
                .column = loc->loc.column,
                .pos = GET_CODE_POS(&ctx->emit)
            };
            dyn_array_put(ctx->locations, l);
            continue;
        } else if (cat == INST_BYTE || cat == INST_BYTE_EXT) {
            if (inst->flags & INST_REP) EMIT1(e, 0xF3);

            EMITA(e, "  %s\n", inst_table[inst->type].mnemonic);
            inst0(e, inst->type, inst->dt);
        } else if (inst->type == INST_ZERO) {
            Val dst;
            resolve_interval(ctx, inst, 0, &dst);

            bool is_xmm = inst->dt >= TB_X86_TYPE_PBYTE && inst->dt <= TB_X86_TYPE_XMMWORD;
            inst2_print(e, is_xmm ? FP_XOR : XOR, &dst, &dst, inst->dt);
        } else if (inst->type >= JMP && inst->type <= JG) {
            Val target;
            if (inst->flags & INST_NODE) {
                target = val_label(inst->n);
            } else if (inst->flags & INST_GLOBAL) {
                target = val_global(inst->s);
            } else {
                tb_todo();
            }

            inst1_print(e, inst->type, &target, inst->dt);
        } else if (inst->type == CALL || inst->type == SYSCALL) {
            bool is_sysv = (ctx->target_abi == TB_ABI_SYSTEMV);
            const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];
            if (inst->type == SYSCALL) {
                desc = &param_descs[2];
            }

            Val target;
            size_t i = resolve_interval(ctx, inst, in_base, &target);
            inst1_print(&ctx->emit, CALL, &target, TB_X86_TYPE_QWORD);
        } else {
            int mov_op = inst->dt >= TB_X86_TYPE_PBYTE && inst->dt <= TB_X86_TYPE_XMMWORD ? FP_MOV : MOV;

            // resolve output
            Val out;
            int i = 0;
            if (inst->out_count == 1) {
                i += resolve_interval(ctx, inst, i, &out);
                assert(i == in_base);
            } else {
                i = in_base;
            }

            // first parameter
            bool ternary = false;
            if (inst->in_count > 0) {
                Val lhs;
                i += resolve_interval(ctx, inst, i, &lhs);

                ternary = (i < in_base + inst->in_count) || (inst->flags & (INST_IMM | INST_ABS));
                if (ternary && inst->type == IMUL && (inst->flags & INST_IMM)) {
                    // there's a special case for ternary IMUL r64, r/m64, imm32
                    if (e->emit_asm) {
                        EMITA(e, "  imul ");
                        print_operand(e, &out, inst->dt);
                        EMITA(e, ", ");
                        print_operand(e, &lhs, inst->dt);
                        EMITA(e, ", %d\n", inst->imm);
                    }

                    inst2(e, IMUL3, &out, &lhs, inst->dt);
                    EMIT4(e, inst->imm);
                    continue;
                }

                if (inst->out_count == 0) {
                    out = lhs;
                } else if (inst->type == IDIV || inst->type == DIV) {
                    inst1_print(e, inst->type, &lhs, inst->dt);
                    continue;
                } else {
                    if (ternary || inst->type == MOV || inst->type == FP_MOV) {
                        if (!is_value_match(&out, &lhs)) {
                            inst2_print(e, mov_op, &out, &lhs, inst->dt);
                        }
                    } else {
                        inst2_print(e, inst->type, &out, &lhs, inst->dt);
                    }
                }
            }

            // unary ops
            if (cat == INST_UNARY || cat == INST_UNARY_EXT) {
                inst1_print(e, inst->type, &out, inst->dt);
                continue;
            }

            if (inst->flags & INST_IMM) {
                Val rhs = val_imm(inst->imm);
                inst2_print(e, inst->type, &out, &rhs, inst->dt);
            } else if (inst->flags & INST_ABS) {
                Val rhs = val_abs(inst->abs);
                inst2_print(e, inst->type, &out, &rhs, inst->dt);
            } else if (ternary) {
                Val rhs;
                i += resolve_interval(ctx, inst, i, &rhs);

                if (inst->type != MOV || (inst->type == MOV && !is_value_match(&out, &rhs))) {
                    inst2_print(e, inst->type, &out, &rhs, inst->dt);
                }
            }
        }
    }

    func_out->epilogue_length = emit_epilogue(ctx);
}

static void emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t stack_usage) {
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

static size_t emit_prologue(Ctx* restrict ctx) {
    uint64_t stack_usage = ctx->stack_usage;
    if (stack_usage <= 16) {
        return 0;
    }

    TB_CGEmitter* e = &ctx->emit;

    // push rbp
    if (stack_usage > 0) {
        EMITA(e, "  push RBP\n");
        EMIT1(e, 0x50 + RBP);

        // mov rbp, rsp
        EMITA(e, "  mov RBP, RSP\n");
        EMIT1(e, rex(true, RSP, RBP, 0));
        EMIT1(e, 0x89);
        EMIT1(e, mod_rx_rm(MOD_DIRECT, RSP, RBP));
    }

    // if there's more than 4096 bytes of stack, we need to insert a chkstk
    if (stack_usage >= 4096) {
        assert(ctx->f->super.module->chkstk_extern);
        Val sym = val_global(ctx->f->super.module->chkstk_extern);
        Val imm = val_imm(stack_usage);
        Val rax = val_gpr(RAX);
        Val rsp = val_gpr(RSP);

        inst2_print(e, MOV, &rax, &imm, TB_X86_TYPE_DWORD);
        inst1_print(e, CALL, &sym, TB_X86_TYPE_QWORD);
        inst2_print(e, SUB, &rsp, &rax, TB_X86_TYPE_QWORD);
    } else if (stack_usage > 0) {
        EMITA(e, "  sub RSP, %d\n", stack_usage);
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

    return e->count;
}

static size_t emit_epilogue(Ctx* restrict ctx) {
    uint64_t saved = ctx->regs_to_save, stack_usage = ctx->stack_usage;
    TB_CGEmitter* e = &ctx->emit;

    if (stack_usage <= 16) {
        EMITA(e, "  ret\n");
        EMIT1(e, 0xC3);
        return 1;
    }

    size_t start = e->count;

    // add rsp, N
    if (stack_usage > 0) {
        EMITA(e, "  add RSP, %d\n", stack_usage);
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
        EMITA(e, "  pop RBP\n");
        EMIT1(&ctx->emit, 0x58 + RBP);
    }

    EMITA(e, "  ret\n");
    EMIT1(&ctx->emit, 0xC3);
    return ctx->emit.count - start;
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
                    size_t actual_pos = out_f->code_pos + patch->pos + 4;

                    uint32_t p = ((TB_Function*) patch->target)->output->code_pos - actual_pos;
                    memcpy(&out_f->code[patch->pos], &p, sizeof(uint32_t));

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
    .compile_function   = compile_function,
};
