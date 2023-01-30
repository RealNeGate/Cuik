#include "../tb_internal.h"
#include "x64.h"

typedef struct Ctx Ctx;

static void jmp(TB_CGEmitter* restrict e, int label);
static void ret_jmp(TB_CGEmitter* restrict e);

#define GAD_REG_PRIORITIES { \
    { RAX, RCX, RDX, R8, R9, R10, R11, RDI, RSI, RBX, R12, R13, R14, R15 }, \
    { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15 } \
}

#if 1
#define GAD_EXTRA_CTX {   \
    int unused;           \
}
#else
#define GAD_EXTRA_CTX {   \
    struct X64_Tile {     \
        TB_Reg  mapping;  \
        GPR     base  : 8;\
        GPR     index : 8;\
        Scale   scale : 8;\
        int32_t disp;     \
    } tile;               \
}
#endif

#define GAD_FN(name) x64v2_ ## name // all exported symbols have this prefix
#define GAD_NUM_REG_FAMILIES 2
#define GAD_MAKE_STACK_SLOT(ctx, f, r_, pos) (Val){ VAL_MEM, .r = (r_), .mem = { .base = RBP, .index = GPR_NONE, .disp = (pos) } }
#define GAD_VAL Val
#include "../codegen/generic_addrdesc.h"
#include "x64_emitter.h"
// #include "x64_proepi.h"

#ifdef TB_COMPILE_TESTS
#include "x64_tests.h"
#endif

size_t x64_emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage);
size_t x64_emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage);
void x64_emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t saved, uint64_t stack_usage);

enum {
    X64_REG_CLASS_GPR,
    X64_REG_CLASS_XMM
};

// a valid type that the x64 backend can eat along with
typedef struct {
    TB_DataType dt;
    uint64_t mask;
} LegalInt;

typedef enum {
    SHL, SHR, SAR
} ShiftType;

static void x64v2_goto(Ctx* restrict ctx, TB_Label label) {
    jmp(&ctx->emit, label);
}

static void x64v2_ret_jmp(Ctx* restrict ctx) {
    ret_jmp(&ctx->emit);
}

static void emit_shift_gpr_imm(Ctx* restrict ctx, TB_Function* f, ShiftType type, GPR dst, uint8_t imm, TB_DataType dt) {
    int bits_in_type = dt.type == TB_PTR ? 64 : dt.data;

    if (bits_in_type <= 16) EMIT1(&ctx->emit, 0x66);
    EMIT1(&ctx->emit, rex(bits_in_type > 32, 0x00, dst, 0x00));
    EMIT1(&ctx->emit, (bits_in_type <= 8 ? 0xC0 : 0xC1));

    uint8_t op = 0x00;
    switch (type) {
        case SHL: op = 0x04; break;
        case SHR: op = 0x05; break;
        case SAR: op = 0x07; break;
        default: tb_unreachable();
    }
    EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, op, dst));
    EMIT1(&ctx->emit, imm);
}

// returns a mask to remove the "out of bounds" bits
static LegalInt legalize_int(TB_DataType dt) {
    if (dt.type != TB_INT) return (LegalInt){ dt, 0 };

    int bits;
    if (!TB_NEXT_BIGGEST(&bits, dt.data, 8, 16, 32, 64)) {
        // support bigger types
        tb_todo();
    }

    int original_bits = dt.data;
    uint64_t mask = ~UINT64_C(0) >> (64 - original_bits);

    // we don't need the mask if it lines up nicely with machine sizes
    if (original_bits == 8 || original_bits == 16 || original_bits == 32 || original_bits == 64) {
        mask = 0;
    }

    dt.data = bits;
    return (LegalInt){ dt, mask };
}

static uint8_t legalize_float(TB_DataType dt) {
    assert(dt.type == TB_FLOAT);

    uint8_t flags = 0;
    if (dt.data == TB_FLT_64) {
        assert(dt.width == 0 || dt.width == 1);
        flags |= INST2FP_DOUBLE;
    } else if (dt.data == TB_FLT_32) {
        assert(dt.width == 0 || dt.width == 2);
    } else {
        tb_unreachable();
    }

    flags |= (dt.width ? INST2FP_PACKED : 0);
    return flags;
}

static bool fits_into_int32(uint64_t x) {
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            // round up bits to a byte
            bool is_big_int = dt.data > 64;
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

            return ((bits+7) / 8) << dt.width;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            return s << dt.width;
        }
        case TB_PTR: {
            return 8;
        }
        default: {
            tb_unreachable();
            return 0;
        }
    }
}

static void x64v2_mask_out(Ctx* restrict ctx, TB_Function* f, const LegalInt l, const GAD_VAL* dst) {
    if (fits_into_int32(l.mask)) {
        Val mask = val_imm(l.dt, l.mask);
        INST2(AND, dst, &mask, l.dt);
    } else {
        tb_todo();
        /*GAD_VAL tmp = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);

        // MOVABS     REX.W B8+r imm64
        EMIT1(&ctx->emit, tmp.gpr >= 8 ? 0x49 : 0x48), EMIT1(&ctx->emit, 0xB8 + (tmp.gpr & 7)), EMIT8(&ctx->emit, l.mask);
        INST2(AND, dst, &tmp, l.dt);

        GAD_FN(unlock_register)(ctx, f, X64_REG_CLASS_GPR, tmp.gpr);*/
    }
}

static size_t x64v2_resolve_stack_usage(Ctx* restrict ctx, TB_Function* f, size_t stack_usage, size_t caller_usage) {
    size_t usage = stack_usage + (caller_usage * 8);

    // Align stack usage to 16bytes and add 8 bytes for the return address
    if (usage > 16) {
        usage = align_up(usage + 8, 16) + 8;
    } else {
        usage = 8;
    }

    return usage;
}

static void x64v2_resolve_local_patches(Ctx* restrict ctx, TB_Function* f) {
    FOREACH_N(i, 0, ctx->ret_patch_count) {
        uint32_t pos = ctx->ret_patches[i];
        PATCH4(&ctx->emit, pos, GET_CODE_POS(&ctx->emit) - (pos + 4));
    }

    FOREACH_N(i, 0, ctx->label_patch_count) {
        uint32_t pos = ctx->label_patches[i].pos;
        uint32_t target_lbl = ctx->label_patches[i].target_lbl;

        PATCH4(&ctx->emit, pos, ctx->emit.labels[target_lbl] - (pos + 4));
    }
}

static void x64v2_resolve_params(Ctx* restrict ctx, TB_Function* f, GAD_VAL* values) {
    bool is_sysv = (f->super.module->target_abi == TB_ABI_SYSTEMV);
    const TB_FunctionPrototype* restrict proto = f->prototype;

    size_t param_count = proto->param_count;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_PARAM) {
                size_t i = n->param.id;
                TB_DataType dt = n->dt;

                // Allocate space in stack
                assert(get_data_type_size(dt) <= 8 && "Parameter too big");

                int startpoint = ctx->ordinal[r];
                printf("r%u (t=%d .. %d)\n", r, startpoint, ctx->ordinal[ctx->intervals[r]]);
                if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
                    // xmm parameters
                    if (i < 4) {
                        // GAD_FN(reserve_register)(ctx, f, r, X64_REG_CLASS_XMM, i);
                    } else {
                        // GAD_FN(force_stack)(ctx, f, r, 16 + (i * 8));
                    }
                    tb_todo();
                } else {
                    // gpr parameters
                    if (is_sysv) {
                        if (i < 6) {
                            ctx->active[ctx->active_count++] = r;
                            values[r] = (GAD_VAL){
                                .type = GAD_VAL_REGISTER + X64_REG_CLASS_GPR,
                                .r = r, .dt = dt, .reg = SYSV_GPR_PARAMETERS[i],
                            };
                        }
                    } else if (i < 4) {
                        ctx->active[ctx->active_count++] = r;
                        values[r] = (GAD_VAL){
                            .type = GAD_VAL_REGISTER + X64_REG_CLASS_GPR,
                            .r = r, .dt = dt, .reg = WIN64_GPR_PARAMETERS[i],
                        };
                    } else {
                        values[r] = GAD_MAKE_STACK_SLOT(ctx, f, r, 16 + (i * 8));
                        // GAD_FN(force_stack)(ctx, f, r, );
                    }

                    printf("  assign to %s\n", GPR_NAMES[values[r].reg]);
                    set_put(&ctx->free_regs[X64_REG_CLASS_GPR], values[r].reg);
                }

                // short circuit
                param_count -= 1;
                if (param_count == 0) break;
            }
        }
    }

    if (proto->has_varargs) {
        const GPR* parameter_gprs = is_sysv ? SYSV_GPR_PARAMETERS : WIN64_GPR_PARAMETERS;

        // spill the rest of the parameters (assumes they're all in the GPRs)
        size_t gpr_count = is_sysv ? 6 : 4;
        size_t extra_param_count = proto->param_count > gpr_count ? 0 : gpr_count - proto->param_count;

        FOREACH_N(i, 0, extra_param_count) {
            size_t param_num = proto->param_count + i;

            Val dst = val_stack(TB_TYPE_I64, 16 + (param_num * 8));
            Val src = val_gpr(TB_TYPE_I64, parameter_gprs[param_num]);
            INST2(MOV, &dst, &src, TB_TYPE_I64);
        }
    }
}

static void x64v2_resolve_stack_slot(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    if (n->type == TB_PARAM_ADDR) {
        TB_Reg r = n->param_addr.param;
        int id = f->nodes[n->param_addr.param].param.id;
        TB_DataType dt = n->dt;

        GAD_VAL dst = GAD_MAKE_STACK_SLOT(ctx, f, r, 16 + (id * 8));
        if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
            tb_todo();
        } else {
            // don't keep a reference of it in GPR if it's in memory
            INST2(MOV, &dst, &ctx->values[r], TB_TYPE_I64);
            tb_todo();
        }

        ctx->stack_usage += 8;
    } else if (n->type == TB_LOCAL) {
        TB_Reg r = n - f->nodes;

        ctx->stack_usage = align_up(ctx->stack_usage + n->local.size, n->local.alignment);
        ctx->values[r] = GAD_MAKE_STACK_SLOT(ctx, f, r, -ctx->stack_usage);
    }
}

static void x64v2_initial_reg_alloc(Ctx* restrict ctx) {
    ctx->stack_usage = 16;
    ctx->free_regs[0] = set_create(16);
    ctx->free_regs[1] = set_create(16);

    set_put(&ctx->free_regs[0], RBP);
    set_put(&ctx->free_regs[0], RSP);
}

static GAD_VAL x64v2_cond_to_reg(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int cc) {
    GAD_VAL* dst = &ctx->values[r];

    EMIT1(&ctx->emit, (dst->gpr >= 8) ? 0x41 : 0x40);
    EMIT1(&ctx->emit, 0x0F);
    EMIT1(&ctx->emit, 0x90 + cc);
    EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0, dst->gpr));
    return *dst;
}

static void x64v2_mov_to_explicit_gpr(Ctx* restrict ctx, TB_Function* f, GPR dst_gpr, TB_Reg r) {
    GAD_VAL dst = val_gpr(f->nodes[r].dt, dst_gpr);
    GAD_VAL* src = &ctx->values[r];
    LegalInt l = legalize_int(f->nodes[r].dt);

    if (src->type == VAL_MEM || src->type == VAL_GLOBAL) {
        INST2(src->mem.is_rvalue ? MOV : LEA, &dst, src, l.dt);
    } else if (src->type == VAL_FLAGS) {
        EMIT1(&ctx->emit, (dst_gpr >= 8) ? 0x41 : 0x40);
        EMIT1(&ctx->emit, 0x0F);
        EMIT1(&ctx->emit, 0x90 + src->cond);
        EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0, dst_gpr));
    } else {
        assert(src->type == VAL_GPR || src->type == VAL_IMM);

        if (src->type != VAL_GPR || (src->type == VAL_GPR && src->gpr != dst_gpr)) {
            INST2(MOV, &dst, src, l.dt);
        }
    }
}

static void x64v2_return(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n) {
    TB_DataType dt = n->dt;

    // Evaluate return value
    if (dt.type == TB_FLOAT) {
        tb_todo();
    } else if ((dt.type == TB_INT && dt.data > 0) || dt.type == TB_PTR) {
        x64v2_mov_to_explicit_gpr(ctx, f, RAX, n->ret.value);
    } else tb_todo();
}

static void x64v2_branch_if(Ctx* restrict ctx, TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false, TB_Reg fallthrough) {
    Cond cc = 0;
    if (ctx->flags_bound == cond) {
        cc = ctx->flags_code;
    } else {
        LegalInt l = legalize_int(f->nodes[cond].dt);
        GAD_VAL* src = &ctx->values[cond];

        if (is_value_mem(src)) {
            if (src->mem.is_rvalue) {
                Val imm = val_imm(TB_TYPE_I32, 0);
                INST2(CMP, src, &imm, l.dt);
            } else {
                tb_todo();
                /* GAD_VAL tmp = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);
                INST2(LEA, &tmp, &src, l.dt);
                INST2(TEST, &tmp, &tmp, l.dt);
                GAD_FN(unlock_register)(ctx, f, X64_REG_CLASS_GPR, tmp.gpr); */
            }

            cc = NE;
        } else if (src->type == VAL_GPR) {
            INST2(TEST, src, src, l.dt);
            cc = NE;
        } else if (src->type == VAL_IMM) {
            tb_todo();

            /*GAD_VAL tmp = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);

            // 'xor a, a' will set ZF to 1
            INST2(XOR, &tmp, &tmp, l.dt);
            GAD_FN(unlock_register)(ctx, f, X64_REG_CLASS_GPR, tmp.gpr);

            cc = (src.imm ? E : NE);*/
        } else {
            tb_todo();
        }
    }

    if (fallthrough == if_true) {
        // invert condition and target to make fallthrough work
        JCC(cc ^ 1, if_false);
    } else {
        // JCC .true
        // JMP .false
        JCC(cc, if_true);
        if (fallthrough != if_false) {
            JMP(if_false);
        }
    }
}

static void x64v2_spill(Ctx* restrict ctx, TB_Function* f, GAD_VAL* dst_val, GAD_VAL* src_val) {
    // masking is unnecessary here due to type safety
    if (!(src_val->type == GAD_VAL_REGISTER + X64_REG_CLASS_GPR && src_val->type == dst_val->type && src_val->reg == dst_val->reg)) {
        LegalInt l = legalize_int(src_val->dt);
        INST2(MOV, dst_val, src_val, l.dt);
    }
}

static void x64v2_move(Ctx* restrict ctx, TB_Function* f, TB_Reg dst, TB_Reg src) {
    LegalInt l = legalize_int(f->nodes[src].dt);
    GAD_VAL* dst_val = &ctx->values[dst];

    /*if (f->nodes[src].type == TB_ADD && f->nodes[src].i_arith.a == dst) {
        GAD_VAL* src_val = &ctx->values[f->nodes[src].i_arith.b];

        INST2(ADD, dst_val, src_val, f->nodes[src].dt);
        if (l.mask) x64v2_mask_out(ctx, f, l, dst_val);
    }*/

    GAD_VAL* src_val = &ctx->values[src];

    // mask is unnecessary due to type safety
    // if (l.mask) x64v2_mask_out(ctx, f, l, &src_val);
    if (!(src_val->type == GAD_VAL_REGISTER + X64_REG_CLASS_GPR && src_val->type == dst_val->type && src_val->reg == dst_val->reg)) {
        INST2(MOV, dst_val, src_val, l.dt);
    }
}

static void x64v2_store(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];

    GAD_VAL addr = ctx->values[n->store.address];
    if (addr.is_spill) {
        // restore from spill
        tb_todo();
        /*GAD_VAL tmp = GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);
        INST2(MOV, &tmp, &addr, TB_TYPE_PTR);

        addr = val_base_disp(TB_TYPE_PTR, tmp.gpr, 0);
        addr.is_spill = true;*/
    } else if (addr.type == VAL_GPR) {
        addr = val_base_disp(TB_TYPE_PTR, addr.gpr, 0);
    }

    LegalInt l = legalize_int(n->dt);
    GAD_VAL* src = &ctx->values[n->store.value];

    if (l.mask) x64v2_mask_out(ctx, f, l, src);
    INST2(MOV, &addr, src, l.dt);

    if (addr.is_spill) {
        tb_assert_once("Freeing temporary register in addr.mem.base");
    }
}

static void x64v2_call(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];
    TB_NodeTypeEnum type = n->type;

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

    bool is_sysv = (f->super.module->target_abi == TB_ABI_SYSTEMV);
    const struct ParamDescriptor* restrict params = &param_descs[is_sysv ? 1 : 0];
    if (type == TB_SCALL) {
        params = &param_descs[2];
    }

    // Evict the GPRs that are caller saved
    // uint16_t caller_saved = params->caller_saved_gprs;

    int param_start = n->call.param_start;
    int param_count = n->call.param_end - n->call.param_start;
    FOREACH_N(i, 0, param_count) {
        TB_Reg param_reg = f->vla.data[param_start + i];
        TB_DataType param_dt = f->nodes[param_reg].dt;

        if (TB_IS_FLOAT_TYPE(param_dt) || param_dt.width) {
            tb_todo();
            /*if (j < params->xmm_count) {
                tb_todo();
            } else {
                GAD_VAL dst = val_base_disp(param_dt, RSP, 8 * i);
                GAD_VAL src = GAD_FN(get_val_gpr)(ctx, f, param_reg);
                INST2(MOV, &dst, &src, param_dt);
            }*/
        } else {
            if (i < params->gpr_count) {
                GAD_VAL dst = GAD_FN(steal)(ctx, f, param_reg, X64_REG_CLASS_GPR, params->gprs[i]);
                // caller_saved &= ~(1u << dst.gpr);

                x64v2_mov_to_explicit_gpr(ctx, f, dst.gpr, param_reg);
            } else {
                GAD_VAL dst = val_base_disp(param_dt, RSP, 8 * i);
                INST2(MOV, &dst, &ctx->values[param_reg], param_dt);
            }
        }
    }

    switch (type) {
        case TB_CALL: {
            const TB_Symbol* target = n->call.target;
            if (target->tag == TB_SYMBOL_FUNCTION) {
                tb_emit_symbol_patch(f->super.module, f, target, GET_CODE_POS(&ctx->emit) + 1, true, s_local_thread_id);
            } else if (target->tag == TB_SYMBOL_EXTERNAL) {
                tb_emit_symbol_patch(f->super.module, f, target, GET_CODE_POS(&ctx->emit) + 1, true, s_local_thread_id);
            } else {
                tb_todo();
            }

            // CALL rel32
            EMIT1(&ctx->emit, 0xE8), EMIT4(&ctx->emit, 0x0);
            break;
        }
        case TB_SCALL: {
            // MOV RAX, syscall number
            GAD_VAL dst = val_gpr(TB_TYPE_PTR, RAX);
            INST2(MOV, &dst, &ctx->values[n->scall.target], TB_TYPE_I64);

            // SYSCALL
            EMIT1(&ctx->emit, 0x0F), EMIT1(&ctx->emit, 0x05);
            break;
        }
        case TB_VCALL: {
            GAD_VAL target = ctx->values[n->vcall.target];

            assert(target.type == VAL_MEM && target.mem.index == GPR_NONE && target.mem.disp == 0);
            target = val_gpr(TB_TYPE_PTR, target.mem.base);

            // call r/m64
            INST1(CALL_RM, &target);
            break;
        }
        default: tb_todo();
    }

    // the return value
    TB_DataType dt = f->nodes[r].dt;
    if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
        /*GAD_VAL xmm0 = {
            .type = GAD_VAL_REGISTER + X64_REG_CLASS_GPR,
            .dt = dt,
            .r = r,
            .reg = XMM0
        };*/

        tb_todo();
    } else {
        if (dt.type == TB_PTR || (dt.type == TB_INT && dt.data > 0)) {
            GAD_VAL rax = GAD_FN(steal)(ctx, f, r, X64_REG_CLASS_GPR, RAX);
            INST2(MOV, &ctx->values[r], &rax, TB_TYPE_I64);
        }
    }
}

static Val x64v2_eval(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];
    TB_NodeTypeEnum type = n->type;

    switch (type) {
        case TB_INTEGER_CONST: {
            if (n->integer.num_words == 1 && fits_into_int32(n->integer.single_word)) {
                LegalInt l = legalize_int(n->dt);
                uint64_t imm = n->integer.single_word;
                if (l.mask) {
                    imm &= l.mask;
                }

                return (ctx->values[r] = val_imm(n->dt, imm));
            } else {
                assert(n->integer.num_words == 1);
                GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);

                // MOVABS REX.W B8+r imm64
                EMIT1(&ctx->emit, dst.gpr >= 8 ? 0x49 : 0x48);
                EMIT1(&ctx->emit, 0xB8 + (dst.gpr & 7));
                EMIT8(&ctx->emit, n->integer.single_word);
                return dst;
            }
        }
        case TB_STRING_CONST: {
            const char* str = n->string.data;
            size_t len = n->string.length;

            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            EMIT1(&ctx->emit, rex(true, dst.gpr, RBP, 0));
            EMIT1(&ctx->emit, 0x8D);
            EMIT1(&ctx->emit, mod_rx_rm(MOD_INDIRECT, dst.gpr, RBP));

            uint32_t disp = tb_emit_const_patch(f->super.module, f, GET_CODE_POS(&ctx->emit), str, len, s_local_thread_id);
            EMIT4(&ctx->emit, disp);
            return dst;
        }

        case TB_VA_START: {
            assert(f->super.module->target_abi == TB_ABI_WIN64 && "How does va_start even work on SysV?");

            // on Win64 va_start just means whatever is one parameter away from
            // the parameter you give it (plus in Win64 the parameters in the stack
            // are 8bytes, no fanciness like in SysV):
            // void printf(const char* fmt, ...) {
            //     va_list args;
            //     va_start(args, fmt); // args = ((char*) &fmt) + 8;
            //     ...
            // }
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL addr = ctx->values[n->member_access.base];

            if (addr.type == VAL_MEM && !addr.mem.is_rvalue) {
                addr.mem.disp += 8;

                INST2(LEA, &dst, &addr, n->dt);
            } else if (addr.type == VAL_GPR) {
                GAD_VAL arith = val_base_disp(TB_TYPE_PTR, addr.gpr, 8);

                INST2(LEA, &dst, &arith, n->dt);
            } else {
                tb_todo();
            }
            return dst;
        }

        case TB_GET_SYMBOL_ADDRESS: {
            TB_Module* m = f->super.module;
            const TB_Symbol* s = n->sym.value;
            if (s->tag == TB_SYMBOL_GLOBAL) {
                const TB_Global* g = (const TB_Global*)s;
                if (g->storage == TB_STORAGE_TLS) {
                    if (m->tls_index_extern == 0) {
                        tb_panic("TB error: no tls_index provided\n");
                    }

                    // since t0 dies before dst is allocated we just recycle it
                    // mov t0, dword    [_tls_index]
                    GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);

                    if (dst.gpr >= 8) EMIT1(&ctx->emit, 0x41);
                    EMIT1(&ctx->emit, 0x8B), EMIT1(&ctx->emit, ((dst.gpr & 7) << 3) | RBP), EMIT4(&ctx->emit, 0x00);
                    tb_emit_symbol_patch(f->super.module, f, m->tls_index_extern, GET_CODE_POS(&ctx->emit) - 4, false, s_local_thread_id);

                    // mov t1, qword gs:[58h]
                    GAD_VAL t1 = { 0 }; // GAD_FN(alloc_reg)(ctx, f, X64_REG_CLASS_GPR, TB_TEMP_REG);
                    tb_todo();
                    EMIT1(&ctx->emit, 0x65);
                    EMIT1(&ctx->emit, t1.gpr >= 8 ? 0x4C : 0x48);
                    EMIT1(&ctx->emit, 0x8B);
                    EMIT1(&ctx->emit, mod_rx_rm(MOD_INDIRECT, t1.gpr, RSP));
                    EMIT1(&ctx->emit, mod_rx_rm(SCALE_X1, RSP, RBP));
                    EMIT4(&ctx->emit, 0x58);

                    // mov t1, qword [t1+dst*8]
                    Val mem = val_base_index(TB_TYPE_PTR, t1.gpr, dst.gpr, SCALE_X8);
                    INST2(MOV, &t1, &mem, TB_TYPE_I64);

                    // lea dst, [t1+relocation]
                    EMIT1(&ctx->emit, rex(true, dst.gpr, t1.gpr, 0)), EMIT1(&ctx->emit, 0x8D);
                    if ((t1.gpr & 7) == RSP) {
                        EMIT1(&ctx->emit, mod_rx_rm(MOD_INDIRECT_DISP32, dst.gpr, RSP));
                        EMIT1(&ctx->emit, mod_rx_rm(SCALE_X1, RSP, t1.gpr));
                    } else {
                        EMIT1(&ctx->emit, mod_rx_rm(MOD_INDIRECT_DISP32, dst.gpr, t1.gpr));
                    }
                    EMIT4(&ctx->emit, 0);
                    tb_emit_symbol_patch(f->super.module, f, (TB_Symbol*) n->global.value, GET_CODE_POS(&ctx->emit) - 4, false, s_local_thread_id);

                    // GAD_FN(unlock_register)(ctx, f, X64_REG_CLASS_GPR, t1.gpr);
                    return dst;
                } else {
                    return val_global(n->global.value);
                }
            } else {
                tb_todo();
            }
        }

        case TB_INT2PTR:
        case TB_SIGN_EXT:
        case TB_ZERO_EXT: {
            TB_DataType src_dt = f->nodes[n->unary.src].dt;
            bool sign_ext = (type == TB_SIGN_EXT);

            // figure out if we can use the cool instructions
            // or if we gotta emulate it like a bitch
            LegalInt l = legalize_int(src_dt);
            int bits_in_type = l.dt.type == TB_PTR ? 64 : l.dt.data;

            // gonna be either MOV, MOVSX or MOVZX
            Inst2Type op = MOV;

            switch (bits_in_type) {
                case 64: op = MOV; break;
                case 32: op = sign_ext ? MOVSXD : MOV; break;
                case 16: op = sign_ext ? MOVSXW : MOVZXW; break;
                case 8:  op = sign_ext ? MOVSXB : MOVZXB; break;
                default: op = MOV; break;
            }

            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL src = ctx->values[n->unary.src];

            INST2(op, &dst, &src, l.dt);
            if (op == MOV && l.mask != 0) {
                // complex extensions
                if (sign_ext) {
                    LegalInt dst_l = legalize_int(n->dt);
                    int dst_bits_in_type = dst_l.dt.type == TB_PTR ? 64 : dst_l.dt.data;
                    int shift_amt = dst_bits_in_type - bits_in_type;

                    emit_shift_gpr_imm(ctx, f, SHL, dst.gpr, shift_amt, dst_l.dt);
                    emit_shift_gpr_imm(ctx, f, SAR, dst.gpr, shift_amt, dst_l.dt);
                } else {
                    x64v2_mask_out(ctx, f, l, &dst);
                }
            }
            return dst;
        }

        case TB_TRUNCATE: {
            // TB_DataType src_dt = f->nodes[n->unary.src].dt;

            if (TB_IS_FLOAT_TYPE(n->dt)) {
                tb_todo();
            } else {
                GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
                GAD_VAL src = ctx->values[n->unary.src];

                LegalInt l = legalize_int(n->dt);
                INST2(MOV, &dst, &src, l.dt);

                if (l.mask) x64v2_mask_out(ctx, f, l, &dst);
                return dst;
            }
        }

        case TB_LOAD: {
            LegalInt l = legalize_int(n->dt);

            // convert entry into memory slot
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL src = ctx->values[n->load.address];

            if (src.is_spill) {
                // restore from spill
                INST2(MOV, &dst, &src, TB_TYPE_PTR);

                src = val_base_disp(TB_TYPE_PTR, dst.gpr, 0);
            } else if (src.type == VAL_GPR) {
                src = val_base_disp(TB_TYPE_PTR, src.gpr, 0);
            }

            if (l.mask) x64v2_mask_out(ctx, f, l, &src);
            INST2(MOV, &dst, &src, l.dt);
            return dst;
        }

        case TB_MEMBER_ACCESS: {
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL src = ctx->values[n->member_access.base];

            GAD_VAL addr = val_base_disp(TB_TYPE_PTR, src.gpr, n->member_access.offset);
            INST2(LEA, &dst, &addr, TB_TYPE_PTR);
            return dst;
        }

        case TB_ARRAY_ACCESS: {
            uint32_t stride = n->array_access.stride;
            GAD_VAL index = ctx->values[n->array_access.index];

            // if it's an LEA index*stride
            // then stride > 0, if not it's free
            // do think of it however
            uint8_t stride_as_shift = 0;

            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            bool written_to_dst = false;

            if (tb_is_power_of_two(stride)) {
                stride_as_shift = tb_ffs(stride) - 1;

                if (stride_as_shift > 3) {
                    assert(stride_as_shift < 64 && "Stride to big!!!");
                    written_to_dst = true;

                    INST2(MOV, &dst, &index, TB_TYPE_PTR);

                    // shl index, stride_as_shift
                    EMIT1(&ctx->emit, rex(true, 0, dst.gpr, 0));
                    EMIT1(&ctx->emit, 0xC1);
                    EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x04, dst.gpr));
                    EMIT1(&ctx->emit, stride_as_shift);

                    stride_as_shift = 0; // pre-multiplied, don't propagate
                }
            } else {
                written_to_dst = true;

                // imul dst, index, stride
                EMIT1(&ctx->emit, rex(true, dst.gpr, index.gpr, 0));
                EMIT1(&ctx->emit, 0x69);
                EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, dst.gpr, index.gpr));
                EMIT4(&ctx->emit, stride);

                stride_as_shift = 0; // pre-multiplied, don't propagate
            }

            // sanity check some post conditions ;)
            assert(stride_as_shift >= 0 && stride_as_shift <= 3
                && "stride_as_shift can't fit into an LEA");

            GAD_VAL base = ctx->values[n->array_access.base];
            assert(base.type == VAL_GPR);

            if (stride_as_shift) {
                GAD_VAL addr = val_base_index(TB_TYPE_PTR, base.gpr, written_to_dst ? dst.gpr : index.gpr, stride_as_shift);

                // INST2(LEA, &dst, &addr, TB_TYPE_PTR);
                return addr;
            } else {
                if (written_to_dst) {
                    INST2(ADD, &dst, &base, TB_TYPE_PTR);
                } else {
                    // tb_assert_once("does this path get hit?");
                    // INST2(LEA, &dst, &addr, TB_TYPE_PTR);
                    return val_base_index(TB_TYPE_PTR, base.gpr, index.gpr, SCALE_X1);
                }
                return dst;
            }
        }

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL: {
            const static Inst2Type ops[] = { AND, OR, XOR, ADD, SUB, IMUL };

            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL a = ctx->values[n->i_arith.a];
            GAD_VAL b = ctx->values[n->i_arith.b];

            if (dst.type == b.type && dst.reg == b.reg) {
                INST2(ops[type - TB_AND], &dst, &a, n->dt);
                return dst;
            } else if (dst.type == a.type && dst.reg == a.reg) {
                INST2(ops[type - TB_AND], &dst, &b, n->dt);
                return dst;
            } else {
                INST2(MOV, &dst, &a, n->dt);
                INST2(ops[type - TB_AND], &dst, &b, n->dt);
                return dst;
            }
        }

        case TB_NOT:
        case TB_NEG: {
            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            GAD_VAL src = ctx->values[n->unary.src];

            INST2(MOV, &dst, &src, n->dt);
            INST1(type == TB_NOT ? NOT : NEG, &dst);
            return dst;
        }

        case TB_SHR:
        case TB_SHL:
        case TB_SAR: {
            LegalInt l = legalize_int(n->dt);
            int bits_in_type = l.dt.type == TB_PTR ? 64 : l.dt.data;

            GAD_VAL dst = GAD_FN(regalloc)(ctx, f, r, X64_REG_CLASS_GPR);
            if (f->nodes[n->i_arith.b].type == TB_INTEGER_CONST &&
                f->nodes[n->i_arith.b].integer.num_words == 1) {
                uint64_t imm = f->nodes[n->i_arith.b].integer.single_word;
                assert(imm < 64);

                GAD_VAL a = ctx->values[n->i_arith.a];
                INST2(MOV, &dst, &a, n->dt);

                ShiftType shift_type = SHL;
                switch (type) {
                    case TB_SHR: shift_type = SHR; break;
                    case TB_SHL: shift_type = SHL; break;
                    case TB_SAR: shift_type = SAR; break;
                    default: tb_unreachable();
                }
                emit_shift_gpr_imm(ctx, f, shift_type, dst.gpr, imm, l.dt);
                if (l.mask) x64v2_mask_out(ctx, f, l, &dst);

                return dst;
            } else {
                // the shift instruction uses RCX (well CL) for the shift amount
                // resolve this after the eviction to avoid it getting caught up in it
                GAD_VAL a = ctx->values[n->i_arith.a];
                INST2(MOV, &dst, &a, l.dt);

                // MOV rcx, b
                GAD_VAL rcx = val_gpr(l.dt, RCX);
                GAD_VAL b = ctx->values[n->i_arith.b];
                INST2(MOV, &rcx, &b, l.dt);

                // D2 /4       shl r/m, cl
                // D2 /5       shr r/m, cl
                // D2 /7       sar r/m, cl
                if (bits_in_type == 16) EMIT1(&ctx->emit, 0x66);
                EMIT1(&ctx->emit, rex(bits_in_type == 64, 0x00, dst.gpr, 0x00));
                EMIT1(&ctx->emit, (bits_in_type == 8 ? 0xD2 : 0xD3));
                switch (type) {
                    case TB_SHL: EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x04, dst.gpr)); break;
                    case TB_SHR: EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x05, dst.gpr)); break;
                    case TB_SAR: EMIT1(&ctx->emit, mod_rx_rm(MOD_DIRECT, 0x07, dst.gpr)); break;
                    default: tb_unreachable();
                }

                if (l.mask) x64v2_mask_out(ctx, f, l, &dst);
                return dst;
            }
        }

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE: {
            TB_DataType cmp_dt = n->cmp.dt;
            assert(cmp_dt.width == 0 && "TODO: Implement vector compares");

            Cond cc = -1;
            if (TB_IS_FLOAT_TYPE(cmp_dt)) {
                tb_todo();
            } else {
                cmp_dt = legalize_int(cmp_dt).dt;

                bool invert = false;
                TB_Reg lhs = n->cmp.a, rhs = n->cmp.b;
                if (f->nodes[n->cmp.a].type == TB_INTEGER_CONST) {
                    tb_swap(TB_Reg, lhs, rhs);
                    invert = true;
                }

                GAD_VAL lhs_val = ctx->values[lhs];
                GAD_VAL rhs_val = ctx->values[rhs];
                INST2(CMP, &lhs_val, &rhs_val, cmp_dt);

                switch (type) {
                    case TB_CMP_EQ: cc = E; break;
                    case TB_CMP_NE: cc = NE; break;
                    case TB_CMP_SLT: cc = invert ? G : L; break;
                    case TB_CMP_SLE: cc = invert ? GE : LE; break;
                    case TB_CMP_ULT: cc = invert ? A : B; break;
                    case TB_CMP_ULE: cc = invert ? NB : BE; break;
                    default: tb_unreachable();
                }
            }
            assert(cc != -1);

            GAD_FN(set_flags)(ctx, f, r, cc);
            return val_flags(cc);
        }

        default: tb_todo();
    }

    tb_panic("You're not supposed to break out of the switch in x64v2_resolve_value, return a proper GAD_VAL");
    return (Val){ 0 };
}

static size_t GAD_FN(emit_call_patches)(TB_Module* restrict m) {
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
ICodeGen tb__x64v2_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .get_data_type_size  = x64v2_get_data_type_size,
    .emit_call_patches   = x64v2_emit_call_patches,
    .emit_prologue       = x64_emit_prologue,
    .emit_epilogue       = x64_emit_epilogue,
    .emit_win64eh_unwind_info = x64_emit_win64eh_unwind_info,

    .fast_path    = x64v2_compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
