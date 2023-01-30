
// there's an infinite number of virtual registers
// which map to some finite number of actual values
typedef enum {
    VREG_FAMILY_GPR,
    VREG_FAMILY_XMM,
    VREG_FAMILY_FLAGS
} VRegFamily;

#define VREG_STACK_POINTER (TreeVReg) { 1, VREG_FAMILY_GPR }
#define VREG_BASE_POINTER  (TreeVReg) { 2, VREG_FAMILY_GPR }
typedef struct {
    enum MIR_OperandType {
        MIR_OPERAND_NONE,

        MIR_OPERAND_IMM,

        MIR_OPERAND_GPR,
        MIR_OPERAND_XMM,

        // both of these use .mem but ADDRESS refers to
        // the resolved address while MEMORY points to some
        // memory at said address
        MIR_OPERAND_ADDRESS,
        MIR_OPERAND_MEMORY,
    } type;
    TB_DataType dt;
    union {
        int gpr;
        int xmm;
        uint64_t imm;
        struct {
            // base and index are VGPRs
            Scale   scale;
            int     base;
            int     index; // if 0, it's unmapped
            int32_t disp;
        } mem;
    };
} MIR_Operand;

typedef struct {
    enum MIR_InstType {
        MIR_INST_NONE,

        MIR_INST_LABEL,

        MIR_INST_RET,
        MIR_INST_DEF,
        MIR_INST_COPY_INTO,
        MIR_INST_STORE,
        MIR_INST_SIGN_EXT,
        MIR_INST_ZERO_EXT,

        MIR_INST_ADD,
        MIR_INST_AND,
        MIR_INST_MUL,
    } type;
    MIR_Operand operands[3];
} MIR_Inst;

typedef struct {
    TB_Reg reg;
    TreeVReg mapping;
} PhiValue;

typedef struct {
    TB_CGEmitter emit;
    int* use_count;

    // Used to allocate spills
    uint32_t stack_usage;

    // GPRs are the bottom 32bit
    // XMM is the top 32bit
    uint64_t regs_to_save;

    bool is_sysv;
    uint32_t caller_usage;

    uint32_t  phi_count;
    PhiValue* phis;

    // virtual registers
    uint32_t vgpr_count, vxmm_count;

    uint32_t inst_count, inst_cap;
    MIR_Inst* insts;

    TreeVReg* parameters;
} X64_ComplexCtx;

typedef struct {
    size_t memory_usage;

    size_t phi_count;
    size_t locals_count;
    size_t return_count;
    size_t line_info_count;
    size_t label_patch_count;
} FunctionTallyComplex;

typedef struct {
    int start, end;
} LiveInterval;

static FunctionTallyComplex tally_memory_usage_complex(TB_Function* restrict f) {
    size_t phi_count = 0;
    size_t locals_count = 0;
    size_t return_count = 0;
    size_t label_patch_count = 0;
    size_t line_info_count = 0;

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];
            TB_NodeTypeEnum t = n->type;

            if (t == TB_PHI2) phi_count++;
            else if (t == TB_RET) return_count++;
            else if (t == TB_LOCAL) locals_count++;
            else if (t == TB_IF) label_patch_count += 2;
            else if (t == TB_GOTO) label_patch_count++;
            else if (t == TB_LINE_INFO) line_info_count++;
            else if (t == TB_SWITCH) {
                label_patch_count += 1 + ((n->switch_.entries_end - n->switch_.entries_start) / 2);
            }
        }
    }

    // parameters are locals too... ish
    locals_count += f->prototype->param_count;

    size_t align_mask = _Alignof(long double) - 1;
    size_t tally = 0;

    // context
    tally += sizeof(X64_ComplexCtx);
    tally = (tally + align_mask) & ~align_mask;

    // use_count
    tally += f->node_count * sizeof(TB_Reg);
    tally = (tally + align_mask) & ~align_mask;

    // intervals
    tally += f->node_count * sizeof(TB_Reg);
    tally = (tally + align_mask) & ~align_mask;

    // phis
    tally += phi_count * sizeof(PhiValue);
    tally = (tally + align_mask) & ~align_mask;

    // labels
    tally += f->bb_count * sizeof(uint32_t);
    tally = (tally + align_mask) & ~align_mask;

    // label_patches
    tally += label_patch_count * sizeof(LabelPatch);
    tally = (tally + align_mask) & ~align_mask;

    // ret_patches
    tally += return_count * sizeof(ReturnPatch);
    tally = (tally + align_mask) & ~align_mask;

    // insts
    tally += f->node_count * 2 * sizeof(MIR_Inst);
    tally = (tally + align_mask) & ~align_mask;

    // parameters
    tally += f->prototype->param_count * sizeof(TreeVReg);
    tally = (tally + align_mask) & ~align_mask;

    return (FunctionTallyComplex) {
        .memory_usage = tally,
        .phi_count = phi_count,
        .line_info_count = line_info_count,
        .locals_count = locals_count,
        .return_count = return_count,
        .label_patch_count = label_patch_count
    };
}

static void print_machine_operand(const MIR_Operand* operand) {
    switch (operand->type) {
        case MIR_OPERAND_GPR: printf("%d:GPR", operand->gpr); break;
        case MIR_OPERAND_XMM: printf("%d:XMM", operand->xmm); break;
        case MIR_OPERAND_IMM: printf("%"PRIu64, operand->imm); break;

        case MIR_OPERAND_ADDRESS:
        case MIR_OPERAND_MEMORY: {
            if (operand->type == MIR_OPERAND_ADDRESS) printf("&");

            printf("[");
            if (operand->mem.base) {
                printf("%d:GPR + ", operand->mem.base);
            }

            if (operand->mem.index) {
                printf("%d:GPR*%d + ", operand->mem.index, 1 << operand->mem.scale);
            }
            printf("%d]", operand->mem.disp);
            break;
        }

        default: tb_todo();
    }
}

static void print_machine_insts(X64_ComplexCtx* restrict ctx) {
    FOREACH_N(i, 0, ctx->inst_count) {
        MIR_Inst* inst = &ctx->insts[i];
        switch (inst->type) {
            case MIR_INST_LABEL: {
                printf("LABEL:\n");
                break;
            }
            case MIR_INST_DEF: {
                printf("  DEF ");
                print_machine_operand(&inst->operands[0]);
                printf(", ");
                print_machine_operand(&inst->operands[1]);
                printf("\n");
                break;
            }
            case MIR_INST_AND: {
                printf("  AND ");
                print_machine_operand(&inst->operands[0]);
                printf(", ");
                print_machine_operand(&inst->operands[1]);
                printf("\n");
                break;
            }
            case MIR_INST_ADD: {
                printf("  ADD ");
                print_machine_operand(&inst->operands[0]);
                printf(", ");
                print_machine_operand(&inst->operands[1]);
                printf(", ");
                print_machine_operand(&inst->operands[2]);
                printf("\n");
                break;
            }
            case MIR_INST_MUL: {
                printf("  MUL ");
                print_machine_operand(&inst->operands[0]);
                printf(", ");
                print_machine_operand(&inst->operands[1]);
                printf(", ");
                print_machine_operand(&inst->operands[2]);
                printf("\n");
                break;
            }
            case MIR_INST_COPY_INTO: {
                printf("  COPY_INTO ");
                print_machine_operand(&inst->operands[0]);
                printf(", ");
                print_machine_operand(&inst->operands[1]);
                printf("\n");
                break;
            }
            case MIR_INST_RET: {
                printf("  RET ");
                print_machine_operand(&inst->operands[0]);
                printf("\n");
                break;
            }
            default: tb_todo();
        }
    }
}

static void complex_live_interval_use(size_t i, const MIR_Operand* o, LiveInterval* intervals) {
    if (o->type == MIR_OPERAND_GPR) {
        intervals[o->gpr].end = i;
    } else if (o->type == MIR_OPERAND_ADDRESS || o->type == MIR_OPERAND_MEMORY) {
        intervals[o->mem.base].end = i;
        intervals[o->mem.index].end = i;
    } else tb_todo();
}

static PhiValue* find_phi(X64_ComplexCtx* restrict ctx, TB_Reg r) {
    for (size_t i = 0; i < ctx->phi_count; i++) {
        if (ctx->phis[i].reg == r) return &ctx->phis[i];
    }

    return NULL;
}

/*static void add_machine_inst(X64_ComplexCtx* restrict ctx, const MachineInst* inst) {
    assert(ctx->inst_count + 1 < ctx->inst_cap);
    ctx->insts[ctx->inst_count++] = *inst;
}*/

static TreeVReg complex_alloc_vgpr(X64_ComplexCtx* restrict ctx) {
    return (TreeVReg){ ctx->vgpr_count++, VREG_FAMILY_GPR };
}

static void complex_emit_inst(X64_ComplexCtx* restrict ctx, TB_Function* f, const MIR_Inst* src) {
    assert(ctx->inst_count + 1 < ctx->inst_cap);
    memcpy(&ctx->insts[ctx->inst_count++], src, sizeof(MIR_Inst));
}

static TreeVReg complex_collapse(X64_ComplexCtx* restrict ctx, TB_Function* f, const MIR_Operand operand) {
    if (operand.type == MIR_OPERAND_GPR) {
        return (TreeVReg){ operand.gpr, VREG_FAMILY_GPR };
    }

    assert(operand.type != MIR_OPERAND_XMM);
    TreeVReg resolved = complex_alloc_vgpr(ctx);

    // DEF resolved, dst
    complex_emit_inst(ctx, f, &(MIR_Inst) {
            MIR_INST_DEF,
            { { MIR_OPERAND_GPR, operand.dt, .gpr = resolved.value }, operand }
        });

    return resolved;
}

static MIR_Operand complex_isel(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node) {
    TB_Node* restrict n = &f->nodes[tree_node->reg];
    TB_NodeTypeEnum reg_type = n->type;
    TB_DataType dt = n->dt;

    if (tree_node->use_count) {
        // shared node
        if (tree_node->vreg.value != 0) {
            if (tree_node->vreg.family == VREG_FAMILY_GPR) {
                return (MIR_Operand){ MIR_OPERAND_GPR, dt, .gpr = tree_node->vreg.value };
            } else if (tree_node->vreg.family == VREG_FAMILY_XMM) {
                return (MIR_Operand){ MIR_OPERAND_XMM, dt, .xmm = tree_node->vreg.value };
            } else tb_todo();
        }
    }

    MIR_Operand dst = { 0 };
    switch (reg_type) {
        case TB_PHI2: {
            PhiValue* phi = find_phi(ctx, tree_node->reg);
            assert(phi != NULL);
            assert(phi->mapping.family == VREG_FAMILY_GPR);

            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = phi->mapping.value };
            break;
        }
        case TB_INTEGER_CONST: {
            assert(dt.type == TB_PTR || (dt.type == TB_INT && dt.data <= 64));
            dst = (MIR_Operand) { MIR_OPERAND_IMM, dt, .imm = n->integer.single_word };
            break;
        }
        case TB_PARAM: {
            assert(!TB_IS_FLOAT_TYPE(dt));
            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = 3 + n->param.id };
            break;
        }
        case TB_TRUNCATE: {
            TreeVReg src = complex_collapse(ctx, f, complex_isel(ctx, f, tree_node->operands[0]));
            TreeVReg dst_vreg = complex_alloc_vgpr(ctx);
            assert(src.family == VREG_FAMILY_GPR);

            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = dst_vreg.value };

            LegalInt l = legalize_int(dt);
            if (l.mask) {
                complex_emit_inst(ctx, f, &(MIR_Inst) {
                        MIR_INST_AND,
                        { dst, { MIR_OPERAND_GPR, dt, .gpr = src.value }, { MIR_OPERAND_IMM, l.dt, .imm = l.mask } }
                    });
            } else {
                complex_emit_inst(ctx, f, &(MIR_Inst) {
                        MIR_INST_COPY_INTO,
                        { dst, { MIR_OPERAND_GPR, dt, .gpr = src.value } }
                    });
            }
            break;
        }
        case TB_SIGN_EXT: {
            TreeVReg src = complex_collapse(ctx, f, complex_isel(ctx, f, tree_node->operands[0]));
            TreeVReg dst_vreg = complex_alloc_vgpr(ctx);
            assert(src.family == VREG_FAMILY_GPR);

            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = dst_vreg.value };
            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_SIGN_EXT,
                    { dst, { MIR_OPERAND_GPR, dt, .gpr = src.value } }
                });
            break;
        }
        case TB_ZERO_EXT: {
            TreeVReg src = complex_collapse(ctx, f, complex_isel(ctx, f, tree_node->operands[0]));
            TreeVReg dst_vreg = complex_alloc_vgpr(ctx);
            assert(src.family == VREG_FAMILY_GPR);

            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = dst_vreg.value };
            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_ZERO_EXT,
                    { dst, { MIR_OPERAND_GPR, dt, .gpr = src.value } }
                });
            break;
        }
        case TB_LOAD: {
            MIR_Operand address = complex_isel(ctx, f, tree_node->operands[0]);

            // if it's not already an address... figure it out...
            if (address.type != MIR_OPERAND_ADDRESS) {
                TreeVReg tmp = complex_collapse(ctx, f, address);
                assert(tmp.family == VREG_FAMILY_GPR);

                dst = (MIR_Operand) { MIR_OPERAND_ADDRESS, dt, .mem = { .base = tmp.value } };
            }

            // convert address into load
            assert(address.type == MIR_OPERAND_ADDRESS);
            dst = address;
            dst.type = MIR_OPERAND_MEMORY;
            dst.dt = dt;
            break;
        }
        case TB_ADD: {
            MIR_Operand left = complex_isel(ctx, f, tree_node->operands[0]);
            MIR_Operand right = complex_isel(ctx, f, tree_node->operands[1]);

            TreeVReg dst_vreg = complex_alloc_vgpr(ctx);
            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = dst_vreg.value };
            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_ADD, { dst, left, right }
                });
            break;
        }
        case TB_MUL: {
            MIR_Operand left = complex_isel(ctx, f, tree_node->operands[0]);
            MIR_Operand right = complex_isel(ctx, f, tree_node->operands[1]);

            TreeVReg dst_vreg = complex_alloc_vgpr(ctx);
            dst = (MIR_Operand) { MIR_OPERAND_GPR, dt, .gpr = dst_vreg.value };
            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_MUL, { dst, left, right }
                });
            break;
        }
        case TB_MEMBER_ACCESS: {
            MIR_Operand base = complex_isel(ctx, f, tree_node->operands[0]);
            TB_CharUnits offset = n->member_access.offset;

            if (base.type == MIR_OPERAND_ADDRESS) {
                dst = base;
                dst.mem.disp += offset;
                break;
            }

            // Convert base into proper VGPR
            TreeVReg base_vgpr = complex_collapse(ctx, f, base);
            assert(base_vgpr.family == VREG_FAMILY_GPR);

            dst = (MIR_Operand){ MIR_OPERAND_ADDRESS, TB_TYPE_PTR, .mem = { .base = base_vgpr.value, .disp = offset } };
            break;
        }
        case TB_ARRAY_ACCESS: {
            MIR_Operand base  = complex_isel(ctx, f, tree_node->operands[0]);
            MIR_Operand index = complex_isel(ctx, f, tree_node->operands[1]);
            TB_CharUnits stride = n->array_access.stride;

            // Convert index into proper VGPR
            TreeVReg index_vgpr = complex_collapse(ctx, f, index);
            assert(index_vgpr.family == VREG_FAMILY_GPR);

            // we only wanna use LEA arithmatic if it fits into *at most* 2 LEA instructions
            int multipliers[2] = { 0 };
            bool should_use_lea_arith = true;
            {
                // local macros are kinda weird
                #define GOES_INTO_N(A, N) (((A) % (N)) == 0)

                int steps = 2;
                uint32_t remaining_stride = stride;
                while (steps--) {
                    if (tb_is_power_of_two(remaining_stride & ~1u)) {
                        if (remaining_stride <= 8) {
                            multipliers[steps] = remaining_stride;
                            break;
                        } else {
                            // couldn't fit so we split it
                            multipliers[steps] = 8;
                            remaining_stride /= 8;
                        }
                    } else {
                        // we might be able to retrofit it
                        if (GOES_INTO_N(remaining_stride & ~1u, 8)) {
                            multipliers[steps] = 8, remaining_stride /= 8;
                            continue;
                        }

                        if (GOES_INTO_N(remaining_stride & ~1u, 4)) {
                            multipliers[steps] = 4, remaining_stride /= 4;
                            continue;
                        }

                        if (GOES_INTO_N(remaining_stride & ~1u, 2)) {
                            multipliers[steps] = 2, remaining_stride /= 2;
                            continue;
                        }

                        should_use_lea_arith = false;
                        break;
                    }
                }

                #undef GOES_INTO_N
            }

            // by the end of this we'll have a resolved base, index and scale
            // but the base doesn't account for the real base vreg so if we have
            // a base here it means we have to collapse.
            dst = (MIR_Operand){ MIR_OPERAND_ADDRESS, TB_TYPE_PTR, .mem = { .index = index_vgpr.value } };
            if (should_use_lea_arith) {
                // accumulate and collapse LEA operations to get an optimal multiply
                FOREACH_REVERSE_N(steps, 0, 2) {
                    if (multipliers[steps] == 0) break;
                    int shift = tb_ffs(multipliers[steps] & ~1u) - 1;

                    assert(shift <= 3);
                    if (steps == 1) {
                        // collapse to make room
                        TreeVReg new_addr = complex_collapse(ctx, f, dst);
                        assert(new_addr.family == VREG_FAMILY_GPR);

                        dst = (MIR_Operand){ MIR_OPERAND_ADDRESS, TB_TYPE_PTR, .mem = { .index = new_addr.value } };
                    }
                    dst.mem.scale = shift;

                    if (multipliers[steps] & 1) {
                        if (dst.mem.base != 0) {
                            // collapse to make room
                            TreeVReg new_addr = complex_collapse(ctx, f, dst);
                            assert(new_addr.family == VREG_FAMILY_GPR);

                            dst = (MIR_Operand){ MIR_OPERAND_ADDRESS, TB_TYPE_PTR, .mem = { .index = new_addr.value } };
                        }

                        dst.mem.base = dst.mem.index;
                    }
                }
            } else {
                tb_todo();
            }

            TreeVReg base_vgpr = complex_collapse(ctx, f, base);
            assert(base_vgpr.family == VREG_FAMILY_GPR);

            if (dst.mem.base != 0) {
                // collapse to make room
                TreeVReg new_addr = complex_collapse(ctx, f, dst);
                assert(new_addr.family == VREG_FAMILY_GPR);

                dst = (MIR_Operand){ MIR_OPERAND_ADDRESS, TB_TYPE_PTR, .mem = { .index = new_addr.value } };
            }

            dst.mem.base = base_vgpr.value;
            break;
        }
        default: tb_todo();
    }

    if (tree_node->use_count > 0) {
        // need to be collapsed since it's shared
        tree_node->vreg = complex_collapse(ctx, f, dst);
    }

    return dst;
}

static void isel_top_level(X64_ComplexCtx* restrict ctx, TB_Function* f, TreeNode* tree_node) {
    TB_Node* restrict n = &f->nodes[tree_node->reg];
    TB_NodeTypeEnum reg_type = n->type;
    TB_DataType dt = n->dt;

    switch (reg_type) {
        case TB_PHI2: {
            // map phi node to shared location
            PhiValue* phi = find_phi(ctx, tree_node->reg);
            assert(phi != NULL);

            // generate new mapping if one doesn't exist
            TreeVReg mapping = phi->mapping;
            if (mapping.value == 0) {
                if (dt.width || TB_IS_FLOAT_TYPE(dt)) {
                    mapping = (TreeVReg) { ctx->vxmm_count++, VREG_FAMILY_XMM };
                } else {
                    mapping = (TreeVReg) { ctx->vgpr_count++, VREG_FAMILY_GPR };
                }
                phi->mapping = mapping;
            }

            // copy correct value into the mapping
            MIR_Operand src = complex_isel(ctx, f, tree_node->operands[0]);
            MIR_Operand dst = { MIR_OPERAND_GPR, dt, .gpr = mapping.value };

            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_COPY_INTO, { dst, src }
                });
            break;
        }
        case TB_STORE: {
            MIR_Operand dst = complex_isel(ctx, f, tree_node->operands[0]);
            MIR_Operand src = complex_isel(ctx, f, tree_node->operands[1]);

            complex_emit_inst(ctx, f, &(MIR_Inst) {
                    MIR_INST_STORE, { dst, src }
                });
            break;
        }
        case TB_RET: {
            if (tree_node->operands[0]) {
                MIR_Operand src = complex_isel(ctx, f, tree_node->operands[0]);

                complex_emit_inst(ctx, f, &(MIR_Inst) {
                        MIR_INST_RET, { src }
                    });
            }
            break;
        }
        case TB_GOTO: {
            break;
        }
        case TB_IF: {
            break;
        }
        default: tb_todo();
    }
}

typedef struct {
    Val* vgpr_vals;
} RegAllocResult;

static RegAllocResult complex_regalloc(X64_ComplexCtx* restrict ctx, TB_Function* f) {
    Val* vgpr_vals = tb_platform_heap_alloc(ctx->vgpr_count * sizeof(Val));
    memset(vgpr_vals, 0, ctx->vgpr_count * sizeof(Val));

    uint16_t gpr_alloc = 0;
    FOREACH_REVERSE_N(i, 0, ctx->inst_count) {
        MIR_Inst* inst = &ctx->insts[i];
        switch (inst->type) {
            case MIR_INST_DEF: {
                // if it's already decided just keep going
                int dst_vgpr = inst->operands[0].gpr;
                if (vgpr_vals[dst_vgpr].type == VAL_NONE) {
                    int free_reg = tb_ffs(~gpr_alloc);
                    if (free_reg > 16) tb_todo();
                    if (free_reg == 0) tb_todo();

                    vgpr_vals[dst_vgpr] = val_gpr(inst->operands[0].dt, free_reg);
                    gpr_alloc |= (1u << (free_reg - 1));
                }
                break;
            }

            case MIR_INST_RET:
            // TODO(NeGate): this should pop the current regalloc
            // context since we left the basic block
            gpr_alloc = 0;
            break;

            default: tb_todo();
        }
    }

    return (RegAllocResult){ vgpr_vals };
}

// *is_address marks if the operand should be treated as an LEA memory operand
static Val complex_resolve_operand(X64_ComplexCtx* restrict ctx, TB_Function* f, bool* is_address, MIR_Operand* restrict o, RegAllocResult regalloc) {
    tb_todo();
}

TB_FunctionOutput x64_complex_compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, size_t local_thread_id) {
    s_local_thread_id = local_thread_id;

    TB_TemporaryStorage* tls = tb_tls_allocate();

    ////////////////////////////////
    // Allocate all the memory we'll need
    ////////////////////////////////
    bool is_ctx_heap_allocated = false;
    X64_ComplexCtx* restrict ctx = NULL;
    {
        // if we can't fit our memory usage into memory, we fallback
        FunctionTallyComplex tally = tally_memory_usage_complex(f);
        is_ctx_heap_allocated = !tb_tls_can_fit(tls, tally.memory_usage);

        if (is_ctx_heap_allocated) {
            // printf("Could not allocate x64 code gen context: using heap fallback.
            // (%zu bytes)\n", tally.memory_usage);
            ctx = tb_platform_heap_alloc(sizeof(X64_ComplexCtx));
            *ctx = (X64_ComplexCtx) {
                .emit = {
                    .f             = f,
                    .capacity      = out_capacity,
                    .data          = out,
                    .labels        = tb_platform_heap_alloc(f->bb_count * sizeof(uint32_t)),
                    .label_patches = tb_platform_heap_alloc(tally.label_patch_count * sizeof(LabelPatch)),
                    .ret_patches   = tb_platform_heap_alloc(tally.return_count * sizeof(ReturnPatch))
                },
            };

            ctx->use_count  = tb_platform_heap_alloc(f->node_count * sizeof(TB_Reg));
            ctx->phis       = tb_platform_heap_alloc(tally.phi_count * sizeof(PhiValue));
            ctx->insts      = tb_platform_heap_alloc(f->node_count * 2 * sizeof(MIR_Inst));
            ctx->parameters = tb_platform_heap_alloc(f->prototype->param_count * sizeof(TreeVReg));
        } else {
            ctx = tb_tls_push(tls, sizeof(X64_ComplexCtx));
            *ctx = (X64_ComplexCtx) {
                .emit = {
                    .f             = f,
                    .capacity      = out_capacity,
                    .data          = out,
                    .labels        = tb_tls_push(tls, f->bb_count * sizeof(uint32_t)),
                    .label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch)),
                    .ret_patches   = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch))
                },
            };

            ctx->use_count  = tb_tls_push(tls, f->node_count * sizeof(TB_Reg));
            ctx->phis       = tb_tls_push(tls, tally.phi_count * sizeof(PhiValue));
            ctx->insts      = tb_tls_push(tls, f->node_count * 2 * sizeof(MIR_Inst));
            ctx->parameters = tb_tls_push(tls, f->prototype->param_count * sizeof(TreeVReg));
        }

        ctx->inst_cap = f->node_count * 2;

        // virtual registers keep 0 as a NULL slot
        ctx->vgpr_count = 1;
        ctx->vxmm_count = 1;

        ctx->is_sysv = (f->super.module->target_abi == TB_ABI_SYSTEMV);

        f->line_count = 0;
        f->lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));
    }

    ////////////////////////////////
    // Analyze function for stack, live intervals and phi nodes
    ////////////////////////////////
    tb_function_calculate_use_count(f, ctx->use_count);

    // Create phi lookup table for later evaluation stages
    // and calculate the maximum parameter usage for a call
    size_t caller_usage = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];
            if (n->type == TB_PHI2) {
                ctx->phis[ctx->phi_count++] = (PhiValue) { n - f->nodes };
            } else if (n->type == TB_CALL || n->type == TB_VCALL) {
                int param_usage = CALL_NODE_PARAM_COUNT(n);
                if (caller_usage < param_usage) caller_usage = param_usage;
            }
        }
    }

    // On Win64 if we have at least one parameter in any of it's calls, the
    // caller must reserve 32bytes called the shadow space.
    if (!ctx->is_sysv && caller_usage > 0 && caller_usage < 4) caller_usage = 4;

    // Allocate local and parameter stack slots
    tb_function_print(f, tb_default_print_callback, stdout, false);
    printf("\n\n\n");

    const TB_FunctionPrototype* restrict proto = f->prototype;

    // NULL vgpr, RSP and RBP and the parameters
    ctx->vgpr_count = 3 + proto->param_count;

    FOREACH_N(i, 0, proto->param_count) {
        TB_DataType dt = proto->params[i].dt;

        // Allocate space in stack
        int size = get_data_type_size(dt);
        (void)STACK_ALLOC(size, size);
        assert(size <= 8 && "Parameter too big");
    }

    ////////////////////////////////
    // Evaluate each basic block
    ////////////////////////////////
    TreeNodeArena tree = { 0 };
    TB_FOR_BASIC_BLOCK(bb, f) {
        // Generate expression tree
        TreeNode* node = tb_tree_generate(&tree, f, ctx->use_count, bb);
        complex_emit_inst(ctx, f, &(MIR_Inst) { MIR_INST_LABEL });

        // Instruction selection
        while (node) {
            assert(node->reg == TB_NULL_REG);

            isel_top_level(ctx, f, node->operands[0]);
            node = node->operands[1];
        }

        // Next Basic block
        tb_tree_clear(&tree);
    }

    tb_tree_free(&tree);
    print_machine_insts(ctx);
    printf("\n\n\n");

    // Compute live intervals
    LiveInterval* intervals = tb_platform_heap_alloc(ctx->vgpr_count * sizeof(LiveInterval));
    FOREACH_N(i, 0, ctx->inst_count) {
        MIR_Inst* inst = &ctx->insts[i];
        switch (inst->type) {
            case MIR_INST_DEF:
            assert(inst->operands[0].type == MIR_OPERAND_GPR);

            intervals[inst->operands[0].gpr].start = i;
            complex_live_interval_use(i, &inst->operands[1], intervals);
            break;

            case MIR_INST_RET:
            complex_live_interval_use(i, &inst->operands[0], intervals);
            break;

            default: tb_todo();
        }
    }
    intervals[0] = (LiveInterval){ 0 };

    // TODO(NeGate): Identify all natural loops

    // Register allocations
    RegAllocResult regalloc = complex_regalloc(ctx, f);
    (void)regalloc;
    // TODO(NeGate): Generate machine code
    FOREACH_N(i, 0, ctx->inst_count) {
        MIR_Inst* inst = &ctx->insts[i];
        switch (inst->type) {
            case MIR_INST_DEF:
            break;

            case MIR_INST_RET:
            break;

            default: tb_todo();
        }
    }

    // Tally up any saved XMM registers
    ctx->stack_usage += tb_popcount((ctx->regs_to_save >> 16) & 0xFFFF) * 16;
    ctx->stack_usage = align_up(ctx->stack_usage + 8, 16) + 8;

    ////////////////////////////////
    // Evaluate internal relocations (return and labels)
    ////////////////////////////////
    FOREACH_N(i, 0, ctx->emit.ret_patch_count) {
        uint32_t pos = ctx->emit.ret_patches[i];
        PATCH4(&ctx->emit, pos, GET_CODE_POS(&ctx->emit) - (pos + 4));
    }

    FOREACH_N(i, 0, ctx->emit.label_patch_count) {
        uint32_t pos = ctx->emit.label_patches[i].pos;
        uint32_t target_lbl = ctx->emit.label_patches[i].target_lbl;

        PATCH4(&ctx->emit, pos, ctx->emit.labels[target_lbl] - (pos + 4));
    }

    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->emit.data,
        .code_size = ctx->emit.count,
        .stack_usage = ctx->stack_usage,
        .prologue_epilogue_metadata = ctx->regs_to_save
    };

    if (is_ctx_heap_allocated) {
        tb_platform_heap_free(ctx->emit.labels);
        tb_platform_heap_free(ctx->emit.label_patches);
        tb_platform_heap_free(ctx->emit.ret_patches);

        tb_platform_heap_free(ctx->use_count);
        tb_platform_heap_free(ctx->phis);

        tb_platform_heap_free(ctx->insts);
        tb_platform_heap_free(ctx->parameters);
        tb_platform_heap_free(ctx);
    }
    return func_out;
}
