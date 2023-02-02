// this is a generic code generator you can use to get started with a new register machine target in TB
// all you have to do is hook in the correct details and functions and it'll do the heavy lifting...
// it's all managed through preprocessor defines and monomorphized using #include like such:
//
//
// #define GAD_FN(name) aarch64_ ## name // all "exported" symbols have this prefix
// #define GAD_NUM_REG_FAMILIES 2
// #define GAD_VAL MyCpuVal
// ...
// #include "generic_addrdesc.h"
//
// Explanations:
#include "emitter.h"

static_assert(sizeof(float) == sizeof(uint32_t), "lil bitch... float gotta be 32bit");
static_assert(sizeof(double) == sizeof(uint64_t), "big bitch... double gotta be 64bit");

static thread_local size_t s_local_thread_id;

#if 0
#define DBG(...) printf(__VA_ARGS__)
#define LISTING(...) printf(__VA_ARGS__)
#else
#define DBG(...)
#define LISTING(fmt, ...)
#endif

#define EITHER2(a, b, c)    ((a) == (b) || (a) == (c))
#define EITHER3(a, b, c, d) ((a) == (b) || (a) == (c) || (a) == (d))
#define FITS_INTO(a, type)  ((a) == ((type)(a)))

#define TB_TEMP_REG INT_MAX

enum {
    GAD_VAL_UNRESOLVED = 0,
    GAD_VAL_FLAGS      = 1,
    GAD_VAL_REGISTER   = 2,
};

typedef struct {
    uint16_t class, num;
} RegisterRef;

struct Ctx {
    TB_CGEmitter emit;

    TB_Function* f;
    TB_Predeccesors preds;
    TB_TemporaryStorage* tls;

    TB_Reg* active;
    size_t active_count;

    // we can allocate 4 temporaries at any one time (which is kinda a lot)
    size_t tmp_count;
    RegisterRef tmps[4];

    // some analysis
    int* intervals; // [reg] = last_use
    int* ordinal;   // [reg] = timeline position

    // Used to allocate stack stuff
    uint32_t stack_usage;
    DynArray(TB_StackSlot) stack_slots;

    // just keeps track of the callee saved registers that
    // we actually used.
    uint64_t regs_to_save;

    // Extra stuff
    // GAD_EXTRA_CTX is a struct body
    struct GAD_EXTRA_CTX;

    // Regalloc
    Set free_regs[GAD_NUM_REG_FAMILIES];

    // If the ISA has flags this is helpful
    TB_Reg flags_bound;
    int flags_code;

    size_t spill_count;
    GAD_VAL* spills;
    GAD_VAL values[];
};

typedef struct {
    size_t memory_usage;

    size_t locals_count;
    size_t return_count;
    size_t line_info_count;
    size_t label_patch_count;
} FunctionTallySimple;

static FunctionTallySimple tally_memory_usage_simple(TB_Function* restrict f) {
    size_t locals_count = 0;
    size_t return_count = 0;
    size_t label_patch_count = 0;
    size_t line_info_count = 0;

    TB_FOR_BASIC_BLOCK(bb, f) {
        label_patch_count += 1;

        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(r, f, bb) {
                TB_Node* n = &f->nodes[r];
                TB_NodeTypeEnum t = n->type;

                if (t == TB_RET) return_count++;
                else if (t == TB_LOCAL) locals_count++;
                else if (t == TB_IF) label_patch_count += 2;
                else if (t == TB_GOTO) label_patch_count++;
                else if (t == TB_LINE_INFO) line_info_count++;
                else if (t == TB_SWITCH) {
                    label_patch_count += 1 + ((n->switch_.entries_end - n->switch_.entries_start) / 2);
                }
            }
        }
    }

    // parameters are locals too... ish
    locals_count += f->prototype->param_count;

    size_t align_mask = _Alignof(long double) - 1;
    size_t tally = 0;

    // context
    tally += sizeof(Ctx) + (f->node_count * sizeof(GAD_VAL));
    tally = (tally + align_mask) & ~align_mask;

    // spills
    tally += f->node_count * sizeof(GAD_VAL);
    tally = (tally + align_mask) & ~align_mask;

    // ordinal
    tally += f->node_count * sizeof(int);
    tally = (tally + align_mask) & ~align_mask;

    // intervals
    tally += f->node_count * sizeof(int);
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

    // postorder.visited
    tally += f->bb_count * sizeof(ReturnPatch);
    tally = (tally + align_mask) & ~align_mask;

    // postorder.traversal
    tally += f->bb_count * sizeof(ReturnPatch);
    tally = (tally + align_mask) & ~align_mask;

    return (FunctionTallySimple) {
        .memory_usage = tally,
        .line_info_count = line_info_count,
        .locals_count = locals_count,
        .return_count = return_count,
        .label_patch_count = label_patch_count
    };
}

// user-defined forward decls
static size_t GAD_FN(resolve_stack_usage)(Ctx* restrict ctx, TB_Function* f, size_t stack_usage, size_t caller_usage);
static void GAD_FN(resolve_local_patches)(Ctx* restrict ctx, TB_Function* f);
static GAD_VAL GAD_FN(phi_alloc)(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void GAD_FN(initializer)(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void GAD_FN(call)(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void GAD_FN(store)(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void GAD_FN(spill)(Ctx* restrict ctx, TB_Function* f, GAD_VAL* dst_val, GAD_VAL* src_val, TB_Reg r);
static void GAD_FN(goto)(Ctx* restrict ctx, TB_Label l);
static void GAD_FN(ret_jmp)(Ctx* restrict ctx);
static void GAD_FN(initial_reg_alloc)(Ctx* restrict ctx);
static void GAD_FN(resolve_params)(Ctx* restrict ctx, TB_Function* f, GAD_VAL* values);
static GAD_VAL GAD_FN(eval)(Ctx* restrict ctx, TB_Function* f, TB_Reg r);
static void GAD_FN(resolve_stack_slot)(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n);
static void GAD_FN(return)(Ctx* restrict ctx, TB_Function* f, TB_Node* restrict n);
static void GAD_FN(move)(Ctx* restrict ctx, TB_Function* f, TB_Reg dst, TB_Reg src);
static void GAD_FN(branch_if)(Ctx* restrict ctx, TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false, TB_Reg fallthrough);
static GAD_VAL GAD_FN(cond_to_reg)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int cc);

static void GAD_FN(get_data_type_size)(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // above 64bits we really dont care that much about natural alignment
            bool is_big_int = dt.data > 64;

            // round up bits to a byte
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data - 1);

            *out_size  = ((bits+7) / 8) << dt.width;
            *out_align = is_big_int ? 8 : ((dt.data + 7) / 8);
            break;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            *out_size = s << dt.width;
            *out_align = s;
            break;
        }
        case TB_PTR: {
            *out_size = 8;
            *out_align = 8;
            break;
        }
        default: tb_unreachable();
    }
}

static bool GAD_FN(fits_into_int32)(TB_Node* n) {
    if (n->type == TB_INTEGER_CONST && n->integer.num_words == 1) {
        uint64_t x = n->integer.single_word;
        int32_t y = x & 0xFFFFFFFF;

        return (int64_t)y == x;
    }

    return false;
}

static void GAD_FN(set_flags)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int cc) {
    ctx->flags_bound = r;
    ctx->flags_code = cc;
}

static void GAD_FN(kill_flags)(Ctx* restrict ctx, TB_Function* f) {
    if (ctx->flags_bound) {
        LISTING("Kill flags: r%d\n", ctx->flags_bound);

        // write value to a more persistent space
        GAD_VAL v = GAD_FN(cond_to_reg)(ctx, f, ctx->flags_bound, ctx->flags_code);
        assert(v.type != GAD_VAL_UNRESOLVED);
        ctx->values[ctx->flags_bound] = v;

        // reset flags
        ctx->flags_bound = TB_NULL_REG;
        ctx->flags_code = -1;
    }
}

typedef struct {
    int start, end;
} LiveInterval;

static LiveInterval get_bb_interval(Ctx* restrict ctx, TB_Function* f, TB_Label bb) {
    LiveInterval li;
    li.start = ctx->ordinal[f->bbs[bb].start];
    li.end = ctx->ordinal[f->bbs[bb].end];

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

static LiveInterval get_live_interval(Ctx* restrict ctx, TB_Reg r) {
    LiveInterval li;
    li.start = ctx->ordinal[r];
    li.end = ctx->ordinal[ctx->intervals[r]];

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

// a lives throughout the entire lifetime of b
static bool GAD_FN(encompass)(Ctx* restrict ctx, TB_Function* f, TB_Reg a, TB_Reg b) {
    LiveInterval a_li = get_live_interval(ctx, a);
    LiveInterval b_li = get_live_interval(ctx, b);
    return b_li.start < a_li.end && b_li.end >= a_li.start;
}

static GAD_VAL* GAD_FN(find_spill)(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    FOREACH_N(i, 0, ctx->spill_count) {
        if (ctx->spills[i].r == r) {
            return &ctx->spills[i];
        }
    }

    return NULL;
}

// Evicts old values and keeps the registers reserved, you'd have to manually free them later
static void GAD_FN(evict)(Ctx* restrict ctx, TB_Function* f, int reg_class, uint64_t regs_to_evict) {
    FOREACH_N(i, 0, ctx->active_count) {
        TB_Reg r = ctx->active[i];

        if (r != 0 && ctx->values[r].type == GAD_VAL_REGISTER + reg_class &&
            ((1u << ctx->values[r].reg) & regs_to_evict) != 0) {
            // replace spill with r
            ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);

            // spill (we're saving the old value because we need to restore before leaving
            // this basic block)
            GAD_VAL* restore_val = GAD_FN(find_spill)(ctx, f, r);
            GAD_VAL slot = GAD_MAKE_STACK_SLOT(ctx, f, r, -ctx->stack_usage);
            if (restore_val == NULL) {
                restore_val = &ctx->spills[ctx->spill_count++];
                *restore_val = ctx->values[r];
            }
            ctx->values[r] = slot;
            ctx->values[r].is_spill = true;

            DBG("  spill r%u to [rbp - %d]\n", r, ctx->stack_usage);
            GAD_FN(spill)(ctx, f, &slot, restore_val, r);

            // TODO(NeGate): sort by increasing end point
            if (i != ctx->active_count - 1) {
                ctx->active[i] = ctx->active[ctx->active_count - 1];
            }
            ctx->active_count -= 1;
        }
    }

    ctx->free_regs[reg_class].data[0] |= regs_to_evict;
}

static void GAD_FN(explicit_steal)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, TB_Reg spill_reg, ptrdiff_t active_i) {
    // replace spill with r
    ctx->stack_usage = align_up(ctx->stack_usage + 8, 8);

    GAD_VAL slot = GAD_MAKE_STACK_SLOT(ctx, f, r, -ctx->stack_usage);
    slot.dt = f->nodes[r].dt;
    ctx->values[r] = ctx->values[spill_reg];

    // spill (we're saving the old value because we need to restore before leaving
    // this basic block)
    GAD_VAL* restore_val = &ctx->spills[ctx->spill_count++];
    *restore_val = ctx->values[spill_reg];
    ctx->values[spill_reg] = slot;

    DBG("  steal r%u for r%u\n", spill_reg, r);
    DBG("  spill r%u to [rbp - %d]\n", spill_reg, ctx->stack_usage);
    GAD_FN(spill)(ctx, f, &slot, &ctx->values[r], r);

    // TODO(NeGate): sort by increasing end point
    ctx->active[active_i] = r;
}

static bool GAD_FN(free_reg)(Ctx* restrict ctx, TB_Function* f, int reg_class, int reg_num) {
    set_remove(&ctx->free_regs[reg_class], reg_num);

    FOREACH_N(i, 0, ctx->active_count) {
        TB_Reg r = ctx->active[i];
        if (ctx->values[r].type == GAD_VAL_REGISTER + reg_class && ctx->values[r].reg == reg_num) {
            DBG("free %s\n", GPR_NAMES[reg_num]);

            if (i != ctx->active_count - 1) {
                ctx->active[i] = ctx->active[ctx->active_count - 1];
            }
            ctx->values[r] = (GAD_VAL){ 0 };
            ctx->active_count -= 1;
            return true;
        }
    }

    return false;
}

static GAD_VAL GAD_FN(steal)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int reg_class, int reg_num) {
    DBG("steal %s for r%u\n", GPR_NAMES[reg_num], r);
    FOREACH_N(i, 0, ctx->active_count) {
        TB_Reg spill_reg = ctx->active[i];
        // LiveInterval spill_li = get_live_interval(ctx, spill_reg);

        if (ctx->values[spill_reg].type == GAD_VAL_REGISTER + reg_class && ctx->values[spill_reg].reg == reg_num) {
            // spill_li.end > r_li.end;
            // it's not a register in the proper regclass and it's not outliving r
            GAD_FN(explicit_steal)(ctx, f, r, spill_reg, i);
            return ctx->values[r];
        }
    }

    DBG("  was free\n");
    assert(!set_get(&ctx->free_regs[reg_class], reg_num));
    set_put(&ctx->free_regs[reg_class], reg_num);
    GAD_VAL v = {
        .type = GAD_VAL_REGISTER + reg_class,
        .r = r, .dt = f->nodes[r].dt, .reg = reg_num
    };

    ctx->values[r] = v;
    ctx->active[ctx->active_count++] = r;
    return v;
}

static GAD_VAL GAD_FN(regalloc2)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int reg_class, bool can_recycle, TB_Reg phi_steal) {
    TB_DataType dt = f->nodes[r].dt;
    LiveInterval r_li = get_live_interval(ctx, r);
    DBG("r%u (t=%d .. %d)\n", r, r_li.start, r_li.end);

    if (ctx->active_count == ctx->free_regs[reg_class].capacity) {
        FOREACH_N(i, 0, ctx->active_count) {
            TB_Reg spill_reg = ctx->active[i];

            if (ctx->values[spill_reg].type == GAD_VAL_REGISTER + reg_class) {
                GAD_FN(explicit_steal)(ctx, f, r, spill_reg, i);
                goto done;
            }
        }

        assert(0 && "Could not find spillable values");
    } else {
        /*if (phi_steal != TB_NULL_REG && tb_node_is_phi_node(f, phi_steal) && ctx->values[phi_steal].type != 0) {
            int count = tb_node_get_phi_width(f, phi_steal);
            TB_PhiInput* inputs = tb_node_get_phi_inputs(f, phi_steal);

            FOREACH_N(i, 0, count) {
                if (inputs[i].val == r) {
                    // check if we're the last use of the PHI in the basic block.
                    // TODO(NeGate): this is a very conservative way to do so lmao, fix it
                    if (tb_node_is_terminator(f, f->nodes[r].next)) {
                        ctx->values[r] = ctx->values[phi_steal];
                        ctx->values[r].is_ref = true;
                        goto done;
                    }
                }
            }
        }*/

        if (can_recycle && ctx->active_count > 0) {
            DBG("  try recycling r%u\n", r);
            FOREACH_N(k, 0, ctx->active_count) {
                TB_Reg other_r = ctx->active[k];
                if (other_r == 0) continue;

                LiveInterval other_li = get_live_interval(ctx, other_r);
                if (other_li.end == r_li.start) {
                    DBG("  recycle r%u for r%u\n", other_r, r);

                    ctx->values[other_r] = (GAD_VAL){ 0 };
                    ctx->values[r] = ctx->values[other_r];
                    ctx->active[k] = r;
                    goto done;
                }
            }
        }

        ptrdiff_t reg_num = set_pop_any(&ctx->free_regs[reg_class]);
        assert(reg_num >= 0);

        DBG("  assign to %s\n", GPR_NAMES[reg_num]);
        ctx->values[r] = (GAD_VAL){
            .type = GAD_VAL_REGISTER + reg_class,
            .r = r, .dt = dt, .reg = reg_num
        };
        set_put(&ctx->free_regs[reg_class], reg_num);

        // TODO(NeGate): sort by increasing end point
        ctx->active[ctx->active_count++] = r;
    }

    done:
    return ctx->values[r];
}

static GAD_VAL GAD_FN(regalloc_tmp)(Ctx* restrict ctx, TB_Function* f, TB_DataType dt, int reg_class) {
    ptrdiff_t reg_num = set_pop_any(&ctx->free_regs[reg_class]);
    assert(reg_num >= 0 && "Could not allocate temporary");

    DBG("allocate temporary %s\n", GPR_NAMES[reg_num]);
    set_put(&ctx->free_regs[reg_class], reg_num);

    assert(ctx->tmp_count+1 < 4);
    ctx->tmps[ctx->tmp_count++] = (RegisterRef){ reg_class, reg_num };

    return (GAD_VAL){
        .type = GAD_VAL_REGISTER + reg_class, .dt = dt, .reg = reg_num
    };
}

static GAD_VAL GAD_FN(regalloc)(Ctx* restrict ctx, TB_Function* f, TB_Reg r, int reg_class) {
    return GAD_FN(regalloc2)(ctx, f, r, reg_class, false, 0);
}

// done before running an instruction
static void GAD_FN(regalloc_step)(Ctx* restrict ctx, TB_Function* f, TB_Reg r) {
    LiveInterval r_li = get_live_interval(ctx, r);

    // expire old intervals
    FOREACH_REVERSE_N(j, 0, ctx->active_count) {
        TB_Reg k = ctx->active[j];
        if (k == 0) {
            continue;
        }

        LiveInterval k_li = get_live_interval(ctx, k);
        if (k_li.end >= r_li.start) {
            // TODO(NeGate): If we sorted by end time, we could make this continue into a break
            continue;
        }

        // remove from active
        DBG("  expired r%u\n", k);
        if (j != ctx->active_count - 1) {
            ctx->active[j] = ctx->active[ctx->active_count - 1];
        }

        // add back to register pool
        if ((ctx->values[k].type >= GAD_VAL_REGISTER && ctx->values[k].type < GAD_VAL_REGISTER + GAD_NUM_REG_FAMILIES) && !ctx->values[k].is_ref) {
            DBG("    free %s\n", GPR_NAMES[ctx->values[k].reg]);
            set_remove(&ctx->free_regs[ctx->values[k].type - GAD_VAL_REGISTER], ctx->values[k].reg);
        }

        ctx->values[k] = (GAD_VAL){ 0 };
        ctx->active_count -= 1;
    }
}

static void GAD_FN(eval_bb_edge)(Ctx* restrict ctx, TB_Function* f, TB_Label from, TB_Label to) {
    TB_FOR_NODE(r, f, to) {
        if (tb_node_is_phi_node(f, r)) {
            int count = tb_node_get_phi_width(f, r);
            TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

            GAD_FN(regalloc_step)(ctx, f, r);

            // if the destination is not initialized, do that
            if (ctx->values[r].type == 0) {
                ctx->values[r] = GAD_FN(phi_alloc)(ctx, f, r);
            }

            FOREACH_N(j, 0, count) {
                if (inputs[j].label == from) {
                    TB_Reg src = inputs[j].val;

                    if (src != TB_NULL_REG) {
                        GAD_FN(move)(ctx, f, r, src);
                    }
                }
            }
        }
    }
}

// returns the register for the next basic block
static void GAD_FN(eval_bb)(Ctx* restrict ctx, TB_Function* f, TB_Label bb, TB_Label fallthrough) {
    ctx->emit.labels[bb] = GET_CODE_POS(&ctx->emit);

    TB_Reg bb_end = f->bbs[bb].end;
    TB_FOR_NODE(r, f, bb) {
        if (r == bb_end) break;
        TB_Node* restrict n = &f->nodes[r];
        TB_NodeTypeEnum reg_type = n->type;
        // TB_DataType dt = n->dt;

        GAD_FN(kill_flags)(ctx, f);
        GAD_FN(regalloc_step)(ctx, f, r);

        switch (reg_type) {
            case TB_NULL:
            case TB_PARAM:
            case TB_LOCAL:
            case TB_PARAM_ADDR:
            case TB_PHI1:
            case TB_PHI2:
            case TB_PHIN:
            break;

            case TB_LINE_INFO: {
                f->lines[f->line_count++] = (TB_Line) {
                    .file = n->line_info.file,
                    .line = n->line_info.line,
                    .pos = GET_CODE_POS(&ctx->emit)
                };
                break;
            }

            case TB_INITIALIZE: {
                GAD_FN(initializer)(ctx, f, r);
                break;
            }

            case TB_STORE: {
                GAD_FN(store)(ctx, f, r);
                break;
            }

            case TB_CALL:
            case TB_SCALL:
            case TB_VCALL: {
                // if we delay resolution we can avoid weird spills and shuffles for
                // the parameters
                GAD_FN(call)(ctx, f, r);
                break;
            }

            default:
            // side effects need special handling, everything else is
            // just added to the queue and resolved at the lastest point
            if (TB_IS_NODE_SIDE_EFFECT(reg_type)) {
                tb_todo();
            }

            GAD_FN(eval)(ctx, f, r);
            break;
        }

        ctx->values[r].r = r;
        if (ctx->tmp_count > 0) {
            FOREACH_N(i, 0, ctx->tmp_count) {
                RegisterRef rr = ctx->tmps[i];
                set_remove(&ctx->free_regs[rr.class], rr.num);
            }
            ctx->tmp_count = 0;
        }
    }

    GAD_FN(regalloc_step)(ctx, f, bb_end);

    // Evaluate the terminator
    TB_Node* end = &f->nodes[bb_end];
    TB_NodeTypeEnum end_type = end->type;

    LiveInterval bb_li = get_bb_interval(ctx, f, bb);
    FOREACH_N(i, 0, ctx->spill_count) {
        GAD_VAL* restore_val = &ctx->spills[i];
        LiveInterval r_li = get_live_interval(ctx, restore_val->r);

        // restore spilled regs... except if it's basic block local
        if (!(r_li.start < bb_li.end && r_li.end >= bb_li.start)) {
            DBG("restore r%u\n", restore_val->r);
            GAD_FN(spill)(ctx, f, restore_val, &ctx->values[restore_val->r], restore_val->r);
        }

        ctx->values[restore_val->r] = *restore_val;
        *restore_val = (GAD_VAL){ 0 };
    }
    ctx->spill_count = 0;

    switch (end_type) {
        case TB_NULL: break;

        case TB_GOTO: {
            GAD_FN(kill_flags)(ctx, f);
            GAD_FN(eval_bb_edge)(ctx, f, bb, end->goto_.label);

            if (end->goto_.label != fallthrough) {
                GAD_FN(goto)(ctx, end->goto_.label);
            }
            break;
        }

        case TB_RET: {
            GAD_FN(kill_flags)(ctx, f);

            if (end->ret.value) {
                GAD_FN(return)(ctx, f, end);
            }

            // Only jump if we aren't literally about to end the function
            if (end->next != fallthrough) {
                GAD_FN(ret_jmp)(ctx);
            }
            break;
        }

        case TB_IF: {
            TB_Label if_true = end->if_.if_true;
            TB_Label if_false = end->if_.if_false;

            if (end->if_.cond != ctx->flags_bound) {
                GAD_FN(kill_flags)(ctx, f);
            }

            // Resolve edges
            GAD_FN(eval_bb_edge)(ctx, f, bb, if_true);
            GAD_FN(eval_bb_edge)(ctx, f, bb, if_false);

            GAD_FN(branch_if)(ctx, f, end->if_.cond, if_true, if_false, fallthrough);
            break;
        }

        default: tb_todo();
    }

    // unbind flags now
    ctx->flags_bound = 0;
}

static TB_FunctionOutput GAD_FN(compile_function)(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, size_t local_thread_id) {
    s_local_thread_id = local_thread_id;
    TB_TemporaryStorage* tls = tb_tls_allocate();
    TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);

    //bool is_ctx_heap_allocated = false;
    Ctx* restrict ctx = NULL;
    {
        size_t ctx_size = sizeof(Ctx) + (2 * f->node_count * sizeof(GAD_VAL));
        FunctionTallySimple tally = tally_memory_usage_simple(f);

        ctx = tb_tls_push(tls, ctx_size);
        *ctx = (Ctx){
            .f = f,
            .tls = tls,
            .emit = {
                .f = f,
                .data = out,
                .capacity = out_capacity,
                .labels = tb_tls_push(tls, f->bb_count * sizeof(uint32_t)),
                .label_patches = tb_tls_push(tls, tally.label_patch_count * sizeof(LabelPatch)),
                .ret_patches = tb_tls_push(tls, tally.return_count * sizeof(ReturnPatch)),
            },
            .preds = preds,
            .ordinal = tb_tls_push(tls, f->node_count * sizeof(int)),
            .intervals = tb_tls_push(tls, f->node_count * sizeof(int)),
            .spills = tb_tls_push(tls, f->node_count * sizeof(GAD_VAL))
        };

        f->line_count = 0;
        f->lines = tb_platform_arena_alloc(tally.line_info_count * sizeof(TB_Line));

        memset(ctx->values, 0, f->node_count * sizeof(GAD_VAL));
    }

    tb_function_print(f, tb_default_print_callback, stdout, false);

    // Compute register allocation
    {
        // Find live intervals (and create a timeline for the nodes)
        FOREACH_N(i, 0, f->node_count) ctx->intervals[i] = 0;

        size_t time = 0;
        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(r, f, bb) {
                TB_Node* n = &f->nodes[r];

                if (tb_node_is_phi_node(f, r)) {
                    // for phi nodes just extend the lifetime to the basic block terminator
                    int count = tb_node_get_phi_width(f, r);
                    TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

                    FOREACH_N(j, 0, count) {
                        TB_Reg src = inputs[j].val;
                        TB_Reg bb_end = f->bbs[inputs[j].label].end;

                        if (ctx->ordinal[ctx->intervals[src]] < ctx->ordinal[bb_end]) {
                            ctx->intervals[src] = bb_end;
                        } else if (ctx->intervals[src] == 0) {
                            ctx->intervals[src] = r;
                        }
                    }
                } else {
                    // mark latest use
                    TB_FOR_INPUT_IN_NODE(it, f, n) {
                        ctx->intervals[it.r] = r;
                    }
                }

                ctx->ordinal[r] = time++;
            }
        }

        // Linear scan
        ctx->active = tb_platform_heap_alloc(f->node_count * sizeof(TB_Reg));
        GAD_FN(initial_reg_alloc)(ctx);
        GAD_FN(resolve_params)(ctx, f, ctx->values);
    }

    // calculate the order of the nodes, it helps since node indices
    // don't actually tell us this especially once the optimizer has
    // taken a jab at it.
    //
    // also calculate the maximum parameter usage for a call
    int counter = 0;
    size_t caller_usage = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_PARAM_ADDR || n->type == TB_LOCAL) {
                GAD_FN(resolve_stack_slot)(ctx, f, n);
            } else if (EITHER2(n->type, TB_CALL, TB_VCALL)) {
                int param_usage = CALL_NODE_PARAM_COUNT(n);
                if (caller_usage < param_usage) caller_usage = param_usage;
            }

            ctx->ordinal[n - f->nodes] = counter++;
        }
    }
    dyn_array_trim(ctx->stack_slots);

    // We generate nodes via a postorder walk
    TB_PostorderWalk walk = {
        .visited = tb_tls_push(tls, f->bb_count * sizeof(bool)),
        .traversal = tb_tls_push(tls, f->bb_count * sizeof(TB_Reg)),
    };
    tb_function_get_postorder_explicit(f, &walk);

    assert(walk.traversal[walk.count - 1] == 0 && "Codegen traversal must always start with L0");
    FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        LISTING("Eval BB: L%d\n", bb);
        GAD_FN(eval_bb)(ctx, f, bb, i > 0 ? walk.traversal[i - 1] : 0);
    }

    // Fix up stack usage
    ctx->stack_usage = GAD_FN(resolve_stack_usage)(ctx, f, ctx->stack_usage, caller_usage);

    // TODO(NeGate): resolve function-level patches (returns and labels)
    GAD_FN(resolve_local_patches)(ctx, f);

    // hack to make the first line in a function think it's at
    // the top of the prologue not within the body
    if (f->line_count > 0) {
        f->lines[0].pos = 0;
    }

    // we're done, clean up
    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->emit.data,
        .code_size = ctx->emit.count,
        .stack_usage = ctx->stack_usage,
        .prologue_epilogue_metadata = ctx->regs_to_save,
        .stack_slots = ctx->stack_slots
    };

    return func_out;
}
