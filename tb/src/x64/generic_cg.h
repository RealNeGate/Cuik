#include "../tb_internal.h"
#include "../codegen/emitter.h"

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

static void get_data_type_size(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align) {
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

// per instruction selection
enum {
    CG_MAX_DEFS  = 8,
    CG_MAX_INSTS = 8,
};

typedef struct {
    TB_Node* last_use;
    int user_count;
    int ordinal;
} Liveness;

typedef struct {
    TB_Node* key;
    Val val;
} ValueDesc;

typedef struct {
    int start, end;
} LiveInterval;

typedef struct Def {
    TB_Node* n;

    // if live_in and we want to make a mutable copy, we use
    // this to define what we copy from.
    TB_Node* based;

    // which pool of registers to grab from
    int reg_class;

    // live_in
    bool live_in, live_out;

    // resolved value
    int reg;
} Def;

typedef struct Ctx {
    TB_CGEmitter emit;

    // for panic-based error handling
    jmp_buf restore_point;

    // some analysis
    NL_Map(TB_Node*, Liveness) intervals;

    // Stack
    uint32_t stack_usage;

    // Reg alloc
    TB_Node** active;
    size_t active_count;

    Set free_regs[CG_REGISTER_CLASSES];

    // instruction queue
    size_t inst_count, def_count;
    Def defs[CG_MAX_DEFS];
    Inst insts[CG_MAX_INSTS];

    // current value table
    size_t values_count, values_exp;
    ValueDesc* values;
} Ctx;

static LiveInterval get_bb_interval(Ctx* restrict ctx, TB_Function* f, TB_Label bb) {
    ptrdiff_t search = nl_map_get(ctx->intervals, f->bbs[bb].start);
    assert(search >= 0);

    ptrdiff_t search2 = nl_map_get(ctx->intervals, f->bbs[bb].end);
    assert(search2 >= 0);

    LiveInterval li;
    li.start = ctx->intervals[search].v.ordinal;
    li.end = ctx->intervals[search2].v.ordinal;

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

static LiveInterval get_live_interval(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->intervals, n);
    assert(search >= 0);

    TB_Node* last_use = ctx->intervals[search].v.last_use;
    if (last_use == NULL) {
        LiveInterval li;
        li.start = ctx->intervals[search].v.ordinal;
        li.end = INT_MAX;
        return li;
    }

    ptrdiff_t search2 = nl_map_get(ctx->intervals, last_use);
    assert(search2 >= 0);

    LiveInterval li;
    li.start = ctx->intervals[search].v.ordinal;
    li.end = ctx->intervals[search2].v.ordinal;

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

#define GET_VAL(n) (*get_val(ctx, n))
static Val* get_val(Ctx* restrict ctx, TB_Node* n) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->values_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->values_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->values[i].key == NULL) {
            assert(ctx->values_count < (1u << ctx->values_exp));

            // new slot
            ctx->values_count++;
            ctx->values[i].key = n;
            ctx->values[i].val = (Val){ 0 };
            return &ctx->values[i].val;
        } else if (ctx->values[i].key == n) {
            return &ctx->values[i].val;
        }
    }
}

static bool fits_into_int32(uint64_t x) {
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

#define SUBMIT_INST(op, ...) (ctx->insts[ctx->inst_count++] = (X86_Inst){ op, __VA_ARGS__ })
#define DEF(n, ...) (ctx->defs[ctx->def_count] = (Def){ n, __VA_ARGS__ }, ctx->def_count++)

static Val isel(Ctx* restrict ctx, TB_Node* n);
static void emit_sequence(Ctx* restrict ctx, TB_Node* n, bool emit_asm);
static void patch_local_labels(Ctx* restrict ctx);
static Val spill_to_stack_slot(Ctx* restrict ctx, TB_Node* n, TB_Node* p, Val* src);
static Val get_initial_param_val(Ctx* restrict ctx, TB_Node* n);
static void copy_value(Ctx* restrict ctx, Val* dst, Val* src, TB_DataType dt, bool emit_asm);

static void add_active(Ctx* restrict ctx, TB_Node* n, LiveInterval r_li) {
    // insert by increasing end point
    // TODO(NeGate): do binary insert since the array is sorted
    size_t i = 0;
    for (; i < ctx->active_count; i++) {
        LiveInterval k_li = get_live_interval(ctx, ctx->active[i]);
        if (k_li.end >= r_li.end) break;
    }

    // we know where to insert
    FOREACH_REVERSE_N(j, i, ctx->active_count) {
        ctx->active[j+1] = ctx->active[j];
    }

    ctx->active[i] = n;
    ctx->active_count += 1;
}

static void remove_active(Ctx* restrict ctx, size_t i, TB_Node* n) {
    if (i + 1 == ctx->active_count) {
        memmove(&ctx->active[i], &ctx->active[i + 1], ctx->active_count - 1);
    }
    ctx->active_count -= 1;
}

static Val alloc_reg(Ctx* restrict ctx, TB_Function* f, TB_Node* n, int reg_class) {
    LiveInterval r_li = get_live_interval(ctx, n);

    if (ctx->active_count == ctx->free_regs[reg_class].capacity) {
        FOREACH_N(i, 0, ctx->active_count) {
            TB_Node* spill = ctx->active[i];
            Val* spill_v = &GET_VAL(spill);

            if (spill_v->type == CG_VAL_REGISTER + reg_class) {
                // GAD_FN(explicit_steal)(ctx, f, n, spill_reg, i);
                tb_todo();
            }
        }

        tb_unreachable();
        return (Val){ 0 };
    } else {
        ptrdiff_t reg_num = set_pop_any(&ctx->free_regs[reg_class]);
        assert(reg_num >= 0);

        Val v = {
            .type = CG_VAL_REGISTER + reg_class,
            .r = n, .dt = n->dt, .reg = reg_num
        };

        GET_VAL(n) = v;
        set_put(&ctx->free_regs[reg_class], reg_num);

        // mark register as to be saved
        // ctx->regs_to_save[reg_class] |= (1u << reg_num) & ctx->callee_saved[reg_class];

        add_active(ctx, n, r_li);
        return v;
    }
}

static void trap(Ctx* restrict ctx, int code) {
    longjmp(ctx->restore_point, code);
}

// Codegen through here is done in phases
static TB_FunctionOutput compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, size_t local_thread_id) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    tb_function_print(f, tb_default_print_callback, stdout, false);

    Ctx* restrict ctx = arena_alloc(&tb__arena, sizeof(Ctx), _Alignof(Ctx));
    *ctx = (Ctx){
        .emit = {
            .f = f,
            .data = out,
            .capacity = out_capacity,
        }
    };

    // Setup restore point so the codegen can safely exit
    int result_code = setjmp(ctx->restore_point);
    if (result_code != 0) {
        // we had an error
        arena_clear(&tb__arena);
        return (TB_FunctionOutput){ .result = result_code };
    }

    ctx->free_regs[0] = set_create(16);
    ctx->free_regs[1] = set_create(16);

    set_put(&ctx->free_regs[0], RBP), set_put(&ctx->free_regs[0], RSP);

    // Live intervals:
    //   We compute this for register allocation along
    //   with the "ordinals" which act as our timeline.
    int counter = 0, label_patch_count = 0, return_count = 0, caller_usage = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            // create new entry with no last user
            Liveness l = { .ordinal = counter++ };
            nl_map_put(ctx->intervals, n, l);

            if (n->type == TB_BRANCH) {
                label_patch_count += 1 + TB_NODE_GET_EXTRA_T(n, TB_NodeBranch)->count;
            } else if (n->type == TB_RET) {
                return_count += 1;
            } else if (n->type == TB_CALL) {
                // system calls don't count, we track this for ABI
                // and stack allocation purposes.
                if (caller_usage < n->input_count) {
                    caller_usage = n->input_count;
                }
            }

            TB_FOR_INPUT_IN_NODE(in, n) if (*in) {
                // extend last user
                ptrdiff_t search = nl_map_get(ctx->intervals, *in);
                assert(search >= 0 && "use before define?");

                ctx->intervals[search].v.last_use = n;
                ctx->intervals[search].v.user_count++;
            }
        }
    }

    // allocate more stuff now that we've run stats on the IR
    ctx->emit.labels = ARENA_ARR_ALLOC(&tb__arena, f->bb_count, uint32_t);
    ctx->emit.label_patches = ARENA_ARR_ALLOC(&tb__arena, label_patch_count, LabelPatch);
    ctx->emit.ret_patches = ARENA_ARR_ALLOC(&tb__arena, return_count, ReturnPatch);

    // allocate values map and active, for linear scan
    size_t ht_cap = tb_next_pow2((counter * 4) / 3);
    size_t ht_exp = tb_ffs(~(ht_cap - 1)) - 1;

    ctx->values_exp = ht_exp;
    ctx->values = arena_alloc(&tb__arena, ht_cap * sizeof(ValueDesc), _Alignof(ValueDesc));
    ctx->active = arena_alloc(&tb__arena, counter * sizeof(TB_Node*), _Alignof(TB_Node*));

    // BB scheduling:
    //   we run through BBs in a reverse postorder walk, currently
    //   there's no reodering based on branch weights (since we don't
    //   do those but if we did that would go here.
    TB_PostorderWalk walk = {
        .visited = tb_tls_push(tls, f->bb_count * sizeof(bool)),
        .traversal = tb_tls_push(tls, f->bb_count * sizeof(TB_Node*)),
    };
    tb_function_get_postorder_explicit(f, &walk);
    assert(walk.traversal[walk.count - 1] == 0 && "Codegen must always schedule entry BB first");

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    bool emit_asm = true;
    FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        // mark BB start
        ctx->emit.labels[bb] = GET_CODE_POS(&ctx->emit);
        if (emit_asm) {
            printf("L%d:\n", bb);
        }

        TB_FOR_NODE(n, f, bb) {
            if (emit_asm) printf("  \x1b[32m# %s sequence\x1b[0m\n", tb_node_get_name(n));

            // selection
            Val v;
            switch (n->type) {
                case TB_NULL:
                case TB_PHI:
                goto skip;

                case TB_PARAM: {
                    Val v = get_initial_param_val(ctx, n);

                    // if we used a register, mark that
                    if (v.type >= CG_VAL_REGISTER && v.type < CG_VAL_REGISTER + CG_REGISTER_CLASSES) {
                        set_remove(&ctx->free_regs[v.type - CG_VAL_REGISTER], v.reg);
                        add_active(ctx, n, get_live_interval(ctx, n));
                    }
                    GET_VAL(n) = v;
                    goto skip;
                }

                case TB_LOCAL: {
                    Val* src = &GET_VAL(n);
                    assert(src != NULL);
                    goto skip;
                }

                case TB_LINE_INFO: {
                    /*f->lines[f->line_count++] = (TB_Line) {
                        .file = n->line_info.file,
                        .line = n->line_info.line,
                        .pos = GET_CODE_POS(&ctx->emit)
                    };*/
                    goto skip;
                }

                case TB_BRANCH: {
                    // handle phi nodes
                    tb_todo();

                    // do normal isel now
                    assert(ctx->def_count == 0);
                    v = isel(ctx, n);
                    break;
                }

                case TB_STORE: {
                    TB_Node* src = n->inputs[1];
                    Liveness* restrict ni = nl_map_get_checked(ctx->intervals, src);
                    if (n->inputs[0]->type == TB_LOCAL && src->type == TB_PARAM &&
                        ni->user_count == 1 && ni->last_use == n) {
                        // we want to use the stack slot for this local.
                        // we don't even need to fill the TB_PARAM since
                        // it's only used here.
                        TB_Node* local = n->inputs[0];
                        v = GET_VAL(src);

                        Val slot = spill_to_stack_slot(ctx, ni->last_use, src, &v);
                        GET_VAL(local) = slot;
                        goto skip;
                    }

                    v = isel(ctx, n);
                    break;
                }
                default:
                assert(ctx->def_count == 0);
                v = isel(ctx, n);
                break;
            }

            // linear scan alloc
            //   expired old intervals
            LiveInterval r_li = get_live_interval(ctx, n);
            FOREACH_N(i, 0, ctx->active_count) {
                TB_Node* k = ctx->active[i];
                LiveInterval k_li = get_live_interval(ctx, k);
                if (k_li.end >= r_li.start) break;

                Val* v = &GET_VAL(k);
                if (v->type >= CG_VAL_REGISTER && v->type < CG_VAL_REGISTER + CG_REGISTER_CLASSES) {
                    set_remove(&ctx->free_regs[v->type - CG_VAL_REGISTER], v->reg);
                    remove_active(ctx, i, k);
                }
            }
            //   allocate new regs
            FOREACH_N(i, 0, ctx->def_count) {
                Def* d = &ctx->defs[i];

                // copied from somewhere else
                if (d->based) {
                    Val* src = &GET_VAL(d->based);

                    // allocate copy
                    Val copy = alloc_reg(ctx, f, d->n, d->reg_class);
                    copy_value(ctx, &copy, src, d->n->dt, emit_asm);
                    d->reg = copy.reg;
                } else {
                    Val* src = &GET_VAL(d->n);
                    if (src->type != CG_VAL_REGISTER + d->reg_class) {
                        Val old = *src;
                        alloc_reg(ctx, f, d->n, d->reg_class);
                        copy_value(ctx, src, &old, d->n->dt, emit_asm);
                        d->reg = src->reg;
                    } else {
                        d->reg = src->reg;
                    }
                }
            }
            ctx->def_count = 0;

            // resolve any potential allocated reg reference
            if (v.type >= CG_VAL_REGISTER && v.type < CG_VAL_REGISTER + CG_REGISTER_CLASSES) {
                ptrdiff_t def_i = -v.reg - 2;
                v.reg = ctx->defs[def_i].reg;
            }
            GET_VAL(n) = v;

            // Arch-specific: convert instruction buffer into actual instructions
            emit_sequence(ctx, n, emit_asm);

            skip:;
        }
    }

    if (emit_asm) {
        printf(".ret:\n");
    }

    //  Label patching: we make sure any local labels
    patch_local_labels(ctx);

    // we're done, clean up
    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->emit.data,
        .code_size = ctx->emit.count,
        // .stack_usage = ctx->stack_usage,
        // .prologue_epilogue_metadata = ctx->regs_to_save[0],
        // .stack_slots = ctx->stack_slots
    };

    arena_clear(&tb__arena);
    // __debugbreak();
    return func_out;
}
