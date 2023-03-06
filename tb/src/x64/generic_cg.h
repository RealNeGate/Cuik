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
typedef struct {
    TB_Node* key;

    // if non-NULL we want to make a mutable copy, we use
    // this to define what we copy from.
    TB_Node* based;

    int t, user_count;
    int start, end;

    // regalloc
    int8_t reg_class;
    int8_t reg;
} Liveness;

typedef struct {
    TB_Node* key;
    Val val;
} ValueDesc;

typedef struct {
    int start, end;
} LiveInterval;

typedef struct {
    TB_Node* n;
    Val v;
} Reload;

typedef struct Sequence {
    struct Sequence* next;

    int inst_count, label;
    Inst insts[4];
} Sequence;

typedef struct {
    TB_CGEmitter emit;
    bool emit_asm;

    // for panic-based error handling
    jmp_buf restore_point;

    // machine output sequences
    Sequence *first, *last;

    // hash map
    size_t interval_count, interval_exp;
    Liveness* intervals;

    TB_Label fallthrough;

    // Stack
    uint32_t stack_usage;

    // Reg alloc
    DynArray(Reload) reloads;

    TB_Node** active;
    size_t active_count;

    Set free_regs[CG_REGISTER_CLASSES];

    // current value table
    size_t values_count, values_exp;
    ValueDesc* values;
} Ctx;

#define NAME(n) (get_liveness(ctx, n)->t)
static Liveness* get_liveness(Ctx* restrict ctx, TB_Node* n) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->interval_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->interval_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->intervals[i].key == NULL) {
            return NULL;
        } else if (ctx->intervals[i].key == n) {
            return &ctx->intervals[i];
        }
    }
}

static void put_liveness(Ctx* restrict ctx, TB_Node* n, int ordinal) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->interval_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->interval_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->intervals[i].key == NULL) {
            assert(ctx->interval_count + 1 < (1u << ctx->interval_exp));

            // new slot
            ctx->interval_count++;
            ctx->intervals[i] = (Liveness){ .key = n, .t = ordinal, .start = ordinal, .end = ordinal };
            return;
        } else if (ctx->intervals[i].key == n) {
            assert(0 && "Huh?");
        }
    }
}

static LiveInterval get_bb_interval(Ctx* restrict ctx, TB_Function* f, TB_Label bb) {
    Liveness* l  = get_liveness(ctx, f->bbs[bb].start);
    Liveness* l2 = get_liveness(ctx, f->bbs[bb].end);

    LiveInterval li;
    li.start = l->start;
    li.end = l2->end;

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

static LiveInterval get_live_interval(Ctx* restrict ctx, TB_Node* n) {
    Liveness* l = get_liveness(ctx, n);
    return (LiveInterval){ l->start, l->end };
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
            assert(ctx->values_count + 1 < (1u << ctx->values_exp));

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

static int classify_reg_class(TB_DataType dt);
static bool should_tile(TB_Node* n);
static Val isel(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n);
static void emit_sequence(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n);
static void patch_local_labels(Ctx* restrict ctx);
static Val spill_to_stack_slot(Ctx* restrict ctx, Sequence* restrict seq, TB_Node* n, Val* src);
static void copy_value(Ctx* restrict ctx, Val* dst, Val* src, TB_DataType dt, bool load, TB_Node* n, const char* reason);

// references an allocated
#define USE(x) (-((x) + 2))
#define DEF(n, rg) def(ctx, n, rg)
#define DEF_BASED(n, src, rg) def_based(ctx, n, src, rg)

static int def(Ctx* restrict ctx, TB_Node* n, int reg_class) {
    Liveness* l = get_liveness(ctx, n);
    l->reg_class = reg_class;
    return l - ctx->intervals;
}

static int def_based(Ctx* restrict ctx, TB_Node* n, TB_Node* src, int reg_class) {
    Liveness* l = get_liveness(ctx, n);
    l->based = src;
    l->reg_class = reg_class;
    return l - ctx->intervals;
}

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

static ptrdiff_t find_active(Ctx* restrict ctx, int reg_class, int reg_num) {
    FOREACH_N(i, 0, ctx->active_count) {
        Val* v = &GET_VAL(ctx->active[i]);

        if (v->type == CG_VAL_REGISTER + reg_class && v->reg == reg_num) {
            return i;
        }
    }

    return 0;
}

static void remove_active(Ctx* restrict ctx, size_t i) {
    if (i + 1 != ctx->active_count) {
        memmove(&ctx->active[i], &ctx->active[i + 1], (ctx->active_count - i) * sizeof(TB_Node*));
    }
    ctx->active_count -= 1;
}

// returns false if we need to spill
static bool alloc_reg(Ctx* restrict ctx, TB_Function* f, TB_Node* n, int reg_class) {
    LiveInterval r_li = get_live_interval(ctx, n);

    ptrdiff_t reg_num = set_pop_any(&ctx->free_regs[reg_class]);
    if (reg_num < 0) {
        // ctx->active_count == ctx->free_regs[reg_class].capacity
        return false;
    }

    Val v = {
        .type = CG_VAL_REGISTER + reg_class,
        .dt = n->dt, .reg = reg_num
    };

    GET_VAL(n) = v;
    set_put(&ctx->free_regs[reg_class], reg_num);

    // mark register as to be saved
    // ctx->regs_to_save[reg_class] |= (1u << reg_num) & ctx->callee_saved[reg_class];

    add_active(ctx, n, r_li);
    printf("  \x1b[32m#   assign r%d to %s\x1b[0m\n", NAME(n), GPR_NAMES[reg_num]);
    return true;
}

static void trap(Ctx* restrict ctx, int code) {
    longjmp(ctx->restore_point, code);
}

static size_t estimate_hash_map_size(size_t s) {
    // allocate values map and active, for linear scan
    size_t ht_cap = tb_next_pow2((s * 8) / 5);
    size_t ht_exp = 32 - tb_clz(ht_cap - 1);

    assert(ht_cap == (1u << ht_exp));
    return ht_exp;
}

static void phi_edge(TB_Function* f, Ctx* restrict ctx, Sequence* restrict seq, TB_Label src, TB_Label dst) {
    TB_FOR_NODE(n, f, dst) {
        if (n->type == TB_NULL) continue;
        if (n->type != TB_PHI) break;

        Val* dst = &GET_VAL(n);
        if (dst->type == 0) {
            if (!alloc_reg(ctx, f, n, classify_reg_class(n->dt))) {
                tb_panic("Cannot allocate phi!\n");
            }
        }

        // handle phis
        TB_NodePhi* phi = TB_NODE_GET_EXTRA(n);
        FOREACH_N(i, 0, n->input_count) if (phi->labels[i] == src) {
            Val* src_v = &GET_VAL(n->inputs[i]);

            copy_value(ctx, dst, src_v, n->dt, false, n, "phi move");
            break;
        }
    }
}

// ctx->active[return value]
static ptrdiff_t find_best_spill(Ctx* restrict ctx) {
    return 0;
}

static void alloc_reg_allow_spill(Ctx* restrict ctx, TB_Function* f, Sequence* seq, TB_Node* n, int reg_class) {
    if (!alloc_reg(ctx, f, n, reg_class)) {
        ptrdiff_t spill = find_best_spill(ctx);
        TB_Node* spill_n = ctx->active[spill];

        // we need to reload the spill before we leave the BB
        Val* spill_v = &GET_VAL(spill_n);
        dyn_array_put(ctx->reloads, (Reload){ spill_n, *spill_v });

        spill_to_stack_slot(ctx, seq, spill_n, spill_v);

        // free register
        set_remove(&ctx->free_regs[spill_v->type - CG_VAL_REGISTER], spill_v->reg);
        remove_active(ctx, spill);
    }
}

static void expire_intervals(Ctx* restrict ctx, int current_time) {
    for (size_t i = 0; i < ctx->active_count;) {
        TB_Node* k = ctx->active[i];
        LiveInterval k_li = get_live_interval(ctx, k);
        if (k_li.end >= current_time) break;

        Val* v = &GET_VAL(k);
        if (v->type >= CG_VAL_REGISTER && v->type < CG_VAL_REGISTER + CG_REGISTER_CLASSES) {
            printf("  \x1b[32m#   free %s from r%d\x1b[0m\n", GPR_NAMES[v->reg], NAME(k));
            set_remove(&ctx->free_regs[v->type - CG_VAL_REGISTER], v->reg);
            remove_active(ctx, i);
        } else {
            i++;
        }
    }
}

static int compare_intervals(const void* a, const void* b) {
    Liveness* aa = *(Liveness**) a;
    Liveness* bb = *(Liveness**) b;

    return aa->start - bb->start;
}

static void linear_scan(Ctx* restrict ctx, TB_Function* f, size_t node_count) {
    // sort by starting point
    Liveness** sorted = ARENA_ARR_ALLOC(&tb__arena, f->node_count, Liveness*);
    size_t j = 0;
    FOREACH_N(i, 0, ctx->interval_exp) if (ctx->intervals[i].key) {
        sorted[j++] = &ctx->intervals[i];
    }
    qsort(sorted, j, sizeof(Liveness*), compare_intervals);

    // apple
    FOREACH_N(i, 0, j) {
        int time = sorted[i]->t;

        expire_intervals(ctx, time);
        __debugbreak();
    }

    #if 0

    // allocate any definitions in the sequence
    FOREACH_N(i, 0, seq->def_count) {
        Def* d = &seq->defs[i];
        Val* src = &GET_VAL(d->n);
        TB_DataType dt = d->n->dt;

        // copied from somewhere else
        Val* based = NULL;
        if (d->based) {
            based = &GET_VAL(d->based);

            if (based->type == 0 && d->based->type == TB_PHI) {
                // allocate phi node late
                if (!alloc_reg(ctx, f, d->based, classify_reg_class(n->dt))) {
                    tb_panic("Cannot allocate phi!\n");
                }
            }

            // if the source dies when the definition is born, we can just alias them
            LiveInterval src_li = get_live_interval(ctx, d->based);
            if (src_li.end >= bb_start && src_li.end <= time && based->type == CG_VAL_REGISTER + d->reg_class) {
                if (ctx->emit_asm) {
                    printf("  \x1b[32m#   recycled r%d for r%d\x1b[0m\n", NAME(d->based), NAME(n));
                }

                // remove from active list
                FOREACH_N(j, 0, ctx->active_count) if (ctx->active[j] == d->based) {
                    remove_active(ctx, j);
                    break;
                }

                // insert new to active
                add_active(ctx, n, get_live_interval(ctx, n));
                d->reg = based->reg;
                continue;
            }
        }

        if (src->type != CG_VAL_REGISTER + d->reg_class || d->load) {
            // try normal allocation
            Val old = *src;
            alloc_reg_allow_spill(ctx, f, seq, d->n, d->reg_class);

            if (d->based == NULL && old.type != 0) {
                copy_value(ctx, src, &old, dt, d->load, d->n, "materialize");
                dyn_array_put(ctx->reloads, (Reload){ d->n, old });
            }
        }

        if (d->based) {
            copy_value(ctx, src, based, dt, d->load, d->n, "clone");
        }

        d->reg = src->reg;
    }
    #endif
}

static Sequence* alloc_sequence(int label) {
    Sequence* restrict s = arena_alloc(&tb__arena, sizeof(Sequence), _Alignof(Sequence));
    s->next = NULL;
    s->label = label;
    s->inst_count = 0;
    return s;
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

    // Live intervals:
    //   We compute this for register allocation along
    //   with the "ordinals" which act as our timeline.
    ctx->interval_exp = estimate_hash_map_size(f->node_count);
    size_t interval_cap = (1u << ctx->interval_exp);

    ctx->intervals = arena_alloc(&tb__arena, interval_cap * sizeof(Liveness), _Alignof(Liveness));
    memset(ctx->intervals, 0, interval_cap * sizeof(Liveness));

    int counter = 0, label_patch_count = 0, return_count = 0, caller_usage = 0;
    FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        TB_FOR_NODE(n, f, bb) if (n->type != TB_NULL) {
            put_liveness(ctx, n, counter++);
        }
    }

    // allocate values map and active, for linear scan
    ctx->values_exp = estimate_hash_map_size(counter);
    size_t values_cap = (1u << ctx->values_exp);

    ctx->values = arena_alloc(&tb__arena, values_cap * sizeof(ValueDesc), _Alignof(ValueDesc));
    ctx->active = arena_alloc(&tb__arena, counter * sizeof(TB_Node*), _Alignof(TB_Node*));
    memset(ctx->values, 0, values_cap * sizeof(ValueDesc));

    CUIK_TIMED_BLOCK("liveness analysis") {
        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(n, f, bb) if (n->type != TB_NULL) {
                int t = get_liveness(ctx, n)->t;

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
                } else if (n->type == TB_PHI) {
                    Liveness* l = get_liveness(ctx, n);

                    TB_NodePhi* phi = TB_NODE_GET_EXTRA(n);
                    FOREACH_N(j, 0, n->input_count) {
                        Liveness* l2 = get_liveness(ctx, n->inputs[j]);

                        // extend last user
                        int t2 = NAME(f->bbs[phi->labels[j]].end);

                        // mark range
                        if (t2 < l->start) l->start = t2;
                        if (t2 > l->end) l->end = t2;

                        // mark self range
                        if (t2 < l2->start) l2->start = t2;
                        if (t2 > l2->end) l2->end = t2;

                        l->user_count++;
                    }
                    continue;
                }

                TB_FOR_INPUT_IN_NODE(in, n) if (*in) {
                    // extend last user
                    Liveness* l = get_liveness(ctx, *in);
                    assert(l);

                    // mark range
                    if (t < l->start) l->start = t;
                    if (t > l->end) l->end = t;

                    l->user_count++;
                }
            }
        }
    }

    // allocate more stuff now that we've run stats on the IR
    ctx->emit.labels = ARENA_ARR_ALLOC(&tb__arena, f->bb_count, uint32_t);
    ctx->emit.label_patches = ARENA_ARR_ALLOC(&tb__arena, label_patch_count, LabelPatch);
    ctx->emit.ret_patches = ARENA_ARR_ALLOC(&tb__arena, return_count, ReturnPatch);

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    ctx->emit_asm = true;
    CUIK_TIMED_BLOCK("isel") FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        // mark BB start & fallthrough
        ctx->fallthrough = (i > 0 ? walk.traversal[i - 1] : -1);

        TB_FOR_NODE(n, f, bb) if (n->type != TB_NULL) {
            if (n->type == TB_LINE_INFO) {
                /*f->lines[f->line_count++] = (TB_Line) {
                    .file = n->line_info.file,
                    .line = n->line_info.line,
                    .pos = GET_CODE_POS(&ctx->emit)
                };*/
                continue;
            }

            Sequence* seq = alloc_sequence(bb);
            GET_VAL(n) = isel(ctx, seq, n);

            if (ctx->last == NULL) {
                ctx->first = ctx->last = seq;
            } else {
                ctx->last->next = seq;
                ctx->last = seq;
            }

            // Handle branch edges
            if (n->type == TB_BRANCH) {
                // write active phi-edges
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                FOREACH_REVERSE_N(i, 0, br->count) {
                    phi_edge(f, ctx, seq, bb, br->targets[i].value);
                }
                phi_edge(f, ctx, seq, bb, br->default_label);

                if (dyn_array_length(ctx->reloads)) {
                    // insert reloads
                    dyn_array_for(i, ctx->reloads) {
                        Reload* restrict r = &ctx->reloads[i];

                        Val* src = &GET_VAL(r->n);
                        copy_value(ctx, &r->v, src, r->n->dt, false, r->n, "reload");
                    }

                    // clear
                    dyn_array_clear(ctx->reloads);
                }
            }

        }
    }

    CUIK_TIMED_BLOCK("linear scan") {
        linear_scan(ctx, f, counter);
    }

    CUIK_TIMED_BLOCK("emit sequences") {
        int last = -1;
        for (Sequence* seq = ctx->first; seq; seq = seq->next) {
            // mark label
            if (last != seq->label) {
                ctx->emit.labels[seq->laebel] = GET_CODE_POS(&ctx->emit);

                if (ctx->emit_asm) printf("L%d:\n", bb);
                last = seq->label;
            }

            TB_Node* n = seq->node;
            if (emit_asm) printf("  \x1b[32m# %s sequence\x1b[0m\n", tb_node_get_name(n));

            // Arch-specific: convert instruction buffer into actual instructions
            emit_sequence(ctx, &seq, n);
        }
    }

    // __debugbreak();
    if (ctx->emit_asm) {
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
