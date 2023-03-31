#include "../tb_internal.h"
#include "../codegen/emitter.h"

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

enum {
    INST_LABEL = -1,
    INST_LINE  = -2,
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

typedef struct {
    TB_Node* key;
    int t, user_count;
} NodeMeta;

typedef struct {
    TB_Node* key;
    int val;
} ValueDesc;

typedef struct Sequence {
    struct Sequence* next;
    TB_Node* node;

    int inst_count, label, time;
    Inst insts[16];
} Sequence;

typedef struct MachineBB {
    Inst* first;

    // on the timeline/slot indices
    int start, end;

    // local live sets
    Set gen, kill;
    // global
    Set live_in, live_out;
} MachineBB;

typedef struct MachineReg {
    uint8_t class, num;
} MachineReg;

typedef struct Clobbers {
    int count;
    MachineReg _[];
} Clobbers;

typedef ptrdiff_t DefIndex;
typedef struct Def {
    TB_Node* node;

    // lifetime
    int start, end;

    // regalloc
    int16_t complete, hint;
    int16_t reg_class, reg;

    // when we preallocate a definition we
    // specify here which definition must
    // be completed for it to be free again
    DefIndex live_until;

    // once the def is live, these registers are clobbered
    Clobbers* clobbers;
} Def;

typedef struct Ctx {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_ABI target_abi;

    // machine output sequences
    Inst *first, *last;
    DynArray(Def) defs;
    DynArray(Reload) reloads;
    DynArray(TB_Node*) in_bound;
    MachineBB* seq_bb;

    // hash map
    size_t meta_count, meta_exp;
    NodeMeta* meta;

    bool in_fence;
    TB_Label fallthrough;

    // Stack
    uint32_t stack_usage;
    NL_Map(TB_Node*, int) stack_slots;

    // Reg alloc
    DefIndex* active;
    size_t active_count;

    Set used_regs[CG_REGISTER_CLASSES];

    // current value table
    size_t values_count, values_exp;
    ValueDesc* values;
} Ctx;

#if 1
#define ASM if (ctx->emit.emit_asm)
#else
#define ASM if (0)
#endif

#define NAME(n) (get_meta(ctx, n)->t)
static NodeMeta* get_meta(Ctx* restrict ctx, TB_Node* n) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->meta_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->meta_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->meta[i].key == NULL) {
            return NULL;
        } else if (ctx->meta[i].key == n) {
            return &ctx->meta[i];
        }
    }
}

static void put_meta(Ctx* restrict ctx, TB_Node* n, int ordinal) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->meta_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->meta_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->meta[i].key == NULL) {
            assert(ctx->meta_count + 1 < (1u << ctx->meta_exp));

            // new slot
            ctx->meta_count++;
            ctx->meta[i] = (NodeMeta){ .key = n, .t = ordinal };
            return;
        } else if (ctx->meta[i].key == n) {
            assert(0 && "Huh?");
        }
    }
}

static bool try_tile(Ctx* restrict ctx, TB_Node* n) {
    dyn_array_for(i, ctx->in_bound) if (ctx->in_bound[i] == n) {
        dyn_array_remove(ctx->in_bound, i);
        return true;
    }

    return false;
}

#define DEF(n, rg) put_def(ctx, n, rg)
static int put_def(Ctx* restrict ctx, TB_Node* n, int reg_class) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .reg_class = reg_class, .reg = -1, .hint = -1 });
    return i;
}

#define DEF_HINTED(n, rg, hint) put_def_hinted(ctx, n, rg, hint)
static int put_def_hinted(Ctx* restrict ctx, TB_Node* n, int reg_class, int hint) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .end = INT_MIN, .reg_class = reg_class, .reg = -1, .hint = hint });
    return i;
}

#define DEF_FORCED(n, rg, reg, live_until) put_def_forced(ctx, n, rg, reg, live_until)
static int put_def_forced(Ctx* restrict ctx, TB_Node* n, int reg_class, int reg, int live_until) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .end = INT_MIN, .reg_class = reg_class, .reg = reg, .live_until = live_until });
    return i;
}

#define GET_VAL(n) (*get_val(ctx, n))
static int* get_val(Ctx* restrict ctx, TB_Node* n) {
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
            ctx->values[i].val = -1;
            return &ctx->values[i].val;
        } else if (ctx->values[i].key == n) {
            return &ctx->values[i].val;
        }
    }
}

static bool fits_into_int8(uint64_t x) {
    int8_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static bool fits_into_int32(uint64_t x) {
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static int classify_reg_class(TB_DataType dt);
static void abi_prepare(Ctx* restrict ctx, TB_Function* f);
static int isel(Ctx* restrict ctx, TB_Node* n);
static void emit_code(Ctx* restrict ctx);
static void patch_local_labels(Ctx* restrict ctx);
static void resolve_stack_usage(Ctx* restrict ctx, size_t caller_usage);
static void copy_value(Ctx* restrict ctx, TB_Node* phi, int dst, TB_Node* src, TB_DataType dt);
static void spill(Ctx* restrict ctx, Inst* basepoint, Reload* r);
static void reload(Ctx* restrict ctx, Inst* basepoint, Reload* r);

#define ISEL(n) USE(isel(ctx, n))

// references an allocated
#define USE(x) (-((x) + 2))
#define USE_VAL(n) (-(GET_VAL(n) + 2))

static void add_active(Ctx* restrict ctx, DefIndex di) {
    int end = ctx->defs[di].end;

    // insert by increasing end point
    // TODO(NeGate): do binary insert since the array is sorted
    size_t i = 0;
    for (; i < ctx->active_count; i++) {
        if (ctx->defs[ctx->active[i]].end >= end) break;
    }

    // we know where to insert
    FOREACH_REVERSE_N(j, i, ctx->active_count) {
        ctx->active[j+1] = ctx->active[j];
    }

    ctx->active[i] = di;
    ctx->active_count += 1;
}

static void remove_active(Ctx* restrict ctx, size_t i) {
    if (i + 1 != ctx->active_count) {
        memmove(&ctx->active[i], &ctx->active[i + 1], (ctx->active_count - i) * sizeof(Def*));
    }
    ctx->active_count -= 1;
}

static void insert_sorted_def(Ctx* restrict ctx, DefIndex* sorted, size_t count, int start, DefIndex di) {
    size_t i = 0;
    for (; i < count; i++) {
        if (ctx->defs[sorted[i]].start >= start) break;
    }

    // we know where to insert
    memmove(&sorted[i + 1], &sorted[i], (count - i) * sizeof(DefIndex));
    sorted[i] = di;
}

static size_t estimate_hash_map_size(size_t s) {
    // allocate values map and active, for linear scan
    size_t ht_cap = tb_next_pow2((s * 8) / 5);
    size_t ht_exp = 32 - tb_clz(ht_cap - 1);

    assert(ht_cap == (1u << ht_exp));
    return ht_exp;
}

static Inst inst_label(int l) {
    return (Inst){
        .type = INST_LABEL,
        .layout = X86_OP_NONE,
        .imm = { l }
    };
}

static Inst inst_line(TB_FileID file, int line) {
    return (Inst){
        .type = INST_LINE,
        .layout = X86_OP_NONE,
        .imm = { file, line }
    };
}

static void phi_edge(TB_Function* f, Ctx* restrict ctx, TB_Label src, TB_Label dst) {
    TB_FOR_NODE(n, f, dst) {
        if (n->type == TB_NULL) continue;
        if (n->type != TB_PHI) break;

        // allocate virtual register
        int* dst_vreg = &GET_VAL(n);
        if (*dst_vreg < 0) {
            *dst_vreg = DEF(n, classify_reg_class(n->dt));
        }

        // handle phis
        TB_NodePhi* phi = TB_NODE_GET_EXTRA(n);
        FOREACH_N(i, 0, n->input_count) if (phi->labels[i] == src) {
            copy_value(ctx, n, USE(*dst_vreg), n->inputs[i], n->dt);
            break;
        }
    }
}

#define SUBMIT(i) append_inst(ctx, i)
static void append_inst(Ctx* restrict ctx, Inst i) {
    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);
    *new_inst = i;

    if (ctx->last == NULL) {
        ctx->first = ctx->last = new_inst;
    } else {
        ctx->last->next = new_inst;
        ctx->last = new_inst;
    }
}

////////////////////////////////
// Linear scan allocation
////////////////////////////////
static int compare_defs(void* ctx, const void* a, const void* b) {
    Def* defs = ctx;
    int as = defs[*(DefIndex*) a].start;
    int bs = defs[*(DefIndex*) b].start;

    return (as > bs) - (as < bs);
}

static void add_range(Ctx* restrict ctx, Def* restrict d, int start, int end) {
    assert(start <= end);

    if (start < d->start) d->start = start;
    if (end < d->start) d->start = end;

    // max
    if (start > d->end) d->end = start;
    if (end > d->end) d->end = end;
}

static void reverse_bb_walk(Ctx* restrict ctx, TB_Function* f, MachineBB* bb, Inst* inst) {
    Inst* next = inst->next;
    if (next && next->type != INST_LABEL) {
        reverse_bb_walk(ctx, f, bb, next);
    }

    // mark def
    if (inst->regs[0] >= 0) {
        Def* d = &ctx->defs[inst->regs[0]];

        if (d->start >= 0) {
            d->start = inst->time;
            if (d->end == INT_MIN) d->end = inst->time;
        }
    }

    // mark users
    FOREACH_N(j, 1, 4) if (inst->regs[j] < -1) {
        Def* d = &ctx->defs[-inst->regs[j] - 2];

        add_range(ctx, d, bb->start, inst->time + (j == 1 ? 0 : 1));
    }
}

// generate live intervals for virtual registers
static DefIndex* liveness(Ctx* restrict ctx, TB_Function* f, TB_PostorderWalk* walk) {
    size_t def_count = dyn_array_length(ctx->defs);

    // find BB boundaries in sequences
    MachineBB* seq_bb = ARENA_ARR_ALLOC(&tb__arena, f->bb_count, MachineBB);

    FOREACH_N(bb, 0, f->bb_count) {
        seq_bb[bb] = (MachineBB){
            .gen = set_create(def_count), .kill = set_create(def_count),
            .live_in = set_create(def_count), .live_out = set_create(def_count)
        };
    }

    // generate local live sets
    if (ctx->first) {
        Set copy_init = set_create(def_count);

        Inst* restrict inst = ctx->first;

        // initial label
        assert(inst->type == INST_LABEL);
        seq_bb[0].first = inst;
        seq_bb[0].start = 2;
        inst = inst->next;

        int bb = 0, timeline = 2;
        for (; inst; inst = inst->next) {
            if (inst->type == INST_LABEL) {
                seq_bb[bb].end = timeline;
                timeline += 2; // reserved two extra spaces at the end of the BB

                bb = inst->imm[0];
                seq_bb[bb].first = inst->next;
                seq_bb[bb].start = timeline;
            }

            Set* gen = &seq_bb[bb].gen;
            Set* kill = &seq_bb[bb].kill;

            inst->time = timeline;
            timeline += 2;

            // convert initial move into copy
            if (inst->type == X86_INST_MOVE) {
                assert(inst->regs[1] < -1);
                int di = -inst->regs[1] - 2;

                if (!set_get(&copy_init, di)) {
                    set_put(&copy_init, di);

                    inst->type = (int) X86_INST_COPY;
                    inst->regs[0] = USE(inst->regs[1]);
                    inst->regs[1] = inst->regs[2];
                    inst->regs[2] = 0;
                }
            }

            FOREACH_N(j, 1, 4) if (inst->regs[j] < -1) {
                int di = -inst->regs[j] - 2;
                if (!set_get(kill, di)) {
                    set_put(gen, di);
                }
            }

            if (inst->regs[0] >= 0) {
                set_put(kill, inst->regs[0]);
            }
        }

        seq_bb[bb].end = timeline;
        set_free(&copy_init);
    }

    // generate global live sets
    bool changes;
    do {
        changes = false;

        FOREACH_REVERSE_N(bb, 0, f->bb_count) {
            set_clear(&seq_bb[bb].live_out);

            // walk all successors
            TB_Node* end = f->bbs[bb].end;
            if (end && end->type == TB_BRANCH) {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(end);
                FOREACH_N(i, 0, br->count) {
                    // union with successor's lives
                    TB_Label succ = br->targets[i].value;
                    set_union(&seq_bb[bb].live_out, &seq_bb[succ].live_in);
                }

                set_union(&seq_bb[bb].live_out, &seq_bb[br->default_label].live_in);
            }

            Set* live_in = &seq_bb[bb].live_in;
            Set* live_out = &seq_bb[bb].live_out;
            Set* kill = &seq_bb[bb].kill;
            Set* gen = &seq_bb[bb].gen;

            // live_in = (live_out - live_kill) U live_gen
            FOREACH_N(i, 0, (def_count + 63) / 64) {
                uint64_t new = (live_out->data[i] & ~kill->data[i]) | gen->data[i];

                changes |= (live_in->data[i] != new);
                live_in->data[i] = new;
            }
        }
    } while (changes);

    // this is a reverse BB walk
    FOREACH_N(i, 0, walk->count) {
        TB_Label bb = walk->traversal[i];

        int bb_start = seq_bb[bb].start;
        int bb_end = seq_bb[bb].end + 2;

        // for anything that's live out, add the entire range
        Set* live_in = &seq_bb[bb].live_in;
        Set* live_out = &seq_bb[bb].live_out;
        FOREACH_N(i, 0, (def_count + 63) / 64) {
            uint64_t bits = live_in->data[i] & live_out->data[i];
            if (bits == 0) continue;

            FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
                size_t k = (i*64) + j;
                add_range(ctx, &ctx->defs[k], bb_start, bb_end);
            }
        }

        // for all instruction in BB (in reverse), add ranges
        if (seq_bb[bb].first) {
            reverse_bb_walk(ctx, f, &seq_bb[bb], seq_bb[bb].first);
        }
    }

    // sort by starting point
    DefIndex* sorted = ARENA_ARR_ALLOC(&tb__arena, def_count * 2, DefIndex);
    FOREACH_N(i, 0, def_count) {
        Def* d = &ctx->defs[i];
        if (d->reg >= 0 && d->live_until >= 0) {
            Def* until = &ctx->defs[d->live_until];
            add_range(ctx, d, until->start, until->start);
        }

        sorted[i] = i;
    }
    qsort_s(sorted, def_count, sizeof(DefIndex), compare_defs, ctx->defs);

    ctx->seq_bb = seq_bb;
    return sorted;
}

// returns true if used in the next n instructions
static bool check_if_used(Ctx* restrict ctx, Inst* inst, int def_i, int n) {
    // check if it's unused for the next instruction
    for (size_t i = 0; inst && i < n; inst = inst->next, i++) {
        FOREACH_N(j, 1, 4) if (inst->regs[j] == USE(def_i)) {
            return true;
        }
    }

    return false;
}

static size_t choose_best_split(Ctx* restrict ctx, TB_Function* f, Inst* inst, int time) {
    FOREACH_REVERSE_N(i, 0, ctx->active_count) {
        DefIndex di = ctx->active[i];

        if (ctx->defs[di].reg >= 0 && !check_if_used(ctx, inst, di, 3)) {
            return i;
        }
    }

    return 0;
}

static Inst* insert_reload_site(Ctx* restrict ctx, TB_Function* f, Inst* inst, size_t def_i) {
    // skip first sequence
    inst = inst->next;

    for (; inst; inst = inst->next) {
        if (inst->type == INST_LABEL) {
            return NULL;
        }

        FOREACH_N(j, 1, 4) if (inst->regs[j] == USE(def_i)) {
            return inst;
        }
    }

    return NULL;
}

static Inst* find_inst_at_time(Ctx* restrict ctx, TB_Function* f, int time) {
    Inst* prev = NULL;
    for (Inst* inst = ctx->first; inst; prev = inst, inst = inst->next) {
        if (inst->time >= time) {
            return prev;
        }
    }

    return NULL;
}

static ptrdiff_t spill_register(Ctx* restrict ctx, TB_Function* f, DefIndex* sorted, DefIndex di, int time, Inst* spill_inst, size_t split_i) {
    size_t split_def = ctx->active[split_i];

    int reg_class = ctx->defs[split_def].reg_class;
    int reg_num = ctx->defs[split_def].reg;

    // insert spill
    Reload r = { TB_TYPE_I64, split_def };
    spill(ctx, spill_inst, &r);

    // keep spilled and generate reloads on the first use in any BB
    DefIndex reload_def = split_def;
    int endpoint = ctx->defs[split_def].end;

    Inst *inst = spill_inst->next, *prev_inst = spill_inst;
    for (; inst; prev_inst = inst, inst = inst->next) {
        if (inst->type == INST_LABEL) reload_def = -1;
        if (inst->time > endpoint) break;

        // if it's used, refer to reload
        bool skip_next = false;
        FOREACH_REVERSE_N(j, 1, 4) if (inst->regs[j] == USE(split_def)) {
            if (reload_def < 0) {
                if (inst->type == X86_INST_MOVE && j == 1) {
                    skip_next = true;
                    r.old = split_def;
                    spill(ctx, inst, &r);
                    continue;
                } else {
                    // spin up new def
                    int t = prev_inst->time + 1;
                    dyn_array_put(ctx->defs, (Def){ .start = t, .end = t, .reg = -1, .hint = reg_num });
                    reload_def = dyn_array_length(ctx->defs) - 1;

                    // insert into sort
                    insert_sorted_def(ctx, sorted, reload_def, t, reload_def);

                    // generate reload before this instruction
                    assert(prev_inst);
                    r.old = reload_def;
                    reload(ctx, prev_inst, &r);
                }
            }

            inst->regs[j] = USE(reload_def);
            ctx->defs[reload_def].end = inst->time + (j == 1 ? 0 : 1);
        }

        if (inst->regs[0] == split_i && reload_def != split_def) {
            // spill and discard our reload spot (if applies)
            r.old = inst->regs[0];
            spill(ctx, inst, &r);
            reload_def = -1;
            skip_next = true;
        }

        // if we're in the clobber list, invalidate the reload_def
        if (inst->regs[0] >= 0 && ctx->defs[inst->regs[0]].clobbers) {
            Clobbers* clobbers = ctx->defs[inst->regs[0]].clobbers;

            FOREACH_N(i, 0, clobbers->count) {
                if (clobbers->_[i].class == reg_class && clobbers->_[i].num == reg_num) {
                    reload_def = -1;
                    break;
                }
            }
        }

        if (skip_next) {
            // skip this instruction to avoid infinite spills
            prev_inst = inst, inst = inst->next;
        }
    }

    set_remove(&ctx->used_regs[reg_class], reg_num);
    remove_active(ctx, split_i);
    return reg_num;
}

static bool evict(Ctx* restrict ctx, TB_Function* f, DefIndex di, int reg_class, int reg, DefIndex* sorted, size_t node_count, int time) {
    if (!set_get(&ctx->used_regs[reg_class], reg)) {
        return false;
    }

    ASM printf("  \x1b[32m#   evict %s\x1b[0m\n", GPR_NAMES[reg]);

    // spill previous user until the end of the definition's lifetime
    size_t split_i = 0;
    for (; split_i < ctx->active_count; split_i++) {
        Def* k = &ctx->defs[ctx->active[split_i]];
        if (k->reg_class == reg_class && k->reg == reg) break;
    }
    assert(split_i < ctx->active_count);

    Inst* spill_inst = find_inst_at_time(ctx, f, time);
    spill_register(ctx, f, sorted, di, time, spill_inst, split_i);
    return true;
}

static void linear_scan(Ctx* restrict ctx, TB_Function* f, DefIndex* sorted, size_t node_count) {
    // actual perform linear scan
    for (size_t i = 0; i < dyn_array_length(ctx->defs); i++) {
        DefIndex di = sorted[i];
        Def* d = &ctx->defs[di];
        if (d->complete) continue;

        int time = d->start;
        ASM {
            printf("  \x1b[32m# D%zu t=[%d,%d) ", di, time, d->end);
            if (d->node) printf("r%d", NAME(d->node));
            printf("\x1b[0m\n");
        }

        // expire old intervals
        for (size_t j = 0; j < ctx->active_count;) {
            Def* k = &ctx->defs[ctx->active[j]];
            if (k->end > time) break;

            if (k->reg >= 0) {
                // move from active to handled
                ASM printf("  \x1b[32m#   free %s\x1b[0m\n", GPR_NAMES[k->reg]);

                set_remove(&ctx->used_regs[k->reg_class], k->reg);
                remove_active(ctx, j);
            } else {
                j++;
            }
        }

        // clobber anything before we continue
        if (d->clobbers) {
            FOREACH_N(i, 0, d->clobbers->count) {
                evict(ctx, f, di, d->clobbers->_[i].class, d->clobbers->_[i].num, sorted, node_count, time);
            }

            // we need to re-eval this element because something got inserted into this slot
            if (di != sorted[i]) {
                i -= 1;
            }
            d = &ctx->defs[di];
        }

        // find register for current
        ptrdiff_t reg_num = -1;
        if (d->reg >= 0) {
            if (evict(ctx, f, di, d->reg_class, d->reg, sorted, node_count, time)) {
                // we need to re-eval this element because something got inserted into this slot
                if (di != sorted[i]) {
                    i -= 1;
                }
                d = &ctx->defs[di];
            }

            reg_num = d->reg;
            ASM printf("  \x1b[32m#   forced assign %s\x1b[0m\n", GPR_NAMES[reg_num]);
        } else if (d->hint >= 0 && !set_get(&ctx->used_regs[d->reg_class], d->hint)) {
            reg_num = d->hint;
            ASM printf("  \x1b[32m#   hinted assign %s\x1b[0m\n", GPR_NAMES[reg_num]);
        } else {
            reg_num = set_pop_any(&ctx->used_regs[d->reg_class]);

            if (reg_num < 0) {
                // choose who to spill
                Inst* spill_inst = find_inst_at_time(ctx, f, time);
                size_t split_i = choose_best_split(ctx, f, spill_inst, time);

                reg_num = spill_register(ctx, f, sorted, di, time, spill_inst, split_i);

                // we need to re-eval this element because something got inserted into this slot
                if (di != sorted[i]) {
                    i -= 1;
                }
                d = &ctx->defs[di];
            }

            ASM printf("  \x1b[32m#   assign %s\x1b[0m\n", GPR_NAMES[reg_num]);
        }

        set_put(&ctx->used_regs[d->reg_class], reg_num);
        add_active(ctx, di);
        d->complete = true;
        d->reg = reg_num;
    }
}

static void hint(Ctx* restrict ctx, DefIndex di, int reg) {
    if (ctx->defs[di].hint < 0) {
        ctx->defs[di].hint = reg;
    }
}

static void fence(Ctx* restrict ctx) {
    // insert compiler fence to handle any leftover in_bounds
    ctx->in_fence = true;
    while (dyn_array_length(ctx->in_bound)) {
        TB_Node* n = dyn_array_pop(ctx->in_bound);
        isel(ctx, n);
    }
    dyn_array_clear(ctx->in_bound);
    ctx->in_fence = false;
}

// Codegen through here is done in phases
static TB_FunctionOutput compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    tb_function_print(f, tb_default_print_callback, stdout, false);

    Ctx* restrict ctx = arena_alloc(&tb__arena, sizeof(Ctx), _Alignof(Ctx));
    *ctx = (Ctx){
        .module = f->super.module,
        .f = f,
        .target_abi = f->super.module->target_abi,
        .emit = {
            .f = f,
            .data = out,
            .capacity = out_capacity,
        }
    };

    ctx->used_regs[0] = set_create(16);
    ctx->used_regs[1] = set_create(16);

    set_put(&ctx->used_regs[0], RBP), set_put(&ctx->used_regs[0], RSP);
    // FOREACH_N(i, 8, 16) set_put(&ctx->used_regs[0], i);

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
    ctx->meta_exp = estimate_hash_map_size(f->node_count);
    size_t meta_cap = (1u << ctx->meta_exp);

    ctx->meta = arena_alloc(&tb__arena, meta_cap * sizeof(NodeMeta), _Alignof(NodeMeta));
    memset(ctx->meta, 0, meta_cap * sizeof(NodeMeta));

    int counter = 0, label_patch_count = 0, return_count = 0, caller_usage = 0, line_count = 0;
    FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        TB_FOR_NODE(n, f, bb) if (1) {
            put_meta(ctx, n, counter++);

            line_count += (n->type == TB_LINE_INFO);
        }
    }

    mtx_lock(&f->super.module->lock);
    f->line_count = 0;
    f->lines = ARENA_ARR_ALLOC(&f->super.module->arena, line_count, TB_Line);
    mtx_unlock(&f->super.module->lock);

    // allocate values map and active, for linear scan
    ctx->values_exp = estimate_hash_map_size(counter);
    size_t values_cap = (1u << ctx->values_exp);

    ctx->values = arena_alloc(&tb__arena, values_cap * sizeof(ValueDesc), _Alignof(ValueDesc));
    ctx->active = arena_alloc(&tb__arena, counter * sizeof(TB_Node*), _Alignof(TB_Node*));
    memset(ctx->values, 0, values_cap * sizeof(ValueDesc));

    CUIK_TIMED_BLOCK("liveness analysis") {
        FOREACH_REVERSE_N(i, 0, walk.count) {
            TB_Label bb = walk.traversal[i];

            TB_FOR_NODE(n, f, bb) if (n->type != TB_NULL) {
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
                    get_meta(ctx, *in)->user_count++;
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
    SUBMIT(inst_label(0));
    abi_prepare(ctx, f);

    ctx->emit.emit_asm = true;
    CUIK_TIMED_BLOCK("isel") FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];

        // mark fallthrough
        ctx->fallthrough = (i > 0 ? walk.traversal[i - 1] : -1);
        if (bb) {
            SUBMIT(inst_label(bb));
        }

        TB_FOR_NODE(n, f, bb) if (n->type != TB_NULL) {
            if (n->type == TB_LINE_INFO) {
                TB_NodeLine* l = TB_NODE_GET_EXTRA(n);
                SUBMIT(inst_line(l->file, l->line));
            }

            // build up tile
            NodeMeta* m = get_meta(ctx, n);
            if (m->user_count <= 1 && tb_is_expr_like(n) && n->type != TB_LOCAL) {
                if (m->user_count == 1) dyn_array_put(ctx->in_bound, n);
                continue;
            }

            // Handle branch edges
            if (n->type == TB_BRANCH) {
                // copy out from active phi-edges
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                FOREACH_REVERSE_N(i, 0, br->count) {
                    phi_edge(f, ctx, bb, br->targets[i].value);
                }
                phi_edge(f, ctx, bb, br->default_label);
            }

            if (n->type != TB_LOCAL || nl_map_get(ctx->stack_slots, n) < 0) {
                isel(ctx, n);
            }

            if (n->type != TB_BRANCH) {
                fence(ctx);
            }
        }
    }

    DefIndex* sorted = NULL;
    CUIK_TIMED_BLOCK("build intervals") {
        sorted = liveness(ctx, f, &walk);
    }

    CUIK_TIMED_BLOCK("linear scan") {
        linear_scan(ctx, f, sorted, counter);
    }

    CUIK_TIMED_BLOCK("emit sequences") {
        // Arch-specific: convert instruction buffer into actual instructions
        emit_code(ctx);
    }

    ASM printf(".ret:\n");

    resolve_stack_usage(ctx, caller_usage);

    //  Label patching: we make sure any local labels
    patch_local_labels(ctx);

    if (f->line_count > 0) {
        f->lines[0].pos = 0;
    }

    // we're done, clean up
    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->emit.data,
        .code_size = ctx->emit.count,
        .stack_usage = ctx->stack_usage,
        // .prologue_epilogue_metadata = ctx->regs_to_save[0],
        // .stack_slots = ctx->stack_slots
    };

    arena_clear(&tb__arena);
    nl_map_free(ctx->stack_slots);
    // __debugbreak();
    return func_out;
}
