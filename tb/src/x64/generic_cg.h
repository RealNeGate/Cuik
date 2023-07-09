#include "../tb_internal.h"
#include "../codegen/emitter.h"
#include <inttypes.h>
#include <log.h>

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

enum {
    INST_LABEL = 1024,
    INST_LINE  = 1023,
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

typedef int ValueRef;

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
    int16_t hint;
    int16_t reg_class, reg;

    // when we preallocate a definition we
    // specify here which definition must
    // be completed for it to be free again
    DefIndex live_until;

    // once the def is live, these registers are clobbered
    Clobbers* clobbers;
} Def;

typedef struct {
    TB_Node* n;

    // if is usually -1 unless there's weird parallel copies
    int val, tmp;
} PhiVal;

typedef struct Effect Effect;
struct Effect {
    Effect* next;
    TB_Node* node;
    int phi_count;
};

typedef NL_Map(TB_Node*, MachineBB) MachineBBs;
typedef DynArray(DefIndex) RegAllocWorklist;

typedef struct {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_ABI target_abi;

    bool in_fence;
    int caller_usage;
    TB_Node* fallthrough;
    TB_PostorderWalk order;

    // Scheduling
    NL_HashSet visited;
    NL_Map(TB_Node*, Effect*) effects;

    // temporary but still precious
    DynArray(PhiVal) phi_vals;

    // machine output sequences
    Inst *first, *last;
    DynArray(Def) defs;
    DynArray(Reload) reloads;

    DynArray(DefIndex) clobbers;

    MachineBBs machine_bbs;

    // Line info
    TB_FileID last_file;
    int last_line;

    // Stack
    uint32_t stack_usage;
    NL_Map(TB_Node*, int) stack_slots;
    DynArray(TB_StackSlot) debug_stack_slots;

    // Reg alloc
    DefIndex* active;
    size_t active_count;

    uint64_t regs_to_save;
    Set used_regs[CG_REGISTER_CLASSES];

    // current value table
    NL_Map(TB_Node*, ValueRef) values;

    TB_SafepointKey* safepoints;
} Ctx;

enum {
    //   dst = COPY src
    INST_COPY = 1022,
    INST_MOVE = 1021,
    INST_USE  = 1020,
};

#if 1
#define ASM if (ctx->emit.emit_asm)
#else
#define ASM if (0)
#endif

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

#define GET_VAL(n) nl_map_get_checked(ctx->values, n)

static bool fits_into_int8(uint64_t x) {
    int8_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static bool fits_into_int32(uint64_t x) {
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static bool wont_spill_around(int type);
static Inst inst_move(TB_DataType dt, int lhs, int rhs);
static int classify_reg_class(TB_DataType dt);
static int isel(Ctx* restrict ctx, TB_Node* n);
static void finna_use_reg(Ctx* restrict ctx, int reg_class, int reg_num);
static void emit_code(Ctx* restrict ctx);
static void patch_local_labels(Ctx* restrict ctx);
static void resolve_stack_usage(Ctx* restrict ctx, size_t caller_usage);
static void copy_value(Ctx* restrict ctx, TB_Node* phi, int dst, TB_Node* src, TB_DataType dt);
static void spill(Ctx* restrict ctx, Inst* basepoint, Reload* r);
static void reload(Ctx* restrict ctx, Inst* basepoint, Reload* r, size_t op_index);

#define ISEL(n) USE(isel(ctx, n))

// references an allocated
#define USE(x) (-((x) + 2))
#define USE_VAL(n) (-(GET_VAL(n) + 2))

static void add_debug_local(Ctx* restrict ctx, TB_Node* n, int pos) {
    // could be costly if you had more than like 2-3 attributes per stack slot... which you
    // wouldn't do right?
    for (TB_Attrib* a = n->first_attrib; a != NULL; a = a->next) {
        if (a->type == TB_ATTRIB_VARIABLE) {
            TB_StackSlot s = {
                .position = pos,
                .storage_type = a->var.storage,
                .name = a->var.name,
            };
            dyn_array_put(ctx->debug_stack_slots, s);
            break;
        }
    }
}

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

static size_t estimate_hash_map_size(size_t s) {
    // allocate values map and active, for linear scan
    size_t ht_cap = tb_next_pow2((s * 8) / 5);
    size_t ht_exp = 64 - tb_clz64(ht_cap - 1);

    assert(ht_cap == (1u << ht_exp));
    return ht_exp;
}

static Inst inst_label(TB_Node* n) {
    return (Inst){
        .type = INST_LABEL,
        .layout = X86_OP_NONE,
        .regs = { -1 },
        .imm = { (uintptr_t) n }
    };
}

static Inst inst_line(TB_FileID file, int line) {
    return (Inst){
        .type = INST_LINE,
        .layout = X86_OP_NONE,
        .imm = { file, line }
    };
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
// Liveness analysis
////////////////////////////////
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

static size_t partition(Def* defs, ptrdiff_t lo, ptrdiff_t hi, DefIndex* arr) {
    int pivot = defs[arr[(hi - lo) / 2 + lo]].start; // middle

    ptrdiff_t i = lo - 1, j = hi + 1;
    for (;;) {
        // Move the left index to the right at least once and while the element at
        // the left index is less than the pivot
        do { i += 1; } while (defs[arr[i]].start > pivot);

        // Move the right index to the left at least once and while the element at
        // the right index is greater than the pivot
        do { j -= 1; } while (defs[arr[j]].start < pivot);

        // If the indices crossed, return
        if (i >= j) return j;

        // Swap the elements at the left and right indices
        SWAP(DefIndex, arr[i], arr[j]);
    }
}

static void cuiksort_defs(Def* defs, ptrdiff_t lo, ptrdiff_t hi, DefIndex* arr) {
    if (lo >= 0 && hi >= 0 && lo < hi) {
        // get pivot
        size_t p = partition(defs, lo, hi, arr);

        // sort both sides
        cuiksort_defs(defs, lo, p, arr);
        cuiksort_defs(defs, p + 1, hi, arr);
    }
}

static int compare_defs(const void* a, const void* b) {
    const DefIndex* aa = a;
    const DefIndex* bb = b;
    return (*bb > *aa) - (*bb < *aa);
}

// generate live intervals for virtual registers
static RegAllocWorklist liveness(Ctx* restrict ctx, TB_Function* f) {
    size_t def_count = dyn_array_length(ctx->defs);
    Arena* arena = &tb__arena;

    // find BB boundaries in sequences
    MachineBBs seq_bb = NULL;
    nl_map_create(seq_bb, ctx->order.count);

    FOREACH_N(i, 0, ctx->order.count) {
        MachineBB bb = {
            .gen = set_create_in_arena(arena, def_count), .kill = set_create_in_arena(arena, def_count),
            .live_in = set_create_in_arena(arena, def_count), .live_out = set_create_in_arena(arena, def_count)
        };

        nl_map_put(seq_bb, ctx->order.traversal[i], bb);
    }

    // generate local live sets
    if (ctx->first) {
        Set copy_init = set_create_in_arena(arena, def_count);

        Inst* restrict inst = ctx->first;
        assert(inst->type == INST_LABEL);

        // initial label
        MachineBB* mbb = &nl_map_get_checked(seq_bb, f->start_node);
        mbb->first = inst;
        mbb->start = 2;
        inst = inst->next;

        TB_Node* bb = f->start_node;
        int timeline = 2;
        for (; inst; inst = inst->next) {
            if (inst->type == INST_LABEL) {
                nl_map_get_checked(seq_bb, bb).end = timeline;
                timeline += 2; // reserved two extra spaces at the end of the BB

                bb = (TB_Node*) inst->imm[0];
                mbb = &nl_map_get_checked(seq_bb, bb);
                mbb->first = inst->next;
                mbb->start = timeline;
            }

            Set* restrict gen = &mbb->gen;
            Set* restrict kill = &mbb->kill;

            inst->time = timeline;
            timeline += 2;

            // convert initial move into copy
            if (inst->type == INST_MOVE) {
                assert(inst->regs[1] < -1);
                int di = -inst->regs[1] - 2;

                // propagate hints
                int si = USE(inst->regs[2]);
                if (ctx->defs[si].hint >= 0) {
                    ctx->defs[si].hint = ctx->defs[di].hint;
                }

                if (!set_get(&copy_init, di)) {
                    set_put(&copy_init, di);

                    inst->type = INST_COPY;
                    inst->regs[0] = di;
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

                // mark clobbers for later, we might wanna improve
                if (ctx->defs[inst->regs[0]].clobbers) {
                    dyn_array_put(ctx->clobbers, inst->regs[0]);
                }
            }
        }

        mbb->end = timeline;
    }

    // generate global live sets
    bool changes;
    do {
        changes = false;

        FOREACH_REVERSE_N(i, 0, ctx->order.count) {
            TB_Node* bb = ctx->order.traversal[i];
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
            MachineBB* mbb = &nl_map_get_checked(seq_bb, bb);

            set_clear(&mbb->live_out);

            // walk all successors
            FOREACH_N(i, 0, r->succ_count) {
                // union with successor's lives
                MachineBB* succ = &nl_map_get_checked(seq_bb, r->succ[i]);
                set_union(&mbb->live_out, &succ->live_in);
            }

            Set* restrict live_in = &mbb->live_in;
            Set* restrict live_out = &mbb->live_out;
            Set* restrict kill = &mbb->kill;
            Set* restrict gen = &mbb->gen;

            // live_in = (live_out - live_kill) U live_gen
            FOREACH_N(i, 0, (def_count + 63) / 64) {
                uint64_t new_val = (live_out->data[i] & ~kill->data[i]) | gen->data[i];

                changes |= (live_in->data[i] != new_val);
                live_in->data[i] = new_val;
            }
        }
    } while (changes);

    FOREACH_N(i, 0, ctx->order.count) {
        TB_Node* bb = ctx->order.traversal[i];
        MachineBB* mbb = &nl_map_get_checked(seq_bb, bb);

        int bb_start = mbb->start;
        int bb_end = mbb->end + 2;

        // for anything that's live out, add the entire range
        Set* live_in = &mbb->live_in;
        Set* live_out = &mbb->live_out;
        FOREACH_N(i, 0, (def_count + 63) / 64) {
            uint64_t bits = live_in->data[i] & live_out->data[i];
            if (bits == 0) continue;

            FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
                size_t k = (i*64) + j;
                add_range(ctx, &ctx->defs[k], bb_start, bb_end);
            }
        }

        // for all instruction in BB (in reverse), add ranges
        if (mbb->first) {
            reverse_bb_walk(ctx, f, mbb, mbb->first);
        }
    }

    RegAllocWorklist sorted = dyn_array_create(DefIndex, (def_count * 4) / 3);
    FOREACH_N(i, 0, def_count) {
        Def* d = &ctx->defs[i];
        if (d->reg >= 0 && d->live_until >= 0) {
            Def* until = &ctx->defs[d->live_until];
            add_range(ctx, d, until->start, until->start);
        }

        dyn_array_put(sorted, i);
    }

    // sort by starting point (lowest at the end)
    cuiksort_defs(ctx->defs, 0, def_count - 1, sorted);

    // sort by starting point (lowest at the end)
    qsort(ctx->clobbers, dyn_array_length(ctx->clobbers), sizeof(DefIndex), compare_defs);

    ctx->machine_bbs = seq_bb;
    return sorted;
}

#include "reg_alloc.h"
#include "fancy_reg_alloc.h"

static void hint(Ctx* restrict ctx, DefIndex di, int reg) {
    if (ctx->defs[di].hint < 0) {
        ctx->defs[di].hint = reg;
    }
}

static void phi_edge(Ctx* restrict ctx, TB_Node* dst, int index) {
    TB_NodeRegion* region = TB_NODE_GET_EXTRA(dst);
    DynArray(PhiVal) phi_vals = ctx->phi_vals;
    dyn_array_clear(phi_vals);

    Effect* parent = nl_map_get_checked(ctx->effects, dst);

    size_t phi_count = 0;
    for (Effect* e = parent->next; phi_count < parent->phi_count; e = e->next) {
        TB_Node* n = e->node;

        // only two things we should expect here
        if (n->type == TB_LOAD) continue;
        if (n->type != TB_PHI) break;

        // allocate virtual register
        ptrdiff_t search = nl_map_get(ctx->values, n);
        PhiVal p = { n, -1, -1 };
        if (search < 0) {
            p.val = DEF(n, classify_reg_class(n->dt));
            nl_map_put(ctx->values, n, p.val);
        } else {
            p.val = ctx->values[search].v;
        }

        dyn_array_put(phi_vals, p);
        phi_count++;
    }
    assert(phi_count == parent->phi_count);

    // do copies which on parallel phis (swaps usually but we don't do those yet)
    bool has_tmps = false;
    dyn_array_for(i, phi_vals) {
        TB_Node* n = phi_vals[i].n;
        assert(n->type == TB_PHI);

        if (n->inputs[1 + index]->type == TB_PHI && n->inputs[1 + index]->inputs[0] == dst) {
            int tmp = DEF(n, classify_reg_class(n->dt));
            copy_value(ctx, n, USE(tmp), n->inputs[1 + index], n->dt);
            phi_vals[i].tmp = tmp;
            has_tmps = true;
        }
    }

    // do normal copies
    dyn_array_for(i, phi_vals) {
        TB_Node* n = phi_vals[i].n;

        if (phi_vals[i].tmp < 0) {
            int dst = USE(phi_vals[i].val);
            copy_value(ctx, n, dst, n->inputs[1 + index], n->dt);
        }
    }

    // do temp copies
    if (has_tmps) {
        dyn_array_for(i, phi_vals) {
            TB_Node* n = phi_vals[i].n;

            if (phi_vals[i].tmp >= 0) {
                int dst = USE(phi_vals[i].val);
                int src = USE(phi_vals[i].tmp);
                SUBMIT(inst_move(n->dt, dst, src));
            }
        }
    }

    ctx->phi_vals = phi_vals;
}

static Effect* insert_effect(Ctx* restrict ctx, Effect* prev, TB_Node* n) {
    Effect* new_e = ARENA_ALLOC(&tb__arena, Effect);
    new_e->next = prev->next;
    new_e->node = n;
    new_e->phi_count = 0;
    prev->next = new_e;
    return new_e;
}

#if 0
// Handle branch edges
if (n->type == TB_BRANCH) {
    // copy out from active phi-edges
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(parent);
    FOREACH_N(i, 0, r->succ_count) {
        TB_Node* dst = r->succ[i];

        // find predecessor index and do that edge
        FOREACH_N(j, 0, dst->input_count) {
            TB_Node* pred = dst->inputs[j];
            while (pred->type != TB_REGION && pred->type != TB_START) pred = pred->inputs[0];

            if (pred == parent) {
                phi_edge(ctx, dst, j);
                break;
            }
        }
    }
}

if (n->type != TB_PROJ && (n->type != TB_LOCAL || nl_map_get(ctx->stack_slots, n) < 0)) {
    isel(ctx, n);
}
#endif

static void scan_for_effects(Ctx* restrict ctx, TB_Node* n) {
    if (!nl_hashset_put(&ctx->visited, n)) {
        return;
    }

    if (n->type == TB_LOAD) {
        insert_effect(ctx, nl_map_get_checked(ctx->effects, n->inputs[0]), n);
    } else if (n->type == TB_PHI) {
        Effect* parent = nl_map_get_checked(ctx->effects, n->inputs[0]);
        insert_effect(ctx, parent, n);
        parent->phi_count++;
    } else if (n->type == TB_REGION || n->type == TB_START) {
        return;
    }

    FOREACH_N(i, 0, n->input_count) {
        scan_for_effects(ctx, n->inputs[i]);
    }
}

// Codegen through here is done in phases
static void compile_function(TB_Function* restrict f, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity) {
    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .target_abi = f->super.module->target_abi,
        .safepoints = f->safepoint_count ? tb_platform_heap_alloc(f->safepoint_count * sizeof(TB_SafepointKey)) : NULL,
        .emit = {
            .f = f,
            .output = func_out,
            .data = out,
            .capacity = out_capacity,
        }
    };

    ctx.emit.emit_asm = true;
    /* if (ctx.emit.emit_asm) {
        tb_function_print(f, tb_default_print_callback, stdout);
    }*/

    ctx.used_regs[0] = set_create_in_arena(&tb__arena, 16);
    ctx.used_regs[1] = set_create_in_arena(&tb__arena, 16);
    ctx.visited = nl_hashset_alloc(f->node_count);

    set_put(&ctx.used_regs[0], RBP), set_put(&ctx.used_regs[0], RSP);
    // FOREACH_N(i, 8, 16) set_put(&ctx.used_regs[0], i);

    // BB scheduling:
    //   we run through BBs in a reverse postorder walk, currently
    //   there's no reodering based on branch weights (since we don't
    //   do those but if we did that would go here.
    ctx.order = tb_function_get_postorder(f);
    assert(ctx.order.traversal[ctx.order.count - 1] == f->start_node && "Codegen must always schedule entry BB first");

    // Live intervals:
    //   We compute this for register allocation along
    //   with the "ordinals" which act as our timeline.
    nl_map_create(ctx.values, f->node_count);

    /*mtx_lock(&f->super.module->lock);
    f->line_count = 0;
    f->lines = ARENA_ARR_ALLOC(&f->super.module->arena, line_count, TB_Line);
    mtx_unlock(&f->super.module->lock);*/

    ctx.active = arena_alloc(&tb__arena, f->node_count * sizeof(DefIndex), _Alignof(DefIndex));

    // allocate more stuff now that we've run stats on the IR
    ctx.emit.return_label = 0;
    nl_map_create(ctx.emit.labels, f->control_node_count);
    nl_map_create(ctx.stack_slots, 8);
    dyn_array_create(ctx.debug_stack_slots, 8);

    // Scheduling:
    //   for now things are simple, we just need to walk all the nodes and
    //   make sure the correct nodes are marked to happen after their control
    //   dependencies
    FOREACH_REVERSE_N(i, 0, ctx.order.count) {
        TB_Node* bb = ctx.order.traversal[i];

        // append region as the first effect
        Effect* bb_effect = ARENA_ALLOC(&tb__arena, Effect);
        bb_effect->next = NULL;
        bb_effect->node = bb;
        bb_effect->phi_count = 0;
        nl_map_put(ctx.effects, bb, bb_effect);

        // do standard walk (this only fills things in the direct line of execution
        // the PHI and LOAD operations will need a separate pass)
        Effect* last = NULL;
        TB_Node* n = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end;
        while (n != NULL && n->type != TB_START && n->type != TB_REGION) {
            Effect* e = ARENA_ALLOC(&tb__arena, Effect);
            e->next = last;
            e->node = n;
            e->phi_count = 0;
            last = e;

            nl_map_put(ctx.effects, n, e);

            // previous control
            n = n->inputs[0];
        }

        // add onto region
        bb_effect->next = last;
    }

    FOREACH_REVERSE_N(i, 0, ctx.order.count) {
        // walk all the nodes to see if we have any PHI or LOAD
        // we don't have conclusive results until all BBs are walked
        // but that's fine.
        TB_Node* n = TB_NODE_GET_EXTRA_T(ctx.order.traversal[i], TB_NodeRegion)->end;
        while (n != NULL && n->type != TB_START && n->type != TB_REGION) {
            FOREACH_N(i, 1, n->input_count) {
                scan_for_effects(&ctx, n->inputs[i]);
            }
            n = n->inputs[0];
        }
    }

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    FOREACH_REVERSE_N(i, 0, ctx.order.count) {
        TB_Node* bb = ctx.order.traversal[i];
        nl_map_put(ctx.emit.labels, bb, 0);

        // mark fallthrough
        ctx.fallthrough = i > 0 ? ctx.order.traversal[i - 1] : NULL;
        if (bb) {
            append_inst(&ctx, inst_label(bb));
        }

        Effect* e = nl_map_get_checked(ctx.effects, bb)->next;
        while (e != NULL) {
            TB_Node* n = e->node;

            // set line info
            for (TB_Attrib* a = n->first_attrib; a; a = a->next) if (a->type == TB_ATTRIB_LOCATION) {
                // check if it's changed
                if (ctx.last_file != a->loc.file || ctx.last_line != a->loc.line) {
                    ctx.last_file = a->loc.file;
                    ctx.last_line = a->loc.line;

                    append_inst(&ctx, inst_line(ctx.last_file, ctx.last_line));
                }
            }

            // Handle branch edges
            if (n->type == TB_BRANCH) {
                // copy out from active phi-edges
                TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
                FOREACH_N(i, 0, r->succ_count) {
                    TB_Node* dst = r->succ[i];

                    // find predecessor index and do that edge
                    FOREACH_N(j, 0, dst->input_count) {
                        TB_Node* pred = dst->inputs[j];
                        while (pred->type != TB_REGION && pred->type != TB_START) pred = pred->inputs[0];

                        if (pred == bb) {
                            phi_edge(&ctx, dst, j);
                            break;
                        }
                    }
                }
            }

            // schedule effect
            if (n->type != TB_PROJ && (n->type != TB_LOCAL || nl_map_get(ctx.stack_slots, n) < 0)) {
                isel(&ctx, e->node);
            }

            e = e->next;
        }
    }

    // maybe it's completely empty
    if (ctx.defs != NULL) {
        RegAllocWorklist worklist = NULL;
        CUIK_TIMED_BLOCK("build intervals") {
            worklist = liveness(&ctx, f);
        }

        CUIK_TIMED_BLOCK("reg alloc") {
            if (1) {
                reg_alloc(&ctx, f, worklist);
            } else {
                fancy_reg_alloc(&ctx, f, worklist);
            }
        }

        // Arch-specific: convert instruction buffer into actual instructions
        CUIK_TIMED_BLOCK("emit sequences") {
            emit_code(&ctx);
        }
    }

    if (ctx.emit.emit_asm) {
        printf(".ret:\n");
    }

    resolve_stack_usage(&ctx, ctx.caller_usage);

    //  Label patching: we make sure any local labels
    patch_local_labels(&ctx);
    nl_map_free(ctx.emit.labels);
    nl_map_free(ctx.values);
    nl_map_free(ctx.machine_bbs);
    nl_hashset_free(ctx.visited);
    dyn_array_destroy(ctx.phi_vals);

    if (dyn_array_length(f->lines)) {
        f->lines[0].pos = 0;
    }

    // we're done, clean up
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->stack_usage = ctx.stack_usage;
    func_out->prologue_epilogue_metadata = ctx.regs_to_save;
    func_out->safepoints = ctx.safepoints;
    func_out->stack_slots = ctx.debug_stack_slots;

    tb_function_free_postorder(&ctx.order);
    arena_clear(&tb__arena);
    nl_map_free(ctx.stack_slots);
}
