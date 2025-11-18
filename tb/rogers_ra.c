// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>
#include <stdlib.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

#define FOREACH_SET(it, set) \
FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    uint64_t key; // key
    int last_use; // val
} InactiveCacheEntry;

// High reg pressure block data, the splitter
// will avoid keeping split values alive in this
// region when possible.
typedef struct {
    int start[MAX_REG_CLASSES];
    int end[MAX_REG_CLASSES];
} HRPRegion;

typedef struct {
    // VReg
    uint32_t target;

    // if this allocation was clobbered, this means
    // we only need to avoid the "failed" node for
    // the definition (since it wanted to use the
    // register we did):
    //
    // # insert %d to put %c into a reg which isn't clobbered.
    // %c = ...            # RAX
    // %d: GPR ~ RAX = %c
    // call foo            # kill: RAX
    // use(%d)
    RegMask* clobber;
} SplitDecision;

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;
    int* fixed;

    // dominance frontiers
    ArenaArray(int)* df; // [ctx->bb_count]
    DynArray(SplitDecision) splits;
    DynArray(int) potential_spills; // [gvn]

    int order_cap;
    int* order;
    TB_Node** gvn2node;

    // [class][reg]
    int* active[8];
    int stack_reg_count;

    Set been_spilled;
    Set future_active;
    Set live;

    int* dirty_bb;
    HRPRegion* hrp;

    // coalesce disjoint set
    int* uf;
    int* uf_size;
    int uf_len;

    // leader -> list of members
    NL_Table coalesce_set;

    // last use in a BB
    InactiveCacheEntry* inactive_cache;

    // where is the linear scan at
    int where_bb;
    int where_order;

    // how many did the RA introduce
    int num_spills;
    int max_regs_in_class;

    // interference mask
    int mask_cap;
    uint64_t* mask;
} Rogers;

static void tb__insert_splits(Ctx* ctx, Rogers* restrict ra);
static void insert_op_at_end(Ctx* ctx, Rogers* ra, TB_BasicBlock* bb, TB_Node* n);

static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node);
static void better_spill_range(Ctx* ctx, Rogers* restrict ra, TB_Node* to_spill, size_t old_node_count);
static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn);

static bool rogers_is_fixed(Ctx* ctx, Rogers* ra, int id) {
    if (ctx->vregs[id].mask == NULL) {
        return false;
    }

    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
}

static TB_Node** coalesce_set_array(Ctx* ctx, Rogers* ra, TB_Node** n_ptr, size_t* out_count) {
    int leader = uf_find(ra->uf, ra->uf_len, (*n_ptr)->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set) {
        *out_count = aarray_length(set);
        return set;
    } else {
        *out_count = 1;
        return n_ptr;
    }
}

static int rogers_insert_op(Ctx* ctx, int bb_id, TB_Node* n, int pos) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

    // skip phis and projections so that they stay nice and snug
    size_t cnt = aarray_length(bb->items);
    aarray_push(bb->items, 0);
    if (cnt > pos) {
        memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
    }
    bb->items[pos] = n;

    tb__insert(ctx, ctx->f, bb, n);
    return pos;
}

static double rogers_get_spill_cost(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    if (isnan(vreg->spill_cost)) {
        int leader = uf_find(ra->uf, ra->uf_len, vreg->n->gvn);
        ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));

        double c = 0.0;
        if (set) {
            aarray_for(i, set) {
                c += get_node_spill_cost(ctx, set[i]);
            }
        } else {
            c = get_node_spill_cost(ctx, vreg->n);
        }

        vreg->spill_cost = c + vreg->spill_bias;
    }

    // no area? this means it's used right after def
    if (vreg->area == 0) {
        // printf("spill_cost(%zu) = %f\n", vreg - ctx->vregs, INFINITY);
        return INFINITY;
    }

    // printf("spill_cost(%zu) = %f\n", vreg - ctx->vregs, vreg->spill_cost / vreg->area);
    return vreg->spill_cost - vreg->area*0.2;
}

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    double cost = rogers_get_spill_cost(ctx, ra, vreg);
    printf("# V%-4"PRIdPTR" cost=%.2f ", vreg - ctx->vregs, cost);
    tb__print_regmask(&OUT_STREAM_DEFAULT, vreg->mask);
    printf("\n");
}

static bool rogers_can_coalesce(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* xn, TB_Node* yn) {
    if (ra->uf[yn->gvn] != yn->gvn || ra->uf_size[yn->gvn] != 1) {
        return false;
    }

    int x = uf_find(ra->uf, ra->uf_len, xn->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
    if (set == NULL) {
        return !interfere(ctx, ra, xn, yn);
    } else {
        aarray_for(i, set) {
            if (interfere(ctx, ra, set[i], yn)) {
                return false;
            }
        }

        return true;
    }
}

static void rogers_resize_uf(Ctx* restrict ctx, Rogers* restrict ra, size_t new_len) {
    ra->uf = cuik_realloc(ra->uf, new_len * sizeof(int));
    ra->uf_size = cuik_realloc(ra->uf_size, new_len * sizeof(int));
    FOR_N(i, ra->uf_len, new_len) {
        ra->uf[i] = i;
        ra->uf_size[i] = 1;
    }
    ra->uf_len = new_len;
}

static void rogers_coalesce(Ctx* restrict ctx, Rogers* restrict ra, int x, int y, TB_Node* xn, TB_Node* yn) {
    if (x == y) {
        return;
    }

    int max = TB_MAX(x, y) + 1;
    if (max >= ra->uf_len) {
        rogers_resize_uf(ctx, ra, max);
    }

    // hard coalesce with direct input
    TB_ASSERT(x < ra->uf_len && y < ra->uf_len);
    ra->uf[y] = x;
    ra->uf_size[x] += ra->uf_size[y];

    TB_ASSERT(ra->uf[x] == x);
    ArenaArray(TB_Node*)* new_set = (ArenaArray(TB_Node*)*) nl_table_getp(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
    if (new_set == NULL) {
        ArenaArray(TB_Node*) set = aarray_create(&ctx->f->arena, TB_Node*, 4);
        nl_table_put(&ra->coalesce_set, (void*) (uintptr_t) (x + 1), set);

        // lazy? yes
        new_set = (ArenaArray(TB_Node*)*) nl_table_getp(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
        aarray_push(*new_set, xn);
    }

    ArenaArray(TB_Node*) old_set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (y + 1));
    if (old_set == NULL) {
        aarray_push(*new_set, yn);
    } else {
        aarray_for(i, old_set) {
            aarray_push(*new_set, old_set[i]);
        }
    }
}

static void rogers_update_mask(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* n, int vreg_id) {
    int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));

    RegMask* mask = NULL;
    if (set && aarray_length(set)) {
        // split the copy
        aarray_for(i, set) {
            int y = set[i]->gvn;
            TB_Node* yy = ra->gvn2node[y];

            ctx->vreg_map[y] = vreg_id;
            mask = tb__reg_mask_meet(ctx, mask, ctx->constraint(ctx, yy, NULL));
            FOR_USERS(u, yy) {
                if (USERI(u) > 0 && USERI(u) < USERN(u)->input_count) {
                    RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                    mask = tb__reg_mask_meet(ctx, mask, in_mask);
                }
            }
        }
    } else {
        ctx->vreg_map[n->gvn] = vreg_id;
        mask = ctx->constraint(ctx, n, NULL);
        FOR_USERS(u, n) {
            if (USERI(u) > 0 && USERI(u) < USERN(u)->input_count) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                mask = tb__reg_mask_meet(ctx, mask, in_mask);
            }
        }
    }
    TB_ASSERT(mask != &TB_REG_EMPTY);

    VReg* vreg = &ctx->vregs[vreg_id];
    vreg->mask = mask;
    // vreg->reg_width = tb__reg_width_from_dt(mask->class, n->dt);
}

static TB_Node* rogers_pick_leader(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* n) {
    int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set == NULL || aarray_length(set) == 0) {
        return n;
    }

    double best_cost = FLT_MAX;
    TB_Node* best = NULL;

    // split the copy
    RegMask* mask = NULL;
    aarray_for(i, set) {
        int y = set[i]->gvn;
        TB_Node* yy = ra->gvn2node[y];
        // we don't want to split... splits
        if (yy->type == TB_MACH_COPY) {
            continue;
        }

        double cost = get_node_spill_cost(ctx, yy);
        if (best == NULL || cost < best_cost) {
            best = yy;
        }
        printf("%%%u %f\n", yy->gvn, cost);
    }

    return best ? best : n;
}

static void rogers_uncoalesce(Ctx* restrict ctx, Rogers* restrict ra, int x) {
    int leader = uf_find(ra->uf, ra->uf_len, x);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set == NULL) {
        return;
    }

    // if the leader is the node being removed from the set, pick a new leader
    int new_leader = -1;
    for (int i = 0; i < aarray_length(set); i++) {
        int y = set[i]->gvn;
        if (y == x) {
            ra->uf[y] = y;
            aarray_remove(set, i);
            i--;
        } else if (leader == x) {
            if (new_leader < 0) {
                new_leader = y;
            }

            ra->uf[y] = new_leader;
        }
    }

    if (new_leader >= 0) {
        nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
        nl_table_put(&ra->coalesce_set, (void*) (uintptr_t) (new_leader + 1), set);
    }
}

static TB_Node* rogers_hard_split(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* n, TB_Node* in, RegMask* rm, int vreg_id) {
    TB_Function* f = ctx->f;

    TB_Node* move;
    if (can_remat(ctx, in)) {
        size_t extra = extra_bytes(in);
        move = tb_alloc_node(f, in->type, in->dt, in->input_count, extra);
        memcpy(move->extra, in->extra, extra);
        FOR_N(j, 0, in->input_count) if (in->inputs[j]) {
            move->inputs[j] = in->inputs[j];
            add_user(f, move, in->inputs[j], j);
        }
    } else {
        move = tb_alloc_node(f, TB_MACH_COPY, in->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, move, in, 1);
        TB_NODE_SET_EXTRA(move, TB_NodeMachCopy, .def = rm, .use = ctx->normie_mask[rm->class]);
    }
    aarray_insert(ctx->vreg_map, move->gvn, vreg_id);
    return move;
}

static void rogers_dump_sched(Ctx* restrict ctx, int old_node_count) {
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf("BB%zu (freq=%f, %%%u):\n", i, bb->freq, bb->start->gvn);

        TB_Node* end = bb->end;
        aarray_for(i, bb->items) {
            // if the first node is a region or
            if (bb->start == bb->items[i]) {
                continue;
            }

            printf("  ");
            // tb_print_dumb_node(NULL, bb->items[i]);
            ctx->print_pretty(ctx, bb->items[i]);
            if (bb->items[i]->gvn >= old_node_count) {
                printf("  #  NEW!!!");
            }
            printf("\n");
        }

        if (!tb_node_is_terminator(bb->end)) {
            TB_Node* succ_n = cfg_next_control(bb->end);
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
            int b = succ_bb - ctx->cfg.blocks;
            if (ctx->cfg.blocks[b].fwd > 0) {
                while (b != ctx->cfg.blocks[b].fwd) {
                    b = ctx->cfg.blocks[b].fwd;
                }
            }

            printf("    jmp BB%d\n", b);
        }
    }
}

static void rogers_dump_split(Ctx* restrict ctx, Rogers* restrict ra, TB_BasicBlock* block, TB_Node* aa, TB_Node* bb) {
    int a[2], b[2];

    bool entry_block = block == &ctx->cfg.blocks[0];
    a[0] = set_get(&block->live_in, aa->gvn) ? 0 : ra->order[aa->gvn] - 1;
    b[0] = set_get(&block->live_in, bb->gvn) ? 0 : ra->order[bb->gvn] - 1;
    a[1] = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, block, aa, aa->gvn) - 1;
    b[1] = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, block, bb, bb->gvn) - 1;

    // if (IS_PROJ(aa)) { aa = aa->inputs[0]; }
    // if (IS_PROJ(bb)) { bb = bb->inputs[0]; }

    int start = TB_MIN(a[0], b[0]);
    int end   = TB_MAX(a[1], b[1]);
    if (end < aarray_length(block->items)) {
        end += 1;
    }

    printf("  A B:\n");
    FOR_N(i, start, end) {
        printf(
            "  %c %c  ",
            i >= a[0] && i <= a[1] ? '*' : ' ',
            i >= b[0] && i <= b[1] ? '*' : ' '
        );
        tb_print_dumb_node(NULL, block->items[i]);
        printf("\n");
    }
}

// mark program point as HRP
static void mark_point_as_hrp(Ctx* ctx, Rogers* ra, TB_Node* n, int reg_class) {
    TB_ASSERT(reg_class > 0);
    TB_OPTDEBUG(REGALLOC6)(printf("#       %%%u is considered HRP\n", gvn));

    uint32_t gvn = n->gvn;
    TB_BasicBlock* bb = ctx->f->scheduled[gvn];
    int bb_id = bb - ctx->cfg.blocks;
    int t = ra->order[gvn] - 1;

    int end_t = t;
    while (end_t+1 < aarray_length(bb->items) && IS_PROJ(bb->items[end_t+1]) && bb->items[end_t+1]->inputs[0] == n) {
        end_t++;
    }

    HRPRegion* hrp = &ra->hrp[bb_id];
    if (hrp->start[reg_class] < 0) {
        hrp->start[reg_class] = t;
        hrp->end[reg_class]   = end_t;
    } else {
        hrp->start[reg_class] = TB_MIN(hrp->start[reg_class], t);
        hrp->end[reg_class]   = TB_MAX(hrp->end[reg_class], end_t);
    }
}

static void gimme_lifetime(Ctx* ctx, Rogers* ra, TB_BasicBlock** scheduled, TB_BasicBlock* bb, TB_Node* n, int* range) {
    uint32_t gvn = n->gvn;
    int start_t = 0;
    if (scheduled[gvn] == bb) {
        if (n->type == TB_PHI) {
            start_t = 0;
        } else if (IS_PROJ(n)) {
            uint32_t tuple_gvn = n->inputs[0]->gvn;
            start_t = ra->order[tuple_gvn] - 1;
        } else {
            start_t = ra->order[gvn] - 1;
        }
    } else if (!set_get(&bb->live_in, gvn)) {
        range[0] = INT_MAX;
        range[1] = -1;
        return;
    }

    int end_t = last_use_in_bb(ctx->cfg.blocks, scheduled, ra, bb, n, gvn) - 1;
    TB_ASSERT(end_t >= start_t);

    range[0] = start_t;
    range[1] = end_t;
}

static void mark_node_as_hrp(Ctx* ctx, Rogers* ra, uint32_t gvn, uint32_t failed_gvn, int reg_class) {
    TB_ASSERT(reg_class > 0);
    TB_ASSERT(ctx->vreg_map[gvn]);

    // we're gonna spill it, so that's not really increasing HRP
    if (ctx->vregs[ctx->vreg_map[gvn]].mask->may_spill) {
        return;
    }

    TB_OPTDEBUG(REGALLOC6)(printf("#       %%%u /\\ %%%u (%s) is considered HRP range\n", gvn, failed_gvn, reg_class_name(reg_class)));

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* n = ra->gvn2node[gvn];
    TB_Node* failed = ra->gvn2node[failed_gvn];

    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

        int A[2], B[2];
        gimme_lifetime(ctx, ra, scheduled, bb, n,      A);
        gimme_lifetime(ctx, ra, scheduled, bb, failed, B);

        int start_t = TB_MAX(A[0], B[0]);
        int end_t   = TB_MIN(A[1], B[1]);
        if (end_t < 0) {
            continue;
        }

        TB_OPTDEBUG(REGALLOC6)(printf("#         BB%zu [%d (%%%u), %d (%%%u)]\n", bb_id, start_t, bb->items[start_t]->gvn, end_t, bb->items[end_t]->gvn));

        HRPRegion* hrp = &ra->hrp[bb_id];
        if (hrp->start[reg_class] < 0) {
            hrp->start[reg_class] = start_t;
            hrp->end[reg_class]   = end_t;
        } else {
            hrp->start[reg_class] = TB_MIN(hrp->start[reg_class], start_t);
            hrp->end[reg_class]   = TB_MAX(hrp->end[reg_class], end_t);
        }
    }
}

static thread_local VReg* compare_split__vregs;
static int compare_split(const void* a, const void* b) {
    VReg* vregs = compare_split__vregs;
    VReg* aa = &vregs[((const SplitDecision*) a)->target];
    VReg* bb = &vregs[((const SplitDecision*) b)->target];

    float as = aa->spill_cost - aa->area*0.2;
    float bs = bb->spill_cost - bb->area*0.2;
    if (as != bs) {
        return as > bs ? -1 : 1;
    }
    return 0;
}

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena) {
    Rogers ra = { .ctx = ctx, .arena = arena };
    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;

    TB_Worklist* ws = f->worklist;
    worklist_clear(ws);

    ra.uf_len = f->node_count;

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        int max_regs_in_class = 0;
        ra.fixed = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));

        FOR_N(i, 0, ctx->num_classes) {
            size_t count = ctx->num_regs[i];
            if (max_regs_in_class < count) {
                max_regs_in_class = count;
            }

            int base = aarray_length(ctx->vregs);
            FOR_N(j, 0, count) {
                RegMask* mask = intern_regmask(ctx, i, false, i == 0 ? j : 1ull << j);
                aarray_push(ctx->vregs, (VReg){
                        .class = i,
                        .assigned = j,
                        .mask = mask,
                        .spill_cost = INFINITY,
                        .uses = 1
                    });
            }
            ra.fixed[i] = base;
        }
        ra.num_regs  = ctx->num_regs;
        ra.max_regs_in_class = max_regs_in_class;
    }

    // used for hard-list list at the very start
    ra.potential_spills = dyn_array_create(int, 32);
    ra.splits = dyn_array_create(SplitDecision, 32);
    bool cisc_stuff = false;

    // create timeline & insert moves
    CUIK_TIMED_BLOCK("insert legalizing moves") {
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                RegMask** ins = ctx->ins;
                ctx->constraint(ctx, n, ins);

                // insert input copies (temporaries & clobbers never introduce
                // these so we're safe don't check those)
                size_t in_count = n->input_count;
                FOR_N(k, 1, in_count) if (n->inputs[k]) {
                    TB_Node* in = n->inputs[k];
                    RegMask* in_mask = ins[k];
                    if (in_mask == &TB_REG_EMPTY) { continue; }

                    VReg* in_vreg = node_vreg(ctx, in);

                    #ifndef NDEBUG
                    // common enough error that i figure i should make a proper error
                    if (in_vreg == NULL) {
                        printf("RA ERROR in %s (%s:%d):\n  ", f->super.name, __FILE__, __LINE__);
                        tb_print_dumb_node(NULL, in);
                        printf("\n  ^^^^^  this node has no vreg even though it's used by %%%u[%zu] in BB%zu:\n  ", n->gvn, k, i);
                        tb_print_dumb_node(NULL, n);
                        printf("\n");
                        tb_integrated_dbg(f, n);
                    }
                    #endif

                    int hint = fixed_reg_mask(in_mask);
                    if (hint >= 0 && in_vreg->mask->class == in_mask->class) {
                        in_vreg->hint_vreg = ra.fixed[in_mask->class] + hint;
                    }

                    // intersect use masks with the vreg's mask, if it becomes empty we've
                    // got a hard-split (not necessarily spilling to the stack)
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[k]);
                    if (in_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                        TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs));
                        dyn_array_put(ra.potential_spills, in_vreg - ctx->vregs);
                    }

                    in_vreg->mask = new_mask;
                }

                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id > 0) {
                    VReg* vreg = &ctx->vregs[vreg_id];
                    RegMask* def_mask = vreg->mask;

                    vreg->spill_cost = NAN;
                    if (n->type == TB_PHI) {
                        ra.uf_len += n->input_count;
                        worklist_push(ws, n);
                    } else if (ctx->node_2addr(n) >= 0) {
                        ra.uf_len += 1;
                        cisc_stuff = true;
                    }
                }
            }
        }
    }

    // this avoids the subsume_node calls adding nodes to the list, they'd
    // do this if you remove nodes such that they get DCE'd
    f->worklist = NULL;

    // resolving hard-splits
    if (dyn_array_length(ra.potential_spills) > 0) {
        cuikperf_region_start("hard splits", NULL);
        // insert hard split code
        FOR_N(i, 0, dyn_array_length(ra.potential_spills)) {
            VReg* vreg = &ctx->vregs[ra.potential_spills[i]];
            RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
            vreg->mask = mask;
            if (can_remat(ctx, vreg->n)) {
                rogers_remat(ctx, &ra, vreg->n, true);
            } else {
                spill_entire_lifetime(ctx, vreg, mask, vreg->n, true);
            }

            ra.uf_len += 1;
        }
        dyn_array_clear(ra.potential_spills);
        cuikperf_region_end();

        f->worklist = ws;
        redo_dataflow(ctx, arena);
        f->worklist = NULL;
    }

    ra.coalesce_set = nl_table_alloc(100);
    ra.uf = cuik_malloc(ra.uf_len * sizeof(int));
    ra.uf_size = cuik_malloc(ra.uf_len * sizeof(int));
    FOR_N(i, 0, ra.uf_len) {
        ra.uf[i] = i;
        ra.uf_size[i] = 1;
    }

    bool changes = false;
    if (cisc_stuff || dyn_array_length(ws->items) > 0) {
        cuikperf_region_start("aggro coalesce", NULL);

        TB_ArenaSavepoint sp = tb_arena_save(arena);
        compute_ordinals(ctx, &ra, arena);

        // CISC ops will coalesce with their shared edge, if not they'll insert a copy node
        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];
                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id == 0) { continue; }

                int shared_edge = ctx->node_2addr(n);
                if (shared_edge >= 0 && n->inputs[shared_edge]) {
                    int x = uf_find(ra.uf, ra.uf_len, n->gvn);
                    TB_Node* in = n->inputs[shared_edge];

                    // this is a necessary copy
                    if (n->type == TB_MACH_COPY) {
                        continue;
                    }

                    int y = uf_find(ra.uf, ra.uf_len, in->gvn);

                    RegMask* in_mask = constraint_in(ctx, n, shared_edge);
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_mask, ctx->vregs[ctx->vreg_map[y]].mask);
                    new_mask = tb__reg_mask_meet(ctx, new_mask, ctx->vregs[vreg_id].mask);

                    if (!rogers_can_coalesce(ctx, &ra, n, in) || new_mask == &TB_REG_EMPTY) {
                        // insert a copy
                        TB_OPTDEBUG(REGALLOC)(printf("CISC OP %%%u (-> %%%u) has conflict\n", n->gvn, in->gvn));

                        TB_Node* copy = rogers_hard_split(ctx, &ra, n, in, in_mask, vreg_id);
                        set_input(f, n, copy, shared_edge);
                        tb__insert_before(ctx, ctx->f, copy, n);

                        if (in->user_count == 0) {
                            // delete the original def
                            ctx->vregs[ctx->vreg_map[in->gvn]].uses -= 1;
                            ctx->vreg_map[in->gvn] = 0;
                            tb__remove_node(ctx, ctx->f, in);
                            tb_kill_node(ctx->f, in);
                        }

                        in = n->inputs[shared_edge];
                        changes = true;
                    } else {
                        ctx->vregs[vreg_id].mask = new_mask;
                    }

                    // hard coalesce with direct input
                    y = uf_find(ra.uf, ra.uf_len, in->gvn);
                    rogers_coalesce(ctx, &ra, x, y, n, in);
                }
            }
        }

        FOR_N(i, 0, dyn_array_length(ws->items)) {
            TB_Node* n = ws->items[i];
            int vreg_id = ctx->vreg_map[n->gvn];
            TB_ASSERT(n->type == TB_PHI);

            // join all these into one lifetime, make n the leader
            int x = uf_find(ra.uf, ra.uf_len, n->gvn);
            RegMask* rm = ctx->vregs[ctx->vreg_map[x]].mask;
            FOR_N(k, 1, n->input_count) {
                // interfere against everything in the set
                TB_Node* in = n->inputs[k];
                int y = uf_find(ra.uf, ra.uf_len, in->gvn);

                RegMask* new_mask = tb__reg_mask_meet(ctx, rm, ctx->vregs[ctx->vreg_map[y]].mask);
                if (!rogers_can_coalesce(ctx, &ra, n, in) || new_mask == &TB_REG_EMPTY) {
                    TB_OPTDEBUG(REGALLOC)(printf("PHI %%%u (-> %%%u) has self-conflict\n", n->gvn, in->gvn));

                    TB_Node* move = rogers_hard_split(ctx, &ra, n, in, rm, vreg_id);
                    set_input(f, n, move, k);

                    rm = tb__reg_mask_meet(ctx, rm, ctx->constraint(ctx, move, NULL));
                    TB_ASSERT(rm != &TB_REG_EMPTY);

                    TB_Node* pred = cfg_get_pred(&ctx->cfg, n->inputs[0], k - 1);
                    TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                    TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                    if (in->user_count == 0) {
                        // delete the original def
                        ctx->vregs[ctx->vreg_map[in->gvn]].uses -= 1;
                        ctx->vreg_map[in->gvn] = 0;
                        tb__remove_node(ctx, ctx->f, in);
                        tb_kill_node(ctx->f, in);
                    }

                    // place at the end of the pred BB to the phi, basically the latest point
                    insert_op_at_end(ctx, NULL, pred_bb, move);
                    changes = true;
                } else {
                    rm = new_mask;
                }

                // hard coalesce with direct input
                y = uf_find(ra.uf, ra.uf_len, n->inputs[k]->gvn);
                rogers_coalesce(ctx, &ra, x, y, n, n->inputs[k]);
            }

            TB_ASSERT(x == ra.uf[x]); // must've stayed the head
            ctx->vregs[ctx->vreg_map[x]].mask = rm;
        }

        // compute lists of coalesced nodes
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            aarray_for(j, bb->items) {
                TB_Node* n = bb->items[j];
                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id == 0) { continue; }

                // remap the vregs
                int leader = uf_find(ra.uf, ra.uf_len, n->gvn);
                if (leader != n->gvn) {
                    ctx->vreg_map[n->gvn] = ctx->vreg_map[leader];
                }
            }
        }

        ra.order_cap = 0;
        ra.order = NULL;

        tb_arena_restore(&f->tmp_arena, sp);
        cuikperf_region_end();
    }
    // RA calls might add dead nodes but we don't care
    f->worklist = ws;

    if (changes) {
        // recompute liveness
        redo_dataflow(ctx, arena);
    }

    int starting_spills = ctx->num_regs[REG_CLASS_STK];
    ra.num_spills = starting_spills;

    TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, f->node_count), printf("\n"));

    ra.hrp = tb_arena_alloc(arena, ctx->bb_count * sizeof(HRPRegion));
    FOR_N(i, 0, ctx->bb_count) {
        ra.hrp[i].start[0] = -1;
        ra.hrp[i].end[0]   = -1;
    }

    TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, f->node_count));

    int rounds = 0;
    size_t last_spills = 0;
    cuikperf_region_start("main loop", NULL);
    for (;;) {
        TB_ArenaSavepoint sp = tb_arena_save(arena);

        rounds++;
        TB_OPTDEBUG(REGALLOC)(printf("# ========= Round %d =========\n", rounds));
        TB_ASSERT(rounds < 50);

        // reset HRP regions
        FOR_N(i, 0, ctx->bb_count) {
            FOR_N(j, 1, ctx->num_classes) {
                ra.hrp[i].start[j] = -1;
                ra.hrp[i].end[j]   = -1;
            }
        }

        cuikperf_region_start("round", NULL);
        bool done = allocate_loop(ctx, &ra, arena);
        cuikperf_region_end();

        if (done) {
            TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, f->node_count));

            tb_arena_restore(arena, sp);
            cuikperf_region_end();

            cuik_free(ra.uf);
            cuik_free(ra.uf_size);
            nl_table_free(ra.coalesce_set);

            #if 0
            // dump_sched(ctx);
            FOR_N(i, 0, f->node_count) {
                int x = ctx->vreg_map[i];
                if (x > 0) {
                    TB_Node* xn = ra.gvn2node[i];
                    FOR_N(j, i+1, f->node_count) {
                        int y = ctx->vreg_map[j];
                        TB_Node* yn = ra.gvn2node[j];

                        if (y > 0 && interfere(ctx, &ra, xn, yn)) {
                            if (ctx->vregs[x].class == ctx->vregs[y].class && ctx->vregs[x].assigned == ctx->vregs[y].assigned) {
                                printf("V%u (%%%u) interfering with V%u (%%%u): ", x, xn->gvn, y, yn->gvn);
                                print_reg_name(ctx->vregs[x].class, ctx->vregs[x].assigned);
                                printf("\n");
                            }
                        }
                    }
                }
            }
            #endif

            if (ctx->features.gen & TB_FEATURE_STACK_MAPS) {
                CUIK_TIMED_BLOCK("build stack maps") {
                    // Sparse set repr
                    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                    ArenaArray(int) stack = aarray_create(&f->tmp_arena, int, 30);
                    int* array = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));
                    FOR_REV_N(i, 0, ctx->bb_count) {
                        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

                        aarray_clear(stack);
                        FOR_N(i, 0, f->node_count) {
                            array[i] = -1;
                        }

                        // start intervals
                        BITS64_FOR(j, bb->live_out.data, bb->live_out.capacity) {
                            if (array[j] < 0 && is_gcref_dt(ra.gvn2node[j]->dt)) {
                                array[j] = aarray_length(stack);
                                aarray_push(stack, j);
                            }
                        }

                        FOR_REV_N(j, 0, aarray_length(bb->items)) {
                            TB_Node* n = bb->items[j];

                            // expire
                            if (array[n->gvn] >= 0) {
                                int last_gvn = stack[aarray_length(stack) - 1];
                                aarray_remove(stack, array[n->gvn]);
                                array[last_gvn] = array[n->gvn];
                                array[n->gvn] = -1;
                            }

                            if (tb_node_is_safepoint(n)) {
                                TB_Safepoint* sfpt = tb_arena_alloc(ctx->emit.arena, sizeof(TB_Safepoint) + aarray_length(stack)*sizeof(int32_t));
                                sfpt->func = ctx->f;
                                sfpt->node = n;
                                sfpt->count = aarray_length(stack);

                                aarray_for(k, stack) {
                                    TB_ASSERT(ctx->vreg_map[stack[k]] > 0);
                                    VReg* v = &ctx->vregs[ctx->vreg_map[stack[k]]];
                                    int ref_type = ra.gvn2node[stack[k]]->dt.elem_or_addrspace;
                                    sfpt->refs[k] = (v->class << 24u) | (v->assigned << 8u) | ref_type;
                                }

                                TB_NodeSafepoint* n_sfpt = TB_NODE_GET_EXTRA(n);
                                sfpt->userdata = n_sfpt->userdata;
                                n_sfpt->sfpt = sfpt;
                            }

                            // start intervals
                            if (n->type != TB_PHI) {
                                FOR_N(k, 1, n->input_count) {
                                    TB_Node* in = n->inputs[k];
                                    if (in && is_gcref_dt(in->dt)) {
                                        // alive
                                        if (array[in->gvn] < 0) {
                                            array[in->gvn] = aarray_length(stack);
                                            aarray_push(stack, in->gvn);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    tb_arena_restore(&f->tmp_arena, sp);
                }
            }

            ctx->num_spills += ra.num_spills - starting_spills;
            // printf("ROUNDS ON %s: %d\n", f->super.name, rounds);
            return;
        }

        // gonna be modding shit too much for it to matter.
        ra.inactive_cache = NULL;
        cuikperf_region_start("insert spills", NULL);

        TB_OPTDEBUG(REGALLOC5)(printf("=== SPILLING ===\n"));

        // alloc failures on may_spill vregs will just drop to stack
        for (size_t i = 0; i < dyn_array_length(ra.splits);) {
            SplitDecision split = ra.splits[i];
            VReg* v = &ctx->vregs[split.target];
            if (v->mask->may_spill) {
                TB_OPTDEBUG(REGALLOC3)(printf("  * V%u is now a proper spill\n", split.target));

                v->spill_cost = INFINITY;
                v->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                v->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, v->n->dt);

                // reset assignment, it's going on the stack next time
                v->class = 0;
                v->assigned = -1;

                dyn_array_remove(ra.splits, i);
            } else {
                i += 1;
            }
        }

        // printf("  %zu\n", dyn_array_length(ra.splits));

        size_t num_spills = dyn_array_length(ra.splits);
        if (num_spills) {
            /* if (rounds == 1) {
                printf("AAA %d %zu\n", rounds, num_spills);
            } else {
                printf("AAA %d %zu (%+d)\n", rounds, num_spills, (int)num_spills - (int)last_spills);
            }
            last_spills = num_spills;*/

            // reset assignment, but don't try to split them this round
            if (num_spills > 64) {
                compare_split__vregs = ctx->vregs;
                qsort(ra.splits, num_spills, sizeof(SplitDecision), compare_split);

                for (size_t i = 64; i < num_spills; i++) {
                    SplitDecision split = ra.splits[i];
                    VReg* v = &ctx->vregs[split.target];
                    v->class = 0;
                    v->assigned = -1;
                }
                dyn_array_set_length(ra.splits, 64);
            }

            tb__insert_splits(ctx, &ra);
            dyn_array_clear(ra.splits);
        }

        cuikperf_region_end();

        // reset assignments
        /*FOR_N(i, 1, aarray_length(ctx->vregs)) {
            if (!rogers_is_fixed(ctx, &ra, i) && ctx->vregs[i].assigned >= 0) {
                ctx->vregs[i].class = 0;
                ctx->vregs[i].assigned = -1;
            }
        }*/

        tb_arena_restore(arena, sp);
        redo_dataflow(ctx, arena);
    }
}

TB_OPTDEBUG(STATS)(static int stats_c = 0);

static uint32_t inactive_hash_index(uint64_t key) {
    const uint8_t* data = (uint8_t*) &key;
    /*uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < sizeof(uint32_t); i++) {
        h = (data[i] ^ h) * 0x01000193;
    }*/
    return (key * 11400714819323198485ull) >> (64ull - 7ull);
}

static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn) {
    // printf("Last use in BB0 for %%%u: ", n->gvn);
    TB_OPTDEBUG(STATS)(stats_c++);

    uint64_t key = n_gvn | ((bb - blocks) << 32ull);
    int hash_index = inactive_hash_index(key);
    TB_ASSERT(hash_index < 128);
    if (ra->inactive_cache && ra->inactive_cache[hash_index].key == key) {
        // printf(" Hit!\n");
        TB_OPTDEBUG(STATS)(stats_hit += 1);
        return ra->inactive_cache[hash_index].last_use;
    }

    TB_OPTDEBUG(STATS)(stats_miss += 1);

    // printf(" Miss.\n");
    int l = 1;
    if (scheduled[n->gvn] == bb) {
        l = ra->order[n->gvn];
    }

    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (USERI(u) < un->input_count &&
            scheduled[un->gvn] == bb &&
            l < ra->order[un->gvn]) {
            l = ra->order[un->gvn];
        }
    }

    if (set_get(&bb->live_out, n->gvn)) {
        // if there's no uses, we'll assume it's live out so the
        // "last use" is the BB end
        int end = aarray_length(bb->items);
        l = TB_MAX(l, end);
    }

    if (ra->inactive_cache) {
        ra->inactive_cache[hash_index].key = key;
        ra->inactive_cache[hash_index].last_use = l;
    }
    return l;
}

static bool order_after(int* order, TB_BasicBlock** scheduled, uint32_t a_gvn, uint32_t b_gvn) {
    // the basic blocks are sequentially allocated in reverse post-order
    return scheduled[a_gvn] > scheduled[b_gvn] || (scheduled[a_gvn] == scheduled[b_gvn] && order[a_gvn] > order[b_gvn]);
}

static bool interfere_in_block(TB_BasicBlock* blocks, int* order, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs, uint32_t lhs_gvn, uint32_t rhs_gvn, TB_BasicBlock* block) {
    TB_ASSERT(lhs != rhs && "i... why?");

    // phis might have a liveness hole in the middle
    bool lhs_live_out = set_get(&block->live_out, lhs_gvn);
    bool rhs_live_out = set_get(&block->live_out, rhs_gvn);
    if (lhs_live_out && rhs_live_out) {
        return true;
    } else if (!lhs_live_out && !rhs_live_out) {
        TB_Node *first = lhs, *last = rhs;
        uint32_t first_gvn = lhs_gvn, last_gvn = rhs_gvn;
        if (order_after(order, scheduled, lhs_gvn, rhs_gvn)) {
            first = rhs, last = lhs;
            first_gvn = rhs_gvn, last_gvn = lhs_gvn;
        }

        int last_t = order[last_gvn];
        int last_use = last_use_in_bb(blocks, scheduled, ra, block, first, first_gvn);
        return scheduled[last_gvn] == block && last_use > last_t;
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
            SWAP(uint32_t, lhs_gvn, rhs_gvn);
        }

        int rhs_t = scheduled[rhs_gvn] == block ? ra->order[rhs_gvn] : 0;
        int last_use = last_use_in_bb(blocks, scheduled, ra, block, lhs, lhs_gvn);
        return last_use > rhs_t;
    }
}

static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs) {
    TB_BasicBlock** scheduled = ctx->f->scheduled;
    int* order = ra->order;

    uint32_t lhs_gvn = lhs->gvn, rhs_gvn = rhs->gvn;
    TB_BasicBlock* lhs_block = scheduled[lhs_gvn];
    TB_BasicBlock* rhs_block = scheduled[rhs_gvn];

    if (interfere_in_block(ctx->cfg.blocks, order, scheduled, ra, lhs, rhs, lhs_gvn, rhs_gvn, lhs_block)) {
        return true;
    }

    return lhs_block != rhs_block && interfere_in_block(ctx->cfg.blocks, order, scheduled, ra, rhs, lhs, rhs_gvn, lhs_gvn, rhs_block);
}

static void mark_active(Ctx* restrict ctx, Rogers* restrict ra, int gvn) {
    VReg* vreg = &ctx->vregs[ctx->vreg_map[gvn]];
    if (vreg->class == REG_CLASS_STK) {
        TB_ASSERT(vreg->assigned < ra->stack_reg_count);
    } else {
        TB_ASSERT(vreg->assigned < ctx->num_regs[vreg->class]);
    }

    // if there's a value already in the active slot and it's
    // the same vreg, we're just handing off our assignment and
    // we need to move that node to future active
    int old = ra->active[vreg->class][vreg->assigned];
    if (old != 0) {
        // TB_ASSERT(ctx->vreg_map[old] == ctx->vreg_map[gvn]);
        set_put(&ra->future_active, old);
    }

    ra->active[vreg->class][vreg->assigned] = gvn;
}

static void unmark_active(Ctx* restrict ctx, Rogers* restrict ra, int gvn) {
    TB_ASSERT(ctx->vreg_map[gvn]);
    VReg* other = &ctx->vregs[ctx->vreg_map[gvn]];
    ra->active[other->class][other->assigned] = 0;
}

static SplitDecision choose_best_spill(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* attempted_n) {
    cuikperf_region_start("choose spill", NULL);

    // these are added late because... there's a lot of them sometimes and we'd rather not add them
    // every allocation loop, if i could do the same with inactives without incurring an extra interference
    // check i would.
    VReg* attempted_vreg = node_vreg(ctx, attempted_n);
    RegMask* useful_mask = attempted_vreg->mask;
    int useful_class = attempted_vreg->mask->class;
    FOR_N(i, 0, ctx->num_regs[useful_class]) {
        // TODO(NeGate): this is bit math & tests, we can speed this process up
        if (ra->active[useful_class][i] > 0 && bits64_member(ra->mask, i)) {
            dyn_array_put(ra->potential_spills, ra->active[useful_class][i]);
        }
    }

    int best_spill = 0;
    int best_gvn = 0;
    float best_score = INFINITY;
    FOR_REV_N(i, 0, dyn_array_length(ra->potential_spills)) {
        int gvn = ra->potential_spills[i];

        TB_ASSERT(ctx->vreg_map[gvn] > 0);
        VReg* vreg  = &ctx->vregs[ctx->vreg_map[gvn]];

        // we'll only spill things which can make aggressive forward progress
        if (attempted_vreg->was_spilled && !within_reg_mask(useful_mask, vreg->assigned)) {
            continue;
        }

        float score = rogers_get_spill_cost(ctx, ra, vreg);
        if (set_get(&ra->been_spilled, ctx->vreg_map[gvn])) {
            score += 1e6;
        }

        if (score <= best_score) {
            if (best_spill >= 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is a better spill! V%d (%f is better than %f)\n", gvn, best_spill, score, best_score));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is... one of the spills of all time! %f\n", gvn, score));
            }
            best_score = score;
            best_spill = ctx->vreg_map[gvn];
            best_gvn = gvn;
        } else {
            TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is a bad pick! %f\n", gvn, score));
        }
    }

    int us = ctx->vreg_map[attempted_n->gvn];
    float us_score = rogers_get_spill_cost(ctx, ra, &ctx->vregs[us]);
    if (us_score < best_score) {
        // mark the second best as the HRP region to split around it
        if (best_spill > 0 && best_score != INFINITY) {
            mark_node_as_hrp(ctx, ra, best_gvn, attempted_n->gvn, useful_class);
        } else {
            // mark all use sites as HRP
            FOR_USERS(u, attempted_n) {
                mark_point_as_hrp(ctx, ra, USERN(u), useful_class);
            }
        }

        best_spill = us;
        best_score = us_score;
    } else {
        mark_node_as_hrp(ctx, ra, attempted_n->gvn, best_gvn, useful_class);
    }
    TB_ASSERT(best_score != INFINITY);

    TB_ASSERT(best_spill > 0);
    cuikperf_region_end();
    return (SplitDecision){ best_spill };
}

static bool allocate_reg(Ctx* ctx, Rogers* ra, TB_Node* n) {
    int vreg_id = ctx->vreg_map[n->gvn];
    VReg* vreg = &ctx->vregs[vreg_id];

    // if we've spilled we shouldn't be able to wake up again.
    if (vreg_id == 0 || set_get(&ra->been_spilled, vreg_id)) {
        return false;
    }

    if (set_get(&ra->future_active, n->gvn)) {
        TB_OPTDEBUG(REGALLOC5)(printf("#     woke up %%%u\n", n->gvn));

        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, n->gvn);
        mark_active(ctx, ra, n->gvn);
        return true;
    }

    if (vreg->assigned >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        mark_active(ctx, ra, n->gvn);
        return true;
    }

    #if TB_OPTDEBUG_REGALLOC5
    printf("#\n");
    rogers_print_vreg(ctx, ra, vreg);
    printf("# ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
    #endif

    int def_class = vreg->mask->class;
    int num_regs = def_class == REG_CLASS_STK ? ra->num_spills : ctx->num_regs[def_class];

    size_t mask_word_count = (num_regs + 63) / 64;
    RegMask* mask = vreg->mask;

    // what the regmask holds only applies up until num_regs[class], this is mostly
    // just relevant for the stack coloring i suppose
    size_t nr = ctx->num_regs[mask->class];
    uint64_t* ra_mask = ra->mask;
    {
        FOR_N(j, 0, mask->count) { ra_mask[j] = ~mask->mask[j]; }
        FOR_N(j, mask->count, (nr + 63) / 64) { ra_mask[j] = UINT64_MAX; }
        if (nr % 64) {
            ra_mask[nr / 64] &= UINT64_MAX >> (64ull - (nr % 64));
        }
    }

    // interfere live things
    dyn_array_clear(ra->potential_spills);
    TB_OPTDEBUG(REGALLOC5)(printf("#   "));

    FOR_N(i, 0, num_regs) {
        uint32_t gvn = ra->active[def_class][i];
        if (gvn > 0) {
            VReg* vreg = &ctx->vregs[ctx->vreg_map[gvn]];
            uint64_t allot_mask = UINT64_MAX >> (64ull - vreg->reg_width);
            ra_mask[i / 64ull] |= allot_mask << (i % 64ull);

            TB_OPTDEBUG(REGALLOC5)(printf("%%%u -- ", ra->active[def_class][i]), print_reg_name(def_class, i), printf("; "));
        }
    }

    // cuikperf_region_start("I", NULL);
    // every node in the coalesced set needs to be checked for interference
    int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    size_t cnt = set ? aarray_length(set) : 1;
    TB_Node** arr = set ? &set[0] : &n;

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    BITS64_FOR(i, ra->future_active.data, ra->future_active.capacity) {
        TB_Node* other = ra->gvn2node[i];
        VReg* other_vreg = &ctx->vregs[ctx->vreg_map[i]];
        if (other_vreg->class != def_class || !reg_mask_may_intersect(other_vreg->mask, mask)) {
            continue;
        }

        if (other_vreg->assigned < 0) {
            set_remove(&ra->future_active, i);
            continue;
        }

        uint64_t allot_mask = UINT64_MAX >> (64ull - other_vreg->reg_width);
        int other_assigned = other_vreg->assigned;
        FOR_N(j, 0, cnt) {
            if (interfere(ctx, ra, arr[j], other)) {
                TB_ASSERT(other_vreg->assigned >= 0);
                if (within_reg_mask(mask, other_vreg->assigned)) {
                    TB_OPTDEBUG(REGALLOC5)(printf("%%%u -- ", other->gvn), print_reg_name(def_class, other_assigned), printf("; "));
                }

                dyn_array_put(ra->potential_spills, i);
                ra_mask[other_assigned / 64ull] |= (allot_mask << (other_assigned % 64ull));
                break;
            }
        }
    }
    TB_OPTDEBUG(REGALLOC5)(printf("\n"));
    // cuikperf_region_end();

    int hint_vreg = vreg->hint_vreg;
    int hint_reg = hint_vreg > 0
        && ctx->vregs[hint_vreg].class == mask->class
        ?  ctx->vregs[hint_vreg].assigned
        :  -1;

    if (hint_reg >= 0 && (ra_mask[hint_reg / 64ull] & (1ull << (hint_reg % 64ull))) == 0) {
        TB_OPTDEBUG(REGALLOC5)(printf("#   assigned to "), print_reg_name(def_class, hint_reg), printf(" (HINTED)\n"));

        vreg->class    = def_class;
        vreg->assigned = hint_reg;
        mark_active(ctx, ra, n->gvn);

        // mark all the other members of the coalesced group as future active
        FOR_N(j, 0, cnt) {
            if (arr[j] != n) {
                TB_ASSERT(arr[j]->gvn < ctx->f->node_count);
                TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
                set_put(&ra->future_active, arr[j]->gvn);
                TB_OPTDEBUG(REGALLOC5)(printf("#     sleep  %%%u\n", arr[j]->gvn));
            }
        }

        return true;
    }

    if (reg_assign(ctx, vreg, ra_mask, num_regs)) {
        TB_OPTDEBUG(REGALLOC5)(printf("#       assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        mark_active(ctx, ra, n->gvn);

        // mark all the other members of the coalesced group as future active
        FOR_N(j, 0, cnt) {
            if (arr[j] != n) {
                TB_ASSERT(arr[j]->gvn < ctx->f->node_count);
                TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
                set_put(&ra->future_active, arr[j]->gvn);
                TB_OPTDEBUG(REGALLOC5)(printf("#     sleep  %%%u\n", arr[j]->gvn));
            }
        }

        return true;
    } else {
        // if a stack slot failed to color then it means we
        // need more stack slots (there's an indefinite amount :p)
        if (def_class == REG_CLASS_STK) {
            // stack's active set is resizable
            if (ra->num_spills + vreg->reg_width > ra->stack_reg_count) {
                size_t new_size = ra->stack_reg_count * 2;
                TB_ASSERT(ra->num_spills + vreg->reg_width - 1 <= new_size);

                ra->active[0] = tb_arena_realloc(ra->arena, ra->active[0], ra->stack_reg_count * sizeof(int), new_size * sizeof(int));
                FOR_N(i, ra->stack_reg_count, new_size) {
                    ra->active[0][i] = 0;
                }
                ra->stack_reg_count = new_size;
            }

            vreg->class = REG_CLASS_STK;
            vreg->assigned = ra->num_spills;
            mark_active(ctx, ra, n->gvn);
            ra->num_spills += vreg->reg_width;

            // mark all the other members of the coalesced group as future active
            FOR_N(j, 0, cnt) {
                if (arr[j] != n) {
                    TB_ASSERT(arr[j]->gvn < ctx->f->node_count);
                    TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
                    set_put(&ra->future_active, arr[j]->gvn);
                    TB_OPTDEBUG(REGALLOC)(printf("#     sleep  %%%u\n", arr[j]->gvn));
                }
            }

            // resize the mask array if necessary
            size_t new_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
            if (((ra->mask_cap+63)/64) < (new_cap+63)/64) {
                ra->mask_cap = new_cap + 64;
                ra->mask = tb_arena_alloc(ra->arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));
            }

            TB_OPTDEBUG(REGALLOC5)(printf("#     assigned to STACK%d (new stack slot)\n", vreg->assigned));
            return true;
        }

        SplitDecision s = choose_best_spill(ctx, ra, n);
        TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mGONNA SPILL V%u\x1b[0m\n", s.target));

        if (!set_get(&ra->been_spilled, s.target)) {
            set_put(&ra->been_spilled, s.target);
            dyn_array_put(ra->splits, s);
        }

        if (s.target == vreg_id) {
            // kill this node for now
            unmark_active(ctx, ra, n->gvn);
            set_remove(&ra->live, n->gvn);
            set_remove(&ra->future_active, n->gvn);

            TB_OPTDEBUG(REGALLOC5)(printf("#       assigned UNCOLORED\n"));
        } else {
            VReg* evicted = &ctx->vregs[s.target];

            vreg->class = evicted->class;
            vreg->assigned = evicted->assigned;
            mark_active(ctx, ra, n->gvn);

            // mark all the other members of the coalesced group as future active
            FOR_N(j, 0, cnt) {
                if (arr[j] != n) {
                    TB_ASSERT(arr[j]->gvn < ctx->f->node_count);
                    TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
                    set_put(&ra->future_active, arr[j]->gvn);
                    TB_OPTDEBUG(REGALLOC5)(printf("#     sleep  %%%u\n", arr[j]->gvn));
                }
            }

            TB_OPTDEBUG(REGALLOC5)(printf("#       assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        }

        return false;
    }
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    size_t new_cap = tb_next_pow2(ctx->f->node_count + 16);
    ra->order = tb_arena_alloc(arena, new_cap * sizeof(int));
    ra->gvn2node = tb_arena_alloc(arena, new_cap * sizeof(TB_Node*));
    ra->order_cap = new_cap;

    // just give the root node a fake ordinal
    TB_ASSERT(ctx->f->root_node->gvn == 0);
    ra->order[0] = 1;

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];

            ra->gvn2node[n->gvn] = n;
            ra->order[n->gvn] = timeline++;
        }
    }
}

// Probably slow...
static void compute_areas(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    CUIK_TIMED_BLOCK("areas") {
        // Sparse set repr
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        ArenaArray(int) stack = aarray_create(arena, int, 30);
        int* array = tb_arena_alloc(arena, ctx->f->node_count * sizeof(int));
        aarray_for(i, ctx->vregs) {
            ctx->vregs[i].area = 0;
        }

        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            uint64_t freq = bb->freq >= 0.1 ? (bb->freq * 10) : 1;
            TB_ASSERT(freq > 0);

            aarray_clear(stack);
            FOR_N(i, 0, ctx->f->node_count) {
                array[i] = -1;
            }

            // start intervals
            BITS64_FOR(j, bb->live_out.data, bb->live_out.capacity) {
                int vreg_id = ctx->vreg_map[j];
                if (vreg_id > 0 && array[j] < 0) {
                    array[j] = aarray_length(stack);
                    aarray_push(stack, j);
                }
            }

            TB_OPTDEBUG(REGALLOC_AREA)(printf("# BB%zu\n", i));
            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                // expire
                if (array[n->gvn] >= 0) {
                    int last_gvn = stack[aarray_length(stack) - 1];
                    aarray_remove(stack, array[n->gvn]);
                    array[last_gvn] = array[n->gvn];
                    array[n->gvn] = -1;
                }

                TB_OPTDEBUG(REGALLOC_AREA)(printf("# "));

                // accumulate area (don't do so on projections and temps)
                if (n->type != TB_MACH_TEMP && !IS_PROJ(n)) {
                    aarray_for(k, stack) {
                        TB_ASSERT(ctx->vreg_map[stack[k]] > 0);
                        VReg* v = &ctx->vregs[ctx->vreg_map[stack[k]]];
                        v->area += freq;

                        TB_OPTDEBUG(REGALLOC_AREA)(printf("%%%u ", stack[k]));
                    }
                }

                TB_OPTDEBUG(REGALLOC_AREA)(printf("\n"));

                // start intervals
                if (n->type != TB_PHI) {
                    FOR_N(k, 1, n->input_count) {
                        TB_Node* in = n->inputs[k];
                        if (in && ctx->vreg_map[in->gvn] > 0) {
                            // alive
                            if (array[in->gvn] < 0) {
                                array[in->gvn] = aarray_length(stack);
                                aarray_push(stack, in->gvn);
                            }
                        }
                    }
                }
            }
        }
        tb_arena_restore(arena, sp);
    }
}

static void expire_interval(Ctx* ctx, Rogers* restrict ra, TB_Node* n, size_t bb_i);
static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    cuikperf_region_start("init", NULL);
    ra->inactive_cache = tb_arena_alloc(arena, 128 * sizeof(InactiveCacheEntry));
    memset(ra->inactive_cache, 0, 128 * sizeof(InactiveCacheEntry));
    cuikperf_region_end();

    ra->been_spilled  = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->future_active = set_create_in_arena(arena, ctx->f->node_count);
    ra->live          = set_create_in_arena(arena, ctx->f->node_count);
    ra->dirty_bb      = tb_arena_alloc(arena, ctx->bb_count * sizeof(int));

    FOR_N(i, 0, ctx->bb_count) {
        ra->dirty_bb[i] = -1;
    }

    // [class][reg]
    FOR_N(i, 1, ctx->num_classes) {
        int nr = (ctx->num_regs[i] + 15) & -16;
        ra->active[i] = tb_arena_alloc(arena, nr * sizeof(int));
        memset(ra->active[i], 0, nr * sizeof(int));
    }

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* root = ctx->f->root_node;
    compute_ordinals(ctx, ra, arena);

    ra->stack_reg_count = tb_next_pow2(ctx->num_regs[REG_CLASS_STK] + ra->num_spills + 8);
    if (ra->stack_reg_count < 16) {
        ra->stack_reg_count = 16;
    }

    ra->mask_cap = ra->max_regs_in_class > ra->stack_reg_count ? ra->max_regs_in_class : ra->stack_reg_count;
    ra->mask = tb_arena_alloc(arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));

    ra->active[0] = tb_arena_alloc(arena, ra->stack_reg_count * sizeof(int));
    memset(ra->active[0], 0, ra->stack_reg_count * sizeof(int));

    // Compute areas (probably slow...)
    compute_areas(ctx, ra, arena);

    TB_OPTDEBUG(REGALLOC5)(printf("=== PRE-COLOR ===\n"));

    // allocate pre-colored ranges
    CUIK_TIMED_BLOCK("pre-color") {
        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];
                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id > 0 && !set_get(&ra->future_active, n->gvn)) {
                    // allocate pre-colored regs before anything else
                    int fixed = fixed_reg_mask(ctx->vregs[vreg_id].mask);
                    if (ctx->vregs[vreg_id].assigned >= 0 || fixed >= 0) {
                        if (ctx->vregs[vreg_id].class != REG_CLASS_STK && fixed < 0) {
                            // this value was colored by a previous allocation pass, we'll limit the mask accordingly
                            ctx->vregs[vreg_id].mask = intern_regmask2(ctx, ctx->vregs[vreg_id].class, false, ctx->vregs[vreg_id].assigned);
                        }

                        // "reset" assignment
                        ctx->vregs[vreg_id].class = 0;
                        ctx->vregs[vreg_id].assigned = -1;

                        // must account for interference against any of the previously defined regs
                        cuikperf_region_start("step", NULL);
                        allocate_reg(ctx, ra, n);

                        // put the pre-colored regs to rest
                        TB_ASSERT(n->gvn < ctx->f->node_count);
                        unmark_active(ctx, ra, n->gvn);

                        size_t cnt;
                        TB_Node** set = coalesce_set_array(ctx, ra, &n, &cnt);
                        FOR_N(k, 0, cnt) {
                            // put the pre-colored regs to rest
                            TB_ASSERT(set[k]->gvn < ctx->f->node_count);
                            set_put(&ra->future_active, set[k]->gvn);
                        }
                        cuikperf_region_end();
                    }
                }
            }
        }
    }

    TB_OPTDEBUG(REGALLOC5)(printf("=== ASSIGN REGS ===\n"));

    size_t uf_len = ctx->f->node_count;
    FOR_REV_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        TB_OPTDEBUG(REGALLOC5)(printf("# ========= BB%zu =========\n", bb_id));

        cuikperf_region_start("expire", NULL);
        // expire intervals for block (was live, isn't now)
        // FOREACH_SET(j, ra->live) if (!set_get(live_out, j)) {
        BITS64_FOR_ANDN(j, ra->live.data, live_out->data, live_out->capacity) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                expire_interval(ctx, ra, ra->gvn2node[j], bb_id);
            }
        }
        cuikperf_region_end();

        // start intervals
        cuikperf_region_start("start", NULL);
        // FOREACH_SET(j, *live_out) if (!set_get(&ra->live, j)) {
        BITS64_FOR_ANDN(j, live_out->data, ra->live.data, live_out->capacity) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                allocate_reg(ctx, ra, ra->gvn2node[j]);
            }
        }
        cuikperf_region_end();

        // it's a backwards walk so...
        //   each use will try to place the node into the active set.
        //   each def will either put the node to sleep or kill it.
        FOR_REV_N(j, 0, aarray_length(bb->items)) {
            TB_Node* n = bb->items[j];
            if (0) { // if (!IS_PROJ(n) || n->inputs[0] != f->root_node) {
                continue;
            }

            TB_OPTDEBUG(REGALLOC5)(printf("#\n# "), tb_print_dumb_node(NULL, n), printf("\n"));

            int def_t  = ra->order[n->gvn];
            cuikperf_region_start("step", NULL);
            // expire intervals
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                // if a node is never used before its def, it's still considered
                // for allocation for that single program point.
                if (!set_get(&ra->live, n->gvn) && allocate_reg(ctx, ra, n)) {
                    set_put(&ra->live, n->gvn);
                }

                expire_interval(ctx, ra, n, bb_id);

                /*FOR_USERS(u, to_spill) {
                    TB_BasicBlock* bb = f->scheduled[USERN(u)->gvn];
                    if (IS_PROJ(USERN(u))) {
                        is_tuple = true;
                    }
                }*/
            }

            // if anything in the active set interferes with
            // the kill set, we need to spill around this op.
            RegMask** kills = ctx->ins;
            int kill_count = ctx->constraint_kill(ctx, n, kills);
            FOR_N(k, 0, kill_count) {
                TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mCLOBBERING "), tb__print_regmask(&OUT_STREAM_DEFAULT, kills[k]), printf("\x1b[0m\n"));

                bool hrp_point = false;
                int kill_class = kills[k]->class;
                int nr = ctx->num_regs[kills[k]->class];
                int* active = ra->active[kills[k]->class];
                BITS64_FOR(l, kills[k]->mask, nr) {
                    // TB_ASSERT(active[l] == 0);

                    #if 1
                    if (active[l] > 0) {
                        int in_use = active[l];
                        int in_use_vreg_id = ctx->vreg_map[in_use];

                        if (!hrp_point) {
                            hrp_point = true;
                            mark_point_as_hrp(ctx, ra, n, kill_class);
                        }

                        if (!set_get(&ra->been_spilled, in_use_vreg_id)) {
                            int best_spill = in_use_vreg_id;
                            double best_cost = rogers_get_spill_cost(ctx, ra, &ctx->vregs[in_use_vreg_id]);

                            TB_OPTDEBUG(REGALLOC5)(printf("#     %%%u V%u (", in_use, in_use_vreg_id), print_reg_name(kill_class, l), printf(") is us! %f\n", best_cost));

                            // if we can split an existing cheaper VReg (which isn't clobbered)
                            // around ourselves, we can steal its register.
                            double split_bias = 0.0;
                            FOR_N(i, 0, ctx->num_regs[kill_class]) {
                                uint32_t gvn = active[i];
                                if (gvn == 0) {
                                    continue;
                                }

                                int other_vreg_id = ctx->vreg_map[gvn];
                                TB_ASSERT(other_vreg_id > 0);
                                if (set_get(&ra->been_spilled, other_vreg_id)) {
                                    continue;
                                }

                                VReg* other_vreg = &ctx->vregs[other_vreg_id];
                                if (other_vreg->class == kill_class && !within_reg_mask(kills[k], other_vreg->assigned)) {
                                    double cost = rogers_get_spill_cost(ctx, ra, other_vreg);
                                    if (cost < best_cost) {
                                        TB_OPTDEBUG(REGALLOC5)(printf("#     %%%u V%u (", gvn, other_vreg_id), print_reg_name(other_vreg->class, other_vreg->assigned), printf(") is a good eviction! %f\n", cost));

                                        best_spill = other_vreg_id;
                                        best_cost  = cost;
                                    } else {
                                        TB_OPTDEBUG(REGALLOC5)(printf("#     %%%u V%u (", gvn, other_vreg_id), print_reg_name(other_vreg->class, other_vreg->assigned), printf(") is a bad eviction! %f\n", cost));
                                    }
                                }
                            }

                            TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mSPILLING V%u FOR %%%u (CLOBBERED BY %%%u)\x1b[0m\n", best_spill, in_use, n->gvn));

                            if (best_spill != in_use_vreg_id && !set_get(&ra->been_spilled, best_spill)) {
                                set_put(&ra->been_spilled, best_spill);

                                SplitDecision s = { .target = best_spill, .clobber = kills[k] };
                                dyn_array_put(ra->splits, s);
                            }

                            if (!set_get(&ra->been_spilled, in_use_vreg_id)) {
                                set_put(&ra->been_spilled, in_use_vreg_id);

                                SplitDecision s = { .target = in_use_vreg_id, .clobber = kills[k] };
                                dyn_array_put(ra->splits, s);
                            }
                        }

                        // kill this node for now (and "EVER")
                        unmark_active(ctx, ra, in_use);
                        set_remove(&ra->live, in_use);
                        set_remove(&ra->future_active, in_use);
                    }
                    #endif
                }
            }

            // start intervals
            if (n->type != TB_PHI) {
                FOR_N(k, 1, n->input_count) {
                    TB_Node* in = n->inputs[k];
                    if (in && ctx->vreg_map[in->gvn] > 0) {
                        if (allocate_reg(ctx, ra, in)) {
                            set_put(&ra->live, in->gvn);
                        }
                    }
                }
            }

            #if 0 // TB_OPTDEBUG_REGALLOC5
            tb_print_dumb_node(NULL, n);
            printf("\n  [Active: ");
            FOR_N(class, 1, ctx->num_classes) {
                FOR_N(reg_num, 0, ctx->num_regs[class]) {
                    if (ra->active[class][reg_num]) {
                        bool new_alloc = false;
                        FOR_N(k, 1, n->input_count) {
                            TB_Node* in = n->inputs[k];
                            if (in && in->gvn == ra->active[class][reg_num]) {
                                new_alloc = true;
                                break;
                            }
                        }

                        if (new_alloc) {
                            printf("\x1b[32m");
                        }
                        printf("%%%u:", ra->active[class][reg_num]);
                        print_reg_name(class, reg_num);
                        if (new_alloc) {
                            printf("\x1b[0m");
                        }
                        printf(" ");
                    }
                }
                printf("    ");
            }
            printf("]\n");
            #endif

            cuikperf_region_end();
        }
    }

    return dyn_array_length(ra->splits) == 0;
}

////////////////////////////////
// Expire or Start intervals
////////////////////////////////
static void expire_interval(Ctx* ctx, Rogers* restrict ra, TB_Node* n, size_t bb_i) {
    uint32_t gvn = n->gvn;
    TB_ASSERT(gvn < ctx->f->node_count);

    if (ctx->vreg_map[gvn]) {
        bool pause = false;
        FOR_N(k, 0, bb_i) {
            TB_BasicBlock* other = &ctx->cfg.blocks[k];
            if (set_get(&other->live_out, gvn)) {
                // move to future active
                set_put(&ra->future_active, gvn);
                pause = true;
                break;
            }
        }

        // if there's any uses above bb_i (phis), we also need to pause
        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            int other_bb = ctx->f->scheduled[un->gvn] - ctx->cfg.blocks;
            if (other_bb < bb_i) {
                // move to future active
                set_put(&ra->future_active, gvn);
                pause = true;
                break;
            }
        }

        if (pause) {
            TB_OPTDEBUG(REGALLOC5)(printf("#   sleep  %%%u\n", gvn));
        } else {
            TB_OPTDEBUG(REGALLOC5)(printf("#   expire %%%u\n", gvn));
        }
        unmark_active(ctx, ra, gvn);
        set_remove(&ra->live, gvn);
    }
}

typedef struct {
    TB_BasicBlock* bb;
    size_t order;
} SlotIndex;

static SlotIndex find_slot_index_start(Ctx* ctx, Rogers* restrict ra, TB_Node* n, bool skip_projs) {
    TB_BasicBlock* block = ctx->f->scheduled[n->gvn];

    TB_Node* tup = n;
    if (IS_PROJ(n)) { tup = n->inputs[0]; }

    bool entry_block = block == &ctx->cfg.blocks[0];
    int t = (entry_block && tup->type == TB_ROOT) || set_get(&block->live_in, n->gvn) ? 0 : ra->order[n->gvn] - 1;

    if (skip_projs) {
        t++;

        size_t cnt = aarray_length(block->items);
        while (t < cnt && (IS_PROJ(block->items[t]) || block->items[t]->type == TB_PHI || block->items[t]->type == TB_MACH_FRAME_PTR)) {
            t++;
        }
    }

    return (SlotIndex){ block, t };
}

static SlotIndex find_slot_index_end(Ctx* ctx, Rogers* restrict ra, TB_Node* n) {
    // find endpoints
    size_t i = ctx->bb_count;
    while (i--) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        if (set_get(&bb->live_in, n->gvn)) {
            int t = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, bb, n, n->gvn) - 1;
            return (SlotIndex){ bb, t };
        }
    }

    TB_BasicBlock* bb = ctx->f->scheduled[n->gvn];
    int t = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, bb, n, n->gvn) - 1;
    return (SlotIndex){ bb, t };
}

static TB_BasicBlock* sched_spill_site(TB_BasicBlock* early, TB_BasicBlock* late) {
    TB_BasicBlock* best = late;
    for (;;) {
        if (late->freq < best->freq) {
            best = late;
        }
        if (late == early) { break; }
        late = late->dom;
    }
    return best;
}

static SlotIndex skip_projs_and_weirdos(SlotIndex s) {
    size_t i = s.order + 1, cnt = aarray_length(s.bb->items);
    while (i < cnt && (IS_PROJ(s.bb->items[i]) || s.bb->items[i]->type == TB_PHI || s.bb->items[i]->type == TB_MACH_FRAME_PTR)) {
        i++;
    }
    s.order = i;
    return s;
}

static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node) {
    size_t extra = extra_bytes(n);
    TB_Function* f = ctx->f;
    TB_Node* root = f->root_node;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    RegMask* src_mask = NULL;
    if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
        src_mask = cpy->use;
    }

    RegMask* def_mask = ctx->constraint(ctx, n, NULL);

    // aggressive reload
    double base_bias = ctx->vregs[ctx->vreg_map[n->gvn]].spill_bias;
    for (size_t i = 0; i < n->user_count;) {
        TB_Node* use_n = USERN(&n->users[i]);
        int use_i      = USERI(&n->users[i]);

        // it's never in[0] lmao
        assert(use_i != 0);

        VReg* reload_vreg;
        TB_Node* remat = NULL;
        if (use_n->type == TB_MACH_COPY && src_mask) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (!reg_mask_is_stack(cpy->def) || !reg_mask_is_stack(src_mask)) {
                // gotta be in separate coalesced groups
                int leader = uf_find(ra->uf, ra->uf_len, use_n->gvn);
                if (uf_find(ra->uf, ra->uf_len, n->gvn) != leader) {
                    remat    = use_n;
                    cpy->use = src_mask;

                    // schedule the split right before use
                    set_input(f, use_n, n->inputs[1], use_i);
                    continue;
                }
            }
        }

        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        // remat per use site
        remat = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
        memcpy(remat->extra, n->extra, extra);
        FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
            remat->inputs[j] = n->inputs[j];
            add_user(f, remat, n->inputs[j], j);
        }

        if (use_n->type == TB_PHI) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, use_n->inputs[0], use_i - 1);
            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            int pos = aarray_length(pred_bb->items);
            TB_Node* last = pred_bb->items[pos - 1];
            if (IS_PROJ(last)) { last = last->inputs[0]; }
            if (tb_node_is_terminator(last)) {
                pos--;

                while (pos > 0 && pred_bb->items[pos] != pred_bb->start && IS_PROJ(pred_bb->items[pos])) {
                    pos--;
                }
            }

            // place at the end of the pred BB to the phi, basically the latest point
            rogers_insert_op(ctx, pred_bb - ctx->cfg.blocks, remat, pos);

            // phis hard coalesce
            int vreg_id = ctx->vreg_map[use_n->gvn];
            reload_vreg = &ctx->vregs[vreg_id];
            aarray_insert(ctx->vreg_map, remat->gvn, vreg_id);
        } else {
            // schedule the split right before use
            tb__insert_before(ctx, ctx->f, remat, use_n);
            reload_vreg = tb__set_node_vreg(ctx, remat);
            reload_vreg->mask = def_mask;
        }

        // insert copy because the mask can't be represented
        RegMask* mask = tb__reg_mask_meet(ctx, in_mask, reload_vreg->mask);
        if (mask == &TB_REG_EMPTY) {
            reload_vreg->reg_width = tb__reg_width_from_dt(reload_vreg->mask->class, remat->dt);
            reload_vreg->spill_bias = 1e6;

            TB_Node* copy = tb_alloc_node(f, TB_MACH_COPY, remat->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, copy, remat, 1);
            TB_NODE_SET_EXTRA(copy, TB_NodeMachCopy, .def = in_mask, .use = def_mask);

            tb__insert_before(ctx, ctx->f, copy, use_n);
            mask = in_mask;
            remat = copy;

            VReg* new_vreg = tb__set_node_vreg(ctx, copy);
            new_vreg->mask = in_mask;
            reload_vreg = new_vreg;
        }

        set_input(f, use_n, remat, use_i);

        reload_vreg->hint_vreg = ctx->vreg_map[use_n->gvn];
        reload_vreg->mask = mask;
        reload_vreg->reg_width = tb__reg_width_from_dt(mask->class, remat->dt);
        reload_vreg->spill_bias = 1e6;
        TB_ASSERT(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu:    use (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(&f->tmp_arena, sp);

    if (kill_node) {
        // delete the original def
        ctx->vregs[ctx->vreg_map[n->gvn]].uses -= 1;
        ctx->vreg_map[n->gvn] = 0;
        tb__remove_node(ctx, f, n);
        tb_kill_node(f, n);
    }
}

