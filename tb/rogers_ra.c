// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

#define FOREACH_SET(it, set) \
FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    uint64_t key; // key
    int last_use; // val
} InactiveCacheEntry;

typedef struct {
    TB_Node* target;

    // this is who was being allocated when we failed, he
    // wants wants your register.
    TB_Node* failed;

    TB_Node* evict;

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

static bool just_spilled[1000];

static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

static void split_range(Ctx* ctx, Rogers* restrict ra, TB_Node* a, TB_Node* b, size_t old_node_count, bool avoid_b);
static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node);
static void better_spill_range(Ctx* ctx, Rogers* restrict ra, TB_Node* to_spill, size_t old_node_count);
static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn);

static bool rogers_is_fixed(Ctx* ctx, Rogers* ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
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

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    double cost = get_spill_cost(ctx, vreg);
    printf("# V%-4"PRIdPTR" cost=%.2f ", vreg - ctx->vregs, cost);
    tb__print_regmask(vreg->mask);
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
    if (vreg->area <= 1) {
        // printf("spill_cost(%zu) = %f\n", vreg - ctx->vregs, INFINITY);
        return INFINITY;
    }

    // printf("spill_cost(%zu) = %f\n", vreg - ctx->vregs, vreg->spill_cost / vreg->area);
    return vreg->spill_cost / vreg->area;
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

        if (!cfg_is_terminator(bb->end)) {
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

    // if (is_proj(aa)) { aa = aa->inputs[0]; }
    // if (is_proj(bb)) { bb = bb->inputs[0]; }

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

static void clean_bb_ordinals(Ctx* ctx, Rogers* ra, bool* dirty_bb) {
    FOR_N(i, 0, ctx->bb_count) {
        if (!dirty_bb[i]) { continue; }
        dirty_bb[i] = false;

        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            ra->order[n->gvn] = 1 + j;
        }
    }
}

static void clean_single_bb_ordinals(Ctx* ctx, Rogers* ra, TB_BasicBlock* bb) {
    int timeline = 1;
    for (size_t j = 0; j < aarray_length(bb->items); j++) {
        TB_Node* n = bb->items[j];

        ra->gvn2node[n->gvn] = n;
        ra->order[n->gvn] = timeline++;
    }
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
            size_t j = 0; // we do insert things while iterating
            for (; j < aarray_length(bb->items); j++) {
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
            spill_entire_lifetime(ctx, vreg, mask, vreg->n, true);

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
            RegMask* rm = ctx->constraint(ctx, n, NULL);
            int x = uf_find(ra.uf, ra.uf_len, n->gvn);
            FOR_N(k, 1, n->input_count) {
                // interfere against everything in the set
                TB_Node* in = n->inputs[k];
                if (!rogers_can_coalesce(ctx, &ra, n, in)) {
                    TB_OPTDEBUG(REGALLOC)(printf("PHI %%%u (-> %%%u) has self-conflict\n", n->gvn, in->gvn));

                    TB_Node* move = rogers_hard_split(ctx, &ra, n, in, rm, vreg_id);
                    set_input(f, n, move, k);

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
                    TB_Node* last = pred_bb->items[aarray_length(pred_bb->items) - 1];
                    rogers_insert_op(ctx, pred_bb - ctx->cfg.blocks, move, aarray_length(pred_bb->items) - (last == in ? 0 : 1));
                    changes = true;
                }

                TB_ASSERT(n->inputs[k]->gvn < ra.uf_len);

                // hard coalesce with direct input
                int y = uf_find(ra.uf, ra.uf_len, n->inputs[k]->gvn);
                rogers_coalesce(ctx, &ra, x, y, n, n->inputs[k]);
            }
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

    // compute dominance frontiers for later SSA splitting work
    ArenaArray(int)* df = ra.df = tb_arena_alloc(ra.arena, ctx->bb_count * sizeof(ArenaArray(int)));
    FOR_N(bb_id, 0, ctx->bb_count) {
        df[bb_id] = NULL;
    }

    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        TB_Node* bb_node = bb->start;
        int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
        if (pred_count >= 2) {
            FOR_N(j, 0, pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
                TB_BasicBlock* runner = f->scheduled[pred->gvn];
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                while (runner != bb->dom) {
                    // add to frontier
                    int runner_id = runner - ctx->cfg.blocks;
                    if (df[runner_id] == NULL) {
                        df[runner_id] = aarray_create(ra.arena, int, 2);
                    }
                    aarray_push(df[runner_id], bb_id);
                    runner = runner->dom;
                }
            }
        }
    }

    int starting_spills = ctx->num_regs[REG_CLASS_STK];
    ra.num_spills = starting_spills;

    TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, f->node_count), printf("\n"));

    int rounds = 0;
    cuikperf_region_start("main loop", NULL);
    for (;;) {
        TB_ArenaSavepoint sp = tb_arena_save(arena);

        rounds++;
        TB_OPTDEBUG(REGALLOC)(printf("# ========= Round %d =========\n", rounds));
        TB_ASSERT(rounds < 100);

        cuikperf_region_start("round", NULL);
        bool done = allocate_loop(ctx, &ra, arena);
        cuikperf_region_end();

        if (done) {
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

            ctx->num_spills += ra.num_spills - starting_spills;
            cuikperf_region_end();
            // printf("ROUNDS: %d\n", rounds);
            return;
        }

        // gonna be modding shit too much for it to matter.
        ra.inactive_cache = NULL;
        cuikperf_region_start("insert spills", NULL);
        memset(just_spilled, 0, 1000);

        bool* dirty_bb = tb_arena_alloc(arena, ctx->bb_count * sizeof(bool));
        FOR_N(i, 0, ctx->bb_count) {
            dirty_bb[i] = false;
        }

        TB_OPTDEBUG(REGALLOC5)(printf("=== SPILLING ===\n"));

        size_t old_node_count = ctx->f->node_count;
        FOR_N(i, 0, dyn_array_length(ra.splits)) {
            SplitDecision split = ra.splits[i];
            TB_OPTDEBUG(REGALLOC3)(printf("#\n# %%%u must be split around %%%u%s\n", split.target->gvn, split.failed->gvn, split.clobber ? "" : "'s entire lifetime"));

            TB_Node* to_spill = split.target;
            if (ctx->vreg_map[to_spill->gvn] == 0) {
                continue;
            }

            // failed to color a may_spill?
            VReg* v = &ctx->vregs[ctx->vreg_map[to_spill->gvn]];
            if (v->mask->may_spill) {
                TB_OPTDEBUG(REGALLOC3)(printf("# * %%%u is now a proper spill\n", split.target->gvn));

                v->spill_cost = INFINITY;
                v->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                v->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, to_spill->dt);

                if (to_spill->type == TB_MACH_COPY) {
                    TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(to_spill);
                    cpy->def = v->mask;

                    FOR_USERS(u, to_spill) {
                        if (USERN(u)->type == TB_MACH_COPY) {
                            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(USERN(u));
                            cpy->use = v->mask;
                        }
                    }
                }
                continue;
            }

            // if something spills, ideally it doesn't try that again, we
            // also want to invalidate the cost to make it re-compute since we'll be
            // modding the graph.
            bool was_spilled = v->was_spilled;
            v->spill_cost = NAN;
            v->spill_bias += 1e6;
            v->was_spilled = true;

            TB_BasicBlock* single_bb = f->scheduled[to_spill->gvn];
            bool is_tuple = false;
            FOR_USERS(u, to_spill) {
                TB_BasicBlock* bb = f->scheduled[USERN(u)->gvn];
                if (is_proj(USERN(u))) {
                    is_tuple = true;
                }

                if (single_bb == NULL) { single_bb = bb; }
                else if (single_bb != bb) { single_bb = NULL; break; }
            }

            // shuffle as close to the single user as possible
            TB_Node* failed = split.failed;
            bool remat = can_remat(ctx, to_spill);
            if (remat && to_spill->user_count == 1 && !is_tuple) {
                TB_Node* use = USERN(&to_spill->users[0]);

                TB_BasicBlock* bb = f->scheduled[use->gvn];
                tb__remove_node(ctx, f, to_spill);
                ra.order[to_spill->gvn] = tb__insert_before(ctx, ctx->f, to_spill, use);
                clean_single_bb_ordinals(ctx, &ra, bb);

                TB_OPTDEBUG(REGALLOC3)(printf("#       shuffled %%%u right before %%%u\n", to_spill->gvn, use->gvn));
                continue;
            }

            // if we're at the last use before death AND we can move past it, we could
            // reschedule such that no spills are inserted.
            if (single_bb && remat && (set_get(&single_bb->live_in, failed->gvn) || f->scheduled[failed->gvn] == single_bb)) {
                int last_use = last_use_in_bb(ctx->cfg.blocks, f->scheduled, &ra, single_bb, failed, failed->gvn);
                if (!set_get(&single_bb->live_out, failed->gvn)) {
                    int first_use = INT_MAX;
                    FOR_USERS(u, to_spill) {
                        TB_Node* un = USERN(u);
                        if (un->type != TB_PHI) {
                            first_use = TB_MIN(first_use, ra.order[un->gvn]);
                        }
                    }
                    TB_ASSERT(first_use < INT_MAX);

                    if (first_use > last_use && single_bb->items[first_use - 1] != to_spill) {
                        TB_Node* at = single_bb->items[first_use - 1];
                        tb__remove_node(ctx, f, to_spill);
                        ra.order[to_spill->gvn] = tb__insert_before(ctx, ctx->f, to_spill, at);

                        clean_single_bb_ordinals(ctx, &ra, single_bb);
                        TB_OPTDEBUG(REGALLOC3)(printf("#       shuffled it past death of %%%u (right before %%%u)\n", failed->gvn, at->gvn));

                        // TB_ASSERT(!interfere(ctx, &ra, to_spill, failed));
                        continue;
                    }
                }
            }

            if (!was_spilled && split.clobber && to_spill->type == TB_MACH_COPY && fixed_reg_mask(v->mask) >= 0) {
                // modify the original copy to fit into an unclobbered reg, then insert new hard splits
                RegMask* clobber = split.clobber;
                TB_ASSERT(ctx->num_regs[clobber->class] < 64);

                uint64_t total_mask = UINT64_MAX >> (64 - ctx->num_regs[clobber->class]);
                RegMask* dual = intern_regmask(ctx, clobber->class, true, ~clobber->mask[0] & total_mask);

                #if 0
                tb__print_regmask(split.clobber);
                printf("\n");
                tb__print_regmask(dual);
                printf("\n");
                #endif

                TB_NODE_GET_EXTRA_T(to_spill, TB_NodeMachCopy)->def = dual;
                v->mask = dual;
            } else if (remat) {
                TB_OPTDEBUG(REGALLOC3)(printf("\x1b[33m#   rematerialized %%%u\x1b[0m\n", to_spill->gvn));

                // we only aggressively rematerialize if we've already tried a simpler split
                TB_BasicBlock** scheduled = ctx->f->scheduled;
                FOR_USERS(u, to_spill) {
                    TB_Node* use_n = USERN(u);
                    int use_i      = USERI(u);

                    if (use_n->gvn < old_node_count && !is_proj(use_n) && use_i < use_n->input_count) {
                        TB_BasicBlock* bb = scheduled[use_n->gvn];
                        dirty_bb[bb - ctx->cfg.blocks] = true;
                    }
                }

                rogers_remat(ctx, &ra, to_spill, false);
                rogers_uncoalesce(ctx, &ra, to_spill->gvn);

                FOR_N(j, 1, to_spill->input_count) {
                    // invalidate stretched input's coalescing
                    TB_Node* in = to_spill->inputs[j];
                    if (in == NULL) { continue; }

                    // if it's coalesced with it's users, we might need to split it
                    int vreg_id = ctx->vreg_map[in->gvn];
                    for (size_t k = 0; k < in->user_count;) {
                        TB_Node* un = USERN(&in->users[k]);
                        int ui      = USERI(&in->users[k]);

                        int user_vreg_id = ctx->vreg_map[un->gvn];
                        if (user_vreg_id == vreg_id && interfere(ctx, &ra, in, un)) {
                            RegMask* def_rm = constraint_in(ctx, un, ui);
                            RegMask* use_rm = ctx->constraint(ctx, in, NULL);

                            rogers_uncoalesce(ctx, &ra, un->gvn);

                            // separate "in" from whatever set it shares with "un"
                            VReg* new_vreg = tb__set_node_vreg(ctx, un);
                            new_vreg->mask = def_rm;
                            new_vreg->reg_width = tb__reg_width_from_dt(def_rm->class, in->dt);
                            new_vreg->was_spilled = true;
                            new_vreg->spill_bias = 1e6;

                            TB_Node* copy = rogers_hard_split(ctx, &ra, un, in, def_rm, new_vreg - ctx->vregs);
                            set_input(ctx->f, un, copy, ui);
                            tb__insert_before(ctx, ctx->f, copy, un);

                            // hard coalesce with direct input
                            int x = uf_find(ra.uf, ra.uf_len, un->gvn);
                            int y = uf_find(ra.uf, ra.uf_len, copy->gvn);
                            rogers_coalesce(ctx, &ra, x, y, un, copy);

                            TB_BasicBlock* use_bb = f->scheduled[un->gvn];
                            dirty_bb[use_bb - ctx->cfg.blocks] = true;
                        } else {
                            k += 1;
                        }
                    }
                }

                TB_BasicBlock* def_bb = f->scheduled[to_spill->gvn];
                dirty_bb[def_bb - ctx->cfg.blocks] = true;

                // delete the original def
                ctx->vregs[ctx->vreg_map[to_spill->gvn]].uses -= 1;
                ctx->vreg_map[to_spill->gvn] = 0;
                tb__remove_node(ctx, ctx->f, to_spill);
                tb_kill_node(ctx->f, to_spill);

                // recompute order for dirty blocks
                clean_bb_ordinals(ctx, &ra, dirty_bb);
                continue;
            }

            // spilling a PHI requires a bit of goop
            if (to_spill->type == TB_PHI && was_spilled) {
                RegMask* phi_rm = NULL;
                RegMask* spill_rm = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                FOR_USERS(u, to_spill) {
                    RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                    phi_rm = tb__reg_mask_meet(ctx, phi_rm, in_mask);
                }

                int old_vreg_id = ctx->vreg_map[to_spill->gvn];

                // assign new phi vreg
                VReg* new_vreg = tb__set_node_vreg(ctx, to_spill);
                new_vreg->mask = spill_rm;
                new_vreg->reg_width = tb__reg_width_from_dt(spill_rm->class, to_spill->dt);
                new_vreg->uses = to_spill->input_count;
                int phi_vreg_id = new_vreg - ctx->vregs;

                size_t new_len = tb_next_pow2(f->node_count + to_spill->input_count + 16);
                rogers_resize_uf(ctx, &ra, new_len);

                // everyone in the phi's set will be separated from each other
                rogers_uncoalesce(ctx, &ra, to_spill->gvn);

                // remove refs (1 + phi paths in)
                ctx->vregs[old_vreg_id].uses -= to_spill->input_count;

                // any non-self referential edges will be split from the phi
                FOR_N(j, 1, to_spill->input_count) {
                    TB_Node* in = to_spill->inputs[j];
                    if (in == to_spill) { continue; }

                    // RegMask* def_rm = constraint_in(ctx, un, ui);
                    RegMask* in_rm = ctx->constraint(ctx, in, NULL);
                    rogers_uncoalesce(ctx, &ra, in->gvn);

                    // separate "in" from whatever set it shares with "un"
                    VReg* new_vreg = tb__set_node_vreg(ctx, in);
                    new_vreg->mask = in_rm;
                    new_vreg->reg_width = tb__reg_width_from_dt(in_rm->class, in->dt);

                    TB_Node* copy = tb_alloc_node(f, TB_MACH_COPY, in->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, copy, in, 1);
                    set_input(f, to_spill, copy, j);
                    TB_NODE_SET_EXTRA(copy, TB_NodeMachCopy, .def = spill_rm, .use = in_rm);
                    aarray_insert(ctx->vreg_map, copy->gvn, phi_vreg_id);
                    tb__insert_after(ctx, f, copy, in);

                    // hard coalesce with direct input
                    int x = uf_find(ra.uf, ra.uf_len, to_spill->gvn);
                    int y = uf_find(ra.uf, ra.uf_len, copy->gvn);
                    rogers_coalesce(ctx, &ra, x, y, to_spill, copy);
                }
                v = &ctx->vregs[phi_vreg_id];

                if (1) {
                    // add copy after phi to users
                    TB_Node* copy = tb_alloc_node(f, TB_MACH_COPY, to_spill->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, copy, to_spill, 1);
                    TB_NODE_SET_EXTRA(copy, TB_NodeMachCopy, .def = phi_rm, .use = spill_rm);
                    aarray_insert(ctx->vreg_map, copy->gvn, old_vreg_id);

                    tb__insert_after(ctx, f, copy, to_spill);

                    for (size_t k = 0; k < to_spill->user_count;) {
                        TB_Node* un = USERN(&to_spill->users[k]);
                        int ui      = USERI(&to_spill->users[k]);

                        int user_vreg_id = ctx->vreg_map[un->gvn];
                        TB_ASSERT(user_vreg_id != old_vreg_id);

                        set_input(f, un, copy, ui);
                        k += 1;
                    }
                }

                // recompute order for dirty blocks
                clean_bb_ordinals(ctx, &ra, dirty_bb);
            } else if (split.failed->user_count > 0) { // earlier splits might've killed whatever we're splitting around
                split_range(ctx, &ra, split.target, split.failed, old_node_count, split.clobber);
            }
            just_spilled[split.target->gvn] = 1;
        }
        TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, old_node_count));
        dyn_array_clear(ra.splits);
        cuikperf_region_end();

        // reset assignments
        FOR_N(i, 1, aarray_length(ctx->vregs)) {
            if (!rogers_is_fixed(ctx, &ra, i) && ctx->vregs[i].assigned >= 0) {
                ctx->vregs[i].class = 0;
                ctx->vregs[i].assigned = -1;
            }
        }

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
    int l = 0;
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
        l = ra->order[bb->end->gvn];
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
    ra->active[vreg->class][vreg->assigned] = gvn;
}

static void unmark_active(Ctx* restrict ctx, Rogers* restrict ra, int gvn) {
    TB_ASSERT(ctx->vreg_map[gvn]);
    VReg* other = &ctx->vregs[ctx->vreg_map[gvn]];
    ra->active[other->class][other->assigned] = 0;
}

// picks the best candidate for spilling, never the attempted_n itself. if it's the
// case that the attempted_n is the better spill then we'll use the "best candidate"
// for spilling attempted_n's node to split around.
static TB_Node* choose_best_spill(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* attempted_n) {
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

    int best_spill = -1;
    float best_score = INFINITY;
    FOR_REV_N(i, 0, dyn_array_length(ra->potential_spills)) {
        int gvn = ra->potential_spills[i];

        TB_ASSERT(ctx->vreg_map[gvn] > 0);
        VReg* vreg  = &ctx->vregs[ctx->vreg_map[gvn]];
        float score = rogers_get_spill_cost(ctx, ra, vreg);

        // we'll only spill things which can make aggressive forward progress
        if (attempted_vreg->was_spilled && !within_reg_mask(useful_mask, vreg->assigned)) {
            continue;
        }

        // bias higher assignments such that we try to pick smaller regs when there's ties
        // score += vreg->assigned*0.0001f;

        if (score < best_score) {
            if (best_spill >= 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is a better spill! V%d (%f is better than %f)\n", gvn, best_spill, score, best_score));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is... one of the spills of all time! %f\n", gvn, score));
            }
            best_score = score;
            best_spill = gvn;
        } else {
            TB_OPTDEBUG(REGALLOC)(printf("#     %%%u is a bad pick! %f\n", gvn, score));
        }
    }

    cuikperf_region_end();
    TB_ASSERT(best_spill >= 0);
    return ra->gvn2node[best_spill];
}

static bool allocate_reg(Ctx* ctx, Rogers* ra, TB_Node* n) {
    int vreg_id = ctx->vreg_map[n->gvn];
    VReg* vreg = &ctx->vregs[vreg_id];

    // if we've spilled we shouldn't be able to wake up again.
    if (vreg_id == 0 || set_get(&ra->been_spilled, n->gvn)) {
        return false;
    }

    if (set_get(&ra->future_active, n->gvn)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   woke up %%%u\n", n->gvn));

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
    TB_OPTDEBUG(REGALLOC)(printf("#   "));

    cuikperf_region_start("A", NULL);
    #if NDEBUG && USE_INTRIN && CUIK__IS_X64
    __m128i* ra_active = (__m128i*) ra->active[def_class];
    FOR_N(i, 0, (num_regs + 15) / 16) {
        __m128i a = _mm_loadu_si128(ra_active++);
        __m128i b = _mm_loadu_si128(ra_active++);
        __m128i c = _mm_loadu_si128(ra_active++);
        __m128i d = _mm_loadu_si128(ra_active++);

        uint32_t bits = 0;
        bits |= _mm_movemask_ps(_mm_cmplt_epi32(_mm_set1_epi32(0), a));
        bits |= _mm_movemask_ps(_mm_cmplt_epi32(_mm_set1_epi32(0), b)) << 4;
        bits |= _mm_movemask_ps(_mm_cmplt_epi32(_mm_set1_epi32(0), c)) << 8;
        bits |= _mm_movemask_ps(_mm_cmplt_epi32(_mm_set1_epi32(0), d)) << 12;

        ra_mask[(i * 16) / 64ull] |= bits;
    }
    #else
    FOR_N(i, 0, num_regs) {
        if (ra->active[def_class][i] > 0) {
            TB_OPTDEBUG(REGALLOC)(printf("%%%u -- ", ra->active[def_class][i]), print_reg_name(def_class, i), printf("; "));
        }

        ra_mask[i / 64ull] |= ra->active[def_class][i] > 0 ? (1ull << (i % 64ull)) : 0;
    }
    #endif
    cuikperf_region_end();

    cuikperf_region_start("I", NULL);
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

        cuikperf_region_start("i", NULL);
        int other_assigned = other_vreg->assigned;
        FOR_N(j, 0, cnt) {
            if (interfere(ctx, ra, arr[j], other)) {
                TB_ASSERT(other_vreg->assigned >= 0);
                if (within_reg_mask(mask, other_vreg->assigned)) {
                    TB_OPTDEBUG(REGALLOC)(printf("%%%u -- ", other->gvn), print_reg_name(def_class, other_assigned), printf("; "));
                }

                dyn_array_put(ra->potential_spills, i);
                ra_mask[other_assigned / 64ull] |= (1ull << (other_assigned % 64ull));
                break;
            }
        }
        cuikperf_region_end();
    }
    TB_OPTDEBUG(REGALLOC)(printf("\n"));
    cuikperf_region_end();

    int hint_vreg = vreg->hint_vreg;
    int hint_reg = hint_vreg > 0
        && ctx->vregs[hint_vreg].class == mask->class
        ?  ctx->vregs[hint_vreg].assigned
        :  -1;

    if (hint_reg >= 0 && (ra_mask[hint_reg / 64ull] & (1ull << (hint_reg % 64ull))) == 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(def_class, hint_reg), printf(" (HINTED)\n"));

        vreg->class    = def_class;
        vreg->assigned = hint_reg;
        mark_active(ctx, ra, n->gvn);
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

        SplitDecision s = { 0 };
        s.failed = n;
        s.target = choose_best_spill(ctx, ra, n);

        // if spilling ourselves is better than spilling the best candidate, we'll do that
        // and we'll split around him.
        float us_cost   = rogers_get_spill_cost(ctx, ra, node_vreg(ctx, s.failed));
        float them_cost = rogers_get_spill_cost(ctx, ra, node_vreg(ctx, s.target));
        if (us_cost < them_cost) {
            SWAP(float, us_cost, them_cost);
            SWAP(TB_Node*, s.target, s.failed);
        }

        if (set_get(&ra->been_spilled, s.target->gvn)) {
            TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mALREADY SPILLING %%%u (FOR %%%u)\x1b[0m\n", s.target->gvn, s.failed->gvn));
        } else {
            TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mGONNA SPILL %%%u (FOR %%%u)\x1b[0m\n", s.target->gvn, s.failed->gvn));
            set_put(&ra->been_spilled, s.target->gvn);
            dyn_array_put(ra->splits, s);
        }

        if (s.target == n) {
            // kill this node for now
            uint32_t best_spill = s.target->gvn;
            set_put(&ra->been_spilled, best_spill);
            unmark_active(ctx, ra, best_spill);
            set_remove(&ra->live, best_spill);
            set_remove(&ra->future_active, best_spill);

            TB_OPTDEBUG(REGALLOC5)(printf("#       assigned UNCOLORED\n"));
        } else {
            VReg* evicted = &ctx->vregs[ctx->vreg_map[s.target->gvn]];

            vreg->class = evicted->class;
            vreg->assigned = evicted->assigned;
            mark_active(ctx, ra, n->gvn);

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

static void expire_interval(Ctx* ctx, Rogers* restrict ra, uint32_t gvn, size_t bb_i);
static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    cuikperf_region_start("init", NULL);
    ra->inactive_cache = tb_arena_alloc(arena, 128 * sizeof(InactiveCacheEntry));
    memset(ra->inactive_cache, 0, 128 * sizeof(InactiveCacheEntry));
    cuikperf_region_end();

    ra->been_spilled  = set_create_in_arena(arena, ctx->f->node_count);
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

            #if TB_OPTDEBUG_REGALLOC4
            printf("# BB%zu\n", i);
            #endif

            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                // expire
                if (array[n->gvn] >= 0) {
                    int last_gvn = stack[aarray_length(stack) - 1];
                    aarray_remove(stack, array[n->gvn]);
                    array[last_gvn] = array[n->gvn];
                    array[n->gvn] = -1;
                }

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

                #if TB_OPTDEBUG_REGALLOC4
                printf("# ");
                #endif

                // accumulate area (don't do so on projections and temps)
                if (n->type != TB_MACH_TEMP && !is_proj(n)) {
                    aarray_for(k, stack) {
                        TB_ASSERT(ctx->vreg_map[stack[k]] > 0);
                        VReg* v = &ctx->vregs[ctx->vreg_map[stack[k]]];
                        v->area += freq;

                        #if TB_OPTDEBUG_REGALLOC4
                        if (just_spilled[stack[k]]) {
                            printf("\x1b[32m%%%u\x1b[0m ", stack[k]);
                        } else {
                            printf("%%%u ", stack[k]);
                        }
                        #endif
                    }
                }

                #if TB_OPTDEBUG_REGALLOC4
                printf("\n");
                #endif
            }
        }
        tb_arena_restore(arena, sp);
    }

    if (dyn_array_length(ra->splits) > 0) {
        return false;
    }

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
                    if (fixed_reg_mask(ctx->vregs[vreg_id].mask) >= 0) {
                        // must account for interference against any of the previously defined regs
                        cuikperf_region_start("step", NULL);
                        allocate_reg(ctx, ra, n);

                        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
                        ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));

                        // put the pre-colored regs to rest
                        TB_ASSERT(n->gvn < ctx->f->node_count);
                        unmark_active(ctx, ra, n->gvn);
                        set_put(&ra->future_active, n->gvn);

                        if (set && aarray_length(set) > 0) {
                            aarray_for(i, set) if (set[i] != n) {
                                // put the pre-colored regs to rest
                                TB_ASSERT(set[i]->gvn < ctx->f->node_count);
                                set_put(&ra->future_active, set[i]->gvn);
                            }
                        }
                        cuikperf_region_end();
                    }
                }
            }
        }
    }

    if (dyn_array_length(ra->splits) > 0) {
        return false;
    }

    TB_OPTDEBUG(REGALLOC5)(printf("=== ASSIGN REGS ===\n"));

    size_t uf_len = ctx->f->node_count;
    FOR_REV_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        TB_OPTDEBUG(REGALLOC5)(printf("# ========= BB%zu =========\n", bb_id));

        cuikperf_region_start("BB", NULL);
        // expire intervals for block (was live, isn't now)
        FOREACH_SET(j, ra->live) if (!set_get(live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                expire_interval(ctx, ra, j, bb_id);
            }
        }
        cuikperf_region_end();

        // start intervals
        FOREACH_SET(j, *live_out) if (!set_get(&ra->live, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                allocate_reg(ctx, ra, ra->gvn2node[j]);
            }
        }

        // it's a backwards walk so...
        //   each use will try to place the node into the active set.
        //   each def will either put the node to sleep or kill it.
        FOR_REV_N(j, 0, aarray_length(bb->items)) {
            TB_Node* n = bb->items[j];
            if (0) { // if (!is_proj(n) || n->inputs[0] != f->root_node) {
                continue;
            }

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

                expire_interval(ctx, ra, n->gvn, bb_id);

                /*FOR_USERS(u, to_spill) {
                    TB_BasicBlock* bb = f->scheduled[USERN(u)->gvn];
                    if (is_proj(USERN(u))) {
                        is_tuple = true;
                    }
                }*/
            }

            // if anything in the active set interferes with
            // the kill set, we need to spill around this op.
            RegMask** kills = ctx->ins;
            int kill_count = ctx->constraint_kill(ctx, n, kills);
            if (kill_count) {
                #if TB_OPTDEBUG_REGALLOC5
                printf("#\n# ");
                tb_print_dumb_node(NULL, n);
                printf("\n");
                #endif
            }

            FOR_N(k, 0, kill_count) {
                TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mCLOBBERING "), tb__print_regmask(kills[k]), printf("\x1b[0m\n"));

                int kill_class = kills[k]->class;
                int nr = ctx->num_regs[kills[k]->class];
                int* active = ra->active[kills[k]->class];
                BITS64_FOR(l, kills[k]->mask, nr) {
                    // TB_ASSERT(active[l] == 0);

                    #if 1
                    if (active[l] > 0) {
                        int in_use = active[l];
                        int in_use_vreg_id = ctx->vreg_map[in_use];

                        if (!set_get(&ra->been_spilled, in_use)) {
                            if (ctx->vregs[in_use_vreg_id].mask->may_spill) {
                                TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mGONNA SPILL %%%u (CLOBBERED BY %%%u)\x1b[0m\n", in_use, n->gvn));
                                set_put(&ra->been_spilled, in_use);

                                SplitDecision s = { .target = ra->gvn2node[in_use], .failed = n, .clobber = kills[k] };
                                dyn_array_put(ra->splits, s);
                            } else {
                                int best_spill = in_use;
                                double best_cost = rogers_get_spill_cost(ctx, ra, &ctx->vregs[in_use_vreg_id]);

                                TB_Node* in_use_n = ra->gvn2node[in_use];

                                // if we can split an existing cheaper VReg (which isn't clobbered)
                                // around ourselves, we can steal its register.
                                double split_bias = 1.0;
                                FOR_N(i, 0, ctx->num_regs[kill_class]) {
                                    uint32_t gvn = active[i];
                                    if (gvn == 0) {
                                        continue;
                                    }

                                    int other_vreg_id = ctx->vreg_map[gvn];
                                    VReg* other_vreg = &ctx->vregs[other_vreg_id];

                                    TB_ASSERT(other_vreg_id > 0);
                                    if (other_vreg->class == kill_class && !within_reg_mask(kills[k], other_vreg->assigned)) {
                                        double cost = rogers_get_spill_cost(ctx, ra, other_vreg) + split_bias;

                                        // two phis nodes from the same header aren't
                                        // likely to make progress splitting
                                        TB_Node* other = ra->gvn2node[gvn];
                                        if (other->type == TB_PHI && in_use_n->type == TB_PHI && in_use_n->inputs[0] == other->inputs[0]) {
                                            cost += 1e6;
                                        }

                                        if (cost < best_cost) {
                                            TB_OPTDEBUG(REGALLOC)(printf("#     %%%u (", gvn), print_reg_name(other_vreg->class, other_vreg->assigned), printf(") is a good eviction! %f\n", score));

                                            best_spill = gvn;
                                            best_cost  = cost;
                                        } else {
                                            TB_OPTDEBUG(REGALLOC)(printf("#     %%%u (", gvn), print_reg_name(other_vreg->class, other_vreg->assigned), printf(") is a bad eviction! %f\n", score));
                                        }
                                    }
                                }

                                SplitDecision s = { 0 };
                                if (best_spill == in_use) {
                                    // if we're the best thing to split, we split around the "killer"
                                    s.target = in_use_n;
                                    s.failed = n;
                                    s.clobber = kills[k];
                                    TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mGONNA SPLIT %%%u AROUND %%%u\x1b[0m\n", s.target->gvn, s.failed->gvn));
                                } else {
                                    // we're gonna evict someone
                                    s.target = ra->gvn2node[best_spill];
                                    s.failed = in_use_n;
                                    TB_OPTDEBUG(REGALLOC5)(printf("#       \x1b[33mGONNA SPILL %%%u TO AVOID CLOBBERING %%%u\x1b[0m\n", s.target->gvn, s.failed->gvn));
                                }

                                if (!set_get(&ra->been_spilled, s.target->gvn)) {
                                    set_put(&ra->been_spilled, s.target->gvn);
                                    dyn_array_put(ra->splits, s);
                                }
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
static void expire_interval(Ctx* ctx, Rogers* restrict ra, uint32_t gvn, size_t bb_i) {
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
    if (is_proj(n)) { tup = n->inputs[0]; }

    bool entry_block = block == &ctx->cfg.blocks[0];
    int t = (entry_block && tup->type == TB_ROOT) || set_get(&block->live_in, n->gvn) ? 0 : ra->order[n->gvn] - 1;

    if (skip_projs) {
        t++;

        size_t cnt = aarray_length(block->items);
        while (t < cnt && (is_proj(block->items[t]) || block->items[t]->type == TB_PHI || block->items[t]->type == TB_MACH_FRAME_PTR)) {
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

// split A into two "in-register" halves.
static void split_range(Ctx* ctx, Rogers* restrict ra, TB_Node* a, TB_Node* b, size_t old_node_count, bool avoid_b) {
    SlotIndex a_start = find_slot_index_start(ctx, ra, a, false);
    SlotIndex b_start = find_slot_index_start(ctx, ra, b, !avoid_b);

    SlotIndex spill_site = a_start;
    if (slow_dommy2(a_start.bb, b_start.bb)) {
        // walk to lower freq BB
        TB_BasicBlock* best = b_start.bb;
        TB_BasicBlock* late = b_start.bb;
        for (;;) {
            if (late->freq < best->freq) {
                best = late;
            }
            if (late == a_start.bb) { break; }
            late = late->dom;
        }

        spill_site.bb = best;
        spill_site.order = 0;

        // spill right before B is born
        if (spill_site.bb == b_start.bb) {
            spill_site.order = b_start.order - 1;
        }

        // we must at least split in front of A
        if (spill_site.bb == a_start.bb) {
            spill_site.order = TB_MIN(spill_site.order, a_start.order);
        }
    }

    // skip any projections and other weirdos
    {
        size_t i = spill_site.order + 1, cnt = aarray_length(spill_site.bb->items);
        while (i < cnt && (is_proj(spill_site.bb->items[i]) || spill_site.bb->items[i]->type == TB_PHI || spill_site.bb->items[i]->type == TB_MACH_FRAME_PTR)) {
            i++;
        }
        spill_site.order = i;
    }

    // we just reload in the LCA of the uses of a (that way there's only one
    // reload site).
    bool aggro = false;
    SlotIndex reload_site;
    {
        TB_BasicBlock* reload_bb = tb_late_sched(ctx->f, &ctx->cfg, NULL, a);
        if (!slow_dommy2(spill_site.bb, reload_bb)) {
            reload_bb = spill_site.bb;
        }

        TB_Node* last = reload_bb->items[aarray_length(reload_bb->items) - 1];
        int min_order = spill_site.bb == reload_bb ? spill_site.order : 0;
        int reload_order = aarray_length(reload_bb->items) - (last == a ? 0 : 1);

        TB_BasicBlock** scheduled = ctx->f->scheduled;
        FOR_USERS(use, a) {
            TB_Node* un = USERN(use);
            if (is_proj(un)) { un = un->inputs[0]; }
            if (scheduled[un->gvn] == reload_bb) {
                // ignore the use at B, we're trying to reload *after* it
                if (avoid_b && un == b) {
                    continue;
                }
                // don't consider the uses before the spill
                if (ra->order[un->gvn] > min_order) {
                    reload_order = TB_MIN(reload_order, ra->order[un->gvn] - 1);
                }
            }
        }

        reload_site.bb = reload_bb;
        reload_site.order = reload_order;

        if (spill_site.bb == reload_site.bb && spill_site.order == reload_site.order) {
            aggro = true;
        }
    }

    #if TB_OPTDEBUG_REGALLOC3
    printf("Spill:  BB%zu (after %%%u)\n", spill_site.bb - ctx->cfg.blocks, spill_site.bb->items[spill_site.order - 1]->gvn);
    printf("Reload: BB%zu @ %%%u\n", reload_site.bb - ctx->cfg.blocks, reload_site.bb->items[reload_site.order]->gvn);

    rogers_dump_split(ctx, ra, spill_site.bb, a, b);
    if (spill_site.bb != reload_site.bb) {
        rogers_dump_split(ctx, ra, reload_site.bb, a, b);
    }
    #endif

    TB_Function* f = ctx->f;
    RegMask* a_def_mask = ctx->constraint(ctx, a, NULL);
    RegMask* b_def_mask = ctx->constraint(ctx, b, NULL);

    TB_ASSERT(avoid_b || a_def_mask->class == b_def_mask->class);
    // RegMask* spill_mask = ctx->mayspill_mask[a_def_mask->class];
    RegMask* spill_mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);

    TB_Node** defs = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(TB_Node*));
    FOR_N(i, 0, ctx->bb_count) {
        defs[i] = NULL;
    }

    TB_Node* new_b = NULL;
    if (!avoid_b && b != a) {
        RegMask* new_b_mask = NULL;
        for (size_t i = 0; i < b->user_count; i++) {
            TB_Node* use_n = USERN(&b->users[i]);
            int use_i      = USERI(&b->users[i]);
            if (!is_proj(use_n) && use_i > 0 && use_i < use_n->input_count) {
                RegMask* in_mask = constraint_in(ctx, use_n, use_i);
                new_b_mask = tb__reg_mask_meet(ctx, in_mask, new_b_mask);
            }
        }

        if (new_b_mask != a_def_mask) {
            if (b->user_count == 1 && USERN(&b->users[0])->type == TB_MACH_COPY) {
                // hoist the copy up
                new_b = USERN(&b->users[0]);

                TB_BasicBlock* old_bb = f->scheduled[new_b->gvn];
                int t = tb__remove_node(ctx, f, new_b);
                spill_site.order -= (spill_site.bb == old_bb && t <= spill_site.order);
                reload_site.order -= (reload_site.bb == old_bb && t <= reload_site.order);

                clean_single_bb_ordinals(ctx, ra, old_bb);
            } else {
                new_b = tb_alloc_node(f, TB_MACH_COPY, b->dt, 2, sizeof(TB_NodeMachCopy));
                for (size_t i = 0; i < b->user_count;) {
                    TB_Node* use_n = USERN(&b->users[i]);
                    int use_i      = USERI(&b->users[i]);
                    if (is_proj(use_n) || use_i == 0 || use_i >= use_n->input_count) {
                        i += 1;
                    } else {
                        set_input(f, use_n, new_b, use_i);
                    }
                }
                set_input(f, new_b, b, 1);

                TB_ASSERT(new_b_mask != &TB_REG_EMPTY);
                RegMask* meet = tb__reg_mask_meet(ctx, new_b_mask, a_def_mask);
                if (meet != &TB_REG_EMPTY) {
                    new_b_mask = meet;
                }

                TB_NODE_SET_EXTRA(new_b, TB_NodeMachCopy, .def = new_b_mask, .use = b_def_mask);

                VReg* new_b_vreg = tb__set_node_vreg(ctx, new_b);
                new_b_vreg->reg_width = tb__reg_width_from_dt(new_b_mask->class, new_b->dt);
                new_b_vreg->mask = new_b_mask;
            }

            // if A and B start at the same place, we split B at the start such that
            // both A and B have separate assignments until right after the spill.
            if (b_start.bb != spill_site.bb || b_start.order != spill_site.order) {
                // place new_b right after b
                int t = rogers_insert_op(ctx, b_start.bb - ctx->cfg.blocks, new_b, b_start.order);
                spill_site.order += (spill_site.bb == b_start.bb && t <= spill_site.order);
                reload_site.order += (reload_site.bb == b_start.bb && t <= reload_site.order);
                clean_single_bb_ordinals(ctx, ra, b_start.bb);
                new_b = NULL;
            }
        }
    }

    TB_BasicBlock* def_bb = f->scheduled[a->gvn];
    defs[def_bb - ctx->cfg.blocks] = a;

    TB_Node* spill_a = NULL;
    if (a->type != TB_MACH_COPY) {
        spill_a = tb_alloc_node(f, TB_MACH_COPY, a->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, spill_a, a, 1);
    } else {
        spill_a = a;
    }
    TB_NODE_SET_EXTRA(spill_a, TB_NodeMachCopy, .def = spill_mask, .use = a_def_mask);

    // insert forced reloads for any uses of a within the spilled range, ideally
    // we only insert one per-BB
    TB_Node** forced_reloads = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(TB_Node*));
    FOR_N(i, 0, ctx->bb_count) {
        forced_reloads[i] = NULL;
    }

    size_t old_mark = f->node_count;
    for (size_t k = 0; k < a->user_count;) {
        TB_Node* un = USERN(&a->users[k]);
        int ui      = USERI(&a->users[k]);
        if (un->gvn >= ra->order_cap || un->gvn >= old_mark) {
            k += 1;
            continue;
        }

        TB_BasicBlock* use_bb = f->scheduled[un->gvn];
        int t = ra->order[un->gvn] - 1;

        int start_t = 0, end_t = INT_MAX;
        if (use_bb == spill_site.bb) { start_t = spill_site.order; }
        if (use_bb == reload_site.bb) { end_t = reload_site.order; }

        // within spill range
        if (aggro || (use_bb >= spill_site.bb && use_bb <= reload_site.bb && t > start_t && t < end_t)) {
            RegMask* in_mask = constraint_in(ctx, un, ui);
            if (un->type != TB_MACH_COPY || (reg_mask_is_stack(in_mask) && reg_mask_is_stack(a_def_mask))) {
                // if there's already no
                int bb_id = use_bb - ctx->cfg.blocks;
                TB_Node* cpy = forced_reloads[bb_id];
                if (cpy == NULL) {
                    // A'' = reload A
                    forced_reloads[bb_id] = cpy = tb_alloc_node(f, TB_MACH_COPY, a->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, cpy, spill_a ? spill_a : a, 1);
                    TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = in_mask, .use = spill_mask);

                    VReg* reload_vreg = tb__set_node_vreg(ctx, cpy);
                    reload_vreg->reg_width = tb__reg_width_from_dt(in_mask->class, cpy->dt);
                    reload_vreg->mask = in_mask;
                    reload_vreg->spill_bias += 1e6;
                    reload_vreg->was_reload = true;
                } else {
                    VReg* reload_vreg = &ctx->vregs[ctx->vreg_map[cpy->gvn]];
                    reload_vreg->mask = tb__reg_mask_meet(ctx, reload_vreg->mask, in_mask);
                }

                set_input(f, un, cpy, ui);
                TB_OPTDEBUG(REGALLOC3)(printf("\x1b[33m#   BB%zu:    forced reload (%%%u)\x1b[0m\n", use_bb - ctx->cfg.blocks, cpy->gvn));
                continue;
            }
        }

        k += 1;
    }

    // insert the reloads now that we know which blocks they go into
    FOR_N(i, 0, ctx->bb_count) {
        if (forced_reloads[i] == NULL) {
            continue;
        }

        TB_BasicBlock* use_bb = &ctx->cfg.blocks[i];
        TB_Node* cpy = forced_reloads[i];

        int earliest_t = aarray_length(use_bb->items);
        FOR_USERS(use, cpy) {
            TB_Node* un = USERN(use);
            earliest_t = TB_MIN(earliest_t, ra->order[un->gvn] - 1);
        }
        TB_ASSERT(earliest_t < aarray_length(use_bb->items));

        int t = rogers_insert_op(ctx, i, cpy, earliest_t);
        spill_site.order += (spill_site.bb == use_bb && t <= spill_site.order);
        reload_site.order += (reload_site.bb == use_bb && t <= reload_site.order);

        // shift ordinals
        FOR_N(j, t, aarray_length(use_bb->items)) {
            TB_Node* member = use_bb->items[j];
            if (member->gvn < ra->order_cap) {
                ra->order[member->gvn] = 1 + j;
            }
        }
    }

    bool in_spilled_region = false;
    TB_Node* reload_a = NULL;

    if (spill_a == a) {
        VReg* spill_vreg = &ctx->vregs[ctx->vreg_map[spill_a->gvn]];
        spill_vreg->reg_width = tb__reg_width_from_dt(spill_mask->class, a->dt);
        spill_vreg->mask = spill_mask;
    } else {
        int bb_id = spill_site.bb - ctx->cfg.blocks;
        int old = spill_site.order;

        // A' = spill A
        rogers_insert_op(ctx, bb_id, spill_a, spill_site.order++);
        defs[bb_id] = spill_a;

        VReg* spill_vreg = tb__set_node_vreg(ctx, spill_a);
        spill_vreg->reg_width = tb__reg_width_from_dt(spill_mask->class, a->dt);
        spill_vreg->mask = spill_mask;

        TB_OPTDEBUG(REGALLOC3)(printf("\x1b[33m#   BB%d:    spill (%%%u)\x1b[0m\n", bb_id, spill_a->gvn));

        // B' = copy B
        if (new_b) {
            rogers_insert_op(ctx, bb_id, new_b, spill_site.order++);
        }
        in_spilled_region = true;

        if (spill_site.bb == reload_site.bb) {
            if (reload_site.order >= old) {
                reload_site.order += spill_site.order - old;
            }

            // if this happens it means the reload was directly after the spill so it shouldn't have split.
            if (reload_a) {
                TB_ASSERT(spill_site.order < reload_site.order);
            }
        }
    }

    int real_uses = 0;
    RegMask* reload_mask = NULL;
    FOR_USERS(u, a) {
        TB_Node* un = USERN(u);
        if (un == spill_a) {
            continue;
        } else if (un->type == TB_MACH_COPY) {
            // this is a reload, it'll be taking in a stack var so it can't
            // also produce one, that'll require some reg-stack interactions
            RegMask* def = TB_NODE_GET_EXTRA_T(un, TB_NodeMachCopy)->def;
            if (!reg_mask_is_stack(def)) {
                continue;
            }
        }

        RegMask* in_mask = constraint_in(ctx, un, USERI(u));
        reload_mask = tb__reg_mask_meet(ctx, in_mask, reload_mask);
        real_uses += 1;
    }

    int reload_vreg_id = 0;
    if (real_uses > 0) {
        TB_ASSERT(spill_a);
        int bb_id = reload_site.bb - ctx->cfg.blocks;

        // A'' = reload A
        TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, a->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, spill_a, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = reload_mask, .use = spill_mask);
        rogers_insert_op(ctx, bb_id, cpy, reload_site.order);
        defs[bb_id] = reload_a = cpy;

        VReg* reload_vreg = tb__set_node_vreg(ctx, cpy);
        reload_vreg->reg_width = tb__reg_width_from_dt(reload_mask->class, cpy->dt);
        reload_vreg->mask = reload_mask;
        reload_vreg->was_reload = true;
        reload_vreg_id = reload_vreg - ctx->vregs;

        TB_OPTDEBUG(REGALLOC3)(printf("\x1b[33m#   BB%d:    reload (%%%u)\x1b[0m\n", bb_id, cpy->gvn));
    }

    uint32_t new_nodes_mark = f->node_count;
    TB_Node* self_phi = NULL;
    ArenaArray(TB_Node*) phis = aarray_create(ra->arena, TB_Node*, 10);
    if (reload_a != NULL) {
        Set has_already = set_create_in_arena(ra->arena, ctx->bb_count);
        Set ever_on_ws  = set_create_in_arena(ra->arena, ctx->bb_count);

        int queue_count = 0;
        int* queue = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(int));

        set_put(&ever_on_ws, spill_site.bb - ctx->cfg.blocks);
        queue[queue_count++] = spill_site.bb - ctx->cfg.blocks;

        if (spill_site.bb != reload_site.bb) {
            set_put(&ever_on_ws, reload_site.bb - ctx->cfg.blocks);
            queue[queue_count++] = reload_site.bb - ctx->cfg.blocks;
        }

        if (0 && a->type == TB_PHI) {
            FOR_N(i, 1, a->input_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, a->inputs[0], i - 1);
                int pred_bb = f->scheduled[pred->gvn] - ctx->cfg.blocks;

                if (!set_get(&ever_on_ws, pred_bb)) {
                    set_put(&ever_on_ws, pred_bb);
                    queue[queue_count++] = pred_bb;

                    defs[pred_bb] = a->inputs[i];
                }
            }
        }

        while (queue_count--) {
            int x = queue[queue_count];
            ArenaArray(int) df = ra->df[x];
            if (df == NULL) {
                continue;
            }

            FOR_N(i, 0, aarray_length(df)) {
                int y = df[i];

                // don't place phis in dead blocks
                TB_Node* y_head = ctx->cfg.blocks[y].start;
                TB_BasicBlock* y_bb = f->scheduled[y_head->gvn];
                if (!set_get(&y_bb->live_in, a->gvn)) {
                    continue;
                }

                if (!set_get(&has_already, y)) {
                    TB_ASSERT(cfg_is_region(y_head));

                    TB_Node* phi = tb_alloc_node(f, TB_PHI, a->dt, 1 + y_head->input_count, 0);
                    set_input(f, phi, y_head, 0);

                    // we know if a block is spilled if it's not dominated by the reload block
                    FOR_N(j, 0, y_head->input_count) {
                        TB_Node* pred = cfg_get_pred(&ctx->cfg, y_head, j);
                        TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                        TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                        if (slow_dommy2(reload_site.bb, pred_bb)) {
                            TB_OPTDEBUG(REGALLOC3)(printf("BB%zu: No need for copy on this edge %zu of %%%u\n", pred_bb - ctx->cfg.blocks, 1 + j, phi->gvn));
                            // if we're dominated by the reload, we don't need a copy.
                            // this will be fixed up in the next phase btw
                            set_input(f, phi, a, 1 + j);
                        } else {
                            // copying from the old def (a_def_mask) to the reloaded def (reload_mask)
                            TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, a->dt, 2, sizeof(TB_NodeMachCopy));
                            set_input(f, cpy, a, 1);
                            TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = reload_mask, .use = a_def_mask);

                            TB_Node* last = pred_bb->items[aarray_length(pred_bb->items) - 1];
                            int t = rogers_insert_op(ctx, pred_bb - ctx->cfg.blocks, cpy, aarray_length(pred_bb->items) - (last == a ? 1 : 0));
                            aarray_insert(ctx->vreg_map, cpy->gvn, reload_vreg_id);
                            set_input(f, phi, cpy, 1 + j);

                            spill_site.order += (spill_site.bb == pred_bb && t <= spill_site.order);
                            reload_site.order += (reload_site.bb == pred_bb && t <= reload_site.order);
                        }
                    }
                    set_put(&has_already, y);
                    aarray_insert(ctx->vreg_map, phi->gvn, reload_vreg_id);
                    aarray_push(phis, phi);

                    TB_OPTDEBUG(REGALLOC3)(printf("\x1b[33m#   BB%d:    phi (%%%u)\x1b[0m\n", y, phi->gvn));

                    if (defs[y] == NULL || reload_site.bb != &ctx->cfg.blocks[y]) {
                        defs[y] = phi;
                    } else {
                        self_phi = phi;
                    }

                    int t = rogers_insert_op(ctx, y, phi, 1);
                    spill_site.order += (spill_site.bb == &ctx->cfg.blocks[y] && t <= spill_site.order);
                    reload_site.order += (reload_site.bb == &ctx->cfg.blocks[y] && t <= reload_site.order);

                    if (!set_get(&ever_on_ws, y)) {
                        set_put(&ever_on_ws, y);
                        queue[queue_count++] = y;
                    }
                }
            }
        }
    }

    // resize ordinals
    if (f->node_count >= ra->order_cap) {
        size_t new_cap = tb_next_pow2(f->node_count + 16);
        ra->order = tb_arena_realloc(ra->arena, ra->order, ra->order_cap * sizeof(int), new_cap * sizeof(int));
        ra->gvn2node = tb_arena_realloc(ra->arena, ra->gvn2node, ra->order_cap * sizeof(TB_Node*), new_cap * sizeof(TB_Node*));
        ra->order_cap = new_cap;
    }

    // resize UF
    if (f->node_count >= ra->uf_len) {
        size_t new_len = tb_next_pow2(f->node_count + 16);
        rogers_resize_uf(ctx, ra, new_len);
    }

    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];

            ra->gvn2node[n->gvn] = n;
            ra->order[n->gvn] = timeline++;
        }
    }

    // TB_OPTDEBUG(REGALLOC3)(rogers_dump_sched(ctx, old_node_count));

    if (reload_a != NULL) {
        // renaming all users of A
        for (size_t k = 0; k < a->user_count;) {
            TB_Node* un = USERN(&a->users[k]);
            int ui      = USERI(&a->users[k]);
            if (ui == 0 || ui >= un->input_count) {
                k++;
                continue;
            }

            TB_BasicBlock* use_bb = f->scheduled[un->gvn];
            if (un->gvn >= new_nodes_mark && un->type == TB_PHI) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, un->inputs[0], ui - 1);
                use_bb = f->scheduled[pred->gvn];
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);
            }

            int t = ra->order[un->gvn] - 1;

            // grab the definition from your nearest dom
            TB_Node* def = NULL;
            if (use_bb == reload_site.bb && t <= reload_site.order) {
                def = self_phi ? self_phi : a;
            }

            // printf("Traverse:\n");
            while (def == NULL) {
                // printf("  BB%zu\n", use_bb - ctx->cfg.blocks);
                def = defs[use_bb - ctx->cfg.blocks];
                TB_ASSERT(def != NULL || use_bb != use_bb->dom);
                use_bb = use_bb->dom;
            }

            if (def != a && (un != def || un->type == TB_PHI)) {
                // any reconnection happening here is to a reloaded node (since the OG node
                // is the only non-reloaded node here).
                RegMask* in_mask = constraint_in(ctx, un, ui);
                ctx->vregs[reload_vreg_id].mask = tb__reg_mask_meet(ctx, ctx->vregs[reload_vreg_id].mask, in_mask);

                set_input(f, un, def, ui);
            } else {
                TB_ASSERT(un != a);
                k += 1;
            }
        }

        // add in-constraints
        /*FOR_USERS() {
            RegMask* in_mask = constraint_in(ctx, use_n, use_i);
            new_b_mask = tb__reg_mask_meet(ctx, ctx->vregs[], in_mask);
        }*/

        // coalesce phis
        int leader = uf_find(ra->uf, ra->uf_len, reload_a->gvn);
        aarray_for(i, phis) {
            TB_Node* def = phis[i];
            TB_ASSERT(def && def->type == TB_PHI);

            int x = uf_find(ra->uf, ra->uf_len, def->gvn);
            rogers_coalesce(ctx, ra, leader, x, reload_a, def);

            FOR_N(k, 1, def->input_count) {
                TB_Node* in = def->inputs[k];
                TB_ASSERT(in != a);

                // hard coalesce with direct input
                int y = uf_find(ra->uf, ra->uf_len, in->gvn);
                rogers_coalesce(ctx, ra, leader, y, reload_a, in);
            }
        }

        // make sure we update the VReg mapping post-coalescing
        #if TB_OPTDEBUG_REGALLOC3
        aarray_for(i, phis) {
            TB_Node* def = phis[i];
            TB_ASSERT(def && def->type == TB_PHI);

            TB_ASSERT(ctx->vreg_map[def->gvn] == reload_vreg_id);
            FOR_N(k, 1, def->input_count) {
                TB_Node* in = def->inputs[k];
                TB_ASSERT(ctx->vreg_map[in->gvn] == reload_vreg_id);
            }
        }
        #endif
    } else if (spill_a != a) {
        // there's no explicit reload because all uses were already copies
        // we could morph into reloads.
        for (size_t k = 0; k < a->user_count;) {
            TB_Node* un = USERN(&a->users[k]);
            int ui      = USERI(&a->users[k]);
            if (un->type == TB_MACH_COPY && un != spill_a) {
                TB_NODE_GET_EXTRA_T(un, TB_NodeMachCopy)->use = spill_mask;
                set_input(f, un, spill_a, ui);
            } else {
                k += 1;
            }
        }
    }
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

                    #if 0
                    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
                    if (set && aarray_length(set)) {
                        // split the copy
                        aarray_for(j, set) {
                            int y = set[i]->gvn;
                            TB_Node* yy = ra->gvn2node[y];

                            RegMask* mask = NULL;
                            mask = tb__reg_mask_meet(mask, ctx->constraint(ctx, yy, NULL));
                            FOR_USERS(u, yy) {
                                if (USERI(u) > 0 && USERI(u) < USERN(u)->input_count) {
                                    RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                                    mask = tb__reg_mask_meet(ctx, mask, in_mask);
                                }
                            }
                            TB_ASSERT(mask != &TB_REG_EMPTY);

                            VReg* vreg;
                            if (j == 0) {
                                vreg = &ctx->vregs[ctx->vreg_map[yy->gvn]];
                            } else {
                                vreg = tb__set_node_vreg(ctx, remat);
                            }
                            vreg->mask = mask;
                            vreg->reg_width = tb__reg_width_from_dt(mask->class, yy->dt);
                            vreg->spill_bias = 1e6;

                            nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (y + 1));
                        }

                        aarray_clear(set);
                    }
                    #endif
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
        set_input(f, use_n, remat, use_i);

        if (use_n->type == TB_PHI) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, use_n->inputs[0], use_i - 1);
            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            int pos = aarray_length(pred_bb->items);
            TB_Node* last = pred_bb->items[pos - 1];
            if (is_proj(last)) { last = last->inputs[0]; }
            if (cfg_is_terminator(last)) {
                pos--;

                while (pos > 0 && pred_bb->items[pos] != pred_bb->start && is_proj(pred_bb->items[pos])) {
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
        }

        // RegMask* remat_mask = ctx->constraint(ctx, remat, NULL);
        reload_vreg->hint_vreg = ctx->vreg_map[use_n->gvn];
        reload_vreg->mask = in_mask; // tb__reg_mask_meet(ctx, in_mask, remat_mask);
        reload_vreg->reg_width = tb__reg_width_from_dt(reload_vreg->mask->class, remat->dt);
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

