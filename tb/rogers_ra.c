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
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;
    int* fixed;

    DynArray(int) potential_spills;

    // VREGs
    DynArray(int) spills;

    int order_cap;
    int* order;
    TB_Node** gvn2node;

    // [class][reg]
    int* active[8];
    int stack_reg_count;

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

enum {
    NO_SPILL, MANY_CONFLICTS, ALLOC_FAIL
};

#define BND(arr, i, limit) ((i) >= (limit) ? abort() : 0, arr)[i]

static bool allocate_loop2(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);

static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node);
static void better_spill_range(Ctx* ctx, Rogers* restrict ra, TB_Node* to_spill, size_t old_node_count);

static bool rogers_is_fixed(Ctx* ctx, Rogers* ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
}

static void rogers_insert_op(Ctx* ctx, int bb_id, TB_Node* n, int pos) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

    // skip phis and projections so that they stay nice and snug
    size_t cnt = aarray_length(bb->items);
    aarray_push(bb->items, 0);
    if (cnt > pos) {
        memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
    }
    bb->items[pos] = n;

    tb__insert(ctx, ctx->f, bb, n);
}

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    double cost = get_spill_cost(ctx, vreg);
    printf("#   V%-4"PRIdPTR" cost=%.2f ", vreg - ctx->vregs, cost);
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

static void rogers_coalesce(Ctx* restrict ctx, Rogers* restrict ra, int x, int y, TB_Node* xn, TB_Node* yn) {
    if (x == y) {
        return;
    }

    // hard coalesce with direct input
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
        move = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, move, in, 1);
        TB_NODE_SET_EXTRA(move, TB_NodeMachCopy, .def = rm, .use = ctx->normie_mask[rm->class]);
    }
    aarray_insert(ctx->vreg_map, move->gvn, vreg_id);
    return move;
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
        ra.fixed  = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));

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
    ra.spills = dyn_array_create(int, 32);

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
                        ra.uf_len += n->input_count - 1;
                        worklist_push(ws, n);
                    } else if (ctx->node_2addr(n) >= 0) {
                        ra.uf_len += 1;
                    }
                }
            }
        }
    }

    ra.coalesce_set = nl_table_alloc(100);
    ra.uf = tb_arena_alloc(arena, ra.uf_len * sizeof(int));
    ra.uf_size = tb_arena_alloc(arena, ra.uf_len * sizeof(int));
    FOR_N(i, 0, ra.uf_len) {
        ra.uf[i] = i;
        ra.uf_size[i] = 1;
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
        }
        dyn_array_clear(ra.potential_spills);
        cuikperf_region_end();

        f->worklist = ws;
        redo_dataflow(ctx, arena);
        f->worklist = NULL;
    }

    bool changes = false;
    if (dyn_array_length(ws->items) > 0) {
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

                    RegMask* in_mask = constraint_in(ctx, n, shared_edge);
                    if (!rogers_can_coalesce(ctx, &ra, n, in)) {
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
                    }

                    // hard coalesce with direct input
                    int y = uf_find(ra.uf, ra.uf_len, in->gvn);
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

    int starting_spills = ctx->num_regs[REG_CLASS_STK];
    ra.num_spills = starting_spills;

    int rounds = 0;
    cuikperf_region_start("main loop", NULL);
    for (;;) {
        TB_ArenaSavepoint sp = tb_arena_save(arena);

        rounds++;
        TB_OPTDEBUG(REGALLOC)(printf("# ========= Round %d =========\n", rounds));

        cuikperf_region_start("round", NULL);
        bool done = allocate_loop(ctx, &ra, arena);
        cuikperf_region_end();

        if (done) {
            tb_arena_restore(arena, sp);
            cuikperf_region_end();

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

        cuikperf_region_start("insert spills", NULL);

        size_t old_node_count = ctx->f->node_count;
        dyn_array_for(i, ra.spills) {
            TB_Node* to_spill = ra.gvn2node[ra.spills[i]];
            VReg* v = &ctx->vregs[ctx->vreg_map[ra.spills[i]]];

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%u: failed (%%%u)\x1b[0m\n", ctx->vreg_map[to_spill->gvn], to_spill->gvn));

            // failed to color a may_spill?
            if (v->mask->may_spill) {
                v->spill_cost = INFINITY;
                v->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                v->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, to_spill->dt);
                continue;
            }

            // re-eval this later
            v->spill_cost = NAN;

            if (can_remat(ctx, to_spill)) {
                TB_Arena* tmp_arena = &ctx->f->tmp_arena;
                TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);
                bool* reload_t = tb_arena_alloc(tmp_arena, ctx->bb_count * sizeof(bool));
                FOR_N(i, 0, ctx->bb_count) {
                    reload_t[i] = false;
                }

                TB_BasicBlock** scheduled = ctx->f->scheduled;
                FOR_USERS(u, to_spill) {
                    TB_Node* use_n = USERN(u);
                    int use_i      = USERI(u);

                    if (use_n->gvn < old_node_count && !is_proj(use_n) && use_i < use_n->input_count) {
                        TB_BasicBlock* bb = scheduled[use_n->gvn];
                        reload_t[bb - ctx->cfg.blocks] = true;
                    }
                }

                rogers_remat(ctx, &ra, to_spill, false);
                rogers_uncoalesce(ctx, &ra, to_spill->gvn);

                // recompute order for dirty blocks
                FOR_N(i, 0, ctx->bb_count) {
                    if (!reload_t[i]) { continue; }
                    TB_BasicBlock* bb = &ctx->cfg.blocks[i];
                    int timeline = 1;
                    for (size_t j = 0; j < aarray_length(bb->items); j++) {
                        TB_Node* n = bb->items[j];
                        ra.order[n->gvn] = 1 + j;
                    }
                }
                tb_arena_restore(tmp_arena, sp);

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

                            TB_Node* copy = rogers_hard_split(ctx, &ra, un, in, def_rm, new_vreg - ctx->vregs);
                            set_input(ctx->f, un, copy, ui);
                            tb__insert_before(ctx, ctx->f, copy, un);

                            // hard coalesce with direct input
                            int x = uf_find(ra.uf, ra.uf_len, un->gvn);
                            int y = uf_find(ra.uf, ra.uf_len, copy->gvn);
                            rogers_coalesce(ctx, &ra, x, y, un, copy);
                        } else {
                            k += 1;
                        }
                    }
                }

                // delete the original def
                ctx->vregs[ctx->vreg_map[to_spill->gvn]].uses -= 1;
                ctx->vreg_map[to_spill->gvn] = 0;
                tb__remove_node(ctx, ctx->f, to_spill);
                tb_kill_node(ctx->f, to_spill);
            } else {
                RegMask* v_mask = ctx->constraint(ctx, to_spill, NULL);

                // if the node is part of a bigger coalesced set, maybe splitting it would fix the issue
                int leader = uf_find(ra.uf, ra.uf_len, to_spill->gvn);
                ArenaArray(TB_Node*) set = nl_table_get(&ra.coalesce_set, (void*) (uintptr_t) (leader + 1));

                // maybe we're spending a lot of area with a very constrained mask, let's
                // split it early (if possible)
                if (fixed_reg_mask(v_mask) >= 0 && set == NULL) {
                    RegMask* in_mask = NULL;
                    FOR_USERS(u, to_spill) {
                        RegMask* rm = constraint_in(ctx, USERN(u), USERI(u));
                        in_mask = tb__reg_mask_meet(ctx, in_mask, rm);
                    }

                    TB_ASSERT(in_mask != &TB_REG_EMPTY);
                    if (in_mask != v_mask) {
                        // remove the may spill property if it applies
                        if (in_mask->may_spill) {
                            TB_ASSERT(in_mask->class != REG_CLASS_STK);
                            TB_ASSERT(in_mask->count == 1);
                            in_mask = intern_regmask(ctx, in_mask->class, false, in_mask->mask[0]);
                        }

                        v->mask = v_mask;
                        v->spill_bias += 1e8;

                        // split
                        TB_Node* split_n = tb_alloc_node(f, TB_MACH_COPY, to_spill->dt, 2, sizeof(TB_NodeMachCopy));
                        subsume_node2(f, to_spill, split_n);
                        set_input(f, split_n, to_spill, 1);
                        TB_NODE_SET_EXTRA(split_n, TB_NodeMachCopy, .def = in_mask, .use = v_mask);

                        // schedule the split right before use
                        tb__insert_after(ctx, ctx->f, split_n, to_spill);
                        VReg* split_vreg = tb__set_node_vreg(ctx, split_n);
                        split_vreg->reg_width = tb__reg_width_from_dt(in_mask->class, split_n->dt);
                        split_vreg->mask = in_mask;

                        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu:  split (%%%u)\x1b[0m\n", split_vreg - ctx->vregs, split_n->gvn));
                        continue;
                    }
                }

                if (set && aarray_length(set) > 1) {
                    bool has_phi = false; // can't split phis
                    aarray_for(i, set) {
                        if (set[i]->type == TB_PHI) {
                            has_phi = true;
                            break;
                        }
                    }

                    if (!has_phi) {
                        VReg* vreg = tb__set_node_vreg(ctx, to_spill);
                        vreg->mask = v_mask;
                        FOR_USERS(u, to_spill) {
                            if (is_proj(USERN(u))) {
                                continue;
                            }

                            RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                            RegMask* new_mask = tb__reg_mask_meet(ctx, vreg->mask, in_mask);

                            TB_ASSERT(new_mask != &TB_REG_EMPTY);
                            vreg->mask = new_mask;
                        }
                        vreg->reg_width = tb__reg_width_from_dt(vreg->mask->class, to_spill->dt);

                        aarray_for(i, set) {
                            if (set[i] == to_spill) {
                                aarray_remove(set, i);
                                break;
                            }
                        }

                        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: split from coalesce set (%%%u)\x1b[0m\n", vreg - ctx->vregs, to_spill->gvn));
                        continue;
                    }
                }

                better_spill_range(ctx, &ra, to_spill, old_node_count);

                VReg* vreg = node_vreg(ctx, to_spill);
                vreg->mask = v_mask;
                FOR_USERS(u, to_spill) {
                    if (is_proj(USERN(u))) {
                        continue;
                    }

                    RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                    RegMask* new_mask = tb__reg_mask_meet(ctx, vreg->mask, in_mask);

                    TB_ASSERT(new_mask != &TB_REG_EMPTY);
                    vreg->mask = new_mask;
                }
            }
        }

        dyn_array_clear(ra.spills);
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
    return (key * 11400714819323198485ull) >> 54ull;
}

static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn) {
    // printf("Last use in BB0 for %%%u: ", n->gvn);
    TB_OPTDEBUG(STATS)(stats_c++);

    uint64_t key = n_gvn | ((bb - blocks) << 32ull);
    int hash_index = inactive_hash_index(key);
    if (ra->inactive_cache && ra->inactive_cache[hash_index].key == key) {
        // printf(" Hit!\n");
        TB_OPTDEBUG(STATS)(stats_hit += 1);
        return ra->inactive_cache[hash_index].last_use;
    }

    TB_OPTDEBUG(STATS)(stats_miss += 1);

    // printf(" Miss.\n");
    int l = 0;
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
    VReg* other = &ctx->vregs[ctx->vreg_map[gvn]];
    ra->active[other->class][other->assigned] = 0;
}

static TB_Node* choose_decent_spill(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* attempted_n) {
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
        float score = get_spill_cost(ctx, vreg);

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

    // flag to be more aggro next time
    attempted_vreg->was_spilled = true;

    // we can only spill ourselves if that meant loosening the vreg's mask
    float self_score = get_spill_cost(ctx, attempted_vreg);
    if (self_score < best_score) {
        TB_OPTDEBUG(REGALLOC)(printf("#     self spilling! (%f is better than %f)\n", self_score, best_score));
        cuikperf_region_end();
        return attempted_n;
    }

    cuikperf_region_end();
    TB_ASSERT(best_spill >= 0);
    return ra->gvn2node[best_spill];
}

static bool allocate_reg(Ctx* ctx, Rogers* ra, TB_Node* n) {
    int vreg_id = ctx->vreg_map[n->gvn];
    VReg* vreg = &ctx->vregs[vreg_id];
    if (vreg_id == 0) {
        return false;
    }

    if (set_get(&ra->future_active, n->gvn)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   woke up %%%u\n", n->gvn));

        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, n->gvn);
        mark_active(ctx, ra, n->gvn);
        return true;
    }

    #if TB_OPTDEBUG_REGALLOC
    printf("#\n");
    rogers_print_vreg(ctx, ra, vreg);
    printf("#   ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
    #endif

    if (vreg->assigned >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        mark_active(ctx, ra, n->gvn);
        return true;
    }

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
        if (other_vreg->class != def_class) {
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
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        mark_active(ctx, ra, n->gvn);

        // mark all the other members of the coalesced group as future active
        FOR_N(j, 0, cnt) {
            if (arr[j] != n) {
                TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
                set_put(&ra->future_active, arr[j]->gvn);
                TB_OPTDEBUG(REGALLOC)(printf("#   sleep  %%%u\n", arr[j]->gvn));
            }
        }

        return true;
    } else {
        // if a stack slot failed to color then it means we
        // need more stack slots (there's an indefinite amount :p)
        if (def_class == REG_CLASS_STK) {
            // mark all the other members of the coalesced group as future active
            TB_ASSERT(cnt == 1);

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

            // resize the mask array if necessary
            size_t new_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
            if (((ra->mask_cap+63)/64) < (new_cap+63)/64) {
                ra->mask_cap = new_cap + 64;
                ra->mask = tb_arena_alloc(ra->arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));
            }

            TB_OPTDEBUG(REGALLOC)(printf("#   assigned to STACK%d (new stack slot)\n", vreg->assigned));
            return true;
        }

        TB_OPTDEBUG(REGALLOC)(printf("#   assigned UNCOLORED\n"));

        // we're gonna split up the entire VReg... at least for now
        TB_Node* best_spill = choose_decent_spill(ctx, ra, n);

        // if the spill interferes
        bool add = true;
        dyn_array_for(i, ra->spills) {
            if (best_spill->gvn == ra->spills[i]) {
                add = false;
                break;
            }

            TB_Node* other = ra->gvn2node[ra->spills[i]];
            if (interfere(ctx, ra, n, other) && reg_mask_may_intersect(mask, node_vreg(ctx, other)->mask)) {
                if (get_spill_cost(ctx, node_vreg(ctx, best_spill)) > get_spill_cost(ctx, node_vreg(ctx, other))) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   ok maybe we shouldn't spill %%%u, %%%u seems better\n", other->gvn, best_spill->gvn));
                    ra->spills[i] = best_spill->gvn;
                } else {
                    TB_OPTDEBUG(REGALLOC)(printf("#   %%%u is gonna interfere so maybe spilling %%%u isn't necessary?\n", other->gvn, best_spill->gvn));
                }

                add = false;
                break;
            }
        }

        if (add) {
            TB_OPTDEBUG(REGALLOC)(printf("#   gonna spill %%%u\n", best_spill->gvn));
            dyn_array_put(ra->spills, best_spill->gvn);
        }

        // kill this node for now (and "EVER")
        unmark_active(ctx, ra, best_spill->gvn);
        set_remove(&ra->live, best_spill->gvn);
        set_remove(&ra->future_active, best_spill->gvn);
        return false;
    }
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    size_t new_cap = ctx->f->node_count;
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
    ra->inactive_cache = tb_arena_alloc(arena, 1024 * sizeof(InactiveCacheEntry));
    memset(ra->inactive_cache, 0, 1024 * sizeof(InactiveCacheEntry));

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

    ra->mask_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
    ra->mask = tb_arena_alloc(arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));

    // stack's active set is resizable
    ra->stack_reg_count = tb_next_pow2(ctx->num_regs[0] + 8);
    if (ra->stack_reg_count < 16) {
        ra->stack_reg_count = 16;
    }

    ra->active[0] = tb_arena_alloc(arena, ra->stack_reg_count * sizeof(int));
    memset(ra->active[0], 0, ra->stack_reg_count * sizeof(int));

    // Compute areas (probably slow...)
    {
        // Sparse set repr
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        ArenaArray(int) stack = aarray_create(arena, int, 30);
        int* array = tb_arena_alloc(arena, ctx->f->node_count * sizeof(int));
        FOR_N(i, 0, ctx->f->node_count) {
            array[i] = -1;
        }

        aarray_for(i, ctx->vregs) {
            ctx->vregs[i].area = 0;
        }

        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                // expire
                if (array[n->gvn] >= 0) {
                    int last_gvn = stack[aarray_length(stack) - 1];
                    aarray_remove(stack, array[n->gvn]);
                    array[last_gvn] = array[n->gvn];
                    array[n->gvn] = -1;
                }

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

                #if TB_OPTDEBUG_REGALLOC
                printf("# ");
                #endif

                // accumulate area (don't do so on projections and temps)
                if (n->type != TB_MACH_TEMP && !is_proj(n)) {
                    aarray_for(i, stack) {
                        TB_ASSERT(ctx->vreg_map[stack[i]] > 0);
                        VReg* v = &ctx->vregs[ctx->vreg_map[stack[i]]];
                        v->area += 1;

                        #if TB_OPTDEBUG_REGALLOC
                        printf("%%%u ", stack[i]);
                        #endif
                    }
                }

                #if TB_OPTDEBUG_REGALLOC
                printf("\n");
                #endif
            }
        }
        tb_arena_restore(arena, sp);
    }

    // allocate pre-colored ranges
    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        FOR_REV_N(j, 0, aarray_length(bb->items)) {
            TB_Node* n = bb->items[j];
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0 && !set_get(&ra->future_active, n->gvn)) {
                // allocate pre-colored regs before anything else
                if (fixed_reg_mask(ctx->vregs[vreg_id].mask) >= 0) {
                    // must account for interference against any of the previously defined regs
                    allocate_reg(ctx, ra, n);

                    int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
                    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));

                    // put the pre-colored regs to rest
                    unmark_active(ctx, ra, n->gvn);
                    set_put(&ra->future_active, n->gvn);

                    if (set && aarray_length(set) > 0) {
                        aarray_for(i, set) if (set[i] != n) {
                            // put the pre-colored regs to rest
                            set_put(&ra->future_active, set[i]->gvn);
                        }
                    }
                }
            }
        }
    }

    if (dyn_array_length(ra->spills) > 0) {
        return false;
    }

    size_t uf_len = ctx->f->node_count;
    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        TB_OPTDEBUG(REGALLOC)(printf("# ========= BB%zu =========\n", i));

        cuikperf_region_start("BB", NULL);
        // expire intervals for block (was live, isn't now)
        FOREACH_SET(j, ra->live) if (!set_get(live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                expire_interval(ctx, ra, j, i);
            }
        }

        // start intervals
        FOREACH_SET(j, *live_out) if (!set_get(&ra->live, j)) {
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
            int def_t  = ra->order[n->gvn];

            #if TB_OPTDEBUG_REGALLOC
            printf("# ===========================\n");
            printf("# ");
            tb_print_dumb_node(NULL, n);
            printf("\n");
            #endif

            cuikperf_region_start("step", NULL);
            // expire intervals
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                // if a node is never used before its def, it's still considered
                // for allocation for that single program point.
                if (ctx->vregs[vreg_id].assigned < 0 && allocate_reg(ctx, ra, n)) {
                    set_put(&ra->live, n->gvn);
                }

                expire_interval(ctx, ra, n->gvn, i);
            }

            // if anything in the active set interferes with
            // the kill set, we need to spill around this op.
            RegMask** kills = ctx->ins;
            int kill_count = ctx->constraint_kill(ctx, n, kills);
            FOR_N(k, 0, kill_count) {
                int nr = ctx->num_regs[kills[k]->class];
                int* active = ra->active[kills[k]->class];
                BITS64_FOR(l, kills[k]->mask, nr) {
                    if (active[l] > 0) {
                        int in_use = active[l];

                        TB_OPTDEBUG(REGALLOC)(printf("#   gonna clobber %%%u\n", in_use));
                        dyn_array_put(ra->spills, in_use);

                        // kill this node for now (and "EVER")
                        unmark_active(ctx, ra, in_use);
                        set_remove(&ra->live, in_use);
                        set_remove(&ra->future_active, in_use);
                    }
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
            cuikperf_region_end();
        }
    }

    return dyn_array_length(ra->spills) == 0;
}

////////////////////////////////
// Expire or Start intervals
////////////////////////////////
static void expire_interval(Ctx* ctx, Rogers* restrict ra, uint32_t gvn, size_t bb_i) {
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
        TB_OPTDEBUG(REGALLOC)(printf("#   sleep  %%%u\n", gvn));
    } else {
        TB_OPTDEBUG(REGALLOC)(printf("#   expire %%%u\n", gvn));
    }
    unmark_active(ctx, ra, gvn);
    set_remove(&ra->live, gvn);
}

////////////////////////////////
// RA Splitting work
////////////////////////////////
static void better_spill_range(Ctx* ctx, Rogers* restrict ra, TB_Node* to_spill, size_t old_node_count) {
    TB_Function* f = ctx->f;
    RegMask* pre_spill_mask = ctx->constraint(ctx, to_spill, NULL);
    RegMask* spill_mask = ctx->mayspill_mask[pre_spill_mask->class];
    // RegMask* spill_mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);

    TB_Node* n = tb_alloc_node(f, TB_MACH_COPY, to_spill->dt, 2, sizeof(TB_NodeMachCopy));
    set_input(f, n, to_spill, 1);
    TB_NODE_SET_EXTRA(n, TB_NodeMachCopy, .def = spill_mask, .use = pre_spill_mask);
    tb__insert_after(ctx, ctx->f, n, to_spill);

    VReg* spill_vreg = tb__set_node_vreg(ctx, n);
    spill_vreg->spill_cost = NAN;
    spill_vreg->mask = spill_mask;
    spill_vreg->reg_width = tb__reg_width_from_dt(spill_mask->class, to_spill->dt);
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu:  spill (%%%u)\x1b[0m\n", spill_vreg - ctx->vregs, n->gvn));

    ctx->vregs[ctx->vreg_map[to_spill->gvn]].spill_bias += 1e8;

    size_t bb_count = ctx->bb_count;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    int* reload_t = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(int));
    FOR_N(i, 0, bb_count) { reload_t[i] = 0; }

    TB_BasicBlock** scheduled = f->scheduled;
    FOR_USERS(u, to_spill) {
        TB_Node* use_n = USERN(u);
        int use_i      = USERI(u);

        TB_BasicBlock* bb = scheduled[use_n->gvn];
        if (use_n->gvn >= old_node_count || is_proj(use_n) || use_i >= use_n->input_count) {
            // extra edges aren't for values
            continue;
        } else if (use_n->type == TB_MACH_COPY) {
            // if it's already a machine copy, inserting an extra one is useless
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (!reg_mask_is_stack(cpy->def) || !reg_mask_is_stack(spill_mask)) {
                TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%d: reload (folded into %%%u)\x1b[0m\n", ctx->vreg_map[use_n->gvn], use_n->gvn));
                cpy->use = spill_mask;
                continue;
            } else {
                // stack-stack move requires a loosened mask for some register family
                cpy->use = ctx->normie_mask[cpy->use->class];
            }
        }

        int use_t = ra->order[use_n->gvn];
        TB_ASSERT(use_t > 0);

        // earliest point within the BB
        int bb_id = bb - ctx->cfg.blocks;
        if (reload_t[bb_id] == 0 || use_t < reload_t[bb_id]) {
            reload_t[bb_id] = use_t;
        }
    }

    TB_Node** reload_n = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(TB_Node*));

    // insert reload nodes in each relevant BB
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] > 0) {
            // reload per use site
            reload_n[i] = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, reload_n[i], n, 1);
            TB_NODE_SET_EXTRA(reload_n[i], TB_NodeMachCopy, .def = NULL, .use = spill_mask);

            TB_Node* at = NULL;
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];
                if (n->gvn < old_node_count && ra->order[n->gvn] == reload_t[i]) {
                    at = n;
                    break;
                }
            }
            TB_ASSERT(at != NULL);

            // schedule the split right before use
            TB_ASSERT(at->gvn != n->gvn);
            tb__insert_before(ctx, ctx->f, reload_n[i], at);
            VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n[i]);

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n[i]->gvn));
        }
    }

    for (size_t i = 0; i < to_spill->user_count;) {
        TB_Node* use_n = USERN(&to_spill->users[i]);
        int use_i      = USERI(&to_spill->users[i]);
        TB_BasicBlock* bb = f->scheduled[use_n->gvn];

        if (use_n->gvn >= old_node_count || is_proj(use_n) || use_i >= use_n->input_count) {
            i += 1;
            continue;
        } else if (use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (cpy->def == NULL || cpy->use == spill_mask) {
                set_input(f, use_n, n, use_i);
                continue;
            }
        }

        TB_Node* reload = reload_n[bb - ctx->cfg.blocks];
        if (use_n != reload) {
            assert(reload);
            set_input(f, use_n, reload, use_i);
        } else {
            i += 1;
        }
    }

    // insert mask that's ok
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] > 0) {
            TB_Node* reload   = reload_n[i];
            VReg* reload_vreg = &ctx->vregs[ctx->vreg_map[reload->gvn]];
            TB_ASSERT(reload->user_count > 0);

            // this process might introduce hard-splits
            bool split = false;
            int class  = -1;
            FOR_USERS(u, reload) {
                if (is_proj(USERN(u))) {
                    continue;
                }

                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                RegMask* new_mask = tb__reg_mask_meet(ctx, reload_vreg->mask, in_mask);
                if (reload_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                    TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", reload_vreg - ctx->vregs));
                    split = true;
                }
                reload_vreg->mask = new_mask;
            }

            // remove the may spill property if it applies
            if (reload_vreg->mask->may_spill) {
                TB_ASSERT(reload_vreg->mask->class != REG_CLASS_STK);
                reload_vreg->mask = ctx->normie_mask[reload_vreg->mask->class];
            }

            reload_vreg->reg_width = tb__reg_width_from_dt(reload_vreg->mask->class, to_spill->dt);

            if (split) {
                // construct separate reloads
                TB_ASSERT(reload->type == TB_MACH_COPY);
                rogers_remat(ctx, ra, reload, true);
            } else {
                TB_NODE_SET_EXTRA(reload_n[i], TB_NodeMachCopy, .def = reload_vreg->mask, .use = spill_mask);

                // it would be hard to a spill when it's got a single use since we must be
                // directly before the use site.
                if (reload->type == TB_MACH_COPY && reload->user_count == 1) {
                    reload_vreg->spill_bias = 1e10;
                }
            }
        }
    }

    // unlike linear scan, we only need order information within blocks
    /*if (f->node_count >= ra->order_cap) {
        ra->order = tb_arena_realloc(ra->arena, ra->order, ra->order_cap * sizeof(int), ra->order_cap * 2 * sizeof(int));
        ra->order_cap *= 2;
    }*/

    // recompute order for dirty blocks
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] == 0) { continue; }
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            ra->order[n->gvn] = 1 + j;
        }
    }

    tb_arena_restore(&f->tmp_arena, sp);
}

static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node) {
    size_t extra = extra_bytes(n);
    TB_Function* f = ctx->f;
    TB_Node* root = f->root_node;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    // don't want weird pointer invalidation crap
    size_t user_count = n->user_count;
    TB_User* users = tb_arena_alloc(&f->tmp_arena, n->user_count * sizeof(TB_User));
    memcpy(users, n->users, n->user_count * sizeof(TB_User));

    // aggressive reload
    double base_bias = ctx->vregs[ctx->vreg_map[n->gvn]].spill_bias;
    for (size_t i = 0; i < user_count; i++) {
        TB_Node* use_n = USERN(&users[i]);
        int use_i      = USERI(&users[i]);

        // it's never in[0] lmao
        assert(use_i != 0);
        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        // remat per use site
        TB_Node* remat = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
        memcpy(remat->extra, n->extra, extra);
        FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
            remat->inputs[j] = n->inputs[j];
            add_user(f, remat, n->inputs[j], j);
        }

        set_input(f, use_n, remat, use_i);

        VReg* reload_vreg;
        if (use_n->type == TB_PHI) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, use_n->inputs[0], use_i - 1);
            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            // place at the end of the pred BB to the phi, basically the latest point
            rogers_insert_op(ctx, pred_bb - ctx->cfg.blocks, remat, aarray_length(pred_bb->items) - 1);

            // phis hard coalesce
            int vreg_id = ctx->vreg_map[use_n->gvn];
            reload_vreg = &ctx->vregs[vreg_id];
            aarray_insert(ctx->vreg_map, remat->gvn, vreg_id);
        } else {
            // schedule the split right before use
            tb__insert_before(ctx, ctx->f, remat, use_n);
            reload_vreg = tb__set_node_vreg(ctx, remat);
        }

        RegMask* remat_mask = ctx->constraint(ctx, remat, NULL);
        reload_vreg->mask = tb__reg_mask_meet(ctx, in_mask, remat_mask);
        reload_vreg->reg_width = tb__reg_width_from_dt(reload_vreg->mask->class, remat->dt);
        reload_vreg->spill_cost = INFINITY;
        TB_ASSERT(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        // if it's remat'ing a copy, we should edit the def mask to match the use
        if (remat->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(remat);
            cpy->def = reload_vreg->mask;
        }

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

#if 0
////////////////////////////////
// Greedy allocator
////////////////////////////////
static bool allocate_loop2(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    ra->inactive_cache = tb_arena_alloc(arena, 1024 * sizeof(InactiveCacheEntry));
    memset(ra->inactive_cache, 0, 1024 * sizeof(InactiveCacheEntry));

    ra->future_active = set_create_in_arena(arena, ctx->f->node_count);
    ra->live          = set_create_in_arena(arena, ctx->f->node_count);
    ra->dirty_bb      = tb_arena_alloc(arena, ctx->bb_count * sizeof(int));
    FOR_N(i, 0, ctx->bb_count) {
        ra->dirty_bb[i] = -1;
    }

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* root = ctx->f->root_node;
    compute_ordinals(ctx, ra, arena);

    ra->mask_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
    ra->mask = tb_arena_alloc(arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));

    IFG_Worklist queue = ifg_ws_alloc(ctx, ra->arena, aarray_length(ctx->vregs));
    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        FOR_REV_N(j, 0, aarray_length(bb->items)) {
            int vreg_id = ctx->vreg_map[bb->items[j]->gvn];
            ifg_ws_push(&hi, vreg_id);
        }
    }

    ra->live = set_create_in_arena(arena, ctx->f->node_count);

    int vreg_id;
    while (vreg_id = ifg_ws_pop(&lo), vreg_id > 0) {
        #if TB_OPTDEBUG_REGALLOC2
        briggs_print_vreg(ctx, ra, &ctx->vregs[vreg_id]);
        printf("#   interfere: ");
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

        // every node in the coalesced set needs to be checked for interference
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
        size_t cnt = set ? aarray_length(set) : 1;
        TB_Node** arr = set ? &set[0] : &n;

        TB_BasicBlock** scheduled = ctx->f->scheduled;
        BITS64_FOR(i, ra->future_active.data, ra->future_active.capacity) {
            TB_Node* other = ra->gvn2node[i];
            VReg* other_vreg = &ctx->vregs[ctx->vreg_map[i]];
            if (other_vreg->class != def_class) {
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
                        dyn_array_put(ra->potential_spills, i);
                    }

                    ra_mask[other_assigned / 64ull] |= (1ull << (other_assigned % 64ull));
                    break;
                }
            }
            cuikperf_region_end();
        }

        __debugbreak();

        #if TB_OPTDEBUG_REGALLOC2
        printf("\n");
        #endif
    }

    __debugbreak();
}
#endif


