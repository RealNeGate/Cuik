// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>

#define FOREACH_SET(it, set) \
FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;
    int* fixed;

    DynArray(int) spills;
    DynArray(int) potential_spills;

    int order_cap;
    int* order;

    Set active;
    Set future_active;
    Set live_out;

    // how many did the RA introduce
    int num_spills;
    int max_regs_in_class;

    // interference mask
    int mask_cap;
    uint64_t* mask;

    uint64_t* in_use;
} Rogers;

enum {
    NO_SPILL, MANY_CONFLICTS, ALLOC_FAIL
};

#define BND(arr, i, limit) ((i) >= (limit) ? abort() : 0, arr)[i]

// returns NO_SPILL, MANY_CONFLICTS, or ALLOC_FAIL
static int allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

static bool rogers_is_fixed(Ctx* ctx, Rogers* ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
}

static void better_spill_range(Ctx* ctx, Rogers* restrict ra, VReg* to_spill, RegMask* spill_mask, size_t old_node_count) {
    TB_Function* f = ctx->f;
    TB_Node* n = to_spill->n;
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: spill  (%%%u)\x1b[0m\n", to_spill - ctx->vregs, n->gvn));

    to_spill->mask = spill_mask;

    size_t bb_count = ctx->bb_count;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    int* reload_t = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(int));
    FOR_N(i, 0, bb_count) { reload_t[i] = 0; }

    TB_BasicBlock** scheduled = f->scheduled;
    FOR_USERS(u, n) {
        TB_Node* use_n = USERN(u);
        int use_i      = USERI(u);

        TB_BasicBlock* bb = scheduled[use_n->gvn];
        int use_t         = ra->order[use_n->gvn];
        assert(use_t > 0);

        if (use_i >= use_n->input_count) {
            // extra edges aren't for values
            continue;
        } else if (use_n->type == TB_MACH_COPY) {
            // if it's already a machine copy, inserting an extra one is useless
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (cpy->def->class != REG_CLASS_STK || spill_mask->class != REG_CLASS_STK) {
                TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%d: folded reload (%%%u)\x1b[0m\n", ctx->vreg_map[use_n->gvn], use_n->gvn));
                cpy->use = spill_mask;
                continue;
            } else {
                // stack-stack move requires a loosened mask for some register family
                cpy->use = ctx->normie_mask[cpy->use->class];
            }
        }

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
            assert(at != NULL);

            // schedule the split right before use
            assert(at->gvn != n->gvn);
            tb__insert_before(ctx, ctx->f, reload_n[i], at);
            VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n[i]);

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n[i]->gvn));
        }
    }

    for (size_t i = 0; i < n->user_count;) {
        TB_Node* use_n = USERN(&n->users[i]);
        int use_i      = USERI(&n->users[i]);
        TB_BasicBlock* bb = f->scheduled[use_n->gvn];

        if (use_i >= use_n->input_count) {
            i += 1;
            continue;
        } else if (use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (cpy->def == NULL || cpy->use == spill_mask) {
                i += 1;
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
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                RegMask* new_mask = tb__reg_mask_meet(ctx, reload_vreg->mask, in_mask);
                if (reload_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                    TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", reload_vreg - ctx->vregs));
                    split = true;
                }
                reload_vreg->mask = new_mask;
            }

            if (split) {
                // construct separate reloads
                assert(reload->type == TB_MACH_COPY);
                rematerialize(ctx, ra->fixed, reload);
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
    if (f->node_count >= ra->order_cap) {
        ra->order = tb_arena_realloc(ra->arena, ra->order, ra->order_cap * sizeof(int), ra->order_cap * 2 * sizeof(int));
        ra->order_cap *= 2;
    }

    // recompute order for dirty blocks
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] > 0) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            int timeline = 1;
            for (size_t j = 0; j < aarray_length(bb->items); j++) {
                TB_Node* n = bb->items[j];
                ra->order[n->gvn] = timeline++;
            }
        }
    }

    tb_arena_restore(&f->tmp_arena, sp);
}

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4"PRIdPTR" cost=%.2f ", vreg - ctx->vregs, cost);
    tb__print_regmask(vreg->mask);
    printf("\n");
}

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena) {
    Rogers ra = { .ctx = ctx, .arena = arena };
    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;

    TB_Worklist* ws = f->worklist;

    // i keep the nodes for the BB entries at the bottom of the worklist at this time so
    // ideally don't kill that info. visited bits are gg tho
    size_t old = dyn_array_length(ws->items);
    worklist_clear_visited(ws);

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        int max_regs_in_class = 0;
        ra.fixed  = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));
        ra.in_use = tb_arena_alloc(arena, ctx->num_classes * sizeof(uint64_t));

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

    ra.spills = dyn_array_create(int, 32);
    ra.potential_spills = dyn_array_create(int, 32);

    // create timeline & insert moves
    CUIK_TIMED_BLOCK("insert legalizing moves") {
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            size_t j = 0; // we do insert things while iterating
            for (; j < aarray_length(bb->items); j++) {
                TB_Node* n = bb->items[j];
                int tmp_count = ctx->tmp_count(ctx, n);

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
                        dyn_array_put(ra.spills, in_vreg - ctx->vregs);
                    }

                    int fixed = fixed_reg_mask(new_mask);
                    if (fixed >= 0) {
                        uint64_t fixed_mask = 1ull << fixed;
                        int shared_edge = ctx->node_2addr(in);
                        if (shared_edge >= 0) {
                            TB_ASSERT(shared_edge < in->input_count);
                            FOR_N(m, 1, in->input_count) if (m != shared_edge && in->inputs[m]) {
                                VReg* m_vreg = node_vreg(ctx, in->inputs[m]);
                                if (m_vreg && m_vreg->mask->class == in_mask->class) {
                                    m_vreg->mask = intern_regmask(ctx, m_vreg->mask->class, m_vreg->mask->may_spill, m_vreg->mask->mask[0] & ~fixed_mask);
                                }
                            }
                        }
                    }

                    in_vreg->mask = new_mask;
                }

                int vreg_id = ctx->vreg_map[n->gvn];
                if (tmp_count > 0) {
                    // used for clobbers/scratch but more importantly they're not bound to a node.
                    Tmps* tmps  = tb_arena_alloc(arena, sizeof(Tmps) + tmp_count*sizeof(int));
                    tmps->count = tmp_count;
                    nl_table_put(&ctx->tmps_map, n, tmps);

                    FOR_N(k, in_count, in_count + tmp_count) {
                        RegMask* in_mask = ins[k];
                        TB_ASSERT(in_mask != &TB_REG_EMPTY);

                        int fixed = fixed_reg_mask(in_mask);
                        if (fixed >= 0) {
                            // insert new range to the existing vreg
                            tmps->elems[k - in_count] = ra.fixed[in_mask->class] + fixed;
                        } else {
                            tmps->elems[k - in_count] = aarray_length(ctx->vregs);
                            aarray_push(ctx->vregs, (VReg){ .n = n, .mask = in_mask, .assigned = -1, .spill_cost = INFINITY, .uses = 1 });
                        }
                    }
                }

                if (vreg_id > 0) {
                    VReg* vreg = &ctx->vregs[vreg_id];
                    RegMask* def_mask = vreg->mask;

                    vreg->spill_cost = NAN;

                    if (n->type == TB_MACH_COPY && n->inputs[1]->type == TB_PHI) {
                        worklist_push(ws, n);
                    }
                }
            }
        }
    }

    bool changes = false;

    // we aggressively place phi copies, let's filter some of them out
    if (dyn_array_length(ws->items) > old) {
        cuikperf_region_start("phi coalesce", NULL);
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        compute_ordinals(ctx, &ra, arena);

        // this avoids the subsume_node calls adding nodes to the list, they'd
        // do this if you remove nodes such that they get DCE'd
        f->worklist = NULL;

        while (dyn_array_length(ws->items) > old) {
            TB_Node* n = worklist_pop(ws);
            assert(n->type == TB_MACH_COPY);
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);

            // if the def_mask got tightened, we needed the copy
            RegMask* def_mask = ctx->vregs[ctx->vreg_map[n->gvn]].mask;
            if (def_mask == cpy->def && !interfere(ctx, &ra, n, n->inputs[1])) {
                // delete copy
                tb__remove_node(ctx, f, n);
                subsume_node(f, n, n->inputs[1]);
                changes = true;
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("PHI %%%u can't coalesce with %%%u\n", n->inputs[1]->gvn, n->gvn));
            }
        }

        // RA calls might add dead nodes but we don't care
        f->worklist = ws;

        tb_arena_restore(&f->tmp_arena, sp);
        cuikperf_region_end();
    }

    // resolving hard-splits
    if (dyn_array_length(ra.spills) > 0) {
        cuikperf_region_start("hard splits", NULL);
        // insert hard split code
        FOR_N(i, 0, dyn_array_length(ra.spills)) {
            VReg* vreg = &ctx->vregs[ra.spills[i]];
            RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
            spill_entire_lifetime(ctx, vreg, mask, true);
        }
        dyn_array_clear(ra.spills);
        cuikperf_region_end();
        changes = true;
    }

    if (changes) {
        // recompute liveness
        redo_dataflow(ctx, arena);
    }

    int rounds = 0;
    int starting_spills = ctx->num_regs[REG_CLASS_STK];
    ra.num_spills = starting_spills;

    cuikperf_region_start("allocate", NULL);
    for (;;) {
        rounds += 1;
        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("#  ROUND %-4d                 #\n", rounds);
        printf("###############################\n");
        #endif

        TB_ArenaSavepoint sp = tb_arena_save(arena);

        cuikperf_region_start("main loop", NULL);
        int res = allocate_loop(ctx, &ra, arena);
        cuikperf_region_end();

        if (res == NO_SPILL) {
            break;
        } else if (res == ALLOC_FAIL) {
            // undo assignments
            FOR_N(i, 1, aarray_length(ctx->vregs)) {
                if (!rogers_is_fixed(ctx, &ra, i)) {
                    if (ctx->vregs[i].assigned >= 0 && ctx->vregs[i].class == REG_CLASS_STK) {
                        // undo stack allocations, they can't cause spilling
                        ctx->vregs[i].class = 0;
                        ctx->vregs[i].assigned = -1;
                    }
                }
            }
        }

        cuikperf_region_start("insert spills", NULL);
        FOR_N(i, 0, dyn_array_length(ra.spills)) {
            int vreg_id = ra.spills[i];
            TB_Node* n  = ctx->vregs[vreg_id].n;
            RegMask* vreg_mask = ctx->vregs[vreg_id].mask;

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# V%u: spilled (%%%u)\x1b[0m\n", vreg_id, n->gvn));
            ctx->vregs[vreg_id].marked_spilled = false;

            // rematerialization candidates will delete the original def and for now, they'll
            // reload per use site (although we might wanna coalesce some later on).
            size_t old_node_count = f->node_count;
            if (can_remat(ctx, n)) {
                rematerialize(ctx, ra.fixed, n);
            } else {
                RegMask* old_rm = ctx->vregs[vreg_id].mask;

                // if the old mask was really tight, let's loosen
                // it without necessarily spilling to the stack.
                RegMask* spill_rm = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                VReg* spill_vreg = NULL;
                if (n->type == TB_PHI) {
                    ctx->vregs[vreg_id].spill_cost = INFINITY;
                    better_spill_range(ctx, &ra, &ctx->vregs[vreg_id], spill_rm, old_node_count);
                } else {
                    TB_Node* spill_n = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                    subsume_node2(f, n, spill_n);
                    set_input(f, spill_n, n, 1);
                    TB_NODE_SET_EXTRA(spill_n, TB_NodeMachCopy, .def = spill_rm, .use = vreg_mask);
                    tb__insert_after(ctx, f, spill_n, n);

                    ctx->vregs[vreg_id].mask = ctx->constraint(ctx, n, NULL);
                    ctx->vregs[vreg_id].spill_cost = NAN;

                    spill_vreg = tb__set_node_vreg(ctx, spill_n);
                    spill_vreg->spill_cost = INFINITY;
                    better_spill_range(ctx, &ra, spill_vreg, spill_rm, old_node_count);
                }
            }
        }
        cuikperf_region_end();

        dyn_array_clear(ra.spills);
        tb_arena_restore(arena, sp);

        // recompute liveness
        redo_dataflow(ctx, arena);
    }

    ctx->num_spills = ra.num_spills - (1 + ctx->param_count + ctx->call_usage);
    cuikperf_region_end();
}

static TB_Node* phi_move_in_block(TB_BasicBlock** scheduled, TB_BasicBlock* block, TB_Node* phi) {
    FOR_N(i, 1, phi->input_count) {
        if (scheduled[phi->inputs[i]->gvn] == block) {
            return phi->inputs[i];
        }
    }
    return NULL;
}

static int last_use_in_bb(TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n) {
    int l = 0;
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (USERI(u) < un->input_count &&
            scheduled[un->gvn] == bb &&
            l < ra->order[un->gvn]) {
            l = ra->order[un->gvn];
        }
    }
    return l;
}

static bool interfere_in_block(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs, TB_BasicBlock* block) {
    assert(lhs != rhs && "i... why?");

    // phis might have a liveness hole in the middle
    bool lhs_live_out = set_get(&block->live_out, lhs->gvn);
    bool rhs_live_out = set_get(&block->live_out, rhs->gvn);
    if (lhs->type == TB_PHI || rhs->type == TB_PHI) {
        TB_Node *phi = rhs, *other = lhs;
        if (lhs->type == TB_PHI && rhs->type != TB_PHI) {
            phi = lhs, other = rhs;
        }

        block = ctx->f->scheduled[phi->gvn];
        if (set_get(&block->live_out, phi->gvn)) {
            TB_Node* move = phi_move_in_block(ctx->f->scheduled, block, phi);

            // some phis have two ranges within a block
            //
            //   a = phi ..., c  OLD
            //   b = a + 1
            //   c = move b      NEW
            //
            // the phis themselves refer to the value in it's latest state, so after the TB_MACH_MOVE
            // that's gonna be the "next" state. we insert copies and coalesce them away such that by
            // this point in the pipeline, a phi CANNOT be referenced by the "bottom half", only the
            // potential copy can.
            if (move) {
                int other_t = ra->order[other->gvn];
                if (set_get(&block->live_out, other->gvn)) {
                    if (other_t >= ra->order[phi->gvn]) {
                        return true;
                    }
                } else {
                    int kill_site = ra->order[move->gvn];
                    int other_end = last_use_in_bb(ctx->f->scheduled, ra, block, other);
                    if (other_t >= ra->order[phi->gvn] && other_end < kill_site) {
                        return true;
                    }
                }

                return false;
            }
        }
    }

    if (lhs_live_out && rhs_live_out) {
        return true;
    } else if (!lhs_live_out && !rhs_live_out) {
        TB_Node *first = lhs, *last = rhs;
        if (ra->order[lhs->gvn] > ra->order[rhs->gvn]) {
            first = rhs, last = lhs;
        }

        block = ctx->f->scheduled[last->gvn];
        FOR_USERS(u, first) {
            TB_Node* un = USERN(u);
            if (USERI(u) < un->input_count &&
                block == ctx->f->scheduled[un->gvn] &&
                ra->order[un->gvn] > ra->order[last->gvn]) {
                return true;
            }
        }
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
        }

        block = ctx->f->scheduled[rhs->gvn];
        FOR_USERS(u, lhs) {
            TB_Node* un = USERN(u);
            if (USERI(u) < un->input_count &&
                block == ctx->f->scheduled[un->gvn] &&
                ra->order[un->gvn] > ra->order[rhs->gvn]) {
                return true;
            }
        }
    }

    return false;
}

static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs) {
    TB_BasicBlock* lhs_block = ctx->f->scheduled[lhs->gvn];
    TB_BasicBlock* rhs_block = ctx->f->scheduled[rhs->gvn];

    if (lhs_block == rhs_block) {
        return interfere_in_block(ctx, ra, lhs, rhs, lhs_block);
    }

    return interfere_in_block(ctx, ra, lhs, rhs, lhs_block)
        || interfere_in_block(ctx, ra, rhs, lhs, rhs_block);
}

static bool interfere_with_def(TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* def, TB_Node* other) {
    int other_start = 0;
    int other_end   = INT_MAX;

    if (!set_get(&bb->live_in, other->gvn)) {
        other_start = ra->order[other->gvn];
    }

    if (!set_get(&bb->live_out, other->gvn)) {
        other_end = last_use_in_bb(scheduled, ra, bb, other);
    }

    int def_t = ra->order[def->gvn];
    return def_t >= other_start && def_t <= other_end;
}

static bool allocate_reg(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id, uint64_t in_use) {
    VReg* vreg = &ctx->vregs[vreg_id];
    if (vreg->marked_spilled) { return true; }

    if (set_get(&ra->future_active, vreg_id)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   woke up V%d\n", vreg_id));

        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, vreg_id);
        set_put(&ra->active, vreg_id);
        return true;
    }

    #if TB_OPTDEBUG_REGALLOC
    rogers_print_vreg(ctx, ra, vreg);
    #endif

    if (vreg->assigned >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        set_put(&ra->active, vreg_id);
        return true;
    }

    int def_class = vreg->mask->class;
    int num_regs = def_class == REG_CLASS_STK ? ra->num_spills : ctx->num_regs[def_class];

    size_t mask_word_count = (num_regs + 63) / 64;
    RegMask* mask = vreg->mask;

    // what the regmask holds only applies up until num_regs[class], this is mostly
    // just relevant for the stack coloring i suppose
    FOR_N(j, 0, mask->count) { ra->mask[j] = ~mask->mask[j]; }
    FOR_N(j, mask->count, (ctx->num_regs[mask->class] + 63) / 64) { ra->mask[j] = UINT64_MAX; }
    if (ctx->num_regs[mask->class] % 64) {
        ra->mask[num_regs / 64] &= UINT64_MAX >> (64ull - (num_regs % 64));
    }

    ra->mask[0] |= in_use;

    // interfere live things
    // there's some cases where we'd rather spill ourselves over anything else we interfere
    // with, for instance with constants. another case is that we've got an overly tight constraint
    // and a spill would loosen that
    dyn_array_clear(ra->potential_spills);
    dyn_array_put(ra->potential_spills, vreg_id);

    cuikperf_region_start("active", NULL);
    TB_OPTDEBUG(REGALLOC)(printf("#   "));
    FOREACH_SET(i, ra->active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class) {
            // if they can interfere, then spilling it can improve colorability directly.
            if (other->mask->mask[0] & mask->mask[0]) {
                TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                dyn_array_put(ra->potential_spills, i);
            }

            TB_ASSERT(other->assigned >= 0);
            ra->mask[other->assigned / 64ull] |= (1ull << (other->assigned % 64ull));
        }
    }
    cuikperf_region_end();

    cuikperf_region_start("inactive", NULL);
    FOREACH_SET(i, ra->future_active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class && interfere(ctx, ra, vreg->n, other->n)) {
            // if they can interfere, log it
            if (other->mask->mask[0] & mask->mask[0]) {
                TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) future interferes as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                dyn_array_put(ra->potential_spills, i);
            }

            TB_ASSERT(other->assigned >= 0);
            ra->mask[other->assigned / 64ull] |= (1ull << (other->assigned % 64ull));
        }
    }
    cuikperf_region_end();

    // 2 address ops will interfere with their own inputs (except for
    // shared dst/src)
    TB_Node* n = vreg->n;
    int hint_vreg = vreg->hint_vreg;
    int shared_edge = ctx->node_2addr(n);
    if (shared_edge >= 0) {
        assert(shared_edge < n->input_count);
        FOR_N(k, 1, n->input_count) if (k != shared_edge && n->inputs[k]) {
            TB_Node* in = n->inputs[k];
            VReg* in_vreg = node_vreg(ctx, in);
            if (in_vreg && in_vreg->class == mask->class) {
                if (in_vreg->mask->mask[0] & mask->mask[0]) {
                    TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", in_vreg - ctx->vregs, in->gvn), print_reg_name(in_vreg->class, in_vreg->assigned), printf("; "));
                    dyn_array_put(ra->potential_spills, in_vreg - ctx->vregs);
                }

                ra->mask[in_vreg->assigned / 64ull] |= (1ull << (in_vreg->assigned % 64ull));
            }
        }

        if (n->inputs[shared_edge]) {
            hint_vreg = ctx->vreg_map[n->inputs[shared_edge]->gvn];
        }
    }
    TB_OPTDEBUG(REGALLOC)(printf("\n"));

    int hint_reg = hint_vreg > 0
        && ctx->vregs[hint_vreg].class == mask->class
        ?  ctx->vregs[hint_vreg].assigned
        :  -1;

    if (hint_reg >= 0 && (ra->mask[hint_reg / 64ull] & (1ull << (hint_reg % 64ull))) == 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(def_class, hint_reg), printf(" (HINTED)\n"));

        vreg->class    = def_class;
        vreg->assigned = hint_reg;
        set_put(&ra->active, vreg_id);
        return true;
    }

    if (reg_assign(ctx, vreg, ra->mask, num_regs)) {
        set_put(&ra->active, vreg_id);
        return true;
    } else {
        // if a stack slot failed to color then it means we
        // need more stack slots (there's an indefinite amount :p)
        if (def_class == REG_CLASS_STK) {
            vreg->class = REG_CLASS_STK;
            vreg->assigned = ra->num_spills++;
            set_put(&ra->active, vreg_id);

            // resize the mask array if necessary
            size_t new_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
            if (((ra->mask_cap+63)/64) < (new_cap+63)/64) {
                ra->mask_cap = new_cap + 64;
                ra->mask = tb_arena_alloc(ra->arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));
            }

            TB_OPTDEBUG(REGALLOC)(printf("#   assigned to STACK%d (new stack slot)\n", vreg->assigned));
            return true;
        }

        return false;
    }
}

static int choose_decent_spill(Ctx* restrict ctx, Rogers* restrict ra, VReg* attempted_vreg, int useful_class, uint64_t useful_spill) {
    cuikperf_region_start("choose spill", NULL);

    bool can_spill_self = false;
    if (attempted_vreg) {
        // this limits the interference thus improving colorability
        if (attempted_vreg->spill_bias < 1e9 && can_remat(ctx, attempted_vreg->n)) {
            can_spill_self = true;
        }

        // we can only spill ourselves if that meant loosening the vreg's mask
        if (fixed_reg_mask(attempted_vreg->mask) >= 0) {
            RegMask* expected_mask = ctx->constraint(ctx, attempted_vreg->n, NULL);
            printf("  self spill? %%%u\n", attempted_vreg->n->gvn);

            FOR_USERS(u, attempted_vreg->n) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                RegMask* new_mask = tb__reg_mask_meet(ctx, expected_mask, in_mask);

                printf("    use %%%u[%d]: ", USERN(u)->gvn, USERI(u));
                tb__print_regmask(new_mask);
                printf("\n");

                // shouldn't see any hard splits here?
                if (new_mask == &TB_REG_EMPTY) {
                    break;
                }
                expected_mask = new_mask;
            }

            if (attempted_vreg->mask != expected_mask) {
                TB_OPTDEBUG(REGALLOC)(printf("#     can self spill since mask is loosened from "), tb__print_regmask(attempted_vreg->mask), printf(" to "), tb__print_regmask(expected_mask), printf("\n"));
            }

            if (attempted_vreg->mask != expected_mask) {
                can_spill_self = true;
            }
        }
    }

    int best_spill = -1;
    float best_score = INFINITY;
    FOR_REV_N(i, 0, dyn_array_length(ra->potential_spills)) {
        int vreg_id = ra->potential_spills[i];
        VReg* vreg  = &ctx->vregs[vreg_id];

        if (attempted_vreg == vreg) {
            if (!can_spill_self) {
                continue;
            }
        } else {
            // skip live values which we couldn't use to accomodate our RA failure
            if (vreg->class != useful_class || (useful_spill & (1ull << vreg->assigned)) == 0) {
                continue;
            }
        }

        float score = get_spill_cost(ctx, vreg);
        if (score < best_score) {
            if (best_spill >= 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is a better spill! V%d (%f is better than %f)\n", vreg_id, best_spill, score, best_score));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is... one of the spills of all time! %f\n", vreg_id, score));
            }
            best_score = score;
            best_spill = vreg_id;
        }
    }
    cuikperf_region_end();
    assert(best_spill >= 0);
    return best_spill;
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    // unlike linear scan, we only need order information within blocks
    ra->order_cap = tb_next_pow2(ctx->f->node_count);
    ra->order = tb_arena_alloc(arena, ra->order_cap * sizeof(int));

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            ra->order[n->gvn] = timeline++;
        }
    }
}

static int commit_spill(Ctx* restrict ctx, Rogers* restrict ra, VReg* attempted_vreg, int useful_class, uint64_t useful_spill) {
    int best_spill = choose_decent_spill(ctx, ra, attempted_vreg, useful_class, useful_spill);
    dyn_array_put(ra->spills, best_spill);

    #if 0
    printf("  V%zu: Spill V%d %%%u (assigned=", attempted_vreg ? attempted_vreg - ctx->vregs : 0, best_spill, ctx->vregs[best_spill].n->gvn);
    print_reg_name(ctx->vregs[best_spill].class, ctx->vregs[best_spill].assigned);
    printf(")\n");
    #endif

    // undo it's allocation and pretend it's unallocated
    TB_ASSERT(!rogers_is_fixed(ctx, ra, best_spill));
    int old_assigned = ctx->vregs[best_spill].assigned;
    ctx->vregs[best_spill].class    = 0;
    ctx->vregs[best_spill].assigned = -1;
    ctx->vregs[best_spill].marked_spilled = true;

    // take out the equation for the rest of the allocation loop
    set_remove(&ra->active, best_spill);
    set_remove(&ra->future_active, best_spill);

    return old_assigned;
}

// returns >0 on failure
static int allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    ra->active        = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->future_active = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->live_out      = set_create_in_arena(arena, ctx->f->node_count);

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* root = ctx->f->root_node;
    compute_ordinals(ctx, ra, arena);

    ra->mask_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
    ra->mask = tb_arena_alloc(arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        // expire intervals for block:
        //   for any values which die this block, pause them
        //   if later blocks will have them live-in and if
        //   not, kill them.
        FOREACH_SET(j, ra->live_out) if (!set_get(live_in, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id == 0) { continue; }

            bool pause = false;
            FOR_N(k, i, ctx->bb_count) {
                TB_BasicBlock* other = &ctx->cfg.blocks[k];
                if (set_get(&other->live_in, j)) {
                    // move to future active
                    set_put(&ra->future_active, vreg_id);
                    pause = true;
                    break;
                }
            }

            if (pause) {
                TB_OPTDEBUG(REGALLOC)(printf("#   sleep  V%-4d\n", vreg_id));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
            }
            set_remove(&ra->active, vreg_id);
            set_remove(&ra->live_out, j);
        }

        // start intervals:
        FOREACH_SET(j, *live_in) if (!set_get(&ra->live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                if (!allocate_reg(ctx, ra, vreg_id, 0)) {
                    RegMask* mask = ctx->vregs[vreg_id].mask;
                    commit_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);
                    return ALLOC_FAIL;
                }
            }
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("#   live on BB start: ");
        FOREACH_SET(j, *live_in) {
            if (ctx->vreg_map[j] > 0) {
                VReg* other = &ctx->vregs[ctx->vreg_map[j]];
                printf("V%d:", ctx->vreg_map[j]), print_reg_name(other->class, other->assigned), printf(" ");
            }
        }
        printf("\n");
        #endif

        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            int def_t  = ra->order[n->gvn];
            if (is_proj(n) && n->inputs[0] != root) {
                continue;
            }

            #if TB_OPTDEBUG_REGALLOC
            printf("\n");
            printf("# ===========================\n");
            printf("# ");
            tb_print_dumb_node(NULL, n);
            printf("\n");
            #endif

            // expire intervals for node
            if (n->type != TB_PHI) {
                FOR_N(k, 1, n->input_count) {
                    TB_Node* in = n->inputs[k];
                    if (in == NULL) { continue; }

                    int vreg_id = ctx->vreg_map[in->gvn];
                    if (vreg_id == 0) {
                        set_remove(&ra->live_out, in->gvn);
                        continue;
                    }

                    if (!set_get(&ra->live_out, in->gvn)) {
                        // duplicate input
                        continue;
                    }

                    int last_use = last_use_in_bb(scheduled, ra, bb, in);
                    if (set_get(&bb->live_out, in->gvn)) {
                        // it's live out so it flows to the end of the block
                        if (in->type == TB_PHI) {
                            TB_Node* move = phi_move_in_block(ctx->f->scheduled, bb, in);

                            // once we've past the move, we can't *really* refer to the phi so it's paused
                            if (move && last_use <= def_t) {
                                // liveness hole within cur
                                TB_OPTDEBUG(REGALLOC)(printf("#   pause  V%-4d (for block)\n", vreg_id));
                                set_remove(&ra->live_out, in->gvn);
                                set_remove(&ra->active, vreg_id);
                                set_put(&ra->future_active, vreg_id);
                            }
                        }
                        continue;
                    }

                    set_remove(&ra->live_out, in->gvn);

                    bool pause = false;
                    if (last_use > def_t) {
                        pause = true;
                    } else {
                        size_t bb_id = bb - ctx->cfg.blocks;
                        FOR_N(k, bb_id, ctx->bb_count) {
                            TB_BasicBlock* other = &ctx->cfg.blocks[k];
                            if (set_get(&other->live_in, in->gvn)) {
                                // move to future active
                                pause = true;
                                break;
                            }
                        }
                    }

                    if (pause) {
                        TB_OPTDEBUG(REGALLOC)(printf("#   pause  V%-4d\n", vreg_id));
                        set_put(&ra->future_active, vreg_id);
                    } else {
                        TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
                    }
                    set_remove(&ra->active, vreg_id);
                }
            }

            // tmps only interfere with the live_out (which is a subset of the active set)
            Tmps* tmps = nl_table_get(&ctx->tmps_map, n);
            FOR_N(k, 0, ctx->num_classes) { ra->in_use[k] = 0; }
            if (tmps != NULL) {
                dyn_array_clear(ra->potential_spills);

                // compute in_use for all classes but also only if they interfere with the def site (not the def's
                // entire lifetime), this is a subset of the active set.
                TB_OPTDEBUG(REGALLOC)(printf("#   "));
                FOREACH_SET(k, ra->active) {
                    VReg* other = &ctx->vregs[k];
                    if (other->class > 0) {
                        TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", k, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                        ra->in_use[other->class] |= (1ull << other->assigned);
                        dyn_array_put(ra->potential_spills, k);
                    }
                }

                FOREACH_SET(k, ra->future_active) {
                    VReg* other = &ctx->vregs[k];
                    if (other->class > 0 && interfere_with_def(scheduled, ra, bb, n, other->n)) {
                        TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", k, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                        ra->in_use[other->class] |= (1ull << other->assigned);
                        dyn_array_put(ra->potential_spills, k);
                    }
                }

                TB_OPTDEBUG(REGALLOC)(printf("\n"));

                // temporaries interfere with each other (although in practice, they'll be fixed
                // so it doesn't matter)
                FOR_N(k, 0, tmps->count) {
                    VReg* tmp_vreg    = &ctx->vregs[tmps->elems[k]];
                    RegMask* tmp_mask = tmp_vreg->mask;

                    TB_OPTDEBUG(REGALLOC)(printf("#   TMP V%d ", tmps->elems[k]), tb__print_regmask(tmp_mask), printf("\n"));

                    uint64_t in_use = ra->in_use[tmp_mask->class] | ~tmp_mask->mask[0];
                    if (in_use == UINT64_MAX) {
                        int r = commit_spill(ctx, ra, NULL, tmp_mask->class, tmp_mask->mask[0]);
                        in_use &= ~(1ull << r);
                        return ALLOC_FAIL;
                    }

                    TB_OPTDEBUG(REGALLOC)(printf("#     available: %#"PRIx64"\n", ~in_use));
                    tmp_vreg->class    = tmp_mask->class;
                    tmp_vreg->assigned = tb_ffs64(~in_use) - 1;
                    ra->in_use[tmp_mask->class] |= (1ull << tmp_vreg->assigned);
                    TB_OPTDEBUG(REGALLOC)(printf("#     assigned to "), print_reg_name(tmp_vreg->class, tmp_vreg->assigned), printf("\n"));
                }

                // temporaries don't interfere with the output tho
                FOR_N(k, 0, ctx->num_classes) { ra->in_use[k] = 0; }
            }

            // allocate free register
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                int class = ctx->vregs[vreg_id].mask->class;
                if (!allocate_reg(ctx, ra, vreg_id, ra->in_use[class])) {
                    RegMask* mask = ctx->vregs[vreg_id].mask;
                    int r = commit_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);

                    // ctx->vregs[vreg_id].class = class;
                    // ctx->vregs[vreg_id].assigned = r;
                    return ALLOC_FAIL;
                }

                TB_Node* def = ctx->vregs[vreg_id].n;
                set_put(&ra->live_out, def->gvn);
            } else if (n->dt.type == TB_TAG_TUPLE) {
                // allocate projections
                FOR_USERS(u, n) if (is_proj(USERN(u))) {
                    TB_Node* un = USERN(u);
                    vreg_id = ctx->vreg_map[un->gvn];
                    if (vreg_id > 0) {
                        int class = ctx->vregs[vreg_id].mask->class;
                        if (!allocate_reg(ctx, ra, vreg_id, ra->in_use[class])) {
                            RegMask* mask = ctx->vregs[vreg_id].mask;
                            int r = commit_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);

                            // ctx->vregs[vreg_id].class = class;
                            // ctx->vregs[vreg_id].assigned = r;
                            return ALLOC_FAIL;
                        }

                        set_put(&ra->live_out, un->gvn);
                    }
                }
            }
        }
    }

    return dyn_array_length(ra->spills) ? ALLOC_FAIL : NO_SPILL;
}

