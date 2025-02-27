// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>

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

    int order_cap;
    int* order;

    // we don't track stack with this one
    Set active_per_class[8];

    Set active;
    Set future_active;
    Set live_out;

    int* dirty_bb;

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

// returns NO_SPILL, MANY_CONFLICTS, or ALLOC_FAIL
static void allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

static bool rogers_is_fixed(Ctx* ctx, Rogers* ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
}

static void rematerialize(Ctx* ctx, int* fixed_vregs, TB_Node* n) {
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

        // schedule the split right before use
        tb__insert_before(ctx, ctx->f, remat, use_n);
        VReg* reload_vreg = tb__set_node_vreg(ctx, remat);

        RegMask* remat_mask = ctx->constraint(ctx, remat, NULL);
        reload_vreg->mask = tb__reg_mask_meet(ctx, in_mask, remat_mask);
        assert(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        // if it's remat'ing a copy, we should edit the def mask to match the use
        if (remat->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(remat);

            // slightly harder to rematerialize than a normal remat because we tightened it
            reload_vreg->spill_bias = base_bias + 1e8;
            cpy->def = reload_vreg->mask;
        } else {
            // reloads are unlikely to spill... but not impossible
            reload_vreg->spill_bias = base_bias + 1e7;
        }

        // if we're going into a fixed-dst copy, we should hint towards that vreg
        if (fixed_vregs && use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            int fixed = fixed_reg_mask(cpy->def);
            if (fixed >= 0) {
                reload_vreg->hint_vreg = fixed_vregs[cpy->def->class] + fixed;
            }
        }

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: remat  (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(&f->tmp_arena, sp);

    // delete the original def
    ctx->vregs[ctx->vreg_map[n->gvn]].uses -= 1;
    ctx->vreg_map[n->gvn] = 0;
    tb__remove_node(ctx, f, n);
    tb_kill_node(f, n);
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
                    reload_vreg->spill_bias = 1e8;
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
    double cost = get_spill_cost(ctx, vreg);
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

            if (i != 0) {
                ra.active_per_class[i] = set_create_in_arena(arena, count);
            }
        }
        ra.num_regs  = ctx->num_regs;
        ra.max_regs_in_class = max_regs_in_class;
    }

    // used for hard-list list at the very start
    ra.potential_spills = dyn_array_create(int, 32);

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

        ra.order_cap = 0;
        ra.order = NULL;

        tb_arena_restore(&f->tmp_arena, sp);
        cuikperf_region_end();
    }

    // resolving hard-splits
    if (dyn_array_length(ra.potential_spills) > 0) {
        cuikperf_region_start("hard splits", NULL);
        // insert hard split code
        FOR_N(i, 0, dyn_array_length(ra.potential_spills)) {
            VReg* vreg = &ctx->vregs[ra.potential_spills[i]];
            RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
            spill_entire_lifetime(ctx, vreg, mask, true);
        }
        dyn_array_clear(ra.potential_spills);
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

    cuikperf_region_start("main loop", NULL);
    allocate_loop(ctx, &ra, arena);
    cuikperf_region_end();

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

TB_OPTDEBUG(STATS)(static int stats_c = 0);

static uint32_t inactive_hash_index(uint64_t key) {
    const uint8_t* data = (uint8_t*) &key;
    uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < sizeof(uint32_t); i++) {
        h = (data[i] ^ h) * 0x01000193;
    }
    return (h * 11400714819323198485ull) >> 54ull;
}

static int next_use_in_bb(TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, int t) {
    int l = INT_MAX;
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (USERI(u) < un->input_count &&
            scheduled[un->gvn] == bb &&
            ra->order[un->gvn] > t &&
            ra->order[un->gvn] < l) {
            l = ra->order[un->gvn];
        }
    }

    l -= t;

    // past 4000 it like... probably doesn't matter?
    return l > 4000 ? 4000 : l;
}

static int last_use_in_bb(Ctx* restrict ctx, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n) {
    // printf("Last use in BB0 for %%%u: ", n->gvn);
    TB_OPTDEBUG(STATS)(stats_c++);

    uint64_t key = n->gvn | ((bb - ctx->cfg.blocks) << 32ull);
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
                    int phi_end = last_use_in_bb(ctx, ctx->f->scheduled, ra, block, phi);
                    int other_end = last_use_in_bb(ctx, ctx->f->scheduled, ra, block, other);
                    if (phi_end > other_t && other_end <= kill_site) {
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
        int last_use = last_use_in_bb(ctx, ctx->f->scheduled, ra, block, first);
        return last_use > ra->order[last->gvn];
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
        }

        block = ctx->f->scheduled[rhs->gvn];
        int last_use = last_use_in_bb(ctx, ctx->f->scheduled, ra, block, lhs);
        return last_use > ra->order[rhs->gvn];
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

static void mark_active(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id) {
    VReg* vreg = &ctx->vregs[vreg_id];
    if (vreg->class != REG_CLASS_STK) {
        set_put(&ra->active_per_class[vreg->class], vreg->assigned);
        // printf("MARK V%u %"PRIx64"\n\n", vreg_id, ra->active_per_class[vreg->class].data[0]);
    }
    set_put(&ra->active, vreg_id);
}

static void unmark_active(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id) {
    VReg* vreg = &ctx->vregs[vreg_id];
    if (vreg->class != REG_CLASS_STK) {
        set_remove(&ra->active_per_class[vreg->class], vreg->assigned);
        // printf("UNMARK V%u %"PRIx64"\n\n", vreg_id, ra->active_per_class[vreg->class].data[0]);
    }
    set_remove(&ra->active, vreg_id);
}

static bool allocate_reg(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id) {
    VReg* vreg = &ctx->vregs[vreg_id];

    if (set_get(&ra->future_active, vreg_id)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   woke up V%d\n", vreg_id));

        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, vreg_id);
        mark_active(ctx, ra, vreg_id);
        return true;
    }

    #if TB_OPTDEBUG_REGALLOC
    rogers_print_vreg(ctx, ra, vreg);
    #endif

    if (vreg->assigned >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        mark_active(ctx, ra, vreg_id);
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

    // interfere live things
    dyn_array_clear(ra->potential_spills);

    cuikperf_region_start("active", NULL);
    TB_OPTDEBUG(REGALLOC)(printf("#   "));
    if (def_class == REG_CLASS_STK) {
        FOREACH_SET(i, ra->active) {
            VReg* other = &ctx->vregs[i];
            if (other->class == mask->class) {
                TB_ASSERT(other->assigned >= 0);
                if (within_reg_mask(mask, other->assigned)) {
                    TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) active as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                }

                ra->mask[other->assigned / 64ull] |= (1ull << (other->assigned % 64ull));
            }
        }
    } else {
        FOR_N(i, 0, (ra->active_per_class[def_class].capacity + 63) / 64) {
            ra->mask[i] |= ra->active_per_class[def_class].data[i];
        }
    }
    cuikperf_region_end();

    cuikperf_region_start("inactive", NULL);
    FOREACH_SET(i, ra->future_active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class && interfere(ctx, ra, vreg->n, other->n)) {
            TB_ASSERT(other->assigned >= 0);
            if (within_reg_mask(mask, other->assigned)) {
                TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) inactive as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                dyn_array_put(ra->potential_spills, i);
            }

            TB_ASSERT(other->assigned >= 0);
            ra->mask[other->assigned / 64ull] |= (1ull << (other->assigned % 64ull));
        }
    }
    cuikperf_region_end();

    // if our users have shared edges and only one option, then we'll ban ourselves from
    // using their only option.
    TB_Node* n = vreg->n;
    FOR_USERS(u, n) {
        int shared_edge = ctx->node_2addr(USERN(u));
        if (shared_edge >= 0 && shared_edge != USERI(u)) {
            VReg* in_vreg = node_vreg(ctx, USERN(u));
            if (in_vreg && in_vreg->mask->class == mask->class) {
                int fixed = fixed_reg_mask(in_vreg->mask);
                if (fixed >= 0) {
                    ra->mask[fixed / 64ull] |= (1ull << (fixed % 64ull));
                }
            }
        }
    }

    // 2 address ops will interfere with their own inputs (except for
    // shared dst/src)
    int hint_vreg = vreg->hint_vreg;
    int shared_edge = ctx->node_2addr(n);
    if (shared_edge >= 0) {
        assert(shared_edge < n->input_count);
        FOR_N(k, 1, n->input_count) if (k != shared_edge && n->inputs[k]) {
            TB_Node* in = n->inputs[k];
            VReg* in_vreg = node_vreg(ctx, in);
            if (in_vreg && in_vreg->class == mask->class) {
                TB_ASSERT(in_vreg->assigned >= 0);
                if (within_reg_mask(mask, in_vreg->assigned)) {
                    TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) CISC-spill as ", in_vreg - ctx->vregs, in->gvn), print_reg_name(in_vreg->class, in_vreg->assigned), printf("; "));

                    // given our current design, spilling this value wouldn't improve
                    // coloring for ourselves since we can't use this register regardless.
                    //
                    // a = cpy b
                    // c = add d, a # c can be d but not a (because 2addr)
                    //
                    // the spill would force out the a *before* is c defined which means
                    // it's definition will stay as whatever it was (thus we can't steal it).
                    // this is something a fancier RA setup would solve with some splitting but
                    // we don't have that here, we just want aggressive forward progress here.
                    dyn_array_for(l, ra->potential_spills) {
                        if (ra->potential_spills[l] == in_vreg - ctx->vregs) {
                            dyn_array_remove(ra->potential_spills, l);
                            break;
                        }
                    }
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
        mark_active(ctx, ra, vreg_id);
        return true;
    }

    int reg_width = 1;
    if (def_class == REG_CLASS_STK) {
        if (vreg->n->dt.type == TB_TAG_V128)      { reg_width = 2; }
        else if (vreg->n->dt.type == TB_TAG_V256) { reg_width = 4; }
        else if (vreg->n->dt.type == TB_TAG_V512) { reg_width = 8; }
    }

    if (reg_assign(ctx, vreg, ra->mask, reg_width, num_regs)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));

        mark_active(ctx, ra, vreg_id);
        return true;
    } else {
        // if a stack slot failed to color then it means we
        // need more stack slots (there's an indefinite amount :p)
        if (def_class == REG_CLASS_STK) {
            vreg->class = REG_CLASS_STK;
            vreg->assigned = ra->num_spills;
            mark_active(ctx, ra, vreg_id);
            ra->num_spills += reg_width;

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
        return false;
    }
}

static bool bits64_member(uint64_t* arr, size_t x) {
    return arr[x / 64] & (1ull << (x % 64));
}

static int choose_decent_spill(Ctx* restrict ctx, Rogers* restrict ra, VReg* attempted_vreg) {
    cuikperf_region_start("choose spill", NULL);

    // these are added late because... there's a lot of them sometimes and we'd rather not add them
    // every allocation loop, if i could do the same with inactives without incurring an extra interference
    // check i would.
    int useful_class = attempted_vreg->mask->class;
    FOREACH_SET(i, ra->active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == useful_class && bits64_member(ra->mask, other->assigned)) {
            dyn_array_put(ra->potential_spills, i);
        }
    }

    int best_spill = -1;
    float best_score = INFINITY;
    FOR_REV_N(i, 0, dyn_array_length(ra->potential_spills)) {
        int vreg_id = ra->potential_spills[i];
        VReg* vreg  = &ctx->vregs[vreg_id];

        float score  = get_spill_cost(ctx, vreg);
        int next_use = next_use_in_bb(ctx->f->scheduled, ra, &ctx->cfg.blocks[ra->where_bb], vreg->n, ra->where_order);
        score = score * (4001 - next_use);

        if (score < best_score) {
            if (best_spill >= 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is a better spill! V%d (%f is better than %f)\n", vreg_id, best_spill, score, best_score));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is... one of the spills of all time! %f\n", vreg_id, score));
            }
            best_score = score;
            best_spill = vreg_id;
        } else {
            TB_OPTDEBUG(REGALLOC)(printf("#     V%d is a bad pick! %f\n", vreg_id, score));
        }
    }

    bool can_spill_self = false;
    if (attempted_vreg) {
        float self_score = get_spill_cost(ctx, attempted_vreg);
        int next_use = next_use_in_bb(ctx->f->scheduled, ra, &ctx->cfg.blocks[ra->where_bb], attempted_vreg->n, ra->where_order);
        self_score = self_score * (4001 - next_use);

        // this limits the interference thus improving colorability
        if (attempted_vreg->spill_bias < 1e7 && can_remat(ctx, attempted_vreg->n)) {
            if (self_score < best_score) {
                TB_OPTDEBUG(REGALLOC)(printf("#     self spilling! (%f is better than %f)\n", self_score, best_score));
                cuikperf_region_end();
                return attempted_vreg - ctx->vregs;
            }
        }

        // we can only spill ourselves if that meant loosening the vreg's mask
        if (fixed_reg_mask(attempted_vreg->mask) >= 0) {
            RegMask* expected_mask = ctx->constraint(ctx, attempted_vreg->n, NULL);

            FOR_USERS(u, attempted_vreg->n) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                RegMask* new_mask = tb__reg_mask_meet(ctx, expected_mask, in_mask);

                // shouldn't see any hard splits here?
                if (new_mask == &TB_REG_EMPTY) {
                    break;
                }
                expected_mask = new_mask;
            }

            if (attempted_vreg->mask != expected_mask) {
                TB_OPTDEBUG(REGALLOC)(printf("#     can self spill since mask is loosened from "), tb__print_regmask(attempted_vreg->mask), printf(" to "), tb__print_regmask(expected_mask), printf("\n"));
            }

            if (attempted_vreg->mask != expected_mask && self_score < best_score) {
                TB_OPTDEBUG(REGALLOC)(printf("#     self spilling! (%f is better than %f)\n", self_score, best_score));
                cuikperf_region_end();
                return attempted_vreg - ctx->vregs;
            }
        }
    }

    cuikperf_region_end();
    TB_ASSERT(best_spill >= 0);
    return best_spill;
}

static void ensure_ordinals_cap(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    // unlike traditional linear scan, we only need order information within blocks
    if (ctx->f->node_count > ra->order_cap) {
        size_t new_cap = tb_next_pow2(ctx->f->node_count);
        if (ra->order == NULL) {
            ra->order = tb_arena_alloc(arena, new_cap * sizeof(int));
        } else {
            ra->order = tb_arena_realloc(arena, ra->order, ra->order_cap * sizeof(int), new_cap * sizeof(int));
        }
        ra->order_cap = new_cap;
    }
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    ensure_ordinals_cap(ctx, ra, arena);

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            ra->order[n->gvn] = timeline++;
        }
    }
}

static void mark_dirty_bb(Rogers* restrict ra, int bb, int i) {
    if (i > ra->dirty_bb[bb]) {
        ra->dirty_bb[bb] = i;
    }
}

// the ra->mask will have the mask that failed to color, we
// need to steal one of the bits from there to make immediate
// progress.
static int commit_spill(Ctx* restrict ctx, Rogers* restrict ra, int attempted_vreg) {
    int best_spill = choose_decent_spill(ctx, ra, attempted_vreg ? &ctx->vregs[attempted_vreg] : NULL);

    #if 0
    printf("  V%zu: Spill V%d %%%u (assigned=", attempted_vreg, best_spill, ctx->vregs[best_spill].n->gvn);
    print_reg_name(ctx->vregs[best_spill].class, ctx->vregs[best_spill].assigned);
    printf(")\n");
    #endif

    int old_class = ctx->vregs[best_spill].class;
    int old_assigned = ctx->vregs[best_spill].assigned;

    ////////////////////////////////
    // Insert reload/remat sites
    ////////////////////////////////
    TB_Function* f = ctx->f;
    TB_Node* n = ctx->vregs[best_spill].n;
    bool remat = can_remat(ctx, n);
    // we only care about it's size during remat
    size_t extra = remat ? extra_bytes(n) : 0;
    RegMask* spill_rm = intern_regmask(ctx, REG_CLASS_STK, true, 0);
    RegMask* def_rm = ctx->vregs[best_spill].mask;

    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# spilling V%-4d (%%%u)\x1b[0m\n", best_spill, n->gvn));

    TB_Node* spill_copy = n;
    int spill_vreg_id = -1;
    if (!remat) {
        spill_copy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        TB_NODE_SET_EXTRA(spill_copy, TB_NodeMachCopy, .def = spill_rm, .use = def_rm);

        VReg* spill_vreg = tb__set_node_vreg(ctx, spill_copy);
        spill_vreg->spill_cost = INFINITY;
        spill_vreg->mask = spill_rm;
        spill_vreg_id = spill_vreg - ctx->vregs;

        int p = tb__insert_after(ctx, ctx->f, spill_copy, n);
        mark_dirty_bb(ra, f->scheduled[n->gvn] - ctx->cfg.blocks, p);
        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: spill-store (%%%u)\x1b[0m\n", spill_vreg - ctx->vregs, spill_copy->gvn));

        if (ra->where_bb == f->scheduled[n->gvn] - ctx->cfg.blocks && p <= ra->where_order) {
            ra->where_order += 1;
        }
    }

    double base_bias = ctx->vregs[ctx->vreg_map[n->gvn]].spill_bias;
    for (size_t i = 0; i < n->user_count;) {
        TB_Node* use_n = USERN(&n->users[i]);
        int use_i      = USERI(&n->users[i]);
        if (use_i >= use_n->input_count || (use_i == 0 && is_proj(use_n))) {
            i += 1;
            continue;
        }

        // it's never in[0] lmao
        TB_ASSERT(use_i != 0);
        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        TB_Node* reload = NULL;
        VReg* reload_vreg = NULL;
        if (remat) {
            // clone
            reload = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
            memcpy(reload->extra, n->extra, extra);
            FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
                set_input(f, reload, n->inputs[j], j);
            }

            reload_vreg = tb__set_node_vreg(ctx, reload);
            reload_vreg->mask = tb__reg_mask_meet(ctx, in_mask, ctx->constraint(ctx, reload, NULL));
            TB_ASSERT(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");
        } else {
            reload = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, reload, spill_copy, 1);
            TB_NODE_SET_EXTRA(reload, TB_NodeMachCopy, .def = in_mask, .use = spill_rm);

            reload_vreg = tb__set_node_vreg(ctx, reload);
            reload_vreg->mask = in_mask;
        }
        set_input(f, use_n, reload, use_i);

        int p = tb__insert_before(ctx, ctx->f, reload, use_n);
        if (ra->where_bb == f->scheduled[use_n->gvn] - ctx->cfg.blocks && p <= ra->where_order) {
            ra->where_order -= 1;
        }
        mark_dirty_bb(ra, f->scheduled[use_n->gvn] - ctx->cfg.blocks, p);

        // if it's remat'ing a copy, we should edit the def mask to match the use
        if (reload->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(reload);

            // slightly harder to rematerialize than a normal remat because we tightened it
            reload_vreg->spill_bias = base_bias + 1e8;
            cpy->def = reload_vreg->mask;
        } else {
            // reloads are unlikely to spill... but not impossible
            reload_vreg->spill_bias = base_bias + 1e7;
        }

        // any reloads or clones before the spill point can keep their assignment
        int use_bb = f->scheduled[use_n->gvn] - ctx->cfg.blocks;
        bool before_spill = use_bb < ra->where_bb || (use_bb == ra->where_bb && ra->order[use_n->gvn] < ra->where_order);
        if (before_spill) {
            reload_vreg->class    = old_class;
            reload_vreg->assigned = old_assigned;
        }

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: %s (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat ? "remat  " : "reload ", reload->gvn));
    }

    // we could choose to only recompute for the piece of the blocks we modded but im lazy
    CUIK_TIMED_BLOCK("recompute ordinals") {
        ensure_ordinals_cap(ctx, ra, ra->arena);

        // reset live-ins and live-outs from the spilled value, since the
        // spilled pieces can't spread across BBs
        aarray_for(i, ctx->cfg.blocks) {
            set_remove(&ctx->cfg.blocks[i].live_in,  n->gvn);
            set_remove(&ctx->cfg.blocks[i].live_out, n->gvn);

            if (ra->dirty_bb[i] >= 0) {
                int start = ra->dirty_bb[i];
                ra->dirty_bb[i] = -1;

                TB_BasicBlock* bb = &ctx->cfg.blocks[i];
                int* restrict order = ra->order;

                FOR_N(j, 0, aarray_length(bb->items)) {
                    TB_Node* n = bb->items[j];
                    order[n->gvn] = 1+j;
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("resize sets") {
        set_resize_in_arena(ra->arena, &ra->active, aarray_length(ctx->vregs));
        set_resize_in_arena(ra->arena, &ra->future_active, aarray_length(ctx->vregs));
        set_resize_in_arena(ra->arena, &ra->live_out, ctx->f->node_count);
    }

    if (remat) {
        // remat will kill the original def
        ra->order[n->gvn] = 0;
        ctx->vreg_map[n->gvn] = 0;

        TB_BasicBlock* kill_bb = f->scheduled[n->gvn];
        size_t kill_p = tb__remove_node(ctx, f, n);
        tb_kill_node(f, n);

        if (ra->where_bb == kill_bb - ctx->cfg.blocks && kill_p <= ra->where_order) {
            ra->where_order -= 1;
        }
    } else {
        // stitch up the original spill copy now
        set_input(f, spill_copy, n, 1);
    }

    // evict now
    unmark_active(ctx, ra, best_spill);
    set_remove(&ra->live_out, n->gvn);
    set_remove(&ra->future_active, best_spill);

    // we only have to clear our spilled n
    CUIK_TIMED_BLOCK("clear") {
        memset(ra->inactive_cache, 0, 1024 * sizeof(InactiveCacheEntry));
    }

    if (attempted_vreg == best_spill) {
        // it's a self spill, if so then we either are dead now (REMAT) or have to
        // retry allocation with a weaker constraint.
        if (!remat) {
            tb_todo();
        }
    } else {
        VReg* vreg = &ctx->vregs[attempted_vreg];

        // we just steal their assignment now
        vreg->class    = old_class;
        vreg->assigned = old_assigned;
        mark_active(ctx, ra, attempted_vreg);

        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
    }

    // allocate spill slot
    if (spill_vreg_id >= 0) {
        allocate_reg(ctx, ra, spill_vreg_id);
    }

    return old_assigned;
}

#if TB_OPTDEBUG_REGALLOC
static _Thread_local DynArray(int) dbg_pause_list;
#endif

static void allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    ra->inactive_cache = tb_arena_alloc(arena, 1024 * sizeof(InactiveCacheEntry));
    ra->active        = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->future_active = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->live_out      = set_create_in_arena(arena, ctx->f->node_count);
    ra->dirty_bb      = tb_arena_alloc(arena, ctx->bb_count * sizeof(int));
    FOR_N(i, 0, ctx->bb_count) {
        ra->dirty_bb[i] = -1;
    }

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* root = ctx->f->root_node;
    compute_ordinals(ctx, ra, arena);

    ra->mask_cap = ra->max_regs_in_class > ra->num_spills ? ra->max_regs_in_class : ra->num_spills;
    ra->mask = tb_arena_alloc(arena, ((ra->mask_cap+63)/64) * sizeof(uint64_t));

    #if TB_OPTDEBUG_REGALLOC
    if (dbg_pause_list == NULL) {
        dbg_pause_list = dyn_array_create(int, 32);
    }
    #endif

    memset(ra->inactive_cache, 0, 1024 * sizeof(InactiveCacheEntry));

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
            unmark_active(ctx, ra, vreg_id);
            set_remove(&ra->live_out, j);
        }

        ra->where_bb = i, ra->where_order = 0;

        // start intervals
        FOREACH_SET(j, *live_in) if (!set_get(&ra->live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0 && !allocate_reg(ctx, ra, vreg_id)) {
                commit_spill(ctx, ra, vreg_id);
            }
        }

        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            int def_t  = ra->order[n->gvn];
            if (is_proj(n) && n->inputs[0] != root) {
                continue;
            }

            ra->where_order = j;

            #if TB_OPTDEBUG_REGALLOC
            printf("# ===========================\n");
            printf("# ");
            tb_print_dumb_node(NULL, n);
            printf("\n");

            dyn_array_clear(dbg_pause_list);
            #endif

            TB_OPTDEBUG(STATS)(stats_c = 0);

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

                    int last_use = last_use_in_bb(ctx, scheduled, ra, bb, in);
                    if (set_get(&bb->live_out, in->gvn)) {
                        // it's live out so it flows to the end of the block
                        if (in->type == TB_PHI) {
                            TB_Node* move = phi_move_in_block(ctx->f->scheduled, bb, in);

                            // once we've past the move, we can't *really* refer to the phi so it's paused
                            if (move && last_use <= def_t) {
                                // liveness hole within cur
                                TB_OPTDEBUG(REGALLOC)(dyn_array_put(dbg_pause_list, vreg_id));
                                set_remove(&ra->live_out, in->gvn);
                                unmark_active(ctx, ra, vreg_id);
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

                    TB_OPTDEBUG(REGALLOC)(dyn_array_put(dbg_pause_list, vreg_id));
                    if (pause) {
                        set_put(&ra->future_active, vreg_id);
                    } else {
                        set_remove(&ra->future_active, vreg_id);
                    }
                    unmark_active(ctx, ra, vreg_id);
                }
            }

            #if TB_OPTDEBUG_REGALLOC
            if (dyn_array_length(dbg_pause_list)) {
                printf("# paused:");
                dyn_array_for(i, dbg_pause_list) {
                    if (set_get(&ra->future_active, dbg_pause_list[i])) {
                        printf(" V%d", dbg_pause_list[i]);
                    }
                }
                printf("\n# expire: ");
                dyn_array_for(i, dbg_pause_list) {
                    if (!set_get(&ra->future_active, dbg_pause_list[i])) {
                        printf(" V%d", dbg_pause_list[i]);
                    }
                }
                printf("\n");
            }
            #endif

            // allocate free register
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                int class = ctx->vregs[vreg_id].mask->class;
                if (!allocate_reg(ctx, ra, vreg_id)) {
                    commit_spill(ctx, ra, vreg_id);
                    // printf("RETRY AT %%%u\n", bb->items[ra->where_order]->gvn);
                    j = ra->where_order;
                }

                TB_Node* def = ctx->vregs[vreg_id].n;
                set_put(&ra->live_out, def->gvn);
            }

            // allocate projections
            FOR_USERS(u, n) if (is_proj(USERN(u))) {
                TB_Node* un = USERN(u);
                vreg_id = ctx->vreg_map[un->gvn];
                if (vreg_id > 0) {
                    #if TB_OPTDEBUG_REGALLOC
                    printf("# ");
                    tb_print_dumb_node(NULL, un);
                    printf("\n");
                    #endif

                    int class = ctx->vregs[vreg_id].mask->class;
                    if (!allocate_reg(ctx, ra, vreg_id)) {
                        commit_spill(ctx, ra, vreg_id);
                        // printf("RETRY AT %%%u\n", bb->items[ra->where_order]->gvn);
                        j = ra->where_order;
                    }

                    // some projections have literally no
                    // uses, they just die immediately.
                    if (un->user_count == 0) {
                        unmark_active(ctx, ra, vreg_id);
                    } else {
                        set_put(&ra->live_out, un->gvn);
                    }
                }
            }

            TB_OPTDEBUG(STATS)(printf("%d queries!!!\n", stats_c));
        }
    }
}

