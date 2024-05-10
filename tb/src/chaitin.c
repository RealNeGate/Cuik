// TODO(NeGate): implement Chaitin-Briggs, if you wanna contribute this would be cool to work
// with someone else on.
#include "codegen.h"
#include <float.h>

// used by codegen.h & it's friends but some of those get compiled multiple
// TUs and i want a consistent address.
RegMask TB_REG_EMPTY = { 1, 0, 1, { 0 } };

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* fixed;
    uint64_t* callee_saved;
    RegMask* normie_mask;

    int max_spills;
    DynArray(int) spills;

    // Interference graph
    size_t ifg_stride;
    size_t ifg_len;
    uint64_t* ifg;
    int* degree;
} Chaitin;

static bool test_n_set(uint32_t* arr, int i) {
    bool old = arr[i / 32] & (1u << (i % 32));
    arr[i / 32] |= (1u << (i % 32));
    return old;
}

static bool test_n_reset(uint32_t* arr, int i) {
    bool old = arr[i / 32] & (1u << (i % 32));
    arr[i / 32] &= ~(1u << (i % 32));
    return old;
}

static void ifg_edge(Chaitin* ra, int i, int j) {
    if (i < j) { SWAP(int, i, j); }
    ra->ifg[i*ra->ifg_stride + j/64] |= 1ull << (j % 64);
}

static bool ifg_test(Chaitin* ra, int i, int j) {
    return ra->ifg[i*ra->ifg_stride + j/64] & (1ull << (j % 64));
}

static void ifg_remove(Chaitin* ra, int i, int j) {
    // TB_OPTDEBUG(REGALLOC)(printf("remove V%d -- V%d\n", i, j));
    assert(ifg_test(ra, i, j));
    ra->ifg[i*ra->ifg_stride + j/64] &= ~(1ull << (j % 64));
}

static void ifg_dump(Chaitin* ra) {
    printf("###############################\n");
    printf("# interference                #\n");
    printf("###############################\n");
    FOR_N(i, 0, ra->ifg_len) {
        printf("V%03zu: ", i);
        FOR_N(j, 0, ra->ifg_stride) {
            uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
            FOR_N(k, 0, 64) {
                printf("%c", (bits >> k) & 1 ? '*' : ' ');
            }
        }
        printf("\n");
    }
}

static void ifg_dump_edge(Chaitin* ra, int i) {
    FOR_N(j, 0, ra->ifg_stride) {
        uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
        if (bits == 0) continue;

        FOR_N(k, 0, 64) if ((bits >> k) & 1) {
            int l = j*64 + k;
            printf("V%d -- V%d%s\n", i, l, l >= ra->fixed[2] && ra->fixed[2] < l + 16 ? " (fixed)" : "");
        }
    }
}

static void ifg_square(Chaitin* ra) {
    // compute degree & fill in the rest of the interference graph
    FOR_N(i, 0, ra->ifg_len) {
        FOR_N(j, 0, ra->ifg_stride) {
            uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
            if (bits == 0) continue;

            FOR_N(k, 0, 64) if ((bits >> k) & 1) {
                int l = j*64 + k;
                ra->ifg[l*ra->ifg_stride + i/64] |= 1ull << (i % 64);
            }
        }
    }

    FOR_N(i, 0, ra->ifg_len) {
        int sum = 0;
        FOR_N(j, 0, ra->ifg_stride) {
            sum += tb_popcount64(ra->ifg[i*ra->ifg_stride + j]);
        }
        ra->degree[i] = sum;
    }
}

static int ifg_remove_edges(Chaitin* ra, int* ws, int ws_cnt, uint32_t* visited, int i) {
    // TB_OPTDEBUG(REGALLOC)(printf("  Remove v%d\n", i));
    FOR_N(j, 0, ra->ifg_stride) {
        uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
        if (bits == 0) continue;

        FOR_N(k, 0, 64) if ((bits >> k) & 1) {
            size_t id = j*64 + k;
            if (!test_n_set(visited, id)) {
                assert(ws_cnt < ra->ifg_len);
                ws[ws_cnt++] = id;
            }
            // TB_OPTDEBUG(REGALLOC)(printf("  remove V%zu\n", id));

            assert(ra->degree[id] > 0);
            ifg_remove(ra, id, i);
            ra->degree[id] -= 1;
        }

        // reset all the bits
        ra->ifg[i*ra->ifg_stride + j] = 0;
    }
    ra->degree[i] = 0;
    return ws_cnt;
}

static bool ifg_empty(Chaitin* ra) {
    FOR_N(i, 0, ra->ifg_len) {
        if (ra->degree[i]) return false;
    }

    return true;
}

VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n) {
    int i = aarray_length(ctx->vregs);
    aarray_insert(ctx->vreg_map, n->gvn, i);
    aarray_push(ctx->vregs, (VReg){ .n = n, .assigned = -1, .spill_cost = NAN });
    return &ctx->vregs[i];
}

void tb__dump(MachineBB* mbb) {
    printf("DUMP:\n");
    aarray_for(i, mbb->items) {
        printf("  v%u\n", mbb->items[i]->gvn);
    }
    printf("\n");
}

MachineBB* tb__insert(Ctx* ctx, TB_Function* f, TB_BasicBlock* bb, TB_Node* n) {
    if (f->node_count >= f->scheduled_n) {
        TB_BasicBlock** new_sched = tb_arena_alloc(f->arena, 2 * f->scheduled_n * sizeof(TB_BasicBlock*));
        memcpy(new_sched, f->scheduled, f->scheduled_n * sizeof(TB_BasicBlock*));
        FOR_N(i, f->scheduled_n, 2 * f->scheduled_n) {
            new_sched[i] = NULL;
        }
        f->scheduled = new_sched;
        f->scheduled_n *= 2;
    }

    assert(bb);
    f->scheduled[n->gvn] = bb;
    return &ctx->machine_bbs[bb->order];
}

void tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n) {
    TB_BasicBlock* bb = f->scheduled[before_n->gvn];
    MachineBB* mbb = tb__insert(ctx, f, bb, n);

    size_t i = 0, cnt = aarray_length(mbb->items);
    while (i < cnt && mbb->items[i] != before_n) { i++; }

    aarray_push(mbb->items, 0);
    memmove(&mbb->items[i + 1], &mbb->items[i], (cnt - i) * sizeof(TB_Node*));
    mbb->items[i] = n;
    nl_hashset_put2(&bb->items, n, tb__node_hash, tb__node_cmp);
}

void tb__remove_node(Ctx* ctx, TB_Function* f, TB_Node* n) {
    TB_BasicBlock* bb = f->scheduled[n->gvn];
    MachineBB* mbb = &ctx->machine_bbs[bb->order];

    size_t i = 0, cnt = aarray_length(mbb->items);
    while (i < cnt && mbb->items[i] != n) { i++; }

    memmove(&mbb->items[i], &mbb->items[i + 1], (cnt - (i + 1)) * sizeof(TB_Node*));
    aarray_pop(mbb->items);
    f->scheduled[n->gvn] = NULL;
    nl_hashset_remove2(&bb->items, n, tb__node_hash, tb__node_cmp);
}

void tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* after_n) {
    TB_BasicBlock* bb = f->scheduled[after_n->gvn];
    MachineBB* mbb = tb__insert(ctx, f, bb, n);

    size_t i = 0, cnt = aarray_length(mbb->items);
    while (i < cnt && mbb->items[i] != after_n) { i++; }

    assert(i != cnt);
    i += 1;

    aarray_push(mbb->items, NULL);
    memmove(&mbb->items[i + 1], &mbb->items[i], (cnt - i) * sizeof(TB_Node*));
    mbb->items[i] = n;
    nl_hashset_put2(&bb->items, n, tb__node_hash, tb__node_cmp);
}

RegMask* tb__reg_mask_meet(Ctx* ctx, RegMask* a, RegMask* b) {
    // a /\ a = a
    if (a == b) { return a; }
    // a /\ TOP = a
    if (a == NULL) { return b; }
    if (b == NULL) { return a; }
    // if they both may spill, we can intersect on the stack
    bool may_spill = a->may_spill && b->may_spill;
    // a /\ b = BOT if their masks disagree
    if (!may_spill && a->class != b->class) { return &TB_REG_EMPTY; }
    // if it's stack and both don't ask for a slot... we're good
    // a /\ b = intersect masks
    assert(a->count == b->count);
    assert(a->count == 1);
    uint64_t i = a->mask[0] & b->mask[0];
    return intern_regmask(ctx, i == 0 ? 1 : a->class, may_spill, i);
}

static bool reg_mask_may_stack(RegMask* a) {
    return a->class == REG_CLASS_STK || a->may_spill;
}

static void interfere_live(Ctx* restrict ctx, Chaitin* ra, Set* live, int vreg_id) {
    RegMask* vreg_mask = ctx->vregs[vreg_id].mask;
    FOR_N(k, 1, ra->ifg_len) {
        TB_Node* kn = ctx->vregs[k].n;
        if (kn && set_get(live, kn->gvn) && reg_mask_may_intersect(vreg_mask, ctx->vregs[k].mask)) {
            // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%td\n", vreg_id, k));
            ifg_edge(ra, vreg_id, k);
        }
    }
}

static void interfere_mask(Ctx* restrict ctx, Chaitin* ra, int vreg_id) {
    RegMask* def_mask = ctx->vregs[vreg_id].mask;

    // definition should interfere with each physical reg's node it doesn't intersect with
    // such that it can't be seen alive in those registers.
    if (def_mask->class > 0 && reg_mask_is_not_empty(def_mask)) {
        size_t reg_count    = ctx->num_regs[def_mask->class];
        uint64_t word_count = (reg_count + 63) / 64;
        FOR_N(i, 0, word_count) {
            size_t j = i*64, k = j + 64;
            uint64_t mask = ~def_mask->mask[i];
            for (; j < k && j < reg_count; j++) {
                if (mask & 1) {
                    int in_vreg_id = ra->fixed[def_mask->class] + j;
                    // TB_OPTDEBUG(REGALLOC)(printf("  V%d -- V%d\n", vreg_id, in_vreg_id));
                    ifg_edge(ra, in_vreg_id, vreg_id);
                }
                mask >>= 1;
            }
        }
    }
}

static void build_ifg(Ctx* restrict ctx, TB_Arena* arena, Chaitin* ra) {
    TB_Function* f = ctx->f;

    ra->ifg_len    = aarray_length(ctx->vregs);
    ra->ifg_stride = (ra->ifg_len + 63) / 64;
    ra->ifg        = tb_arena_alloc(arena, ra->ifg_len * ra->ifg_stride * sizeof(uint64_t));
    ra->degree     = tb_arena_alloc(arena, ra->ifg_len * sizeof(int));

    memset(ra->ifg,    0, ra->ifg_len * ra->ifg_stride * sizeof(uint64_t));
    memset(ra->degree, 0, ra->ifg_len * sizeof(int));

    // fixed vregs interfere with their fellow fixed vregs
    FOR_N(i, 0, ctx->num_classes) {
        int base = ra->fixed[i];
        FOR_N(j, 0, ctx->num_regs[i]) FOR_N(k, j + 1, ctx->num_regs[i]) {
            ifg_edge(ra, base + k, base + j);
        }
    }

    Set live = set_create_in_arena(arena, f->node_count);
    FOR_REV_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        TB_BasicBlock* bb = f->scheduled[mbb->n->gvn];

        set_copy(&live, &bb->live_out);

        size_t item_count = aarray_length(mbb->items);
        FOR_REV_N(j, 0, item_count) {
            TB_Node* n = mbb->items[j];
            int vreg_id = ctx->vreg_map[n->gvn];

            if (vreg_id > 0) {
                VReg* vreg = &ctx->vregs[vreg_id];
                RegMask* def_mask = vreg->mask;

                set_remove(&live, n->gvn);
                interfere_live(ctx, ra, &live, vreg_id);
                interfere_mask(ctx, ra, vreg_id);

                // 2 address ops will interfere with their own inputs (except for
                // shared dst/src)
                int shared_edge = ctx->node_2addr(n);
                if (shared_edge >= 0) {
                    assert(shared_edge < n->input_count);
                    FOR_N(k, 1, n->input_count) if (k != shared_edge) {
                        TB_Node* in = n->inputs[k];
                        VReg* in_vreg = node_vreg(ctx, in);
                        if (in_vreg && reg_mask_may_intersect(def_mask, in_vreg->mask)) {
                            int in_vreg_id = in_vreg - ctx->vregs;
                            // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%d\n", vreg_id, in_vreg_id));
                            ifg_edge(ra, in_vreg_id, vreg_id);
                        }
                    }
                }

                RegMask** ins = tb_arena_alloc(arena, n->input_count * sizeof(RegMask*));
                ctx->constraint(ctx, n, ins);

                FOR_N(j, 1, n->input_count) {
                    TB_Node* in = n->inputs[j];
                    if (ins[j] != &TB_REG_EMPTY) {
                        VReg* in_vreg = node_vreg(ctx, in);
                        assert(in_vreg);

                        // intersect use masks with the vreg's mask, if it becomes empty we've
                        // got a hard-split (not necessarily spilling to the stack)
                        RegMask* new_mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[j]);
                        if (in_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                            TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs));
                            dyn_array_put(ra->spills, in_vreg - ctx->vregs);
                        }
                        in_vreg->mask = new_mask;

                        // uses are live now
                        set_put(&live, in->gvn);
                    }
                }
                tb_arena_free(arena, ins, n->input_count * sizeof(RegMask*));
            } else {
                // uses are live now
                FOR_N(j, 0, n->input_count) {
                    if (n->inputs[j]) { set_put(&live, n->inputs[j]->gvn); }
                }
            }

            int tmp_count = ctx->tmp_count(ctx, n);
            if (tmp_count > 0) {
                Tmps* tmps = nl_table_get(&ctx->tmps_map, n);
                assert(tmps && tmps->count == tmp_count);

                // temporaries basically just live across this instruction alone (thus intereferes
                // with the inputs)
                FOR_N(k, 0, tmps->count) {
                    interfere_live(ctx, ra, &live, tmps->elems[k]);
                    interfere_mask(ctx, ra, tmps->elems[k]);
                }
            }
        }
    }

    ifg_square(ra);

    // conservative coalescing
    /*FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        TB_BasicBlock* bb = f->scheduled[mbb->n->gvn];

        size_t item_count = aarray_length(mbb->items);
        FOR_N(j, 0, item_count) {
            TB_Node* n = mbb->items[j];
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id <= 0) { continue; }

            int shared_edge = ctx->node_2addr(n);
            if (shared_edge >= 0) {
                VReg* vreg = &ctx->vregs[vreg_id];

                TB_Node* in = n->inputs[shared_edge];
                VReg* in_vreg = node_vreg(ctx, in);
                if (in_vreg == vreg) { continue; }

                if (in_vreg && vreg->mask->class != REG_CLASS_STK && in_vreg->mask->class == vreg->mask->class) {
                    RegMask* intersect = tb__reg_mask_meet(ctx, vreg->mask, in_vreg->mask);
                    int k_limit = ctx->num_regs[vreg->mask->class];

                    // this way we guarentee coalescing doesn't introduce spills
                    if (ra->degree[in_vreg - ctx->vregs] + ra->degree[vreg_id] < k_limit && !ifg_test(ra, vreg_id, in_vreg - ctx->vregs)) {
                        ifg_union(ra, vreg_id, in_vreg - ctx->vregs);
                        vreg->coalesced++;
                        ctx->vreg_map[in->gvn] = vreg_id;
                    }
                }
            }
        }
    }*/
}

static void chaitin_print_vreg(Ctx* restrict ctx, Chaitin* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4"PRIdPTR" deg=%d cost=%.2f ", vreg - ctx->vregs, ra->degree[vreg - ctx->vregs], cost);
    tb__print_regmask(vreg->mask);
    if (vreg->coalesced > 0) {
        printf(" (%d DEFS)", vreg->coalesced + 1);
    }
    printf("\n");
    if (vreg->n) {
        printf("#   ");
        tb_print_dumb_node(NULL, vreg->n);
        printf("\n");
    }
}

// returns the stack size (0 on failure).
static int simplify(Ctx* restrict ctx, Chaitin* restrict ra, int* stk, int cap) {
    // really dumb worklist (so we're not poking at the entire IFG each time
    // we need to check on certain bits)
    uint32_t* visited    = tb_arena_alloc(ra->arena, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
    uint32_t* visited_lo = tb_arena_alloc(ra->arena, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
    uint32_t* visited_hi = tb_arena_alloc(ra->arena, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
    int* ws = tb_arena_alloc(ra->arena, ra->ifg_len * sizeof(uint32_t));
    int ws_cnt = 0;

    memset(visited,    0, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
    memset(visited_lo, 0, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
    memset(visited_hi, 0, ((ra->ifg_len+31)/32) * sizeof(uint32_t));

    // initial fill of the vregs
    FOR_N(i, 0, ctx->f->node_count) {
        int vreg_id = ctx->vreg_map[i];
        if (vreg_id > 0 && !test_n_set(visited, vreg_id)) {
            ws[ws_cnt++] = vreg_id;
        }
    }

    FOR_N(i, 0, ctx->num_classes) {
        int base = ra->fixed[i];
        FOR_N(j, 0, ctx->num_regs[i]) {
            test_n_set(visited, base + j);
            ws[ws_cnt++] = base + j;
        }
    }

    // temporaries can't coalesce
    nl_table_for(it, &ctx->tmps_map) {
        // used for clobbers/scratch but more importantly they're not bound to a node.
        Tmps* tmps = it->v;
        FOR_N(i, 0, tmps->count) {
            test_n_set(visited, tmps->elems[i]);
            ws[ws_cnt++] = tmps->elems[i];
        }
    }

    #if TB_OPTDEBUG_REGALLOC
    printf("###############################\n");
    printf("# simplify phase              #\n");
    printf("###############################\n");
    #endif

    int cnt = 0;
    for (;;) {
        // find known colorable nodes (degree < k)
        while (ws_cnt) {
            int vreg_id = ws[--ws_cnt];
            visited[vreg_id / 32] &= ~(1u << (vreg_id % 32));

            VReg* vreg = &ctx->vregs[vreg_id];
            int d = ra->degree[vreg_id];

            #if TB_OPTDEBUG_REGALLOC
            chaitin_print_vreg(ctx, ra, vreg);
            #endif

            // note the stack has infinite colors so it never spills
            if (reg_mask_is_stack(vreg->mask) || d < ctx->num_regs[vreg->mask->class]) {
                // don't wanna add twice but also we've got a lot of fucking visited sets now lol
                if (!test_n_set(visited_lo, vreg_id)) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   colorable!\n"));

                    assert(cnt < cap);
                    stk[cnt++] = vreg_id;

                    if (vreg->mask->class == REG_CLASS_STK || vreg->mask->may_spill) {
                        // TODO(NeGate): assuming 1 stack slot per vreg (will become incorrect later on)
                        ra->max_spills += 1;
                    }
                }

                // unmark as hi-degree
                if (test_n_reset(visited_hi, vreg_id)) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   used to be uncolorable, we fixed that!\n"));
                    FOR_N(i, 0, dyn_array_length(ra->spills)) {
                        if (ra->spills[i] == vreg_id) {
                            dyn_array_remove(ra->spills, i);
                            break;
                        }
                    }
                }

                // any edges now removed will add the connected node to check degrees again
                ws_cnt = ifg_remove_edges(ra, ws, ws_cnt, visited, vreg_id);
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#   uncolorable!\n"));

                // mark as hi-degree
                if (!test_n_set(visited_hi, vreg_id)) {
                    dyn_array_put(ra->spills, vreg_id);
                }
            }
        }

        // nothing in high-degree? ok the IFG must actually be empty then
        if (dyn_array_length(ra->spills) == 0) {
            return cnt;
        }

        TB_OPTDEBUG(REGALLOC)(printf("#  searching for spill candidate!\n"));

        // ok we've got too much pressure, let's split a bit
        int best_spill = -1;
        float best_score = INFINITY;

        // pick next best spill
        FOR_N(i, 0, dyn_array_length(ra->spills)) {
            VReg* vreg  = &ctx->vregs[ra->spills[i]];
            float score = get_spill_cost(ctx, vreg) / ra->degree[ra->spills[i]];
            if (score < best_score) {
                if (best_spill >= 0) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   V%d is a better spill! V%d (%f is better than %f)\n", ra->spills[i], best_spill, score, best_score));
                } else {
                    TB_OPTDEBUG(REGALLOC)(printf("#   V%d is... one of the spills of all time! %f\n", ra->spills[i], score));
                }
                best_score = score;
                best_spill = ra->spills[i];
            }
        }
        assert(best_spill >= 0);

        // optimistically simplify
        ws_cnt = ifg_remove_edges(ra, ws, ws_cnt, visited, best_spill);
        ws[ws_cnt++] = best_spill;

        // TODO(NeGate): assuming 1 stack slot per vreg (will become incorrect later on)
        ra->max_spills += 1;
    }
}

void tb__chaitin(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    Chaitin ra = { .ctx = ctx, .arena = arena };
    ra.spills = dyn_array_create(int, 32);

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        ra.fixed    = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));
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
                        .spill_cost = INFINITY
                    });
            }
            ra.fixed[i] = base;
        }
    }

    // prolly wanna track all the temporaries early (this doesn't change across
    // coloring attempts)
    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        for (size_t j = 0; j < aarray_length(mbb->items); j++) {
            TB_Node* n = mbb->items[j];
            int tmp_count = ctx->tmp_count(ctx, n);
            int in_count = n->input_count;

            RegMask** ins = ctx->ins;
            ctx->constraint(ctx, n, ins);

            if (tmp_count > 0) {
                // used for clobbers/scratch but more importantly they're not bound to a node.
                Tmps* tmps  = tb_arena_alloc(arena, sizeof(Tmps) + tmp_count*sizeof(int));
                tmps->count = tmp_count;
                nl_table_put(&ctx->tmps_map, n, tmps);

                FOR_N(k, in_count, in_count + tmp_count) {
                    RegMask* in_mask = ins[k];
                    assert(in_mask != &TB_REG_EMPTY);

                    int fixed = fixed_reg_mask(in_mask);
                    if (fixed >= 0) {
                        // insert new range to the existing vreg
                        tmps->elems[k - in_count] = ra.fixed[in_mask->class] + fixed;
                    } else {
                        tmps->elems[k - in_count] = aarray_length(ctx->vregs);
                        aarray_push(ctx->vregs, (VReg){ .mask = in_mask, .assigned = -1, .spill_cost = INFINITY });
                    }
                }
            }
        }
    }

    // simplify/select stack
    int cnt;
    int* stk; // live interval indices

    TB_ArenaSavepoint sp;
    for (;;) {
        sp = tb_arena_save(arena);
        log_debug("%s: chaitin: building IFG", f->super.name);

        // build IFG (and degree table)
        CUIK_TIMED_BLOCK("build IFG") {
            build_ifg(ctx, arena, &ra);
        }

        if (dyn_array_length(ra.spills) > 0) {
            CUIK_TIMED_BLOCK("hard splits") {
                // insert hard split code
                FOR_N(i, 0, dyn_array_length(ra.spills)) {
                    VReg* vreg = &ctx->vregs[ra.spills[i]];
                    RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
                    spill_entire_lifetime(ctx, vreg, mask, true);
                }
                dyn_array_clear(ra.spills);
                tb_arena_restore(arena, sp);

                // recompute liveness
                redo_dataflow(ctx, arena);
            }
            continue;
        }

        // clone before doing all the fancy node removals
        uint64_t* ifg_copy = tb_arena_alloc(arena, ra.ifg_len * ra.ifg_stride * sizeof(uint64_t));
        int* deg_copy      = tb_arena_alloc(arena, ra.ifg_len * sizeof(int));
        memcpy(ifg_copy, ra.ifg,    ra.ifg_len * ra.ifg_stride * sizeof(uint64_t));
        memcpy(deg_copy, ra.degree, ra.ifg_len * sizeof(int));

        log_debug("%s: chaitin: let's try to simplify %d nodes", f->super.name, ra.ifg_len);
        ra.max_spills = 0;

        // simplify (accumulates potential spills)
        stk = tb_arena_alloc(arena, ra.ifg_len * sizeof(VReg*));
        cnt = simplify(ctx, &ra, stk, ra.ifg_len);

        ra.ifg    = ifg_copy;
        ra.degree = deg_copy;

        ifg_dump(&ra);

        int highest_stack_slot = 0;
        Set live_stack = set_create_in_arena(arena, ra.max_spills);

        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("# select phase                #\n");
        printf("###############################\n");
        #endif

        // optimistic select phase
        while (cnt) {
            int vreg_id = stk[--cnt];
            VReg* vreg  = &ctx->vregs[vreg_id];

            #if TB_OPTDEBUG_REGALLOC
            chaitin_print_vreg(ctx, &ra, vreg);
            #endif

            // coloring stack slots
            if (reg_mask_is_spill(vreg->mask)) {
                ptrdiff_t empty_slot = set_pop_any(&live_stack);
                if (empty_slot + 1 > highest_stack_slot) {
                    highest_stack_slot = empty_slot + 1;
                }

                TB_OPTDEBUG(REGALLOC)(printf("#   assign to [BP - %zu]\n", 8 + empty_slot*8));
                vreg->class    = REG_CLASS_STK;
                vreg->assigned = STACK_BASE_REG_NAMES + empty_slot;
                continue;
            }

            uint64_t mask = vreg->mask->mask[0];

            int def_class = vreg->mask->class;
            FOR_N(j, 0, ra.ifg_stride) {
                uint64_t bits = ra.ifg[vreg_id*ra.ifg_stride + j];
                if (bits == 0) continue;

                FOR_N(k, 0, 64) if ((bits >> k) & 1) {
                    int other_id = j*64 + k;
                    VReg* other = &ctx->vregs[other_id];
                    assert(other->mask->class == def_class);

                    int assigned = other->assigned;
                    if (assigned >= 0) {
                        mask &= ~(1ull << assigned);
                        TB_OPTDEBUG(REGALLOC)(printf("  => %#08"PRIx64" (we can't be R%d because V%d)\n", mask, assigned, other_id));
                        if (mask == 0) { goto leave; }
                    }
                }
            }

            leave:
            if (mask == 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#   assigned UNCOLORED\n"));

                // failed to color.. sadge
                vreg->class    = 0;
                vreg->assigned = -1;
                dyn_array_put(ra.spills, vreg_id);
            } else {
                int hint_reg = -1;
                if (vreg->hint_vreg >= 0 && ctx->vregs[vreg->hint_vreg].class == def_class) {
                    hint_reg = ctx->vregs[vreg->hint_vreg].assigned;
                }

                if (hint_reg >= 0 && (mask & (1ull << hint_reg)) == 0) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(def_class, hint_reg), printf("(hinted from V%d)\n", vreg->hint_vreg));

                    // biased coloring
                    vreg->class    = def_class;
                    vreg->assigned = 1ull << hint_reg;
                } else {
                    vreg->class    = def_class;
                    vreg->assigned = tb_ffs64(mask) - 1;
                    TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(def_class, vreg->assigned), printf("\n"));
                }
            }
        }

        if (dyn_array_length(ra.spills) == 0) {
            log_debug("%s: tmp_arena=%.1f KiB (good coloring)", f->super.name, tb_arena_current_size(arena) / 1024.0f);
            dyn_array_destroy(ra.spills);
            ctx->num_spills += highest_stack_slot;
            break;
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("# spill phase                 #\n");
        printf("###############################\n");
        #endif

        // if anything in the spill candidates failed to color, we need to spill. note that
        // it wouldn't have been able to fail coloring without making it to this list.
        FOR_N(i, 0, dyn_array_length(ra.spills)) {
            int vreg_id = ra.spills[i];
            TB_Node* n  = ctx->vregs[vreg_id].n;
            RegMask* vreg_mask = ctx->vregs[vreg_id].mask;

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# V%d: SPILL (%%%u)\x1b[0m\n", vreg_id, n->gvn));

            // rematerialization candidates will delete the original def and for now, they'll
            // reload per use site (although we might wanna coalesce some later on).
            if (n->type == TB_ICONST || n->type == TB_F32CONST || n->type == TB_F64CONST) {
                rematerialize(ctx, NULL, n);
            } else {
                ctx->vregs[vreg_id].mask = ctx->constraint(ctx, n, NULL);
                ctx->vregs[vreg_id].spill_cost = NAN;

                RegMask* spill_rm = intern_regmask(ctx, 1, true, 0);
                TB_Node* spill_n  = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                subsume_node2(f, n, spill_n);
                set_input(f, spill_n, n, 1);
                TB_NODE_SET_EXTRA(spill_n, TB_NodeMachCopy, .def = spill_rm, .use = vreg_mask);

                tb__insert_after(ctx, f, spill_n, n);
                VReg* spill_vreg = tb__set_node_vreg(ctx, spill_n);
                spill_vreg->spill_cost = INFINITY;
                spill_entire_lifetime(ctx, spill_vreg, spill_rm, false);
            }
        }
        // time to retry
        dyn_array_clear(ra.spills);
        log_debug("%s: tmp_arena=%.1f KiB (failed colors)", f->super.name, tb_arena_current_size(arena) / 1024.0f);
        tb_arena_restore(arena, sp);

        // recompute liveness
        dump_sched(ctx);
        redo_dataflow(ctx, arena);
    }
}
