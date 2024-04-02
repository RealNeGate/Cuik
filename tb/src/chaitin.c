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
    int* num_regs;
    uint64_t* callee_saved;
    RegMask* normie_mask;

    // We accumulate spills in the simplify & select phases on failure
    DynArray(int) spills;

    // Interference graph
    size_t ifg_stride;
    size_t ifg_len;
    uint64_t* ifg;
    int* degree;
} Chaitin;

static void ifg_edge(Chaitin* ra, int i, int j) {
    ra->ifg[i*ra->ifg_stride + j/64] |= 1ull << (j % 64);
}

static bool ifg_test(Chaitin* ra, int i, int j) {
    return ra->ifg[i*ra->ifg_stride + j/64] & (1ull << (j % 64));
}

static void ifg_remove(Chaitin* ra, int i, int j) {
    printf("remove V%d -- V%d\n", i, j);
    assert(ifg_test(ra, i, j));
    ra->ifg[i*ra->ifg_stride + j/64] &= ~(1ull << (j % 64));
}

static void ifg_dump(Chaitin* ra) {
    FOR_N(i, 0, ra->ifg_len) {
        FOR_N(j, 0, ra->ifg_stride) {
            uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
            if (bits == 0) continue;

            FOR_N(k, 0, 64) if ((bits >> k) & 1) {
                printf("V%zu -- V%zu\n", j*64 + k, i);
            }
        }
    }
}

static void ifg_remove_edges(Chaitin* ra, int i) {
    FOR_N(j, 0, ra->ifg_stride) {
        uint64_t bits = ra->ifg[i*ra->ifg_stride + j];
        if (bits == 0) continue;

        FOR_N(k, 0, 64) if ((bits >> k) & 1) {
            ifg_remove(ra, j*64 + k, i);
            ra->degree[j*64 + k] -= 1;
        }

        // reset all the bits
        ra->ifg[i*ra->ifg_stride + j] = 0;
    }
    ra->degree[i] = 0;
}

static bool ifg_empty(Chaitin* ra) {
    FOR_N(i, 0, ra->ifg_len) {
        if (ra->degree[i]) return false;
    }

    return true;
}

static void* resize_stuff(void* ptr, size_t old) {
    abort();
    return NULL;
}

VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n) {
    int i = aarray_length(ctx->vregs);
    aarray_insert(ctx->vreg_map, n->gvn, i);
    aarray_push(ctx->vregs, (VReg){ .n = n, .assigned = -1 });
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
        TB_BasicBlock** new_sched = tb_arena_alloc(f->tmp_arena, 2 * f->scheduled_n * sizeof(TB_BasicBlock*));
        memcpy(new_sched, f->scheduled, f->scheduled_n * sizeof(TB_BasicBlock*));
        FOR_N(i, f->scheduled_n, 2 * f->scheduled_n) {
            new_sched[i] = NULL;
        }
        f->scheduled = new_sched;
        f->scheduled_n *= 2;
    }

    f->scheduled[n->gvn] = bb;
    return &ctx->machine_bbs[bb->order];
}

void tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n) {
    MachineBB* mbb = tb__insert(ctx, f, f->scheduled[before_n->gvn], n);

    size_t i = 0, cnt = aarray_length(mbb->items);
    while (i < cnt && mbb->items[i] != before_n) { i++; }

    aarray_push(mbb->items, 0);
    memmove(&mbb->items[i + 1], &mbb->items[i], (cnt - i) * sizeof(TB_Node*));
    mbb->items[i] = n;
}

void tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* after_n) {
    MachineBB* mbb = tb__insert(ctx, f, f->scheduled[after_n->gvn], n);

    size_t i = 0, cnt = aarray_length(mbb->items);
    while (i < cnt && mbb->items[i] != after_n) { i++; }

    assert(i != cnt);
    i += 1;

    aarray_push(mbb->items, NULL);
    memmove(&mbb->items[i + 1], &mbb->items[i], (cnt - i) * sizeof(TB_Node*));
    mbb->items[i] = n;
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

static float node_cost(Ctx* ctx, TB_Node* n) {
    // let's assume all frequencies are "1" for now, eventually we'll use
    // loop info and PGO to do better.
    float c = 1.0f;
    FOR_USERS(u, n) { c += 1.0f; }
    return c;
}

static bool reg_mask_may_stack(RegMask* a) {
    return a->class == REG_CLASS_STK || a->may_spill;
}

static void build_ifg(Ctx* restrict ctx, TB_Arena* arena, Chaitin* ra) {
    TB_Function* f = ctx->f;

    ra->ifg_len    = aarray_length(ctx->vregs);
    ra->ifg_stride = (ra->ifg_len + 63) / 64;
    ra->ifg        = tb_arena_alloc(arena, ra->ifg_len * ra->ifg_stride * sizeof(uint64_t));
    ra->degree     = tb_arena_alloc(arena, ra->ifg_len * sizeof(int));

    memset(ra->ifg,    0, ra->ifg_len * ra->ifg_stride * sizeof(uint64_t));
    memset(ra->degree, 0, ra->ifg_len * sizeof(int));

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

                // interfere
                FOR_N(k, 1, ra->ifg_len) {
                    TB_Node* kn = ctx->vregs[k].n;
                    if (set_get(&live, kn->gvn) && reg_mask_may_intersect(def_mask, ctx->vregs[k].mask)) {
                        printf("V%d -- V%td\n", vreg_id, k);
                        ifg_edge(ra, vreg_id, k);
                        ifg_edge(ra, k, vreg_id);
                    }
                }

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
                            printf("V%d -- V%d\n", vreg_id, in_vreg_id);
                            ifg_edge(ra, vreg_id, in_vreg_id);
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
                        in_vreg->mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[j]);
                        if (in_vreg->mask == &TB_REG_EMPTY) {
                            printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs);
                            dyn_array_put(ra->spills, in_vreg - ctx->vregs);
                        }

                        // uses are live now
                        set_put(&live, in->gvn);
                    }
                }
                tb_arena_free(arena, ins, n->input_count * sizeof(RegMask*));
            } else {
                // uses are live now
                FOR_N(j, 0, n->input_count) {
                    if (n->inputs[i]) { set_put(&live, n->inputs[i]->gvn); }
                }
            }
        }

        // compute degree
        FOR_N(i, 0, ra->ifg_len) {
            int sum = 0;
            FOR_N(j, 0, ra->ifg_stride) {
                sum += tb_popcount64(ra->ifg[i*ra->ifg_stride + j]);
            }
            ra->degree[i] = sum;
        }
    }
}

// returns the stack size (0 on failure).
static int simplify(Ctx* restrict ctx, Chaitin* restrict ra, int* stk, int cap) {
    int cnt = 0;

    FOR_N(i, 1, ra->ifg_len) {
        VReg* vreg = &ctx->vregs[i];
        if (vreg) {
            int d = ra->degree[i];
            // note the stack has infinite colors so we're always below the limit
            if (vreg->mask->class == REG_CLASS_STK || d < ctx->num_regs[vreg->mask->class]) {
                assert(cnt < cap);
                stk[cnt++] = i;
                ifg_remove_edges(ra, i);
            }
        }
    }

    if (!ifg_empty(ra)) {
        // ok we've got too much pressure, let's split a bit
        int best_spill = -1;
        float best_cost = FLT_MAX;

        // pick next best spill
        FOR_N(i, 1, ra->ifg_len) {
            VReg* vreg = &ctx->vregs[i];
            if (vreg && ra->degree[i] >= ctx->num_regs[vreg->mask->class]) {
                float c = node_cost(ctx, ctx->vregs[i].n);
                if (c < best_cost) {
                    printf("  better spill? v%d (%f is better than %f)\n", best_spill, c, best_cost);
                    best_cost  = c;
                    best_spill = i;
                }
            }
        }

        assert(best_spill >= 0);
        printf("  spill v%d\n", best_spill);

        dyn_array_put(ra->spills, best_spill);
        ifg_remove_edges(ra, best_spill);
        return 0;
    } else {
        return cnt;
    }
}

void tb__chaitin(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    Chaitin ra = { .ctx = ctx, .arena = arena };
    ra.spills = dyn_array_create(int, 32);

    CUIK_TIMED_BLOCK("build IFG") {
        // simplify/select stack
        int cnt;
        int* stk; // live interval indices

        bool has_spills;
        TB_ArenaSavepoint sp;
        for (;;) {
            sp = tb_arena_save(arena);

            log_debug("%s: chaitin: building IFG", f->super.name);

            // build IFG (and degree table)
            build_ifg(ctx, arena, &ra);

            // clone before doing all the fancy node removals
            uint64_t* ifg_copy = tb_arena_alloc(arena, ra.ifg_len * ra.ifg_stride * sizeof(uint64_t));
            int* deg_copy      = tb_arena_alloc(arena, ra.ifg_len * sizeof(int));
            memcpy(ifg_copy, ra.ifg,    ra.ifg_len * ra.ifg_stride * sizeof(uint64_t));
            memcpy(deg_copy, ra.degree, ra.ifg_len * sizeof(int));

            log_debug("%s: chaitin: let's try to simplify %d nodes", f->super.name, ra.ifg_len);

            // simplify (accumulates spills)
            stk = tb_arena_alloc(arena, ra.ifg_len * sizeof(VReg*));
            do {
                cnt = simplify(ctx, &ra, stk, ra.ifg_len);
            } while (cnt == 0);

            ra.ifg    = ifg_copy;
            ra.degree = deg_copy;

            if (dyn_array_length(ra.spills) > 0) {
                // insert spill code
                FOR_N(i, 0, dyn_array_length(ra.spills)) {
                    int vreg_id = ra.spills[i];
                    VReg* spill = &ctx->vregs[vreg_id];
                    TB_Node* n = spill->n;

                    // reset the out mask
                    spill->mask = ctx->constraint(ctx, n, NULL);

                    // aggressive split
                    for (size_t i = 0; i < n->user_count; i++) {
                        TB_Node* use_n = USERN(&n->users[i]);
                        int use_i      = USERI(&n->users[i]);

                        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

                        // reload per use site
                        TB_Node* reload_n = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                        set_input(f, use_n, reload_n, use_i);
                        TB_NODE_SET_EXTRA(reload_n, TB_NodeMachCopy, .def = in_mask, .use = spill->mask);

                        // we'll inplace replace the user edge to avoid
                        // iterator invalidation problems.
                        #if TB_PACKED_USERS
                        #error todo
                        #else
                        n->users[i]._n    = reload_n;
                        n->users[i]._slot = 1;
                        #endif
                        reload_n->inputs[1] = n;

                        // schedule the split right before use
                        tb__insert_before(ctx, ctx->f, reload_n, use_n);
                        VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n);
                        reload_vreg->mask = in_mask;
                    }
                }

                // time to retry
                dyn_array_clear(ra.spills);
                log_debug("%s: tmp_arena=%.1f KiB (failed colors)", f->super.name, tb_arena_current_size(arena) / 1024.0f);
                tb_arena_restore(arena, sp);
            } else {
                // finally... we're done
                assert(cnt > 0);
                break;
            }
        }

        // select phase
        while (cnt) {
            int vreg_id = stk[--cnt];
            VReg* vreg  = &ctx->vregs[vreg_id];

            uint64_t mask = vreg->mask->mask[0];
            printf("v%d:\n", vreg_id);
            printf("  => %#"PRIx64"\n", mask);

            int def_class = vreg->mask->class;
            FOR_N(j, 0, ra.ifg_stride) {
                uint64_t bits = ra.ifg[vreg_id*ra.ifg_stride + j];
                if (bits == 0) continue;

                FOR_N(k, 0, 64) if ((bits >> k) & 1) {
                    VReg* other = &ctx->vregs[j*64 + k];
                    assert(other->mask->class == def_class);

                    int fixed = fixed_reg_mask(other->mask);
                    if (fixed >= 0) {
                        mask &= ~(1ull << fixed);
                        printf("  => %#"PRIx64" (neighbor precolored R%d)\n", mask, fixed);
                    } else {
                        int assigned = other->assigned;
                        if (assigned >= 0) {
                            mask &= ~(1ull << assigned);
                            printf("  => %#"PRIx64" (we can't be R%d)\n", mask, assigned);
                        }
                    }
                }
            }

            assert(mask != 0 && "couldn't select color :(");
            vreg->class    = def_class;
            vreg->assigned = tb_ffs64(mask) - 1;
            printf("  => R%d\n", vreg->assigned);
        }

        log_debug("%s: tmp_arena=%.1f KiB (good coloring)", f->super.name, tb_arena_current_size(arena) / 1024.0f);
        tb_arena_restore(arena, sp);
    }

    // ctx->stack_usage += (ra.spills - ctx->num_regs[0]) * 8;
}

