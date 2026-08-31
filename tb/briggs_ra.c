// Mostly implementing Briggs' thesis "Register Allocation via Graph Coloring"
//
// Also:
//   https://cr.openjdk.org/~thartmann/offsite_2018/register_allocator.pdf
//
// Oh and special thanks to Cliff for advice
#include "codegen.h"
#include <float.h>

enum {
    BRIGGS_WORDS_PER_CHUNK = 128 / sizeof(uint64_t),
    BRIGGS_BITS_PER_CHUNK = BRIGGS_WORDS_PER_CHUNK * 32,
};

// 128B worth of bits
typedef struct Briggs_Chunk Briggs_Chunk;
struct Briggs_Chunk {
    uint64_t bits[BRIGGS_WORDS_PER_CHUNK];
};

typedef struct {
    Briggs_Chunk* chunks[0];
} Briggs_Set;

typedef struct {
    int curr, max;
    // index in the block where the pressure went from lo->hi
    int lo2hi;
} Briggs_Pressure;

typedef struct {
    RABase base;

    // for arena memory usage debooging
    size_t baseline;
    int max_spills;

    // Bit matrix IFG
    size_t ifg_len;
    size_t ifg_chunks_per_set;
    Briggs_Set** ifg;

    // Adjancency list IFG
    ArenaArray(uint32_t)* adj;

    // Splitting crap:
    //   [def_i*num_spills + spill_i]
    TB_Node** defs;
    TB_Node** phis;
    int* spill_vregs;

    RegMask** spill_mask;
    RegMask** reload_mask;

    uint64_t* spilled;
} Briggs;

typedef struct {
    uint32_t vreg_id;
    // degree before removal
    uint32_t degree;
} SimplifiedElem;

static void ifg_build(Ctx* restrict ctx, Briggs* ra, bool clobber);
static void ifg_raw_edge(Briggs* ra, int i, int j);
static bool ifg_coalesce(Ctx* restrict ctx, Briggs* ra, bool aggro);
static ArenaArray(SimplifiedElem) ifg_simplify(Ctx* restrict ctx, Briggs* ra);

static bool bits64_test(uint64_t* arr, size_t x) {
    uint64_t y = arr[x / 64];
    return y & (1ull << (x % 64));
}

static bool bits64_test_n_set(uint64_t* arr, size_t x) {
    uint64_t y = arr[x / 64];
    arr[x / 64] = y | (1ull << (x % 64));
    return y & (1ull << (x % 64));
}

static void bits64_remove(uint64_t* arr, size_t x) {
    arr[x / 64] &= ~(1ull << (x % 64));
}

static int ifg_raw_degree(Briggs* ra, int i) {
    return ra->adj[i][0];
}

static int ifg_degree(Ctx* ctx, Briggs* ra, int i) {
    int d = 0, w = ctx->vregs[i].reg_width;
    FOR_N(j, 1, ra->adj[i][0]+1) {
        int other = ra->adj[i][j];
        d += TB_MAX(w, ctx->vregs[other].reg_width);
    }
    return d;
}

static void ifg_print(Ctx* restrict ctx, Briggs* ra, int i) {
    int d = ra->adj[i][0];
    FOR_N(j, 1, d+1) {
        TB_OPTDEBUG(REGALLOC)(printf("V%-4d -- V%-4d\n", i, ra->adj[i][j]));
    }
}

static IFG_Worklist ifg_ws_alloc(Ctx* restrict ctx, TB_Arena* arena, int len) {
    IFG_Worklist ws = { 0 };
    ws.count = 0;
    ws.visited = tb_arena_alloc(arena, ((len+63)/64) * sizeof(uint64_t)),
    ws.stack = tb_arena_alloc(arena, len * sizeof(uint64_t));
    memset(ws.visited, 0, ((len+63)/64) * sizeof(uint64_t));
    return ws;
}

static void ifg_ws_remove(IFG_Worklist* ws, int vreg_id) {
    if (ws->count == 1) {
        TB_ASSERT(ws->stack[0] == vreg_id);
        ws->count = 0;
        return;
    }

    // remove-swap
    FOR_N(i, 0, ws->count) {
        if (ws->stack[i] == vreg_id) {
            bits64_remove(ws->visited, vreg_id);
            ws->stack[i] = ws->stack[--ws->count];
            return;
        }
    }

    TB_ASSERT(0); // not found
}

static bool ifg_ws_push(IFG_Worklist* ws, int vreg_id) {
    if (!bits64_test_n_set(ws->visited, vreg_id)) {
        ws->stack[ws->count++] = vreg_id;
        return true;
    } else {
        return false;
    }
}

static int ifg_ws_pop(IFG_Worklist* ws) {
    if (ws->count == 0) {
        return 0;
    }

    int vreg_id = ws->stack[--ws->count];
    bits64_remove(ws->visited, vreg_id);
    return vreg_id;
}

static void briggs_print_vreg(Ctx* restrict ctx, Briggs* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4"PRIdPTR" deg=%d cost=%.2f area=%"PRId64" ", vreg - ctx->vregs, ifg_raw_degree(ra, vreg - ctx->vregs), cost, vreg->area);
    if (vreg->hint_vreg > 0) {
        printf(" hint=V%d", vreg->hint_vreg);
    }
    tb__print_regmask(&OUT_STREAM_DEFAULT, vreg->mask);
    printf("\n");
    if (vreg->n) {
        printf("#   ");
        tb_print_dumb_node(NULL, vreg->n);
        printf("\n");
    }
}

static void briggs_dump_defs(Ctx* restrict ctx, Briggs* restrict ra, int i, int num_spills) {
    printf("{");
    FOR_N(j, 0, num_spills) {
        if (ra->defs[i*num_spills + j]) {
            printf(" %%%-4u", ra->defs[i*num_spills + j]->gvn);
        } else {
            printf(" ___  ");
        }
    }
    printf(" }\n");
}

static void ra_dump_block(Ctx* restrict ctx, int old_node_count, size_t i) {
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

static void ra_dump_sched(Ctx* restrict ctx, int old_node_count) {
    FOR_N(i, 0, ctx->bb_count) {
        ra_dump_block(ctx, old_node_count, i);
    }
}

static void briggs_insert_op(Ctx* ctx, Briggs* ra, int bb_id, TB_Node* n, int pos) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

    // skip phis and projections so that they stay nice and snug
    size_t cnt = aarray_length(bb->items);
    while (pos < cnt && (IS_PROJ(bb->items[pos]) || bb->items[pos]->type == TB_PHI)) {
        pos += 1;
    }

    aarray_push(bb->items, 0);
    memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
    bb->items[pos] = n;

    FOR_N(i, 1, ctx->num_classes) {
        RAPressure* p = &ra->base.hrp[i][bb - ctx->cfg.blocks];
        if (pos >= p->lo2hi) {
            p->lo2hi += 1;
        }
    }

    tb__insert(ctx, ctx->f, bb, n);
}

static void rematerialize(Ctx* ctx, int* fixed_vregs, TB_Node* n, bool kill_node) {
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
        TB_ASSERT(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

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

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu:   use (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(&f->tmp_arena, sp);

    if (kill_node) {
        // delete the original def
        ctx->vreg_map[n->gvn] = 0;
        tb__remove_node(ctx, f, n);
        tb_kill_node(f, n);
    }
}

#define BITS64_TEST(x, i) (((x) >> (i)) & 1)

static bool ifg_member(Briggs* ra, int i, int j);
static bool briggs_interfere(Ctx* restrict ctx, RABase* ra_base, TB_Node* lhs, TB_Node* rhs) {
    int x = ctx->vreg_map[lhs->gvn];
    int y = ctx->vreg_map[rhs->gvn];
    return x != y && ifg_member((Briggs*) ra_base, x, y);
}

// First round of coalescing uses this
static void briggs_rebuild_intr(Ctx* ctx, RABase* ra) {
    ifg_build(ctx, (Briggs*) ra, false);
}

void tb__briggs(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    Briggs ra = { { .ctx = ctx, .arena = arena, .rebuild_intr = briggs_rebuild_intr, .interfere = briggs_interfere } };
    tb__ra_init(&ra.base, arena);

    size_t baseline = ra.baseline = tb_arena_current_size(arena);
    log_debug("%s: briggs: arena starting with %.1f KiB", f->super.name, baseline / 1024.0f);
    TB_OPTDEBUG(REGSPLIT)(ra_dump_sched(ctx, f->node_count));

    int rounds = 0;
    for (;;) {
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        cuikperf_region_start("round", NULL);

        // that's a lot of rounds... you sure we're making forward progress?
        rounds += 1;

        // build IFG
        CUIK_TIMED_BLOCK("IFG build") {
            log_debug("%s: briggs: building IFG", f->super.name);
            ifg_build(ctx, &ra, true);
        }
        TB_ASSERT(rounds <= 10);

        if (dyn_array_length(ra.base.splits) == 0) {
            CUIK_TIMED_BLOCK("IFG square") {
                FOR_N(i, 0, ra.ifg_len) {
                    FOR_N(j, 0, ra.ifg_chunks_per_set) {
                        Briggs_Chunk* chk = ra.ifg[i]->chunks[j];
                        if (chk == NULL) { continue; }

                        FOR_N(k, 0, BRIGGS_WORDS_PER_CHUNK) {
                            uint64_t bits = chk->bits[k];
                            while (bits) {
                                int l = tb_ffs64(bits) - 1;
                                int m = j*BRIGGS_BITS_PER_CHUNK + k*64 + l;
                                if (i < m) {
                                    ifg_raw_edge(&ra, m, i);
                                }
                                bits &= ~(1ull << l);
                            }
                        }
                    }
                }
            }

            size_t old_count = f->node_count;
            CUIK_TIMED_BLOCK("convert to adjacency") {
                ra.adj = tb_arena_alloc(arena, ra.ifg_len * sizeof(ArenaArray(uint32_t)));
                FOR_N(i, 0, ra.ifg_len) {
                    // first element is the count
                    ra.adj[i] = aarray_create(arena, uint32_t, 16);
                    aarray_push(ra.adj[i], 0);
                }

                FOR_N(i, 0, ra.ifg_len) {
                    FOR_N(j, 0, ra.ifg_chunks_per_set) {
                        Briggs_Chunk* chk = ra.ifg[i]->chunks[j];
                        if (chk == NULL) { continue; }

                        FOR_N(k, 0, BRIGGS_WORDS_PER_CHUNK) {
                            uint64_t bits = chk->bits[k];
                            while (bits) {
                                int l = tb_ffs64(bits) - 1;
                                int m = j*BRIGGS_BITS_PER_CHUNK + k*64 + l;

                                /* if (i < m) {
                                printf("V%zu -- V%d\n", i, m);
                                } */

                                // insert into adjancency list
                                aarray_push(ra.adj[i], m);
                                ra.adj[i][0] += 1;

                                bits &= ~(1ull << l);
                            }
                        }
                    }
                }
                ra.ifg = NULL;
            }

            IF_OPT(REGALLOC) {
                printf("###############################\n");
                printf("# simplify phase              #\n");
                printf("###############################\n");
            }

            // simplify (find potential spills)
            cuikperf_region_start("simplify", NULL);
            ArenaArray(SimplifiedElem) stk = ifg_simplify(ctx, &ra);
            cuikperf_region_end();

            // select (can fail to color due)
            IF_OPT(REGALLOC) {
                printf("###############################\n");
                printf("# select phase                #\n");
                printf("###############################\n");
            }

            int stack_cap = ctx->num_regs[REG_CLASS_STK] + ra.max_spills;
            int mask_cap = stack_cap < ra.base.max_regs_in_class ? ra.base.max_regs_in_class : stack_cap;
            uint64_t* mask = tb_arena_alloc(arena, ((mask_cap+63)/64) * sizeof(uint64_t));

            bool failure = false;
            int num_spills = ctx->num_regs[REG_CLASS_STK];
            cuikperf_region_start("select", NULL);
            FOR_REV_N(i, 0, aarray_length(stk)) {
                SimplifiedElem s = stk[i];
                VReg* vreg = &ctx->vregs[s.vreg_id];
                // un-yank, this will let us know how far we
                // need to undo the removals on a node
                int degree = ra.adj[s.vreg_id][0] = s.degree;

                // printf("V%u %d\n", s.vreg_id, degree);

                // nothing precolored should've landed into this stack
                TB_ASSERT(vreg->assigned < 0);

                IF_OPT(REGALLOC) {
                    briggs_print_vreg(ctx, &ra, vreg);
                }

                RegMask* rm = vreg->mask;
                int def_class = rm->class;
                size_t num_fixed_regs = ctx->num_regs[rm->class];
                size_t num_regs = def_class == REG_CLASS_STK ? stack_cap : ctx->num_regs[rm->class];

                size_t fixed_word_count = (num_fixed_regs + 63) / 64;
                size_t mask_word_count = (num_regs + 63) / 64;
                TB_ASSERT(mask_word_count <= mask_cap);

                // make sure it thinks the unusable regs are "in use" by someone else
                FOR_N(j, 0, rm->count) { mask[j] = ~rm->mask[j]; }
                FOR_N(j, rm->count, (num_fixed_regs + 63) / 64) { mask[j] = UINT64_MAX; }
                if (num_fixed_regs % 64) {
                    mask[num_fixed_regs / 64] &= UINT64_MAX >> (64ull - (num_fixed_regs % 64));
                }
                // all the above space is unblocked, this only matters for stack slot coloring
                FOR_N(j, fixed_word_count, mask_word_count) { mask[j] = 0; }

                IF_OPT(REGALLOC) {
                    printf("#\n");
                }

                FOR_N(j, 1, degree + 1) {
                    uint32_t other_id = ra.adj[s.vreg_id][j];
                    VReg* other = &ctx->vregs[other_id];
                    if (other->mask->class == def_class && other->assigned >= 0) {
                        int other_assigned = other->assigned;
                        uint64_t allot_mask = UINT64_MAX >> (64ull - other->reg_width);
                        mask[other_assigned / 64u] |= (allot_mask << (other_assigned % 64u));

                        #if 0 && TB_OPTDEBUG_REGALLOC3
                        if (def_class != 1) {
                            continue;
                        }
                        printf("#   MASK: 0x");

                        FOR_REV_N(i, 0, mask_word_count - 1) {
                            printf("%016"PRIx64, mask[i]);
                        }

                        if (num_regs % 64 != 0) {
                            char buf[16];
                            static const char hexnums[16] = "0123456789abcdef";

                            uint64_t bits = mask[mask_word_count - 1];
                            size_t l = ((num_regs % 64) + 3) / 4;
                            FOR_REV_N(k, 0, l) {
                                buf[l - (k + 1)] = hexnums[(bits >> (k*4)) & 0xF];
                            }

                            printf("%.*s", (int) l, buf);
                        }

                        printf(" (we can't be ");
                        print_reg_name(def_class, other_assigned);
                        printf(" because V%d)\n", other_id);
                        #endif
                    }
                }

                // mark as being at least ASSIGN
                if (vreg->stage == VREG_STAGE_UNDEF) {
                    vreg->stage = VREG_STAGE_ASSIGN;
                }

                if (!reg_assign(ctx, vreg, mask, num_regs)) {
                    // make any may-spills into "will-spills"
                    if (vreg->mask->may_spill && def_class != REG_CLASS_STK) {
                        vreg->spill_cost = INFINITY;
                        vreg->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                        vreg->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, vreg->n->dt);
                        TB_OPTDEBUG(REGALLOC3)(printf("#   assigned UNCOLORED (will be treated as spilled next time)\n"));
                    } else {
                        // if a stack slot failed to color then it means we
                        // need more stack slots (there's an indefinite amount :p)
                        TB_ASSERT(def_class != REG_CLASS_STK);
                        SplitDecision split = { s.vreg_id };
                        dyn_array_put(ra.base.splits, split);
                        TB_OPTDEBUG(REGALLOC3)(printf("#   assigned UNCOLORED\n"));
                    }
                    // printf("  FAILURE!\n");
                    failure = true;
                } else {
                    if (def_class == REG_CLASS_STK && vreg->assigned+vreg->reg_width > num_spills) {
                        num_spills = vreg->assigned+vreg->reg_width;
                    }

                    #if TB_OPTDEBUG_REGALLOC3
                    printf("#   assigned to ");
                    print_reg_name(vreg->class, vreg->assigned);
                    FOR_N(i, 1, vreg->reg_width) {
                        printf(", ");
                        print_reg_name(vreg->class, vreg->assigned + i);
                    }
                    printf("\n");
                    #endif
                }
            }
            cuikperf_region_end();

            if (!failure) {
                // we've successfully colored all vregs
                log_debug("%s: briggs: good coloring (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
                dyn_array_destroy(ra.base.splits);
                tb_arena_restore(arena, sp);

                ctx->num_spills += num_spills - ctx->num_regs[REG_CLASS_STK];
                cuikperf_region_end();
                break;
            }
        }

        IF_OPT(REGALLOC) {
            printf("###############################\n");
            printf("# spill phase (%4zu)          #\n", dyn_array_length(ra.base.splits));
            printf("###############################\n");
        }

        // if anything in the spill candidates failed to color, we need to spill. note that
        // it wouldn't have been able to fail coloring without making it to this list.
        cuikperf_region_start("spill", NULL);
        if (dyn_array_length(ra.base.splits) > 0) {
            size_t old_node_count = f->node_count;
            tb__insert_splits(ctx, &ra.base, ra.base.splits, dyn_array_length(ra.base.splits));
            dyn_array_clear(ra.base.splits);

            TB_OPTDEBUG(REGSPLIT)(ra_dump_sched(ctx, old_node_count));
        }
        cuikperf_region_end();

        log_debug("%s: briggs: failed to color (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
        tb_arena_restore(arena, sp);

        // recompute liveness
        redo_dataflow(ctx, arena);

        // time to retry
        dyn_array_clear(ra.base.splits);
        cuikperf_region_end();
    }
}

////////////////////////////////
// IFG construction
////////////////////////////////
static void ifg_raw_edge(Briggs* ra, int i, int j) {
    TB_ASSERT(i != j);
    Briggs_Chunk* chk = ra->ifg[i]->chunks[j / BRIGGS_BITS_PER_CHUNK];
    if (chk == NULL) {
        // first time we're writing to this chunk
        // TODO(NeGate): put these into a free list, we wanna recycle chunks as much as possible.
        chk = tb_arena_alloc(ra->base.arena, sizeof(Briggs_Chunk));
        memset(chk, 0, sizeof(Briggs_Chunk));

        ra->ifg[i]->chunks[j / BRIGGS_BITS_PER_CHUNK] = chk;
    }

    uint32_t k = j % BRIGGS_BITS_PER_CHUNK;
    chk->bits[k / 64] |= 1ull << (k % 64);
}

static void ifg_union(Briggs* ra, int dst, int src) {
    FOR_N(j, 0, ra->ifg_chunks_per_set) {
        Briggs_Chunk* chk = ra->ifg[dst]->chunks[j];
        if (ra->ifg[src]->chunks[j] == NULL) {
            continue;
        }

        uint64_t* src_bits = ra->ifg[src]->chunks[j]->bits;
        if (chk == NULL) {
            // first time we're writing to this chunk
            // TODO(NeGate): put these into a free list, we wanna recycle chunks as much as possible.
            chk = tb_arena_alloc(ra->base.arena, sizeof(Briggs_Chunk));
            memset(chk, 0, sizeof(Briggs_Chunk));

            ra->ifg[dst]->chunks[j] = chk;
        }

        // newly set
        FOR_N(k, 0, BRIGGS_WORDS_PER_CHUNK) {
            uint64_t bits = ~chk->bits[k] & src_bits[k];
            chk->bits[k] |= src_bits[k];

            while (bits) {
                int l = tb_ffs64(bits) - 1;
                int m = j*BRIGGS_BITS_PER_CHUNK + k*64 + l;
                if (dst < m) {
                    ifg_raw_edge(ra, m, dst);
                }
                bits &= ~(1ull << l);
            }
        }
    }
}

static void ifg_edge(Briggs* ra, int i, int j) {
    if (i > j) {
        SWAP(int, i, j);
    }

    TB_ASSERT(i < ra->ifg_len);
    TB_ASSERT(j < ra->ifg_len);
    ifg_raw_edge(ra, i, j);
}

static bool ifg_member(Briggs* ra, int i, int j) {
    if (i > j) {
        SWAP(int, i, j);
    }

    Briggs_Chunk* chk = ra->ifg[i]->chunks[j / BRIGGS_BITS_PER_CHUNK];
    if (chk == NULL) {
        return false;
    }

    uint64_t k = j % BRIGGS_BITS_PER_CHUNK;
    return chk->bits[k / 64] & (1ull << (k % 64));
}

static void p_up(Briggs* ra, int class, int bb_id, int pos, int limit) {
    RAPressure* p = &ra->base.hrp[class][bb_id];
    if (++p->curr > p->max) {
        p->max = p->curr;
    }
}

static void p_down(Briggs* ra, int class, int bb_id, int pos, int limit) {
    RAPressure* p = &ra->base.hrp[class][bb_id];
    p->curr--;
    if (p->curr == 16) {
        p->lo2hi = pos;
    }
}

static void interfere_live(Ctx* restrict ctx, Briggs* ra, SparseSet* active, int vreg_id) {
    RegMask* vreg_mask = ctx->vregs[vreg_id].mask;
    aarray_for(i, active->stack) {
        int other_id = ctx->vreg_map[active->stack[i]];
        if (other_id > 0 && vreg_id != other_id) {
            if (reg_mask_may_intersect(vreg_mask, ctx->vregs[other_id].mask)) {
                // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%td\n", vreg_id, k));
                ifg_edge(ra, vreg_id, other_id);
            }
        }
    }
}

static void ifg_build(Ctx* restrict ctx, Briggs* ra, bool clobbers) {
    TB_Function* f = ctx->f;

    ra->ifg_len = aarray_length(ctx->vregs);
    ra->ifg = tb_arena_alloc(ra->base.arena, ra->ifg_len * sizeof(Briggs_Set*));

    ra->base.hrp[0] = NULL;
    FOR_N(i, 1, ctx->num_classes) {
        ra->base.hrp[i] = tb_arena_alloc(ra->base.arena, ctx->bb_count * sizeof(RAPressure));
        memset(ra->base.hrp[i], 0, ctx->bb_count * sizeof(RAPressure));

        FOR_N(j, 0, ctx->bb_count) {
            ra->base.hrp[i][j].lo2hi = -1;
        }
    }

    log_debug("%s: briggs: allocating IFG skeleton (%zu nodes)", f->super.name, ra->ifg_len);

    ra->ifg_chunks_per_set = (ra->ifg_len + BRIGGS_BITS_PER_CHUNK - 1) / BRIGGS_BITS_PER_CHUNK;
    size_t set_size = sizeof(Briggs_Set) + ra->ifg_chunks_per_set*sizeof(Briggs_Chunk);
    FOR_N(i, 0, ra->ifg_len) {
        ra->ifg[i] = tb_arena_alloc(ra->base.arena, set_size);
        memset(ra->ifg[i], 0, set_size);
    }

    size_t live_count = (f->node_count + 63) / 64;
    SparseSet active  = sparse_set_alloc(ra->base.arena, f->node_count);
    uint64_t* is_vreg = tb_arena_alloc(ra->base.arena, live_count * sizeof(uint64_t));

    aarray_for(i, ctx->vregs) {
        ctx->vregs[i].area = 0;
    }

    FOR_N(i, 0, live_count) {
        uint64_t mask = 0;
        size_t end = i*64 + 64;
        if (end > f->node_count) { end = f->node_count; }

        FOR_N(j, 0, end - i*64) {
            size_t k = i*64 + j;
            if (ctx->vreg_map[k] > 0) {
                mask |= 1ull << j;
            }
        }
        is_vreg[i] = mask;
    }

    FOR_REV_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        uint64_t freq = bb->freq >= 0.1 ? (bb->freq * 10) : 1;
        TB_ASSERT(freq > 0);

        TB_ASSERT(f->node_count <= bb->live_out.capacity);

        // clear live
        sparse_set_clear(&active);

        int last_phi = 1;
        while (last_phi < aarray_length(bb->items) &&
               (bb->items[last_phi]->type == TB_PHI ||
                NODE_ISA(bb->items[last_phi], PROJ))
               ) {
            last_phi++;
        }
        size_t item_count = aarray_length(bb->items);
        uint64_t inst_count = item_count - last_phi;

        // start int
        BITS64_FOR_AND(j, bb->live_out.data, is_vreg, bb->live_out.capacity) {
            int vreg_id = ctx->vreg_map[j];
            TB_ASSERT(vreg_id > 0);

            int def_class = ctx->vregs[vreg_id].mask->class;
            if (def_class != REG_CLASS_STK) {
                p_up(ra, def_class, bb_id, item_count, ctx->num_regs[def_class]);
            }

            VReg* v = &ctx->vregs[vreg_id];
            v->area += inst_count*freq;
            sparse_set_put(&active, j);
        }

        size_t proj_count = 0;
        FOR_REV_N(j, last_phi, item_count) {
            TB_Node* n = bb->items[j];
            TB_ASSERT(n->gvn < f->node_count);

            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                VReg* vreg = &ctx->vregs[vreg_id];
                RegMask* def_mask = vreg->mask;

                if (sparse_set_test(&active, n->gvn)) {
                    uint64_t delta = (proj_count + inst_count)*freq;
                    TB_ASSERT(vreg->area >= delta);
                    vreg->area -= delta;
                }
                sparse_set_remove(&active, n->gvn);
                interfere_live(ctx, ra, &active, vreg_id);

                // 2 address ops will interfere with their own inputs (except for
                // shared dst/src)
                int instant_interferences = 0;
                int shared_edge = ctx->node_2addr(n);
                if (shared_edge >= 0) {
                    TB_ASSERT(shared_edge < n->input_count);
                    FOR_N(k, 1, n->input_count) if (k != shared_edge) {
                        TB_Node* in = n->inputs[k];
                        VReg* in_vreg = node_vreg(ctx, in);
                        if (in_vreg && reg_mask_may_intersect(def_mask, in_vreg->mask)) {
                            int in_vreg_id = in_vreg - ctx->vregs;
                            if (in_vreg_id != vreg_id) {
                                ifg_edge(ra, in_vreg_id, vreg_id);
                                instant_interferences += 1;
                            }
                        }
                    }

                    if (instant_interferences) {
                        RAPressure* p = &ra->base.hrp[def_mask->class][bb_id];
                        int highest = p->curr + instant_interferences;
                        if (highest > p->max) {
                            p->max = highest;
                        }

                        if (p->max >= ctx->num_regs[def_mask->class]) {
                            p->lo2hi = j;
                        }
                    }
                }

                if (def_mask->class != REG_CLASS_STK) {
                    p_down(ra, def_mask->class, bb_id, j, ctx->num_regs[def_mask->class]);
                }
            }

            // don't allow intersecting vregs to allocate from these regs
            int kill_count = clobbers ? ctx->constraint_kill(ctx, n, ctx->ins) : 0;
            if (kill_count > 0) {
                FOR_N(k, 0, kill_count) {
                    RegMask* kill_mask = ctx->ins[k];
                    uint64_t hard_splits = 0;

                    aarray_for(stk_i, active.stack) {
                        int l = active.stack[stk_i];
                        int other_id = ctx->vreg_map[l];
                        RegMask* old_mask = ctx->vregs[other_id].mask;
                        if (other_id == 0 || vreg_id == other_id ||
                            !reg_mask_may_intersect(kill_mask, old_mask) ||
                            old_mask == &TB_REG_EMPTY) {
                            continue;
                        }

                        size_t size = sizeof(RegMask) + old_mask->count*sizeof(uint64_t);
                        RegMask* rm = tb_arena_alloc(&f->arena, size);
                        rm->may_spill = old_mask->may_spill;
                        rm->class = old_mask->class;
                        rm->count = old_mask->count;

                        uint64_t ors = 0;
                        size_t split = TB_MIN(old_mask->count, kill_mask->count);
                        FOR_N(m, 0, split) {
                            rm->mask[m] = old_mask->mask[m] & ~kill_mask->mask[m];
                            ors |= rm->mask[m];
                        }

                        FOR_N(m, split, old_mask->count) {
                            rm->mask[m] = old_mask->mask[m];
                            ors |= rm->mask[m];
                        }

                        // empty? we've got a hard split case
                        if (ors == 0) {
                            // printf("A %%%u %s\n", l, reg_class_name(old_mask->class));

                            if (((hard_splits >> old_mask->class) & 1) == 0) {
                                hard_splits |= 1ull << old_mask->class;

                                RAPressure* p = &ra->base.hrp[old_mask->class][bb_id];
                                p->lo2hi = j;
                                p->max = ctx->num_regs[old_mask->class];
                            }

                            if (!ctx->vregs[other_id].hard_split) {
                                ctx->vregs[other_id].hard_split = true;

                                SplitDecision split = { other_id };
                                dyn_array_put(ra->base.splits, split);

                                // If we don't set this we can hit more "conflicts" more quickly... i think?
                                // ctx->vregs[other_id].mask = &TB_REG_EMPTY;
                            }
                            continue;
                        }

                        RegMask* search = nl_hashset_put2(&ctx->mask_intern, rm, rm_hash, rm_compare);
                        if (search != NULL) {
                            tb_arena_free(&ctx->f->arena, rm, size);
                            ctx->vregs[other_id].mask = search;
                        } else {
                            ctx->vregs[other_id].mask = rm;
                        }
                    }
                }
            }

            // uses are live now
            if (n->type != TB_PHI) {
                FOR_N(k, 0, n->input_count) {
                    TB_Node* in = n->inputs[k];
                    if (in && ctx->vreg_map[in->gvn] > 0) {
                        TB_ASSERT(in->gvn < f->node_count);
                        int in_class = ctx->vregs[ctx->vreg_map[in->gvn]].mask->class;
                        if (!sparse_set_test(&active, in->gvn)) {
                            if (in_class != REG_CLASS_STK) {
                                p_up(ra, in_class, bb_id, j, ctx->num_regs[in_class]);
                            }
                            VReg* v = &ctx->vregs[ctx->vreg_map[in->gvn]];
                            v->area += (proj_count + inst_count)*freq;
                            sparse_set_put(&active, in->gvn);
                        }
                    }
                }
            }

            proj_count += IS_PROJ(n);
            inst_count--;
        }

        FOR_N(j, 1, ctx->num_classes) {
            RAPressure* p = &ra->base.hrp[j][bb_id];
            if (p->curr >= ctx->num_regs[j]) {
                p->lo2hi = 0;
            }
        }
    }
}

// Compact VRegs
static void ifg_compact(Ctx* restrict ctx, Briggs* ra, bool aggro) {
    #if 0
    int* uf = tb_arena_alloc(ra->base.arena, ra->ifg_len * sizeof(int));
    FOR_N(i, 0, ra->ifg_len) {
        uf[i] = i;
    }

    bool changes = false;
    FOR_N(i, 1, ra->ifg_len) {
        if (uf_find(uf, ra->ifg_len, i) != i || ctx->vregs[i].mask == NULL) {
            changes = true;
        }
    }

    if (!changes) {
        return false;
    }

    // compact vregs
    int new_vreg_count = 1;
    FOR_N(i, 1, ra->ifg_len) {
        int p = uf[i];
        if (p == i && ctx->vregs[p].mask != NULL) {
            TB_ASSERT(new_vreg_count <= p);
            if (new_vreg_count != p) {
                // printf("MOVE V%d <- V%d\n", new_vreg_count, p);
                ctx->vregs[new_vreg_count] = ctx->vregs[p];
            }
            uf[i] = new_vreg_count++;
        } else {
            // printf("REF  V%d <- V%d\n", uf[p], p);
            uf[i] = uf[p];
        }
    }

    // UF holds the relocations of the old vreg -> new vreg.
    // time to rewrite the vreg_map
    FOR_N(i, 1, aarray_length(ctx->vreg_map)) {
        // printf("RELOCATE V%d -> V%d\n", ctx->vreg_map[i], uf[ctx->vreg_map[i]]);
        ctx->vreg_map[i] = uf[ctx->vreg_map[i]];
    }

    aarray_set_length(ctx->vregs, new_vreg_count);
    log_debug("%s: briggs: coalescing %zu -> %d", ctx->f->super.name, ra->ifg_len, new_vreg_count);
    return true;
    #endif
}

////////////////////////////////
// Simplify phase
////////////////////////////////
static bool ifg_is_lo_degree(Ctx* ctx, Briggs* ra, int vreg_id) {
    RegMask* mask = ctx->vregs[vreg_id].mask;
    // note the stack has infinite colors so it never spills
    return mask->class == REG_CLASS_STK || ifg_degree(ctx, ra, vreg_id) < popcnt_reg_mask(mask);
}

static void ifg_remove(Briggs* ra, int i, int j) {
    int d = ra->adj[i][0];

    // move item past the inner list
    FOR_N(l, 1, d+1) {
        if (ra->adj[i][l] == j) {
            TB_ASSERT(d < aarray_length(ra->adj[i]));
            // printf("V%d: Remove V%d (%zu %d)\n", i, j, l, d);
            SWAP(uint32_t, ra->adj[i][d], ra->adj[i][l]);
            ra->adj[i][0] = d - 1;
            return;
        }
    }

    // not found
    TB_ASSERT(0);
}

static void ifg_remove_edges2(Ctx* ctx, Briggs* ra, int vreg_id) {
    int d = ra->adj[vreg_id][0];
    FOR_N(j, 1, d+1) {
        int k = ra->adj[vreg_id][j];
        ifg_remove(ra, k, vreg_id);
    }
    // mark degree as 0, the entries are still there tho
    ra->adj[vreg_id][0] = 0;
}

static void ifg_remove_edges(Ctx* ctx, Briggs* ra, int i, IFG_Worklist* lo, IFG_Worklist* hi) {
    int d = ra->adj[i][0];
    FOR_N(j, 1, d+1) {
        int k = ra->adj[i][j];
        ifg_remove(ra, k, i);

        if (bits64_member(hi->visited, k) && ifg_is_lo_degree(ctx, ra, k)) {
            TB_OPTDEBUG(REGALLOC2)(printf("#   V%d used to be uncolorable, we fixed that!\n", k));

            ifg_ws_remove(hi, k);
            ifg_ws_push(lo, k);
        }
    }

    // mark degree as 0, the entries are still there tho
    ra->adj[i][0] = 0;
}

// returns the stack size (0 on failure).
static ArenaArray(SimplifiedElem) ifg_simplify(Ctx* restrict ctx, Briggs* ra) {
    ArenaArray(SimplifiedElem) stk = aarray_create(ra->base.arena, SimplifiedElem, ra->ifg_len);

    TB_ArenaSavepoint sp = tb_arena_save(ra->base.arena);
    IFG_Worklist lo = ifg_ws_alloc(ctx, ra->base.arena, aarray_length(ctx->vregs));
    IFG_Worklist hi = ifg_ws_alloc(ctx, ra->base.arena, aarray_length(ctx->vregs));

    ra->max_spills = 0;

    // bucket all the IFG nodes into low or high degree
    aarray_for(i, ctx->vregs) {
        if (ctx->vregs[i].n == NULL) { continue; }

        // reset assignment
        VReg* vreg = &ctx->vregs[i];
        vreg->class    = 0;
        vreg->assigned = -1;

        if (ifg_is_lo_degree(ctx, ra, i)) {
            ifg_ws_push(&lo, i);
        } else {
            ifg_ws_push(&hi, i);
        }
    }

    #if 0
    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        aarray_for(i, bb->items) {
            int vreg_id = ctx->vreg_map[bb->items[i]->gvn];
            if (vreg_id != 0) {
                TB_ASSERT(ctx->vregs[vreg_id].assigned < 0);
            }
        }
    }
    #endif

    // printf("Hi: %d, Lo: %d\n", hi.count, lo.count);
    for (;;) {
        // find known colorable nodes (degree < k)
        int vreg_id;
        while (vreg_id = ifg_ws_pop(&lo), vreg_id > 0) {
            IF_OPT(REGALLOC) {
                briggs_print_vreg(ctx, ra, &ctx->vregs[vreg_id]);
                printf("#   colorable!\n");
            }

            if (ctx->vregs[vreg_id].mask->class == REG_CLASS_STK) {
                ra->max_spills += ctx->vregs[vreg_id].reg_width;
            }

            int d = ra->adj[vreg_id][0];
            ifg_remove_edges(ctx, ra, vreg_id, &lo, &hi);

            if (ctx->vregs[vreg_id].assigned < 0) {
                SimplifiedElem s = { vreg_id, d };
                aarray_push(stk, s);
            }
        }

        if (hi.count == 0) {
            tb_arena_restore(ra->base.arena, sp);
            return stk;
        }

        // if we run out but there's still high degree nodes it means
        // we're not colorable, speculatively spill voodoo time.
        IF_OPT(REGALLOC) {
            printf("#\n#  SEARCHING FOR SPILL CANDIDATE! (in %d vregs)\n#\n", hi.count);
        }

        // ok we've got too much pressure, let's split a bit
        int best_spill = -1;
        double best_score = INFINITY;

        // pick next best spill
        FOR_N(i, 0, hi.count) {
            VReg* vreg = &ctx->vregs[hi.stack[i]];
            double score = tb__ra_get_spill_cost(&ra->base, vreg); // ifg_degree(ctx, ra, hi.stack[i]);

            IF_OPT(REGALLOC) {
                briggs_print_vreg(ctx, ra, vreg);

                printf("INTR: ");
                int vreg_id = hi.stack[i];
                FOR_N(j, 1, ra->adj[vreg_id][0]+1) {
                    int other = ra->adj[vreg_id][j];
                    printf("V%d ", other);
                }
                printf("\n");
            }

            if (score < best_score) {
                IF_OPT(REGALLOC) {
                    if (best_spill >= 0) {
                        printf("#   V%d is a better spill! V%d (%f is better than %f)\n", hi.stack[i], best_spill, score, best_score);
                    } else {
                        printf("#   V%d is... one of the spills of all time! %f\n", hi.stack[i], score);
                    }
                }
                best_score = score;
                best_spill = hi.stack[i];
            }
        }
        TB_ASSERT(best_spill >= 0);
        TB_OPTDEBUG(REGALLOC)(printf("#\n#  V%d WAS SPECULATIVELY SPILLED!\n#\n", best_spill));

        // speculatively simplify
        int d = ra->adj[best_spill][0];
        ifg_remove_edges(ctx, ra, best_spill, &lo, &hi);
        ifg_ws_remove(&hi, best_spill);

        // how was it spilled and precolored?!
        TB_ASSERT(ctx->vregs[best_spill].assigned < 0);
        SimplifiedElem s = { best_spill, d };
        aarray_push(stk, s);

        // everything it coalesces with should agree on the data type such that
        // this gives us a reliable "spill reg width"
        ra->max_spills += tb__reg_width_from_dt(REG_CLASS_STK, ctx->vregs[best_spill].n->dt);
    }
}
