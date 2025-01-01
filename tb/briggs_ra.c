// Mostly based on reading Briggs' thesis "Register Allocation via Graph Coloring"
#include "codegen.h"
#include <float.h>

enum {
    BRIGGS_WORDS_PER_CHUNK = 128 / sizeof(uint32_t),
    BRIGGS_BITS_PER_CHUNK = BRIGGS_WORDS_PER_CHUNK * 32,
};

// 128B worth of bits
typedef struct Briggs_Chunk Briggs_Chunk;
struct Briggs_Chunk {
    uint32_t bits[BRIGGS_WORDS_PER_CHUNK];
};

typedef struct {
    Briggs_Chunk* chunks[];
} Briggs_Set;

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    // for arena memory usage debooging
    size_t baseline;

    int num_classes;
    int* num_regs;
    int* fixed;

    int max_spills;
    DynArray(int) spills;

    // Bit matrix IFG
    size_t ifg_len;
    size_t ifg_chunks_per_set;
    Briggs_Set** ifg;

    // Adjancency list IFG
    ArenaArray(uint32_t)* adj;
} Briggs;

typedef struct {
    uint32_t vreg_id;
    // degree before removal
    uint32_t degree;
} SimplifiedElem;

typedef struct {
    int count;
    int* stack;
    uint32_t* visited;
} IFG_Worklist;

static void ifg_build(Ctx* restrict ctx, Briggs* ra);
static void ifg_raw_edge(Briggs* ra, int i, int j);
static bool ifg_coalesce(Ctx* restrict ctx, Briggs* ra, bool aggro);
static ArenaArray(SimplifiedElem) ifg_simplify(Ctx* restrict ctx, Briggs* ra);

static bool bits32_test_n_set(uint32_t* arr, size_t x) {
    uint32_t y = arr[x / 32];
    arr[x / 32] = y | (1u << (x % 32));
    return y & (1u << (x % 32));
}

static void bits32_remove(uint32_t* arr, size_t x) {
    arr[x / 32] &= ~(1u << (x % 32));
}

static bool bits32_member(uint32_t* arr, size_t x) {
    return arr[x / 32] & (1u << (x % 32));
}

static int ifg_degree(Briggs* ra, int i) {
    return ra->adj[i][0];
}

static void ifg_print(Ctx* restrict ctx, Briggs* ra, int i) {
    int d = ra->adj[i][0];
    FOR_N(j, 1, d+1) {
        TB_OPTDEBUG(REGALLOC)(printf("V%-4d -- V%-4d\n", i, ra->adj[i][j]));
    }
}

static IFG_Worklist ifg_ws_alloc(Ctx* restrict ctx, Briggs* restrict ra) {
    IFG_Worklist ws = { 0 };
    ws.count = 0;
    ws.visited = tb_arena_alloc(ra->arena, ((ra->ifg_len+31)/32) * sizeof(uint32_t)),
    ws.stack = tb_arena_alloc(ra->arena, ra->ifg_len * sizeof(uint32_t));
    memset(ws.visited, 0, ((ra->ifg_len+31)/32) * sizeof(uint32_t));
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
            bits32_remove(ws->visited, vreg_id);
            ws->stack[i] = ws->stack[--ws->count];
            return;
        }
    }

    TB_ASSERT(0); // not found
}

static bool ifg_ws_push(IFG_Worklist* ws, int vreg_id) {
    if (!bits32_test_n_set(ws->visited, vreg_id)) {
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
    bits32_remove(ws->visited, vreg_id);
    return vreg_id;
}

static bool briggs_is_fixed(Ctx* ctx, Briggs* ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
}

static void briggs_print_vreg(Ctx* restrict ctx, Briggs* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4"PRIdPTR" deg=%d cost=%.2f ", vreg - ctx->vregs, ifg_degree(ra, vreg - ctx->vregs), cost);
    if (vreg->hint_vreg > 0) {
        printf(" hint=V%d", vreg->hint_vreg);
    }
    tb__print_regmask(vreg->mask);
    if (vreg->uses > 1) {
        printf(" (%d USES)", vreg->uses);
    }
    printf("\n");
    if (vreg->n) {
        printf("#   ");
        tb_print_dumb_node(NULL, vreg->n);
        printf("\n");
    }
}

void tb__briggs(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    Briggs ra = { .ctx = ctx, .arena = arena };
    ra.spills = dyn_array_create(int, 32);

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
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
                        .uses = 1,
                    });
            }
            ra.fixed[i] = base;
        }
        ra.num_regs = ctx->num_regs;
        assert(max_regs_in_class <= 64 && "TODO: we assume some 64bit masks in places lol");
    }

    // construct register masks
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        FOR_N(j, 0, aarray_length(bb->items)) {
            TB_Node* n = bb->items[j];

            RegMask** ins = ctx->ins;
            ctx->constraint(ctx, n, ins);

            size_t in_count = n->input_count;
            FOR_N(k, 1, in_count) {
                TB_Node* in = n->inputs[k];
                if (ins[k] != &TB_REG_EMPTY) {
                    VReg* in_vreg = node_vreg(ctx, in);
                    TB_ASSERT(in_vreg);

                    // intersect use masks with the vreg's mask, if it becomes empty we've
                    // got a hard-split (not necessarily spilling to the stack)
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[k]);
                    if (in_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                        TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs));
                        dyn_array_put(ra.spills, in_vreg - ctx->vregs);
                    }
                    in_vreg->mask = new_mask;
                }
            }

            int tmp_count = ctx->tmp_count(ctx, n);
            if (tmp_count > 0) {
                // used for clobbers/scratch but more importantly they're not bound to a node.
                Tmps* tmps  = tb_arena_alloc(arena, sizeof(Tmps) + tmp_count*sizeof(int));
                tmps->count = tmp_count;
                nl_table_put(&ctx->tmps_map, n, tmps);

                FOR_N(k, in_count, in_count + tmp_count) {
                    RegMask* in_mask = ins[k];
                    TB_ASSERT(in_mask != &TB_REG_EMPTY);

                    /* int fixed = fixed_reg_mask(in_mask);
                    if (fixed >= 0) {
                        // insert new range to the existing vreg
                        int vreg_id = tmps->elems[k - in_count] = ra.fixed[in_mask->class] + fixed;
                        ctx->vregs[vreg_id].uses += 1;
                    } else {
                    } */
                    tmps->elems[k - in_count] = aarray_length(ctx->vregs);
                    aarray_push(ctx->vregs, (VReg){ .mask = in_mask, .assigned = -1, .spill_cost = INFINITY, .uses = 1 });
                }
            }
        }
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
    }

    size_t baseline = ra.baseline = tb_arena_current_size(arena);
    log_debug("%s: briggs: arena starting with %.1f KiB", f->super.name, baseline / 1024.0f);

    TB_ArenaSavepoint sp;
    int rounds = 0;
    for (;;) {
        sp = tb_arena_save(arena);
        cuikperf_region_start("round", NULL);

        // that's a lot of rounds... you sure we're making forward progress?
        rounds += 1;
        TB_ASSERT(rounds < 10);

        // build IFG
        CUIK_TIMED_BLOCK("IFG build") {
            log_debug("%s: briggs: building IFG", f->super.name);
            ifg_build(ctx, &ra);
        }

        CUIK_TIMED_BLOCK("IFG square") {
            FOR_N(i, 0, ra.ifg_len) {
                FOR_N(j, 0, ra.ifg_chunks_per_set) {
                    Briggs_Chunk* chk = ra.ifg[i]->chunks[j];
                    if (chk == NULL) { continue; }

                    FOR_N(k, 0, BRIGGS_WORDS_PER_CHUNK) {
                        uint32_t bits = chk->bits[k];
                        while (bits) {
                            int l = tb_ffs(bits) - 1;
                            int m = j*BRIGGS_BITS_PER_CHUNK + k*32 + l;
                            if (i < m) {
                                ifg_raw_edge(&ra, m, i);
                            }
                            bits &= ~(1u << l);
                        }
                    }
                }
            }
        }

        if (ifg_coalesce(ctx, &ra, rounds == 1)) {
            cuikperf_region_end();
            tb_arena_restore(arena, sp);
            continue;
        }

        CUIK_TIMED_BLOCK("Convert to adjacency") {
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
                        uint32_t bits = chk->bits[k];
                        while (bits) {
                            int l = tb_ffs(bits) - 1;
                            int m = j*BRIGGS_BITS_PER_CHUNK + k*32 + l;

                            /* if (i < m) {
                                printf("V%zu -- V%d\n", i, m);
                            } */

                            // insert into adjancency list
                            aarray_push(ra.adj[i], m);
                            ra.adj[i][0] += 1;

                            bits &= ~(1u << l);
                        }
                    }
                }
            }

            ra.ifg = NULL;
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("# simplify phase              #\n");
        printf("###############################\n");
        #endif

        // simplify (find potential spills)
        cuikperf_region_start("simplify", NULL);
        ArenaArray(SimplifiedElem) stk = ifg_simplify(ctx, &ra);
        cuikperf_region_end();

        // select (can fail to color due)
        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("# select phase                #\n");
        printf("###############################\n");
        #endif

        int num_spills = ctx->num_spills;
        Set live_stack = set_create_in_arena(arena, ra.max_spills);

        cuikperf_region_start("select", NULL);
        dyn_array_clear(ra.spills);
        FOR_REV_N(i, 0, aarray_length(stk)) {
            SimplifiedElem s = stk[i];
            VReg* vreg = &ctx->vregs[s.vreg_id];
            // un-yank, this will let us know how far we
            // need to undo the removals on a node
            int degree = ra.adj[s.vreg_id][0] = s.degree;

            // precolored? skip it
            if (vreg->assigned >= 0) {
                continue;
            }

            #if TB_OPTDEBUG_REGALLOC
            briggs_print_vreg(ctx, &ra, vreg);
            #endif

            // coloring stack slots
            if (reg_mask_is_spill(vreg->mask)) {
                int empty_slot = num_spills++;

                TB_OPTDEBUG(REGALLOC)(printf("#   assigned SPILL%u\n", empty_slot));
                vreg->class    = REG_CLASS_STK;
                vreg->assigned = STACK_BASE_REG_NAMES + empty_slot + 1;
                continue;
            }

            uint64_t mask = vreg->mask->mask[0];
            int def_class = vreg->mask->class;
            FOR_N(j, 1, degree + 1) {
                uint32_t other_id = ra.adj[s.vreg_id][j];

                VReg* other = &ctx->vregs[other_id];
                if (other->mask->class == def_class && other->assigned >= 0) {
                    mask &= ~(1ull << other->assigned);
                    // TB_OPTDEBUG(REGALLOC)(printf("  => %#04"PRIx64" (we can't be R%d because V%d)\n", mask, other->assigned, other_id));
                    if (mask == 0) { goto leave; }
                }
            }

            leave:
            if (mask == 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#   assigned UNCOLORED\n"));

                // failed to color.. sadge
                vreg->class    = 0;
                vreg->assigned = -1;
                dyn_array_put(ra.spills, s.vreg_id);
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
        cuikperf_region_end();

        // if we found nothing to spill, we've successfully colored vregs
        if (dyn_array_length(ra.spills) == 0) {
            log_debug("%s: briggs: good coloring (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
            dyn_array_destroy(ra.spills);
            tb_arena_restore(arena, sp);

            ctx->num_spills = num_spills;
            cuikperf_region_end();
            break;
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("###############################\n");
        printf("# spill phase                 #\n");
        printf("###############################\n");
        #endif

        // if anything in the spill candidates failed to color, we need to spill. note that
        // it wouldn't have been able to fail coloring without making it to this list.
        cuikperf_region_start("spill", NULL);
        FOR_N(i, 0, dyn_array_length(ra.spills)) {
            int vreg_id = ra.spills[i];
            TB_Node* n  = ctx->vregs[vreg_id].n;
            RegMask* vreg_mask = ctx->vregs[vreg_id].mask;

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# V%d: spill (%%%u)\x1b[0m\n", vreg_id, n->gvn));

            // rematerialization candidates will delete the original def and for now, they'll
            // reload per use site (although we might wanna coalesce some later on).
            if (can_remat(ctx, n)) {
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
        cuikperf_region_end();

        // undo assignments
        FOR_N(i, 1, aarray_length(ctx->vregs)) {
            if (!briggs_is_fixed(ctx, &ra, i) && ctx->vregs[i].mask->class != REG_CLASS_STK) {
                ctx->vregs[i].class    = 0;
                ctx->vregs[i].assigned = -1;
                ctx->vregs[i].marked_spilled = false;
            }
        }
        log_debug("%s: briggs: failed to color (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
        tb_arena_restore(arena, sp);

        // recompute liveness
        redo_dataflow(ctx, arena);

        // time to retry
        dyn_array_clear(ra.spills);
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
        chk = tb_arena_alloc(ra->arena, sizeof(Briggs_Chunk));
        memset(chk, 0, sizeof(Briggs_Chunk));

        ra->ifg[i]->chunks[j / BRIGGS_BITS_PER_CHUNK] = chk;
    }

    uint32_t k = j % BRIGGS_BITS_PER_CHUNK;
    chk->bits[k / 32] |= 1u << (k % 32);
}

static void ifg_union(Briggs* ra, int dst, int src) {
    FOR_N(j, 0, ra->ifg_chunks_per_set) {
        Briggs_Chunk* chk = ra->ifg[dst]->chunks[j];
        if (ra->ifg[src]->chunks[j] == NULL) {
            continue;
        }

        uint32_t* src_bits = ra->ifg[src]->chunks[j]->bits;
        if (chk == NULL) {
            // first time we're writing to this chunk
            // TODO(NeGate): put these into a free list, we wanna recycle chunks as much as possible.
            chk = tb_arena_alloc(ra->arena, sizeof(Briggs_Chunk));
            memset(chk, 0, sizeof(Briggs_Chunk));

            ra->ifg[dst]->chunks[j] = chk;
        }

        // newly set
        FOR_N(k, 0, BRIGGS_WORDS_PER_CHUNK) {
            uint32_t bits = ~chk->bits[k] & src_bits[k];
            chk->bits[k] |= src_bits[k];

            while (bits) {
                int l = tb_ffs(bits) - 1;
                int m = j*BRIGGS_BITS_PER_CHUNK + k*32 + l;
                if (dst < m) {
                    ifg_raw_edge(ra, m, dst);
                }
                bits &= ~(1u << l);
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

    uint32_t k = j % BRIGGS_BITS_PER_CHUNK;
    return chk->bits[k / 32] & (1u << (k % 32));
}

static void interfere_live(Ctx* restrict ctx, Briggs* ra, uint32_t* live, int vreg_id) {
    RegMask* vreg_mask = ctx->vregs[vreg_id].mask;
    FOR_N(k, 1, ctx->f->node_count) {
        int other_id = ctx->vreg_map[k];
        if (other_id > 0 && vreg_id != other_id && bits32_member(live, k) && reg_mask_may_intersect(vreg_mask, ctx->vregs[other_id].mask)) {
            // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%td\n", vreg_id, k));
            ifg_edge(ra, vreg_id, other_id);
        }
    }
}

static void interfere_mask(Ctx* restrict ctx, Briggs* ra, int vreg_id, RegMask* def_mask) {
    // definition should interfere with each physical reg's node it doesn't intersect with
    // such that it can't be seen alive in those registers.
    if (def_mask->class > 0 && reg_mask_is_not_empty(def_mask)) {
        size_t reg_count    = ctx->num_regs[def_mask->class];
        uint64_t word_count = (reg_count + 63) / 64;
        FOR_N(i, 0, word_count) {
            uint64_t bits = ~def_mask->mask[i];
            while (bits) {
                int j = tb_ffs64(bits) - 1;
                if (i*64 + j >= reg_count) {
                    return;
                }

                int in_vreg_id = ra->fixed[def_mask->class] + i*64 + j;

                TB_ASSERT(in_vreg_id < aarray_length(ctx->vregs));
                ifg_edge(ra, in_vreg_id, vreg_id);
                bits &= ~(1ull << j);
            }
        }
    }
}

static void ifg_build(Ctx* restrict ctx, Briggs* ra) {
    TB_Function* f = ctx->f;

    ra->ifg_len = aarray_length(ctx->vregs);
    ra->ifg = tb_arena_alloc(ra->arena, ra->ifg_len * sizeof(Briggs_Set*));

    log_debug("%s: briggs: allocating IFG skeleton (%zu nodes)", f->super.name, ra->ifg_len);

    ra->ifg_chunks_per_set = (ra->ifg_len + BRIGGS_BITS_PER_CHUNK - 1) / BRIGGS_BITS_PER_CHUNK;
    size_t set_size = sizeof(Briggs_Set) + ra->ifg_chunks_per_set*sizeof(Briggs_Chunk);
    FOR_N(i, 0, ra->ifg_len) {
        ra->ifg[i] = tb_arena_alloc(ra->arena, set_size);
        memset(ra->ifg[i], 0, set_size);
    }

    // fixed vregs interfere with their fellow fixed vregs
    FOR_N(i, 0, ctx->num_classes) {
        int base = ra->fixed[i];
        FOR_N(j, 0, ctx->num_regs[i]) FOR_N(k, j + 1, ctx->num_regs[i]) {
            ifg_edge(ra, base + k, base + j);
        }
    }

    size_t live_count = ((f->node_count + 63) / 32);
    uint32_t* live = tb_arena_alloc(ra->arena, live_count * sizeof(uint32_t));

    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        FOR_N(j, 0, (live_count + 1) / 2) {
            live[j*2 + 0] = bb->live_out.data[j] & 0xFFFFFFFF;
            live[j*2 + 1] = bb->live_out.data[j] >> 32ull;
        }

        size_t item_count = aarray_length(bb->items);
        FOR_REV_N(j, 0, item_count) {
            TB_Node* n = bb->items[j];
            int vreg_id = ctx->vreg_map[n->gvn];

            if (n->type == TB_MACH_MOVE) {
                TB_ASSERT(n->user_count == 1);
                TB_Node* phi = USERN(n->users);

                // write to phi, "logically" defined here
                bits32_remove(live, phi->gvn);
            }

            if (vreg_id > 0) {
                VReg* vreg = &ctx->vregs[vreg_id];
                RegMask* def_mask = vreg->mask;

                TB_ASSERT(n->gvn < f->node_count);
                bits32_remove(live, n->gvn);

                interfere_live(ctx, ra, live, vreg_id);
                interfere_mask(ctx, ra, vreg_id, vreg->mask);

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
                            if (in_vreg_id != vreg_id) {
                                // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%d\n", vreg_id, in_vreg_id));
                                ifg_edge(ra, in_vreg_id, vreg_id);
                            }
                        }
                    }
                }
            }

            int tmp_count = ctx->tmp_count(ctx, n);
            if (tmp_count > 0) {
                Tmps* tmps = nl_table_get(&ctx->tmps_map, n);
                TB_ASSERT(tmps && tmps->count == tmp_count);

                // temporaries basically just live across this instruction alone
                FOR_N(k, 0, tmps->count) {
                    RegMask* tmp_mask = ctx->vregs[tmps->elems[k]].mask;
                    interfere_live(ctx, ra, live, tmps->elems[k]);
                    interfere_mask(ctx, ra, tmps->elems[k], tmp_mask);
                }
            }

            // uses are live now
            if (n->type != TB_PHI) {
                FOR_N(j, 0, n->input_count) {
                    TB_Node* in = n->inputs[j];
                    if (in) {
                        TB_ASSERT(in->gvn < f->node_count);
                        bits32_test_n_set(live, in->gvn);
                    }
                }
            }
        }
    }
}

////////////////////////////////
// Coalescing
////////////////////////////////
static int uf_find(int* uf, int a) {
    // leader
    int l = a;
    while (uf[l] != l) {
        l = uf[l];
    }

    // path compaction
    while (uf[a] != a) {
        int p = uf[a];
        uf[a] = l, a = p;
    }

    return l;
}

static void uf_union(int* uf, int x, int y) {
    x = uf_find(uf, x);
    y = uf_find(uf, y);

    // parent should be the smaller number
    if (x > y) {
        SWAP(int, x, y);
    }

    if (x != y) {
        uf[y] = x;
    }
}

static bool ifg_coalesce(Ctx* restrict ctx, Briggs* ra, bool aggro) {
    // TODO(NeGate): implement conservative coalescing
    if (!aggro) {
        return false;
    }

    int* uf = tb_arena_alloc(ra->arena, ra->ifg_len * sizeof(int));
    FOR_N(i, 0, ra->ifg_len) {
        uf[i] = i;
    }

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        size_t item_count = aarray_length(bb->items);

        size_t j = 0;
        while (j < item_count) {
            TB_Node* n = bb->items[j++];
            int dst_id = ctx->vreg_map[n->gvn];
            if (dst_id < 0) { continue; }

            int shared_edge = ctx->node_2addr(n);
            if (shared_edge <= 0) { continue; }

            TB_ASSERT(shared_edge < n->input_count);
            int src_id = ctx->vreg_map[n->inputs[shared_edge]->gvn];

            int x = uf_find(uf, dst_id);
            int y = uf_find(uf, src_id);
            if (!ifg_member(ra, x, y)) {
                // parent should be the smaller number
                if (x > y) {
                    SWAP(int, x, y);
                }
                // union if they're not already in the same
                // set... and they can intersect
                if (x != y && ctx->vregs[x].assigned < 0 && ctx->vregs[y].assigned < 0) {
                    RegMask* meet = tb__reg_mask_meet(ctx, ctx->vregs[x].mask, ctx->vregs[y].mask);
                    if (meet != &TB_REG_EMPTY) {
                        uf[y] = x;

                        #if 0
                        printf("COALESCED %%%u and %%%u\n  V%d ", n->gvn, n->inputs[shared_edge]->gvn, x);
                        tb__print_regmask(ctx->vregs[x].mask);
                        printf(" /\\ V%d ", y);
                        tb__print_regmask(ctx->vregs[y].mask);
                        printf("\n");
                        #endif

                        // edit the IFG so that the rest of the intersection
                        // queries can see "x" with all of y's edges.
                        ifg_union(ra, x, y);

                        ctx->vregs[x].uses += ctx->vregs[y].uses;
                        ctx->vregs[x].mask = meet;

                        // if it's a machine copy and it's coalesced, just get rid of it
                        if (n->type == TB_MACH_COPY) {
                            ctx->vregs[x].uses -= 1;
                            ctx->vreg_map[n->gvn] = 0;

                            tb__remove_node(ctx, ctx->f, n);
                            subsume_node(ctx->f, n, n->inputs[1]);

                            item_count = aarray_length(bb->items);
                            j -= 1;
                        }
                        continue;
                    }
                }
            }

            #if 0
            printf("FAILED TO COALESCED %%%u and %%%u\n  V%d ", n->gvn, n->inputs[shared_edge]->gvn, x);
            tb__print_regmask(ctx->vregs[x].mask);
            printf(" /\\ V%d ", y);
            tb__print_regmask(ctx->vregs[y].mask);
            printf("\n");
            #endif
        }
    }

    // compact path
    bool changes = false;
    FOR_N(i, 1, ra->ifg_len) {
        if (uf_find(uf, i) != i) {
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
        if (p == i) {
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

    // relocate the fixed regs, they couldn't be compacted so they should
    // still be next to each other.
    FOR_N(i, 0, ctx->num_classes) {
        int base = ra->fixed[i];
        ra->fixed[i] = uf[base];

        #if TB_OPTDEBUG_REGALLOC
        size_t count = ctx->num_regs[i];
        FOR_N(j, 0, count) {
            TB_ASSERT(uf[base] + j == uf[base + j]);
        }
        #endif
    }

    // relocate temporaries
    nl_table_for(it, &ctx->tmps_map) {
        Tmps* tmps = it->v;
        FOR_N(i, 0, tmps->count) {
            tmps->elems[i] = uf[tmps->elems[i]];
        }
    }

    aarray_set_length(ctx->vregs, new_vreg_count);
    log_debug("%s: briggs: aggro coalescing %zu -> %d", ctx->f->super.name, ra->ifg_len, new_vreg_count);

    return true;
}

////////////////////////////////
// Simplify phase
////////////////////////////////
static bool ifg_is_lo_degree(Ctx* ctx, Briggs* ra, int vreg_id) {
    RegMask* mask = ctx->vregs[vreg_id].mask;
    // note the stack has infinite colors so it never spills
    return reg_mask_is_stack(mask) || ifg_degree(ra, vreg_id) < ctx->num_regs[mask->class];
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

static void ifg_remove_edges(Ctx* ctx, Briggs* ra, int i, IFG_Worklist* lo, IFG_Worklist* hi) {
    int d = ra->adj[i][0];
    FOR_N(j, 1, d+1) {
        int k = ra->adj[i][j];
        ifg_remove(ra, k, i);

        if (bits32_member(hi->visited, k) && ifg_is_lo_degree(ctx, ra, k)) {
            TB_OPTDEBUG(REGALLOC)(printf("#   V%d used to be uncolorable, we fixed that!\n", k));

            ifg_ws_remove(hi, k);
            ifg_ws_push(lo, k);
        }
    }

    // mark degree as 0, the entries are still there tho
    ra->adj[i][0] = 0;
}

// returns the stack size (0 on failure).
static ArenaArray(SimplifiedElem) ifg_simplify(Ctx* restrict ctx, Briggs* ra) {
    ArenaArray(SimplifiedElem) stk = aarray_create(ra->arena, SimplifiedElem, ra->ifg_len);

    TB_ArenaSavepoint sp = tb_arena_save(ra->arena);
    IFG_Worklist lo = ifg_ws_alloc(ctx, ra);
    IFG_Worklist hi = ifg_ws_alloc(ctx, ra);

    ra->max_spills = 0;

    // bucket all the IFG nodes into low or high degree
    FOR_N(i, 1, ra->ifg_len) {
        if (ctx->vregs[i].uses == 0) {
            continue;
        }

        if (ifg_is_lo_degree(ctx, ra, i)) {
            ifg_ws_push(&lo, i);
        } else {
            ifg_ws_push(&hi, i);
        }
    }

    // printf("Hi: %d, Lo: %d\n", hi.count, lo.count);
    for (;;) {
        // find known colorable nodes (degree < k)
        int vreg_id;
        while (vreg_id = ifg_ws_pop(&lo), vreg_id > 0) {
            #if TB_OPTDEBUG_REGALLOC
            briggs_print_vreg(ctx, ra, &ctx->vregs[vreg_id]);
            printf("#   colorable!\n");
            #endif

            if (reg_mask_is_spill(ctx->vregs[vreg_id].mask)) {
                ra->max_spills += 1;
            }

            int d = ra->adj[vreg_id][0];
            ifg_remove_edges(ctx, ra, vreg_id, &lo, &hi);

            SimplifiedElem s = { vreg_id, d };
            aarray_push(stk, s);
        }

        if (hi.count == 0) {
            tb_arena_restore(ra->arena, sp);
            return stk;
        }

        // if we run out but there's still high degree nodes it means
        // we're not colorable, speculatively spill voodoo time.
        TB_OPTDEBUG(REGALLOC)(printf("#\n#  SEARCHING FOR SPILL CANDIDATE! (in %d vregs)\n#\n", hi.count));

        // ok we've got too much pressure, let's split a bit
        int best_spill = -1;
        float best_score = INFINITY;

        // pick next best spill
        FOR_N(i, 0, hi.count) {
            VReg* vreg = &ctx->vregs[hi.stack[i]];
            float score = get_spill_cost(ctx, vreg) / ifg_degree(ra, hi.stack[i]);

            #if TB_OPTDEBUG_REGALLOC
            briggs_print_vreg(ctx, ra, vreg);
            #endif

            if (score < best_score) {
                if (best_spill >= 0) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   V%d is a better spill! V%d (%f is better than %f)\n", hi.stack[i], best_spill, score, best_score));
                } else {
                    TB_OPTDEBUG(REGALLOC)(printf("#   V%d is... one of the spills of all time! %f\n", hi.stack[i], score));
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

        SimplifiedElem s = { best_spill, d };
        aarray_push(stk, s);

        // TODO(NeGate): assuming 1 stack slot per vreg (will become incorrect later on)
        ra->max_spills += 1;
    }
}
