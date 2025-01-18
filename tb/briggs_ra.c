// Mostly implementing Briggs' thesis "Register Allocation via Graph Coloring" +
//
// Also:
//   https://cr.openjdk.org/~thartmann/offsite_2018/register_allocator.pdf
//
// Oh and special thanks to Cliff for advice
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
    Briggs_Chunk* chunks[0];
} Briggs_Set;

typedef struct {
    int curr, max;
    // index in the block where the pressure went from lo->hi
    int lo2hi;
} Briggs_Pressure;

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    // for arena memory usage debooging
    size_t baseline;

    int num_classes;
    int* num_regs;

    int max_spills;
    DynArray(int) spills;

    // Block pressures per reg class, we don't track stack
    // pressure because... we uhh don't care? this is for
    // splitting heuristics.
    Briggs_Pressure* block_pressures[MAX_REG_CLASSES];

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

static int bits32_next(uint32_t* arr, size_t cnt, int x) {
    // unpack coords
    size_t i = x / 32, j = x % 32;

    uint32_t word;
    for (;;) {
        // we're done
        if (i >= cnt) { return -1; }
        // if there's no more bits in the word, we move along
        word = arr[i] >> (j + 1);
        if (word != 0) {
            j += tb_ffs(word);
            return i*32 + j;
        }
        i += 1, j = 0;
    }
}

static int bits32_first(uint32_t* arr, size_t cnt) {
    TB_ASSERT(cnt > 0);
    return arr[0] & 1 ? 0 : bits32_next(arr, cnt, 0);
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

static void briggs_dump_sched(Ctx* restrict ctx, Briggs* restrict ra) {
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf("BB %zu (freq=%f, pressures: {%d, %d}):\n", i, bb->freq, ra->block_pressures[1][i].max, ra->block_pressures[2][i].max);
        aarray_for(i, bb->items) {
            printf("  ");
            tb_print_dumb_node(NULL, bb->items[i]);
            printf("\n");
        }
    }
}

// inserts spills and reloads that happen across blocks
static TB_Node* spill_fancy_xbb(Ctx* ctx, Briggs* ra, TB_DataType dt, int i, TB_Node* phi, TB_Node** pred_defs, bool* is_spilled, int def_class) {
    TB_Function* f = ctx->f;
    RegMask* spill_rm  = intern_regmask(ctx, 0, true, 0);
    RegMask* reload_rm = ctx->normie_mask[def_class];

    TB_Node* bb_node = ctx->cfg.blocks[i].start;
    int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;

    bool same = true;
    TB_Node* leader = NULL;
    FOR_N(j, 0, pred_count) {
        TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
        TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

        TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
        int pred_id = pred_bb - ctx->cfg.blocks;

        TB_Node* pred_def = pred_defs[j];
        if (pred_def == NULL) {
            continue;
        }

        // RELOAD FROM P TO B IF:
        //   it's in a register in B's entry but not in P's exit
        if (!is_spilled[i] && is_spilled[pred_id]) {
            #if TB_OPTDEBUG_REGALLOC
            printf("  reloaded in BB%d!\n", pred_id);
            #endif

            TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, v, pred_defs[j], 1);
            TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = reload_rm, .use = spill_rm);

            tb__insert_before(ctx, f, v, pred_bb->end);
            pred_defs[j] = v;
        }
        // SPILL FROM P TO B IF:
        //   it's spilled in B's entry but not in P's exit and is in a register during
        //   the end of P.
        else if (is_spilled[i] && !is_spilled[pred_id]) {
            #if TB_OPTDEBUG_REGALLOC
            printf("  spilled in BB%d!\n", pred_id);
            #endif

            TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, v, pred_defs[j], 1);
            TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = spill_rm, .use = reload_rm);

            tb__insert_before(ctx, f, v, pred_bb->end);
            pred_defs[j] = v;
        }

        printf("  DEF[%zu] %%%u\n", j, pred_def ? pred_def->gvn : 0);

        // are they all the same edge?
        if (leader != phi) {
            if (leader == NULL) { leader = pred_defs[j]; }
            else if (leader != pred_defs[j]) { same = false; }
        }
    }

    if (same) {
        return leader ? leader : phi;
    }

    // construct phi
    if (phi == NULL) {
        phi = tb_alloc_node(f, TB_PHI, dt, 1 + pred_count, 0);
        set_input(f, phi, bb_node, 0);
        tb__insert_after(ctx, f, phi, bb_node);
    }

    FOR_N(j, 0, pred_count) {
        set_input(f, phi, pred_defs[j], j+1);
    }

    return phi;
}

// Matthias Braun and Sebastian Hack "Register Spilling and Live-Range Splitting for SSA-Form Programs" (2009)
//   https://pp.ipd.kit.edu/uploads/publikationen/braun09cc.pdf
static void spill_fancy(Ctx* ctx, Briggs* ra, int vreg_id) {
    VReg* to_spill = &ctx->vregs[vreg_id];
    TB_ASSERT(to_spill->uses == 1);

    TB_Function* f = ctx->f;
    TB_Node* spilled_n = to_spill->n;
    int def_class = to_spill->mask->class;

    to_spill->mask = ctx->constraint(ctx, spilled_n, NULL);
    to_spill->spill_cost = NAN;

    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: spill  (%%%u)\x1b[0m\n", to_spill - ctx->vregs, spilled_n->gvn));

    briggs_dump_sched(ctx, ra);
    __debugbreak();

    ArenaArray(int)* df = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(ArenaArray(int)));
    {
        FOR_N(i, 0, ctx->bb_count) { df[i] = NULL; }
        // compute dominance frontiers
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];

            TB_Node* bb_node = bb->start;
            int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
            if (pred_count < 2) {
                continue;
            }

            FOR_N(j, 0, pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                TB_BasicBlock* runner = f->scheduled[pred->gvn];
                while (runner != bb->dom) {
                    int id = runner - ctx->cfg.blocks;

                    // add to frontier
                    if (df[id] == NULL) {
                        df[id] = aarray_create(ra->arena, int, 4);
                    }
                    aarray_push(df[id], i);
                    runner = runner->dom;
                }
            }
        }
        printf("DF:\n");
        FOR_N(i, 0, ctx->bb_count) {
            printf("  [%4zu]", i);
            if (df[i]) aarray_for(j, df[i]) {
                printf(" %-3d", df[i][j]);
            }
            printf("\n");
        }
    }

    RegMask* spill_rm  = intern_regmask(ctx, 0, true, 0);
    RegMask* reload_rm = ctx->normie_mask[def_class];

    bool* is_spilled = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(bool));
    TB_Node** defs = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(TB_Node*));
    TB_Node** phis = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(TB_Node*));

    // everything starts as dead and unspilled
    FOR_N(i, 0, ctx->bb_count) {
        defs[i] = NULL;
        phis[i] = NULL;
        is_spilled[i] = false;
    }

    // loops will have incomplete phis we'll wanna return to once the loop is
    // done, to do that we just push the loop tail onto this stack and pop it
    // later.
    ArenaArray(int) loop_stack = aarray_create(ra->arena, int, ctx->bb_count);

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        if (phis[i] != NULL) {
            TB_Node* k = identity_phi(f, phis[i]);
            if (k != phis[i]) { subsume_node(f, phis[i], k); }
        }
    }

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        #if 0
        aarray_for(i, bb->items) {
            printf("  ");
            tb_print_dumb_node(NULL, bb->items[i]);
            printf("\n");
        }
        #endif

        // how many preds consider the value to be registers
        int freq = 0;
        int loop_tail = -1;

        TB_Node* bb_node = bb->start;
        int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
        TB_Node** pred_defs = tb_arena_alloc(ra->arena, pred_count * sizeof(TB_Node*));

        bool is_hrp = ra->block_pressures[def_class][i].max >= ctx->num_regs[def_class];
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;

            pred_defs[j] = defs[pred_id];
            if (pred_id >= i && loop_tail < pred_id) {
                loop_tail = pred_id;
                continue;
            }
            freq += !is_spilled[pred_id];
        }

        const char* rg = ra->block_pressures[1][i].max >= 16 ? "HRP" : "LRP";
        printf("BB %zu (freq=%f, preds=%d, %s): ", i, bb->freq, pred_count, rg);
        FOR_SUCC(it, bb->end) {
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, it.succ);
            printf("  BB%-3zu", succ_bb - ctx->cfg.blocks);
        }
        printf("\n");

        TB_Node* phi = NULL;

        // if we're a loop we wanna consider ourselves an HRP if *ANY* blocks are HRP
        // within the body.
        if (loop_tail >= 0) {
            FOR_N(j, i+1, loop_tail) {
                is_hrp |= ra->block_pressures[def_class][j].max >= ctx->num_regs[def_class];
            }

            if (is_hrp) {
                TB_OPTDEBUG(REGALLOC2)(printf("  we crossed an HRP loop!\n"));
                is_spilled[i] = true;
            }

            // all loops need phis because we don't know what we'll do in the body yet
            phis[i] = tb_alloc_node(f, TB_PHI, spilled_n->dt, 1 + pred_count, 0);
            set_input(f, phis[i], bb_node, 0);
            tb__insert_after(ctx, f, phis[i], bb_node);

            // push both head & tail, we'll fill the phis later
            aarray_push(loop_stack, i);
            aarray_push(loop_stack, loop_tail);
        } else if (pred_count > 0) {
            // if none of our predecessors want it in a reg, we don't either... yet?
            if (freq == 0) {
                TB_OPTDEBUG(REGALLOC2)(printf("  our preds didn't put it in regs!\n"));
                is_spilled[i] = true;
            }
            // we can't fit the value in regs right now (i mean unless we already have :p)
            else if (is_hrp && freq != pred_count) {
                TB_OPTDEBUG(REGALLOC2)(printf("  we crossed the HRP on entry!\n"));
                is_spilled[i] = true;
            } else {
                is_spilled[i] = false;
            }
        }

        // we can resolve the phi now
        if (loop_tail < 0) {
            defs[i] = spill_fancy_xbb(ctx, ra, spilled_n->dt, i, phis[i], pred_defs, is_spilled, def_class);
        } else {
            defs[i] = phis[i];
        }
        tb_arena_free(ra->arena, pred_defs, pred_count * sizeof(TB_Node*));

        int loop_stack_len = aarray_length(loop_stack);
        if (loop_stack_len && loop_stack[loop_stack_len - 1] == i) {
            int head = loop_stack[loop_stack_len - 2];
            aarray_set_length(loop_stack, loop_stack_len - 2);

            printf("  COMPLETED LOOP BB%d!\n", head);

            TB_Node* loop_node = ctx->cfg.blocks[head].start;
            TB_ASSERT(cfg_is_region(loop_node));

            int loop_pred_count = loop_node->input_count;
            TB_Node** loop_pred_defs = tb_arena_alloc(ra->arena, loop_pred_count * sizeof(TB_Node*));
            FOR_N(j, 0, loop_pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, loop_node, j);
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                loop_pred_defs[j] = defs[pred_bb - ctx->cfg.blocks];
            }

            // insert fully resolved phis now
            TB_Node* p = spill_fancy_xbb(ctx, ra, spilled_n->dt, head, phis[head], loop_pred_defs, is_spilled, def_class);
            if (p != phis[head]) {
                // tb__remove_node(ctx, ctx->f, phis[head]);
                // subsume_node(f, phis[head], p);
            }
            tb_arena_free(ra->arena, loop_pred_defs, loop_pred_count * sizeof(TB_Node*));
        }
        printf("  \x1b[32mdef=%%%u, is_spilled=%d, freq=%d\x1b[0m\n", defs[i] ? defs[i]->gvn : 0, is_spilled[i], freq);

        // if we cross LRP->HRP then we spill, use we're needed then we reload
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            // uses of the value when spilled cause a reload
            if (is_spilled[i]) {
                FOR_N(k, 1, n->input_count) {
                    if (n->inputs[k] == spilled_n) {
                        TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, spilled_n->dt, 2, sizeof(TB_NodeMachCopy));
                        set_input(f, v, defs[i], 1);
                        TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = reload_rm, .use = spill_rm);

                        tb__insert_before(ctx, f, v, n);
                        defs[i] = v;

                        is_spilled[i] = false;
                        TB_OPTDEBUG(REGALLOC2)(printf("  we need to reload now! %%%u\n", v->gvn));
                        break;
                    }
                }
            }
            // "limit" can only spill spilled_n
            if (!is_spilled[i]) {
                // if we hit an HRP, we must spill before that point if we haven't already.
                if (is_hrp || ra->block_pressures[def_class][i].lo2hi == j) {
                    TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, spilled_n->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, v, defs[i], 1);
                    TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = spill_rm, .use = reload_rm);

                    tb__insert_before(ctx, f, v, n);
                    defs[i] = v;

                    TB_OPTDEBUG(REGALLOC2)(printf("  onto stack now! (we've crossed an HRP region)\n"));
                    is_spilled[i] = true;
                }
            }
            if (n == spilled_n) {
                TB_OPTDEBUG(REGALLOC2)(printf("  in register! (just defined)\n"));

                defs[i] = n;
                is_spilled[i] = false;
            }
        }
    }

    briggs_dump_sched(ctx, ra);
    __debugbreak();

    #if 0
    tb__insert_after(ctx, f, spill_n, n);
    VReg* spill_vreg = tb__set_node_vreg(ctx, spill_n);
    spill_vreg->spill_cost = INFINITY;
    spill_entire_lifetime(ctx, spill_vreg, spill_rm, false);
    #endif
}

void tb__briggs(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    Briggs ra = { .ctx = ctx, .arena = arena };
    ra.spills = dyn_array_create(int, 32);

    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        FOR_N(i, 0, ctx->num_classes) {
            size_t count = ctx->num_regs[i];
            if (max_regs_in_class < count) {
                max_regs_in_class = count;
            }
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

                TB_OPTDEBUG(REGALLOC)(printf("TMPS %%%u:\n", n->gvn));
                FOR_N(k, in_count, in_count + tmp_count) {
                    RegMask* in_mask = ins[k];
                    TB_ASSERT(in_mask != &TB_REG_EMPTY);

                    #if TB_OPTDEBUG_REGALLOC
                    printf("  V%u: ", aarray_length(ctx->vregs));
                    tb__print_regmask(in_mask);
                    printf("\n");
                    #endif

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
        TB_ASSERT(rounds < 5);

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

        int stack_cap = ctx->num_regs[REG_CLASS_STK] + ra.max_spills;
        int mask_cap = stack_cap < max_regs_in_class ? max_regs_in_class : stack_cap;
        uint64_t* mask = tb_arena_alloc(arena, ((mask_cap+63)/64) * sizeof(uint64_t));

        int num_spills = ctx->num_spills;
        cuikperf_region_start("select", NULL);
        dyn_array_clear(ra.spills);
        FOR_REV_N(i, 0, aarray_length(stk)) {
            SimplifiedElem s = stk[i];
            VReg* vreg = &ctx->vregs[s.vreg_id];
            // un-yank, this will let us know how far we
            // need to undo the removals on a node
            int degree = ra.adj[s.vreg_id][0] = s.degree;

            // nothing precolored should've landed into this stack
            TB_ASSERT(vreg->assigned < 0);

            #if TB_OPTDEBUG_REGALLOC
            briggs_print_vreg(ctx, &ra, vreg);
            #endif

            RegMask* rm = vreg->mask;
            int def_class = rm->class;
            int num_regs = def_class == REG_CLASS_STK ? stack_cap : ctx->num_regs[rm->class];
            size_t mask_word_count = (num_regs + 63) / 64;
            TB_ASSERT(mask_word_count <= mask_cap);

            // make sure it thinks the unusable regs are "in use" by someone else
            FOR_N(j, 0, rm->count) { mask[j] = ~rm->mask[j]; }
            FOR_N(j, rm->count, (ctx->num_regs[rm->class] + 63) / 64) { mask[j] = UINT64_MAX; }
            if (ctx->num_regs[rm->class] % 64) {
                mask[num_regs / 64] &= UINT64_MAX >> (64ull - (ctx->num_regs[rm->class] % 64));
            }

            #if TB_OPTDEBUG_REGALLOC
            printf("#\n");
            #endif

            FOR_N(j, 1, degree + 1) {
                uint32_t other_id = ra.adj[s.vreg_id][j];
                VReg* other = &ctx->vregs[other_id];
                if (other->mask->class == def_class && other->assigned >= 0) {
                    int other_assigned = other->assigned;
                    mask[other_assigned / 64u] |= (1ull << (other_assigned % 64u));

                    #if 1 && TB_OPTDEBUG_REGALLOC
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

            if (!reg_assign(ctx, vreg, mask, num_regs)) {
                // if a stack slot failed to color then it means we
                // need more stack slots (there's an indefinite amount :p)
                if (def_class == REG_CLASS_STK) {
                    vreg->class = REG_CLASS_STK;
                    vreg->assigned = num_spills++;

                    TB_OPTDEBUG(REGALLOC)(printf("#   assigned to STACK%d (new stack slot)\n", vreg->assigned));
                } else {
                    dyn_array_put(ra.spills, s.vreg_id);
                }
            }
        }
        cuikperf_region_end();

        // if we found nothing to spill, we've successfully colored vregs
        if (dyn_array_length(ra.spills) == 0) {
            log_debug("%s: briggs: good coloring (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
            dyn_array_destroy(ra.spills);
            tb_arena_restore(arena, sp);

            ctx->num_spills = num_spills - (1 + ctx->param_count + ctx->call_usage);
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

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# V%d: spill (%%%u)\x1b[0m\n", vreg_id, n->gvn));

            // rematerialization candidates will delete the original def and for now, they'll
            // reload per use site (although we might wanna coalesce some later on).
            if (can_remat(ctx, n)) {
                rematerialize(ctx, NULL, n);
            } else {
                ctx->vregs[vreg_id].mask = ctx->constraint(ctx, n, NULL);
                ctx->vregs[vreg_id].spill_cost = NAN;

                RegMask* spill_rm = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                TB_Node* spill_n  = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                subsume_node2(f, n, spill_n);
                set_input(f, spill_n, n, 1);
                TB_NODE_SET_EXTRA(spill_n, TB_NodeMachCopy, .def = spill_rm, .use = ctx->vregs[vreg_id].mask);

                tb__insert_after(ctx, f, spill_n, n);
                VReg* spill_vreg = tb__set_node_vreg(ctx, spill_n);
                spill_vreg->spill_cost = INFINITY;
                spill_entire_lifetime(ctx, spill_vreg, spill_rm, false);

                // spill_fancy(ctx, &ra, vreg_id);
            }
        }
        cuikperf_region_end();

        // undo assignments
        FOR_N(i, 0, aarray_length(stk)) {
            SimplifiedElem s = stk[i];
            VReg* vreg = &ctx->vregs[s.vreg_id];

            vreg->class    = 0;
            vreg->assigned = -1;
        }
        log_debug("%s: briggs: failed to color (arena = %.1f KiB)", f->super.name, (tb_arena_current_size(arena) - baseline) / 1024.0f);
        tb_arena_restore(arena, sp);

        // recompute liveness
        redo_dataflow(ctx, arena);
        dump_sched(ctx);

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
        if (other_id > 0 && vreg_id != other_id && bits32_member(live, k)) {
            if (reg_mask_may_intersect(vreg_mask, ctx->vregs[other_id].mask)) {
                // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%td\n", vreg_id, k));
                ifg_edge(ra, vreg_id, other_id);
            }
        }
    }
}

static void p_up(Briggs_Pressure* p, int pos) {
    if (++p->curr > p->max) {
        p->max = p->curr;
        if (p->lo2hi < 0 && p->max > 16) {
            p->lo2hi = pos;
        }
    }
}

static void p_down(Briggs_Pressure* p) {
    p->curr--;
}

static void ifg_build(Ctx* restrict ctx, Briggs* ra) {
    TB_Function* f = ctx->f;

    ra->ifg_len = aarray_length(ctx->vregs);
    ra->ifg = tb_arena_alloc(ra->arena, ra->ifg_len * sizeof(Briggs_Set*));

    ra->block_pressures[0] = NULL;
    FOR_N(i, 1, ctx->num_classes) {
        ra->block_pressures[i] = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(Briggs_Pressure));
        memset(ra->block_pressures[i], 0, ctx->bb_count * sizeof(Briggs_Pressure));

        FOR_N(j, 0, ctx->bb_count) {
            ra->block_pressures[i][j].lo2hi = -1;
        }
    }

    log_debug("%s: briggs: allocating IFG skeleton (%zu nodes)", f->super.name, ra->ifg_len);

    ra->ifg_chunks_per_set = (ra->ifg_len + BRIGGS_BITS_PER_CHUNK - 1) / BRIGGS_BITS_PER_CHUNK;
    size_t set_size = sizeof(Briggs_Set) + ra->ifg_chunks_per_set*sizeof(Briggs_Chunk);
    FOR_N(i, 0, ra->ifg_len) {
        ra->ifg[i] = tb_arena_alloc(ra->arena, set_size);
        memset(ra->ifg[i], 0, set_size);
    }

    size_t live_count = ((f->node_count + 63) / 64) * 2;
    uint32_t* live = tb_arena_alloc(ra->arena, live_count * sizeof(uint32_t));

    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        FOR_N(j, 0, (bb->live_out.capacity + 63) / 64) {
            live[j*2 + 0] = bb->live_out.data[j] & 0xFFFFFFFF;
            live[j*2 + 1] = bb->live_out.data[j] >> 32ull;
        }

        // current
        int j = bits32_first(live, live_count);
        while (j >= 0) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                int def_class = ctx->vregs[vreg_id].mask->class;
                if (def_class != REG_CLASS_STK) {
                    p_up(&ra->block_pressures[def_class][i], 0);
                }
            }
            j = bits32_next(live, live_count, j);
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
                if (def_mask->class != REG_CLASS_STK) {
                    p_down(&ra->block_pressures[def_mask->class][i]);
                }

                interfere_live(ctx, ra, live, vreg_id);

                // 2 address ops will interfere with their own inputs (except for
                // shared dst/src)
                int shared_edge = ctx->node_2addr(n);
                if (shared_edge >= 0) {
                    TB_ASSERT(shared_edge < n->input_count);
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
                }
            }

            // uses are live now
            if (n->type != TB_PHI) {
                FOR_N(k, 0, n->input_count) {
                    TB_Node* in = n->inputs[k];
                    if (in) {
                        TB_ASSERT(in->gvn < f->node_count);

                        int in_class = ctx->vreg_map[in->gvn] > 0 ? ctx->vregs[ctx->vreg_map[in->gvn]].mask->class : REG_CLASS_STK;
                        if (!bits32_test_n_set(live, in->gvn) &&
                            in_class != REG_CLASS_STK
                        ) {
                            p_up(&ra->block_pressures[in_class][i], k);
                        }
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
            if (n->inputs[shared_edge] == NULL) { continue; }
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
    return mask->class == REG_CLASS_STK || ifg_degree(ra, vreg_id) < popcnt_reg_mask(mask);
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

            if (ctx->vregs[vreg_id].mask->class == REG_CLASS_STK) {
                ra->max_spills += 1;
            }

            int d = ra->adj[vreg_id][0];
            ifg_remove_edges(ctx, ra, vreg_id, &lo, &hi);

            if (ctx->vregs[vreg_id].assigned < 0) {
                SimplifiedElem s = { vreg_id, d };
                aarray_push(stk, s);
            }
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

        // how was it spilled and precolored?!
        TB_ASSERT(ctx->vregs[best_spill].assigned < 0);
        SimplifiedElem s = { best_spill, d };
        aarray_push(stk, s);

        // TODO(NeGate): assuming 1 stack slot per vreg (will become incorrect later on)
        ra->max_spills += 1;
    }
}
