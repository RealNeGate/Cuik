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

    // Splitting crap:
    //   [def_i*num_spills + spill_i]
    TB_Node** defs;
    TB_Node** phis;
    int* spill_vregs;

    RegMask** spill_mask;
    RegMask** reload_mask;

    uint64_t* spill_in;
    uint64_t* spill_out;
    uint64_t* copy_first;
    uint64_t* copy_last;
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

static void briggs_dump_defs(Ctx* restrict ctx, Briggs* restrict ra, int i, int num_spills) {
    printf("{");
    FOR_N(j, 0, num_spills) {
        if (ra->defs[i*num_spills + j]) {
            printf(" %%%-4u", ra->defs[i*num_spills + j]->gvn);
        } else {
            printf(" ___");
        }
    }
    printf(" }\n");
}

static void briggs_dump_sched(Ctx* restrict ctx, Briggs* restrict ra) {
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf("BB %zu (freq=%f, pressures: {%d, %d}):\n", i, bb->freq, ra->block_pressures[1][i].max, ra->block_pressures[2][i].max);
        aarray_for(i, bb->items) {
            ctx->print_pretty(ctx, bb->items[i]);
            // printf("  ");
            // tb_print_dumb_node(NULL, bb->items[i]);
            printf("\n");
        }
    }
}

static void briggs_insert_op(Ctx* ctx, Briggs* ra, int bb_id, TB_Node* n, int pos) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

    // skip phis and projections so that they stay nice and snug
    size_t cnt = aarray_length(bb->items);
    /* while (pos < cnt && is_proj(bb->items[pos])) {
        pos += 1;
    } */

    aarray_push(bb->items, 0);
    memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
    bb->items[pos] = n;

    FOR_N(i, 1, ctx->num_classes) {
        Briggs_Pressure* p = &ra->block_pressures[i][bb - ctx->cfg.blocks];
        if (pos >= p->lo2hi) {
            p->lo2hi += 1;
        }
    }

    tb__insert(ctx, ctx->f, bb, n);
}

// inserts spills and reloads that happen across blocks
static TB_Node* spill_fancy_xbb(Ctx* ctx, Briggs* ra, int i, int spill_i, TB_Node* phi, int def_class, int num_spills) {
    TB_Function* f = ctx->f;
    RegMask* spill_rm = ctx->mayspill_mask[def_class];
    RegMask* reload_rm = ctx->normie_mask[def_class];

    TB_Node* bb_node = ctx->cfg.blocks[i].start;
    int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;

    TB_Node** pred_defs = tb_arena_alloc(ra->arena, pred_count * sizeof(TB_Node*));

    bool same = true;
    TB_Node* leader = NULL;
    FOR_N(j, 0, pred_count) {
        TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
        TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

        TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
        int pred_id = pred_bb - ctx->cfg.blocks;

        TB_Node* pred_def = ra->defs[pred_id*num_spills + spill_i];
        if (pred_def == NULL) {
            continue;
        }

        // SPILL FROM P TO B IF:
        //   if it was in a register but we'd rather it be spilled, then we'll do
        //   that at the end of the pred block.
        if (!bits64_member(ra->spill_out, i) && bits64_member(ra->copy_last, i)) {
            #if TB_OPTDEBUG_REGALLOC
            printf("      Spill%d: spilled in BB%d!\n", spill_i, pred_id);
            #endif

            TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, pred_def->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, v, pred_def, 1);
            TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = spill_rm, .use = reload_rm);

            TB_ASSERT(ra->spill_vregs[spill_i] != 0);
            aarray_insert(ctx->vreg_map, v->gvn, ra->spill_vregs[spill_i]);

            tb__insert_before(ctx, f, v, pred_bb->end);
            pred_defs[j] = v;
        } else {
            pred_defs[j] = pred_def;
        }

        // printf("  DEF[%zu] %%%u\n", j, pred_def ? pred_def->gvn : 0);

        // are they all the same edge?
        if (leader == NULL) { leader = pred_defs[j]; }
        else if (pred_defs[j] != phi && pred_defs[j] != leader) { same = false; }
    }

    if (same) {
        tb_arena_free(ra->arena, pred_defs, pred_count * sizeof(TB_Node*));
        return leader ? leader : phi;
    }

    // construct phi
    if (phi == NULL) {
        phi = tb_alloc_node(f, TB_PHI, TB_TYPE_VOID, 1 + pred_count, 0);
        set_input(f, phi, bb_node, 0);
        briggs_insert_op(ctx, ra, i, phi, 1);

        // give it a VReg
        tb__set_node_vreg(ctx, phi);
    }

    FOR_N(j, 0, pred_count) {
        set_input(f, phi, pred_defs[j], j+1);
        if (pred_defs[j] && phi->dt.type == TB_TAG_VOID) {
            phi->dt = pred_defs[j]->dt;
        }
    }

    tb_arena_free(ra->arena, pred_defs, pred_count * sizeof(TB_Node*));
    return phi;
}

static int spill_map_get(NL_Table* spill_map, int k) {
    intptr_t p = (intptr_t) nl_table_get(spill_map, (void*) (uintptr_t) k);
    return p - 1;
}

static void briggs_split_def(Ctx* ctx, Briggs* ra, int bb, int spill_i, int pos) {
    int num_spills = dyn_array_length(ra->spills);

    TB_Node* def = ra->defs[bb*num_spills + spill_i];
    RegMask* def_mask = ctx->constraint(ctx, def, NULL);
    TB_ASSERT(def_mask->class != REG_CLASS_STK);

    RegMask* spill_mask = ra->spill_mask[spill_i];
    TB_Node* v = tb_alloc_node(ctx->f, TB_MACH_COPY, def->dt, 2, sizeof(TB_NodeMachCopy));
    set_input(ctx->f, v, def, 1);
    TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = spill_mask, .use = def_mask);

    briggs_insert_op(ctx, ra, bb, v, pos);
    ra->defs[bb*num_spills + spill_i] = v;

    // there's one shared spilled vreg for all the stack copies
    int spill_vreg_id = ra->spill_vregs[spill_i];
    if (spill_vreg_id == 0) {
        VReg* spill_vreg = tb__set_node_vreg(ctx, v);
        // spill_vreg->spill_cost = INFINITY;
        spill_vreg->mask = spill_mask;
        spill_vreg->was_spilled = true;

        ra->spill_vregs[spill_i] = spill_vreg - ctx->vregs;
    } else {
        ctx->vregs[spill_vreg_id].uses += 1;
        ctx->vregs[spill_vreg_id].was_spilled = true;
        aarray_insert(ctx->vreg_map, v->gvn, spill_vreg_id);
    }

    TB_OPTDEBUG(REGALLOC4)(printf("      Spill%d: onto stack now! %%%u (we've crossed an HRP region)\n", spill_i, v->gvn));
}

static void split_fancy(Ctx* ctx, Briggs* ra) {
    TB_Function* f = ctx->f;

    // TODO(NeGate): use a hash map, far more compact
    int num_spills = dyn_array_length(ra->spills);
    TB_ASSERT(num_spills <= 64);

    NL_Table spill_map = nl_table_alloc(num_spills);

    RegMask** og_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));

    ra->spill_mask  = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    ra->reload_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    FOR_N(i, 0, num_spills) {
        nl_table_put(&spill_map, (void*) (uintptr_t) ra->spills[i], (void*) (i + 1));

        // reset the spilled vregs since we're gonna split off the uses now
        VReg* to_spill = &ctx->vregs[ra->spills[i]];

        // all reloads will use the normie mask for the class
        og_mask[i] = to_spill->mask;

        // once we've been "raised" to the highest normie mask
        ra->spill_mask[i] = ctx->mayspill_mask[to_spill->mask->class];
        ra->reload_mask[i] = ctx->normie_mask[to_spill->mask->class];

        to_spill->mask = NULL;
        to_spill->spill_cost = NAN;
        to_spill->uses = 0;
    }

    // [def_i*num_spills + spill_i]
    ra->defs = tb_arena_alloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    ra->phis = tb_arena_alloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    ra->spill_vregs = tb_arena_alloc(ra->arena, num_spills * sizeof(int));

    FOR_N(i, 0, num_spills * ctx->bb_count) {
        ra->spill_vregs[i] = 0;
    }

    // everything starts as dead and unspilled
    FOR_N(i, 0, num_spills * ctx->bb_count) {
        ra->defs[i] = NULL;
        ra->phis[i] = NULL;
    }

    uint64_t* def_map = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* use_map = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    ra->spill_in  = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    ra->spill_out = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    // if our preds differ, we can either handle the copies ourselves or let them handle it.
    ra->copy_last  = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    ra->copy_first = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    // for each of the reg classes, we wanna know which vregs fit into
    // which camps.
    uint64_t class2vreg[8] = { 0 };
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        uint64_t x = 0;
        FOR_N(j, 0, num_spills) {
            int class = ra->reload_mask[j]->class;
            TB_ASSERT(class > 0);

            bool is_hrp = ra->block_pressures[class][i].max >= popcnt_reg_mask(og_mask[j]);
            x |= is_hrp ? (1ull << j) : 0;

            class2vreg[class] |= 1ull << j;
        }
        ra->copy_first[i] = 0;
        ra->copy_last[i] = 0;
        ra->spill_in[i] = 0;
        ra->spill_out[i] = 0;

        // fill defs & use maps
        def_map[i] = use_map[i] = 0;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            int dst_vreg = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;

            FOR_N(k, 1, n->input_count) {
                if (n->inputs[k] == NULL) { continue; }

                int src_vreg = ctx->vreg_map[n->inputs[k]->gvn];
                int spill_i = spill_map_get(&spill_map, src_vreg);
                if (spill_i >= 0) {
                    use_map[i] |= 1ull << spill_i;
                }
            }

            int spill_i = spill_map_get(&spill_map, dst_vreg);
            if (spill_i >= 0) {
                def_map[i] |= 1ull << spill_i;
            }
        }

        printf("  BB%-3zu: ", i);
        FOR_REV_N(j, 0, num_spills) {
            putchar((x >> j) & 1 ? '1' : '0');
        }
        printf(" D=");
        FOR_REV_N(j, 0, num_spills) {
            putchar((def_map[i] >> j) & 1 ? '1' : '0');
        }
        printf(" U=");
        FOR_REV_N(j, 0, num_spills) {
            putchar((use_map[i] >> j) & 1 ? '1' : '0');
        }
        printf(" ");
        FOR_SUCC(it, bb->end) {
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, it.succ);
            printf("  BB%-3zu", succ_bb - ctx->cfg.blocks);
        }
        printf("\n");
    }

    ////////////////////////////////
    // Split around loops
    ////////////////////////////////
    // Any splits which happen in a block will opt to split around
    // the block if there's no uses within the block.
    printf("\nSplit around loops:\n");
    FOR_N(i, 1, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        TB_Node* bb_node = bb->start;
        int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
        int loop_tail = -1;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id >= i && loop_tail < pred_id) {
                loop_tail = pred_id;
            }
        }

        if (loop_tail >= 0) {
            printf("  Loop(BB%3zu BB%3d):\n", i, loop_tail);

            uint64_t spill_in_loop = 0;
            uint64_t use_in_loop = 0;
            FOR_N(j, i, loop_tail) {
                use_in_loop |= def_map[j];
                use_in_loop |= use_map[j];

                spill_in_loop |= ra->spill_out[j];
            }

            // if it's unused in the loop and spilled in it? hoist that
            spill_in_loop = ~use_in_loop;
            if (spill_in_loop) {
                FOR_N(j, 0, num_spills) {
                    if ((spill_in_loop >> j) & 1) {
                        printf("    Spill%zu: split around loop\n", j);
                    }
                }

                FOR_N(j, i, loop_tail) {
                    ra->spill_out[j] |= spill_in_loop;
                }
            }
        }
    }

    // compute "spill in" statuses, for blocks with all matching preds they'll just
    // flow that up. blocks which differ will try to minimize inserting moves in high
    // frequency blocks
    int freq[64];
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        TB_Node* bb_node = bb->start;
        int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
        int* preds = tb_arena_alloc(ra->arena, pred_count * sizeof(int));

        // gonna track how many preds consider the value to be registers
        FOR_N(j, 0, num_spills) {
            freq[j] = 0;
        }

        int loop_tail = -1;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            FOR_N(k, 0, num_spills) {
                freq[k] += ((ra->spill_out[pred_id] >> k) & 1) == 0;
            }
            preds[j] = pred_id;
        }
        tb_arena_free(ra->arena, preds, pred_count * sizeof(int));

        printf("  BB%-3zu:\n", i);
        FOR_N(j, 0, num_spills) {
            int f = freq[j];
            if (f != 0 && f != pred_count) {
                // disagreement
                ra->spill_in[i]  |= 1ull << j;
                ra->copy_last[i] |= 1ull << j;
                printf("  Spill%zu: spilling\n", j);

                // if the value was not spilled, it'll do so in the pred
                // merge in as a stack-phi and reload.
                /* ra->spill_in[i]  |= 1ull << j;
                ra->copy_first[i] |= 1ull << j;
                printf("  Spill%zu: reloading\n", j); */
            }
        }
    }
    __debugbreak();

    ////////////////////////////////
    // Insert copies and stitch everything back together now
    ////////////////////////////////
    // loops will have incomplete phis we'll wanna return to once the loop is
    // done, to do that we just push the loop tail onto this stack and pop it
    // later.
    ArenaArray(int) loop_stack = aarray_create(ra->arena, int, ctx->bb_count);
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];

        TB_Node* bb_node = bb->start;
        int pred_count = bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT ? 0 : bb_node->input_count;
        int* preds = tb_arena_alloc(ra->arena, pred_count * sizeof(int));

        int loop_tail = -1;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, bb_node, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id >= i && loop_tail < pred_id) {
                loop_tail = pred_id;
            }
            preds[j] = pred_id;
        }

        // pre-emptively place loop phis, if the loop has any potential defs in it.
        if (loop_tail >= 0) {
            // insert phis only if there's defs in the loop
            uint64_t def_in_loop = 0;
            FOR_N(j, i, loop_tail) {
                def_in_loop |= def_map[j];
            }

            // there can only be one path coming from behind, since
            // that means it's the only candidate for dominator.
            int dom = -1;
            FOR_N(j, 0, pred_count) {
                if (preds[j] < i) {
                    if (dom < 0) { dom = j; }
                    else if (dom != j) { dom = -1; break; }
                }
            }

            FOR_N(j, 0, num_spills) {
                if (dom >= 0 && (def_in_loop & (1ull << j)) == 0) {
                    ra->phis[i*num_spills + j] = NULL;
                    ra->defs[i*num_spills + j] = ra->defs[dom*num_spills + j];
                    continue;
                }

                TB_Node* phi = tb_alloc_node(f, TB_PHI, TB_TYPE_VOID, 1 + pred_count, 0);
                set_input(f, phi, bb_node, 0);
                briggs_insert_op(ctx, ra, i, phi, 1);
                ra->phis[i*num_spills + j] = phi;
                ra->defs[i*num_spills + j] = phi;
                tb__set_node_vreg(ctx, phi);
            }

            // push both head & tail, we'll fill the phis later
            aarray_push(loop_stack, i);
            aarray_push(loop_stack, loop_tail);
        } else {
            FOR_N(j, 0, num_spills) {
                int reg_class = ra->reload_mask[j]->class;
                TB_ASSERT(reg_class != REG_CLASS_STK);

                ra->defs[i*num_spills + j] = spill_fancy_xbb(ctx, ra, i, j, ra->phis[i*num_spills + j], reg_class, num_spills);
            }
        }
        tb_arena_free(ra->arena, preds, pred_count * sizeof(int));

        if (ra->copy_first[i]) {
            FOR_N(j, 0, num_spills) {
                if ((ra->copy_first[i] >> j) & 1) {
                    #if TB_OPTDEBUG_REGALLOC
                    printf("  Spill%zu: reloaded in BB%zu!\n", j, i);
                    #endif

                    TB_Node* def = ra->defs[i*num_spills + j];
                    RegMask* spill_mask  = ra->spill_mask[j];
                    RegMask* reload_mask = ra->reload_mask[j];

                    TB_Node* v = tb_alloc_node(f, TB_MACH_COPY, def->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, v, def, 1);
                    TB_NODE_SET_EXTRA(v, TB_NodeMachCopy, .def = reload_mask, .use = spill_mask);
                    tb__set_node_vreg(ctx, v);

                    // append right before the terminator block
                    int p = 1;
                    while (p < aarray_length(bb->items) && is_proj(bb->items[p])) {
                        p++;
                    }
                    briggs_insert_op(ctx, ra, i, v, p);
                    ra->defs[i*num_spills + j] = v;
                }
            }
        }

        printf("BB%-3zu ", i);
        briggs_dump_defs(ctx, ra, i, num_spills);

        uint64_t curr = ra->spill_in[i];

        // if we cross LRP->HRP then we spill, use we're needed then we reload
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            int dst_vreg = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;

            // if we should spill, we'll try to do it right before the LRP->HRP shift.
            FOR_N(k, 1, ctx->num_classes) {
                uint64_t to_spill_in_class = class2vreg[k] & ra->spill_out[i] & ~curr;
                if (to_spill_in_class != 0 && (ra->block_pressures[k][i].lo2hi < 0 || j >= ra->block_pressures[k][i].lo2hi)) {
                    FOR_N(spill_i, 0, num_spills) {
                        TB_Node* def = ra->defs[i*num_spills + spill_i];
                        if (def != NULL && ((to_spill_in_class >> spill_i) & 1)) {
                            briggs_split_def(ctx, ra, i, spill_i, j++);
                            curr |= 1ull << spill_i;
                            continue;
                        }
                    }
                }
            }

            // forced reloads before use
            bool in_constraints = false;
            FOR_N(k, 1, n->input_count) {
                if (n->inputs[k] == NULL) { continue; }

                int src_vreg = ctx->vreg_map[n->inputs[k]->gvn];
                int spill_i = spill_map_get(&spill_map, src_vreg);
                if (spill_i >= 0) {
                    TB_Node* def = ra->defs[i*num_spills + spill_i];
                    TB_ASSERT(def);

                    if (!in_constraints) {
                        in_constraints = true;
                        ctx->constraint(ctx, n, ctx->ins);
                    }

                    RegMask* reload_mask = ctx->ins[k];
                    if (n->inputs[k]->type == TB_MACH_COPY) {
                        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n->inputs[k]);
                        cpy->use = ra->reload_mask[spill_i];
                        curr &= ~(1ull << spill_i);

                        TB_OPTDEBUG(REGALLOC4)(printf("  Spill%d: rewrote copy node! %%%u\n", spill_i, def->gvn));
                        continue;
                    }

                    if (reload_mask->may_spill) {
                        TB_OPTDEBUG(REGALLOC4)(printf("  Spill%d: we did a folded reload! %%%u\n", spill_i, def->gvn));
                    } else if ((curr >> spill_i) & 1) {
                        RegMask* spill_mask = ra->spill_mask[spill_i];

                        TB_Node* reload = tb_alloc_node(f, TB_MACH_COPY, def->dt, 2, sizeof(TB_NodeMachCopy));
                        set_input(f, reload, def, 1);
                        TB_NODE_SET_EXTRA(reload, TB_NodeMachCopy, .def = reload_mask, .use = spill_mask);

                        // reloads have their own vregs, but never overlap (useful for phis)
                        VReg* reload_vreg = tb__set_node_vreg(ctx, reload);
                        reload_vreg->mask = NULL;

                        int reload_vreg_id = reload_vreg - ctx->vregs;
                        nl_table_put(&spill_map, (void*) (uintptr_t) reload_vreg_id, (void*) (uintptr_t) (spill_i + 1));

                        briggs_insert_op(ctx, ra, i, reload, j++);
                        ra->defs[i*num_spills + spill_i] = def = reload;

                        curr &= ~(1ull << spill_i);
                        TB_OPTDEBUG(REGALLOC4)(printf("      Spill%d: we need to reload now for %%%u! %%%u\n", spill_i, n->gvn, reload->gvn));
                    }

                    // intersect against uses
                    VReg* reload_vreg = &ctx->vregs[ctx->vreg_map[def->gvn]];
                    if (!reload_mask->may_spill) {
                        reload_vreg->mask = tb__reg_mask_meet(ctx, reload_vreg->mask, reload_mask);
                    }
                    set_input(f, n, def, k);
                }
            }

            int spill_i = spill_map_get(&spill_map, dst_vreg);
            if (spill_i >= 0) {
                if (n->type == TB_MACH_COPY) {
                    // there's one shared spilled vreg for all the stack copies
                    int spill_vreg_id = ra->spill_vregs[spill_i];
                    if (spill_vreg_id == 0) {
                        VReg* spill_vreg = tb__set_node_vreg(ctx, n);
                        spill_vreg->spill_cost = INFINITY;
                        spill_vreg->mask = ra->spill_mask[spill_i];
                        spill_vreg->was_spilled = true;

                        ra->spill_vregs[spill_i] = spill_vreg - ctx->vregs;
                    } else {
                        ctx->vregs[spill_vreg_id].uses += 1;
                        ctx->vregs[spill_vreg_id].was_spilled = true;
                        aarray_insert(ctx->vreg_map, n->gvn, spill_vreg_id);
                    }

                    TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
                    cpy->def = ra->spill_mask[spill_i];

                    FOR_USERS(u, n) {
                        TB_Node* un = USERN(u);
                        if (un->type == TB_MACH_COPY) {
                            TB_NodeMachCopy* un_cpy = TB_NODE_GET_EXTRA(un);
                            un_cpy->use = cpy->def;
                        }
                    }

                    nl_table_put(&spill_map, (void*) (uintptr_t) ctx->vreg_map[n->gvn], (void*) (uintptr_t) (spill_i + 1));
                    ra->defs[i*num_spills + spill_i] = n;
                    curr |= 1ull << spill_i;
                } else if (can_remat(ctx, n)) {
                    TB_OPTDEBUG(REGALLOC4)(printf("      Spill%d: remateralize! (%%%u)\n", spill_i, n->gvn));
                    rematerialize(ctx, NULL, n);
                    j--;

                    // no longer alive, we don't propagate further
                    ra->defs[i*num_spills + spill_i] = NULL;
                    curr &= ~(1ull << spill_i);
                } else {
                    TB_OPTDEBUG(REGALLOC4)(printf("      Spill%d: in register! (%%%u)\n", spill_i, n->gvn));

                    // inherit the vreg from the guy right above you... unless
                    // he's a spill
                    TB_Node* old = ra->defs[i*num_spills + spill_i];
                    if (((curr >> spill_i) & 1) == 0 && old != NULL) {
                        int old_vreg = ctx->vreg_map[old->gvn];

                        // intersect both lifetimes, they should never produce an empty set because
                        // we've gotten rid of hard conflicts by this point.
                        ctx->vregs[old_vreg].mask = tb__reg_mask_meet(ctx, ctx->vregs[old_vreg].mask, ctx->constraint(ctx, n, NULL));
                        ctx->vregs[old_vreg].uses += 1;

                        aarray_insert(ctx->vreg_map, n->gvn, old_vreg);
                    } else {
                        // assign new vreg
                        VReg* new_vreg = tb__set_node_vreg(ctx, n);
                        new_vreg->mask = ctx->constraint(ctx, n, NULL);
                    }
                    int vreg_id = ctx->vreg_map[n->gvn];
                    nl_table_put(&spill_map, (void*) (uintptr_t) vreg_id, (void*) (uintptr_t) (spill_i + 1));
                    ra->defs[i*num_spills + spill_i] = n;
                    curr &= ~(1ull << spill_i);

                    // if it's been spilled before, we aggressively split it
                    if (ctx->vregs[vreg_id].was_spilled) {
                        briggs_split_def(ctx, ra, i, spill_i, j++);
                        curr |= 1ull << spill_i;
                    }
                }
            }
        }

        printf("      ");
        briggs_dump_defs(ctx, ra, i, num_spills);

        int loop_stack_len = aarray_length(loop_stack);
        if (loop_stack_len && loop_stack[loop_stack_len - 1] == i) {
            int head = loop_stack[loop_stack_len - 2];
            aarray_set_length(loop_stack, loop_stack_len - 2);

            TB_OPTDEBUG(REGALLOC4)(printf("  COMPLETED LOOP BB%d!\n", head));

            TB_Node* loop_node = ctx->cfg.blocks[head].start;
            TB_ASSERT(cfg_is_region(loop_node));

            FOR_N(j, 0, num_spills) {
                if (ra->phis[head*num_spills + j] == NULL) {
                    continue;
                }

                int reg_class = ra->reload_mask[j]->class;
                TB_ASSERT(reg_class != REG_CLASS_STK);

                // insert fully resolved phis now
                TB_Node* p = spill_fancy_xbb(ctx, ra, head, j, ra->phis[head*num_spills + j], reg_class, num_spills);
                if (p != ra->phis[head*num_spills + j]) {
                    TB_Node* old_p = ra->phis[head*num_spills + j];

                    TB_ASSERT(ctx->vreg_map[old_p->gvn] > 0);
                    ctx->vregs[ctx->vreg_map[old_p->gvn]].uses -= 1;

                    tb__remove_node(ctx, ctx->f, old_p);
                    subsume_node(f, old_p, p);
                }

                ra->phis[head*num_spills + j] = p;
                ra->defs[head*num_spills + j] = p;
            }
        }
    }
    nl_table_free(spill_map);
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

        // build IFG
        CUIK_TIMED_BLOCK("IFG build") {
            log_debug("%s: briggs: building IFG", f->super.name);
            ifg_build(ctx, &ra);
        }
        TB_ASSERT(rounds <= 10);

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

        #if TB_OPTDEBUG_REGALLOC2
        printf("###############################\n");
        printf("# simplify phase              #\n");
        printf("###############################\n");
        #endif

        // simplify (find potential spills)
        cuikperf_region_start("simplify", NULL);
        ArenaArray(SimplifiedElem) stk = ifg_simplify(ctx, &ra);
        cuikperf_region_end();

        // select (can fail to color due)
        #if TB_OPTDEBUG_REGALLOC3
        printf("###############################\n");
        printf("# select phase                #\n");
        printf("###############################\n");
        #endif

        int stack_cap = ctx->num_regs[REG_CLASS_STK] + ra.max_spills;
        int mask_cap = stack_cap < max_regs_in_class ? max_regs_in_class : stack_cap;
        uint64_t* mask = tb_arena_alloc(arena, ((mask_cap+63)/64) * sizeof(uint64_t));

        int num_spills = ctx->num_regs[REG_CLASS_STK];
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

            #if TB_OPTDEBUG_REGALLOC3
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

            #if TB_OPTDEBUG_REGALLOC3
            printf("#\n");
            #endif

            FOR_N(j, 1, degree + 1) {
                uint32_t other_id = ra.adj[s.vreg_id][j];
                VReg* other = &ctx->vregs[other_id];
                if (other->mask->class == def_class && other->assigned >= 0) {
                    int other_assigned = other->assigned;
                    mask[other_assigned / 64u] |= (1ull << (other_assigned % 64u));

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

            if (!reg_assign(ctx, vreg, mask, 1, num_regs)) {
                // if a stack slot failed to color then it means we
                // need more stack slots (there's an indefinite amount :p)
                TB_ASSERT(def_class != REG_CLASS_STK);
                dyn_array_put(ra.spills, s.vreg_id);
                TB_OPTDEBUG(REGALLOC3)(printf("#   assigned UNCOLORED\n"));
            } else {
                if (def_class == REG_CLASS_STK && vreg->assigned+1 > num_spills) {
                    num_spills = vreg->assigned+1;
                }

                TB_OPTDEBUG(REGALLOC3)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
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

        #if TB_OPTDEBUG_REGALLOC4
        printf("###############################\n");
        printf("# spill phase (%4zu)          #\n", dyn_array_length(ra.spills));
        printf("###############################\n");
        dyn_array_for(i, ra.spills) {
            briggs_print_vreg(ctx, &ra, &ctx->vregs[ra.spills[i]]);
        }
        #endif

        // if anything in the spill candidates failed to color, we need to spill. note that
        // it wouldn't have been able to fail coloring without making it to this list.
        cuikperf_region_start("spill", NULL);
        split_fancy(ctx, &ra);
        TB_OPTDEBUG(REGALLOC4)(briggs_dump_sched(ctx, &ra));
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
        // dump_sched(ctx);

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

static void p_up(Briggs_Pressure* p) {
    if (++p->curr > p->max) {
        p->max = p->curr;
    }
}

static void p_down(Briggs_Pressure* p, int pos) {
    p->curr--;
    if (p->curr == 16) {
        p->lo2hi = pos;
    }
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
        size_t item_count = aarray_length(bb->items);
        int j = bits32_first(live, live_count);
        while (j >= 0) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                int def_class = ctx->vregs[vreg_id].mask->class;
                if (def_class != REG_CLASS_STK) {
                    p_up(&ra->block_pressures[def_class][i]);
                }
            }
            j = bits32_next(live, live_count, j);
        }

        #if 0 // TB_OPTDEBUG_REGALLOC4
        printf("           ");
        for (size_t i = 0; i < f->node_count; i += 10) {
            printf("%-3zu       ", i);
        }
        printf("\n");
        #endif

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
                                // TB_OPTDEBUG(REGALLOC)(printf("V%d -- V%d\n", vreg_id, in_vreg_id));
                                ifg_edge(ra, in_vreg_id, vreg_id);
                                instant_interferences += 1;
                            }
                        }
                    }

                    if (instant_interferences) {
                        Briggs_Pressure* p = &ra->block_pressures[def_mask->class][i];
                        int highest = p->curr + instant_interferences;
                        if (highest > p->max) {
                            p->max = highest;
                        }

                        if (p->max >= 16) {
                            p->lo2hi = j;
                        }
                    }
                }

                if (def_mask->class != REG_CLASS_STK) {
                    p_down(&ra->block_pressures[def_mask->class][i], j);
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
                            p_up(&ra->block_pressures[in_class][i]);
                        }
                    }
                }
            }

            #if 0 // TB_OPTDEBUG_REGALLOC4
            printf("%3d  %3d: %%%-3u: ", ra->block_pressures[1][0].curr, ra->block_pressures[1][0].lo2hi, n->gvn);
            FOR_N(i, 0, f->node_count) {
                if (bits32_member(live, i)) {
                    if (ctx->vreg_map[i] > 0) {
                        int def_class = ctx->vregs[ctx->vreg_map[i]].mask->class;
                        if (def_class == 1) {
                            printf("\x1b[32mX\x1b[0m");
                        } else if (def_class == 2) {
                            printf("\x1b[33mX\x1b[0m");
                        } else {
                            printf("X");
                        }
                    } else {
                        printf("?");
                    }
                } else {
                    printf(" ");
                }
            }
            printf("\n");
            #endif
        }

        FOR_N(j, 1, ctx->num_classes) {
            if (ra->block_pressures[j][i].curr >= 16) {
                ra->block_pressures[j][i].lo2hi = 0;
            }
        }
    }
}

////////////////////////////////
// Coalescing
////////////////////////////////
static bool ifg_coalesce(Ctx* restrict ctx, Briggs* ra, bool aggro) {
    // i just don't trust my ref counts...
    FOR_N(i, 0, ra->ifg_len) {
        ctx->vregs[i].uses = 0;
    }

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        aarray_for(j, bb->items) {
            TB_Node* n = bb->items[j];
            int dst_id = ctx->vreg_map[n->gvn];
            if (dst_id >= 0) {
                ctx->vregs[dst_id].uses += 1;
            }
        }
    }

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
        if (uf_find(uf, i) != i || ctx->vregs[i].mask == NULL) {
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
            #if TB_OPTDEBUG_REGALLOC2
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
        TB_OPTDEBUG(REGALLOC2)(printf("#\n#  SEARCHING FOR SPILL CANDIDATE! (in %d vregs)\n#\n", hi.count));

        // ok we've got too much pressure, let's split a bit
        int best_spill = -1;
        float best_score = INFINITY;

        // pick next best spill
        FOR_N(i, 0, hi.count) {
            VReg* vreg = &ctx->vregs[hi.stack[i]];
            float score = get_spill_cost(ctx, vreg) / ifg_degree(ra, hi.stack[i]);

            TB_OPTDEBUG(REGALLOC2)(briggs_print_vreg(ctx, ra, vreg));
            if (score < best_score) {
                if (best_spill >= 0) {
                    TB_OPTDEBUG(REGALLOC2)(printf("#   V%d is a better spill! V%d (%f is better than %f)\n", hi.stack[i], best_spill, score, best_score));
                } else {
                    TB_OPTDEBUG(REGALLOC2)(printf("#   V%d is... one of the spills of all time! %f\n", hi.stack[i], score));
                }
                best_score = score;
                best_spill = hi.stack[i];
            }
        }
        TB_ASSERT(best_spill >= 0);
        TB_OPTDEBUG(REGALLOC2)(printf("#\n#  V%d WAS SPECULATIVELY SPILLED!\n#\n", best_spill));

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
