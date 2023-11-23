// See codegen.h for more details, this is the implementation file for it, each target
// will include this to define their own copy of the codegen.
//
// Your job is to implement:
//   isel_node, out_reg_mask, in_reg_mask, init_ctx, emit_tile, disassemble.
//
#include "codegen.h"

static void isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n);
static RegMask out_reg_mask(Ctx* restrict ctx, TB_Node* n);
static RegMask in_reg_mask(Ctx* restrict ctx, Tile* tile, TB_Node* n, int i);
static void init_ctx(Ctx* restrict ctx, TB_ABI abi);

// This is where we do the byte emitting phase
static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t);

// Disassembles a basic block
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end);

static uint32_t node_to_bb_hash(void* ptr) { return (((uintptr_t) ptr) * 11400714819323198485ull) >> 32ull; }
static MachineBB* node_to_bb(Ctx* restrict ctx, TB_Node* n) {
    uint32_t h = node_to_bb_hash(n);

    size_t mask = (1 << ctx->node_to_bb.exp) - 1;
    size_t first = h & mask, i = first;
    do {
        if (ctx->node_to_bb.entries[i].k == n) {
            return ctx->node_to_bb.entries[i].v;
        }

        i = (i + 1) & mask;
    } while (i != first);

    abort();
}

static void node_to_bb_put(Ctx* restrict ctx, TB_Node* n, MachineBB* bb) {
    uint32_t h = node_to_bb_hash(n);

    size_t mask = (1 << ctx->node_to_bb.exp) - 1;
    size_t first = h & mask, i = first;
    do {
        if (ctx->node_to_bb.entries[i].k == NULL) {
            ctx->node_to_bb.entries[i].k = n;
            ctx->node_to_bb.entries[i].v = bb;
            return;
        }

        i = (i + 1) & mask;
    } while (i != first);

    abort();
}

static Tile* get_tile(Ctx* restrict ctx, TB_Node* n) {
    if (ctx->values[n->gvn] == NULL) {
        Tile* tile = TB_ARENA_ALLOC(tmp_arena, Tile);
        *tile = (Tile){
            .tag = TILE_NORMAL, .id = ctx->tile_count++,
            .reg = -1, .hint = -1, .assigned = -1, .n = n,
            .range_cap = 4, .range_count = 1,
            .ranges = tb_platform_heap_alloc(4 * sizeof(LiveRange))
        };
        tile->ranges[0] = (LiveRange){ INT_MAX, INT_MAX };
        ctx->values[n->gvn] = tile;
        return tile;
    } else {
        return ctx->values[n->gvn];
    }
}

static void tile_set_ins(Ctx* restrict ctx, Tile* t, TB_Node* n, int start, int end) {
    t->ins = tb_arena_alloc(tmp_arena, (end - start) * sizeof(Tile*));
    t->in_count = end - start;
    FOREACH_N(i, start, end) {
        t->ins[i - start].tile = get_tile(ctx, n->inputs[i]);
        t->ins[i - start].mask = in_reg_mask(ctx, t, n, i);
    }
}

static void fold_node(Ctx* restrict ctx, TB_Node* n) { set_put(&ctx->folded, n->gvn); }
static void compile_function(TB_Passes* restrict p, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, bool emit_asm) {
    verify_tmp_arena(p);

    TB_Arena* arena = tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    TB_Function* restrict f = p->f;
    tb_pass_print(p);

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .p = p,
        .num_classes = REG_CLASS_COUNT,
        .emit = {
            .f = f,
            .output = func_out,
            .data = out,
            .capacity = out_capacity,
        }
    };
    init_ctx(&ctx, f->super.module->target_abi);

    Worklist* restrict ws = &p->worklist;
    worklist_clear(ws);

    ctx.values = tb_arena_alloc(arena, f->node_count * sizeof(Tile*));
    memset(ctx.values, 0, f->node_count * sizeof(Tile*));

    // We need to generate a CFG
    TB_CFG cfg = tb_compute_rpo(f, p);
    // And perform global scheduling
    tb_pass_schedule(p, cfg);

    // allocate more stuff now that we've run stats on the IR
    ctx.emit.label_count = cfg.block_count;
    ctx.emit.labels = tb_arena_alloc(arena, cfg.block_count * sizeof(uint32_t));
    memset(ctx.emit.labels, 0, cfg.block_count * sizeof(uint32_t));

    int bb_count = 0;
    MachineBB* restrict machine_bbs = tb_arena_alloc(arena, cfg.block_count * sizeof(MachineBB));
    TB_Node** bbs = ws->items;

    size_t cap = ((cfg.block_count * 4) / 3);
    ctx.node_to_bb.exp = 64 - __builtin_clzll((cap < 4 ? 4 : cap) - 1);
    ctx.node_to_bb.entries = tb_arena_alloc(arena, (1u << ctx.node_to_bb.exp) * sizeof(NodeToBB));
    memset(ctx.node_to_bb.entries, 0, (1u << ctx.node_to_bb.exp) * sizeof(NodeToBB));

    CUIK_TIMED_BLOCK("isel") {
        assert(dyn_array_length(ws->items) == cfg.block_count);

        // define all PHIs early and sort BB order
        int stop_bb = -1;
        FOREACH_N(i, 0, cfg.block_count) {
            TB_Node* end = nl_map_get_checked(cfg.node_to_block, bbs[i]).end;
            if (end->type == TB_END) {
                stop_bb = i;
            } else {
                machine_bbs[bb_count++] = (MachineBB){ i };
            }
        }

        // enter END block at the... end
        if (stop_bb >= 0) {
            machine_bbs[bb_count++] = (MachineBB){ stop_bb };
        }

        DynArray(PhiVal) phi_vals = NULL;
        FOREACH_N(i, 0, bb_count) {
            int bbid = machine_bbs[i].id;
            TB_Node* bb_start = bbs[bbid];
            TB_BasicBlock* bb = nl_map_get_checked(p->scheduled, bb_start);

            node_to_bb_put(&ctx, bb_start, &machine_bbs[i]);

            if (i + 1 < bb_count) {
                ctx.fallthrough = machine_bbs[i + 1].id;
            } else {
                ctx.fallthrough = INT_MAX;
            }

            // phase 1: logical schedule
            CUIK_TIMED_BLOCK("phase 1") {
                ctx.sched(p, &cfg, ws, &phi_vals, bb, bb->end);
                // assert(phi_vals == NULL);
            }

            // phase 2: reverse walk to generate tiles (greedily)
            CUIK_TIMED_BLOCK("phase 2") {
                TB_OPTDEBUG(CODEGEN)(printf("BB %d\n", bbid));

                Tile* top = NULL;
                Tile* bot = NULL;
                FOREACH_REVERSE_N(i, cfg.block_count, dyn_array_length(ws->items)) {
                    TB_Node* n = ws->items[i];
                    if (set_get(&ctx.folded, n->gvn)) {
                        TB_OPTDEBUG(CODEGEN)(printf("  FOLDED "), print_node_sexpr(n, 0), printf("\n"));
                    } else {
                        TB_OPTDEBUG(CODEGEN)(printf("  TILE "), print_node_sexpr(n, 0), printf("\n"));

                        tb_arena_realign(arena);

                        Tile* tile = get_tile(&ctx, n);
                        tile->next = top;

                        // attach to list
                        if (top) top->prev = tile;
                        if (!bot) bot = tile;
                        top = tile;

                        ctx.values[n->gvn] = tile;
                        isel_node(&ctx, tile, n);

                        tile->mask = out_reg_mask(&ctx, n);
                        if (tile->mask.mask != 0) {
                            TB_OPTDEBUG(CODEGEN)(printf("    [%#04llx]\n", tile->mask.mask));
                        } else {
                            TB_OPTDEBUG(CODEGEN)(printf("    no def\n"));
                        }
                    }
                }

                // if the endpoint is a not a terminator, we've hit some implicit GOTO edge
                TB_Node* end = bb->end;
                if (!cfg_is_terminator(end)) {
                    TB_OPTDEBUG(CODEGEN)(printf("  TERMINATOR %u: ", end->gvn), print_node_sexpr(end, 0), printf("\n"));

                    Tile* tile = TB_ARENA_ALLOC(arena, Tile);
                    TB_Node* succ_n = cfg_next_control(end);

                    // TODO(NeGate): writeback phis
                    /*FOREACH_N(i, 0, ctx->our_phis) {
                        PhiVal* v = &phi_vals[i];
                    }*/

                    *tile = (Tile){ .prev = bot, .tag = TILE_GOTO, .id = ctx.tile_count++, .reg = -1, .hint = -1, .assigned = -1, .n = end, .succ = succ_n };
                    bot->next = tile;
                    bot = tile;
                }

                machine_bbs[bbid].start = top;
                machine_bbs[bbid].end = bot;
                machine_bbs[bbid].end_n = end;
            }
        }
        dyn_array_destroy(phi_vals);
    }

    CUIK_TIMED_BLOCK("liveness") {
        int tile_count = ctx.tile_count;
        ctx.id2tile = tb_arena_alloc(arena, tile_count * sizeof(Tile*));

        // local liveness (along with placing tiles on a timeline)
        FOREACH_N(i, 0, bb_count) {
            MachineBB* mbb = &machine_bbs[i];
            mbb->live_in = set_create_in_arena(arena, tile_count);
            mbb->live_out = set_create_in_arena(arena, tile_count);
        }

        // we don't need to keep the GEN and KILL sets, this doesn't save us
        // much memory but it would potentially mean not using new cachelines
        // in a few of the later stages.
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        CUIK_TIMED_BLOCK("local") {
            int timeline = 2;
            FOREACH_N(i, 0, bb_count) {
                MachineBB* mbb = &machine_bbs[i];
                int bbid = mbb->id;
                Tile* t = mbb->start;

                mbb->gen = set_create_in_arena(arena, tile_count);
                mbb->kill = set_create_in_arena(arena, tile_count);

                Set* gen = &mbb->gen;
                Set* kill = &mbb->kill;
                for (Tile* t = mbb->start; t; t = t->next) {
                    t->time = timeline;
                    timeline += 2;

                    // fill local live sets
                    TB_Node* n = t->n;
                    FOREACH_N(j, 1, n->input_count) if (n->inputs[j]) {
                        TB_Node* in = n->inputs[j];
                        Tile* in_def = ctx.values[in->gvn];
                        if (in_def) {
                            if (!set_get(kill, in_def->id)) {
                                set_put(gen, in_def->id);
                            }
                        }
                    }

                    set_put(kill, t->id);
                    ctx.id2tile[t->id] = t;
                }

                timeline += 2;
            }
        }

        // generate global live sets
        CUIK_TIMED_BLOCK("global") {
            size_t base = dyn_array_length(ws->items);

            // all BB go into the worklist
            FOREACH_REVERSE_N(i, 0, bb_count) {
                // in(bb) = use(bb)
                set_copy(&machine_bbs[i].live_in, &machine_bbs[i].gen);

                TB_Node* n = bbs[machine_bbs[i].id];
                dyn_array_put(ws->items, n);
            }

            Set visited = set_create_in_arena(arena, bb_count);
            while (dyn_array_length(ws->items) > base) CUIK_TIMED_BLOCK("iter")
            {
                TB_Node* bb = dyn_array_pop(ws->items);
                MachineBB* mbb = node_to_bb(&ctx, bb);
                set_remove(&visited, mbb - machine_bbs);

                Set* live_out = &mbb->live_out;
                set_clear(live_out);

                // walk all successors
                TB_Node* end = mbb->end_n;
                if (end->type == TB_BRANCH) {
                    for (User* u = end->users; u; u = u->next) {
                        if (u->n->type == TB_PROJ) {
                            // union with successor's lives
                            TB_Node* succ = cfg_next_bb_after_cproj(u->n);
                            set_union(live_out, &node_to_bb(&ctx, succ)->live_in);
                        }
                    }
                } else {
                    // union with successor's lives
                    TB_Node* succ = cfg_next_control(end);
                    if (succ) set_union(live_out, &node_to_bb(&ctx, succ)->live_in);
                }

                Set* restrict live_in = &mbb->live_in;
                Set* restrict kill = &mbb->kill;
                Set* restrict gen = &mbb->gen;

                // live_in = (live_out - live_kill) U live_gen
                bool changes = false;
                FOREACH_N(i, 0, (tile_count + 63) / 64) {
                    uint64_t new_in = (live_out->data[i] & ~kill->data[i]) | gen->data[i];

                    changes |= (live_in->data[i] != new_in);
                    live_in->data[i] = new_in;
                }

                // if we have changes, mark the predeccesors
                if (changes && !(bb->type == TB_PROJ && bb->inputs[0]->type == TB_START)) {
                    FOREACH_N(i, 0, bb->input_count) {
                        TB_Node* pred = get_pred_cfg(&cfg, bb, i);
                        if (pred->input_count > 0) {
                            MachineBB* pred_mbb = node_to_bb(&ctx, pred);
                            if (!set_get(&visited, pred_mbb - machine_bbs)) {
                                set_put(&visited, pred_mbb - machine_bbs);
                                dyn_array_put(ws->items, pred);
                            }
                        }
                    }
                }
            }
            dyn_array_set_length(ws->items, base);
        }
        tb_arena_restore(arena, sp);

        #if TB_OPTDEBUG_DATAFLOW
        // log live ins and outs
        FOREACH_N(i, 0, bb_count) {
            MachineBB* mbb = &machine_bbs[i];

            printf("BB%d:\n  live-ins:", mbb->id);
            FOREACH_N(j, 0, tile_count) if (set_get(&mbb->live_in, j)) {
                printf(" v%zu", j);
            }
            printf("\n  live-outs:");
            FOREACH_N(j, 0, tile_count) if (set_get(&mbb->live_out, j)) {
                printf(" v%zu", j);
            }
            printf("\n");
        }
        #endif
    }

    CUIK_TIMED_BLOCK("regalloc") {
        ctx.bb_count = bb_count;
        ctx.machine_bbs = machine_bbs;
        ctx.regalloc(&ctx, arena);
    }

    CUIK_TIMED_BLOCK("emit") {
        TB_CGEmitter* e = &ctx.emit;
        FOREACH_N(i, 0, bb_count) {
            int bbid = machine_bbs[i].id;
            Tile* t = machine_bbs[i].start;

            // mark label
            tb_resolve_rel32(e, &e->labels[bbid], e->count);
            while (t) {
                emit_tile(&ctx, e, t);
                t = t->next;
            }
        }
    }

    if (emit_asm) CUIK_TIMED_BLOCK("dissassembly") {
        EMITA(&ctx.emit, "%s:\n", f->super.name);

        Disasm d = { func_out->first_patch, ctx.locations, &ctx.locations[dyn_array_length(ctx.locations)] };
        FOREACH_N(i, 0, bb_count) {
            int bbid = machine_bbs[i].id;
            TB_Node* bb = bbs[bbid];

            uint32_t start = ctx.emit.labels[bbid] & ~0x80000000;
            uint32_t end   = ctx.emit.count;
            if (i + 1 < bb_count) {
                end = ctx.emit.labels[machine_bbs[i + 1].id] & ~0x80000000;
            }

            disassemble(&ctx.emit, &d, bbid, start, end);
        }
    }

    // cleanup memory
    tb_free_cfg(&cfg);
    tb_arena_restore(arena, sp);

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->stack_usage = ctx.stack_usage;
    func_out->prologue_length = ctx.prologue_length;
}

static void get_data_type_size(TB_DataType dt, size_t* out_size, size_t* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // above 64bits we really dont care that much about natural alignment
            bool is_big_int = dt.data > 64;

            // round up bits to a byte
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data - 1);

            *out_size  = ((bits+7) / 8);
            *out_align = is_big_int ? 8 : ((dt.data + 7) / 8);
            break;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            *out_size = s;
            *out_align = s;
            break;
        }
        case TB_PTR: {
            *out_size = 8;
            *out_align = 8;
            break;
        }
        default: tb_unreachable();
    }
}
