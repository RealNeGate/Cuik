
static int bbb;

// "Register Spilling and Live-Range Splitting for SSA-Form Programs" Sebastian Hack, Matthias Braun (2009)
typedef struct {
    size_t old_node_count;
    size_t num_spills;
    ArenaArray(TB_Node*) all_phis;
    ArenaArray(TB_Node*) all_defs;

    DynArray(RAInsert) inserts;

    uint64_t single_def;
    uint64_t remat_all;

    TB_Node** phis;
    TB_Node** defs;

    TB_Node** leaders;

    uint64_t* W_entry;
    uint64_t* W_exit;

    uint64_t* live_in;
    uint64_t* live_out;

    RegMask** spill_mask;
    RegMask** reload_mask;

    int* spill_vreg_id;
    VRegStage* stage;

    NL_Table spill_map;
    NL_Table uses_spill;
} RegSplitter;

typedef struct {
    int head, tail;
} SplitterLoop;

static TB_Node* clone_node(TB_Function* f, TB_Node* n, size_t extra) {
    TB_Node* clone = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
    memcpy(clone->extra, n->extra, extra);
    FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
        clone->inputs[j] = n->inputs[j];
        add_user(f, clone, n->inputs[j], j);
    }
    return clone;
}

static int spill_map_get2(NL_Table* spill_map, TB_Node* n) {
    intptr_t p = (intptr_t) nl_table_get(spill_map, n);
    return p - 1;
}

static bool should_skip_over(TB_Node* n) {
    return IS_PROJ(n) || n->type == TB_PHI || n->type == TB_MACH_FRAME_PTR;
}

static bool is_in_stack(Ctx* ctx, TB_Node* n, bool W) {
    if (n->type == TB_PHI) {
        return !W;
    }

    RegMask* def_mask = ctx->constraint(ctx, n, NULL);
    return reg_mask_is_stack(def_mask);
}

static TB_Node* find_existing_copy(Ctx* ctx, RABase* ra, TB_BasicBlock* bb, TB_Node* src, RegMask* rm) {
    /* int i = aarray_length(bb->items) - 1;
    if (bb->items[i] == bb->end) {
    i--;
    }

    while (i > 0 && bb->items[i]->type == TB_MACH_COPY) {
    RegMask* cpy_rm = TB_NODE_GET_EXTRA_T(bb->items[i], TB_NodeMachCopy)->def;
    if (tb__reg_mask_meet(ctx, cpy_rm, rm) == rm && bb->items[i]->inputs[1] == src) {
    return bb->items[i];
    }
    i--;
    }
    return NULL; */
    return NULL;
}

static TB_Node* find_existing_spill(Ctx* ctx, RABase* ra, RegSplitter* splitter, TB_BasicBlock* bb, TB_Node* old_val, int spill) {
    int i = aarray_length(bb->items) - 1;
    if (bb->items[i] == bb->end) {
        i--;
    }

    int vreg_id = splitter->spill_vreg_id[spill];
    while (i > 0 && bb->items[i]->type == TB_MACH_COPY) {
        if (bb->items[i] == old_val) {
            return NULL;
        }

        if (ctx->vreg_map[bb->items[i]->gvn] == vreg_id) {
            return bb->items[i];
        }
        i--;
    }
    return NULL;
}

static TB_Node* ra_split_def(Ctx* ctx, RABase* ra, RegSplitter* splitter, TB_BasicBlock* bb, int pos, TB_Node* n, int spill) {
    size_t bb_id = bb - ctx->cfg.blocks;
    if (can_remat(ctx, n)) {
        // The definition isn't removed, it's marked as "spilled" so it'll be
        // cloned whenever it's actually used next.
        IF_OPT(REGSPLIT) {
            printf("  BB%zu: SPILL%d: remat-def! ", bb_id, spill);
            ctx->print_pretty(ctx, n);
            printf("\n");
        }
        return n;
    } else {
        RegMask* spill_mask  = splitter->spill_mask[spill];
        RegMask* reload_mask = splitter->reload_mask[spill];
        TB_ASSERT_MSG(spill_mask, "cannot insert spill move for this kind of node");

        TB_Function* f = ctx->f;
        TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, n, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = spill_mask, .use = reload_mask);

        aarray_push(splitter->all_defs, cpy);
        nl_table_put(&splitter->spill_map, cpy, (void*) ((uintptr_t) spill + 1));

        IF_OPT(REGSPLIT) {
            printf("  BB%zu: SPILL%d: spill! ", bb_id, spill);
            ctx->print_pretty(ctx, cpy);
            printf("\n");
        }

        RAInsert ins = { pos, cpy };
        dyn_array_put(splitter->inserts, ins);
        return cpy;
    }
}

static TB_Node* ra_split_use(Ctx* ctx, RABase* ra, RegSplitter* splitter, TB_BasicBlock* bb, int pos, TB_Node* n, int spill, TB_Node** defs) {
    TB_Function* f = ctx->f;
    size_t bb_id = bb - ctx->cfg.blocks;

    if (n == NULL) {
        TB_ASSERT((splitter->remat_all >> spill) & 1);
        n = splitter->leaders[spill];
    }

    TB_Node* cpy;
    if (!is_spill_store(n) && can_remat(ctx, n)) {
        // insert copies to avoid stretching a lifetime over itself
        FOR_N(k, 1, n->input_count) {
            TB_Node* in = n->inputs[k];
            if (in == NULL) { continue; }
            // If we're a known single-def, just don't bother cloning
            if (in->gvn < splitter->old_node_count) {
                size_t cnt;
                coalesce_set_array(ra, &in, &cnt);
                if (cnt <= 1) {
                    continue;
                }
            }
            // Insert copy to avoid stretching the value over itself
            RegMask* rm = ctx->constraint(ctx, in, NULL);
            TB_Node* reuse = find_existing_copy(ctx, ra, f->scheduled[in->gvn], in, rm);
            if (reuse != NULL) {
                set_input(f, n, reuse, k);
            } else {
                // Copy "in" value
                TB_Node* in_cpy = tb_alloc_node(f, TB_MACH_COPY, in->dt, 2, sizeof(TB_NodeMachCopy));
                set_input(f, in_cpy, in, 1);
                TB_NODE_SET_EXTRA(in_cpy, TB_NodeMachCopy, .def = rm, .use = rm);
                aarray_push(splitter->all_defs, in_cpy);

                IF_OPT(REGSPLIT) {
                    printf("  BB%zu: SPILL%d: remat-stretch! ", bb_id, spill);
                    ctx->print_pretty(ctx, in_cpy);
                    printf("\n");
                }

                if (in->gvn >= f->scheduled_n || (f->scheduled[in->gvn] == NULL || f->scheduled[in->gvn] == bb)) {
                    RAInsert ins = { pos, in_cpy };
                    dyn_array_put(splitter->inserts, ins);
                } else {
                    // Place within the in's block
                    insert_op_at_end(ctx, ra, f->scheduled[in->gvn], in_cpy);
                }

                // TODO(NeGate): we wanna avoid making too many copies so maybe collapse some?
                set_input(f, n, in_cpy, k);
            }
        }

        size_t extra = extra_bytes(n);
        cpy = clone_node(f, n, extra);

        IF_OPT(REGSPLIT) {
            printf("  BB%zu: SPILL%d: remat-use! ", bb_id, spill);
            ctx->print_pretty(ctx, cpy);
            printf("\n");
        }

        // use the latest variants
        FOR_N(k, 1, cpy->input_count) {
            TB_Node* in = cpy->inputs[k];
            if (in == NULL) { continue; }

            int spill = spill_map_get2(&splitter->spill_map, in);
            if (spill >= 0) {
                int num_spills = splitter->num_spills;
                TB_Node* fresh_def = defs[bb_id*num_spills + spill];
                set_input(f, cpy, fresh_def, k);
            }
        }
    } else {
        cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, n, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = splitter->reload_mask[spill], .use = splitter->spill_mask[spill]);

        IF_OPT(REGSPLIT) {
            printf("  BB%zu: SPILL%d: reload! ", bb_id, spill);
            ctx->print_pretty(ctx, cpy);
            printf("\n");
        }
    }

    aarray_push(splitter->all_defs, cpy);
    nl_table_put(&splitter->spill_map, cpy, (void*) ((uintptr_t) spill + 1));

    RAInsert ins = { pos, cpy };
    dyn_array_put(splitter->inserts, ins);
    return cpy;
}

// Called to fill in the edges for a specific branch into the region.
// These aren't necessarily in the right "mode" (SPILL/REG) that is
// something we sort out at the end.
static void ra_split_phi_edge(Ctx* ctx, RABase* ra, RegSplitter* splitter, TB_BasicBlock* bb, int pred_id, int pred_path, bool complete, bool backedge, uint64_t loop_touched) {
    TB_Function* f = ctx->f;
    size_t bb_id = bb - ctx->cfg.blocks;
    int num_spills = splitter->num_spills;

    TB_Node* header = bb->start;
    TB_Node** bb_defs = &splitter->defs[bb_id*num_spills];
    TB_Node** pred_defs = &splitter->defs[pred_id*num_spills];

    if (!NODE_ISA(header, REGION) || header->input_count == 1) {
        // carry defs through, no phis needed
        FOR_N(k, 0, num_spills) {
            TB_Node* pred_def = pred_defs[k];
            if ((splitter->live_out[pred_id] >> k) & 1) {
                bb_defs[k] = pred_def;
            } else {
                bb_defs[k] = NULL;
            }
        }
        return;
    }

    int pred_count = header->input_count;
    TB_Node** bb_phis = &splitter->phis[bb_id*num_spills];
    FOR_N(k, 0, num_spills) {
        TB_Node* pred_def = pred_defs[k];
        if (!((splitter->live_out[pred_id] >> k) & 1)) {
            continue;
        }

        TB_Node* phi = bb_phis[k];
        bool needs_phi;
        if (complete) {
            // if we're complete, then we might be able to get away without a phi
            // if the paths agree.
            needs_phi = (bb_defs[k] && bb_defs[k] != pred_def);
        } else {
            // if we're incomplete, we can go without a phi if we know that there's
            // no activity on the def during the loop that this phi is a part of.
            needs_phi = (loop_touched >> k) & 1;

            // this is a backedge path
            if (pred_def == NULL) {
                continue;
            }
        }

        // I don't think this is possible? im not sure rn
        if (!needs_phi) {
            needs_phi = (pred_def->type == TB_PHI && pred_def->inputs[0] == header);
        }

        if (phi != NULL) {
            TB_ASSERT(phi->type == TB_PHI && phi->inputs[0] == header);
            set_input(f, phi, pred_def, 1+pred_path);
        } else if (needs_phi) {
            // You can't create phis in the backedge path, that's too late, if they
            // weren't created in the first place that probably means you marked
            // something as untouched that was?
            TB_ASSERT(!backedge);

            // we just need at least one defined pred to choose the datatype
            TB_Node* literally_any = splitter->leaders[k];
            TB_ASSERT(literally_any);

            phi = tb_alloc_node(f, TB_PHI, literally_any->dt, 1 + pred_count, 0);

            RAInsert ins = { 1, phi };
            dyn_array_put(splitter->inserts, ins);
            aarray_push(splitter->all_phis,  phi);
            nl_table_put(&splitter->spill_map, phi, (void*) ((uintptr_t) k + 1));

            // move up if necessary
            FOR_N(class, 1, ctx->num_classes) {
                ra->hrp[class][bb_id].lo2hi += 1 <= ra->hrp[class][bb_id].lo2hi;
            }

            set_input(f, phi, header, 0);
            // all the edges up until this point were the same
            TB_Node* same = bb_defs[k];
            FOR_N(i, 0, pred_path) {
                set_input(f, phi, same, 1+i);
            }
            set_input(f, phi, pred_def, 1+pred_path);

            bb_defs[k] = phi;
            bb_phis[k] = phi;

            IF_OPT(REGSPLIT) {
                printf("  BB%zu: SPILL%zu: phi! ", bb_id, k);
                ctx->print_pretty(ctx, phi);
                printf(" (%s)\n", ((loop_touched >> k) & 1) ? "TOUCHED" : "UNTOUCHED");
            }
        } else {
            // Doesn't need a phi, doesn't have a phi
            TB_ASSERT(bb_defs[k] == NULL || bb_defs[k] == pred_def);
            bb_defs[k] = pred_def;
        }
    }
}

static void add_splitter_def(RegSplitter* splitter, TB_Node* n) {
    aarray_for(i, splitter->all_defs) {
        if (splitter->all_defs[i] == n) { return; }
    }
    aarray_push(splitter->all_defs, n);
}

static bool has_immediate_use(TB_Node* n, TB_Node* of) {
    FOR_N(i, 1, n->input_count) {
        if (n->inputs[i] == of) {
            return true;
        }
    }
    return false;
}

static void dump_split_state(TB_Node** defs, int bb_id, int num_spills, uint64_t W, uint64_t S) {
    printf("  BB%-3d: W=", bb_id);
    FOR_REV_N(j, 0, num_spills) {
        putchar((W >> j) & 1 ? '1' : '0');
    }
    /* printf(" S=");
    FOR_REV_N(j, 0, num_spills) {
    putchar((S >> j) & 1 ? '1' : '0');
    } */
    printf(" [ ");
    FOR_N(j, 0, num_spills) {
        if (defs[j]) {
            printf("%%%u ", defs[j]->gvn);
        } else {
            printf("___ ");
        }
    }
    printf("]\n");
}

// Index where we next start or end an HRP region
static int nearest_hrp_event(Ctx* ctx, RABase* ra, int bb_id, int j) {
    int nearest = INT_MAX;
    FOR_N(class, 1, ctx->num_classes) {
        int lo2hi = ra->hrp[class][bb_id].lo2hi;
        if (lo2hi >= j && lo2hi < nearest) {
            nearest = lo2hi;
        }
    }

    return nearest;
}

static void* arena_zalloc(TB_Arena* arena, size_t size) {
    void* ptr = tb_arena_alloc(arena, size);
    memset(ptr, 0, size);
    return ptr;
}

static const char* STAGE_NAMES[] = {
    "UNDEF", "ASSIGN", "EVICT", "SPLIT", "SPILL"
};

void tb__insert_splits(Ctx* ctx, RABase* ra, SplitDecision* splits, size_t num_spills) {
    TB_Arena* arena = ra->arena;
    TB_Function* f = ctx->f;

    // bbb++;
    // tb_opt__REGSPLIT = bbb == 6 || bbb == 7;

    TB_OPTDEBUG(REGSPLIT)(printf("== INSERT NODES ==\n"));

    size_t old_node_count = f->node_count;
    cuikperf_region_start("alloc", NULL);

    // we can only spill 64 vregs at once due to some bitsets i don't feel like changing
    TB_ASSERT(num_spills <= 64);

    RegSplitter splitter = { 0 };
    splitter.old_node_count = old_node_count;
    splitter.num_spills = num_spills;
    splitter.spill_map = nl_table_alloc(num_spills);
    splitter.uses_spill = nl_table_alloc(num_spills);
    splitter.spill_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    splitter.reload_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    splitter.leaders = tb_arena_alloc(ra->arena, num_spills * sizeof(TB_Node*));

    splitter.spill_vreg_id = tb_arena_alloc(ra->arena, num_spills * sizeof(int));
    splitter.stage = tb_arena_alloc(ra->arena, num_spills * sizeof(VRegStage));
    splitter.inserts = dyn_array_create(RAInsert, 32);

    // these are nodes which aggressively try to spill their entire range, generally because it's
    // not used throughout most of it until the end
    uint64_t spill_aggro = 0;
    uint64_t class2vreg[MAX_REG_CLASSES] = { 0 };
    FOR_N(i, 0, num_spills) {
        uint32_t vreg_id = splits[i].target;

        VReg* to_spill = &ctx->vregs[vreg_id];
        int class = to_spill->mask->class;
        TB_ASSERT(class > 0);
        class2vreg[class] |= 1ull << i;

        // the splitter cannot operate on stack nodes, they also
        // don't ever need "splitting"
        TB_ASSERT(class != REG_CLASS_STK);
        splitter.reload_mask[i] = ctx->normie_mask[class];
        splitter.spill_mask[i] = ctx->mayspill_mask[class];
        splitter.spill_vreg_id[i] = 0;

        assert(to_spill->stage <= VREG_STAGE_SPILL);
        splitter.stage[i] = to_spill->stage < VREG_STAGE_SPLIT ? VREG_STAGE_SPLIT : VREG_STAGE_SPILL;

        if (splitter.stage[i] == VREG_STAGE_SPILL) {
            spill_aggro |= 1ull << i;
        }

        to_spill->mask = NULL;
        to_spill->spill_cost = NAN;

        TB_ASSERT(to_spill->n);
        splitter.leaders[i] = to_spill->n;

        size_t cnt;
        TB_Node** arr = coalesce_set_array(ra, &splitter.leaders[i], &cnt);
        FOR_N(j, 0, cnt) {
            nl_table_put(&splitter.spill_map, arr[j], (void*) (i + 1));

            FOR_USERS(u, arr[j]) {
                nl_table_put(&splitter.uses_spill, USERN(u), (void*) 1);
            }
        }
    }

    // [def_i*num_spills + spill_i]
    TB_Node** defs = splitter.defs = arena_zalloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    TB_Node** phis = splitter.phis = arena_zalloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    FOR_N(i, 0, num_spills * ctx->bb_count) {
        defs[i] = NULL;
        phis[i] = NULL;
    }
    splitter.single_def = 0;

    uint64_t* def_map = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* use_map = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    // W means it's in a register right now
    splitter.W_entry  = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.W_exit   = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.live_in  = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.live_out = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    cuikperf_region_end();

    cuikperf_region_start("init", NULL);
    ////////////////////////////////
    // 1. Classify spilled regions
    ////////////////////////////////
    FOR_N(spill_i, 0, num_spills) {
        size_t cnt;
        TB_Node** arr = coalesce_set_array(ra, &splitter.leaders[spill_i], &cnt);
        FOR_N(j, 0, cnt) {
            TB_Node* n = arr[j];
            int bb_id = f->scheduled[n->gvn] - ctx->cfg.blocks;

            def_map[bb_id] |= 1ull << spill_i;
            FOR_USERS(u, n) {
                TB_BasicBlock* use_bb = f->scheduled[USERN(u)->gvn];
                use_map[use_bb - ctx->cfg.blocks] |= 1ull << spill_i;
            }
        }

        // append the liveness info
        FOR_N(k, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[k];
            bool live_in = false, live_out = false;

            FOR_N(j, 0, cnt) {
                TB_Node* n = arr[j];
                live_in  |= set_get(&bb->live_in, n->gvn);
                live_out |= set_get(&bb->live_out, n->gvn);
            }

            splitter.live_in[k]  |= ((uint64_t)live_in) << spill_i;
            splitter.live_out[k] |= ((uint64_t)live_out) << spill_i;
        }

        if (cnt == 1) {
            splitter.single_def |= 1ull << spill_i;
            if (can_remat(ctx, arr[0]) && splitter.spill_mask[spill_i] == NULL) {
                splitter.remat_all |= 1ull << spill_i;
            }

            if (arr[0]->user_count == 1) {
                spill_aggro |= 1ull << spill_i;
            }
        }
    }

    IF_OPT(REGSPLIT) {
        printf("== TO BE SPILLED (%zu) ==\n", num_spills);
        FOR_N(i, 0, num_spills) {
            uint32_t vreg_id = splits[i].target;
            double cost = tb__ra_get_spill_cost(ra, &ctx->vregs[vreg_id]);

            printf("  V%-5u %f %"PRIu64, vreg_id, cost, ctx->vregs[vreg_id].area);
            printf(" (%s STAGE)", STAGE_NAMES[splitter.stage[i]]);
            if ((spill_aggro >> i) & 1) {
                printf(" (SPILL AGGRO)");
            }
            if ((splitter.remat_all >> i) & 1) {
                printf(" (REMAT HARD)");
            }
            printf("\n");

            size_t cnt;
            TB_Node** arr = coalesce_set_array(ra, &splitter.leaders[i], &cnt);
            FOR_N(j, 0, cnt) {
                TB_Node* n = arr[j];

                printf("  * ");
                ctx->print_pretty(ctx, n);
                printf(" (%d uses, SPILL%zu)\n", n->user_count, i);
                TB_ASSERT(ctx->vreg_map[n->gvn] == vreg_id);

                FOR_USERS(u, n) {
                    printf("||  ");
                    ctx->print_pretty(ctx, USERN(u));
                    printf("\n");
                }
            }
        }
    }

    IF_OPT(REGSPLIT) {
        printf("== DEF-USE INFO ==\n");
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            printf("  BB%-3zu: D=", i);
            FOR_REV_N(j, 0, num_spills) {
                putchar((def_map[i] >> j) & 1 ? '1' : '0');
            }
            printf(" U=");
            FOR_REV_N(j, 0, num_spills) {
                putchar((use_map[i] >> j) & 1 ? '1' : '0');
            }
            printf(" ");
            FOR_N(class, 1, ctx->num_classes) {
                int lo2hi = ra->hrp[class][i].lo2hi;
                if (lo2hi >= 0) {
                    printf("[%s, %d (%%%u)] ", reg_class_name(class), lo2hi, bb->items[lo2hi]->gvn);
                }
            }
            FOR_SUCC(it, bb->end) {
                TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, it.succ);
                printf("  BB%-3zu", succ_bb - ctx->cfg.blocks);
            }
            printf("\n");
        }
    }
    cuikperf_region_end();

    ////////////////////////////////
    // 2. Insert spills and reloads
    ////////////////////////////////
    TB_OPTDEBUG(REGSPLIT)(printf("== INSERT NODES ==\n"));
    // rogers_dump_block(ctx, old_node_count, 18);

    splitter.all_phis = aarray_create(arena, TB_Node*, 30);
    splitter.all_defs = aarray_create(arena, TB_Node*, 30);

    int freq[64];
    ArenaArray(SplitterLoop) loops = aarray_create(arena, SplitterLoop, 10);
    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

        TB_Node* header = bb->start;
        int pred_count = header->type == TB_PROJ && header->inputs[0]->type == TB_ROOT ? 0 : header->input_count;

        TB_Node** bb_defs = &defs[bb_id*num_spills];
        uint64_t W = 0;

        ////////////////////////////////
        // Compute W_entry
        ////////////////////////////////
        int loop_dom = -1;
        int loop_tail = -1;
        int fwd_pred_count = 0;

        uint64_t loop_touched = 0;
        if (pred_count) {
            FOR_N(j, 0, num_spills) {
                freq[j] = 0;
            }

            FOR_N(j, 0, pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                int pred_id = pred_bb - ctx->cfg.blocks;
                if (pred_id >= bb_id) {
                    // pick the furthest backedge
                    loop_tail = TB_MAX(loop_tail, pred_id);
                } else {
                    FOR_N(k, 0, num_spills) {
                        freq[k] += (splitter.W_exit[pred_id] >> k) & 1;
                    }
                    fwd_pred_count++;
                }
            }

            // if all forward preds go into this block in a register
            // we'll keep it that way
            uint64_t mismatch = 0;
            FOR_N(k, 0, num_spills) {
                if (freq[k] == fwd_pred_count) {
                    W |= 1ull << k;
                } else if (freq[k] != 0) {
                    mismatch |= 1ull << k;
                }
            }

            // Split around loops
            if (loop_tail >= 0) {
                FOR_N(i, bb_id, loop_tail + 1) {
                    loop_touched |= def_map[i];
                    loop_touched |= use_map[i];
                }

                IF_OPT(REGSPLIT) {
                    printf("  LOOP[%-3zu, %-3d): ", bb_id, loop_tail);
                    FOR_REV_N(j, 0, num_spills) {
                        putchar((loop_touched >> j) & 1 ? '1' : '0');
                    }
                    printf(" | W=");
                    FOR_REV_N(j, 0, num_spills) {
                        putchar((W >> j) & 1 ? '1' : '0');
                    }
                    printf("\n");
                }

                // Only touched entries will be allowed to stay in regs
                //
                // Because values can only be reloaded due to uses and we've
                // guarenteed that there's no uses in this space we can communicate
                // that to the phi creation to tell it not to make them.
                W &= loop_touched;
            }

            if (1) { // loop_tail >= 0) {
                // if the Join block is HRP and the value isn't used in
                // the first block, we'll spill
                uint64_t is_hrp = 0;
                FOR_N(class, 1, ctx->num_classes) {
                    int lo2hi = ra->hrp[class][bb_id].lo2hi;
                    if (lo2hi == 0) {
                        is_hrp |= class2vreg[class];
                        W &= ~is_hrp;
                    } else {
                        // if the preds mismatch but we're not HRP on entry
                        // we'll use regs.
                        W |= class2vreg[class] & mismatch;
                    }
                }

                for (size_t j = 0; j < aarray_length(bb->items); j++) {
                    TB_Node* n = bb->items[j];
                    if (n->type == TB_PHI) {
                        continue;
                    }

                    // any uses of the "about" to be spilled values?
                    FOR_N(k, 1, n->input_count) {
                        TB_Node* in = n->inputs[k];
                        int spill = spill_map_get2(&splitter.spill_map, in);
                        if (spill >= 0 && !((splitter.remat_all >> spill) & 1)) {
                            W |= (1ull << spill) & is_hrp;
                            TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: immediate use of %%%u, don't spill it\n", bb_id, spill, in->gvn));
                        }
                    }

                    // def was assigned
                    if (n->gvn >= aarray_length(ctx->vreg_map) || ctx->vreg_map[n->gvn]) {
                        break;
                    }
                }
            }
        }

        // process phi defs
        if (NODE_ISA(header, REGION)) {
            FOR_USERS(u, header) {
                if (USERN(u)->type != TB_PHI) {
                    continue;
                }

                TB_Node* phi = USERN(u);
                int spill = spill_map_get2(&splitter.spill_map, phi);
                if (spill >= 0) {
                    bb_defs[spill] = phi;
                    phis[bb_id*num_spills + spill] = phi;

                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: def %%%u\n", bb_id, spill, phi->gvn));
                    aarray_push(splitter.all_phis, phi);
                }
            }
        }

        splitter.W_entry[bb_id] = W;
        // If the def is in a register it may need to interact
        // mostly by spilling when hitting an HRP. So we only
        // consider them "untouched" if they're actually on the
        // stack atm.
        loop_touched |= W;

        bool complete = fwd_pred_count == pred_count;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            ra_split_phi_edge(ctx, ra, &splitter, bb, pred_id, j, complete, false, loop_touched);
        }

        // queue up a loop completion task once we've walked to the furthest loop tail
        if (loop_tail >= 0) {
            aarray_push(loops, (SplitterLoop){ bb_id, loop_tail });
        }

        // How long until we need to check for an HRP transition
        int nearest_hrp_t = nearest_hrp_event(ctx, ra, bb_id, 0);

        // Marked for spilling since it crossed HRP or we've defined
        // it and it's aggressively spilling.
        uint64_t needs_spill = 0;

        TB_OPTDEBUG(REGSPLIT)(dump_split_state(bb_defs, bb_id, num_spills, W, 0));

        cuikperf_region_start("BB", NULL);
        size_t local_old_node_count = ctx->f->node_count;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];
            if (NODE_ISA(n, REGION) || n->type == TB_PHI) {
                continue;
            }
            // nodes shouldn't be inserted while we're iterating
            TB_ASSERT(n->gvn < local_old_node_count);

            // check for HRP crossings
            if (j >= nearest_hrp_t) {
                nearest_hrp_t = nearest_hrp_event(ctx, ra, bb_id, j + 1);

                // track HRP region
                FOR_N(class, 1, ctx->num_classes) {
                    // never even got high pressure
                    int lo2hi = ra->hrp[class][bb_id].lo2hi;
                    if (lo2hi != j) {
                        continue;
                    }

                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: %%%u: entered HRP region for %s\n", bb_id, n->gvn, reg_class_name(class)));
                    needs_spill |= class2vreg[class];
                }
            }

            // insert spill-store
            BITS64_FOR_AND(spill, &W, &needs_spill, num_spills) {
                // if it's gonna be used immediately then we're bound to
                // reload so there's no colorability win
                if (has_immediate_use(bb->items[j], n)) {
                    continue;
                }

                bb_defs[spill] = ra_split_def(ctx, ra, &splitter, bb, j, bb_defs[spill], spill);
                needs_spill &= ~(1u << spill);
                W &= ~(1ull << spill);
            }

            // remap inputs, insert reloads
            FOR_N(k, 0, n->input_count) {
                TB_Node* in = n->inputs[k];
                if (in == NULL) { continue; }
                int spill = spill_map_get2(&splitter.spill_map, in);
                if (spill < 0) { continue; }

                TB_Node* def = bb_defs[spill];
                if (((W >> spill) & 1) == 0) {
                    RegMask* in_mask = constraint_in(ctx, n, k);

                    // if we could do a folded reload but the destination can't, we
                    // fail to do perform it (since we couldn't coalesce).
                    bool can_fold = in_mask->may_spill;
                    if (can_fold) {
                        if (can_remat(ctx, def)) {
                            // this would just neither spill or reload the original range and thus
                            // not shrink it, so we force a "reload" here.
                            can_fold = false;
                        } else if (ctx->node_2addr(n) == k) {
                            RegMask* rm = ctx->constraint(ctx, n, NULL);
                            if (!rm->may_spill) {
                                can_fold = false;
                            }
                        }
                    }

                    if (n->type == TB_MACH_COPY && !is_spill_store(n)) {
                        // we can convert this copy into a reload of def
                        TB_NodeMachCopy* cpy_extra = TB_NODE_GET_EXTRA(n);
                        cpy_extra->use = splitter.reload_mask[spill];
                    } else if (can_fold) {
                        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: folded-reload at %%%u\n", bb_id, spill, n->gvn));
                    } else {
                        // create reload or remat
                        def = ra_split_use(ctx, ra, &splitter, bb, j, def, spill, defs);

                        // if we haven't spilled before, we can reuse the reloaded value
                        // even if we're in an HRP.
                        if (((spill_aggro >> spill) & 1) == 0) {
                            bb_defs[spill] = def;
                            W |= 1ull << spill;
                        }
                    }
                }

                if (in != def) {
                    set_input(f, n, def, k);

                    // if there's other edges on the node that refer to the old node, we
                    // should update them now. we only need to check for a direct node match since indirect
                    // matches to other nodes in the same vreg couldn't be possible (they'd be interfering).
                    FOR_N(l, k+1, n->input_count) {
                        if (n->inputs[l] == in) {
                            set_input(f, n, def, l);
                        }
                    }
                }
            }

            // find def
            int spill = spill_map_get2(&splitter.spill_map, n);
            if (spill >= 0) {
                TB_ASSERT(f->scheduled[n->gvn] == bb);
                TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: def %%%u\n", bb_id, spill, n->gvn));

                bb_defs[spill] = n;
                W |= 1ull << spill;
                aarray_push(splitter.all_defs, n);

                if ((spill_aggro >> spill) & 1) {
                    needs_spill |= 1ull << spill;
                }
            }
        }
        cuikperf_region_end();

        // insert last minute spill-stores
        size_t t = aarray_length(bb->items);
        BITS64_FOR_AND(spill, &W, &needs_spill, num_spills) {
            bb_defs[spill] = ra_split_def(ctx, ra, &splitter, bb, t, bb_defs[spill], spill);
            needs_spill &= (1u << spill);
            W &= ~(1ull << spill);
        }

        // apply edits
        tb__ra_bulk_insert(ctx, bb, splitter.inserts);
        dyn_array_clear(splitter.inserts);

        TB_OPTDEBUG(REGSPLIT)(dump_split_state(bb_defs, bb_id, num_spills, W, 0));
        splitter.W_exit[bb_id] = W;

        // complete loop phis
        size_t j = aarray_length(loops);
        while (j--) {
            if (loops[j].tail != bb_id) {
                continue;
            }

            uint32_t head = loops[j].head;
            aarray_remove(loops, j);

            // this is always a region so the pred count is simpler
            TB_Node* header = ctx->cfg.blocks[head].start;
            size_t pred_count = header->input_count;

            uint64_t W_head = splitter.W_entry[head];
            FOR_N(j, 0, pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                int pred_id = pred_bb - ctx->cfg.blocks;
                if (pred_id < head) {
                    continue;
                }

                ra_split_phi_edge(ctx, ra, &splitter, &ctx->cfg.blocks[head], pred_id, j, true, true, 0);
            }

            FOR_N(k, 0, num_spills) {
                TB_Node* phi = phis[head*num_spills + k];
                if (phi == NULL) {
                    continue;
                }

                TB_ASSERT(phi->type == TB_PHI && phi->input_count == 1+pred_count);

                // remove the unnecessary phis
                TB_Node* leader = NULL;
                FOR_N(i, 1, phi->input_count) {
                    TB_Node* in = phi->inputs[i];
                    if (in == phi) { continue; }
                    else if (leader == NULL) { leader = in; }
                    else if (leader != in) { leader = phi; break; }
                }

                TB_ASSERT(leader);
                if (leader != phi) {
                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%td: phi %%%u (pruned to %%%u)\n", bb_id, k, phi->gvn, leader->gvn));

                    // replace all def sites that use it, these could
                    // only exist within the loop itself so we can avoid
                    // checking unnecessary blocks.
                    FOR_N(i, head, bb_id+1) {
                        if (defs[i*num_spills + k] == phi) {
                            defs[i*num_spills + k] = leader;
                        }
                    }

                    subsume_node(f, phi, leader);
                    tb__remove_node(ctx, f, phi);

                    phis[head*num_spills + k] = NULL;
                } else {
                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%td: phi %%%u (complete)\n", k, bb_id, phi->gvn));
                }
            }
        }

        TB_OPTDEBUG(REGSPLIT)(printf("\n"));
    }
    // TB_OPTDEBUG(REGSPLIT)(rogers_dump_sched(ctx, old_node_count));
    // printf("A %d %d %d\n", ttt, uuu, vvv);

    ////////////////////////////////
    // 3. Re-coalesce nodes
    ////////////////////////////////
    // insert copies on any phis which have pre-split and post-split
    // paths coming in (inserting copies to make these phis into post-split)
    size_t j = 0;
    aarray_for(i, splitter.all_phis) {
        TB_Node* n = splitter.all_phis[i];
        if (n->type == TB_NULL) {
            // it was pruned, remove it from the sets
            continue;
        }

        int spill = spill_map_get2(&splitter.spill_map, n);
        TB_BasicBlock* bb = f->scheduled[n->gvn];
        size_t bb_id = bb - ctx->cfg.blocks;
        bool phi_in_reg = (splitter.W_entry[bb_id] >> spill) & 1;
        TB_Node* header = n->inputs[0];
        FOR_N(j, 1, n->input_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j - 1);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            size_t pred_id = pred_bb - ctx->cfg.blocks;
            TB_Node* pred_def = n->inputs[j];

            bool pred_in_reg = (splitter.W_exit[pred_id] >> spill) & 1;
            if (phi_in_reg == pred_in_reg) {
                continue;
            }

            RegMask* spill_mask  = splitter.spill_mask[spill];
            RegMask* reload_mask = splitter.reload_mask[spill];

            TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, pred_def->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, cpy, pred_def, 1);
            if (phi_in_reg) {
                TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = reload_mask, .use = spill_mask);
            } else {
                TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = spill_mask, .use = reload_mask);
            }

            insert_op_at_end(ctx, ra, pred_bb, cpy);
            aarray_push(splitter.all_defs, cpy);

            set_input(f, n, cpy, j);
        }

        // it might be time to include these guys
        aarray_push(splitter.all_defs, n);
        splitter.all_phis[j++] = splitter.all_phis[i];
    }
    aarray_set_length(splitter.all_phis, j);

    // split all the defs and then re-coalesce them
    if (f->node_count >= ra->uf_len) {
        size_t new_len = tb_next_pow2(f->node_count + 16);
        tb__ra_resize_uf(ra, new_len);
    }

    // make sure there's enough room for the vreg_map
    ctx->vreg_map = aarray__reserve2(ctx->vreg_map, sizeof(*ctx->vreg_map), f->node_count);

    TB_OPTDEBUG(REGSPLIT)(printf("== RE-COALESCE ==\n"));

    j = 0;
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];

        // kill vreg
        TB_ASSERT(n->gvn < aarray_length(ctx->vreg_map));
        if (ctx->vreg_map[n->gvn] != 0) {
            ctx->vregs[ctx->vreg_map[n->gvn]].n = NULL;
        }
        ctx->vreg_map[n->gvn] = 0;

        // reset UF
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        if (leader == n->gvn) {
            nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
        }
        ra->uf[n->gvn] = n->gvn;
        ra->uf_size[n->gvn] = 1;

        IF_OPT(REGSPLIT) {
            printf("  ");
            ctx->print_pretty(ctx, n);
            printf("%s\n", n->user_count == 0 ? " (KILL)" : "");
        }

        if (n->user_count == 0 && f->scheduled[n->gvn]) {
            // delete the original def
            tb__remove_node(ctx, f, n);
            tb_kill_node(f, n);
            continue;
        }

        // most of these nodes don't really have a bound set but whatever
        splitter.all_defs[j++] = splitter.all_defs[i];
    }
    aarray_set_length(splitter.all_defs, j);

    // Phi-coalesce
    aarray_for(i, splitter.all_phis) {
        TB_Node* n = splitter.all_phis[i];
        TB_ASSERT(n->type == TB_PHI);
        int x = uf_find(ra->uf, ra->uf_len, n->gvn);
        RegMask* rm = ctx->constraint(ctx, n, NULL);
        FOR_N(k, 1, n->input_count) {
            // interfere against everything in the set
            TB_Node* in = n->inputs[k];
            int y = uf_find(ra->uf, ra->uf_len, in->gvn);
            tb__ra_coalesce(ra, x, y, n, in);
        }
    }
    // CISC-coalesce
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int x = uf_find(ra->uf, ra->uf_len, n->gvn);
        RegMask* rm = ctx->constraint(ctx, n, NULL);
        int shared_edge = ctx->node_2addr(n);
        if (shared_edge >= 0 && n->inputs[shared_edge]) {
            int y = uf_find(ra->uf, ra->uf_len, n->inputs[shared_edge]->gvn);
            tb__ra_coalesce(ra, y, x, n->inputs[shared_edge], n);
        }
    }

    // create or reset assignments
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        if (n->gvn != leader) {
            continue;
        }

        int vreg_id = ctx->vreg_map[leader];
        TB_ASSERT(vreg_id == 0);

        VReg* new_vreg = tb__set_node_vreg(ctx, n);
        ctx->vreg_map[leader] = new_vreg - ctx->vregs;

        int spill = spill_map_get2(&splitter.spill_map, n);
        if (spill >= 0) {
            new_vreg->stage = splitter.stage[spill];
            if (new_vreg->stage == VREG_STAGE_SPILL) {
                new_vreg->spill_cost = INFINITY;
            }
        }
    }

    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        RegMask* mask = ctx->constraint(ctx, n, NULL);

        int vreg_id = ctx->vreg_map[leader];
        if (n->gvn == leader) {
            VReg* vreg = &ctx->vregs[vreg_id];
            vreg->reg_width  = tb__reg_width_from_dt(mask->class, n->dt);
            vreg->spill_bias = mask->may_spill ? -100.0f : 0.0f;
        }
        mask = tb__reg_mask_meet(ctx, mask, ctx->vregs[vreg_id].mask);

        FOR_USERS(u, n) {
            if (USERI(u) > 0 && USERI(u) < USERN(u)->input_count) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                mask = tb__reg_mask_meet(ctx, mask, in_mask);
            }
        }

        // TB_ASSERT(mask != &TB_REG_EMPTY);
        ctx->vregs[vreg_id].mask = mask;
        ctx->vreg_map[n->gvn] = vreg_id;
    }

    // hint every copy across vregs
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        if (n->gvn == leader) {
            int spill = spill_map_get2(&splitter.spill_map, n);
            if (spill >= 0 && splitter.stage[spill] == VREG_STAGE_SPILL) {
                int vreg_id = ctx->vreg_map[leader];
                VReg* vreg  = &ctx->vregs[vreg_id];
                if (vreg->mask->may_spill) {
                    vreg->spill_cost = INFINITY;
                    vreg->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                    vreg->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, n->dt);
                }
            }
        }

        // hint the source to be like the destination (we're allocating
        // in reverse).
        if (n->type == TB_MACH_COPY) {
            int x = ctx->vreg_map[n->gvn];
            int y = ctx->vreg_map[n->inputs[1]->gvn];
            if (x != y) {
                ctx->vregs[y].hint_vreg = x;
            }
        }
    }

    IF_OPT(REGSPLIT) {
        printf("== NEW GROUPS ==\n");

        aarray_for(i, splitter.all_defs) {
            TB_Node* n = splitter.all_defs[i];

            #ifndef NDEBUG
            // PHIs better coalesce with their direct inputs
            if (n->type == TB_PHI) {
                FOR_N(j, 1, n->input_count) {
                    if (ctx->vreg_map[n->gvn] != ctx->vreg_map[n->inputs[j]->gvn]) {
                        TB_OPTDEBUG(REGSPLIT)(rogers_dump_sched(ctx, old_node_count));
                        TB_ASSERT(ctx->vreg_map[n->gvn] == ctx->vreg_map[n->inputs[j]->gvn]);
                    }
                }
            }
            #endif

            if (ra->uf[n->gvn] == n->gvn) {
                int vreg_id = ctx->vreg_map[n->gvn];
                VReg* vreg = &ctx->vregs[vreg_id];

                printf("# V%-4"PRIdPTR" bias=%.2f ", vreg - ctx->vregs, vreg->spill_bias);
                tb__print_regmask(&OUT_STREAM_DEFAULT, vreg->mask);
                printf("\n  ");
                ctx->print_pretty(ctx, n);
                printf(" # UF size = %d, mask = ", ra->uf_size[n->gvn]);
                tb__print_regmask(&OUT_STREAM_DEFAULT, ctx->vregs[vreg_id].mask);
                printf("\n");

                size_t cnt;
                TB_Node** arr = coalesce_set_array(ra, &n, &cnt);
                FOR_N(j, 0, cnt) {
                    TB_Node* k = arr[j];
                    if (k != n) {
                        printf("    * ");
                        ctx->print_pretty(ctx, k);
                        printf("\n");
                    }
                }
            }
        }
    }

    nl_table_free(splitter.spill_map);
    nl_table_free(splitter.uses_spill);

    // rogers_dump_block(ctx, old_node_count, 78);
    // rogers_dump_block(ctx, old_node_count, 84);

    TB_OPTDEBUG(SERVER)(dbg_submit_event_sched(&ctx->cfg, f, "Post-split"));

    #if 0
    rogers_dump_sched(ctx, old_node_count);

    // check for some low quality spills
    int bads = 0;
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        FOR_N(j, 0, aarray_length(bb->items)) {
            if (ctx->vreg_map[bb->items[j]->gvn]) {
                TB_Node* n = bb->items[j];
                if (j > 0 && n->inputs[1] == bb->items[j - 1] && n->type == TB_MACH_COPY) {
                    if (is_spill_store(n) && is_reload(n->inputs[1])) {
                        printf("BACK TO BACK COPY!!! %%%u\n", n->gvn);
                        bads++;
                    } else if (is_reload(n) && is_spill_store(n->inputs[1])) {
                        printf("BACK TO BACK COPY!!! %%%u\n", n->gvn);
                        bads++;
                    }
                }
            }
        }
    }
    printf("VALIDATED!! %d\n", bads);
    #endif

    // TB_OPTDEBUG(REGSPLIT)(rogers_dump_sched(ctx, old_node_count));
}
