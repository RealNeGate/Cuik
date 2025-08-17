// "Register Spilling and Live-Range Splitting for SSA-Form Programs" Sebastian Hack, Matthias Braun (2009)
typedef struct {
    size_t num_spills;
    ArenaArray(TB_Node*) all_phis;
    ArenaArray(TB_Node*) all_defs;

    uint64_t single_def;
    uint64_t* W_entry;
    uint64_t* W_exit;

    uint64_t* live_in;
    uint64_t* live_out;

    RegMask** spill_mask;
    RegMask** reload_mask;

    int* spill_vreg_id;
    NL_Table spill_map;
} RegSplitter;

static TB_Node* clone_node(TB_Function* f, TB_Node* n, size_t extra) {
    TB_Node* clone = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
    memcpy(clone->extra, n->extra, extra);
    FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
        clone->inputs[j] = n->inputs[j];
        add_user(f, clone, n->inputs[j], j);
    }
    return clone;
}

static int spill_map_get2(NL_Table* spill_map, int k) {
    intptr_t p = (intptr_t) nl_table_get(spill_map, (void*) (uintptr_t) k);
    return p - 1;
}

static bool should_skip_over(TB_Node* n) {
    return is_proj(n) || n->type == TB_PHI || n->type == TB_MACH_FRAME_PTR;
}

static TB_Node** coalesce_set_array(Ctx* ctx, Rogers* ra, TB_Node** n_ptr, size_t* out_count) {
    int leader = uf_find(ra->uf, ra->uf_len, (*n_ptr)->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set) {
        *out_count = aarray_length(set);
        return set;
    } else {
        *out_count = 1;
        return n_ptr;
    }
}

static TB_Node* alloc_spill_node(Ctx* ctx, Rogers* ra, RegMask* spill_mask, TB_Node* def) {
    TB_Function* f = ctx->f;
    RegMask* def_mask = ctx->constraint(ctx, def, NULL);
    TB_ASSERT(def_mask->class != REG_CLASS_STK);

    // remove may_spill if it was there
    if (def_mask->may_spill) {
        TB_ASSERT(def_mask->count == 1);
        def_mask = intern_regmask(ctx, def_mask->class, false, def_mask->mask[0]);
    }

    TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, def->dt, 2, sizeof(TB_NodeMachCopy));
    set_input(f, cpy, def, 1);
    TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = spill_mask, .use = def_mask);
    return cpy;
}

static void insert_op_at_end(Ctx* ctx, Rogers* ra, TB_BasicBlock* bb, TB_Node* n) {
    int pos = aarray_length(bb->items);
    TB_Node* last = bb->items[pos - 1];
    if (is_proj(last)) { last = last->inputs[0]; }
    if (cfg_is_terminator(last)) {
        pos--;

        while (pos > 0 && bb->items[pos] != bb->start && is_proj(bb->items[pos])) {
            pos--;
        }
    }

    size_t bb_id = bb - ctx->cfg.blocks;
    rogers_insert_op(ctx, bb_id, n, pos);

    if (ra != NULL) {
        // move up if necessary
        FOR_N(class, 1, ctx->num_classes) {
            ra->hrp[bb_id].start[class] += pos <= ra->hrp[bb_id].start[class];
            ra->hrp[bb_id].end[class] += pos <= ra->hrp[bb_id].end[class];
        }
    }
}

static void assign_spill_vreg(Ctx* ctx, Rogers* ra, RegSplitter* splitter, TB_Node* n, int spill) {
    RegMask* spill_mask = splitter->spill_mask[spill];
    int vreg_id = splitter->spill_vreg_id[spill];

    // TODO(NeGate): not the fastest way to approach this, consider optimizing in the future
    rogers_uncoalesce(ctx, ra, n->gvn);

    // there's only one VReg for all the spill space
    if (vreg_id == 0) {
        VReg* new_vreg = tb__set_node_vreg(ctx, n);
        new_vreg->reg_width = tb__reg_width_from_dt(spill_mask->class, n->dt);
        new_vreg->mask = spill_mask;
        vreg_id = new_vreg - ctx->vregs;

        nl_table_put(&splitter->spill_map, (void*) (uintptr_t) vreg_id, (void*) ((uintptr_t) spill + 1));
        splitter->spill_vreg_id[spill] = vreg_id;
    } else {
        VReg* vreg = &ctx->vregs[vreg_id];
        int y = uf_find(ra->uf, ra->uf_len, n->gvn);
        rogers_coalesce(ctx, ra, vreg->n->gvn, y, vreg->n, n);
    }

    aarray_insert(ctx->vreg_map, n->gvn, vreg_id);
}

static TB_Node* insert_spill(Ctx* ctx, Rogers* ra, RegSplitter* splitter, TB_BasicBlock* bb, TB_Node* n, int spill, size_t* insert_pos) {
    RegMask* spill_mask = splitter->spill_mask[spill];
    TB_Node* cpy = alloc_spill_node(ctx, ra, spill_mask, n);
    assign_spill_vreg(ctx, ra, splitter, cpy, spill);
    aarray_push(splitter->all_defs, cpy);

    if (insert_pos) {
        size_t t = *insert_pos, cnt = aarray_length(bb->items);
        while (t < cnt && should_skip_over(bb->items[t])) {
            t++;
        }
        size_t bb_id = bb - ctx->cfg.blocks;
        rogers_insert_op(ctx, bb_id, cpy, t);
        *insert_pos = t;

        // move up if necessary
        FOR_N(class, 1, ctx->num_classes) {
            ra->hrp[bb_id].start[class] += t <= ra->hrp[bb_id].start[class];
            ra->hrp[bb_id].end[class] += t <= ra->hrp[bb_id].end[class];
        }
    } else {
        insert_op_at_end(ctx, ra, bb, cpy);
    }

    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: spill-store %%%u (of %%%u)\n", bb - ctx->cfg.blocks, spill, cpy->gvn, n->gvn));
    return cpy;
}

static TB_Node* insert_reload(Ctx* ctx, Rogers* ra, RegSplitter* splitter, TB_BasicBlock* bb, TB_Node* n, int spill, size_t* insert_pos) {
    TB_Function* f = ctx->f;
    size_t bb_id = bb - ctx->cfg.blocks;

    // if there's local definitions, we'll hook them now, if not we'll hook them to
    // the "leader" node and rewrite these during the PHI insertion stage
    bool is_spill = false;
    if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
        is_spill = reg_mask_is_stack(cpy->def);
    }

    TB_Node* cpy;
    if (!is_spill && can_remat(ctx, n)) {
        size_t extra = extra_bytes(n);
        cpy = clone_node(f, n, extra);
        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: remat %%%u (of %%%u)\n", bb_id, spill, cpy->gvn, n->gvn));
    } else {
        cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, n, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = splitter->reload_mask[spill], .use = splitter->spill_mask[spill]);

        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: reload %%%u (of %%%u)\n", bb_id, spill, cpy->gvn, n->gvn));
    }
    aarray_push(splitter->all_defs, cpy);

    if (insert_pos) {
        // should probably place it above any temps
        size_t t = *insert_pos;
        while (t > 0 && bb->items[t]->type == TB_MACH_TEMP) {
            t--;
        }
        rogers_insert_op(ctx, bb_id, cpy, t);
        *insert_pos = t;

        // move up if necessary
        FOR_N(class, 1, ctx->num_classes) {
            ra->hrp[bb_id].start[class] += t <= ra->hrp[bb_id].start[class];
            ra->hrp[bb_id].end[class] += t <= ra->hrp[bb_id].end[class];
        }
    } else {
        insert_op_at_end(ctx, ra, bb, cpy);
    }

    return cpy;
}

static void dump_split_state(TB_Node** defs, int bb_id, int num_spills, uint64_t W, uint64_t S) {
    printf("  BB%-3d: W=", bb_id);
    FOR_REV_N(j, 0, num_spills) {
        putchar((W >> j) & 1 ? '1' : '0');
    }
    printf(" S=");
    FOR_REV_N(j, 0, num_spills) {
        putchar((S >> j) & 1 ? '1' : '0');
    }
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

static void update_phi_edges(Ctx* ctx, Rogers* ra, RegSplitter* splitter, TB_Node** defs, TB_Node** phis, int bb_id, int pred_id, uint64_t all_spills, int pred_path, bool backedge) {
    TB_Function* f = ctx->f;
    int num_spills = splitter->num_spills;

    TB_Node* header = ctx->cfg.blocks[bb_id].start;
    int pred_count = header->type == TB_PROJ && header->inputs[0]->type == TB_ROOT ? 0 : header->input_count;

    TB_Node** bb_phis = &phis[bb_id*num_spills];
    TB_Node** bb_defs = &defs[bb_id*num_spills];
    TB_Node** pred_defs = &defs[pred_id*num_spills];

    uint64_t W = splitter->W_entry[bb_id];
    // uint64_t S = splitter->S_entry[bb_id];

    // insert copies:
    // * in reg but not in pred, reload.
    // * spilled but not in pred and in reg, spill.
    uint64_t insert_reloads = W & ~splitter->W_exit[pred_id]; // (W & ~splitter->W_exit[pred_id]) & all_spills;
    uint64_t insert_spills  = splitter->W_exit[pred_id] & ~W; // ((S & ~splitter->S_exit[pred_id]) & all_spills) & splitter->W_exit[pred_id];
    uint64_t conflict = insert_spills ^ insert_reloads;
    insert_spills  &= conflict;
    insert_reloads &= conflict;
    // no path should both spill and reload
    TB_ASSERT((insert_spills ^ insert_reloads) == (insert_spills | insert_reloads));

    // check if the forward edges differ, if they do we need phis
    FOR_N(k, 0, num_spills) {
        TB_Node* pred_def = pred_defs[k];

        if (!((splitter->live_out[pred_id] >> k) & 1)) {
            continue;
        }

        if (pred_def) {
            if ((insert_reloads >> k) & 1) {
                pred_def = insert_reload(ctx, ra, splitter, &ctx->cfg.blocks[pred_id], pred_def, k, NULL);
            }

            if ((insert_spills >> k) & 1) {
                // if there's already a copy, we can steal it
                if (pred_def->type == TB_MACH_COPY && pred_def->user_count == 1 && !reg_mask_is_stack(TB_NODE_GET_EXTRA_T(pred_def, TB_NodeMachCopy)->use)) {
                    TB_NODE_GET_EXTRA_T(pred_def, TB_NodeMachCopy)->def = splitter->spill_mask[k];
                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%d: SPILL%zu: convert copy to spill-store %%%u\n", bb_id, k, pred_def->gvn));
                } else if (((splitter->single_def >> k) & 1) && can_remat(ctx, pred_def)) {
                    // don't affect it
                } else {
                    pred_def = insert_spill(ctx, ra, splitter, &ctx->cfg.blocks[pred_id], pred_def, k, NULL);
                }
            }
        }

        if (backedge) {
            // we know there's a phi so there's no need for the rest of this goop
            if (bb_phis[k]) {
                set_input(f, bb_phis[k], pred_def, 1+pred_path);
            }
        } else {
            if (bb_defs[k] == NULL) {
                bb_defs[k] = pred_def;
            } else if (bb_defs[k]->type == TB_PHI && bb_defs[k]->inputs[0] == header) {
                if (bb_defs[k]->dt.type == TB_TAG_VOID) {
                    // just in case...
                    bb_defs[k]->dt = pred_def->dt;
                }
                set_input(f, bb_defs[k], pred_def, 1+pred_path);
            } else if (bb_defs[k] != pred_def) {
                if (((splitter->single_def >> k) & 1) && can_remat(ctx, pred_def)) {
                    // they're the same node, just different locations, we remat once more
                    TB_ASSERT(bb_defs[k]->type == pred_def->type);

                    size_t extra = extra_bytes(pred_def);
                    TB_Node* cpy = clone_node(f, pred_def, extra);
                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%d: SPILL%zu: remat %%%u (of %%%u)\n", bb_id, k, cpy->gvn, pred_def->gvn));
                    aarray_push(splitter->all_defs, cpy);

                    rogers_insert_op(ctx, bb_id, cpy, 1);
                    bb_defs[k] = cpy;
                } else {
                    // convert from single value to PHI
                    TB_Node* phi = phis[bb_id*num_spills + k];
                    if (phi == NULL) {
                        phi = tb_alloc_node(f, TB_PHI, pred_def->dt, 1 + pred_count, 0);
                        phis[bb_id*num_spills + k] = phi;
                        rogers_insert_op(ctx, bb_id, phi, 1);
                    }
                    set_input(f, phi, header, 0);
                    // all the edges up until this point were the same
                    TB_Node* same = bb_defs[k];
                    FOR_N(i, 0, pred_path) {
                        set_input(f, phi, same, 1+i);
                    }
                    set_input(f, phi, pred_def, 1+pred_path);
                    bb_defs[k] = phi;

                    aarray_push(splitter->all_phis, phi);
                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%d: phi %%%u\n", bb_id, phi->gvn));
                }
            }
        }
    }
}

static void* arena_zalloc(TB_Arena* arena, size_t size) {
    void* ptr = tb_arena_alloc(arena, size);
    memset(ptr, 0, size);
    return ptr;
}

static void tb__insert_splits(Ctx* ctx, Rogers* restrict ra) {
    TB_Arena* arena = ra->arena;
    TB_Function* f = ctx->f;

    size_t old_node_count = f->node_count;
    size_t num_spills = dyn_array_length(ra->splits);

    // we can only spill 64 vregs at once due to some bitsets i don't feel like changing
    TB_ASSERT(num_spills < 64);

    RegSplitter splitter = { 0 };
    splitter.num_spills = num_spills;
    splitter.spill_map = nl_table_alloc(num_spills);
    splitter.spill_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    splitter.reload_mask = tb_arena_alloc(ra->arena, num_spills * sizeof(RegMask*));
    TB_Node** leaders = tb_arena_alloc(ra->arena, num_spills * sizeof(TB_Node*));

    splitter.spill_vreg_id = tb_arena_alloc(ra->arena, num_spills * sizeof(int));
    uint64_t class2vreg[MAX_REG_CLASSES] = { 0 };
    FOR_N(i, 0, num_spills) {
        uint32_t vreg_id = ra->splits[i].target;
        nl_table_put(&splitter.spill_map, (void*) (uintptr_t) vreg_id, (void*) (i + 1));

        VReg* to_spill = &ctx->vregs[vreg_id];
        int class = to_spill->mask->class;
        TB_ASSERT(class > 0);
        class2vreg[class] |= 1ull << i;

        // the splitter cannot operate on stack nodes, they also
        // don't ever need "splitting"
        TB_ASSERT(!reg_mask_is_stack(to_spill->mask));
        splitter.reload_mask[i] = ctx->normie_mask[class];
        if (ra->splits[i].clobber) {
            RegMask* clobber = ra->splits[i].clobber;
            TB_ASSERT(ctx->num_regs[clobber->class] < 64);

            uint64_t total_mask = UINT64_MAX >> (64 - ctx->num_regs[clobber->class]);
            splitter.spill_mask[i] = intern_regmask(ctx, clobber->class, true, ~clobber->mask[0] & total_mask);
        } else {
            splitter.spill_mask[i] = ctx->mayspill_mask[class];
        }
        splitter.spill_vreg_id[i] = 0;

        to_spill->mask = NULL;
        to_spill->spill_cost = NAN;

        TB_ASSERT(to_spill->n);
        leaders[i] = to_spill->n;
    }

    // [def_i*num_spills + spill_i]
    TB_Node** defs = arena_zalloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    TB_Node** phis = arena_zalloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    FOR_N(i, 0, num_spills * ctx->bb_count) {
        defs[i] = NULL;
        phis[i] = NULL;
    }

    // these are nodes which aggressively try to spill their entire range, generally because it's
    // not used throughout most of it until the end
    uint64_t spill_aggro = 0;
    splitter.single_def  = 0;

    uint64_t* def_map = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* use_map = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    // W means it's in a register right now
    splitter.W_entry  = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.W_exit   = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.live_in  = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    splitter.live_out = arena_zalloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    ////////////////////////////////
    // 1. Classify spilled regions
    ////////////////////////////////
    FOR_N(spill_i, 0, num_spills) {
        size_t cnt;
        TB_Node** arr = coalesce_set_array(ctx, ra, &leaders[spill_i], &cnt);
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
            if (arr[0]->user_count == 1) {
                spill_aggro |= 1ull << spill_i;
            }
        }
    }

    #if TB_OPTDEBUG_REGSPLIT
    printf("== TO BE SPILLED ==\n");
    FOR_N(i, 0, num_spills) {
        uint32_t vreg_id = ra->splits[i].target;
        double cost = rogers_get_spill_cost(ctx, ra, &ctx->vregs[vreg_id]);

        printf("  V%-5u %f", vreg_id, cost);
        if ((spill_aggro >> i) & 1) {
            printf(" (SPILL AGGRO)");
        }
        printf("\n");

        size_t cnt;
        TB_Node** arr = coalesce_set_array(ctx, ra, &leaders[i], &cnt);
        FOR_N(j, 0, cnt) {
            TB_Node* n = arr[j];

            printf("  * ");
            ctx->print_pretty(ctx, n);
            printf("\n");
        }
    }
    #endif

    #if TB_OPTDEBUG_REGSPLIT
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
            HRPRegion* hrp = &ra->hrp[i];
            if (hrp->start[class] >= 0) {
                printf("[%s, %d (%%%u), %d (%%%u)] ", reg_class_name(class), hrp->start[class], bb->items[hrp->start[class]]->gvn, hrp->end[class], bb->items[hrp->end[class]]->gvn);
            }
        }
        FOR_SUCC(it, bb->end) {
            TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, it.succ);
            printf("  BB%-3zu", succ_bb - ctx->cfg.blocks);
        }
        printf("\n");
    }
    #endif

    ////////////////////////////////
    // 2. Insert spills and reloads
    ////////////////////////////////
    #if TB_OPTDEBUG_REGSPLIT
    printf("== INSERT NODES ==\n");
    #endif

    uint64_t all_spills = (UINT64_MAX >> (64 - num_spills));
    splitter.all_phis = aarray_create(arena, TB_Node*, 30);
    splitter.all_defs = aarray_create(arena, TB_Node*, 30);

    int freq[64];
    ArenaArray(uint32_t) loops = aarray_create(arena, uint32_t, 10);
    FOR_N(bb_id, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
        HRPRegion* hrp = &ra->hrp[bb_id];

        TB_Node* header = bb->start;
        int pred_count = header->type == TB_PROJ && header->inputs[0]->type == TB_ROOT ? 0 : header->input_count;

        TB_Node** bb_defs = &defs[bb_id*num_spills];
        uint64_t W = 0;
        // uint64_t S = 0;

        ////////////////////////////////
        // Compute W_entry and S_entry
        ////////////////////////////////
        int loop_dom = -1;
        int loop_tail = -1;
        if (pred_count) {
            FOR_N(j, 0, num_spills) {
                freq[j] = 0;
            }

            int fwd_pred_count = 0;
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

                // S |= splitter.S_exit[pred_id];
            }

            // if all forward preds go into this block in a register
            // we'll keep it that way
            FOR_N(k, 0, num_spills) {
                if (freq[k] == fwd_pred_count) {
                    W |= 1ull << k;
                }
            }
            // S &= W;

            if (loop_tail >= 0) {
                // "live thru" spills are those which go around the loop AND are used within it
                uint64_t live_thru = 0;
                uint64_t in_hrp = 0;
                FOR_N(j, bb_id, loop_tail+1) {
                    live_thru |= def_map[j];
                    live_thru |= use_map[j];

                    HRPRegion* hrp = &ra->hrp[j];
                    FOR_N(class, 1, ctx->num_classes) {
                        in_hrp |= (hrp->start[class] >= 0 ? class2vreg[class] : 0);
                    }
                }

                // always spill live_thrus
                W &= ~in_hrp;
            }
        }

        // process phi defs
        if (cfg_is_region(header)) {
            FOR_USERS(u, header) {
                if (USERN(u)->type != TB_PHI) {
                    continue;
                }

                TB_Node* phi = USERN(u);
                int src_vreg = phi->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[phi->gvn] : 0;
                int spill = spill_map_get2(&splitter.spill_map, src_vreg);
                if (spill >= 0 && phi->gvn < old_node_count) {
                    bb_defs[spill] = phi;
                    phis[bb_id*num_spills + spill] = phi;

                    if (!((W >> spill) & 1)) {
                        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: def %%%u (spill-phi)\n", bb_id, spill, phi->gvn));
                        assign_spill_vreg(ctx, ra, &splitter, phi, spill);
                        // S |= (1ull << spill);
                    } else {
                        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: def %%%u\n", bb_id, spill, phi->gvn));
                    }
                    aarray_push(splitter.all_phis, phi);
                }
            }
        }

        splitter.W_entry[bb_id] = W;
        // splitter.S_entry[bb_id] = S;

        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id >= bb_id) {
                // backedge, always insert phis
                FOR_N(k, 0, num_spills) {
                    TB_Node* phi = phis[bb_id*num_spills + k];
                    if (phi == NULL) {
                        TB_Node* same = bb_defs[k];
                        if (same == NULL || !((splitter.live_in[bb_id] >> k) & 1)) {
                            TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%zu: the spill doesn't actually live through this block\n", bb_id, k));
                            continue;
                        }

                        // single-def nodes don't need to split
                        if (((splitter.single_def >> k) & 1) && can_remat(ctx, same)) {
                            // __debugbreak();
                            bb_defs[k] = same;
                            continue;
                        }

                        // TB_ASSERT(same != NULL && "how is there no dominating edge yet?");
                        phi = tb_alloc_node(f, TB_PHI, same ? same->dt : TB_TYPE_VOID, 1 + pred_count, 0);
                        set_input(f, phi, header, 0);
                        FOR_N(i, 0, j) {
                            set_input(f, phi, same, 1+i);
                        }
                        phis[bb_id*num_spills + k] = phi;
                        rogers_insert_op(ctx, bb_id, phi, 1);
                        /*if (!((W >> k) & 1)) {
                            assign_spill_vreg(ctx, ra, &splitter, phi, k);
                        }*/
                        aarray_push(splitter.all_phis, phi);

                        bb_defs[k] = phi;
                        TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%zu: phi %%%u (incomplete)\n", bb_id, k, phi->gvn));
                    }
                }
            } else {
                update_phi_edges(ctx, ra, &splitter, defs, phis, bb_id, pred_id, all_spills, j, false);
            }
        }

        TB_OPTDEBUG(REGSPLIT)(dump_split_state(bb_defs, bb_id, num_spills, W, 0));

        // queue up a loop completion task once we've walked to the furthest loop tail
        if (loop_tail >= 0) {
            aarray_push(loops, bb_id);
            aarray_push(loops, loop_tail);
        }

        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];

            // ignore the newly inserted nodes, they don't need any fun treatment
            if (n->type == TB_PHI || n->gvn >= old_node_count) {
                continue;
            }

            uint64_t is_hrp_rn = 0;

            // spill if we're in an HRP region
            FOR_N(class, 0, ctx->num_classes) {
                // never even got high pressure
                if (ra->hrp[bb_id].start[class] < 0) {
                    continue;
                }

                if (j >= ra->hrp[bb_id].start[class] && j <= ra->hrp[bb_id].end[class]) {
                    FOR_N(spill, 0, num_spills) {
                        TB_Node* def = bb_defs[spill];
                        if (((class2vreg[class] & W) >> spill) & 1) {
                            if (def == NULL) {
                                TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%zu: wasn't live in HRP region\n", bb_id, spill));
                            } else if (!can_remat(ctx, def)) {
                                size_t t = j;
                                bb_defs[spill] = insert_spill(ctx, ra, &splitter, bb, def, spill, &t);
                                j += t <= j;
                            } else if (def->type == TB_MACH_COPY && !reg_mask_is_stack(TB_NODE_GET_EXTRA_T(def, TB_NodeMachCopy)->use)) {
                                TB_NODE_GET_EXTRA_T(def, TB_NodeMachCopy)->def = splitter.spill_mask[spill];
                                TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%zu: convert copy to spill-store %%%u\n", bb_id, spill, def->gvn));
                            } else {
                                TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%zu: split remat %%%u\n", bb_id, spill, def->gvn));
                            }

                            // S &= ~(1ull << spill);
                            W &= ~(1ull << spill);
                            is_hrp_rn |= 1ull << spill;
                        }
                    }
                }
            }

            // reload on demand if a use comes up and we're spilled
            bool vaporized = false;
            FOR_N(k, 1, n->input_count) {
                TB_Node* in = n->inputs[k];
                if (in == NULL) {
                    continue;
                }

                int src_vreg = in->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[in->gvn] : 0;
                int spill = spill_map_get2(&splitter.spill_map, src_vreg);
                if (spill >= 0 && in->gvn < old_node_count) {
                    TB_Node* def = bb_defs[spill];

                    if ((W >> spill) & 1) {
                        // the reload mask might not overlap with the use mask if we clobbered
                        // a reg and realized we couldn't maintain the old mask, this might force a copy right before the
                        // "tight" uses.
                        /* RegMask* in_mask = constraint_in(ctx, n, k);
                        RegMask* meet = tb__reg_mask_meet(ctx, splitter.reload_mask[spill], in_mask);
                        if (meet == &TB_REG_EMPTY) {
                            __debugbreak();
                        } */
                    } else {
                        if (n->type == TB_MACH_COPY) {
                            TB_NodeMachCopy* dst = TB_NODE_GET_EXTRA(n);
                            if (reg_mask_is_stack(dst->def) && ctx->vreg_map[n->gvn] == src_vreg) {
                                // we're asking to spill something that's already spilled, just extend the range.
                                TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: connecting spill range at %%%u with %%%u\n", bb_id, spill, n->gvn, def->gvn));
                                RegMask* def_mask = splitter.spill_mask[spill];

                                // update any copy masks, the users of a spill can only really be copies
                                // or folded reload uses
                                FOR_USERS(u, n) {
                                    if (USERN(u)->type == TB_MACH_COPY) {
                                        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(USERN(u));
                                        cpy->use = def_mask;
                                    }
                                }

                                subsume_node2(f, n, def);
                                vaporized = true;
                                TB_ASSERT(n->user_count == 0);
                            } else {
                                // convert copy into reload
                                if (def->type == TB_MACH_COPY && reg_mask_is_stack(TB_NODE_GET_EXTRA_T(def, TB_NodeMachCopy)->use)) {
                                    dst->use = TB_NODE_GET_EXTRA_T(def, TB_NodeMachCopy)->use;
                                    set_input(f, n, def->inputs[1], k);
                                    continue;
                                } else if (!reg_mask_is_stack(dst->def)) {
                                    dst->use = splitter.spill_mask[spill];
                                    set_input(f, n, def, k);
                                    continue;
                                }
                            }
                        }

                        RegMask* in_mask = constraint_in(ctx, n, k);
                        bool can_fold = in_mask->may_spill;

                        // if we could do a folded reload but the destination can't, we
                        // fail to do perform it (since we couldn't coalesce).
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

                        if (!can_fold) { // || (n->type == TB_MACH_COPY && n->dt.raw != def->dt.raw)) {
                            // should probably place it above any temps
                            size_t t = j;
                            def = insert_reload(ctx, ra, &splitter, bb, def, spill, &t);
                            j += t <= j;

                            // if we're reloading in an HRP region, keep ourselves spilled for now
                            if (!((is_hrp_rn >> spill) & 1)) {
                                bb_defs[spill] = def;
                                W |= 1ull << spill;
                            }

                            // S |= 1ull << spill;
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

                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: use at %%%u:%zu (of %%%u)\n", bb_id, spill, n->gvn, k, def->gvn));
                }
            }

            if (vaporized) {
                aarray_push(splitter.all_defs, n);
            } else {
                // mark new definitions
                int dst_vreg = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;
                int spill = spill_map_get2(&splitter.spill_map, dst_vreg);
                if (spill >= 0) {
                    bb_defs[spill] = n;
                    aarray_push(splitter.all_defs, n);

                    W |= 1ull << spill;

                    if (n->type == TB_MACH_COPY) {
                        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
                        cpy->def = splitter.reload_mask[spill];
                    }

                    TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: def %%%u\n", bb_id, spill, n->gvn));

                    // spill immediately
                    if ((spill_aggro >> spill) & 1) {
                        if (!can_remat(ctx, n)) {
                            size_t t = j - 1;
                            bb_defs[spill] = insert_spill(ctx, ra, &splitter, bb, n, spill, &t);
                            j += t <= j;
                        } else if (n->type == TB_MACH_COPY && !reg_mask_is_stack(TB_NODE_GET_EXTRA_T(n, TB_NodeMachCopy)->use)) {
                            TB_NODE_GET_EXTRA_T(n, TB_NodeMachCopy)->def = splitter.spill_mask[spill];
                            TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: convert copy to spill-store %%%u\n", bb_id, spill, n->gvn));
                        } else {
                            TB_OPTDEBUG(REGSPLIT)(printf("  BB%zu: SPILL%d: split remat %%%u\n", bb_id, spill, n->gvn));
                        }

                        // S &= ~(1ull << spill);
                        W &= ~(1ull << spill);
                    }
                }
            }
        }

        TB_OPTDEBUG(REGSPLIT)(dump_split_state(bb_defs, bb_id, num_spills, W, 0));

        splitter.W_exit[bb_id] = W;
        // splitter.S_exit[bb_id] = S;

        // complete loop phis
        size_t top = aarray_length(loops);
        if (top && loops[top - 1] == bb_id) {
            uint32_t head = loops[top - 2];
            aarray_set_length(loops, top - 2);

            // this is always a region so the pred count is simpler
            TB_Node* header = ctx->cfg.blocks[head].start;
            size_t pred_count = header->input_count;

            FOR_N(j, 0, pred_count) {
                TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
                TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                int pred_id = pred_bb - ctx->cfg.blocks;
                if (pred_id < head) {
                    continue;
                }

                update_phi_edges(ctx, ra, &splitter, defs, phis, head, pred_id, all_spills, j, true);
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

    ////////////////////////////////
    // 3. Re-coalesce nodes
    ////////////////////////////////
    #if TB_OPTDEBUG_REGSPLIT
    printf("== PHIS ==\n");
    #endif

    // insert copies on any phis which have pre-split and post-split
    // paths coming in (inserting copies to make these phis into post-split)
    aarray_for(i, splitter.all_phis) {
        TB_Node* n = splitter.all_phis[i];
        if (n->type == TB_NULL) { // it was pruned, remove it from the sets
            rogers_uncoalesce(ctx, ra, n->gvn);
            continue;
        }

        #if TB_OPTDEBUG_REGSPLIT
        printf("  ");
        ctx->print_pretty(ctx, n);
        printf("\n");
        #endif

        // it might be time to include these guys
        aarray_push(splitter.all_defs, n);

        int vreg_id = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;
        int spill = spill_map_get2(&splitter.spill_map, vreg_id);
        if (spill < 0 || splitter.spill_vreg_id[spill] == vreg_id) {
            continue;
        }

        int post_split = 0;
        FOR_N(j, 1, n->input_count) {
            post_split += (n->inputs[j]->gvn >= old_node_count);
        }

        if (post_split != n->input_count - 1) {
            TB_Node* header = n->inputs[0];
            FOR_N(j, 1, n->input_count) {
                TB_Node* pred_def = n->inputs[j];
                if (pred_def->gvn < old_node_count) {
                    TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j - 1);
                    TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);
                    TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                    int pred_id = pred_bb - ctx->cfg.blocks;

                    RegMask* def_mask = ctx->constraint(ctx, pred_def, NULL);

                    TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, pred_def->dt, 2, sizeof(TB_NodeMachCopy));
                    set_input(f, cpy, pred_def, 1);
                    TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = def_mask, .use = def_mask);

                    insert_op_at_end(ctx, ra, pred_bb, cpy);
                    aarray_push(splitter.all_defs, cpy);

                    set_input(f, n, cpy, j);
                }
            }
        }
    }

    // split all the defs and then re-coalesce them
    #if TB_OPTDEBUG_REGSPLIT
    printf("== RE-COALESCE ==\n");
    #endif

    if (f->node_count >= ra->uf_len) {
        size_t new_len = tb_next_pow2(f->node_count + 16);
        rogers_resize_uf(ctx, ra, new_len);
    }

    // make sure there's enough room for the vreg_map
    ctx->vreg_map = aarray__reserve2(ctx->vreg_map, sizeof(*ctx->vreg_map), f->node_count);

    for (size_t i = 0; i < aarray_length(splitter.all_defs);) {
        TB_Node* n = splitter.all_defs[i];
        if (n->user_count == 0) {
            rogers_uncoalesce(ctx, ra, n->gvn);

            #if TB_OPTDEBUG_REGSPLIT
            printf("  KILL ");
            ctx->print_pretty(ctx, n);
            printf("\n");
            #endif

            // delete the original def
            TB_ASSERT(f->scheduled[n->gvn]);
            ctx->vreg_map[n->gvn] = 0;
            tb__remove_node(ctx, f, n);
            tb_kill_node(f, n);

            aarray_remove(splitter.all_defs, i);
            continue;
        }

        // make them all their own roots
        TB_ASSERT(n->gvn < ra->uf_len);
        ra->uf[n->gvn] = n->gvn;
        ra->uf_size[n->gvn] = 1;

        TB_ASSERT(n->gvn < aarray_length(ctx->vreg_map));
        ctx->vreg_map[n->gvn] = 0;

        #if TB_OPTDEBUG_REGSPLIT
        printf("  ");
        ctx->print_pretty(ctx, n);
        printf("\n");
        #endif

        // most of these nodes don't really have a bound set but whatever
        nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (n->gvn + 1));
        i += 1;
    }

    #if TB_OPTDEBUG_REGSPLIT
    printf("\n");
    #endif

    // we can now aggressively coalesce nodes without
    // worry... unless it's a copy node, we assume those
    // are always relevant splits.
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int x = uf_find(ra->uf, ra->uf_len, n->gvn);
        RegMask* rm = ctx->constraint(ctx, n, NULL);

        // phi-coalesce
        if (n->type == TB_PHI) {
            FOR_N(k, 1, n->input_count) {
                // interfere against everything in the set
                TB_Node* in = n->inputs[k];
                int y = uf_find(ra->uf, ra->uf_len, in->gvn);
                rogers_coalesce(ctx, ra, x, y, n, in);
            }
        }

        // CISC-coalesce
        int shared_edge = ctx->node_2addr(n);
        if (n->type != TB_MACH_COPY && shared_edge >= 0 && n->inputs[shared_edge]) {
            int y = uf_find(ra->uf, ra->uf_len, n->inputs[shared_edge]->gvn);
            rogers_coalesce(ctx, ra, y, x, n->inputs[shared_edge], n);
        }
    }

    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
        RegMask* mask = ctx->constraint(ctx, n, NULL);

        int vreg_id = ctx->vreg_map[leader];
        if (vreg_id == 0) {
            VReg* new_vreg = tb__set_node_vreg(ctx, n);
            new_vreg->reg_width = tb__reg_width_from_dt(mask->class, n->dt);
            new_vreg->spill_bias = mask->may_spill ? -1.0f : 1e6;
            // new_vreg->was_spilled = true;
            vreg_id = new_vreg - ctx->vregs;
            ctx->vreg_map[leader] = vreg_id;
        } else {
            mask = tb__reg_mask_meet(ctx, mask, ctx->vregs[vreg_id].mask);
        }

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

    #if TB_OPTDEBUG_REGSPLIT
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
            tb__print_regmask(vreg->mask);
            printf("\n  ");
            ctx->print_pretty(ctx, n);
            printf(" # UF size = %d, mask = ", ra->uf_size[n->gvn]);
            tb__print_regmask(ctx->vregs[vreg_id].mask);
            printf("\n");

            size_t cnt;
            TB_Node** arr = coalesce_set_array(ctx, ra, &n, &cnt);
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
    #endif

    TB_OPTDEBUG(REGSPLIT)(rogers_dump_sched(ctx, old_node_count));
    TB_OPTDEBUG(REGSPLIT)(__debugbreak());
}
