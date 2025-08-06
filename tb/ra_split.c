// "Register Spilling and Live-Range Splitting for SSA-Form Programs" Sebastian Hack, Matthias Braun (2009)
typedef struct {
    ArenaArray(TB_Node*) all_phis;
    ArenaArray(TB_Node*) all_defs;

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
    TB_ASSERT(!reg_mask_is_stack(def_mask));

    TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, def->dt, 2, sizeof(TB_NodeMachCopy));
    set_input(f, cpy, def, 1);
    TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = spill_mask, .use = def_mask);
    return cpy;
}

static void insert_op_at_end(Ctx* ctx, TB_BasicBlock* bb, TB_Node* n) {
    int pos = aarray_length(bb->items);
    TB_Node* last = bb->items[pos - 1];
    if (is_proj(last)) { last = last->inputs[0]; }
    if (cfg_is_terminator(last)) {
        pos--;

        while (pos > 0 && bb->items[pos] != bb->start && is_proj(bb->items[pos])) {
            pos--;
        }
    }

    rogers_insert_op(ctx, bb - ctx->cfg.blocks, n, pos);
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
        rogers_insert_op(ctx, bb - ctx->cfg.blocks, cpy, t);
        *insert_pos = t;
    } else {
        insert_op_at_end(ctx, bb, cpy);
    }

    TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: spill %%%u (%%%u)\n", bb - ctx->cfg.blocks, n->gvn, cpy->gvn));
    return cpy;
}

static TB_Node* insert_reload(Ctx* ctx, Rogers* ra, RegSplitter* splitter, TB_BasicBlock* bb, TB_Node* n, int spill, size_t* insert_pos) {
    TB_Function* f = ctx->f;
    int bb_id = bb - ctx->cfg.blocks;

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
        TB_OPTDEBUG(REGALLOC4)(printf("  BB%d: remat %%%u (of %%%u)\n", bb_id, cpy->gvn, n->gvn));
    } else {
        cpy = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, cpy, n, 1);
        TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = splitter->reload_mask[spill], .use = splitter->spill_mask[spill]);

        TB_OPTDEBUG(REGALLOC4)(printf("  BB%d: reload %%%u (of %%%u)\n", bb_id, cpy->gvn, n->gvn));
    }
    aarray_push(splitter->all_defs, cpy);

    // should probably place it above any temps
    size_t t = *insert_pos;
    while (t > 0 && bb->items[t]->type == TB_MACH_TEMP) {
        t--;
    }
    *insert_pos = t;

    rogers_insert_op(ctx, bb_id, cpy, t);
    return cpy;
}

static void tb__insert_splits(Ctx* ctx, Rogers* restrict ra) {
    TB_Arena* arena = ra->arena;
    TB_Function* f = ctx->f;

    size_t old_node_count = f->node_count;
    size_t num_spills = dyn_array_length(ra->splits);

    // we can only spill 64 vregs at once due to some bitsets i don't feel like changing
    TB_ASSERT(num_spills < 64);

    RegSplitter splitter = { 0 };
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
        splitter.spill_mask[i] = ctx->mayspill_mask[class];
        if (ra->splits[i].clobber) {
            RegMask* clobber = ra->splits[i].clobber;
            TB_ASSERT(ctx->num_regs[clobber->class] < 64);

            uint64_t total_mask = UINT64_MAX >> (64 - ctx->num_regs[clobber->class]);
            splitter.reload_mask[i] = intern_regmask(ctx, clobber->class, true, ~clobber->mask[0] & total_mask);
        } else {
            splitter.reload_mask[i] = ctx->normie_mask[class];
        }
        splitter.spill_vreg_id[i] = 0;

        to_spill->mask = NULL;
        to_spill->spill_cost = NAN;

        TB_ASSERT(to_spill->n);
        leaders[i] = to_spill->n;
    }

    #if TB_OPTDEBUG_REGALLOC4
    printf("== TO BE SPILLED ==\n");
    FOR_N(i, 0, num_spills) {
        uint32_t vreg_id = ra->splits[i].target;
        double cost = rogers_get_spill_cost(ctx, ra, &ctx->vregs[vreg_id]);

        printf("  V%-5u %f\n", vreg_id, cost);

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

    // [def_i*num_spills + spill_i]
    TB_Node** defs = tb_arena_alloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    TB_Node** phis = tb_arena_alloc(ra->arena, num_spills * ctx->bb_count * sizeof(TB_Node*));
    FOR_N(i, 0, num_spills * ctx->bb_count) {
        defs[i] = NULL;
        phis[i] = NULL;
    }

    // these are nodes which aggressively try to spill their entire range, generally because it's
    // not used throughout most of it until the end
    uint64_t spill_all = 0;

    uint64_t* def_map = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* use_map = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));

    // W means it's in a register right now
    uint64_t* W_entry = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* W_exit  = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* S_entry = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    uint64_t* S_exit  = tb_arena_alloc(ra->arena, ctx->bb_count * sizeof(uint64_t));
    FOR_N(i, 0, ctx->bb_count) {
        def_map[i] = 0;
        use_map[i] = 0;

        S_entry[i] = 0;
        S_exit[i]  = 0;
        W_entry[i] = 0;
        W_exit[i]  = 0;
    }

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

            // append the liveness info
            /*live_map[bb_id] |= 1ull << spill_i;
            FOR_N(k, 0, ctx->bb_count) {
                TB_BasicBlock* bb = &ctx->cfg.blocks[k];
                if (set_get(&bb->live_in, n->gvn) || set_get(&bb->live_out, n->gvn)) {
                    live_map[k] |= 1ull << spill_i;
                }
            }*/
        }

        if (cnt == 1 && arr[0]->user_count == 1) {
            spill_all |= 1ull << spill_i;
        }
    }

    #if TB_OPTDEBUG_REGALLOC4
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
    #if TB_OPTDEBUG_REGALLOC4
    printf("== INSERT NODES ==\n");
    #endif

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
        uint64_t S = 0;

        int loop_dom = -1;
        int loop_tail = -1;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id >= bb_id) {
                // pick the furthest backedge
                loop_tail = TB_MAX(loop_tail, pred_id);
            }

            S |= S_exit[pred_id];
        }

        ////////////////////////////////
        // Compute W_entry
        ////////////////////////////////
        FOR_N(j, 0, num_spills) {
            freq[j] = 0;
        }

        int fwd_pred_count = 0;
        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id < bb_id) {
                FOR_N(k, 0, num_spills) {
                    freq[k] += (W_exit[pred_id] >> k) & 1;
                }
                fwd_pred_count++;
            }
        }

        // if all forward preds go into this block in a register
        // we'll keep it that way
        FOR_N(k, 0, num_spills) {
            if (freq[k] == pred_count) {
                W |= 1ull << k;
            }
        }

        if (loop_tail >= 0) {
            // "live thru" spills are those which go around the loop AND are used within it
            uint64_t live_thru = 0;
            HRPRegion* hrp = &ra->hrp[bb_id];
            bool is_hrp_loop = false;
            FOR_N(j, bb_id, loop_tail+1) {
                live_thru |= def_map[j];
                live_thru |= use_map[j];
            }

            // always spill live_thrus
            W &= ~live_thru;

            // __debugbreak();
        }
        S &= W;

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
                    TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: def %%%u\n", bb_id, phi->gvn));

                    if (!((W >> spill) & 1)) {
                        assign_spill_vreg(ctx, ra, &splitter, phi, spill);
                    }
                    aarray_push(splitter.all_phis, phi);
                }
            }
        }

        #if TB_OPTDEBUG_REGALLOC4
        printf("  BB%-3zu: W=", bb_id);
        FOR_REV_N(j, 0, num_spills) {
            putchar((W >> j) & 1 ? '1' : '0');
        }
        printf(" S=");
        FOR_REV_N(j, 0, num_spills) {
            putchar((S >> j) & 1 ? '1' : '0');
        }
        printf("\n");
        #endif

        W_entry[bb_id] = W;
        S_entry[bb_id] = S;

        FOR_N(j, 0, pred_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, j);
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            int pred_id = pred_bb - ctx->cfg.blocks;
            if (pred_id >= bb_id) {
                loop_tail = TB_MAX(loop_tail, pred_id);

                // backedge, always insert phis
                FOR_N(k, 0, num_spills) {
                    TB_Node* phi = phis[bb_id*num_spills + k];
                    if (phi == NULL) {
                        TB_Node* same = bb_defs[k];
                        if (same == NULL) {
                            TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: V%zu isn't alive during this loop\n", bb_id, k));
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
                        if ((W >> k) & 1) {
                            assign_spill_vreg(ctx, ra, &splitter, phi, k);
                        }
                        aarray_push(splitter.all_phis, phi);

                        bb_defs[k] = phi;
                        TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: phi %%%u (incomplete)\n", bb_id, phi->gvn));
                    }
                }
            } else {
                // insert copies:
                // * in reg but not in pred, reload.
                // * spilled but not in pred and in reg, spill.
                uint64_t all_spills = (UINT64_MAX >> (64 - num_spills));
                uint64_t insert_spills  = (W ^ W_exit[pred_id]) & all_spills;
                uint64_t insert_reloads = ((S ^ S_exit[pred_id]) & all_spills) & W_exit[pred_id];
                // no path should both spill and reload
                TB_ASSERT((insert_spills ^ insert_reloads) == (insert_spills | insert_reloads));

                // check if the forward edges differ, if they do we need phis
                FOR_N(k, 0, num_spills) {
                    TB_Node* pred_def = defs[pred_id*num_spills + k];

                    if (pred_def) {
                        if ((insert_spills >> k) & 1) {
                            pred_def = insert_spill(ctx, ra, &splitter, pred_bb, pred_def, k, NULL);
                        } else if ((insert_reloads >> k) & 1) {
                            __debugbreak();
                        }
                    }

                    if (bb_defs[k] == NULL) {
                        bb_defs[k] = pred_def;
                    } else if (bb_defs[k]->type == TB_PHI && bb_defs[k]->inputs[0] == header) {
                        if (loop_tail >= 0) {
                            // just in case...
                            bb_defs[k]->dt = pred_def->dt;
                        }
                        set_input(f, bb_defs[k], pred_def, 1+j);
                    } else if (bb_defs[k] != pred_def) {
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
                        FOR_N(i, 0, j) {
                            set_input(f, phi, same, 1+i);
                        }
                        set_input(f, phi, pred_def, 1+j);
                        bb_defs[k] = phi;

                        TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: phi %%%u\n", bb_id, phi->gvn));
                    }
                }
            }
        }

        // insert spills for these nodes
        FOR_N(i, 0, num_spills) {
            if (!((W >> i) & 1) && bb_defs[i]) {
                TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: spill %%%u\n", bb_id, bb_defs[i]->gvn));
                assign_spill_vreg(ctx, ra, &splitter, bb_defs[i], i);
            }
        }

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

            // spill if we're in an HRP region
            FOR_N(class, 0, ctx->num_classes) {
                // never even got high pressure
                if (ra->hrp[bb_id].start[class] < 0) {
                    continue;
                }

                if (j == ra->hrp[bb_id].start[class]) {
                    FOR_N(spill, 0, num_spills) {
                        TB_Node* def = bb_defs[spill];
                        if (((class2vreg[class] & W) >> spill) & 1) {
                            if (!can_remat(ctx, def)) {
                                size_t t = j - 1;
                                bb_defs[spill] = insert_spill(ctx, ra, &splitter, bb, def, spill, &t);
                                j += t <= j;
                            }

                            S |= 1ull << spill;
                            W &= ~(1ull << spill);
                        }
                    }
                }
            }

            // reload on demand if a use comes up and we're spilled
            FOR_N(k, 1, n->input_count) {
                TB_Node* in = n->inputs[k];
                if (in == NULL) {
                    continue;
                }

                int src_vreg = in->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[in->gvn] : 0;
                int spill = spill_map_get2(&splitter.spill_map, src_vreg);
                if (spill >= 0 && !((W >> spill) & 1) && in->gvn < old_node_count) {
                    TB_Node* def = bb_defs[spill];

                    // should probably place it above any temps
                    size_t t = j;
                    bb_defs[spill] = insert_reload(ctx, ra, &splitter, bb, def, spill, &t);
                    set_input(f, n, bb_defs[spill], k);

                    // if there's other edges on the node that refer to the old node, we
                    // should update them now. we only need to check for a direct node match since indirect
                    // matches to other nodes in the same vreg couldn't be possible (they'd be interfering).
                    FOR_N(l, k+1, n->input_count) {
                        if (n->inputs[l] == def) {
                            set_input(f, n, bb_defs[spill], l);
                        }
                    }

                    j += t <= j;
                    W |= 1ull << spill;
                }
            }

            // mark new definitions
            int dst_vreg = n->gvn < aarray_length(ctx->vreg_map) ? ctx->vreg_map[n->gvn] : 0;
            int spill = spill_map_get2(&splitter.spill_map, dst_vreg);
            if (spill >= 0) {
                bb_defs[spill] = n;
                aarray_push(splitter.all_defs, n);

                W |= 1ull << spill;
                S &= ~(1ull << spill);

                if (n->type == TB_MACH_COPY) {
                    TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
                    cpy->def = splitter.reload_mask[spill];
                }

                TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: def %%%u\n", bb_id, n->gvn));

                // spill immediately
                if ((spill_all >> spill) & 1) {
                    if (!can_remat(ctx, n)) {
                        size_t t = j - 1;
                        bb_defs[spill] = insert_spill(ctx, ra, &splitter, bb, n, spill, &t);
                        j += t <= j;
                    }

                    S |= 1ull << spill;
                    W &= ~(1ull << spill);
                }
            }
        }

        W_exit[bb_id] = W;
        S_exit[bb_id] = S;

        // check if we're the furthest loop tail, if so we need
        // to insert the missing edges on the phi.
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
                if (pred_id < bb_id) {
                    continue;
                }

                uint64_t all_spills = (UINT64_MAX >> (64 - num_spills));
                uint64_t insert_spills  = (W ^ W_exit[pred_id]) & all_spills;
                uint64_t insert_reloads = ((S ^ S_exit[pred_id]) & all_spills) & W_exit[pred_id];
                // no path should both spill and reload
                TB_ASSERT((insert_spills ^ insert_reloads) == (insert_spills | insert_reloads));

                // we only process backedges here and the defs are always the relevant PHIs
                FOR_N(k, 0, num_spills) {
                    TB_Node* phi = phis[head*num_spills + k];
                    if (phi == NULL) {
                        continue;
                    }

                    TB_ASSERT(phi->type == TB_PHI && phi->input_count == 1+pred_count);
                    TB_Node* pred_def = defs[pred_id*num_spills + k];
                    if ((insert_spills >> k) & 1) {
                        pred_def = insert_spill(ctx, ra, &splitter, pred_bb, pred_def, k, NULL);
                    } else if ((insert_reloads >> k) & 1) {
                        __debugbreak();
                    }

                    set_input(f, phi, pred_def, 1+j);
                }
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
                    TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: phi %%%u (pruned to %%%u)\n", bb_id, phi->gvn, leader->gvn));

                    if (defs[head*num_spills + k] == phi) {
                        defs[head*num_spills + k] = leader;
                    }
                    subsume_node(f, phi, leader);
                    tb__remove_node(ctx, f, phi);

                    phis[head*num_spills + k] = NULL;
                } else {
                    TB_OPTDEBUG(REGALLOC4)(printf("  BB%zu: phi %%%u (complete)\n", bb_id, phi->gvn));
                }
            }
        }

        TB_OPTDEBUG(REGALLOC4)(printf("\n"));
    }

    ////////////////////////////////
    // 3. Re-coalesce nodes
    ////////////////////////////////
    // insert copies on any phis which have pre-split and post-split
    // paths coming in (inserting copies to make these phis into post-split)
    aarray_for(i, splitter.all_phis) {
        TB_Node* n = splitter.all_phis[i];
        if (n->type == TB_NULL) { // it was pruned, remove it from the sets
            rogers_uncoalesce(ctx, ra, n->gvn);
            continue;
        }

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

                    insert_op_at_end(ctx, pred_bb, cpy);
                    aarray_push(splitter.all_defs, cpy);

                    set_input(f, n, cpy, j);
                }
            }
        }
    }

    // split all the defs and then re-coalesce them
    #if TB_OPTDEBUG_REGALLOC4
    printf("== RE-COALESCE ==\n");
    #endif

    if (f->node_count >= ra->uf_len) {
        size_t new_len = tb_next_pow2(f->node_count + 16);
        rogers_resize_uf(ctx, ra, new_len);
    }

    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        // make them all their own roots
        TB_ASSERT(n->gvn < ra->uf_len);
        ra->uf[n->gvn] = n->gvn;
        ra->uf_size[n->gvn] = 1;
        ctx->vreg_map[n->gvn] = 0;

        #if TB_OPTDEBUG_REGALLOC4
        printf("  ");
        ctx->print_pretty(ctx, n);
        printf("\n");
        #endif

        // most of these nodes don't really have a bound set but whatever
        nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (n->gvn + 1));
    }

    #if TB_OPTDEBUG_REGALLOC4
    printf("\n");
    #endif

    // make sure there's enough room for the vreg_map
    ctx->vreg_map = aarray__reserve2(ctx->vreg_map, sizeof(*ctx->vreg_map), f->node_count);

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
        ctx->vregs[vreg_id].mask = mask;
        ctx->vreg_map[n->gvn] = vreg_id;
    }

    #if TB_OPTDEBUG_REGALLOC4
    aarray_for(i, splitter.all_defs) {
        TB_Node* n = splitter.all_defs[i];
        if (ra->uf[n->gvn] == n->gvn) {
            int vreg_id = ctx->vreg_map[n->gvn];

            printf("  ");
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

    TB_OPTDEBUG(REGALLOC4)(rogers_dump_sched(ctx, old_node_count));
}
