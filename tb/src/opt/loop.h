
// for the usage histogram
typedef struct {
    TB_DataType dt;
    int uses;
} UsageBin;

typedef struct {
    TB_Arena* arena;
    int leftovers;
    int total_uses;
    ArenaArray(UsageBin) bins;
} UsageHisto;

static int usage_bin_cmp(const void* a, const void* b) {
    UsageBin* const* aa = a;
    UsageBin* const* bb = b;
    return aa[0]->uses - bb[0]->uses;
}

static UsageBin* usage_histo_bin(UsageHisto* histo, TB_DataType dt) {
    aarray_for(i, histo->bins) {
        if (histo->bins[i].dt.raw == dt.raw) {
            return &histo->bins[i];
        }
    }

    // add new bin
    UsageBin b = { dt };
    aarray_push(histo->bins, b);
    return &histo->bins[aarray_length(histo->bins) - 1];
}

// Affine loop accessors
static TB_Node* affine_loop_latch(TB_Node* header) {
    if (header->type == TB_AFFINE_LOOP &&
        header->inputs[1]->type == TB_BRANCH_PROJ &&
        header->inputs[1]->inputs[0]->type == TB_AFFINE_LATCH) {
        return header->inputs[1]->inputs[0];
    }

    return NULL;
}

// clone anything except control edges and phis to region
static TB_Node* loop_clone_node(TB_Function* f, TB_Node** cloned, size_t pre_clone_index, TB_Node* header, TB_Node* n) {
    if (n->gvn >= pre_clone_index) {
        return n;
    } else if (cloned[n->gvn]) {
        return cloned[n->gvn];
    }

    TB_Node* k = n;
    if (n->type == TB_PHI) {
        if (n->inputs[0] == header) {
            // replace OG with loop phi's initial value
            k = n->inputs[1];
        }
    } else if (cfg_is_region(n) || n->type == TB_ROOT || (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT)) {
        // doesn't clone
    } else {
        size_t extra = extra_bytes(n);
        k = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);

        // clone extra data (i hope it's that easy lol)
        memcpy(k->extra, n->extra, extra);

        // fill cloned edges
        FOR_N(i, 1, n->input_count) if (n->inputs[i]) {
            TB_Node* in = loop_clone_node(f, cloned, pre_clone_index, header, n->inputs[i]);
            k->inputs[i] = in;
            add_user(f, k, in, i);

            // uncloned form should refer to the "next" edge on the phi
            if (n->inputs[i]->type == TB_PHI && n->inputs[i]->inputs[0] == header) {
                set_input(f, n, n->inputs[i]->inputs[2], i);
            }
        }

        // keep original ctrl edge, we may replace it later tho
        if (n->inputs[0]) {
            k->inputs[0] = n->inputs[0];
            add_user(f, k, n->inputs[0], 0);
        }

        // mark new node
        mark_node(f, k);
    }
    cloned[n->gvn] = k;

    #if TB_OPTDEBUG_LOOP
    printf("CLONE: ");
    tb_print_dumb_node(NULL, n);
    printf(" => ");
    tb_print_dumb_node(NULL, k);
    printf("\n");
    #endif

    return k;
}

static uint64_t* iconst(TB_Node* n) {
    return n->type == TB_ICONST ? &TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value : NULL;
}

static void swap_nodes(TB_Function* f, TB_Node* n, int i, int j) {
    TB_Node* old = n->inputs[i];
    set_input(f, n, n->inputs[j], i);
    set_input(f, n, old, j);
}

static TB_Node* branch_cproj(TB_Function* f, TB_Node* n, uint64_t taken, int64_t key, int index) {
    TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
    set_input(f, cproj, n, 0);
    TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = index, .taken = taken, .key = key);
    return cproj;
}

static void hoist_ops(TB_Function* f, TB_Node* ctrl, TB_Node* earlier) {
    for (size_t i = 0; i < ctrl->user_count;) {
        TB_Node* un = USERN(&ctrl->users[i]);
        int ui      = USERI(&ctrl->users[i]);
        if (un->type == TB_LOAD && ui == 0) {
            set_input(f, un, earlier, 0);
        } else {
            i += 1;
        }
    }
}

static void mark_reachable(TB_Function* f, uint32_t* before, TB_Node* n) {
    assert(n->gvn < f->node_count);
    if (before[n->gvn / 32] & (1u << (n->gvn % 32))) {
        return;
    } else if (n->type == TB_ROOT) {
        // don't go past root, you'll wrap around and it'll be
        // nasty
        return;
    } else if (cfg_is_region(n)) {
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            mark_reachable(f, before, USERN(u));
        }
    }

    #if TB_OPTDEBUG_LOOP
    printf("REACH: ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
    #endif

    before[n->gvn / 32] |= (1u << (n->gvn % 32));
    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        mark_reachable(f, before, n->inputs[i]);
    }
}

static void replace_phis(TB_Function* f, uint32_t* before, TB_Node* phi, TB_Node* new_phi) {
    for (size_t i = 0; i < phi->user_count;) {
        TB_Node* un = USERN(&phi->users[i]);
        int ui      = USERI(&phi->users[i]);
        if ((before[un->gvn / 32] & (1u << (un->gvn % 32))) == 0) {
            // printf("PHI %%%-3u was used past the loop at %%%-3u (%%%-3u -> %%%-3u)\n", phi->gvn, un->gvn, un->inputs[ui]->gvn, new_phi->gvn);
            set_input(f, un, new_phi, ui);
        } else {
            i += 1;
        }
    }
}

static bool indvar_scan_uses(TB_Function* f, UsageHisto* histo, TB_Node* used, TB_Node* phi, TB_Node* op, int depth) {
    bool found = false;
    FOR_USERS(u, used) {
        TB_Node* use_n = USERN(u);
        if (use_n == phi || use_n == op) { continue; }

        uint64_t* scale;
        UsageBin* bin = NULL;
        if (use_n->type == TB_SIGN_EXT || use_n->type == TB_ZERO_EXT) {
            bin = usage_histo_bin(histo, use_n->dt);
            found = true;
        } else {
            // scan users for anything interesting
            if (depth < 3) {
                if (indvar_scan_uses(f, histo, use_n, phi, op, depth + 1)) {
                    found = true;
                } else {
                    histo->leftovers += 1;
                    histo->total_uses += 1;
                }
            }
            continue;
        }
        FOR_USERS(u2, use_n) {
            TB_Node* use_of_use = USERN(u2);
            if (use_of_use == phi || use_of_use == op) { continue; }
            bin->uses += 1;
            histo->total_uses += 1;
        }
    }
    return found;
}

static TB_Node* indvar_apply(TB_Function* f, TB_Node* n, TB_DataType best_dt) {
    assert(best_dt.raw != n->dt.raw);
    TB_Node* k = tb_alloc_node(f, TB_ZERO_EXT, best_dt, 2, 0);
    set_input(f, k, n, 1);
    mark_node(f, k);
    return tb__gvn(f, k, 0);
}

static bool indvar_simplify(TB_Function* f, TB_Node* phi, TB_Node* op, TB_InductionVar* latch_iv) {
    TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);
    UsageHisto histo = { f->tmp_arena, 0, 0, aarray_create(f->tmp_arena, UsageBin, 16) };

    indvar_scan_uses(f, &histo, phi, phi, op, 0);
    int64_t step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value;

    #if 1
    printf("IV %%%u:\n", phi->gvn);
    {
        printf("  BASE:   ");
        FOR_N(j, 0, histo.leftovers) { printf("#"); }
        printf("\n");
    }
    aarray_for(i, histo.bins) {
        printf("  dt=i%d: ", histo.bins[i].dt.data);
        FOR_N(j, 0, histo.bins[i].uses) { printf("#"); }
        printf("\n");
    }
    #endif

    if (aarray_length(histo.bins) == 0) {
        return false;
    }

    // find most uses
    int best_i = 0;
    FOR_N(i, 1, aarray_length(histo.bins)) {
        if (histo.bins[i].uses > histo.bins[best_i].uses) {
            best_i = i;
        }
    }

    UsageBin* best_bin = &histo.bins[best_i];
    float use_percent = (float)best_bin->uses / (float)histo.total_uses;
    assert(best_bin->dt.type == TB_TAG_INT);

    if (use_percent > 0.95f && best_bin->dt.data > phi->dt.data) {
        TB_DataType best_dt = best_bin->dt;
        uint64_t* inc = iconst(op->inputs[2]);

        // downscaled form
        TB_Node* trunc_phi = tb_alloc_node(f, TB_TRUNCATE, phi->dt, 2, 0);
        mark_node_n_users(f, trunc_phi);

        TB_Node* trunc_op = tb_alloc_node(f, TB_TRUNCATE, op->dt, 2, 0);
        mark_node_n_users(f, trunc_op);

        // upscale op
        TB_Node* scaled_inc = make_int_node(f, best_bin->dt, *inc);
        set_input(f, op, scaled_inc, 2);
        latuni_set(f, scaled_inc, value_of(f, scaled_inc));
        mark_node(f, scaled_inc);

        // upscale phi
        TB_Node* scaled_init = indvar_apply(f, phi->inputs[1], best_bin->dt);
        latuni_set(f, scaled_init, value_of(f, scaled_init));
        set_input(f, phi, scaled_init, 1);

        phi->dt = best_dt;
        op->dt  = best_dt;

        // replace old uses with truncated forms, the peeps can fold away
        // casts like this.
        for (size_t i = 0; i < op->user_count;) {
            TB_User* u = &op->users[i];
            if (USERN(u) != phi) {
                mark_users(f, USERN(u));
                set_input(f, USERN(u), trunc_op, USERI(u));
            } else {
                i += 1;
            }
        }

        for (size_t i = 0; i < phi->user_count;) {
            TB_User* u = &phi->users[i];
            if (USERN(u) != op) {
                mark_users(f, USERN(u));
                set_input(f, USERN(u), trunc_phi, USERI(u));
            } else {
                i += 1;
            }
        }

        set_input(f, trunc_phi, phi, 1);
        set_input(f, trunc_op, op, 1);

        // we run a bunch of constant prop here to avoid forward progress asserts
        latuni_set(f, phi, value_of(f, phi));
        latuni_set(f, op, value_of(f, op));
        latuni_set(f, trunc_phi, value_of(f, trunc_phi));
        latuni_set(f, trunc_op, value_of(f, trunc_op));

        mark_node_n_users(f, op);
        mark_node_n_users(f, phi);
        tb_arena_restore(f->tmp_arena, sp);
        return true;
    } else {
        tb_arena_restore(f->tmp_arena, sp);
        return false;
    }
}

static TB_Node* get_simple_loop_exit(TB_CFG* cfg, TB_Node* header, TB_Node* latch) {
    if ((latch->type != TB_BRANCH && latch->type != TB_AFFINE_LATCH) || TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
        return NULL;
    }

    // for this latch to count it needs to exit (have a path not dominated
    // by the loop's backedge)
    TB_Node* exit = NULL;
    TB_Node* backedge_bb = cfg_get_pred(cfg, header, 1);
    FOR_USERS(u, latch) {
        if (USERN(u)->type != TB_BRANCH_PROJ) { continue; }
        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
        assert(index < 2);

        TB_Node* succ = cfg_next_bb_after_cproj(USERN(u));
        if (!slow_dommy(cfg, succ, backedge_bb)) {
            // successor doesn't dom backedge, we're leaving the loop then
            if (exit) { return NULL; }
            else { exit = USERN(u);  }
        }
    }

    return exit;
}

static const char* ind_pred_names[] = { "ne", "slt", "sle", "ult", "ule" };

// since we're looking at the rotated form:
//
//   i  = phi(init, i2)
//   i2 = i + step where step is constant
static bool affine_indvar(TB_Node* n, TB_Node* header) {
    return n->type == TB_ADD && n->dt.type == TB_TAG_INT
        && n->inputs[1]->type == TB_PHI
        && n->inputs[1]->inputs[0] == header
        && n->inputs[1]->inputs[2] == n
        && n->inputs[2]->type == TB_ICONST;
}

static uint64_t* find_affine_indvar(TB_Node* n, TB_Node* header) {
    if (n->type == TB_PHI &&
        n->inputs[0] == header &&
        n->inputs[2]->type == TB_ADD &&
        n->inputs[2]->inputs[1] == n &&
        n->inputs[2]->inputs[2]->type == TB_ICONST) {
        return &TB_NODE_GET_EXTRA_T(n->inputs[2]->inputs[2], TB_NodeInt)->value;
    }

    return NULL;
}

static bool find_latch_indvar(TB_Node* header, TB_Node* latch, TB_InductionVar* restrict var) {
    bool exit_when_key = !TB_NODE_GET_EXTRA_T(header->inputs[1], TB_NodeProj)->index;
    TB_NodeBranchProj* if_br = cfg_if_branch(latch);
    assert(if_br);

    TB_Node* cond = latch->inputs[1];
    if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE) {
        // canonicalize compare, this way it's always shaped such that "true" means continue
        TB_NodeTypeEnum type = cond->type;
        TB_Node* a = cond->inputs[1];
        TB_Node* b = cond->inputs[2];

        // flip condition
        if ((exit_when_key && if_br->key == 0) ||
            (!exit_when_key && if_br->key == 1)) {
            if (type == TB_CMP_EQ) { type = TB_CMP_NE; }
            else if (type == TB_CMP_NE) { type = TB_CMP_EQ; }
            else {
                SWAP(TB_Node*, a, b);
                switch (type) {
                    case TB_CMP_ULT: type = TB_CMP_ULE; break;
                    case TB_CMP_ULE: type = TB_CMP_ULT; break;
                    case TB_CMP_SLT: type = TB_CMP_SLE; break;
                    case TB_CMP_SLE: type = TB_CMP_SLT; break;
                    default: tb_todo();
                }
            }
        }

        // you're washed if you made it here with an equal... equal? that means it's only
        // looping if it's one specific value, idk go canonicalize that loop elsewhere wtf
        if (cond->type == TB_CMP_EQ) { return false; }

        // shit's scary if both are indvars, it's not "illegal" but also wtf
        bool backwards = false;
        TB_Node *indvar = NULL, *limit = NULL;
        if (affine_indvar(a, header))      { indvar = a->inputs[1], limit = b; }
        else if (affine_indvar(b, header)) { indvar = b->inputs[1], limit = a, backwards = true; }

        if (indvar) {
            // we're a real affine loop now!
            TB_Node* op = indvar->inputs[2];
            assert(indvar->inputs[2]->type == TB_ADD);

            *var = (TB_InductionVar){
                .cond = cond,
                .end_cond = limit,
                .phi  = indvar,
                .step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value,
                .backwards = false
            };

            switch (cond->type) {
                case TB_CMP_NE:  var->pred = IND_NE;  break;
                case TB_CMP_ULE: var->pred = IND_ULE; break;
                case TB_CMP_ULT: var->pred = IND_ULT; break;
                case TB_CMP_SLE: var->pred = IND_SLE; break;
                case TB_CMP_SLT: var->pred = IND_SLT; break;
                default: tb_todo();
            }
            return true;
        }
    } else if (affine_indvar(cond, header) && exit_when_key) {
        assert(cond->inputs[2]->type == TB_ADD);
        *var = (TB_InductionVar){
            .cond = cond,
            .phi  = cond,
            .step = TB_NODE_GET_EXTRA_T(cond->inputs[2]->inputs[2], TB_NodeInt)->value,
            .end_const = if_br->key,
            .pred = IND_NE,
            .backwards = false
        };
        return true;
    }

    return false;
}

void tb_opt_build_loop_tree(TB_Function* f) {
    cuikperf_region_start("loop tree", NULL);

    TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);
    TB_CFG cfg = tb_compute_rpo(f, f->worklist);
    tb_compute_dominators(f, f->worklist, cfg);

    TB_Node** blocks = tb_arena_alloc(f->tmp_arena, cfg.block_count * sizeof(TB_Node*));
    memcpy(blocks, &f->worklist->items[0], cfg.block_count * sizeof(TB_Node*));
    worklist_clear(f->worklist);

    TB_LoopInfo* loop_list = NULL;

    // canonicalize regions into natural loop headers (or affine loops)
    DynArray(ptrdiff_t) backedges = NULL;
    FOR_N(i, 0, cfg.block_count) {
        TB_Node* header = blocks[i];
        if (!cfg_is_region(header) || header->input_count < 2) { continue; }

        // find all backedges
        dyn_array_clear(backedges);
        FOR_N(j, 0, header->input_count) {
            TB_Node* pred = cfg_get_pred(&cfg, header, j);
            if (slow_dommy(&cfg, header, pred)) { dyn_array_put(backedges, j); }
        }

        // found a loop :)
        if (dyn_array_length(backedges) == 0) { continue; }

        TB_OPTDEBUG(LOOP)(printf("found loop on .bb%zu (v%u) with %zu backedges\n", i, header->gvn, dyn_array_length(backedges)));

        // as part of loop simplification we convert backedges into one, this
        // makes it easier to analyze the exit condition.
        ptrdiff_t single_backedge = backedges[0];
        if (dyn_array_length(backedges) > 1) {
            // TODO(NeGate): i haven't thought about if we care much yet...
            single_backedge = -1;
        }

        // somehow we couldn't simplify the loop? welp
        if (single_backedge >= 0) {
            assert(header->input_count == 2);

            // guarentee that the dominator is inputs[0]
            if (single_backedge == 0) {
                swap_nodes(f, header, 0, 1);
                FOR_USERS(phi, header) {
                    if (USERN(phi)->type == TB_PHI) { swap_nodes(f, USERN(phi), 1, 2); }
                }
                single_backedge = 1;
            }

            TB_NodeTypeEnum type = TB_NATURAL_LOOP;

            // if we don't have the latch in the header
            TB_BasicBlock* header_info = &nl_map_get_checked(cfg.node_to_block, header);

            TB_InductionVar var;
            TB_Node* latch = NULL;

            // if there's a latch on the header, move it to the backedge. also not properly
            // rotated if there's things attached to the backedge cproj, they should've been moved above it.
            TB_Node* exit_proj = get_simple_loop_exit(&cfg, header, header_info->end);
            if (exit_proj && (exit_proj->type != TB_BRANCH_PROJ || exit_proj->inputs[0] != header->inputs[1]->inputs[0] || header->inputs[1]->user_count != 1)) {
                TB_OPTDEBUG(PASSES)(printf("      * Rotating loop %%%u\n", header->gvn));

                latch = header_info->end;
                int exit_loop_i = TB_NODE_GET_EXTRA_T(exit_proj, TB_NodeProj)->index;
                TB_ArenaSavepoint sp2 = tb_arena_save(f->tmp_arena);

                TB_Node** cloned;
                CUIK_TIMED_BLOCK("alloc cloned table") {
                    cloned = tb_arena_alloc(f->tmp_arena, f->node_count * sizeof(TB_Node*));
                    memset(cloned, 0, f->node_count * sizeof(TB_Node*));
                }

                // let's rotate the loop:
                //
                //     header:                    ztc:
                //       i = phi(init, next)        ...
                //       ...                        if (A) header else exit
                //       if (A) body else exit    header:
                //     body:                        i = phi(init, next)
                //       ...                        ...
                //       jmp body                   if (A) header else exit
                //     exit:                      exit:
                //
                // construct the ZTC's version of the branch (same as the original latch but
                // uses the phi's inputs[1] edge instead of the phis directly)
                size_t pre_clone_index = f->node_count;
                TB_Node* ztc_start = header->inputs[0];
                TB_Node *bot_cloned = NULL, *top_cloned = NULL;
                for (TB_Node* curr = latch; curr != header; curr = curr->inputs[0]) {
                    TB_Node* k = loop_clone_node(f, cloned, pre_clone_index, header, curr);
                    mark_users(f, k);

                    // attach control edge
                    if (top_cloned) {
                        set_input(f, top_cloned, k, 0);
                    } else {
                        bot_cloned = k;
                    }
                    top_cloned = k;
                }
                // make a ZTC branch
                set_input(f, top_cloned, ztc_start, 0);
                TB_Node* into_loop = branch_cproj(f, bot_cloned, 90, 0, 1 - exit_loop_i);
                TB_Node* exit_loop = branch_cproj(f, bot_cloned, 10, 0, exit_loop_i);
                mark_node(f, into_loop), mark_node(f, exit_loop);
                // connect up to the loop
                set_input(f, header, into_loop, 0);
                // intercept exit path and place a region (merging the ZTC & rotated loop)
                TB_User* after_exit = cfg_next_user(exit_proj);
                mark_node_n_users(f, USERN(after_exit));
                TB_Node* join = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
                set_input(f, join, exit_loop, 0);
                set_input(f, join, exit_proj, 1);
                set_input(f, USERN(after_exit), join, USERI(after_exit));
                mark_node(f, join);
                // latch gets moved down to the bottom (backedge)
                TB_Node* into_loop2 = USERN(proj_with_index(latch, 1 - exit_loop_i));
                {
                    // latch no longer connected to the top
                    TB_User* after_into_loop2 = cfg_next_user(into_loop2);
                    mark_node_n_users(f, USERN(after_into_loop2));
                    set_input(f, USERN(after_into_loop2), latch->inputs[0], USERI(after_into_loop2));
                    // backedge has a lovely "new" latch
                    set_input(f, latch,  header->inputs[1], 0);
                    set_input(f, header, into_loop2,        1);

                    mark_node_n_users(f, latch);
                    mark_node_n_users(f, header);
                }
                // loads with control deps on the loop's body can also
                // safe once you're guarenteed to run at least once (ZTC)
                hoist_ops(f, into_loop2, into_loop);
                // sometimes there's things leftover which are still dependent on the latch's backedge
                for (size_t i = 0; i < into_loop2->user_count;) {
                    TB_Node* un = USERN(&into_loop2->users[i]);
                    int ui      = USERI(&into_loop2->users[i]);
                    if (ui == 0 && un != header) {
                        set_input(f, un, into_loop, ui);
                    } else {
                        i += 1;
                    }
                }
                // some loop phis escape the loop, we wanna tie these to the exit phis not the
                // loop body phis (since we've constructed two exit paths now).
                CUIK_TIMED_BLOCK("discover loop reachability") {
                    uint32_t* before = tb_arena_alloc(f->tmp_arena, ((f->node_count + 31) / 32) * sizeof(uint32_t));
                    memset(before, 0, ((f->node_count + 31) / 32) * sizeof(uint32_t));

                    // TODO(NeGate): there's probably a faster way to solve for this btw
                    mark_reachable(f, before, header);

                    FOR_USERS(u, header) if (USERN(u)->type == TB_PHI) {
                        assert(USERI(u) == 0);
                        TB_Node* phi = USERN(u);
                        TB_Node* new_phi = tb_alloc_node(f, TB_PHI, phi->dt, 3, 0);
                        set_input(f, new_phi, join,           0);
                        set_input(f, new_phi, phi->inputs[1], 1);
                        set_input(f, new_phi, phi->inputs[2], 2);

                        replace_phis(f, before, phi, new_phi);
                        mark_node(f, new_phi);
                    }
                }
                tb_arena_restore(f->tmp_arena, sp2);

                TB_OPTDEBUG(PASSES)(printf("        * Added extra latch %%%u\n", latch->gvn));
                TB_OPTDEBUG(PASSES)(printf("        * Added extra join %%%u\n", join->gvn));
            } else {
                // the loop is already rotated if there's a latch at the bottom, maybe
                // it's marked, maybe it's not.
                if (header->inputs[1]->type == TB_BRANCH_PROJ) {
                    TB_Node* exit_proj = get_simple_loop_exit(&cfg, header, header->inputs[1]->inputs[0]);
                    if (exit_proj) {
                        TB_OPTDEBUG(PASSES)(printf("      * Found rotated loop %%%u\n", header->gvn));
                        latch = exit_proj->inputs[0];
                    }
                }
            }

            if (latch != NULL) {
                TB_InductionVar var;
                if (find_latch_indvar(header, latch, &var)) {
                    type = TB_AFFINE_LOOP;

                    uint64_t* init = iconst(var.phi->inputs[1]);
                    uint64_t* end  = var.end_cond ? iconst(var.end_cond) : &var.end_const;

                    #if TB_OPTDEBUG_LOOP
                    TB_Node *phi = var.phi, *cond = var.cond;
                    int64_t step = var.step;

                    if (init) {
                        printf("  affine loop: %%%u = %"PRId64"*x + %"PRId64"\n", phi->gvn, step, *init);
                    } else {
                        printf("  affine loop: %%%u = %"PRId64"*x + %%%u\n", phi->gvn, step, phi->inputs[1]->gvn);
                    }

                    if (end) {
                        printf("        latch: %s(%%%u, %"PRId64, ind_pred_names[var.pred], phi->gvn, *end);
                    } else {
                        printf("        latch: %s(%%%u, %%%u", ind_pred_names[var.pred], phi->gvn, var.end_cond->gvn);
                    }

                    if (var.backwards) {
                        printf(", flipped");
                    }
                    printf(")\n");

                    // fixed trip count loops aren't *uncommon*
                    if (init && end) {
                        int64_t trips = (*end - *init) / step;
                        int64_t rem   = (*end - *init) % step;
                        if (rem != 0 && var.pred == IND_NE) {
                            printf("        trips: overshoot\n");
                        } else {
                            printf("        trips: %"PRId64" (%"PRId64" ... %"PRId64")\n", trips, *init, *end);
                        }
                    }
                    #endif
                }
            }

            if (header->type != type) {
                header->type = type;
                mark_node_n_users(f, header);

                // all affine loops have an affine latch
                if (type == TB_AFFINE_LOOP) {
                    header->inputs[1]->inputs[0]->type = TB_AFFINE_LATCH;
                    mark_node_n_users(f, header->inputs[1]->inputs[0]);
                }

                // ok cool, more loops
                TB_LoopInfo* new_loop = tb_arena_alloc(f->arena, sizeof(TB_LoopInfo));
                *new_loop = (TB_LoopInfo){
                    .next      = loop_list,
                    .header    = header,
                };
                loop_list = new_loop;
            }
        }
    }
    tb_free_cfg(&cfg);
    f->loop_list = loop_list;

    tb_arena_restore(f->tmp_arena, sp);
    cuikperf_region_end();
}

void tb_opt_loops(TB_Function* f) {
    #if 0
    cuikperf_region_start("loop", NULL);
    tb_print_dumb(f, false);

    for (TB_LoopInfo *loop = f->loop_list; loop; loop = loop->next) {
        if (loop->header->type == TB_AFFINE_LOOP) {
            TB_InductionVar var;
            bool has_latch_iv = false;
            TB_Node* latch = affine_loop_latch(loop->header);
            if (latch && find_latch_indvar(loop->header, latch, &var)) {
                has_latch_iv = true;
            }

            for (size_t i = 0; i < loop->header->user_count; i++) {
                TB_Node* phi = USERN(&loop->header->users[i]);
                if (USERI(&loop->header->users[i]) != 0) { continue; }

                // stepping induction var? ok cool, we actually care for these
                uint64_t* step = find_affine_indvar(phi, loop->header);
                if (step) {
                    TB_OPTDEBUG(PASSES)(printf("      * Found IV: %%%u = %"PRId64"*trips + %%%u\n", phi->gvn, *step, phi->inputs[1]->gvn));
                    if (indvar_simplify(f, phi, phi->inputs[2], has_latch_iv ? &var : NULL)) {
                        TB_OPTDEBUG(PASSES)(printf("      * Simplified induction var on phi %%%d\n", var.phi->gvn));
                    }
                }
            }
        }
    }

    tb_print_dumb(f, false);
    cuikperf_region_end();
    #endif
}
