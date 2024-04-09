
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
static TB_Node* loop_clone_node(TB_Function* f, TB_Node* header, TB_Node* n) {
    TB_Node* cloned = n;
    if (n->type == TB_PHI) {
        if (n->inputs[0] == header) {
            // replace OG with loop phi's initial value
            cloned = n->inputs[1];
        }
    } else if (cfg_is_region(n) || n->type == TB_ROOT || (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT)) {
        // doesn't clone
    } else {
        size_t extra = extra_bytes(n);
        cloned = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);

        // clone extra data (i hope it's that easy lol)
        memcpy(cloned->extra, n->extra, extra);

        // mark new node
        mark_node(f, cloned);

        // fill cloned edges
        FOR_N(i, 1, n->input_count) {
            TB_Node* in = loop_clone_node(f, header, n->inputs[i]);
            cloned->inputs[i] = in;
            add_user(f, cloned, in, i);
        }

        if (n->inputs[0]) { cloned = tb__gvn(f, cloned, extra); }
    }

    #if TB_OPTDEBUG_LOOP
    printf("CLONE: ");
    tb_print_dumb_node(NULL, n);
    printf(" => ");
    tb_print_dumb_node(NULL, cloned);
    printf("\n");
    #endif

    return cloned;
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

static void replace_phis(TB_Function* f, TB_Node* phi) {
    for (size_t i = 0; i < phi->user_count;) {
        TB_Node* un = USERN(&phi->users[i]);
        int ui      = USERI(&phi->users[i]);
        if (un != phi && un != phi->inputs[2]) {
            set_input(f, un, phi->inputs[2], ui);
        } else {
            i += 1;
        }
    }
}

typedef struct {
    int leftovers;
    int uses;
    TB_DataType dt;
} UseCast;

static void indvar_scan_uses(TB_Function* f, UseCast* best, TB_Node* phi, TB_Node* cond, TB_Node* op, int steps) {
    FOR_USERS(u, op) {
        TB_Node* use_n = USERN(u);

        if (use_n == phi) continue;
        if (use_n == cond) continue;
        if (use_n == op) continue;

        if (use_n->type == TB_ADD || use_n->type == TB_SUB) {
            // arithmetic is something we can walk past
            if (steps < 2) {
                indvar_scan_uses(f, best, phi, cond, use_n, steps + 1);
            } else {
                best->leftovers += 1;
            }
        } else if (use_n->type == TB_SIGN_EXT || use_n->type == TB_ZERO_EXT) {
            // if we've got any casts, let's try to see which is most common
            int node_uses = USERN(u)->user_count;
            if (node_uses > best->uses) {
                best->uses = node_uses;
                best->dt = use_n->dt;
            }
        } else {
            best->leftovers += 1;
        }
    }
}

static bool indvar_simplify(TB_Function* f, TB_Node* phi, TB_Node* cond, TB_Node* op, TB_Node* limit) {
    assert(op->dt.raw == phi->dt.raw);
    UseCast best = { 0, 0, op->dt };

    indvar_scan_uses(f, &best, phi, cond, op, 0);
    if (best.uses < best.leftovers || best.dt.raw == phi->dt.raw) {
        return false;
    }

    TB_DataType old_dt = phi->dt;

    // induction var can trivially scale up, the leftover users can
    // use the truncate
    phi->dt = best.dt;
    op->dt  = best.dt;
    latuni_set(f, op,  NULL);
    latuni_set(f, phi, NULL);

    // upscale init
    TB_Node* scaled_init = tb_alloc_node(f, TB_ZERO_EXT, best.dt, 2, 0);
    set_input(f, scaled_init, phi->inputs[1], 1);
    set_input(f, phi, scaled_init, 1);
    mark_node(f, scaled_init);

    // upscale limit
    if (limit != NULL) {
        // TODO(NeGate): does signedness matter here?
        TB_Node* scaled_limit = tb_alloc_node(f, TB_ZERO_EXT, best.dt, 2, 0);
        set_input(f, scaled_limit, limit, 1);
        mark_node(f, scaled_limit);

        assert(cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE);
        TB_NODE_SET_EXTRA(cond, TB_NodeCompare, .cmp_dt = best.dt);

        // if the limit is on the left, the add+phi are on the right (and vice versa)
        if (cond->inputs[1] == limit) {
            set_input(f, cond, scaled_limit, 1);
        } else {
            assert(cond->inputs[2] == limit);
            set_input(f, cond, scaled_limit, 2);
        }
    }

    TB_Node* phi_cast = tb_alloc_node(f, TB_TRUNCATE, old_dt, 2, 0);
    mark_node(f, phi_cast);

    // replace all uses with a different size with a dumb truncate (peeps will clean up most likely)
    for (size_t i = 0; i < phi->user_count;) {
        TB_Node* un = USERN(&phi->users[i]);
        int ui      = USERI(&phi->users[i]);
        if (un == op || un == phi || un == cond) { i += 1; continue; }
        set_input(f, un, phi_cast, ui);
    }
    set_input(f, phi_cast, phi, 1);
    mark_users(f, phi_cast);

    TB_Node* op_cast = tb_alloc_node(f, TB_TRUNCATE, old_dt, 2, 0);
    mark_node(f, op_cast);

    for (size_t i = 0; i < op->user_count;) {
        TB_Node* un = USERN(&op->users[i]);
        int ui      = USERI(&op->users[i]);
        if (un == op || un == phi || un == cond) { i += 1; continue; }
        set_input(f, un, op_cast, ui);
    }
    set_input(f, op_cast, op, 1);
    mark_users(f, op_cast);

    mark_node_n_users(f, op);
    mark_node_n_users(f, phi);
    return true;
}

// only case we handle rn is making a bigger stride when we proved the induction var never goes past
static bool indvar_strength_reduction(TB_Function* f, TB_Node* phi, TB_Node* cond, TB_Node* end_cond, TB_Node* op) {
    uint64_t* init = iconst(phi->inputs[1]);
    uint64_t* end = end_cond ? iconst(end_cond) : NULL;

    if (init && end) {
        tb_todo();
    }

    return false;
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

static bool find_indvar(TB_Node* header, TB_Node* latch, TB_InductionVar* restrict var) {
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
                latch = header_info->end;
                int exit_loop_i = TB_NODE_GET_EXTRA_T(exit_proj, TB_NodeProj)->index;

                // let's rotate the loop:
                //
                //     header:                    latch:
                //       ...                        ...
                //       if (A) body else exit      if (A) header else exit
                //     body:                      header:
                //       ...                        ...
                //       jmp body                   if (A) header else exit
                //     exit:                      exit:
                //
                // construct the ZTC's version of the branch (same as the original latch but
                // uses the phi's inputs[1] edge instead of the phis directly)
                TB_Node* ztc_start = header->inputs[0];
                TB_Node *bot_cloned = NULL, *top_cloned = NULL;
                for (TB_Node* curr = latch; curr != header; curr = curr->inputs[0]) {
                    TB_Node* cloned = loop_clone_node(f, header, curr);
                    mark_users(f, cloned);

                    // attach control edge
                    if (top_cloned) {
                        set_input(f, top_cloned, cloned, 0);
                    } else {
                        bot_cloned = cloned;
                    }
                    top_cloned = cloned;
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
                set_input(f, join, exit_proj, 0);
                set_input(f, join, exit_loop, 1);
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
                // any operations dependent on our phis will now depend on the backedge form
                FOR_USERS(u, header) if (USERN(u)->type == TB_PHI) {
                    replace_phis(f, USERN(u));
                }
            } else {
                // the loop is already rotated if there's a latch at the bottom, maybe
                // it's marked, maybe it's not.
                if (header->inputs[1]->type == TB_BRANCH_PROJ) {
                    TB_Node* exit_proj = get_simple_loop_exit(&cfg, header, header->inputs[1]->inputs[0]);
                    if (exit_proj) {
                        latch = exit_proj->inputs[0];
                    }
                }
            }

            if (latch != NULL) {
                TB_InductionVar var;
                if (find_indvar(header, latch, &var)) {
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
                    .exit_proj = exit_proj,
                };
                loop_list = new_loop;
            }
        }
    }
    f->loop_list = loop_list;

    tb_arena_restore(f->tmp_arena, sp);
    cuikperf_region_end();
}

void tb_opt_loops(TB_Function* f) {
    cuikperf_region_start("loop", NULL);

    // find loops with affine induction vars
    for (TB_LoopInfo *loop = f->loop_list; loop; loop = loop->next) {
        if (loop->header->type == TB_AFFINE_LOOP) {
            TB_Node* latch = affine_loop_latch(loop->header);

            TB_InductionVar var;
            if (latch && find_indvar(loop->header, latch, &var)) {
                // we'll just run these alongside the loop detection but maybe they
                // should be a separate pass?
                if (indvar_simplify(f, var.phi, var.cond, var.phi->inputs[2], var.end_cond)) {
                    TB_OPTDEBUG(PASSES)(printf("      * Simplified induction var on phi %%%d\n", var.phi->gvn));
                }
            }
        }
    }

    cuikperf_region_end();
}
