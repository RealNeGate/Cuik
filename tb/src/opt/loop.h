
// clone anything except control edges and phis to region
static TB_Node* loop_clone_node(TB_Function* f, TB_Node* region, TB_Node* n, int phi_index) {
    TB_Node* cloned = n;
    if (n->type == TB_PHI && n->inputs[0] == region) {
        // replace OG with phi's edge
        cloned = n->inputs[phi_index];
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
            TB_Node* in = loop_clone_node(f, region, n->inputs[i], phi_index);
            cloned->inputs[i] = in;
            add_user(f, cloned, in, i);
        }

        if (n->inputs[0]) { cloned = tb__gvn(f, cloned, extra); }
    }

    #if TB_OPTDEBUG_LOOP
    printf("CLONE: ");
    print_node_sexpr(n, 0);
    printf(" => ");
    print_node_sexpr(cloned, 0);
    printf("\n");
    #endif

    return cloned;
}

static uint64_t* iconst(TB_Node* n) {
    return n->type == TB_INTEGER_CONST ? &TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value : NULL;
}

static void swap_nodes(TB_Function* f, TB_Node* n, int i, int j) {
    TB_Node* old = n->inputs[i];
    set_input(f, n, n->inputs[j], i);
    set_input(f, n, old, j);
}

static TB_Node* upcast(TB_Function* f, TB_Node* src, TB_DataType dt) {
    TB_Node* cast = tb_alloc_node(f, TB_ZERO_EXT, dt, 2, 0);
    set_input(f, cast, src, 1);
    mark_node(f, cast);
    return cast;
}

static bool indvar_simplify(TB_Function* f, TB_Node* phi, TB_Node* cond, TB_Node* op) {
    TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);
    ArenaArray(TB_User) uses = aarray_create(f->tmp_arena, TB_User, phi->user_count);

    // -1 means signed, 1 means unsigned, 0 means don't care
    int best_uses = 0;
    TB_DataType best_dt = phi->dt;

    // in the simple case we wanna see if our induction var is usually casted
    int phi_uses = 0;
    FOR_USERS(u, phi) {
        TB_Node* use_n = USERN(u);
        aarray_push(uses, *u);

        if (use_n == phi) continue;
        if (use_n == cond) continue;
        if (use_n == op) continue;

        int node_uses = 0;
        FOR_USERS(u2, USERN(u)) { node_uses += 1; }

        // if we've got any casts, let's try to see which is most common
        if (use_n->type == TB_SIGN_EXT || use_n->type == TB_ZERO_EXT) {
            if (node_uses > best_uses) {
                best_uses = node_uses;
                best_dt = use_n->dt;
            }
        } else {
            phi_uses += 1;
        }
    }

    if (best_uses <= phi_uses) {
        tb_arena_restore(f->tmp_arena, sp);
        return false;
    }

    // replace all uses with a different size
    TB_Node* cast = tb_alloc_node(f, TB_TRUNCATE, phi->dt, 2, 0);
    set_input(f, cast, phi, 1);
    mark_node(f, cast);

    size_t new_node_mark = f->node_count;
    aarray_for(i, uses) {
        TB_Node* use_n = USERN(&uses[i]);
        int use_i      = USERI(&uses[i]);

        if (use_n == cond && cond != phi) {
            assert(use_i == 1);
            TB_NODE_SET_EXTRA(cond, TB_NodeCompare, .cmp_dt = best_dt);

            // upcast the other comparand
            TB_Node* b_cast = upcast(f, cond->inputs[2], best_dt);
            set_input(f, cond, b_cast, 2);
            mark_users(f, cond);
            mark_node(f, cond);
        } else if (use_n == op) {
            assert(use_i == 1);
            op->dt = best_dt;
            latuni_set(f, op, NULL);

            TB_Node* b_cast = upcast(f, op->inputs[2], best_dt);
            set_input(f, op, b_cast, 2);
            mark_users(f, op);
            mark_node(f, op);
        } else if (use_n != cast) {
            if (use_n->dt.raw == best_dt.raw && (use_n->type == TB_ZERO_EXT || use_n->type == TB_SIGN_EXT)) {
                // no cast necessary
                mark_users(f, use_n);
                subsume_node2(f, use_n, phi);
            } else {
                // generic upgrade
                set_input(f, use_n, cast, use_i);
                mark_users(f, use_n);
            }
        }
    }

    if (cast->users == NULL) {
        tb_kill_node(f, cast);
    } else {
        mark_users(f, cast);
    }

    // upscale init var
    TB_Node* init_cast = upcast(f, phi->inputs[1], best_dt);
    set_input(f, phi, init_cast, 1);
    mark_node(f, phi);

    phi->dt = best_dt;
    latuni_set(f, phi, NULL);
    tb_arena_restore(f->tmp_arena, sp);
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
    if (latch->type != TB_BRANCH || TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
        return NULL;
    }

    // for this latch to count it needs to exit (have a path not dominated
    // by the loop's backedge)
    TB_Node* exit = NULL;
    TB_Node* backedge_bb = cfg_get_pred(cfg, header, 1);
    FOR_USERS(u, latch) {
        if (USERN(u)->type != TB_PROJ) { continue; }
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

// i = phi(init, i + step) where step is constant.
static bool affine_indvar(TB_Node* n, TB_Node* header) {
    return n->type == TB_PHI && n->dt.type == TB_INT
        && n->inputs[0] == header && n->inputs[2]->type == TB_ADD
        && n->inputs[2]->inputs[1] == n
        && n->inputs[2]->inputs[2]->type == TB_INTEGER_CONST;
}

static bool find_indvar(TB_Node* header, TB_Node* latch, TB_Node* exit, TB_InductionVar* restrict var) {
    bool exit_when_key = TB_NODE_GET_EXTRA_T(exit, TB_NodeProj)->index;
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
        if (affine_indvar(a, header))      { indvar = a, limit = b; }
        else if (affine_indvar(b, header)) { indvar = b, limit = a, backwards = true; }

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

            // given the induction var exists and is representable by an affine function we'll reorder things
            // such that it's the first backedge in the header's preds (inputs[1]) alongside making the header
            // into a TB_AFFINE_LOOP.
            //
            // if we don't have the latch in the header BB... ngmi
            TB_BasicBlock* header_info = &nl_map_get_checked(cfg.node_to_block, header);

            TB_InductionVar var;
            TB_Node* exit_proj = get_simple_loop_exit(&cfg, header, header_info->end);
            if (exit_proj && find_indvar(header, header_info->end, exit_proj, &var)) {
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

            if (header->type != type) {
                header->type = type;

                // ok cool, more loops
                TB_LoopInfo* new_loop = tb_arena_alloc(f->arena, sizeof(TB_LoopInfo));
                *new_loop = (TB_LoopInfo){
                    .next   = loop_list,
                    .header = header,
                    .latch  = header_info->end,
                    .exit   = exit_proj,
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

    // ind var simplification & loop deletion
    for (TB_LoopInfo *prev = NULL, *loop = f->loop_list; loop; prev = loop, loop = loop->next) {
        if (loop->header->type != TB_AFFINE_LOOP) { continue; }

        TB_InductionVar var;
        if (find_indvar(loop->header, loop->latch, loop->exit, &var)) {
            uint64_t* init = iconst(var.phi->inputs[1]);
            uint64_t* end  = var.end_cond ? iconst(var.end_cond) : &var.end_const;

            if (init && end) {
                int64_t trips = (*end - *init) / var.step;
                if (trips <= 1) {
                    // the backedge is useless, remove input & jump out at the end
                    assert(loop->header->input_count == 2);
                    TB_Node* backedge = loop->header->inputs[1];
                    set_input(f, loop->header, NULL, 1);
                    loop->header->type = TB_REGION;
                    loop->header->input_count = 1;
                    // remove phis
                    for (size_t i = 0; i < loop->header->user_count;) {
                        TB_User* use = &loop->header->users[i];
                        if (USERN(use)->type == TB_PHI) {
                            assert(USERI(use) == 0);
                            assert(USERN(use)->input_count == 3);
                            subsume_node(f, USERN(use), USERN(use)->inputs[1]);
                        } else {
                            i += 1;
                        }
                    }
                    TB_Node* succ = cfg_next_control(loop->exit);
                    TB_Node* join = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
                    set_input(f, join, loop->exit, 0);
                    set_input(f, join, backedge,   1);

                    // intercept exit path with a newly made region
                    if (cfg_is_region(succ)) {
                        FOR_N(i, 0, succ->input_count) if (succ->inputs[i] == loop->exit) {
                            set_input(f, succ, join, i);
                            break;
                        }
                    } else {
                        set_input(f, succ, join, 0);
                    }

                    // get rid of it
                    if (prev) { prev->next = loop->next; }
                    else { f->loop_list = loop->next; }

                    // peeps clean up later (namely stacked region collapsing the new region away)
                    mark_users(f, join);
                    mark_node(f, join);
                    mark_users(f, backedge);
                    mark_node(f, backedge);
                    mark_users(f, loop->header);
                    mark_node(f, loop->header);
                }
            }

            // we'll just run these alongside the loop detection but maybe they
            // should be a separate pass?
            /*if (indvar_simplify(f, var.phi, var.cond, var.phi->inputs[2])) {
                TB_OPTDEBUG(PASSES)(printf("      * Simplified induction var on phi %%%d\n", var.phi->gvn));
            }*/
        }
    }

    cuikperf_region_end();
}
