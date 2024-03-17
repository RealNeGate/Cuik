
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
            add_user(f, cloned, in, i, NULL);
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
    ArenaArray(TB_User) uses = aarray_create(f->tmp_arena, TB_User, 16);

    // -1 means signed, 1 means unsigned, 0 means don't care
    int best_uses = 0;
    TB_DataType best_dt = phi->dt;

    // in the simple case we wanna see if our induction var is usually casted
    int phi_uses = 0;
    FOR_USERS(u, phi) {
        TB_Node* use_n = USERN(u);
        aarray_push(uses, u);

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
        TB_Node* use_n = USERN(uses[i]);
        int use_i      = USERI(uses[i]);

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

void tb_opt_loops(TB_Function* f) {
    cuikperf_region_start("loop", NULL);

    TB_CFG cfg = tb_compute_rpo(f, f->worklist);
    tb_compute_dominators(f, f->worklist, cfg);

    size_t block_count = cfg.block_count;
    TB_Node** blocks = &f->worklist->items[0];

    // canonicalize regions into natural loop headers (or affine loops)
    DynArray(ptrdiff_t) backedges = NULL;
    FOR_N(i, 0, block_count) {
        TB_Node* header = blocks[i];
        if (!cfg_is_region(header) || header->input_count < 2) {
            continue;
        }

        // find all backedges
        dyn_array_clear(backedges);
        FOR_N(j, 0, header->input_count) {
            TB_Node* pred = cfg_get_pred(&cfg, header, j);
            if (slow_dommy(&cfg, header, pred)) {
                dyn_array_put(backedges, j);
            }
        }

        // found a loop :)
        if (dyn_array_length(backedges) == 0) {
            continue;
        }

        TB_OPTDEBUG(LOOP)(printf("found loop on .bb%zu (v%u) with %zu backedges\n", i, header->gvn, dyn_array_length(backedges)));
        ptrdiff_t single_backedge = backedges[0];

        // as part of loop simplification we convert backedges into one, this
        // makes it easier to analyze the exit condition.
        if (dyn_array_length(backedges) > 1) {
            // TODO(NeGate): i haven't thought about if we care much yet...
        }

        // somehow we couldn't simplify the loop? welp
        if (single_backedge < 0 || header->input_count != 2) {
            continue;
        }

        // guarentee that the dominator is inputs[0]
        if (single_backedge == 0) {
            swap_nodes(f, header, 0, 1);
            FOR_USERS(phi, header) {
                if (USERN(phi)->type == TB_PHI) { swap_nodes(f, USERN(phi), 1, 2); }
            }
            single_backedge = 1;
        }
        header->type = TB_NATURAL_LOOP;

        // given the induction var exists and is representable by an affine function we'll reorder things
        // such that it's the first backedge in the header's preds (inputs[1]) alongside making the header
        // into a TB_AFFINE_LOOP.
        //
        // if we don't have the latch in the header BB... ngmi
        TB_BasicBlock* header_info = &nl_map_get_checked(cfg.node_to_block, header);
        TB_Node* latch = header_info->end;
        if (latch->type != TB_BRANCH && TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
            break;
        }

        TB_Node* ind_var = NULL;
        TB_Node* cond = latch->inputs[1];
        TB_Node* end_cond = NULL;
        TB_Node* phi = cond;

        // detect induction var for affine loops (only valid if it's a phi on the header)
        if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE &&
            cond->inputs[1]->type == TB_PHI && cond->inputs[1]->inputs[0] == header) {
            phi = cond->inputs[1];
            end_cond = cond->inputs[2];
        }

        // affine loop's induction var should loop like:
        //   i = phi(init, i + step) where step is constant.
        if (phi->inputs[0] == header && phi->dt.type == TB_INT) {
            TB_Node* op = phi->inputs[2];
            if ((op->type == TB_ADD || op->type == TB_SUB) && op->inputs[2]->type == TB_INTEGER_CONST) {
                // we're a real affine loop now!
                header->type = TB_AFFINE_LOOP;

                int64_t step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value;
                if (phi->inputs[2]->type == TB_SUB) {
                    step = -step;
                }

                uint64_t* init = iconst(phi->inputs[1]);
                uint64_t* end = end_cond ? iconst(end_cond) : NULL;

                if (0 && init && end) {
                    int64_t trips = (*end - *init) / step;
                    if (cond->type == TB_CMP_SLT || cond->type == TB_CMP_ULT) {
                        // if we know the right number of trips we can just append that to the value info
                        // TODO(NeGate): we should be using a JOIN op to narrow.
                        Lattice* range = lattice_gimme_uint(f, *init, *init + trips*step);

                        mark_node(f, phi);
                        if (latuni_set_progress(f, phi, range)) { mark_users(f, phi); }
                    }
                }

                #if TB_OPTDEBUG_LOOP
                if (init) {
                    printf("  affine loop: v%u = %"PRId64"*x + %"PRId64"\n", phi->gvn, step, *init);
                } else {
                    printf("  affine loop: v%u = %"PRId64"*x + v%u\n", phi->gvn, step, phi->inputs[1]->gvn);
                }

                if (end) {
                    printf("        latch: %s(v%u, %"PRId64")\n", tb_node_get_name(cond), phi->gvn, *end);
                } else {
                    printf("        latch: %s(v%u, v%u)\n", tb_node_get_name(cond), phi->gvn, end_cond->gvn);
                }

                // fixed trip count loops aren't *uncommon*
                if (init && end) {
                    printf("        trips: %"PRId64" (%"PRId64" ... %"PRId64")\n", (*end - *init) / step, *init, *end);
                }
                #endif

                // we'll just run these alongside the loop detection but maybe they
                // should be a separate pass?
                indvar_simplify(f, phi, cond, op);
                // indvar_strength_reduction(p, phi, cond, end_cond, op);
            }
        }
    }
    dyn_array_destroy(backedges);
    tb_free_cfg(&cfg);

    #if 0
    // find & canonicalize loops
    DynArray(ptrdiff_t) backedges = NULL;
    FOR_N(i, 0, block_count) {
        TB_Node* header = blocks[i];
        if (header->type != TB_REGION || header->input_count < 2) {
            continue;
        }

        // find all backedges
        dyn_array_clear(backedges);
        FOR_N(j, 0, header->input_count) {
            TB_Node* pred = cfg_get_pred(&cfg, header, j);
            if (slow_dommy(&cfg, header, pred)) {
                dyn_array_put(backedges, j);
            }
        }

        // found a loop :)
        if (dyn_array_length(backedges) == 0) {
            continue;
        }

        TB_OPTDEBUG(LOOP)(printf("found loop on .bb%zu with %zu backedges\n", i, dyn_array_length(backedges)));
        TB_NODE_GET_EXTRA_T(header, TB_NodeRegion)->freq = 10.0f;
        ptrdiff_t single_backedge = backedges[0];

        // as part of loop simplification we convert backedges into one, this
        // makes it easier to analyze the exit condition.
        if (dyn_array_length(backedges) > 1) {
            tb_todo();
        }

        // somehow we couldn't simplify the loop? welp
        if (single_backedge < 0 || header->input_count != 2) {
            continue;
        }

        // canonicalize loop
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(header);
        r->natty = true;
        if (single_backedge == 0) {
            SWAP(TB_Node*, header->inputs[0], header->inputs[1]);
            FOR_USERS(phi, header) {
                if (phi->n->type == TB_PHI) {
                    SWAP(TB_Node*, phi->n->inputs[1], phi->n->inputs[2]);
                }
            }
            single_backedge = 1;
        }

        // if it's already rotated don't do it again
        /* if (header->inputs[single_backedge]->type == TB_PROJ && header->inputs[single_backedge]->inputs[0]->type == TB_BRANCH) {
            continue;
        } */

        // if we don't have the latch in the header BB... ngmi
        TB_BasicBlock* header_info = &nl_map_get_checked(cfg.node_to_block, header);
        TB_Node* latch = header_info->end;
        if (latch->type != TB_BRANCH && TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
            break;
        }

        // detect induction var for affine loops (only valid if it's a phi on the header)
        TB_Node* ind_var = NULL;
        TB_Node* cond = latch->inputs[1];
        TB_Node* end_cond = NULL;
        TB_Node* phi = cond;

        if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE && cond->inputs[1]->type == TB_PHI) {
            phi = cond->inputs[1];
            end_cond = cond->inputs[2];
        }

        // affine loop's induction var should loop like:
        //
        //   i = phi(init, i + step) where step is constant.
        if (phi->inputs[0] == header && phi->dt.type == TB_INT) {
            TB_Node* op = phi->inputs[2];
            if ((op->type == TB_ADD || op->type == TB_SUB) && op->inputs[2]->type == TB_INTEGER_CONST) {
                int64_t step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value;
                if (phi->inputs[2]->type == TB_SUB) {
                    step = -step;
                }

                uint64_t* init = iconst(phi->inputs[1]);
                if (init) {
                    TB_OPTDEBUG(LOOP)(printf("  affine loop: v%u = %"PRId64"*x + %"PRId64"\n", phi->gvn, step, *init));
                } else {
                    TB_OPTDEBUG(LOOP)(printf("  affine loop: v%u = %"PRId64"*x + v%u\n", phi->gvn, step, phi->inputs[1]->gvn));
                }

                __debugbreak();

                #if 0
                // track the kinds of casts/ops users do and then see who's the winner
                int cnt = 0;
                bool legal_scale = true;

                int keys[4] = { -1, -1, -1, -1 };
                int uses[4] = { 0 };
                uint32_t use_signed = 0;
                TB_DataType use_dt[4];

                IndVar iv = {
                    .users = nl_chunked_arr_alloc(tmp_arena)
                };

                indvar_push(&indvar, phi);

                // find the best option
                for (NL_ArrChunk* restrict chk = iv.users; chk; chk = chk->next) {
                    FOR_N(i, 0, chk->count) {
                        TB_Node* use = chk->elems[i];
                        __debugbreak();
                    }
                }

                nl_chunked_arr_reset(&projs);

                FOR_USERS(u, phi) {
                    if (u->n == op) continue;
                    if (u->n == cond) continue;
                    if (u->n == latch) continue;

                    // look past the casts, we do wanna know if we benefit from using a
                    // before type tho.
                    if (u->n->type == TB_SIGN_EXT || u->n->type == TB_ZERO_EXT) {
                        is_signed = u->n->type == TB_SIGN_EXT;
                        stride = 1;
                        dt = u->n->dt;
                    }

                    bool is_signed = false;
                    ptrdiff_t stride = 0;
                    TB_DataType dt = phi->dt;
                    if (u->n->type == TB_ARRAY_ACCESS) {
                        stride = TB_NODE_GET_EXTRA_T(u->n, TB_NodeArray)->stride;
                    } else if (u->n->type == TB_SIGN_EXT || u->n->type == TB_ZERO_EXT) {
                        is_signed = u->n->type == TB_SIGN_EXT;
                        stride = 1;
                        dt = u->n->dt;
                    } else {
                        TB_OPTDEBUG(LOOP)(printf("  * cannot scale because we don't understand %s\n", tb_node_get_name(u->n)));

                        legal_scale = false;
                        break;
                    }

                    int node_uses = 0;
                    FOR_USERS(u2, u->n) node_uses += 1;

                    TB_OPTDEBUG(LOOP)(printf("  * scaled by %tu (with %d uses)\n", stride, node_uses));

                    // track uses of ARRAY
                    int i = 0;
                    for (; i < cnt; i++) {
                        if (keys[i] == stride) break;
                    }

                    if (i == cnt) {
                        if (cnt == 4) break;
                        keys[cnt++] = stride;
                    }

                    use_signed |= 1 << i;
                    use_dt[i] = u->n->dt;
                    uses[i] += node_uses;
                }

                // TODO(NeGate): support figuring out scaling when things
                // get wacky (*2 vs *6, if we pick the larger number it might
                // be more useful but also we'd need to div3 to make this work
                // which isn't worth it)
                if (legal_scale && cnt == 1) {
                    // choose high use step to scale by
                    int biggest = 0, big_use = uses[0];
                    FOR_N(i, 1, cnt) if (uses[i] > big_use) {
                        big_use = uses[i];
                        biggest = i;
                    }

                    int best_stride = keys[biggest];

                    // rescale the step
                    TB_Node* scale_n = make_int_node(f, p, phi->dt, best_stride);
                    set_input(f, op, scale_n, 2);
                    mark_node(f, op);
                    mark_node(f, scale_n);

                    // rescale the limit
                    if (cond->type != TB_PHI) {
                        assert(cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE);
                        assert(cond->inputs[1] == phi);

                        TB_Node* limit_scaled = tb_alloc_node(f, TB_MUL, phi->dt, 3, sizeof(TB_NodeBinopInt));
                        set_input(f, limit_scaled, cond->inputs[2], 1);
                        set_input(f, limit_scaled, scale_n,         2);
                        TB_NODE_GET_EXTRA_T(limit_scaled, TB_NodeBinopInt)->ab = 0;

                        set_input(f, cond, limit_scaled, 2);

                        mark_node(f, cond);
                        mark_node(f, limit_scaled);
                    }

                    FOR_USERS(u, phi) {
                        TB_Node* use = u->n;

                        // we'll just scale each stride down
                        if (!worklist_test_n_set(&p->worklist, use)) {
                            dyn_array_put(p->worklist.items, use);

                            if (use->type == TB_ARRAY_ACCESS) {
                                TB_NodeArray* arr = TB_NODE_GET_EXTRA(use);
                                arr->stride /= best_stride;
                            } else {
                                tb_todo();
                            }
                        }
                    }
                }
                #endif
            }
        }

        if (0) {
            TB_Node* backedge_bb = cfg_get_pred(&cfg, header, single_backedge);

            // check which paths lead to exitting the loop (don't dominate the single backedge)
            int exit_proj_i = -1;
            TB_Node* projs[2];
            for (User* u = latch->users; u; u = u->next) {
                if (u->n->type == TB_PROJ) {
                    TB_Node* succ = cfg_next_bb_after_cproj(u->n);

                    int proj_i = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
                    projs[proj_i] = u->n;

                    if (!fast_dommy(succ, backedge_bb)) {
                        // if we have multiple exit latches... just leave
                        if (exit_proj_i >= 0) {
                            exit_proj_i = -1;
                            break;
                        }

                        exit_proj_i = proj_i;
                    } else {
                        // body must not have any PHIs
                        if (cfg_has_phis(succ)) {
                            exit_proj_i = -1;
                            break;
                        }
                    }
                }
            }

            // infinite loop? yikes
            if (exit_proj_i < 0) {
                continue;
            }

            // update latch before killing it (we want it's current users to know shit's
            // in motion)
            mark_users(p, latch);

            // convert to rotated loops
            //
            //     header:                    latch:
            //       ...                        ...
            //       if (A) body else exit      if (A) body else exit
            //     body:                      body:
            //       ...                        ...
            //       jmp body                   if (A) body else exit
            //     exit:                      exit:
            //
            // clone latch and intercept the backedge (making sure to replace all phis with
            // the value that's not in the body)
            TB_Node* exit_region;
            {
                int init_edge = 1 - single_backedge;
                TB_Node *bot_cloned = NULL, *top_cloned = NULL;
                for (TB_Node* curr = latch; curr != header; curr = curr->inputs[0]) {
                    TB_Node* cloned = loop_clone_node(p, f, header, curr, 1 + init_edge);
                    mark_users(p, cloned);

                    // attach control edge
                    if (top_cloned) {
                        set_input(f, top_cloned, cloned, 0);
                    } else {
                        bot_cloned = cloned;
                    }
                    top_cloned = cloned;
                }

                // create cprojs
                assert(bot_cloned->type == TB_BRANCH);
                TB_Node* proj0 = make_proj_node(f, TB_TYPE_CONTROL, bot_cloned, 0);
                TB_Node* proj1 = make_proj_node(f, TB_TYPE_CONTROL, bot_cloned, 1);

                mark_node(f, proj0);
                mark_node(f, proj1);

                // add zero trip count check
                TB_Node* ztc_check = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 1, sizeof(TB_NodeRegion));
                set_input(f, ztc_check, header->inputs[init_edge], 0);
                TB_NODE_GET_EXTRA_T(ztc_check, TB_NodeRegion)->freq = 1.0f;
                mark_node(f, ztc_check);

                // intercept the init path on the header
                set_input(f, top_cloned, ztc_check, 0);
                set_input(f, header, exit_proj_i ? proj0 : proj1, init_edge);

                exit_region = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
                TB_NODE_GET_EXTRA_T(exit_region, TB_NodeRegion)->freq = 1.0f;
                mark_node(f, exit_region);
                set_input(f, exit_region, exit_proj_i ? proj1 : proj0, 0);

                // connect exit projection to exit region, then connect the exit region to
                // what the exit successor wanted
                User* after_exit = cfg_next_user(projs[exit_proj_i]);
                assert(after_exit != NULL && "no successors after exit?");

                set_input(f, exit_region, projs[exit_proj_i], 1);
                set_input(f, after_exit->n, exit_region, after_exit->slot);

                DO_IF(TB_OPTDEBUG_LOOP)(TB_NODE_GET_EXTRA_T(ztc_check, TB_NodeRegion)->tag = lil_name(f, "loop.ztc.%d", i));
                DO_IF(TB_OPTDEBUG_LOOP)(TB_NODE_GET_EXTRA_T(exit_region, TB_NodeRegion)->tag = lil_name(f, "loop.exit.%d", i));
                DO_IF(TB_OPTDEBUG_LOOP)(TB_NODE_GET_EXTRA_T(header, TB_NodeRegion)->tag = lil_name(f, "loop.body.%d", i));
            }

            // insert duplicated branch for backedge
            {
                TB_Node *bot_cloned = NULL, *top_cloned = NULL;
                for (TB_Node* curr = latch; curr != header; curr = curr->inputs[0]) {
                    TB_Node* cloned = loop_clone_node(p, f, header, curr, 1 + single_backedge);

                    // attach control edge
                    if (top_cloned) {
                        set_input(f, top_cloned, cloned, 0);
                    } else {
                        bot_cloned = cloned;
                    }
                    top_cloned = cloned;
                }

                // create cprojs
                assert(bot_cloned->type == TB_BRANCH);
                TB_Node* proj0 = make_proj_node(f, TB_TYPE_CONTROL, bot_cloned, 0);
                TB_Node* proj1 = make_proj_node(f, TB_TYPE_CONTROL, bot_cloned, 1);
                mark_node(f, proj0);
                mark_node(f, proj1);

                set_input(f, top_cloned, header->inputs[single_backedge], 0);
                set_input(f, header, exit_proj_i ? proj0 : proj1, single_backedge);

                // remove initial latch now
                User* after_next = cfg_next_user(projs[1 - exit_proj_i]);
                assert(after_next != NULL && "no successors after next?");
                assert(after_next->slot == 0 && "pretty sure it should've been passed through in[0]");

                set_input(f, exit_region, exit_proj_i ? proj1 : proj0, 1);
                set_input(f, after_next->n, latch->inputs[0], after_next->slot);

                tb_pass_kill_node(f, projs[exit_proj_i]);
                tb_pass_kill_node(f, projs[1 - exit_proj_i]);
                tb_pass_kill_node(f, latch);
            }
        }

        skip:;
    }
    #endif

    dyn_array_destroy(backedges);
    cuikperf_region_end();
}
