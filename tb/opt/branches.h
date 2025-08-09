
// we do this as part of peepholes and also the optimistic solver's rewrite phase
static TB_Node* prune_region(TB_Function* f, TB_Node* n) {
    // if there's a dead loop, we need to expunge the body such that
    // it doesn't leave a messy trace
    if (cfg_is_natural_loop(n) && is_dead_ctrl(f, n->inputs[0]) && n->input_count == 2) {
        TB_Node* dead = dead_node(f);
        set_input(f, n, NULL, 0);
        set_input(f, n, NULL, 1);
        return dead;
    }

    // prune dead predeccessors
    bool changes = false;
    size_t i = 0, extra_edges = 0;
    while (i < n->input_count) {
        if (is_dead_ctrl(f, n->inputs[i])) {
            changes = true;
            remove_input(f, n, i);

            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_PHI && USERI(u) == 0) {
                    remove_input(f, USERN(u), i + 1);
                }
            }
        } else {
            i += 1;
        }
    }

    // demote loop regions if they lose their backedge
    if (n->input_count != 2 && cfg_is_natural_loop(n)) {
        n->type = TB_REGION;
        changes = true;
    }

    // kill useless phis to kill the region itself
    if (n->input_count <= 1) {
        // 0 or 1 preds means no phis
        for (size_t i = 0; i < n->user_count;) {
            TB_User* use = &n->users[i];
            if (USERN(use)->type == TB_PHI) {
                TB_ASSERT(USERI(use) == 0);
                TB_ASSERT(USERN(use)->input_count == 2);

                TB_Node* phi  = USERN(use);
                TB_Node* init = phi->inputs[1];
                if (phi == init && phi->user_count == 1) {
                    tb_kill_node(f, phi);
                } else {
                    subsume_node(f, USERN(use), USERN(use)->inputs[1]);
                }
            } else {
                i += 1;
            }
        }

        return n->input_count == 1 ? n->inputs[0] : dead_node(f);
    }

    return changes ? n : NULL;
}

static TB_Node* ideal_region(TB_Function* f, TB_Node* n) {
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

    TB_Node* k = prune_region(f, n);
    if (k) {
        return k;
    }

    bool changes = false;
    // joining region stacks doesn't apply to region's subtypes
    if (n->type == TB_REGION && n->input_count > 1) {
        size_t i = 0;
        while (i < n->input_count) {
            TB_Node* pred = n->inputs[i];
            if (pred->type == TB_REGION && !is_dead_ctrl(f, pred)) {
                bool pure = true;
                FOR_USERS(u, pred) {
                    TB_Node* un = USERN(u);
                    if (un != n) {
                        // if the phi is exclusively used by our region's phis we can combine them
                        if (un->type == TB_PHI && un->user_count == 1) {
                            TB_Node* next = USERN(&un->users[0]);
                            if (next->type == TB_PHI && next->inputs[0] == n && USERI(&un->users[0]) == 1 + i) {
                                continue;
                            }
                        }

                        pure = false;
                        break;
                    }
                }

                // pure regions can be collapsed into direct edges
                if (pure) {
                    // printf("Collapsing %%%u of %%%u...\n", pred->gvn, n->gvn);

                    int expected_phi_in_count = n->input_count + pred->input_count;
                    FOR_N(j, 0, pred->input_count) {
                        TB_Node* pred_pred = pred->inputs[j];
                        // printf("  PRED %%%u\n", pred_pred->gvn);

                        // we replace on the final iteration because it'll avoid mutating the pred_phi
                        // while we're trying to read it in this loop.
                        bool replace = j == pred->input_count - 1;
                        if (replace) {
                            set_input(f, n, pred_pred, i);
                        } else {
                            add_input_late(f, n, pred_pred);
                        }

                        // for each phi, we append the respective edge
                        FOR_USERS(u, n) {
                            TB_Node* phi = USERN(u);
                            if (phi->type == TB_PHI) {
                                TB_ASSERT(phi->inputs[0] == n);

                                TB_Node* pred_phi = phi->inputs[1 + i];
                                if (pred_phi->type == TB_PHI && pred_phi->inputs[0] == pred) {
                                    pred_phi = pred_phi->inputs[1 + j];
                                }

                                if (replace) {
                                    set_input(f, phi, pred_phi, 1 + i);
                                } else {
                                    add_input_late(f, phi, pred_phi);
                                }
                            }
                        }
                    }
                    TB_ASSERT(n->input_count == expected_phi_in_count - 1);

                    while (pred->user_count > 0) {
                        TB_Node* use_n = USERN(&pred->users[pred->user_count - 1]);
                        int use_i      = USERI(&pred->users[pred->user_count - 1]);

                        TB_ASSERT(use_n->type == TB_PHI);
                        tb_kill_node(f, use_n);
                    }
                    tb_kill_node(f, pred);
                    changes = true;
                    continue;
                }

                #if 0
                if (n->inputs[i]->user_count == 1 && n->inputs[i]->input_count > 0) {
                    assert(USERN(n->inputs[i]->users) == n);
                    changes = true;

                    TB_Node* pred = n->inputs[i];
                    {
                        size_t old_count = n->input_count;
                        size_t new_count = old_count + (pred->input_count - 1);

                        // convert pred-of-pred into direct pred
                        set_input(f, n, pred->inputs[0], i);

                        // append rest to the end (order really doesn't matter)
                        //
                        // NOTE(NeGate): we might waste quite a bit of space because of the arena
                        // alloc and realloc
                        TB_Node** new_inputs = tb_arena_alloc(&f->arena, new_count * sizeof(TB_Node*));
                        memcpy(new_inputs, n->inputs, old_count * sizeof(TB_Node*));
                        n->inputs = new_inputs;
                        n->input_count = new_count;
                        n->input_cap = new_count;

                        FOR_N(j, 0, pred->input_count - 1) {
                            new_inputs[old_count + j] = pred->inputs[j + 1];
                            add_user(f, n, pred->inputs[j + 1], old_count + j);
                        }
                    }

                    // update PHIs
                    FOR_USERS(u, n) {
                        if (USERN(u)->type == TB_PHI && USERI(u) == 0) {
                            // we don't replace the initial, just the rest
                            TB_Node* phi = USERN(u);
                            TB_Node* phi_val = phi->inputs[i + 1];

                            // append more phi vals... lovely allocs
                            size_t phi_ins = phi->input_count;
                            size_t new_phi_ins = phi_ins + (pred->input_count - 1);

                            TB_Node** new_inputs = tb_arena_alloc(&f->arena, new_phi_ins * sizeof(TB_Node*));
                            memcpy(new_inputs, phi->inputs, phi_ins * sizeof(TB_Node*));
                            phi->inputs = new_inputs;
                            phi->input_count = new_phi_ins;
                            phi->input_cap = new_phi_ins;

                            FOR_N(j, 0, pred->input_count - 1) {
                                new_inputs[phi_ins + j] = phi_val;
                                add_user(f, phi, phi_val, phi_ins + j);
                            }
                        }
                    }
                    continue;
                }
                #endif
            }
            i += 1;
        }

        if (changes) {
            // affine loops can't be single edge
            if (cfg_is_natural_loop(n) && n->input_count != 2) {
                n->type = TB_REGION;
            }

            return n;
        }
    }

    // fold out diamond shaped patterns
    TB_Node* same = n->inputs[0];
    if (n->type == TB_REGION && same->type == TB_BRANCH_PROJ && same->user_count == 1 && same->inputs[0]->type == TB_BRANCH) {
        same = same->inputs[0];
        if (same->user_count != n->input_count) {
            return NULL;
        }

        // if it has phis... quit
        FOR_USERS(u, n) {
            if (USERN(u)->type == TB_PHI) { return NULL; }
        }

        FOR_N(i, 1, n->input_count) {
            if (n->inputs[i]->type != TB_BRANCH_PROJ || n->inputs[i]->user_count != 1 || n->inputs[i]->inputs[0] != same) {
                return NULL;
            }
        }

        // kill projections
        TB_Node* before = same->inputs[0];
        FOR_USERS(u, before) {
            assert(cfg_is_cproj(USERN(u)));
            tb_kill_node(f, USERN(u));
        }
        tb_kill_node(f, same);
        return before;
    }

    // this loop is empty, none of the operations require iteration so we can
    // fold all the phis into closed-form equations and kill the backedge.
    if (n->type == TB_AFFINE_LOOP) {
        // if the latch and the loop header are attached to each other that means
        // there's no CFG effects in this loop
        TB_Node* latch = affine_loop_latch(n);
        if (latch->inputs[0] == n) {
            bool closed = true;
            FOR_USERS(u, n) {
                TB_Node* phi = USERN(u);
                if (phi->type != TB_PHI) { continue; }

                // we wanna know loop bounds
                uint64_t trips_min = 1, trips_max = UINT64_MAX;
                if (find_affine_indvar(phi, n) == NULL) {
                    closed = false;
                    break;
                }
            }

            TB_InductionVar var;
            if (closed && find_latch_indvar(n, latch, &var) && !var.backwards) {
                TB_Node* trip_node = generate_loop_trip_count(f, var);

                // replace with closed-form equations
                for (size_t i = 0; i < n->user_count;) {
                    TB_Node* un = USERN(&n->users[i]);
                    if (un->type == TB_PHI) {
                        Lattice* init = latuni_get(f, un->inputs[1]);
                        Lattice* step = latuni_get(f, un->inputs[2]->inputs[2]);
                        TB_ASSERT(lattice_is_iconst(step));

                        TB_Node* casted_trip_node = trip_node;
                        TB_ASSERT(un->dt.raw == trip_node->dt.raw);

                        TB_Node* closed_form = casted_trip_node;
                        if (lattice_int_ne(step, 1)) {
                            closed_form = make_int_binop(f, TB_MUL, closed_form, un->inputs[2]->inputs[2]);
                        }

                        if (!lattice_is_izero(init)) {
                            closed_form = make_int_binop(f, TB_ADD, un->inputs[1], closed_form);
                        }

                        set_input(f, un, NULL, 0);
                        subsume_node(f, un, closed_form);
                        mark_node_n_users(f, closed_form);
                    } else {
                        i += 1;
                    }
                }

                n->type = TB_NATURAL_LOOP;
                return n;
            }
        }
    }

    return NULL;
}

static TB_Node* ideal_phi(TB_Function* f, TB_Node* n) {
    // degenerate PHI, poison it
    if (n->input_count == 1) {
        log_warn("%s: ir: generated poison due to PHI with no edges", f->super.name);
        return make_poison(f, n->dt);
    }

    #if 0
    // equivalent nodes on all previous paths? just sink the op
    TB_NodeTypeEnum first_type = n->inputs[1]->type;
    if (first_type >= TB_AND && first_type <= TB_CMP_FLE) {
        TB_Node* rhs = n->inputs[1]->inputs[2];
        FOR_N(i, 2, n->input_count) {
            if (n->inputs[i]->inputs[2] != rhs) {
                rhs = NULL;
                break;
            }
        }

        if (rhs) {
            FOR_N(i, 1, n->input_count) {
                set_input(f, n, n->inputs[i]->inputs[1], i);
            }

            size_t extra = extra_bytes(n);
            TB_Node* new_n = tb_alloc_node(f, first_type, n->dt, 3, extra);
            set_input(f, new_n, n,   1);
            set_input(f, new_n, rhs, 2);
            if (extra) {
                memcpy(new_n->extra, n->inputs[1]->extra, extra);
            }
            return new_n;
        }
    }
    #endif

    // if branch, both paths are empty => select(cond, t, f)
    //
    // TODO(NeGate): we can make this diamond trick work for bigger
    // branches, we should support a lookup instruction similar to
    // "switch" logic for data.
    TB_DataType dt = n->dt;
    TB_Node* region = n->inputs[0];
    if (n->dt.type != TB_TAG_MEMORY) {
        if (region->input_count == 2) {
            // for now we'll leave multi-phi scenarios alone, we need
            // to come up with a cost-model around this stuff.
            FOR_USERS(u, region) {
                if (USERN(u)->type == TB_PHI) { if (USERN(u) != n) return NULL; }
            }

            // guarentee paths are effectless (there's only one data phi and no control nodes)
            //
            //        If
            //       /  \
            // CProjT    CProjF          Region[0][0] == Region[1][0]
            //       \  /
            //      Region
            //
            TB_Node* left = region->inputs[0];
            TB_Node* right = region->inputs[1];
            if (left->type == TB_PROJ && right->type == TB_PROJ &&
                left->inputs[0]->type == TB_IF && left->inputs[0] == right->inputs[0]) {
                TB_Node* branch = left->inputs[0];
                TB_ASSERT(branch->input_count == 2);

                TB_Node *values[2];
                FOR_USERS(u, branch) {
                    TB_Node* proj = USERN(u);
                    if (proj->type == TB_PROJ) {
                        int index = TB_NODE_GET_EXTRA_T(proj, TB_NodeProj)->index;
                        // the projection needs to exclusively refer to the region,
                        // if not we can't elide those effects here.
                        if (proj->user_count > 1 || USERN(proj->users) != region) {
                            return NULL;
                        }

                        int phi_i = USERI(proj->users);
                        TB_ASSERT(phi_i + 1 < n->input_count);
                        values[index] = n->inputs[1 + phi_i];
                    }
                }

                // header -> merge
                TB_Node* cond = branch->inputs[1];
                {
                    TB_Node* parent = branch->inputs[0];
                    tb_kill_node(f, branch);
                    tb_kill_node(f, left);
                    tb_kill_node(f, right);

                    // attach the header and merge to each other
                    mark_node(f, parent);
                    mark_users(f, region);
                    subsume_node(f, region, parent);
                }

                TB_Node* selector = tb_alloc_node(f, TB_SELECT, dt, 4, 0);
                set_input(f, selector, cond, 1);
                set_input(f, selector, values[0], 2);
                set_input(f, selector, values[1], 3);
                return selector;
            }
        }

        #if 0
        if (region->input_count > 2) {
            if (region->inputs[0]->type != TB_BRANCH_PROJ || region->inputs[0]->inputs[0]->type != TB_BRANCH) {
                return NULL;
            }

            TB_Node* phi = NULL;
            FOR_USERS(u, region) {
                if (USERN(u)->type == TB_PHI) {
                    if (USERN(u)->dt.type == TB_TAG_MEMORY) { return NULL; }
                    if (phi != NULL) { return NULL; }
                    phi = USERN(u);
                }
            }

            // try to make a multi-way lookup:
            //
            //      Branch
            //       / | \
            //    ... ... ...         each of these is a CProj
            //       \ | /
            //       Region
            //            \
            //             \ ... ...  each of these is a trivial value (int consts only for now)
            //              \ | /
            //               Phi
            TB_Node* parent = region->inputs[0]->inputs[0];
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(parent);
            if (parent->type != TB_BRANCH || br->succ_count != n->input_count - 1 || br->succ_count >= 64) {
                return NULL;
            }

            uint64_t min = 0;
            uint64_t max = 0;
            uint64_t entropy = 0;

            static int used;
            static int all_entries[64];

            // verify we have a really clean looking diamond shape and a short enough range
            bool bail = false;
            FOR_N(i, 0, n->input_count - 1) {
                if (region->inputs[i]->type != TB_BRANCH_PROJ || region->inputs[i]->inputs[0] != parent) {
                    return NULL;
                }

                TB_NodeBranchProj* p = TB_NODE_GET_EXTRA(region->inputs[i]);
                printf("KEY: %llu\n", p->key);

                if (max - min > 64) {
                    return NULL;
                }
            }

            // identify how many kinds of values we might see, if there's only 2
            // then we could use a bitmap
            NL_Table dict = nl_table_arena_alloc(&f->tmp_arena, 64);
            FOR_N(i, 0, n->input_count - 1) {
                TB_NodeBranchProj* p = TB_NODE_GET_EXTRA(region->inputs[i]);
                all_entries[p->key - min] = used++;
                void* k = (void*) (uintptr_t) ((p->key - min) + 1);
                TB_Node* v = cfg_next_control();
                void* nl_table_put2(NL_Table* restrict tbl, k, v, NL_HashFunc hash, NL_CompareFunc cmp);
            }

            // Make a table representation of this big switch statement.
            tb_print_dumb(f);
            __debugbreak();

        }
        #endif
    }

    return NULL;
}

// TODO(NeGate): convert 2-way switches into if statements
static TB_Node* ideal_branch(TB_Function* f, TB_Node* n) {
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

    /* if (br->succ_count == 2) {
        TB_NodeBranchProj* if_br = cfg_if_branch(n);
        if (if_br && if_br->key == 0) {
            TB_Node* cmp_node = n->inputs[1];
            TB_NodeTypeEnum cmp_type = cmp_node->type;

        }
    } */

    return NULL;
}

static Lattice* value_call(TB_Function* f, TB_Node* n) {
    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);

    TB_Arena* arena = get_permanent_arena(f->super.module);
    size_t size = sizeof(Lattice) + c->proj_count*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = c->proj_count };

    // non-NULL when we need to update the IPSCCP type
    TB_Function* callee = NULL;

    // control just flows through
    Lattice* in_ctrl = l->elems[0] = latuni_get(f, n->inputs[0]);
    if (in_ctrl != &LIVE_IN_THE_SKY) {
        FOR_N(i, 1, c->proj_count) {
            l->elems[i] = &TOP_IN_THE_SKY;
        }
    } else {
        Lattice* target = latuni_get(f, n->inputs[2]);
        Lattice* rets = NULL;
        if (
            target->tag == LATTICE_PTRCON &&
            target->_ptr->tag == TB_SYMBOL_FUNCTION
        ) {
            // the IPSCCP rets are stored in the shape of the root node's type
            callee = (TB_Function*) target->_ptr;
            rets = callee->ipsccp_ret;

            if (rets) {
                TB_ASSERT(rets->tag == LATTICE_TUPLE);
                TB_ASSERT(rets->_elem_count + 1 >= c->proj_count);
                FOR_N(i, 0, c->proj_count) {
                    // skip the RPC
                    l->elems[i] = rets->elems[i + (i >= 2 ? 1 : 0)];
                }
            }
        }

        if (rets == NULL) {
            if (f->super.module->during_ipsccp && callee != NULL) {
                // optimistic assumption of a function return is that
                // it never happens.
                l->elems[0] = &DEAD_IN_THE_SKY;
                FOR_N(i, 1, c->proj_count) {
                    l->elems[i] = &TOP_IN_THE_SKY;
                }
            } else {
                FOR_N(i, 1, c->proj_count) {
                    l->elems[i] = &BOT_IN_THE_SKY;
                }

                FOR_USERS(u, n) {
                    if (is_proj(USERN(u))) {
                        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                        if (index > 0) { l->elems[index] = lattice_from_dt(f, USERN(u)->dt); }
                    }
                }
            }
        }
    }

    Lattice* k = latticehs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }

    // update the callee's "reachable" arguments
    if (callee && in_ctrl == &LIVE_IN_THE_SKY && f->super.module->during_ipsccp) {
        size_t elem_count = 3 + callee->prototype->param_count;
        size_t size = sizeof(Lattice) + elem_count*sizeof(Lattice*);
        Lattice* args = tb_arena_alloc(arena, size);
        *args = (Lattice){ LATTICE_TUPLE, ._elem_count = elem_count };

        args->elems[0] = latuni_get(f, n->inputs[0]);
        args->elems[1] = latuni_get(f, n->inputs[1]);
        args->elems[2] = &XNULL_IN_THE_SKY; // RPC is at least not NULL
        FOR_N(i, 3, elem_count) {
            args->elems[i] = latuni_get(f, n->inputs[i]);
        }

        // intern & free
        Lattice* k = latticehs_intern(&f->super.module->lattice_elements, args);
        if (k != args) { tb_arena_free(arena, l, size); args = k; }

        // move down, unless we're already there
        Lattice* old = atomic_load_explicit(&callee->ipsccp_args, memory_order_acquire);
        do {
            args = old ? lattice_meet(f, args, old) : args;
        } while (old != args && !atomic_compare_exchange_strong(&callee->ipsccp_args, &old, args));

        if (old != args) {
            #if 0
            mtx_lock(&aaa);
            printf("%s: ARGS: ", callee->super.name);
            print_lattice(old ? old : &TOP_IN_THE_SKY);
            printf(" => ");
            print_lattice(args);
            printf("\n");
            mtx_unlock(&aaa);
            #endif

            push_ipsccp_job(f->super.module, callee);
        }
    }

    return k;
}

static Lattice* value_never_branch(TB_Function* f, TB_Node* n) {
    Lattice* before = latuni_get(f, n->inputs[0]);
    if (before == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    return lattice_branch_goto(f, 2, 0);
}

static TB_Node* ideal_if(TB_Function* f, TB_Node* n) {
    TB_Node* cmp = n->inputs[1];
    TB_NodeTypeEnum cmp_type = cmp->type;

    // if ((y <= x)) => if (x < y) flipped conditions
    if (cmp_type == TB_CMP_SLE || cmp_type == TB_CMP_ULE) {
        TB_Node* new_cmp = tb_alloc_node(f, cmp_type == TB_CMP_SLE ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
        set_input(f, new_cmp, cmp->inputs[2], 1);
        set_input(f, new_cmp, cmp->inputs[1], 2);
        TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(cmp, TB_NodeCompare)->cmp_dt);

        // flip
        FOR_USERS(u, n) {
            TB_ASSERT(USERN(u)->type == TB_PROJ);
            TB_NodeProj* p = TB_NODE_GET_EXTRA(USERN(u));
            p->index = !p->index;
        }

        set_input(f, n, new_cmp, 1);
        mark_node(f, new_cmp);
        return n;
    }

    // if (x != 0) => if (x)
    // if (x == 0) => if (x) (FLIPPED)
    if ((cmp_type == TB_CMP_NE || cmp_type == TB_CMP_EQ) && cmp->inputs[2]->type == TB_ICONST) {
        uint64_t imm = TB_NODE_GET_EXTRA_T(cmp->inputs[2], TB_NodeInt)->value;
        if (imm == 0) {
            set_input(f, n, cmp->inputs[1], 1);

            // flip successors
            if (cmp_type == TB_CMP_EQ) {
                FOR_USERS(u, n) {
                    TB_ASSERT(USERN(u)->type == TB_PROJ);
                    TB_NodeProj* p = TB_NODE_GET_EXTRA(USERN(u));
                    p->index = !p->index;
                }
            }

            return n;
        }
    }

    // zero and sign extension doesn't change the "falsey-ness" of a value, 0 will still be zero.
    if (cmp->type == TB_ZERO_EXT || cmp->type == TB_SIGN_EXT) {
        set_input(f, n, cmp->inputs[1], 1);
        return n;
    }

    // empty BB, just does if branch but the condition is effect-less
    // if (a && b) A else B => if (a ? b : 0) A else B
    //
    // TODO(NeGate): implement form which works on an arbitrary falsey
    if (n->inputs[0]->type == TB_PROJ && n->inputs[0]->inputs[0]->type == TB_IF && is_empty_bb(f, n)) {
        TB_NodeBranchProj* br_path = TB_NODE_GET_EXTRA(n->inputs[0]);

        int index = br_path->index;
        TB_Node* pred_branch = n->inputs[0]->inputs[0];

        // needs one pred
        if (pred_branch->type == TB_IF) {
            // check our parent's aux path
            TB_User* other_proj   = proj_with_index(pred_branch, 1 - index);
            TB_Node* shared_edge  = cfg_next_control(USERN(other_proj));

            // check our aux path
            TB_User* other_proj2  = proj_with_index(n, 1 - index);
            TB_Node* shared_edge2 = cfg_next_control(USERN(other_proj2));

            // if they're the same then we've got a shortcircuit eval setup
            if (shared_edge == shared_edge2) {
                TB_ASSERT(cfg_is_region(shared_edge));
                int shared_i  = USERI(USERN(other_proj)->users);
                int shared_i2 = USERI(USERN(other_proj2)->users);

                bool match = true;
                FOR_USERS(phis, shared_edge) if (USERN(phis)->type == TB_PHI) {
                    if (USERN(phis)->inputs[1+shared_i] != USERN(phis)->inputs[1+shared_i2]) {
                        match = false;
                        break;
                    }
                }

                if (match) {
                    // remove pred from shared edge
                    remove_input(f, shared_edge, shared_i);
                    FOR_USERS(u, shared_edge) {
                        if (USERN(u)->type == TB_PHI && USERI(u) == 0) {
                            remove_input(f, USERN(u), shared_i + 1);
                            mark_node(f, USERN(u));
                        }
                    }

                    TB_Node* before = pred_branch->inputs[0];
                    TB_Node* cmp = pred_branch->inputs[1];

                    // remove first branch
                    while (pred_branch->user_count > 0) {
                        TB_Node* un = USERN(&pred_branch->users[pred_branch->user_count - 1]);
                        assert(is_proj(un));

                        tb_kill_node(f, un);
                    }
                    tb_kill_node(f, pred_branch);
                    set_input(f, n, before, 0);

                    // construct branchless merge
                    TB_Node* false_node = make_int_node(f, n->inputs[1]->dt, 0);

                    // a ? b : 0
                    TB_Node* selector = tb_alloc_node(f, TB_SELECT, n->inputs[1]->dt, 4, 0);
                    set_input(f, selector, cmp,          1);
                    set_input(f, selector, n->inputs[1], 2);
                    set_input(f, selector, false_node,   3);

                    set_input(f, n, selector, 1);
                    mark_node(f, selector);
                    return n;
                }
            }
        }
    }

    return NULL;
}

static Lattice* value_if(TB_Function* f, TB_Node* n) {
    Lattice* before = latuni_get(f, n->inputs[0]);
    if (before != &LIVE_IN_THE_SKY) {
        return lattice_branch_none(f, 2);
    }

    Lattice* pred = latuni_get(f, n->inputs[1]);
    pred = lattice_truthy(pred);

    if (pred == &TOP_IN_THE_SKY) {
        return lattice_branch_none(f, 2);
    } else if (pred == &TRUE_IN_THE_SKY) {
        return lattice_branch_goto(f, 2, 0);
    } else if (pred == &FALSE_IN_THE_SKY) {
        return lattice_branch_goto(f, 2, 1);
    } else {
        return NULL;
    }
}

static Lattice* value_branch(TB_Function* f, TB_Node* n) {
    Lattice* before = latuni_get(f, n->inputs[0]);
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    if (before != &LIVE_IN_THE_SKY) {
        return lattice_branch_none(f, br->succ_count);
    }

    // constant fold branch
    assert(n->input_count == 2);
    Lattice* key = latuni_get(f, n->inputs[1]);
    if (key == &TOP_IN_THE_SKY) {
        return lattice_branch_none(f, br->succ_count);
    }

    if (key->tag == LATTICE_NULL) {
        ptrdiff_t taken = 0;
        FOR_USERS(u, n) {
            TB_NodeBranchProj* path = TB_NODE_GET_EXTRA(USERN(u));
            if (path->index > 0 && path->key == 0) {
                taken = path->index;
                break;
            }
        }

        return lattice_branch_goto(f, br->succ_count, taken);
    } else if (key->tag == LATTICE_INT && key->_int.min == key->_int.max) {
        int64_t key_const = key->_int.min;
        ptrdiff_t taken = 0;

        FOR_USERS(u, n) {
            TB_NodeBranchProj* path = TB_NODE_GET_EXTRA(USERN(u));
            if (path->index > 0 && key_const == path->key) {
                taken = path->index;
                break;
            }
        }

        return lattice_branch_goto(f, br->succ_count, taken);
    } else {
        // check for redundant conditions
        /* FOR_USERS(u, n->inputs[1]) {
            if (USERN(u)->type != TB_BRANCH || USERI(u) != 1 || USERN(u) == n) {
                continue;
            }

            TB_Node* end = USERN(u);
            if (same_sorta_branch(end, n)) {
                FOR_USERS(succ_user, end) {
                    assert(USERN(succ_user)->type == TB_BRANCH_PROJ);
                    int index = TB_NODE_GET_EXTRA_T(USERN(succ_user), TB_NodeProj)->index;
                    TB_Node* succ = cfg_next_bb_after_cproj(USERN(succ_user));

                    // we must be dominating for this to work
                    if (fast_dommy(succ, n)) {
                        return lattice_branch_goto(f, br->succ_count, index);
                    }
                }
            }
        } */
    }

    return NULL;
}

static Lattice* value_safepoint(TB_Function* f, TB_Node* n) {
    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);

    TB_Arena* arena = get_permanent_arena(f->super.module);
    size_t size = sizeof(Lattice) + 3*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = 3 };

    l->elems[0] = &LIVE_IN_THE_SKY;
    l->elems[1] = &LIVE_IN_THE_SKY;
    // memory just flows through
    l->elems[2] = &MEM_IN_THE_SKY;

    Lattice* k = latticehs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static TB_Node* identity_safepoint(TB_Function* f, TB_Node* n) {
    if (n->inputs[0]->type == TB_SAFEPOINT) {
        // (safepoint (safepoint X)) => (safepoint X)
        return n->inputs[0];
    } else {
        return n;
    }
}

static TB_Node* identity_phi(TB_Function* f, TB_Node* n) {
    TB_Node* same = NULL;
    if (f->gcf_nodes) {
        FOR_N(i, 1, n->input_count) {
            // not reachable, don't care
            Lattice* ctrl = latuni_get(f, n->inputs[0]->inputs[i - 1]);
            if (ctrl == &LIVE_IN_THE_SKY) {
                TB_Node* in = gcf_congruent_leader(f, n->inputs[i]);
                if (same == NULL) { same = in; }
                else if (!gcf_is_congruent(f, same, in)) { return n; }
            }
        }
    } else {
        FOR_N(i, 1, n->input_count) {
            // not reachable, don't care
            Lattice* ctrl = latuni_get(f, n->inputs[0]->inputs[i - 1]);
            if (ctrl == &LIVE_IN_THE_SKY) {
                if (n->inputs[i] == n) continue;
                if (n->inputs[i]->type == TB_POISON) continue;
                if (same && same != n->inputs[i]) return n;
                same = n->inputs[i];
            }
        }

        if (same == NULL) {
            // phi has no possible paths, simplify it into a poison
            return make_poison(f, n->dt);
        }
    }
    return same;
}


