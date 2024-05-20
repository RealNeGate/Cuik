
static TB_Node* ideal_region(TB_Function* f, TB_Node* n) {
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

    // if a region is dead, start a violent death chain
    if (n->input_count == 0) {
        return NULL;
    } else if (n->input_count == 1) {
        // remove phis, because we're single entry they're all degens
        for (size_t i = 0; i < n->user_count;) {
            TB_User* use = &n->users[i];
            if (USERN(use)->type == TB_PHI) {
                assert(USERI(use) == 0);
                assert(USERN(use)->input_count == 2);
                subsume_node(f, USERN(use), USERN(use)->inputs[1]);
            } else {
                i += 1;
            }
        }

        // we might want this as an identity
        return n->inputs[0];
    } else {
        bool changes = false;

        size_t i = 0, extra_edges = 0;
        while (i < n->input_count) {
            Lattice* ty = latuni_get(f, n->inputs[i]);
            if (ty == &TOP_IN_THE_SKY) {
                // both the region and phi are doing remove swap so the order should
                // stay fine across them.
                changes = true;

                FOR_USERS(use, n) {
                    TB_Node* phi = USERN(use);
                    if (phi->type == TB_PHI) {
                        assert(USERI(use) == 0);
                        assert(phi->input_count == 1 + n->input_count);
                        remove_input(f, phi, i + 1);
                    }
                }

                remove_input(f, n, i);
                continue;
            } else if (n->type == TB_REGION && n->inputs[i]->type == TB_REGION) {
                #if 1
                // pure regions can be collapsed into direct edges
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
                        TB_Node** new_inputs = tb_arena_alloc(f->arena, new_count * sizeof(TB_Node*));
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

                            TB_Node** new_inputs = tb_arena_alloc(f->arena, new_phi_ins * sizeof(TB_Node*));
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

                    extra_edges += 1;
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
                mark_users(f, n);
            }

            f->invalidated_loops = true;
            return n;
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
            if (left->type == TB_BRANCH_PROJ && right->type == TB_BRANCH_PROJ &&
                left->inputs[0]->type == TB_BRANCH && left->inputs[0] == right->inputs[0]) {
                TB_Node* branch = left->inputs[0];

                TB_NodeBranchProj* header_br = cfg_if_branch(branch);
                if (header_br) {
                    assert(branch->input_count == 2);

                    TB_Node *values[2];
                    FOR_USERS(u, branch) {
                        TB_Node* proj = USERN(u);
                        if (proj->type == TB_BRANCH_PROJ) {
                            int index = TB_NODE_GET_EXTRA_T(proj, TB_NodeProj)->index;
                            // the projection needs to exclusively refer to the region,
                            // if not we can't elide those effects here.
                            if (proj->user_count > 1 || USERN(proj->users) != region) {
                                return NULL;
                            }

                            int phi_i = USERI(proj->users);
                            assert(phi_i + 1 < n->input_count);
                            values[index] = n->inputs[1 + phi_i];
                        }
                    }

                    // TODO(NeGate): handle non-zero falsey
                    TB_Node* cond = branch->inputs[1];
                    if (header_br->key == 0) {
                        // header -> merge
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
            }
        }

        if (region->input_count > 2 && n->dt.type == TB_TAG_INT) {
            if (region->inputs[0]->type != TB_BRANCH_PROJ || region->inputs[0]->inputs[0]->type != TB_BRANCH) {
                return NULL;
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
            if (parent->type != TB_BRANCH || br->succ_count != n->input_count - 1) {
                return NULL;
            }

            // verify we have a really clean looking diamond shape
            FOR_N(i, 0, n->input_count - 1) {
                if (region->inputs[i]->type != TB_BRANCH_PROJ || region->inputs[i]->inputs[0] != parent) return NULL;
                if (n->inputs[1 + i]->type != TB_ICONST) return NULL;
            }

            // convert to lookup node
            TB_Node* lookup = tb_alloc_node(f, TB_LOOKUP, n->dt, 2, sizeof(TB_NodeLookup) + (br->succ_count * sizeof(TB_LookupEntry)));
            set_input(f, lookup, parent->inputs[1], 1);

            TB_NodeLookup* l = TB_NODE_GET_EXTRA(lookup);
            l->entry_count = br->succ_count;
            FOR_N(i, 0, n->input_count - 1) {
                TB_Node* k = region->inputs[i];
                assert(k->type == TB_BRANCH_PROJ);

                TB_NodeBranchProj* p = TB_NODE_GET_EXTRA(k);
                assert(p->index < br->succ_count);
                if (p->index == 0) {
                    l->entries[p->index].key = 0; // default value, doesn't matter
                } else {
                    l->entries[p->index].key = p->key;
                }

                TB_NodeInt* v = TB_NODE_GET_EXTRA(n->inputs[1 + i]);
                l->entries[p->index].val = v->value;
            }

            // kill branch, we don't really need it anymore
            TB_Node* before = parent->inputs[0];
            tb_kill_node(f, parent);
            subsume_node(f, region, before);

            return lookup;
        }
    }

    return NULL;
}

static TB_Node* ideal_branch(TB_Function* f, TB_Node* n) {
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

    if (br->succ_count == 2) {
        TB_NodeBranchProj* if_br = cfg_if_branch(n);
        if (if_br && if_br->key == 0) {
            TB_Node* cmp_node = n->inputs[1];
            TB_NodeTypeEnum cmp_type = cmp_node->type;

            // empty BB, just does if branch but the condition is effect-less
            // if (a && b) A else B => if (a ? b : 0) A else B
            //
            // TODO(NeGate): implement form which works on an arbitrary falsey
            if (n->inputs[0]->type == TB_BRANCH_PROJ && n->inputs[0]->inputs[0]->type == TB_BRANCH && is_empty_bb(f, n)) {
                TB_NodeBranchProj* br_path = TB_NODE_GET_EXTRA(n->inputs[0]);

                int index = br_path->index;
                uint64_t falsey = br_path->key;
                TB_Node* pred_branch = n->inputs[0]->inputs[0];

                // needs one pred
                TB_NodeBranchProj* pred_if_br = cfg_if_branch(pred_branch);
                if (pred_if_br && pred_if_br->key == 0) {
                    uint64_t pred_falsey = pred_if_br->key;
                    TB_NodeBranch* pred_br_info = TB_NODE_GET_EXTRA(pred_branch);

                    // check our parent's aux path
                    TB_User* other_proj   = proj_with_index(pred_branch, 1 - index);
                    TB_Node* shared_edge  = cfg_next_bb_after_cproj(USERN(other_proj));

                    // check our aux path
                    TB_User* other_proj2  = proj_with_index(n, 1 - index);
                    TB_Node* shared_edge2 = cfg_next_bb_after_cproj(USERN(other_proj2));

                    // if they're the same then we've got a shortcircuit eval setup
                    if (shared_edge == shared_edge2) {
                        assert(cfg_is_region(shared_edge));
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

                            // we wanna normalize into a comparison (not a boolean -> boolean)
                            if (!(cmp->dt.type == TB_TAG_INT && cmp->dt.data == 1)) {
                                assert(!TB_IS_FLOAT_TYPE(cmp->dt) && "TODO");
                                TB_Node* imm = make_int_node(f, cmp->dt, pred_falsey);

                                TB_Node* new_node = tb_alloc_node(f, TB_CMP_NE, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
                                set_input(f, new_node, cmp, 1);
                                set_input(f, new_node, imm, 2);
                                TB_NODE_SET_EXTRA(new_node, TB_NodeCompare, .cmp_dt = cmp->dt);

                                mark_node(f, new_node);
                                cmp = new_node;
                            }

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

            // br ((y <= x)) => br (x < y) flipped conditions
            if (cmp_type == TB_CMP_SLE || cmp_type == TB_CMP_ULE) {
                TB_Node* new_cmp = tb_alloc_node(f, cmp_type == TB_CMP_SLE ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
                set_input(f, new_cmp, cmp_node->inputs[2], 1);
                set_input(f, new_cmp, cmp_node->inputs[1], 2);
                TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(cmp_node, TB_NodeCompare)->cmp_dt);

                // flip
                FOR_USERS(u, n) {
                    TB_NodeProj* p = TB_NODE_GET_EXTRA(USERN(u));
                    p->index = !p->index;
                }

                set_input(f, n, new_cmp, 1);
                mark_node(f, new_cmp);
                return n;
            }

            // br ((x != y) != 0) => br (x != y)
            if ((cmp_type == TB_CMP_NE || cmp_type == TB_CMP_EQ) && cmp_node->inputs[2]->type == TB_ICONST) {
                uint64_t imm = TB_NODE_GET_EXTRA_T(cmp_node->inputs[2], TB_NodeInt)->value;
                set_input(f, n, cmp_node->inputs[1], 1);
                if_br->key = imm;

                // flip successors
                if (cmp_type == TB_CMP_EQ) {
                    FOR_USERS(u, n) {
                        TB_NodeProj* p = TB_NODE_GET_EXTRA(USERN(u));
                        p->index = !p->index;
                    }
                }

                return n;
            }
        }
    }

    return NULL;
}

static Lattice* value_call(TB_Function* f, TB_Node* n) {
    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);

    TB_Arena* arena = get_permanent_arena(f->super.module);
    size_t size = sizeof(Lattice) + c->proj_count*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = c->proj_count };

    FOR_N(i, 1, c->proj_count) {
        l->elems[i] = &BOT_IN_THE_SKY;
    }

    FOR_USERS(u, n) {
        if (is_proj(USERN(u))) {
            int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
            if (index > 0) { l->elems[index] = lattice_from_dt(f, USERN(u)->dt); }
        }
    }

    // control just flows through
    l->elems[0] = latuni_get(f, n->inputs[0]);

    Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static Lattice* value_never_branch(TB_Function* f, TB_Node* n) {
    Lattice* before = latuni_get(f, n->inputs[0]);
    if (before == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    return lattice_branch_goto(f, 2, 0);
}

static Lattice* value_branch(TB_Function* f, TB_Node* n) {
    Lattice* before = latuni_get(f, n->inputs[0]);
    if (before == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

    // constant fold branch
    assert(n->input_count == 2);
    Lattice* key = latuni_get(f, n->inputs[1]);
    if (key == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    if (key->tag == LATTICE_INT && key->_int.min == key->_int.max) {
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

static TB_Node* identity_safepoint(TB_Function* f, TB_Node* n) {
    if (n->inputs[0]->type == TB_SAFEPOINT_POLL) {
        // (safepoint (safepoint X)) => (safepoint X)
        return n->inputs[0];
    } else {
        return n;
    }
}

static TB_Node* identity_region(TB_Function* f, TB_Node* n) {
    // fold out diamond shaped patterns
    TB_Node* same = n->inputs[0];
    if (same->type == TB_BRANCH_PROJ && same->user_count == 1 && same->inputs[0]->type == TB_BRANCH) {
        same = same->inputs[0];
        if (same->user_count != n->input_count) {
            return n;
        }

        // if it has phis... quit
        FOR_USERS(u, n) {
            if (USERN(u)->type == TB_PHI) { return n; }
        }

        FOR_N(i, 1, n->input_count) {
            if (n->inputs[i]->type != TB_BRANCH_PROJ || n->inputs[i]->user_count != 1 || n->inputs[i]->inputs[0] != same) {
                return n;
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

    return n;
}

static TB_Node* identity_phi(TB_Function* f, TB_Node* n) {
    TB_Node* same = NULL;
    FOR_N(i, 1, n->input_count) {
        if (n->inputs[i] == n) continue;
        if (same && same != n->inputs[i]) return n;
        same = n->inputs[i];
    }

    assert(same);
    if (f->worklist != NULL) {
        mark_users(f, n->inputs[0]);
    }

    return same;
}
