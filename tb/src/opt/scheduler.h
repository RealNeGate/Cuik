// Local instruction scheduling is handled here, the idea is just to do a topological
// sort which is anti-dependency aware, a future TB could implement multiple schedulers.
//
// Once the worklist is filled, you can walk backwards and generate instructions accordingly.
static bool is_same_bb(TB_Node* bb, TB_Node* n) {
    while (n->type != TB_START && n->type != TB_REGION) {
        n = n->inputs[0];
    }

    return n == bb;
}

static bool is_region(TB_Node* n) {
    return n->type == TB_REGION || n->type == TB_START;
}

static bool is_mem_out_op(TB_Node* n) {
    return n->type == TB_END || (n->type >= TB_STORE && n->type <= TB_ATOMIC_CAS);
}

void sched_walk(TB_Passes* passes, Worklist* ws, DynArray(PhiVal)* phi_vals, TB_Node* bb, TB_Node* n) {
    if (!is_same_bb(bb, n) || worklist_test_n_set(ws, n)) {
        return;
    }

    // push inputs
    FOREACH_REVERSE_N(i, 0, n->input_count) {
        sched_walk(passes, ws, phi_vals, bb, n->inputs[i]);
    }

    // memory effects have anti-dependencies, the previous loads
    // must finish before the next memory effect is applied.
    if (is_mem_out_op(n)) {
        TB_Node* prev_mem = n->inputs[1];
        for (User* use = find_users(passes, prev_mem); use; use = use->next) {
            TB_Node* use_n = use->n;
            if (use_n->type == TB_LOAD) {
                sched_walk(passes, ws, phi_vals, bb, use_n);
            }
        }
    }

    // if we're a branch, push our PHI nodes
    if (n->type == TB_BRANCH) {
        TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
        TB_Node** succ = br->succ;

        FOREACH_N(i, 0, br->succ_count) {
            TB_Node* dst = br->succ[i];

            // find predecessor index and do that edge
            ptrdiff_t phi_index = -1;
            FOREACH_N(j, 0, dst->input_count) {
                TB_Node* pred = tb_get_parent_region(dst->inputs[j]);

                if (pred == bb) {
                    phi_index = j;
                    break;
                }
            }
            if (phi_index < 0) continue;

            // schedule PHIs
            for (User* use = find_users(passes, dst); use; use = use->next) {
                TB_Node* phi = use->n;
                if (phi->type != TB_PHI) continue;

                TB_Node* val = phi->inputs[1 + phi_index];

                // reserve PHI space
                if (phi->dt.type != TB_MEMORY) {
                    if (phi_vals) {
                        PhiVal p;
                        p.phi = phi;
                        p.n   = val;
                        p.dst = -1;
                        p.src = -1;
                        dyn_array_put(*phi_vals, p);
                    }

                    // make sure there's a value for it
                    sched_walk(passes, ws, phi_vals, bb, val);
                } else {
                    // fill in the anti-dependencies for the memory PHI, it will handle
                    // the rest later.
                    int antis = 0;
                    for (User* use = find_users(passes, val); use; use = use->next) {
                        TB_Node* use_n = use->n;
                        if (use_n->type == TB_LOAD) {
                            sched_walk(passes, ws, phi_vals, bb, use_n);
                            antis++;
                        }
                    }

                    // if there's no anti-deps, let's just push the MEMORY
                    if (antis == 0) {
                        sched_walk(passes, ws, phi_vals, bb, val);
                    }
                }
            }
        }
    }

    dyn_array_put(ws->items, n);

    // push outputs (projections, if they apply)
    if (n->dt.type == TB_TUPLE) {
        for (User* use = find_users(passes, n); use; use = use->next) {
            TB_Node* use_n = use->n;
            if (use_n->type == TB_PROJ) {
                sched_walk(passes, ws, phi_vals, bb, use_n);
            }
        }
    }

    // push any leftovers that GCM linked here
    if (n->type == TB_REGION && (n->type == TB_PROJ && n->inputs[0]->type == TB_START)) {
        for (User* use = find_users(passes, n); use; use = use->next) {
            sched_walk(passes, ws, phi_vals, bb, use->n);
        }
    }
}
