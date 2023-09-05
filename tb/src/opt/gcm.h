// Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf

////////////////////////////////
// Early scheduling
////////////////////////////////
// schedule nodes below any of their pinned dependencies
static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_START && n->type <= TB_SAFEPOINT_POLL) || n->type == TB_PROJ;
}

static void schedule_early(TB_Passes* passes, TB_Node* n) {
    if (is_pinned(n) || worklist_test_n_set(&passes->worklist, n)) {
        // already visited
        return;
    }

    if (n->inputs[0] == NULL) {
        // add_user without remove because we know there's nothing there
        TB_Node* root = passes->f->start_node;
        n->inputs[0] = root;
        add_user(passes, n, root, 0, NULL);
    }

    TB_Node* best = get_block_begin(n->inputs[0]);
    int best_depth = TB_NODE_GET_EXTRA_T(best, TB_NodeRegion)->dom_depth;

    FOREACH_N(i, 1, n->input_count) {
        schedule_early(passes, n->inputs[i]);

        // choose deepest block
        TB_Node* bb = get_block_begin(n->inputs[i]->inputs[0]);

        int bb_depth = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->dom_depth;
        if (best_depth < bb_depth) {
            best = bb;
            best_depth = bb_depth;
        }
    }

    set_input(passes, n, best, 0);
}

////////////////////////////////
// Late scheduling
////////////////////////////////
// schedule nodes such that they appear the least common
// ancestor to all their users
static TB_Node* walk_up(TB_Node* a, TB_Node* b) {
    // if a is deeper, line it up with b
    int bdom = dom_depth(b);
    while (a->input_count > 0) {
        TB_Node* aa = tb_get_parent_region(a);
        if (dom_depth(aa) >= bdom) {
            break;
        }

        a = idom(aa);
    }

    return a;
}

static TB_Node* find_lca(TB_Node* a, TB_Node* b) {
    if (a == NULL) return b;

    // line both up
    a = walk_up(a, b);
    b = walk_up(b, a);

    while (a != b) {
        a = idom(a);
        if (a == NULL) return b;

        b = idom(b);
        if (b == NULL) return a;
    }

    return a;
}

static void schedule_late(TB_Passes* passes, TB_Node* n) {
    // uses doubles as the visited map for this function
    if (is_pinned(n) || worklist_test_n_set(&passes->worklist, n)) {
        // already visited
        return;
    }

    // we're gonna find the least common ancestor
    TB_Node* lca = NULL;
    for (User* use = find_users(passes, n); use; use = use->next) {
        // only care about control nodes
        if (use->slot == 0) continue;

        TB_Node* y = use->n;
        // dead node
        if (y->inputs[0] == NULL) continue;

        schedule_late(passes, y);

        TB_Node* use_block = tb_get_parent_region(y->inputs[0]);
        if (y->type == TB_PHI) {
            if (y->input_count != use_block->input_count + 1) {
                tb_panic("phi has parent with mismatched predecessors");
            }

            ptrdiff_t j = -1;
            for (; j < y->input_count; j++) {
                if (y->inputs[j] == n) {
                    break;
                }
            }
            assert(j >= 0);

            use_block = tb_get_parent_region(use_block->inputs[j - 1]);
        }

        lca = find_lca(lca, use_block);
    }

    // tb_assert(lca, "missing least common ancestor");
    set_input(passes, n, lca, 0);
}

// We'll be using this for late schedling
static void postorder_all_nodes(NL_HashSet* visited, DynArray(TB_Node*)* worklist, TB_Node* n) {
    if (!nl_hashset_put(visited, n)) {
        return;
    }

    // walk successors first
    FOREACH_N(i, 0, n->input_count) {
        postorder_all_nodes(visited, worklist, n->inputs[i]);
    }

    dyn_array_put(*worklist, n);
}

void tb_pass_schedule(TB_Passes* p) {
    if (p->scheduled) {
        return;
    }

    CUIK_TIMED_BLOCK("schedule") {
        Worklist* restrict ws = &p->worklist;
        p->scheduled = true;

        CUIK_TIMED_BLOCK("dominators") {
            worklist_clear(ws);

            size_t block_count = tb_push_postorder(p->f, &p->worklist);
            TB_Node** blocks   = &p->worklist.items[0];
            tb_compute_dominators(p->f, block_count, blocks);
        }

        CUIK_TIMED_BLOCK("gen worklist") {
            worklist_clear(ws);
            push_all_nodes(ws, p->f->start_node);
        }

        CUIK_TIMED_BLOCK("early schedule") {
            worklist_clear_visited(ws);
            FOREACH_REVERSE_N(i, 0, dyn_array_length(ws->items)) {
                TB_Node* n = ws->items[i];

                // schedule all pinned instructions
                if (is_pinned(n)) {
                    worklist_test_n_set(ws, n);

                    FOREACH_N(i, 0, n->input_count) {
                        schedule_early(p, n->inputs[i]);
                    }
                }
            }
        }

        // move nodes closer to their usage site
        CUIK_TIMED_BLOCK("late schedule") {
            worklist_clear_visited(ws);
            FOREACH_REVERSE_N(i, 0, dyn_array_length(ws->items)) {
                TB_Node* n = ws->items[i];

                if (is_pinned(n)) {
                    worklist_test_n_set(ws, n);
                    for (User* use = find_users(p, n); use; use = use->next) {
                        if (use->n->inputs[0] != NULL) {
                            schedule_late(p, use->n);
                        }
                    }
                }
            }
        }
    }
}
