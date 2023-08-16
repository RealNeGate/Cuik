////////////////////////////////
// Early scheduling
////////////////////////////////
// schedule nodes below any of their pinned dependencies
static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_START && n->type <= TB_TRAP) || n->type == TB_LOAD || n->type == TB_PHI || (n->type == TB_PROJ && n->dt.type == TB_CONTROL);
}

static void schedule_early(TB_Passes* passes, NL_HashSet* visited, TB_Node* n) {
    if (!nl_hashset_put(visited, n)) {
        // already visited
        return;
    }

    if (is_pinned(n)) {
        // pinned nodes will schedule their inputs but they themselves can't move
        tb_assert(n->inputs[0], "needs a control node already");
        FOREACH_N(i, 1, n->input_count) {
            schedule_early(passes, visited, n->inputs[i]);
        }
    } else {
        if (n->inputs[0] == NULL) {
            // add_user without remove because we know there's nothing there
            TB_Node* root = passes->f->start_node;
            n->inputs[0] = root;
            add_user(passes, n, root, 0, NULL);
        }

        TB_Node* best = tb_get_parent_region(n->inputs[0]);
        int best_depth = TB_NODE_GET_EXTRA_T(best, TB_NodeRegion)->dom_depth;

        FOREACH_N(i, 1, n->input_count) {
            schedule_early(passes, visited, n->inputs[i]);

            // choose deepest block
            TB_Node* bb = n->inputs[i]->inputs[0];
            if (UNLIKELY(bb->type != TB_START && bb->type != TB_REGION)) {
                do {
                    bb = bb->inputs[0];
                } while (bb->type != TB_START && bb->type != TB_REGION);
            }

            int bb_depth = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->dom_depth;
            if (best_depth < bb_depth) {
                best = bb;
                best_depth = bb_depth;
            }
        }

        set_input(passes, n, best, 0);
    }
}

static void schedule_region(TB_Passes* passes, NL_HashSet* visited, TB_Node* n) {
    TB_Node* parent = n->inputs[0];
    if (parent->type != TB_START && parent->type != TB_REGION) {
        schedule_region(passes, visited, parent);
    }

    FOREACH_N(i, 1, n->input_count) {
        schedule_early(passes, visited, n->inputs[i]);
    }
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

static void schedule_late(TB_Passes* passes, NL_HashSet* visited, TB_Node* n) {
    // uses doubles as the visited map for this function
    if (!nl_hashset_put(visited, n) || is_pinned(n)) {
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

        schedule_late(passes, visited, y);

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

    tb_assert(lca, "missing least common ancestor");
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

void tb_pass_schedule(TB_Passes* passes) {
    // Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
    //   https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf
    CUIK_TIMED_BLOCK("schedule") {
        NL_HashSet visited = nl_hashset_alloc(passes->f->node_count);

        CUIK_TIMED_BLOCK("early schedule") {
            FOREACH_REVERSE_N(i, 0, passes->order.count) {
                TB_Node* bb = passes->order.traversal[i];

                // schedule all pinned instructions
                schedule_region(passes, &visited, TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end);
            }
        }

        // generate instruction list we can walk
        DynArray(TB_Node*) worklist = NULL;

        CUIK_TIMED_BLOCK("gen worklist") {
            nl_hashset_clear(&visited);
            FOREACH_N(i, 0, passes->order.count) {
                TB_Node* bb = passes->order.traversal[i];
                postorder_all_nodes(&visited, &worklist, TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end);
            }
        }

        // move nodes closer to their usage site
        CUIK_TIMED_BLOCK("late schedule") {
            nl_hashset_clear(&visited);
            FOREACH_REVERSE_N(i, 0, dyn_array_length(worklist)) {
                TB_Node* n = worklist[i];

                if (is_pinned(n)) {
                    nl_hashset_put(&visited, n);

                    for (User* use = find_users(passes, n); use; use = use->next) {
                        if (use->n->inputs[0] != NULL) {
                            schedule_late(passes, &visited, use->n);
                        }
                    }
                } else if (n->input_count == 1) {
                    // this is gonna usually be the constants
                    schedule_late(passes, &visited, worklist[i]);
                }
            }
        }

        CUIK_TIMED_BLOCK("freeing") {
            dyn_array_destroy(worklist);
            nl_hashset_free(visited);
        }
    }
}
