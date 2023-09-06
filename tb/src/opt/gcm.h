// Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf

////////////////////////////////
// Early scheduling
////////////////////////////////
// schedule nodes below any of their pinned dependencies
static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_START && n->type <= TB_SAFEPOINT_POLL) || n->type == TB_PROJ || n->type == TB_LOCAL;
}

static bool is_memory_writer(TB_Node* n) {
    return (n->type >= TB_CALL && n->type == TB_SYSCALL) && (n->type >= TB_STORE && n->type <= TB_ATOMIC_CAS);
}

static void schedule_early(TB_Passes* passes, TB_Node* n) {
    // already visited
    if (worklist_test_n_set(&passes->worklist, n)) {
        return;
    }

    // schedule inputs first
    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
        schedule_early(passes, n->inputs[i]);
    }

    if (!is_pinned(n)) {
        TB_Node* best = passes->f->start_node;
        int best_depth = 0;

        // choose deepest block
        FOREACH_N(i, 0, n->input_count) if (n->inputs[i] && n->inputs[i]->inputs[0]) {
            TB_Node* bb = get_block_begin(n->inputs[i]->inputs[0]);

            int bb_depth = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->dom_depth;
            if (best_depth < bb_depth) {
                best = bb;
                best_depth = bb_depth;
            }
        }

        if (passes->f->start_node == best) {
            best = passes->f->params[0];
        }

        set_input(passes, n, best, 0);
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

static void schedule_late(TB_Passes* passes, TB_Node* n) {
    // already visited
    if (worklist_test_n_set(&passes->worklist, n)) {
        return;
    }

    // schedule all users first
    for (User* use = find_users(passes, n); use; use = use->next) {
        schedule_late(passes, use->n);
    }

    // pinned nodes can't be rescheduled
    if (is_pinned(n)) {
        return;
    }

    // we're gonna find the least common ancestor
    TB_Node* lca = NULL;
    for (User* use = find_users(passes, n); use; use = use->next) {
        TB_Node* y = use->n;
        if (y->inputs[0] == NULL) continue; // dead

        TB_Node* use_block = tb_get_parent_region(y->inputs[0]);
        if (y->type == TB_PHI) {
            if (y->input_count != use_block->input_count + 1) {
                tb_panic("phi has parent with mismatched predecessors");
            }

            ptrdiff_t j = 0;
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

        CUIK_TIMED_BLOCK("early schedule") {
            worklist_clear_visited(ws);
            schedule_early(p, p->f->stop_node);
        }

        // move nodes closer to their usage site
        CUIK_TIMED_BLOCK("late schedule") {
            worklist_clear_visited(ws);
            schedule_late(p, p->f->start_node);
        }
    }
}
