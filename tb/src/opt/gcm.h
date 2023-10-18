// Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf
static uint32_t node_hash(void* a) { return ((TB_Node*) a)->gvn; }
static bool node_compare(void* a, void* b) { return a == b; }

////////////////////////////////
// Early scheduling
////////////////////////////////
static TB_BasicBlock* schedule_early(TB_Passes* p, TB_Node* n) {
    // already visited
    if (worklist_test_n_set(&p->worklist, n)) {
        return NULL;
    }

    // schedule inputs first
    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
        schedule_early(p, n->inputs[i]);
    }

    TB_BasicBlock* best = NULL;
    if (is_pinned(n) && n->input_count != 0) {
        ptrdiff_t search = nl_map_get(p->scheduled, n->inputs[0]);
        if (search >= 0) {
            best = p->scheduled[search].v;
        } else {
            // default to entry
            best = nl_map_get_checked(p->scheduled, p->worklist.items[0]);
        }
    } else {
        // start at the entry point
        best = nl_map_get_checked(p->scheduled, p->worklist.items[0]);
        int best_depth = 0;

        // choose deepest block
        FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
            ptrdiff_t search = nl_map_get(p->scheduled, n->inputs[i]);
            TB_BasicBlock* bb;
            if (search >= 0) {
                bb = p->scheduled[search].v;
            } else {
                bb = nl_map_get_checked(p->scheduled, p->worklist.items[0]);
            }

            if (best_depth < bb->dom_depth) {
                best_depth = bb->dom_depth;
                best = bb;
            }
        }
    }

    DO_IF(TB_OPTDEBUG_GCM)(printf("%s: v%u into .bb%d\n", p->f->super.name, n->gvn, best->id));

    nl_hashset_put2(&best->items, n, node_hash, node_compare);
    nl_map_put(p->scheduled, n, best);
    return best;
}

////////////////////////////////
// Late scheduling
////////////////////////////////
// schedule nodes such that they appear the least common
// ancestor to all their users
static TB_BasicBlock* find_lca(TB_Passes* p, TB_BasicBlock* a, TB_BasicBlock* b) {
    if (a == NULL) return b;

    // line both up
    while (a->dom_depth > b->dom_depth) a = nl_map_get_checked(p->scheduled, a->dom);
    while (b->dom_depth > a->dom_depth) b = nl_map_get_checked(p->scheduled, b->dom);

    while (a != b) {
        b = idom_bb(p, b);
        a = idom_bb(p, a);
    }

    return a;
}

static void place_in_block(TB_Passes* p, TB_Node* n, TB_BasicBlock* bb) {
    ptrdiff_t search = nl_map_get(p->scheduled, n);
    if (search >= 0) {
        // replace old
        TB_BasicBlock* old = p->scheduled[search].v;
        p->scheduled[search].v = bb;
        nl_hashset_remove2(&old->items, n, node_hash, node_compare);
    } else {
        nl_map_put(p->scheduled, n, bb);
    }

    nl_hashset_put2(&bb->items, n, node_hash, node_compare);
}

static void simple_schedule_late(TB_Passes* p, TB_Node* n, TB_BasicBlock* lca) {
    // already visited
    if (worklist_test_n_set(&p->worklist, n) || is_pinned(n)) {
        return;
    }

    if (n->users->next == NULL) {
        // if we're the sole user (usually with immediates and other
        // simple values) we should just make them happen late.
        place_in_block(p, n, lca);

        // try with inputs
        FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
            simple_schedule_late(p, n->inputs[i], lca);
        }
    }
}

static void schedule_late(TB_Passes* p, TB_Node* n) {
    // already visited
    if (worklist_test_n_set(&p->worklist, n)) {
        return;
    }

    // schedule all users first
    for (User* use = n->users; use; use = use->next) {
        schedule_late(p, use->n);
    }

    // pinned nodes can't be rescheduled
    if (is_pinned(n)) {
        return;
    }

    // we're gonna find the least common ancestor
    TB_BasicBlock* lca = NULL;
    for (User* use = n->users; use; use = use->next) {
        TB_Node* y = use->n;

        ptrdiff_t search = nl_map_get(p->scheduled, y);
        if (search < 0) continue; // dead

        TB_BasicBlock* use_block = p->scheduled[search].v;
        if (y->type == TB_PHI) {
            TB_Node* use_node = y->inputs[0];
            assert(use_node->type == TB_REGION);

            if (y->input_count != use_node->input_count + 1) {
                tb_panic("phi has parent with mismatched predecessors");
            }

            ptrdiff_t j = 1;
            for (; j < y->input_count; j++) {
                if (y->inputs[j] == n) {
                    break;
                }
            }
            assert(j >= 0);

            use_block = nl_map_get_checked(p->scheduled, use_node->inputs[j - 1]);
        }

        lca = find_lca(p, lca, use_block);
    }

    TB_OPTDEBUG(GCM)(
        printf("  LATE v%u into .bb%d: ", n->gvn, lca->id),
        print_node_sexpr(n, 0),
        printf("\n")
    );

    // tb_assert(lca, "missing least common ancestor");
    if (lca != NULL) {
        place_in_block(p, n, lca);
    }

    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
        simple_schedule_late(p, n->inputs[i], lca);
    }
}

void tb_pass_schedule(TB_Passes* p, TB_CFG cfg) {
    if (p->scheduled != NULL) {
        nl_map_free(p->scheduled);
    }

    CUIK_TIMED_BLOCK("schedule") {
        Worklist* restrict ws = &p->worklist;
        nl_map_create(p->scheduled, 256);

        CUIK_TIMED_BLOCK("dominators") {
            // jarvis pull up the dommies
            tb_compute_dominators(p->f, p, cfg);

            worklist_clear_visited(ws);
            FOREACH_N(i, 0, cfg.block_count) {
                TB_BasicBlock* best = &nl_map_get_checked(cfg.node_to_block, ws->items[i]);
                if (i == 0) {
                    worklist_test_n_set(ws, p->f->start_node);
                    nl_map_put(p->scheduled, p->f->start_node, best);
                }

                best->items = nl_hashset_alloc(32);
                nl_map_put(p->scheduled, ws->items[i], best);
                worklist_test_n_set(ws, ws->items[i]);
            }
        }

        CUIK_TIMED_BLOCK("early schedule") {
            FOREACH_N(i, 0, cfg.block_count) {
                TB_Node* end = nl_map_get_checked(cfg.node_to_block, ws->items[i]).end;
                schedule_early(p, end);

                TB_BasicBlock* bb = nl_map_get_checked(p->scheduled, end);
                for (User* use = end->users; use; use = use->next) {
                    TB_Node* proj = use->n;

                    if (proj->type == TB_PROJ && !worklist_test_n_set(&p->worklist, proj)) {
                        nl_hashset_put2(&bb->items, proj, node_hash, node_compare);
                        nl_map_put(p->scheduled, proj, bb);
                    }
                }
            }
        }

        // move nodes closer to their usage site
        CUIK_TIMED_BLOCK("late schedule") {
            worklist_clear_visited(ws);
            schedule_late(p, ws->items[0]);
        }
    }
}
