
typedef struct SchedNode SchedNode;
struct SchedNode {
    SchedNode* parent;

    TB_Node* n;
    int index;
    User* antis;
};

static SchedNode* sched_make_node(TB_Arena* arena, SchedNode* parent, TB_Node* n) {
    SchedNode* s = TB_ARENA_ALLOC(arena, SchedNode);
    *s = (SchedNode){ .parent = parent, .n = n, .index = 0 };

    if (is_mem_out_op(n) && n->type != TB_PHI && n->type != TB_PROJ) {
        s->antis = n->inputs[1]->users;
    }

    return s;
}

static bool sched_in_bb(TB_Passes* passes, Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    ptrdiff_t search = nl_map_get(passes->scheduled, n);
    return search >= 0 && passes->scheduled[search].v == bb && !worklist_test_n_set(ws, n);
}

// basically just topological sort, no fancy shit
void greedy_scheduler(TB_Passes* passes, Worklist* ws, DynArray(PhiVal)* phi_vals, TB_BasicBlock* bb, TB_Node* end) {
    TB_Arena* arena = tmp_arena;

    SchedNode* top = sched_make_node(arena, NULL, end);
    worklist_test_n_set(ws, end);

    // find phis
    int phi_i = 0;
    User* phis = NULL;
    if (end->type != TB_BRANCH) {
        for (User* u = end->users; u; u = u->next) {
            if (u->slot == 0 && u->n->type == TB_REGION) {
                phi_i = u->slot;
                phis = u->n->users;
                break;
            }
        }
    }

    size_t leftovers = 0;
    size_t leftover_count = 1ull << bb->items.exp;

    while (top != NULL) {
        TB_Node* n = top->n;

        // resolve inputs first
        if (top->index < n->input_count) {
            TB_Node* in = n->inputs[top->index++];
            if (in != NULL && sched_in_bb(passes, ws, bb, in)) {
                top = sched_make_node(arena, top, in);
            }
            continue;
        }

        // skip non-memory edges
        while (top->antis && (top->antis->slot != 1 || top->antis->n == n)) {
            top->antis = top->antis->next;
        }

        // resolve anti-deps
        if (top->antis != NULL && sched_in_bb(passes, ws, bb, top->antis->n)) {
            assert(top->antis->slot == 1 && top->antis->n != n);

            TB_Node* anti = top->antis->n;
            top->antis = top->antis->next;

            top = sched_make_node(arena, top, anti);
            continue;
        }

        // resolve phi edges when we're at the endpoint
        if (end == n) {
            // skip non-phis
            while (phis && phis->n->type != TB_PHI) {
                phis = phis->next;
            }

            if (phis && sched_in_bb(passes, ws, bb, phis->n->inputs[1 + phi_i])) {
                assert(phis->n->type == TB_PHI);
                TB_Node* val = phis->n->inputs[1 + phi_i];

                // reserve PHI space
                if (phi_vals && val->dt.type != TB_MEMORY) {
                    PhiVal p;
                    p.phi = phis->n;
                    p.n   = val;
                    p.dst = -1;
                    p.src = -1;
                    dyn_array_put(*phi_vals, p);
                }

                top = sched_make_node(arena, top, val);
                phis = phis->next;
                continue;
            }

            // resolve leftover nodes placed here by GCM
            while (leftovers < leftover_count && (bb->items.data[leftovers] == NULL || bb->items.data[leftovers] == NL_HASHSET_TOMB)) {
                leftovers++;
            }

            if (leftovers < leftover_count && sched_in_bb(passes, ws, bb, bb->items.data[leftovers])) {
                top = sched_make_node(arena, top, bb->items.data[leftovers]);
                leftovers += 1;
                continue;
            }
        }

        dyn_array_put(ws->items, n);
        top = top->parent;

        // push outputs (projections, if they apply)
        if (n->dt.type == TB_TUPLE && n->type != TB_BRANCH) {
            for (User* use = n->users; use; use = use->next) {
                if (use->n->type == TB_PROJ && !worklist_test_n_set(ws, use->n)) {
                    dyn_array_put(ws->items, use->n);
                }
            }
        }
    }
}
