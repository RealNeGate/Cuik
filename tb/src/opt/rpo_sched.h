
typedef struct SchedNode SchedNode;
struct SchedNode {
    SchedNode* parent;
    TB_ArenaSavepoint sp;

    TB_Node* n;
    int index;
};

typedef struct {
    TB_Node* phi;
    TB_Node* n;
} SchedPhi;

static SchedNode* sched_make_node(TB_Arena* arena, SchedNode* parent, TB_Node* n) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    SchedNode* s = tb_arena_alloc(arena, sizeof(SchedNode));
    *s = (SchedNode){ .parent = parent, .sp = sp, .n = n, .index = 0 };
    return s;
}

static bool sched_in_bb(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    return f->scheduled[n->gvn] == bb && !worklist_test_n_set(ws, n);
}

static void fill_phis(TB_Arena* arena, ArenaArray(SchedPhi)* phis, TB_Node* succ, int phi_i) {
    FOR_USERS(u, succ) {
        if (USERN(u)->type != TB_PHI) continue;
        SchedPhi p = { .phi = USERN(u), .n = USERN(u)->inputs[1 + phi_i] };
        aarray_push(*phis, p);
    }
}

// basically just topological sort, no fancy shit
void tb_greedy_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb) {
    TB_Arena* arena = &f->tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node* end = bb->end;

    // find phis
    size_t phi_curr = 0;
    ArenaArray(SchedPhi) phis = aarray_create(arena, SchedPhi, 32);

    if (cfg_is_fork(end)) {
        FOR_USERS(u, end) {
            TB_Node* un = USERN(u);
            if (!cfg_is_cproj(un)) continue;

            // we might have some memory phis over here if the projections aren't bbs
            ptrdiff_t search = nl_map_get(cfg->node_to_block, un);
            if (search >= 0) continue;

            TB_User* succ = cfg_next_user(end);
            if (cfg_is_region(USERN(succ))) {
                fill_phis(arena, &phis, USERN(succ), USERI(succ));
            }
        }
    } else if (!cfg_is_endpoint(end)) {
        TB_User* succ = cfg_next_user(end);
        if (cfg_is_region(USERN(succ))) {
            fill_phis(arena, &phis, USERN(succ), USERI(succ));
        }
    }

    SchedNode* top = sched_make_node(arena, NULL, end);
    worklist_test_n_set(ws, end);

    // reserve projections for the top
    if (bb == &cfg->blocks[0]) {
        FOR_USERS(u, f->root_node) {
            if (is_proj(USERN(u)) && !worklist_test_n_set(ws, USERN(u))) {
                dyn_array_put(ws->items, USERN(u));
            }
        }
    }

    size_t leftovers = 0;
    size_t leftover_count = 1ull << bb->items.exp;

    while (top != NULL) {
        TB_Node* n = top->n;

        // resolve inputs first (including the extra edges because that's where anti-deps go)
        if (n->type != TB_PHI && top->index < n->input_cap) {
            TB_Node* in = n->inputs[top->index++];
            if (in != NULL) {
                // projections don't get scheduled, their tuple node does
                if (is_proj(in)) { in = in->inputs[0]; }
                if (sched_in_bb(f, ws, bb, in)) {
                    top = sched_make_node(arena, top, in);
                }
            }
            continue;
        }

        // resolve phi edges & leftovers when we're at the endpoint
        if (end == n) {
            // skip non-phis
            if (phi_curr < aarray_length(phis)) {
                TB_Node* phi = phis[phi_curr].phi;
                TB_Node* val = phis[phi_curr].n;
                phi_curr += 1;

                // reserve PHI space
                if (phi_vals && val->dt.type != TB_TAG_MEMORY) {
                    PhiVal p;
                    p.phi = phi;
                    p.n   = val;
                    p.dst = -1;
                    p.src = -1;
                    dyn_array_put(*phi_vals, p);
                }

                if (sched_in_bb(f, ws, bb, val)) {
                    top = sched_make_node(arena, top, val);
                }
                continue;
            }

            // resolve leftover nodes placed here by GCM
            while (leftovers < leftover_count && (bb->items.data[leftovers] == NULL || bb->items.data[leftovers] == NL_HASHSET_TOMB)) {
                leftovers++;
            }

            if (leftovers < leftover_count) {
                if (!worklist_test_n_set(ws, bb->items.data[leftovers])) {
                    top = sched_make_node(arena, top, bb->items.data[leftovers]);
                }
                leftovers += 1;
                continue;
            }
        }

        dyn_array_put(ws->items, n);

        SchedNode* parent = top->parent;
        tb_arena_restore(arena, top->sp);
        top = parent;

        // push outputs (projections, if they apply)
        if (n->dt.type == TB_TAG_TUPLE && !cfg_is_fork(n) && n->type != TB_ROOT) {
            FOR_USERS(u, n) {
                if (is_proj(USERN(u)) && !worklist_test_n_set(ws, USERN(u))) {
                    dyn_array_put(ws->items, USERN(u));
                }
            }
        }
    }

    tb_arena_restore(arena, sp);
}
