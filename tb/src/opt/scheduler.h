
typedef struct {
    int index;
    int count;
    TB_User* users;
} Anti;

typedef struct SchedNode SchedNode;
struct SchedNode {
    SchedNode* parent;
    TB_ArenaSavepoint sp;

    TB_Node* n;
    int index;

    int anti_i;
    int anti_count;
    Anti antis[];
};

typedef struct {
    TB_Node* phi;
    TB_Node* n;
} SchedPhi;

static SchedNode* sched_make_node(TB_Arena* arena, SchedNode* parent, TB_Node* n) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    int anti_count = 0;
    if (n->type == TB_MERGEMEM) {
        anti_count = n->input_count - 2;
    } else if (n->type != TB_PHI && n->type != TB_PROJ) {
        if (is_mem_out_op(n) || n->dt.type == TB_MEMORY) {
            anti_count = 1;
        } else if (n->dt.type == TB_TUPLE) {
            FOR_USERS(u, n) {
                if (USERN(u)->type == TB_PROJ && USERN(u)->dt.type == TB_MEMORY) {
                    assert(USERI(u) == 0);
                    anti_count += 1;
                }
            }
            assert(anti_count < 2);
        }
    }

    SchedNode* s = tb_arena_alloc(arena, sizeof(SchedNode) + anti_count*sizeof(TB_User));
    *s = (SchedNode){ .parent = parent, .sp = sp, .n = n, .index = 0, .anti_count = anti_count };

    if (n->type == TB_MERGEMEM) {
        FOR_N(i, 2, n->input_count) {
            s->antis[i - 2] = (Anti){ 0, n->inputs[i]->user_count, n->inputs[i]->users };
        }
    } else if (anti_count == 1) {
        s->antis[0] = (Anti){ 0, n->inputs[1]->user_count, n->inputs[1]->users };
    }

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
    TB_Arena* arena = f->tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node* end = bb->end;

    // find phis
    size_t phi_curr = 0;
    ArenaArray(SchedPhi) phis = aarray_create(arena, SchedPhi, 32);

    if (end->type == TB_BRANCH) {
        FOR_USERS(u, end) {
            if (USERN(u)->type != TB_PROJ) continue;

            // we might have some memory phis over here if the projections aren't bbs
            ptrdiff_t search = nl_map_get(cfg->node_to_block, USERN(u));
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
    TB_Node* start = bb->id == 0 ? f->root_node : NULL;
    if (start) FOR_USERS(u, start) {
        if ((USERN(u)->type == TB_PROJ || USERN(u)->type == TB_MACH_PROJ) && !worklist_test_n_set(ws, USERN(u))) {
            dyn_array_put(ws->items, USERN(u));
        }
    }

    size_t leftovers = 0;
    size_t leftover_count = 1ull << bb->items.exp;

    while (top != NULL) {
        TB_Node* n = top->n;

        // resolve inputs first
        if (n->type != TB_PHI && top->index < n->input_count) {
            TB_Node* in = n->inputs[top->index++];
            if (in != NULL && sched_in_bb(f, ws, bb, in)) {
                top = sched_make_node(arena, top, in);
            }
            continue;
        }

        // resolve anti-deps
        if (top->anti_i < top->anti_count) {
            Anti* anti = &top->antis[top->anti_i];
            if (anti->index < anti->count) {
                TB_User* use = &anti->users[anti->index++];
                if (USERN(use) != n && USERI(use) == 1 && sched_in_bb(f, ws, bb, USERN(use))) {
                    top = sched_make_node(arena, top, USERN(use));
                }

                if (anti->index == anti->count) {
                    top->anti_i++;
                }
                continue;
            }
        }

        // resolve phi edges & leftovers when we're at the endpoint
        if (end == n) {
            // skip non-phis
            if (phi_curr < aarray_length(phis)) {
                TB_Node* phi = phis[phi_curr].phi;
                TB_Node* val = phis[phi_curr].n;
                phi_curr += 1;

                // reserve PHI space
                if (phi_vals && val->dt.type != TB_MEMORY) {
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
        if (n->dt.type == TB_TUPLE && n->type != TB_BRANCH && n->type != TB_ROOT) {
            FOR_USERS(u, n) {
                if ((USERN(u)->type == TB_PROJ || USERN(u)->type == TB_MACH_PROJ) && !worklist_test_n_set(ws, USERN(u))) {
                    dyn_array_put(ws->items, USERN(u));
                }
            }
        }
    }

    tb_arena_restore(arena, sp);
}
