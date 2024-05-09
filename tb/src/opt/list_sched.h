
typedef struct {
    TB_Node* n;
    int end;
    int unit_i;
} InFlight;

typedef struct {
    TB_Node* n;
    uint64_t unit_mask;
    int prio;
} ReadyNode;

static bool is_node_ready(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    FOR_N(i, 0, n->input_count) {
        TB_Node* in = n->inputs[i];
        if (in && f->scheduled[in->gvn] == bb && !worklist_test(ws, in)) {
            return false;
        }
    }

    // anti-deps
    if (is_mem_out_op(n) || n->dt.type == TB_TAG_MEMORY) {
        FOR_USERS(u, n->inputs[1]) {
            TB_Node* un = USERN(u);
            if (n != un && USERI(u) == 1) {
                // wait for anti-deps (maybe we should
                // add these as true deps)
                if (f->scheduled[un->gvn] == bb && !worklist_test(ws, un)) {
                    return false;
                }
            }
        }
    }

    return true;
}

typedef struct {
    TB_Function* f;
    TB_GetLatency get_lat;
    TB_GetUnitMask get_unit_mask;

    TB_Node* cmp;
    Set ready_set;
    ArenaArray(ReadyNode) ready;
} ListSched;

// should probably move this out, it's useful elsewhere
static void ready_up(ListSched* sched, TB_Node* n, TB_Node* end) {
    int prio           = sched->get_lat(sched->f, n, end);
    uint64_t unit_mask = sched->get_unit_mask(sched->f, n);

    TB_OPTDEBUG(SCHEDULE)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf("\n"));
    set_put(&sched->ready_set, n->gvn);

    // projections are readied but not in the ready list
    if (n->dt.type == TB_TAG_TUPLE) FOR_USERS(u, n) {
        if (is_proj(USERN(u))) { set_put(&sched->ready_set, USERN(u)->gvn); }
    }

    // sorted insertion
    size_t i = 0, count = aarray_length(sched->ready);
    for (; i < count; i++) {
        if (prio < sched->ready[i].prio) break;
    }

    aarray_push(sched->ready, (ReadyNode){ 0 });
    memmove(&sched->ready[i + 1], &sched->ready[i], (count - i) * sizeof(ReadyNode));
    sched->ready[i] = (ReadyNode){ n, unit_mask, prio };
}

static void remove_ready(ListSched* sched, size_t i) {
    // remove from being ready (ordered)
    if (i + 1 < aarray_length(sched->ready)) {
        memmove(&sched->ready[i], &sched->ready[i + 1], (aarray_length(sched->ready) - (i + 1)) * sizeof(ReadyNode));
    }
    aarray_pop(sched->ready);
}

static bool can_ready_user(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, Set* ready_set, TB_Node* n) {
    return !set_get(ready_set, n->gvn) && f->scheduled[n->gvn] == bb && !worklist_test(ws, n) && is_node_ready(f, ws, bb, n);
}

// hands you the best ready candidate, the ready list is sorted by latency but
// there's a few other bits which might skew scheduling, for now those are:
// * Condition attached to the terminator branch should be scheduled right before it.
//
// returns an index from the ready array (or -1 when it can't find an answer)
static int best_ready_node(ListSched* sched, uint64_t in_use_mask) {
    // nothing else we could do
    int len = aarray_length(sched->ready);
    if (len == 1) {
        // machines available? if not we'll have to wait
        uint64_t avail = sched->ready[0].unit_mask & ~in_use_mask;
        return avail ? 0 : -1;
    }

    while (len--) {
        TB_Node* n = sched->ready[len].n;

        // delay branch compares
        if (n == sched->cmp) { continue; }

        // actually fits on the available machines
        uint64_t avail = sched->ready[len].unit_mask & ~in_use_mask;
        if (avail == 0) { continue; }

        return len;
    }

    return -1;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count) {
    assert(phi_vals == NULL && "TODO");
    TB_Arena* tmp_arena = f->tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);

    TB_OPTDEBUG(SCHEDULE)(printf("BB %d\n", bb->id));
    worklist_push(ws, bb->start);

    // first block has access to root's users
    if (bb->id == 0) {
        TB_Node* root = f->root_node;
        FOR_USERS(u, root) {
            if (is_proj(USERN(u))) {
                assert(USERI(u) == 0);
                TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

                worklist_push(ws, USERN(u));
            }
        }
    } else {
        FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
            assert(USERI(u) == 0);
            TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

            worklist_push(ws, USERN(u));
        }
    }

    // wanna move the increment op to the end of an affine loop
    TB_Node* end = bb->end;

    int cycle = 0;
    ArenaArray(InFlight) active = aarray_create(tmp_arena, InFlight, 32);

    ListSched sched = {
        .f = f, .get_lat = get_lat, .get_unit_mask = get_unit_mask
    };
    sched.ready_set = set_create_in_arena(tmp_arena, f->node_count);
    sched.ready     = aarray_create(tmp_arena, ReadyNode, 32);

    // fill up initial ready list (everything used by the live-ins)
    nl_hashset_for(e, &bb->items) {
        TB_Node* n = *e;
        if (!worklist_test(ws, n) && f->scheduled[n->gvn] == bb && is_node_ready(f, ws, bb, n)) {
            ready_up(&sched, n, end);
        }
    }

    // TODO(NeGate): we shouldn't do this on VLIWs, ideally we schedule predication earlier there
    if ((end->type == TB_BRANCH || end->type == TB_AFFINE_LATCH) && f->scheduled[end->inputs[1]->gvn] == bb &&
        end->inputs[1]->user_count == 1 && !is_proj(end->inputs[1])) {
        sched.cmp = end->inputs[1];
    }

    uint64_t in_use_mask = 0;
    // what it looks like when all units are in use
    uint64_t blocked_mask = UINT64_MAX >> (64 - unit_count);

    // btw worklist has the final schedule
    while (aarray_length(active) > 0 || aarray_length(sched.ready) > 0) {
        bool stall = true;

        // dispatch one instruction per machine per cycle
        while (in_use_mask != blocked_mask && aarray_length(sched.ready) > 0) {
            int idx = best_ready_node(&sched, in_use_mask);
            if (idx < 0) { break; }

            uint64_t avail = sched.ready[idx].unit_mask & ~in_use_mask;
            int unit_i = tb_ffs64(avail) - 1;

            TB_Node* n = sched.ready[idx].n;
            in_use_mask |= 1ull << unit_i;
            stall = false;

            remove_ready(&sched, idx);

            assert(!is_proj(n));
            worklist_push(ws, n);

            // make sure to place all projections directly after their tuple node
            if (n->dt.type == TB_TAG_TUPLE && !cfg_is_fork(n)) {
                FOR_USERS(u, n) if (is_proj(USERN(u))) {
                    assert(USERI(u) == 0);
                    assert(!worklist_test(ws, USERN(u)));
                    worklist_push(ws, USERN(u));
                }
            }

            int end_cycle = cycle + get_lat(f, n, end);
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, n), printf(" (on machine %d, until t=%d)\n", unit_i, end_cycle));
            aarray_push(active, (InFlight){ n, end_cycle, unit_i });
        }

        if (stall) {
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: STALL\n", cycle));
        }
        cycle += 1;

        for (size_t i = 0; i < aarray_length(active);) {
            TB_Node* n = active[i].n;
            if (active[i].end > cycle) { i++; continue; }

            in_use_mask &= ~(1ull << active[i].unit_i);
            aarray_remove(active, i);
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: RETIRE   ", cycle), tb_print_dumb_node(NULL, n), printf("\n"));

            // instruction's retired, time to ready up users
            FOR_USERS(u, n) {
                TB_Node* un = USERN(u);
                if (is_proj(un)) {
                    // projections are where all the real users ended up
                    FOR_USERS(proj_u, un) {
                        if (can_ready_user(f, ws, bb, &sched.ready_set, USERN(proj_u))) {
                            ready_up(&sched, USERN(proj_u), end);
                        }
                    }
                } else if (can_ready_user(f, ws, bb, &sched.ready_set, un)) {
                    ready_up(&sched, un, end);
                }
            }

            // push all anti-deps
            if (is_mem_in_op(n) || (n->input_count >= 2 && n->inputs[1] && n->inputs[1]->dt.type == TB_TAG_MEMORY)) {
                FOR_USERS(u, n->inputs[1]) {
                    TB_Node* un = USERN(u);
                    if (n != un && USERI(u) == 1) {
                        if (un != end && can_ready_user(f, ws, bb, &sched.ready_set, un)) {
                            ready_up(&sched, un, end);
                        }
                    }
                }
            }
        }
    }

    tb_arena_restore(tmp_arena, sp);
}


