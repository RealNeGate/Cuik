
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
    if (n->type == TB_CALLGRAPH) {
        return false;
    }

    // we also care about extra edges here so we're iterating on input_cap
    FOR_N(i, 0, n->input_cap) {
        TB_Node* in = n->inputs[i];
        if (in && f->scheduled[in->gvn] == bb && !worklist_test(ws, in)) {
            return false;
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
    if (set_get(&sched->ready_set, n->gvn)) {
        return;
    }

    CUIK_TIMED_BLOCK("add ready") {
        int prio           = sched->get_lat(sched->f, n, end);
        uint64_t unit_mask = sched->get_unit_mask(sched->f, n);

        // TB_OPTDEBUG(SCHEDULE)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf(" (lat=%d)\n", prio));
        set_put(&sched->ready_set, n->gvn);

        // projections are readied but not in the ready list
        if (n->dt.type == TB_TAG_TUPLE) FOR_USERS(u, n) {
            if (is_proj(USERN(u))) { set_put(&sched->ready_set, USERN(u)->gvn); }
        }

        // sorted insertion
        size_t i = 0, count = aarray_length(sched->ready);
        for (; i < count; i++) {
            if (prio < sched->ready[i].prio) { break; }
        }

        aarray_push(sched->ready, (ReadyNode){ 0 });
        memmove(&sched->ready[i + 1], &sched->ready[i], (count - i) * sizeof(ReadyNode));
        sched->ready[i] = (ReadyNode){ n, unit_mask, prio };
    }
}

static void remove_ready(ListSched* sched, size_t i) {
    CUIK_TIMED_BLOCK("remove ready") {
        // remove from being ready (ordered)
        if (i + 1 < aarray_length(sched->ready)) {
            memmove(&sched->ready[i], &sched->ready[i + 1], (aarray_length(sched->ready) - (i + 1)) * sizeof(ReadyNode));
        }
        aarray_pop(sched->ready);
    }
}

static bool can_ready_user(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, Set* ready_set, TB_Node* n) {
    return !set_get(ready_set, n->gvn) && f->scheduled[n->gvn] == bb && !worklist_test(ws, n) && is_node_ready(f, ws, bb, n);
}

// hands you the best ready candidate, the ready list is sorted by latency but
// there's a few other bits which might skew scheduling, for now those are:
// * Condition attached to the terminator branch should be scheduled right before it.
//
// returns an index from the ready array (or -1 when it can't find an answer)
static int best_ready_node(ListSched* sched, int unit_i) {
    uint64_t unit_mask = 1ull << unit_i;

    // nothing else we could do
    int len = aarray_length(sched->ready);
    if (len > 1) {
        FOR_REV_N(i, 0, len) {
            TB_Node* n = sched->ready[i].n;

            // delay branch compares
            if (n == sched->cmp) { continue; }

            // delay moves
            if (n->type == TB_MACH_MOVE) { continue; }

            // actually fits on the available machines
            uint64_t avail = sched->ready[i].unit_mask & ~unit_mask;
            if (avail == 0) { continue; }

            return i;
        }
    }

    // if we couldn't find a good answer, we still need to make progress
    // so we pick the lowest latency fella
    uint64_t avail = sched->ready[len - 1].unit_mask & ~unit_mask;
    return avail ? len - 1 : -1;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count) {
    TB_ASSERT(phi_vals == NULL && "TODO");
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    TB_OPTDEBUG(SCHEDULE)(printf("BB %zu\n", bb - cfg->blocks));
    worklist_push(ws, bb->start);
    // TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, bb->start), printf("\n"));

    // first block has access to root's users
    int id = bb - cfg->blocks;
    if (id == 0) {
        FOR_USERS(u, f->root_node) {
            if (is_proj(USERN(u))) {
                TB_ASSERT(USERI(u) == 0);
                // TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

                worklist_push(ws, USERN(u));
            }
        }
    } else {
        FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
            TB_ASSERT(USERI(u) == 0);
            // TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

            worklist_push(ws, USERN(u));
        }
    }

    // wanna move the increment op to the end of an affine loop
    TB_Node* end = bb->end;

    int cycle = 0;
    ArenaArray(InFlight) active = aarray_create(&f->tmp_arena, InFlight, 32);

    ListSched sched = {
        .f = f, .get_lat = get_lat, .get_unit_mask = get_unit_mask
    };
    sched.ready_set = set_create_in_arena(&f->tmp_arena, f->node_count);
    sched.ready     = aarray_create(&f->tmp_arena, ReadyNode, 32);

    // fill up initial ready list (everything used by the live-ins)
    aarray_for(i, bb->items) {
        TB_Node* n = bb->items[i];
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
        cuikperf_region_start("step", NULL);
        bool stall = true;

        #if TB_OPTDEBUG_SCHEDULE
        TB_Node* arr[64];
        FOR_N(i, 0, unit_count) {
            arr[i] = NULL;
        }

        for (size_t i = 0; i < aarray_length(active); i++) {
            arr[active[i].unit_i] = active[i].n;
        }

        printf("T=%3d: ", cycle);
        FOR_N(i, 0, unit_count) {
            if (in_use_mask & (1ull << i)) {
                printf("%%%-3u ", arr[i]->gvn);
            } else {
                printf("___  ");
            }
        }
        #endif

        // retire active nodes
        int retired = 0;
        for (size_t i = 0; i < aarray_length(active);) {
            TB_Node* n = active[i].n;
            if (active[i].end > cycle) { i++; continue; }

            in_use_mask &= ~(1ull << active[i].unit_i);
            aarray_remove(active, i);
            retired += 1;

            // TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: RETIRE   ", cycle), tb_print_dumb_node(NULL, n), printf("\n"));

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
        }

        #if TB_OPTDEBUG_SCHEDULE
        printf("  retired %d ops [ ", retired);
        FOR_N(i, 0, aarray_length(sched.ready)) {
            printf("%%%u ", sched.ready[i].n->gvn);
        }
        printf("]\n");
        #endif

        // dispatch one instruction per unit per cycle
        CUIK_TIMED_BLOCK("dispatch") {
            FOR_N(i, 0, unit_count) {
                if ((in_use_mask >> i) & 1) { continue; }

                int idx = best_ready_node(&sched, i);
                if (idx < 0) { continue; }

                TB_Node* n = sched.ready[idx].n;
                in_use_mask |= 1ull << i;
                stall = false;

                remove_ready(&sched, idx);
                TB_ASSERT(!is_proj(n));

                int end_cycle = cycle + get_lat(f, n, end);
                aarray_push(active, (InFlight){ n, end_cycle, i });

                if (n != end) {
                    worklist_push(ws, n);

                    // make sure to place all projections directly after their tuple node
                    if (n->dt.type == TB_TAG_TUPLE) {
                        TB_ASSERT(!cfg_is_fork(n));
                        FOR_USERS(u, n) if (is_proj(USERN(u))) {
                            TB_ASSERT(USERI(u) == 0);
                            TB_ASSERT(!worklist_test(ws, USERN(u)));
                            worklist_push(ws, USERN(u));
                        }
                    }

                    // TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, n), printf(" (on machine %d, until t=%d)\n", unit_i, end_cycle));
                }
            }
        }

        cuikperf_region_end();
        cycle += 1;
    }

    // place end node
    if (end != bb->start) {
        // TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, end), printf("\n"));
        worklist_push(ws, end);
    }

    tb_arena_restore(&f->tmp_arena, sp);
}


