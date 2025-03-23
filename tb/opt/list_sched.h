
typedef struct {
    TB_Node* n;
    int end;
    int unit_i;
} InFlight;

typedef struct {
    TB_Node* n;
    uint64_t unit_mask;
    int latency;
} ReadyNode;

static int count_waiting_deps(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    // we also care about extra edges here so we're iterating on input_cap
    int waiting = 0;
    FOR_N(i, 0, n->input_cap) {
        TB_Node* in = n->inputs[i];
        if (in && f->scheduled[in->gvn] == bb && in->type != TB_MACH_TEMP && !worklist_test(ws, in)) {
            waiting++;
        }
    }

    return waiting;
}

typedef struct {
    TB_Function* f;
    TB_GetLatency get_lat;
    TB_GetUnitMask get_unit_mask;

    Set ready_set;
    ArenaArray(ReadyNode) ready;
} ListSched;

// should probably move this out, it's useful elsewhere
static void ready_up(ListSched* sched, TB_Node* n, TB_Node* end) {
    if (set_get(&sched->ready_set, n->gvn) || n->type == TB_MACH_TEMP) {
        return;
    }

    CUIK_TIMED_BLOCK("add ready") {
        int latency = sched->get_lat(sched->f, n, end);
        uint64_t unit_mask = sched->get_unit_mask(sched->f, n);

        // TB_OPTDEBUG(SCHEDULE)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf(" (lat=%d, mask=%#08lx)\n", latency, unit_mask));
        set_put(&sched->ready_set, n->gvn);

        // projections are readied but not in the ready list
        FOR_USERS(u, n) {
            if (is_proj(USERN(u))) { set_put(&sched->ready_set, USERN(u)->gvn); }
        }

        ReadyNode r = { n, unit_mask, latency };
        aarray_push(sched->ready, r);
    }
}

static bool can_ready_user(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, Set* ready_set, TB_Node* n) {
    return !set_get(ready_set, n->gvn) && f->scheduled[n->gvn] == bb && !worklist_test(ws, n) && count_waiting_deps(f, ws, bb, n) == 0;
}

static bool is_real_datatype(TB_DataType dt) {
    return TB_IS_INT_OR_PTR(dt) || TB_IS_VECTOR_TYPE(dt) || TB_IS_FLOAT_TYPE(dt);
}

// hands you the best ready candidate, the ready list is sorted by latency but
// there's a few other bits which might skew scheduling, for now those are:
// * Condition attached to the terminator branch should be scheduled right before it.
//
// returns an index from the ready array (or -1 when it can't find an answer)
static int best_ready_node(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, ListSched* sched, int unit_i) {
    uint64_t unit_mask = 1ull << unit_i;

    TB_OPTDEBUG(SCHEDULE)(printf("Best on Port%d?\n", unit_i));

    int best_i = -1;
    int best_score = 0;
    FOR_REV_N(i, 0, aarray_length(sched->ready)) {
        TB_Node* n = sched->ready[i].n;
        uint64_t unit_mask = sched->ready[i].unit_mask;
        int lat = sched->ready[i].latency;

        // actually fits on the available machine
        if (((unit_mask >> unit_i) & 1) == 0) { continue; }

        // high latency ops should be scheduled first
        int score = lat*100;

        // things which have lots of inputs are ideally processed first
        FOR_N(j, 0, n->input_count) {
            if (n->inputs[j] && is_real_datatype(n->inputs[j]->dt)) {
                score += 10;
            }
        }

        // bump the score if we're chained to the previous dispatched op
        TB_ASSERT(dyn_array_length(ws->items) > 0);
        TB_Node* prev_op = ws->items[dyn_array_length(ws->items) - 1];
        if (prev_op->user_count == 1 && USERN(&prev_op->users[0]) == n) {
            score += 200;
        }

        // things which produce more values are cringe
        if (is_real_datatype(n->dt)) {
            score -= 10;
        }

        // if we have a single use and it's waiting on us? then we
        // really wanna schedule, if it's waiting on a lot of others then
        // we want them to fill up first
        if (n->user_count == 1) {
            TB_Node* use = USERN(&n->users[0]);

            if (count_waiting_deps(f, ws, bb, use) == 1) {
                score += use == bb->end ? -200 : 100;
                if (score < 1) { score = 1; }
            }
        }

        #if TB_OPTDEBUG_SCHEDULE
        printf("  ");
        tb_print_dumb_node(NULL, n);
        printf("  score=%d\n", score);
        #endif

        if (score > best_score) {
            best_i = i;
            best_score = score;
        }
    }

    return best_i;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count) {
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    TB_OPTDEBUG(SCHEDULE)(printf("BB %zu\n", bb - cfg->blocks));
    worklist_push(ws, bb->start);
    // TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, bb->start), printf("\n"));

    // first block has access to root's users
    int id = bb - cfg->blocks;
    if (id == 0) {
        FOR_USERS(u, f->root_node) {
            if (USERN(u)->type == TB_MACH_FRAME_PTR) {
                TB_ASSERT(USERI(u) == 0);
                worklist_push(ws, USERN(u));
            }
        }

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
        if (!worklist_test(ws, n) && f->scheduled[n->gvn] == bb && count_waiting_deps(f, ws, bb, n) == 0) {
            ready_up(&sched, n, end);
        }
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

            // TB_OPTDEBUG(SCHEDULE)(printf("  RETIRE   "), tb_print_dumb_node(NULL, n), printf("\n"));

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

                int idx = best_ready_node(f, ws, bb, &sched, i);
                if (idx < 0) { continue; }

                TB_ASSERT((sched.ready[idx].unit_mask >> i) & 1);
                TB_Node* n = sched.ready[idx].n;
                in_use_mask |= 1ull << i;
                stall = false;

                aarray_remove(sched.ready, idx);
                TB_ASSERT(!is_proj(n));

                int end_cycle = cycle + get_lat(f, n, end);
                aarray_push(active, (InFlight){ n, end_cycle, i });

                if (n != end) {
                    // idk, it's ugly and a bookkeeping op
                    if (n->type != TB_CALLGRAPH) {
                        // push any temporaries for the node right before
                        FOR_N(i, 0, n->input_count) if (n->inputs[i] && n->inputs[i]->type == TB_MACH_TEMP) {
                            worklist_push(ws, n->inputs[i]);
                        }

                        worklist_push(ws, n);
                    }

                    // make sure to place all projections directly after their tuple node
                    TB_ASSERT(!cfg_is_fork(n));
                    FOR_USERS(u, n) if (is_proj(USERN(u))) {
                        TB_ASSERT(USERI(u) == 0);
                        TB_ASSERT(!worklist_test(ws, USERN(u)));
                        worklist_push(ws, USERN(u));
                    }

                    TB_OPTDEBUG(SCHEDULE)(printf("  DISPATCH "), tb_print_dumb_node(NULL, n), printf("\n"));
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


