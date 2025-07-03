
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

    int* depth;
} ListSched;

// assuming the inputs are on their final leg, what
// is the effect on register pressure.
static int pressure_delta(ListSched* sched, TB_Node* n) {
    int kills = 0;
    FOR_N(i, 0, n->input_count) {
        kills += (n->dt.type != TB_TAG_MEMORY);
    }

    // new lifetime, pressure increase
    int defs = 0;
    if (n->dt.type != TB_TAG_CONTROL && n->dt.type != TB_TAG_MEMORY) {
        defs += 1;
    }

    return defs - kills;
}

// should probably move this out, it's useful elsewhere
static void ready_up(ListSched* sched, TB_Node* n) {
    if (set_get(&sched->ready_set, n->gvn) || n->type == TB_MACH_TEMP) {
        return;
    }

    CUIK_TIMED_BLOCK("add ready") {
        int latency = sched->get_lat(sched->f, n, NULL);
        uint64_t unit_mask = sched->get_unit_mask(sched->f, n);

        TB_OPTDEBUG(SCHED1)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf(" (lat=%d, mask=%#"PRIx64")\n", latency, unit_mask));
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
static int best_ready_node(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* end, ListSched* sched, uint64_t unit_mask, bool can_stall) {
    #if 0
    TB_OPTDEBUG(SCHED1)(printf("Best on "));
    FOR_N(i, 0, 64) if ((unit_mask >> i) & 1) {
        TB_OPTDEBUG(SCHED1)(printf("Port%zu ", i));
    }
    TB_OPTDEBUG(SCHED1)(printf("?\n"));
    #endif

    if (aarray_length(sched->ready) == 1) {
        TB_Node* n = sched->ready[0].n;
        uint64_t op_unit_mask = sched->ready[0].unit_mask;
        if (unit_mask & op_unit_mask) {
            return 0;
        }
    }

    int best_i = -1;
    int best_score = 0;
    int best_depth = 0;
    FOR_REV_N(i, 0, aarray_length(sched->ready)) {
        TB_Node* n = sched->ready[i].n;
        uint64_t op_unit_mask = sched->ready[i].unit_mask;
        int lat = sched->ready[i].latency;

        // this should never really need to be considered here
        if (n == end) { continue; }

        // actually fits on the available machine
        if ((unit_mask & op_unit_mask) == 0) { continue; }

        // high latency ops should be scheduled first
        int score = lat*100;

        // bump the score if we're chained to the previous dispatched op
        TB_ASSERT(dyn_array_length(ws->items) > 0);
        TB_Node* prev_op = ws->items[dyn_array_length(ws->items) - 1];
        if (prev_op->user_count == 1 && USERN(&prev_op->users[0]) == n) {
            score += 200;
        }

        int deepest_use = 0;
        FOR_USERS(u, n) {
            if (f->scheduled[USERN(u)->gvn] == bb) {
                int use_depth = sched->depth[USERN(u)->gvn];
                deepest_use = TB_MAX(deepest_use, use_depth);
            }
        }
        // deepest_use -= sched->depth[n->gvn];

        // score things which decrease pressure higher
        int dt = pressure_delta(sched, n);
        score -= dt * 50;

        // if we have a single use and it's waiting on us? then we
        // really wanna schedule, if it's waiting on a lot of others then
        // we want them to fill up first
        if (n->user_count == 1) {
            TB_Node* use = USERN(&n->users[0]);

            // try to schedule the condition to the
            // branch as late as possible if it's got
            // only one use.
            if (can_stall && use == bb->end) {
                continue;
            }

            if (count_waiting_deps(f, ws, bb, use) == 1) {
                score += use == bb->end ? -200 : 100;
                if (score < 1) { score = 1; }
            }
        }

        // we wanna handle shallower uses first
        score += deepest_use * 10;
        if (score < 1) { score = 1; }

        #if TB_OPTDEBUG_SCHED1
        printf("  ");
        tb_print_dumb_node(NULL, n);
        printf("  score=%d, deepest_use=%d\n", score, deepest_use);
        #endif

        if ((score == best_score && deepest_use > best_depth) || score > best_score) {
            best_i = i;
            best_score = score;
            best_depth = deepest_use;
        }
    }

    return best_i;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count) {
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    TB_OPTDEBUG(SCHED1)(printf("BB %zu\n", bb - cfg->blocks));
    // wanna move the increment op to the end of an affine loop
    TB_Node* end = bb->end;

    int cycle = 0;
    ArenaArray(InFlight) active = aarray_create(&f->tmp_arena, InFlight, 32);

    ListSched sched = {
        .f = f, .get_lat = get_lat, .get_unit_mask = get_unit_mask
    };
    sched.ready_set = set_create_in_arena(&f->tmp_arena, f->node_count);
    sched.ready     = aarray_create(&f->tmp_arena, ReadyNode, 32);
    sched.depth     = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));

    CUIK_TIMED_BLOCK("trace") {
        TB_Node* top = bb->start;

        // DFS walk
        worklist_clear(ws);
        worklist_push(ws, top);
        sched.depth[top->gvn] = 0;

        FOR_USERS(u, top) {
            if (USERN(u)->type == TB_PHI && USERI(u) == 0) {
                worklist_push(ws, USERN(u));
            }
        }

        aarray_for(i, bb->items) {
            TB_Node* n = bb->items[i];
            if (!worklist_test(ws, n) && f->scheduled[n->gvn] == bb && count_waiting_deps(f, ws, bb, n) == 0) {
                worklist_push(ws, n);
            }
        }

        while (dyn_array_length(ws->items)) retry: {
            TB_Node* n = ws->items[dyn_array_length(ws->items) - 1];

            // process users before placing ourselves
            int max_depth = 0;
            FOR_USERS(u, n) {
                TB_Node* un = USERN(u);
                if (!worklist_test_n_set(ws, un) && f->scheduled[un->gvn] == bb) {
                    dyn_array_put(ws->items, un);
                    goto retry;
                }

                max_depth = TB_MAX(max_depth, sched.depth[un->gvn]);
            }

            #if TB_OPTDEBUG_SCHED1
            tb_print_dumb_node(NULL, n);
            printf(" depth=%d\n", max_depth + 1);
            #endif

            dyn_array_pop(ws->items);
            sched.depth[n->gvn] = max_depth + 1;
        }

        #ifndef NDEBUG
        aarray_for(i, bb->items) {
            TB_Node* n = bb->items[i];
            int d = sched.depth[n->gvn];

            FOR_N(j, 0, n->input_cap) {
                if (n->inputs[j] && f->scheduled[n->inputs[j]->gvn] == bb) {
                    int e = sched.depth[n->inputs[j]->gvn];
                    if (d >= e && n->type != TB_PHI && n->inputs[j]->type != TB_PHI) {
                        tb_print_dumb(f);
                        tb_panic("CYCLE DETECTED BETWEEN %%%u and %%%u!!!\n", n->gvn, n->inputs[j]->gvn);
                    }
                }
            }
        }
        #endif

        worklist_clear(ws);
    }

    TB_OPTDEBUG(SCHED1)(printf("         Dispatch "), tb_print_dumb_node(NULL, bb->start), printf("\n"));
    worklist_push(ws, bb->start);

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
                worklist_push(ws, USERN(u));
            }
        }
    } else {
        FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
            TB_ASSERT(USERI(u) == 0);
            // TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

            TB_OPTDEBUG(SCHED1)(printf("         Dispatch "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));
            worklist_push(ws, USERN(u));
        }
    }

    // fill up initial ready list (everything used by the live-ins)
    aarray_for(i, bb->items) {
        TB_Node* n = bb->items[i];
        if (!worklist_test(ws, n) && f->scheduled[n->gvn] == bb && count_waiting_deps(f, ws, bb, n) == 0) {
            ready_up(&sched, n);
        }
    }

    uint64_t in_use_mask = 0;
    // what it looks like when all units are in use
    uint64_t blocked_mask = UINT64_MAX >> (64 - unit_count);

    /*if (is_proj(end)) {
        end = end->inputs[0];
    }*/

    // btw worklist has the final schedule
    int decode_width = 4;
    while (aarray_length(active) > 0 || aarray_length(sched.ready) > 0) {
        cuikperf_region_start("step", NULL);

        #if TB_OPTDEBUG_SCHED1
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
                            ready_up(&sched, USERN(proj_u));
                        }
                    }
                } else if (can_ready_user(f, ws, bb, &sched.ready_set, un)) {
                    ready_up(&sched, un);
                }
            }
        }

        #if TB_OPTDEBUG_SCHED1
        printf("  retired %d ops [ ", retired);
        FOR_N(i, 0, aarray_length(sched.ready)) {
            printf("%%%u ", sched.ready[i].n->gvn);
        }
        printf("]\n");
        #endif

        // dispatch 1 op per decoder
        bool stall = true;
        CUIK_TIMED_BLOCK("dispatch") {
            FOR_N(i, 0, decode_width) {
                int idx = best_ready_node(f, ws, bb, end, &sched, ~in_use_mask & blocked_mask, in_use_mask != 0);
                if (idx < 0) {
                    continue;
                }

                uint64_t unit_mask = ~in_use_mask & sched.ready[idx].unit_mask;
                if (unit_mask == 0) {
                    continue;
                }
                TB_ASSERT(unit_mask != 0);

                int port = tb_ffs64(unit_mask) - 1;
                TB_Node* n = sched.ready[idx].n;
                in_use_mask |= 1ull << port;
                stall = false;

                aarray_remove(sched.ready, idx);
                TB_ASSERT(!is_proj(n));

                if (n != end) {
                    int end_cycle = cycle + get_lat(f, n, NULL);
                    aarray_push(active, (InFlight){ n, end_cycle, port });

                    // idk, it's ugly and a bookkeeping op
                    if (n->type != TB_CALLGRAPH) {
                        // push any temporaries for the node right before
                        FOR_N(i, 0, n->input_count) if (n->inputs[i] && n->inputs[i]->type == TB_MACH_TEMP) {
                            worklist_push(ws, n->inputs[i]);
                        }

                        worklist_push(ws, n);
                    }

                    // make sure to place all projections directly after their tuple node
                    if (n != end || !cfg_is_fork(n)) {
                        FOR_USERS(u, n) if (is_proj(USERN(u))) {
                            TB_ASSERT(USERI(u) == 0);
                            TB_ASSERT(!worklist_test(ws, USERN(u)));
                            worklist_push(ws, USERN(u));
                        }
                    }

                    TB_OPTDEBUG(SCHED1)(printf("  Port%d: Dispatch ", port), tb_print_dumb_node(NULL, n), printf(" (ready on t=%d)\n", end_cycle));
                    stall = false;
                }
            }
        }

        cuikperf_region_end();
        cycle += 1;
    }

    // place end node
    if (end != bb->start) {
        // push any temporaries for the node right before
        FOR_N(i, 0, end->input_count) if (end->inputs[i] && end->inputs[i]->type == TB_MACH_TEMP) {
            worklist_push(ws, end->inputs[i]);
        }

        worklist_push(ws, end);
        TB_OPTDEBUG(SCHED1)(printf("         Dispatch "), tb_print_dumb_node(NULL, end), printf("\n"));
    }

    tb_arena_restore(&f->tmp_arena, sp);
}


