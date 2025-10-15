
typedef struct {
    TB_Node* n;
    int end;
    int unit_i;
} InFlight;

typedef struct {
    TB_Node* n;
} ReadyNode;

static int count_waiting_deps(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    if (n->type == TB_PHI && n->inputs[0] == bb->start) {
        return 0;
    }

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

    Set has_sfpt;
    Set ready_set;
    ArenaArray(ReadyNode) ready;

    int* latency;
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
        TB_OPTDEBUG(SCHED1)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf(" (lat=%d)\n", sched->latency[n->gvn]));
        set_put(&sched->ready_set, n->gvn);

        // projections are readied but not in the ready list
        FOR_USERS(u, n) {
            if (IS_PROJ(USERN(u))) { set_put(&sched->ready_set, USERN(u)->gvn); }
        }

        ReadyNode r = { n };
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
static int best_ready_node(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* end, ListSched* sched) {
    if (aarray_length(sched->ready) == 1) {
        return 0;
    }

    int best_i = -1;
    int best_score = 0;
    FOR_REV_N(i, 0, aarray_length(sched->ready)) {
        TB_Node* n = sched->ready[i].n;
        int lat = sched->latency[n->gvn];

        // high latency ops should be scheduled first
        int score = lat*100;

        // bump the score if we're chained to the previous dispatched op
        TB_ASSERT(dyn_array_length(ws->items) > 0);
        TB_Node* prev_op = ws->items[dyn_array_length(ws->items) - 1];
        if (prev_op->user_count == 1 && USERN(&prev_op->users[0]) == n) {
            if (!(IS_PROJ(prev_op) && prev_op->inputs[0] == f->root_node) && prev_op->type != TB_MACH_SYMBOL) {
                score += 200;
            }
        }

        // score things which decrease pressure higher
        // int dt = pressure_delta(sched, n);
        // score -= dt * 10;

        // if we have a single use and it's waiting on us? then we
        // really wanna schedule, if it's waiting on a lot of others then
        // we want them to fill up first
        if (n->user_count == 1) {
            TB_Node* use = USERN(&n->users[0]);

            // try to schedule the latch IVs late
            if (NODE_ISA(bb->end, IF) && n == bb->end->inputs[1]) {
                continue;
            }
        }

        // ideally the IV stepping gets postponed
        if (n->type != TB_PHI && n->dt.type != TB_TAG_MEMORY && n->dt.type != TB_TAG_CONTROL && n->dt.type != TB_TAG_TUPLE) {
            TB_Node* phi = NULL;
            FOR_N(i, 1, n->input_count) {
                if (n->inputs[i] && n->inputs[i]->type == TB_PHI && cfg_is_natural_loop(n->inputs[i]->inputs[0])) {
                    phi = n->inputs[i];
                    break;
                }
            }

            // natural phis can only have 2 incoming paths
            TB_ASSERT(phi == NULL || phi->input_count == 3);
            if (phi && phi->inputs[2] == n) {
                score = 1;
            }
        }

        if (score < 1) { score = 1; }

        #if TB_OPTDEBUG_SCHED1
        printf("  ");
        tb_print_dumb_node(NULL, n);
        printf("  score=%d, latency=%d\n", score, lat);
        #endif

        if (score > best_score) {
            best_i = i;
            best_score = score;
        }
    }

    TB_ASSERT(best_i >= 0);
    return best_i;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, TB_BasicBlock* bb, TB_GetLatency get_lat) {
    cuikperf_region_start("list_sched", NULL);
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    TB_OPTDEBUG(SCHED1)(printf("BB %zu\n", bb - cfg->blocks));
    // wanna move the increment op to the end of an affine loop
    TB_Node* end = bb->end;

    ListSched sched = {
        .f = f, .get_lat = get_lat
    };
    sched.has_sfpt  = set_create_in_arena(&f->tmp_arena, f->node_count);
    sched.ready_set = set_create_in_arena(&f->tmp_arena, f->node_count);
    sched.ready     = aarray_create(&f->tmp_arena, ReadyNode, 32);
    sched.latency   = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));

    CUIK_TIMED_BLOCK("trace") {
        TB_Node* top = bb->start;
        int* aux_stack = tb_arena_alloc(&f->tmp_arena, aarray_length(bb->items) * sizeof(int));

        int cnt = 0;
        TB_Node** ordered = tb_arena_alloc(&f->tmp_arena, aarray_length(bb->items) * sizeof(TB_Node*));

        // DFS walk
        worklist_clear(ws);
        worklist_push(ws, top);
        aux_stack[0] = top->user_count;

        // fill up initial ready list (everything used by the live-ins)
        aarray_for(i, bb->items) {
            TB_Node* n = bb->items[i];
            if (f->scheduled[n->gvn] == bb && n != top && count_waiting_deps(f, ws, bb, n) == 0) {
                #if 0
                tb_print_dumb_node(NULL, n);
                printf(" %d\n", worklist_test(ws, n));
                #endif

                // this being true means there's duplicate nodes in the bb->items
                TB_ASSERT(!worklist_test(ws, n));
                worklist_push(ws, n);
                aux_stack[dyn_array_length(ws->items) - 1] = n->user_count;
            }
        }

        #ifndef NDEBUG
        int* depth = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));
        #endif

        while (dyn_array_length(ws->items)) retry: {
            int i = dyn_array_length(ws->items) - 1;
            TB_Node* n = ws->items[i];
            while (aux_stack[i] > 0) {
                // push next user in the same block
                TB_User* u = &n->users[--aux_stack[i]];
                TB_Node* un = USERN(u);
                if (f->scheduled[un->gvn] != bb) {
                    continue;
                }

                if (!worklist_test_n_set(ws, un)) {
                    dyn_array_put(ws->items, un);
                    aux_stack[dyn_array_length(ws->items) - 1] = un->user_count;
                    goto retry;
                }
            }

            TB_ASSERT(cnt < aarray_length(bb->items));
            ordered[cnt++] = n;

            #ifndef NDEBUG
            depth[n->gvn] = -1;
            #endif

            dyn_array_pop(ws->items);
        }

        // Cycle detection
        #ifndef NDEBUG
        {
            bool cycle = false;
            FOR_REV_N(i, 0, cnt) {
                TB_Node* n = ordered[i];
                int max_depth = 0;

                if (n->type != TB_PHI && !NODE_ISA(n, REGION)) {
                    FOR_N(i, 0, n->input_cap) {
                        TB_Node* in = n->inputs[i];
                        if (in && in->type != TB_MACH_TEMP && f->scheduled[in->gvn] == bb) {
                            int d = depth[in->gvn];
                            #ifndef NDEBUG
                            if (d < 0) {
                                tb_print_dumb(f);
                                printf("CYCLE DETECTED BETWEEN %%%u and %%%u!!!\n", n->gvn, in->gvn);
                                cycle = true;
                            }
                            #endif
                            max_depth = TB_MAX(max_depth, d);
                        }
                    }
                }

                depth[n->gvn] = max_depth + 1;
            }

            if (cycle) {
                printf("digraph G {\n");
                FOR_REV_N(i, 0, cnt) {
                    TB_Node* n = ordered[i];
                    printf("N%d [label=\"%%%u: %s\"]\n", n->gvn, n->gvn, tb_node_get_name(n->type));

                    int max_depth = 0;
                    if (n->type != TB_PHI && n != top) {
                        FOR_N(i, 0, n->input_cap) {
                            TB_Node* in = n->inputs[i];
                            if (in && f->scheduled[in->gvn] == bb) {
                                printf("N%d -> N%d\n", in->gvn, n->gvn);
                            }
                        }
                    }
                }
                printf("}\n");
                __debugbreak();
            }
        }
        #endif

        FOR_N(i, 0, cnt) {
            TB_Node* n = ordered[i];
            sched.latency[n->gvn] = 0;
        }

        TB_OPTDEBUG(SCHED4)(printf("digraph G {\n"));

        // compute depths
        // TB_ASSERT(cnt == aarray_length(bb->items));
        bool cycle = false;
        FOR_N(i, 0, cnt) {
            TB_Node* n = ordered[i];
            int use_latency = sched.latency[n->gvn];

            TB_OPTDEBUG(SCHED4)(printf("N%d [label=\"%%%u: %s\"]\n", n->gvn, n->gvn, tb_node_get_name(n->type)));

            if (n->type != TB_PHI && !NODE_ISA(n, REGION)) {
                FOR_N(i, 0, n->input_cap) {
                    TB_Node* in = n->inputs[i];
                    if (in) {
                        if (IS_PROJ(in)) { in = in->inputs[0]; }
                        if (in->type != TB_MACH_TEMP && f->scheduled[in->gvn] == bb) {
                            int edge_latency = sched.get_lat(sched.f, n, i);
                            int curr_latency = edge_latency + use_latency;
                            if (curr_latency > sched.latency[in->gvn]) {
                                sched.latency[in->gvn] = curr_latency;
                            }

                            TB_OPTDEBUG(SCHED4)(printf("N%d -> N%d [label=\"%d\"]\n", in->gvn, n->gvn, edge_latency));
                        }
                    }
                }
            }
        }
        TB_OPTDEBUG(SCHED4)(printf("}\n"));

        #if TB_OPTDEBUG_SCHED3
        FOR_N(i, 0, cnt) {
            TB_Node* n = ordered[i];
            tb_print_dumb_node(NULL, n);
            printf(" latency=%d\n", sched.latency[n->gvn]);
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
            if (IS_PROJ(USERN(u))) {
                TB_ASSERT(USERI(u) == 0);
                worklist_push(ws, USERN(u));
            }
        }

        FOR_USERS(u, f->root_node) {
            if (USERN(u)->type == TB_MACH_SYMBOL) {
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

    // btw worklist has the final schedule
    bool in_use = false;
    while (aarray_length(sched.ready) > 0) {
        cuikperf_region_start("step", NULL);

        #if TB_OPTDEBUG_SCHED1
        printf("  ready [ ");
        FOR_N(i, 0, aarray_length(sched.ready)) {
            printf("%%%u ", sched.ready[i].n->gvn);
        }
        printf("]\n");
        #endif

        int idx = best_ready_node(f, ws, bb, end, &sched);
        if (idx < 0) {
            continue;
        }

        TB_Node* n = sched.ready[idx].n;

        aarray_remove(sched.ready, idx);
        TB_ASSERT(!IS_PROJ(n));

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
            if (!tb_node_is_fork_ctrl(n)) {
                FOR_USERS(u, n) {
                    if (IS_PROJ(USERN(u))) {
                        TB_ASSERT(USERI(u) == 0);
                        TB_ASSERT(!worklist_test(ws, USERN(u)));
                        worklist_push(ws, USERN(u));
                    }
                }
            }

            // ready up the users
            FOR_USERS(u, n) {
                TB_Node* un = USERN(u);
                if (IS_PROJ(un)) {
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

            TB_OPTDEBUG(SCHED1)(printf("  Dispatch "), tb_print_dumb_node(NULL, n), printf("\n"));
        }
        cuikperf_region_end();
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
    cuikperf_region_end();
}


