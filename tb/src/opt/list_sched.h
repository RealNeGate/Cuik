
typedef struct {
    TB_Node* n;
    int end;
} InFlight;

typedef struct {
    TB_Node* n;
    int prio;
} ReadyNode;

static bool is_node_ready(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    FOREACH_N(i, 0, n->input_count) {
        TB_Node* in = n->inputs[i];
        if (in && f->scheduled[in->gvn] == bb && !worklist_test(ws, in)) {
            return false;
        }
    }
    return true;
}

static ArenaArray(ReadyNode) add_to_ready(ArenaArray(ReadyNode) ready, TB_Node* n, int prio) {
    size_t i = 0, count = aarray_length(ready);
    for (; i < count; i++) {
        if (prio > ready[i].prio) break;
    }

    // we know where to insert
    aarray_push(ready, (ReadyNode){ 0 });
    memmove(&ready[i + 1], &ready[i], (count - i) * sizeof(ReadyNode));
    ready[i] = (ReadyNode){ n, prio };
    return ready;
}

void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetPriority get_prio) {
    assert(phi_vals == NULL && "TODO");
    TB_Arena* tmp_arena = f->tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);

    TB_OPTDEBUG(SCHEDULE)(printf("BB %d\n", bb->id));
    worklist_push(ws, bb->start);

    // first block has access to root's users
    if (bb->id == 0) {
        TB_Node* root = f->root_node;
        FOR_USERS(u, root) {
            if (USERN(u)->type == TB_MACH_PROJ || USERN(u)->type == TB_PROJ) {
                assert(USERI(u) == 0);
                TB_OPTDEBUG(SCHEDULE)(printf("  DISPATCH: "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

                worklist_push(ws, USERN(u));
            }
        }
    } else {
        FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
            assert(USERI(u) == 0);
            TB_OPTDEBUG(SCHEDULE)(printf("  DISPATCH: "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

            worklist_push(ws, USERN(u));
        }
    }

    int cycle = 0;
    TB_Node* end = bb->end;
    Set ready_set = set_create_in_arena(tmp_arena, f->node_count);
    ArenaArray(InFlight) active = aarray_create(tmp_arena, InFlight, 32);
    ArenaArray(ReadyNode) ready = aarray_create(tmp_arena, ReadyNode, 32);

    // fill up initial ready list (everything used by the live-ins)
    nl_hashset_for(e, &bb->items) {
        TB_Node* n = *e;
        if (!worklist_test(ws, n) && f->scheduled[n->gvn] == bb && is_node_ready(f, ws, bb, n)) {
            TB_OPTDEBUG(SCHEDULE)(printf("  READY: "), tb_print_dumb_node(NULL, n), printf("\n"));

            set_put(&ready_set, n->gvn);
            ready = add_to_ready(ready, n, get_prio(f, n, NULL));
        }
    }

    // btw worklist has the final schedule
    while (aarray_length(active) > 0 || aarray_length(ready) > 0) {
        // instruction dispatches
        while (aarray_length(ready) > 0) {
            TB_Node* n = aarray_pop(ready).n;
            worklist_push(ws, n);

            // make sure to place all projections directly after their tuple node
            if (n->dt.type == TB_TUPLE && n->type != TB_BRANCH) {
                FOR_USERS(u, n) if (USERN(u)->type == TB_MACH_PROJ || USERN(u)->type == TB_PROJ) {
                    assert(USERI(u) == 0);
                    assert(!worklist_test(ws, USERN(u)));
                    worklist_push(ws, USERN(u));
                }
            }

            TB_OPTDEBUG(SCHEDULE)(printf("  T=%d: DISPATCH ", cycle), tb_print_dumb_node(NULL, n), printf("\n"));

            int end_cycle = cycle + get_lat(f, n);
            aarray_push(active, (InFlight){ n, end_cycle });
        }
        cycle += 1;

        bool stall = true;
        for (size_t i = 0; i < aarray_length(active);) {
            TB_Node* n = active[i].n;
            if (active[i].end > cycle) { i++; continue; }

            // instruction's retired, time to ready up users
            stall = false;
            aarray_remove(active, i);
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%d: RETIRE   ", cycle), tb_print_dumb_node(NULL, n), printf("\n"));

            if (n != end) {
                FOR_USERS(u, n) {
                    TB_Node* un = USERN(u);
                    if (!set_get(&ready_set, un->gvn) && f->scheduled[un->gvn] == bb && is_node_ready(f, ws, bb, un)) {
                        TB_OPTDEBUG(SCHEDULE)(printf("       READY    "), tb_print_dumb_node(NULL, un), printf("\n"));

                        set_put(&ready_set, un->gvn);
                        ready = add_to_ready(ready, un, get_prio(f, n, un));
                    }
                }
            }
        }

        if (stall) {
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%d: STALL\n", cycle));
        }
    }

    tb_arena_restore(tmp_arena, sp);
}


