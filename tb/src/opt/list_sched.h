
typedef struct {
    TB_Node* n;
    int end;
} InFlight;

typedef struct {
    TB_Node* n;
    int prio;
} ReadyNode;

static bool is_node_ready(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, TB_Node* n) {
    FOR_N(i, 0, n->input_count) {
        TB_Node* in = n->inputs[i];
        if (in && f->scheduled[in->gvn] == bb && !worklist_test(ws, in)) {
            return false;
        }
    }
    return true;
}

// should probably move this out, it's useful elsewhere
static bool is_proj(TB_Node* n) { return n->type == TB_PROJ || n->type == TB_MACH_PROJ; }
static ArenaArray(ReadyNode) ready_up(ArenaArray(ReadyNode) ready, Set* ready_set, TB_Node* n, int prio) {
    TB_OPTDEBUG(SCHEDULE)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf("\n"));
    set_put(ready_set, n->gvn);

    // projections are readied but not in the ready list
    if (n->dt.type == TB_TUPLE) FOR_USERS(u, n) {
        if (is_proj(USERN(u))) { set_put(ready_set, USERN(u)->gvn); }
    }

    // sorted insertion
    size_t i = 0, count = aarray_length(ready);
    for (; i < count; i++) {
        if (prio < ready[i].prio) break;
    }

    aarray_push(ready, (ReadyNode){ 0 });
    memmove(&ready[i + 1], &ready[i], (count - i) * sizeof(ReadyNode));
    ready[i] = (ReadyNode){ n, prio };
    return ready;
}

static bool can_ready_user(TB_Function* f, TB_Worklist* ws, TB_BasicBlock* bb, Set* ready_set, TB_Node* n) {
    return !set_get(ready_set, n->gvn) && f->scheduled[n->gvn] == bb && is_node_ready(f, ws, bb, n);
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
                TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH: "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

                worklist_push(ws, USERN(u));
            }
        }
    } else {
        FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
            assert(USERI(u) == 0);
            TB_OPTDEBUG(SCHEDULE)(printf("        DISPATCH: "), tb_print_dumb_node(NULL, USERN(u)), printf("\n"));

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
            ready = ready_up(ready, &ready_set, n, get_prio(f, n, NULL));
        }
    }

    TB_Node* cmp = NULL;
    if (end->type == TB_BRANCH && f->scheduled[end->inputs[1]->gvn] == bb && end->inputs[1]->users->next == NULL &&
        end->inputs[1]->type != TB_PROJ && end->inputs[1]->type != TB_MACH_PROJ) {
        cmp = end->inputs[1];
    }

    // btw worklist has the final schedule
    bool busy = false;
    while (aarray_length(active) > 0 || aarray_length(ready) > 0) {
        // dispatch one instruction to a machine per cycle
        if (!busy && aarray_length(ready) > 0) {
            TB_Node* n = aarray_pop(ready).n;
            assert(n->type != TB_PROJ);
            worklist_push(ws, n);

            // make sure to place all projections directly after their tuple node
            if (n->dt.type == TB_TUPLE) {
                assert(n->type != TB_BRANCH && "branches should only show up as the end node which can't be here");
                FOR_USERS(u, n) if (is_proj(USERN(u))) {
                    assert(USERI(u) == 0);
                    assert(!worklist_test(ws, USERN(u)));
                    worklist_push(ws, USERN(u));
                }
            }

            int end_cycle = cycle + get_lat(f, n);
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, n), printf(" (until %d)\n", end_cycle));
            aarray_push(active, (InFlight){ n, end_cycle });
            busy = true;
        } else {
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: STALL\n", cycle));
        }
        cycle += 1;

        for (size_t i = 0; i < aarray_length(active);) {
            TB_Node* n = active[i].n;
            if (active[i].end > cycle) { i++; continue; }

            // instruction's retired, time to ready up users
            busy = false;
            aarray_remove(active, i);
            TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: RETIRE   ", cycle), tb_print_dumb_node(NULL, n), printf("\n"));

            if (n != end && n != cmp) {
                FOR_USERS(u, n) {
                    TB_Node* un = USERN(u);
                    if (un->type == TB_PROJ || un->type == TB_MACH_PROJ) {
                        // projections are where all the real users ended up
                        FOR_USERS(proj_u, un) {
                            if (USERN(proj_u) != end && USERN(proj_u) != cmp && can_ready_user(f, ws, bb, &ready_set, USERN(proj_u))) {
                                ready = ready_up(ready, &ready_set, USERN(proj_u), get_prio(f, n, USERN(proj_u)));
                            }
                        }
                    } else if (un != end && un != cmp && can_ready_user(f, ws, bb, &ready_set, un)) {
                        ready = ready_up(ready, &ready_set, un, get_prio(f, n, un));
                    }
                }
            }
        }
    }

    if (cmp) {
        TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, cmp), printf("\n"));
        worklist_push(ws, cmp);
        cycle += get_lat(f, cmp);
    }

    TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, end), printf("\n"));
    worklist_push(ws, end);
    tb_arena_restore(tmp_arena, sp);
}


