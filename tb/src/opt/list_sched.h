
typedef struct {
    TB_Node* n;
    int end;
    int unit_i;
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

// should probably move this out, it's useful elsewhere
static ArenaArray(ReadyNode) ready_up(ArenaArray(ReadyNode) ready, Set* ready_set, TB_Node* n, int prio) {
    TB_OPTDEBUG(SCHEDULE)(printf("        READY    "), tb_print_dumb_node(NULL, n), printf("\n"));
    set_put(ready_set, n->gvn);

    // projections are readied but not in the ready list
    if (n->dt.type == TB_TAG_TUPLE) FOR_USERS(u, n) {
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
    return !set_get(ready_set, n->gvn) && f->scheduled[n->gvn] == bb && !worklist_test(ws, n) && is_node_ready(f, ws, bb, n);
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
            if (USERN(u)->type == TB_MACH_PROJ || USERN(u)->type == TB_PROJ) {
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
    Set ready_set = set_create_in_arena(tmp_arena, f->node_count);
    ArenaArray(InFlight) active = aarray_create(tmp_arena, InFlight, 32);
    ArenaArray(ReadyNode) ready = aarray_create(tmp_arena, ReadyNode, 32);

    // fill up initial ready list (everything used by the live-ins)
    nl_hashset_for(e, &bb->items) {
        TB_Node* n = *e;
        if (!worklist_test(ws, n) && end != n && f->scheduled[n->gvn] == bb && is_node_ready(f, ws, bb, n)) {
            ready = ready_up(ready, &ready_set, n, get_lat(f, n, end));
        }
    }

    TB_Node* cmp = NULL;

    // TODO(NeGate): we shouldn't do this on VLIWs, ideally we schedule predication earlier there
    if ((end->type == TB_BRANCH || end->type == TB_AFFINE_LATCH) && f->scheduled[end->inputs[1]->gvn] == bb &&
        end->inputs[1]->user_count == 1 && !is_proj(end->inputs[1])) {
        cmp = end->inputs[1];
    }

    uint64_t in_use_mask = 0;
    // what it looks like when all units are in use
    uint64_t blocked_mask = UINT64_MAX >> (64 - unit_count);

    // btw worklist has the final schedule
    while (aarray_length(active) > 0 || aarray_length(ready) > 0) {
        bool stall = true;

        // dispatch one instruction to a machine per cycle
        while (in_use_mask != blocked_mask && aarray_length(ready) > 0) {
            TB_Node* n = aarray_top(ready).n;
            uint64_t avail = get_unit_mask(f, n) & ~in_use_mask;
            if (avail == 0) { continue; }

            aarray_pop(ready);

            int unit_i = tb_ffs64(avail) - 1;
            in_use_mask |= 1ull << unit_i;
            stall = false;

            assert(!is_proj(n));
            worklist_push(ws, n);

            // make sure to place all projections directly after their tuple node
            if (n->dt.type == TB_TAG_TUPLE) {
                assert(!cfg_is_fork(n) && "branches should only show up as the end node which can't be here");
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
            if (n != end && n != cmp) {
                FOR_USERS(u, n) {
                    TB_Node* un = USERN(u);
                    if (is_proj(un)) {
                        // projections are where all the real users ended up
                        FOR_USERS(proj_u, un) {
                            if (USERN(proj_u) != end && USERN(proj_u) != cmp && can_ready_user(f, ws, bb, &ready_set, USERN(proj_u))) {
                                ready = ready_up(ready, &ready_set, USERN(proj_u), get_lat(f, USERN(proj_u), end));
                            }
                        }
                    } else if (un != end && un != cmp && can_ready_user(f, ws, bb, &ready_set, un)) {
                        ready = ready_up(ready, &ready_set, un, get_lat(f, un, end));
                    }
                }
            }
        }
    }

    if (cmp) {
        TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, cmp), printf("\n"));
        worklist_push(ws, cmp);
        cycle += get_lat(f, cmp, end);
    }

    TB_OPTDEBUG(SCHEDULE)(printf("  T=%2d: DISPATCH ", cycle), tb_print_dumb_node(NULL, end), printf("\n"));
    worklist_push(ws, end);
    tb_arena_restore(tmp_arena, sp);
}


