// for more consistent hashing than a pointer
uint32_t tb__node_hash(void* a) { return ((TB_Node*) a)->gvn; }
bool tb__node_cmp(void* a, void* b) { return a == b; }

// Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf
typedef struct Elem {
    struct Elem* parent;
    TB_ArenaSavepoint sp;
    TB_Node* n;
    int i;
} Elem;

// any blocks in the dom tree between and including early and late are valid schedules.
static TB_BasicBlock* try_to_hoist(TB_Function* f, TB_GetLatency get_lat, TB_Node* n, TB_BasicBlock* early, TB_BasicBlock* late) {
    if (get_lat == NULL) return late;

    // phi copies should be kept in the same block
    if (n->type == TB_MACH_COPY && n->inputs[1]->type == TB_PHI) {
        return early;
    }

    int lat = get_lat(f, n, NULL);
    if (lat >= 2) {
        TB_BasicBlock* best = late;
        while (late != early) {
            if (late->freq < best->freq) {
                best = late;
            }
            late = late->dom;
        }
        return best;
    }

    return late;
}

// schedule nodes such that they appear the least common
// ancestor to all their users
static TB_BasicBlock* find_lca(TB_BasicBlock* a, TB_BasicBlock* b) {
    if (a == NULL) return b;

    // line both up
    while (a->dom_depth > b->dom_depth) a = a->dom;
    while (b->dom_depth > a->dom_depth) b = b->dom;

    while (a != b) {
        b = b->dom;
        a = a->dom;
    }

    return a;
}

static TB_BasicBlock* find_use_block(TB_Function* f, TB_Node* n, TB_Node* actual_n, TB_User* use) {
    TB_Node* y = USERN(use);
    if (USERI(use) >= y->input_count) { return NULL; } // extra deps not counted

    TB_BasicBlock* use_block = f->scheduled[y->gvn];
    if (use_block == NULL) { return NULL; } // dead

    TB_OPTDEBUG(GCM)(printf("  user v%u @ bb%d\n", y->gvn, use_block->id));
    if (y->type == TB_PHI) {
        TB_Node* use_node = y->inputs[0];
        TB_ASSERT(cfg_is_region(use_node));
        TB_ASSERT_MSG(y->input_count == use_node->input_count + 1, "phi has parent with mismatched predecessors");

        ptrdiff_t j = USERI(use);
        TB_ASSERT(y->inputs[j] == actual_n);

        TB_BasicBlock* bb = f->scheduled[use_node->inputs[j - 1]->gvn];
        if (bb) { use_block = bb; }
    }
    return use_block;
}

void tb_compact_nodes(TB_Function* f, TB_Worklist* ws) {
    TB_Node** fwd = tb_arena_alloc(&f->arena, f->node_count * sizeof(TB_Node*));
    FOR_N(i, 0, f->node_count) { fwd[i] = NULL; }

    SWAP(TB_Arena, f->arena, f->tmp_arena);
    CUIK_TIMED_BLOCK("compact") {
        f->node_count = 0;
        f->dead_node_bytes = 0;

        CUIK_TIMED_BLOCK("mark") {
            // BFS walk all the nodes
            worklist_push(ws, f->root_node);
            for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
                TB_Node* n = ws->items[i];

                size_t extra = extra_bytes(n);
                TB_Node* k = tb_alloc_node_dyn(f, n->type, n->dt, n->input_count, n->input_cap, extra);
                memcpy(k->extra, n->extra, extra);
                fwd[n->gvn] = k;

                // place projections first & sequentially
                if (n->dt.type == TB_TAG_TUPLE) {
                    FOR_USERS(u, n) if (is_proj(USERN(u))) {
                        worklist_push(ws, USERN(u));
                    }
                }

                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }
        }

        CUIK_TIMED_BLOCK("compact nodes") {
            TB_Node* new_root = NULL;
            if (f->types) {
                f->type_cap = tb_next_pow2(f->node_count + 16);

                Lattice** new_types = tb_platform_heap_alloc(f->type_cap * sizeof(Lattice*));
                FOR_N(i, 0, f->type_cap) { new_types[i] = NULL; }

                FOR_N(i, 0, dyn_array_length(ws->items)) {
                    TB_Node* n = ws->items[i];
                    TB_Node* k = fwd[n->gvn];
                    new_types[k->gvn] = f->types[n->gvn];
                }

                TB_ASSERT(f->root_node->gvn == 0);
                tb_platform_heap_free(f->types);
                f->types = new_types;
            }

            FOR_N(i, 0, dyn_array_length(ws->items)) {
                TB_Node* n = ws->items[i];
                TB_Node* k = fwd[n->gvn];

                // connect new node to new inputs
                FOR_N(j, 0, n->input_cap) if (n->inputs[j]) {
                    TB_Node* moved_k = fwd[n->inputs[j]->gvn];
                    TB_ASSERT(moved_k != NULL);

                    #ifndef NDEBUG
                    set_input(f, n, NULL, j);
                    #endif

                    set_input(f, k, moved_k, j);
                }

                TB_OPTDEBUG(COMPACT)(printf("%s: %p (%u) -> %p (%u)\n", n, n->gvn, k, k->gvn));
            }

            // redo the params list now
            TB_ASSERT(f->root_node->gvn == 0);
            f->root_node = fwd[f->root_node->gvn];

            #ifndef NDEBUG
            FOR_N(i, 0, dyn_array_length(ws->items)) {
                memset(ws->items[i], 0xAF, sizeof(TB_Node));
            }
            #endif

            size_t param_count = 3 + f->param_count;
            f->params = tb_arena_alloc(&f->arena, param_count * sizeof(TB_Node*));
            FOR_N(i, 0, param_count) { f->params[i] = NULL; }

            FOR_USERS(u, f->root_node) {
                TB_Node* un = USERN(u);
                if (is_proj(un)) {
                    TB_ASSERT(USERI(u) == 0);
                    int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
                    if (index < param_count) {
                        f->params[index] = un;
                    }
                }
            }

            nl_hashset_clear(&f->gvn_nodes);
        }
        worklist_clear(ws);
    }

    // free old nodes
    tb_arena_clear(&f->tmp_arena);
}

void tb_renumber_nodes(TB_Function* f, TB_Worklist* ws) {
    CUIK_TIMED_BLOCK("renumber") {
        CUIK_TIMED_BLOCK("find live") {
            // BFS walk all the nodes
            worklist_push(ws, f->root_node);
            for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
                TB_Node* n = ws->items[i];
                if (n->dt.type == TB_TAG_TUPLE) {
                    // place projections first
                    FOR_USERS(u, n) if (is_proj(USERN(u))) {
                        worklist_push(ws, USERN(u));
                    }
                }

                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }
        }

        CUIK_TIMED_BLOCK("compact IDs") {
            f->node_count = dyn_array_length(ws->items);
            if (f->types) {
                f->type_cap = tb_next_pow2(f->node_count + 16);

                Lattice** new_types = tb_platform_heap_alloc(f->type_cap * sizeof(Lattice*));
                FOR_N(i, 0, f->type_cap) { new_types[i] = NULL; }

                FOR_N(i, 0, dyn_array_length(ws->items)) {
                    uint32_t old_gvn = ws->items[i]->gvn;
                    new_types[i] = f->types[old_gvn];
                    ws->items[i]->gvn = i;
                }

                TB_ASSERT(f->root_node->gvn == 0);
                tb_platform_heap_free(f->types);
                f->types = new_types;
            } else {
                FOR_N(i, 0, dyn_array_length(ws->items)) {
                    ws->items[i]->gvn = i;
                }
            }

            // invalidate all of the GVN table since it hashes with value numbers
            nl_hashset_clear(&f->gvn_nodes);
        }

        worklist_clear(ws);
    }
}

void tb_dataflow(TB_Function* f, TB_Arena* arena, TB_CFG cfg) {
    size_t bb_count   = aarray_length(cfg.blocks);
    size_t node_count = f->node_count;

    TB_ArenaSavepoint sp = tb_arena_save(arena);

    TB_Worklist* ws = f->worklist;
    worklist_clear_visited(ws);

    size_t old = dyn_array_length(ws->items);
    CUIK_TIMED_BLOCK("dataflow") {
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            bb->gen  = set_create_in_arena(arena, node_count);
            bb->kill = set_create_in_arena(arena, node_count);
        }

        CUIK_TIMED_BLOCK("local") {
            // we're doing dataflow analysis without the local schedule :)
            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                nl_hashset_for(e, &bb->items) {
                    TB_Node* n = *e;

                    // PHI
                    if (n->type == TB_PHI) {
                        // every block which has the phi edges will def the phi, this emulates
                        // the phi move.
                        FOR_N(i, 1, n->input_count) {
                            TB_Node* in = n->inputs[i];
                            if (in) {
                                TB_BasicBlock* in_bb = f->scheduled[in->gvn];
                                set_put(&in_bb->kill, n->gvn);
                            }
                        }
                    } else {
                        // other than phis every node dominates all uses which means it's KILL
                        // within it's scheduled block and since it's single assignment this is
                        // the only KILL for that a through all sets.
                        set_put(&bb->kill, n->gvn);
                    }
                }
            }

            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                nl_hashset_for(e, &bb->items) {
                    TB_Node* n = *e;
                    if (n->type == TB_PHI) continue;

                    FOR_N(i, 1, n->input_count) {
                        TB_Node* in = n->inputs[i];
                        if (in && (in->type == TB_PHI || !set_get(&bb->kill, in->gvn))) {
                            set_put(&bb->gen, in->gvn);
                        }
                    }
                }
            }
        }

        // generate global live sets
        CUIK_TIMED_BLOCK("global") {
            // all BB go into the worklist
            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];

                // in(bb) = use(bb)
                set_copy(&bb->live_in, &bb->gen);
                worklist_push(ws, bb->start);
            }

            Set visited = set_create_in_arena(arena, bb_count);
            while (dyn_array_length(ws->items) > old) CUIK_TIMED_BLOCK("iter")
            {
                TB_Node* bb_node = worklist_pop(ws);
                TB_BasicBlock* bb = f->scheduled[bb_node->gvn];

                Set* live_out = &bb->live_out;
                set_clear(live_out);

                // walk all successors
                TB_Node* end = bb->end;
                if (cfg_is_fork(end)) {
                    FOR_USERS(u, end) {
                        if (cfg_is_cproj(USERN(u))) {
                            // union with successor's lives
                            TB_Node* succ = cfg_next_bb_after_cproj(USERN(u));
                            TB_BasicBlock* succ_bb = f->scheduled[succ->gvn];
                            set_union(live_out, &succ_bb->live_in);
                        }
                    }
                } else if (!cfg_is_endpoint(end)) {
                    // union with successor's lives
                    TB_Node* succ = cfg_next_control(end);
                    TB_BasicBlock* succ_bb = f->scheduled[succ->gvn];
                    set_union(live_out, &succ_bb->live_in);
                }

                Set* restrict live_in = &bb->live_in;
                Set* restrict kill = &bb->kill;
                Set* restrict gen = &bb->gen;

                // live_in = (live_out - live_kill) U live_gen
                bool changes = false;
                FOR_N(i, 0, (node_count + 63) / 64) {
                    uint64_t new_in = (live_out->data[i] & ~kill->data[i]) | gen->data[i];

                    changes |= (live_in->data[i] != new_in);
                    live_in->data[i] = new_in;
                }

                // if we have changes, mark the predeccesors
                if (changes && !(bb_node->type == TB_PROJ && bb_node->inputs[0]->type == TB_ROOT)) {
                    FOR_N(i, 0, bb_node->input_count) {
                        TB_Node* pred = cfg_get_pred(&cfg, bb_node, i);
                        if (pred->input_count > 0) {
                            worklist_push(ws, pred);
                        }
                    }
                }
            }
        }

        #if TB_OPTDEBUG_DATAFLOW
        // log live ins and outs
        FOR_N(i, 0, cfg.block_count) {
            TB_Node* n = rpo_nodes[i];
            TB_BasicBlock* bb = f->scheduled[n->gvn];

            printf("BB%zu:\n  live-ins:", i);
            FOR_N(j, 0, node_count) if (set_get(&bb->live_in, j)) {
                printf(" %%%zu", j);
            }
            printf("\n  live-outs:");
            FOR_N(j, 0, node_count) if (set_get(&bb->live_out, j)) {
                printf(" %%%zu", j);
            }
            printf("\n  gen:");
            FOR_N(j, 0, node_count) if (set_get(&bb->gen, j)) {
                printf(" %%%zu", j);
            }
            printf("\n  kill:");
            FOR_N(j, 0, node_count) if (set_get(&bb->kill, j)) {
                printf(" %%%zu", j);
            }
            printf("\n");
        }
        #endif
    }
    tb_arena_restore(arena, sp);
}

void tb_clear_anti_deps(TB_Function* f, TB_Worklist* ws) {
    worklist_clear(ws);
    worklist_push(ws, f->root_node);
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];

        if (is_mem_out_op(n) || n->dt.type == TB_TAG_MEMORY) {
            // the anti-deps are applied to the tuple node (projs can't have extra inputs anyways)
            TB_Node* k = is_proj(n) ? n->inputs[0] : n;
            if (k->type != TB_ROOT) {
                tb_node_clear_extras(f, k);
            }
        }

        FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
    }
}

void tb_global_schedule(TB_Function* f, TB_Worklist* ws, TB_CFG cfg, bool loop_nests, bool dataflow, TB_GetLatency get_lat) {
    assert(f->scheduled == NULL && "make sure when you're done with the schedule, you throw away the old one");

    CUIK_TIMED_BLOCK("schedule") {
        size_t node_count = f->node_count;

        // arraychads stay up
        f->scheduled_n = node_count + 32;
        f->scheduled = tb_arena_alloc(&f->tmp_arena, f->scheduled_n * sizeof(TB_BasicBlock*));
        memset(f->scheduled, 0, f->scheduled_n * sizeof(TB_BasicBlock*));

        if (dataflow) {
            // live ins & outs will outlive this function so we wanna alloc before the savepoint
            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                bb->live_in = set_create_in_arena(&f->tmp_arena, node_count);
                bb->live_out = set_create_in_arena(&f->tmp_arena, node_count);
            }
        }

        TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

        CUIK_TIMED_BLOCK("dominators") {
            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];

                TB_ASSERT(bb->start->type != TB_NULL);
                bb->items = nl_hashset_alloc(32);
                nl_hashset_put2(&bb->items, bb->start, tb__node_hash, tb__node_cmp);
                f->scheduled[bb->start->gvn] = bb;
            }

            worklist_clear(ws);
        }

        ArenaArray(TB_Node*) pins = aarray_create(&f->tmp_arena, TB_Node*, (f->node_count / 32) + 16);
        CUIK_TIMED_BLOCK("pinned schedule") {
            // BFS walk all the nodes
            worklist_push(ws, f->root_node);
            for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
                TB_Node* n = ws->items[i];
                if (is_pinned(n)) {
                    // a region might refer to itself, but a node within a
                    // BB will refer to it's parent (who should've been scheduled
                    // by now)
                    TB_BasicBlock* bb = NULL;
                    if ((n->type == TB_MACH_PROJ || n->type == TB_PROJ) && n->inputs[0]->type == TB_ROOT) {
                        bb = &cfg.blocks[0];
                    } else if (n->type == TB_PHI) {
                        TB_ASSERT(f->scheduled[n->inputs[0]->gvn] != NULL && "where tf is the BB?");
                        bb = f->scheduled[n->inputs[0]->gvn];
                    } else if (n->type != TB_ROOT) {
                        TB_Node* curr = n;
                        do {
                            bb = f->scheduled[curr->gvn];
                            curr = curr->inputs[0];
                            if (curr == NULL || curr->type == TB_ROOT) {
                                break;
                            }
                        } while (!bb);
                    }

                    if (bb) {
                        TB_ASSERT(n->type != TB_NULL);
                        nl_hashset_put2(&bb->items, n, tb__node_hash, tb__node_cmp);
                        TB_ASSERT(n->gvn < f->scheduled_n);
                        f->scheduled[n->gvn] = bb;
                        aarray_push(pins, n);

                        TB_OPTDEBUG(GCM)(printf("%s: %%%u pinned to .bb%d\n", f->super.name, n->gvn, bb->id));
                    }
                }

                // insert anti-dep edges
                if (is_mem_out_op(n) || n->dt.type == TB_TAG_MEMORY) {
                    // clear old anti-deps
                    TB_Node* k = is_proj(n) ? n->inputs[0] : n;
                    if (k->type != TB_ROOT) {
                        tb_node_clear_extras(f, k);

                        FOR_USERS(u, k->inputs[1]) {
                            TB_Node* un = USERN(u);
                            if (k != un && USERI(u) == 1) {
                                tb_node_add_extra(f, k, un);
                            }
                        }
                    }
                }

                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }

            TB_OPTDEBUG(GCM)(printf("%s: scheduled %zu nodes (%zu recorded in the graph)\n", f->super.name, dyn_array_length(ws->items), f->node_count));
        }

        CUIK_TIMED_BLOCK("early schedule") {
            // we're gonna use this space to store the DFS order, we'll walk it in reverse for
            // late sched
            worklist_clear_visited(ws);
            dyn_array_set_length(ws->items, 0);

            aarray_for(i, pins) {
                TB_Node* pin_n = pins[i];

                TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                Elem* top = tb_arena_alloc(&f->tmp_arena, sizeof(Elem));
                top->parent = NULL;
                top->sp = sp;
                top->n = pin_n;
                top->i = pin_n->input_count;
                TB_ASSERT(pin_n->type != TB_NULL);

                // DFS nodes by inputs
                while (top) {
                    TB_Node* n = top->n;

                    if (top->i > 0) {
                        // push next unvisited in
                        TB_Node* in = n->inputs[--top->i];

                        if (in) {
                            // projections don't get scheduled, their tuple nodes do
                            if (is_proj(in)) { in = in->inputs[0]; }

                            // pinned nodes can't be rescheduled
                            if (!is_pinned(in) && !worklist_test_n_set(ws, in)) {
                                TB_ASSERT(in->type != TB_NULL);

                                TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                                Elem* new_top = tb_arena_alloc(&f->tmp_arena, sizeof(Elem));
                                new_top->parent = top;
                                new_top->sp = sp;
                                new_top->n = in;
                                new_top->i = in->input_count;
                                top = new_top;
                            }
                        }
                        continue;
                    }

                    if (n != pin_n) { // only pinned node in the stack
                        // start at the entry point
                        int best_depth = 0;
                        TB_BasicBlock* best = &cfg.blocks[0];

                        // choose deepest block
                        FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
                            if (n->inputs[i]->type == TB_ROOT) {
                                TB_OPTDEBUG(GCM)(printf("  in %%%u @ bb0\n", n->inputs[i]->gvn));
                                continue;
                            }

                            TB_ASSERT(n->inputs[i]->gvn < f->scheduled_n);
                            TB_BasicBlock* bb = f->scheduled[n->inputs[i]->gvn];
                            if (bb == NULL) {
                                // input has no scheduling... weird?
                                TB_OPTDEBUG(GCM)(printf("  in %%%u @ dead\n", n->inputs[i]->gvn));
                                continue;
                            }

                            TB_OPTDEBUG(GCM)(printf("  in %%%u @ bb%d\n", n->inputs[i]->gvn, bb->id));
                            if (best_depth < bb->dom_depth) {
                                best_depth = bb->dom_depth;
                                best = bb;
                            }
                        }

                        TB_OPTDEBUG(GCM)(printf("%s: %%%u into .bb%d\n", f->super.name, n->gvn, best->id));

                        f->scheduled[n->gvn] = best;

                        // unpinned nodes getting moved means their users need to move too
                        if (n->dt.type == TB_TAG_TUPLE) {
                            FOR_USERS(u, n) if (is_proj(USERN(u))) {
                                TB_ASSERT(USERN(u)->type != TB_NULL);
                                nl_hashset_put2(&best->items, USERN(u), tb__node_hash, tb__node_cmp);
                                f->scheduled[USERN(u)->gvn] = best;
                            }
                        }

                        nl_hashset_put2(&best->items, n, tb__node_hash, tb__node_cmp);
                        dyn_array_put(ws->items, n);
                    }

                    struct Elem* parent = top->parent;
                    tb_arena_restore(&f->tmp_arena, top->sp);
                    top = parent;
                }
            }
        }

        // move nodes closer to their usage site
        CUIK_TIMED_BLOCK("late schedule") {
            FOR_REV_N(i, 0, dyn_array_length(ws->items)) {
                TB_Node* n = ws->items[i];
                TB_ASSERT(n->type != TB_NULL);
                TB_OPTDEBUG(GCM)(printf("%s: try late %%%u\n", f->super.name, n->gvn));

                TB_BasicBlock* lca = NULL;
                if (n->dt.type == TB_TAG_TUPLE) {
                    FOR_USERS(use, n) {
                        // to avoid projections stopping the sinking of nodes, we walk past
                        // them whenever decision making here
                        if (is_proj(USERN(use))) {
                            FOR_USERS(use2, USERN(use)) {
                                TB_BasicBlock* use_block = find_use_block(f, n, USERN(use), use2);
                                if (use_block) { lca = find_lca(lca, use_block); }
                            }
                        } else {
                            TB_BasicBlock* use_block = find_use_block(f, n, n, use);
                            if (use_block) { lca = find_lca(lca, use_block); }
                        }
                    }
                } else {
                    FOR_USERS(use, n) {
                        TB_BasicBlock* use_block = find_use_block(f, n, n, use);
                        if (use_block) { lca = find_lca(lca, use_block); }
                    }
                }

                if (lca != NULL) {
                    TB_BasicBlock* old = f->scheduled[n->gvn];
                    TB_ASSERT_MSG(old, "we made it to late sched without an early sched?");

                    // replace old BB entry, also if old is a natural loop we might
                    // be better off hoisting the values if possible.
                    if (old != lca && lca->dom_depth > old->dom_depth) {
                        // some ops deserve hoisting more than others (cough cough loads)
                        TB_BasicBlock* better = try_to_hoist(f, get_lat, n, old, lca);

                        if (old != better) {
                            TB_OPTDEBUG(GCM)(
                                printf("  LATE  %%%u into .bb%d: ", n->gvn, lca->id),
                                tb_print_dumb_node(NULL, n),
                                printf("\n")
                            );

                            f->scheduled[n->gvn] = better;

                            // unpinned nodes getting moved means their users need to move too
                            if (n->dt.type == TB_TAG_TUPLE) {
                                FOR_USERS(u, n) if (is_proj(USERN(u))) {
                                    TB_Node* un = USERN(u);
                                    nl_hashset_remove2(&old->items, un, tb__node_hash, tb__node_cmp);
                                    nl_hashset_put2(&better->items, un, tb__node_hash, tb__node_cmp);

                                    f->scheduled[un->gvn] = better;
                                }
                            }

                            nl_hashset_remove2(&old->items, n, tb__node_hash, tb__node_cmp);
                            nl_hashset_put2(&better->items, n, tb__node_hash, tb__node_cmp);
                        }
                    }
                }
            }
        }

        if (dataflow) {
            worklist_clear(ws);
            tb_dataflow(f, &f->tmp_arena, cfg);
        }

        tb_arena_restore(&f->tmp_arena, sp);
    }
}

