// Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
// https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf
typedef struct Elem {
    struct Elem* parent;
    TB_Node* n;
    int i;
} Elem;

// any blocks in the dom tree between and including early and late are valid schedules.
static TB_BasicBlock* sched_into_good_block(TB_Function* f, TB_GetLatency get_lat, TB_Node* n, TB_BasicBlock* early, TB_BasicBlock* late) {
    TB_ASSERT(early != late);
    if (n->type == TB_LOCAL || n->type == TB_MACH_SYMBOL) {
        return early;
    }

    if (get_lat == NULL || n->type == TB_MACH_TEMP) {
        return late;
    }

    // ideally we don't place things into the backedge of a rotated loop
    if (is_proj(late->start) && late->start == late->end) {
        TB_Node* next = cfg_next_control(late->end);
        if (cfg_is_natural_loop(next)) {
            return early != late ? late->dom : late;
        }
    }

    TB_BasicBlock* best = late;
    for (;;) {
        if (late->freq < best->freq) {
            best = late;
        }
        if (late == early) { break; }
        late = late->dom;
    }
    return best;
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

static TB_BasicBlock* find_use_block(TB_Function* f, TB_CFG* cfg, TB_Node* n, TB_Node* actual_n, TB_User* use) {
    TB_Node* y = USERN(use);
    TB_ASSERT(!is_proj(y));

    // extra deps not counted
    if (USERI(use) >= y->input_count) { return NULL; }

    TB_BasicBlock* use_block = f->scheduled[y->gvn];
    if (use_block == NULL) { return NULL; } // dead

    if (y->type == TB_PHI) {
        TB_Node* use_node = y->inputs[0];
        TB_ASSERT(cfg_is_region(use_node));
        TB_ASSERT_MSG(y->input_count == use_node->input_count + 1, "phi has parent with mismatched predecessors");

        ptrdiff_t j = USERI(use);
        TB_ASSERT(y->inputs[j] == actual_n);

        TB_BasicBlock* bb = f->scheduled[use_node->inputs[j - 1]->gvn];
        if (bb) { use_block = bb; }
    }

    TB_OPTDEBUG(GCM)(printf("  user v%u @ bb%zu\n", y->gvn, use_block - cfg->blocks));
    return use_block;
}

void tb_clear_anti_deps(TB_Function* f, TB_Worklist* ws) {
    worklist_clear(ws);
    worklist_push(ws, f->root_node);
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];

        if (tb_node_has_mem_out(n) || n->dt.type == TB_TAG_MEMORY) {
            // the anti-deps are applied to the tuple node (projs can't have extra inputs anyways)
            TB_Node* k = is_proj(n) ? n->inputs[0] : n;
            if (k->type != TB_ROOT) {
                tb_node_clear_extras(f, k);
            }
        }

        FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
    }
}

static void add_anti_deps(TB_Function* f, TB_CFG* cfg, TB_Node* ld, TB_Node* mem, TB_BasicBlock* early) {
    if (mem->type == TB_SPLITMEM) {
        return;
    }

    FOR_USERS(u, mem) {
        TB_Node* un = USERN(u);
        if (USERI(u) <= un->input_count) {
            // phis don't actually write to memory so we can move around them
            if (un->type != TB_PHI && tb_node_has_mem_out(un)) {
                tb_node_add_extra(f, un, ld);
            }
        }
    }
}

TB_BasicBlock* tb_late_sched(TB_Function* f, TB_CFG* cfg, TB_BasicBlock* lca, TB_Node* n) {
    FOR_USERS(use, n) {
        // to avoid projections stopping the sinking of nodes, we walk past
        // them whenever decision making here
        if (is_proj(USERN(use))) {
            FOR_USERS(use2, USERN(use)) {
                TB_BasicBlock* use_block = find_use_block(f, cfg, n, USERN(use), use2);
                if (use_block) { lca = find_lca(lca, use_block); }
            }
        } else {
            TB_BasicBlock* use_block = find_use_block(f, cfg, n, n, use);
            if (use_block) { lca = find_lca(lca, use_block); }
        }
    }

    return lca;
}

void tb_global_schedule(TB_Function* f, TB_Worklist* ws, TB_CFG cfg, bool early_only, TB_GetLatency get_lat) {
    TB_ASSERT_MSG(f->scheduled == NULL, "make sure when you're done with the schedule, you throw away the old one");

    CUIK_TIMED_BLOCK("schedule") {
        size_t node_count = f->node_count;

        // arraychads stay up
        f->scheduled_n = node_count + 32;
        f->scheduled = tb_arena_alloc(&f->tmp_arena, f->scheduled_n * sizeof(TB_BasicBlock*));
        FOR_N(i, 0, f->scheduled_n) { f->scheduled[i] = NULL; }

        TB_BasicBlock* bb0 = &cfg.blocks[0];
        ArenaArray(TB_Node*) pins = aarray_create(&f->tmp_arena, TB_Node*, (f->node_count / 32) + 16);
        CUIK_TIMED_BLOCK("pinned schedule") {
            TB_ASSERT_MSG(f->root_node->gvn == 0, "TB_ROOT can't move from VN 0");

            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                TB_ASSERT(bb->start->type != TB_NULL);

                f->scheduled[bb->start->gvn] = bb;
                bb->items = aarray_create(&f->tmp_arena, TB_Node*, 16);
                aarray_push(bb->items, bb->start);

                if (i == 0) {
                    // pin the ROOT's projections to the entry block
                    FOR_USERS(u, f->root_node) {
                        if (is_proj(USERN(u))) {
                            f->scheduled[USERN(u)->gvn] = bb0;
                            if (USERN(u) != bb->start) {
                                aarray_push(bb0->items, USERN(u));
                            }
                        }
                    }
                }

                if (cfg_is_region(bb->start)) {
                    FOR_USERS(u, bb->start) if (USERN(u)->type == TB_PHI) {
                        f->scheduled[USERN(u)->gvn] = bb;
                        aarray_push(bb->items, USERN(u));
                    }
                }
            }

            worklist_push(ws, f->root_node);
            for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
                TB_Node* n = ws->items[i];
                if (tb_node_is_pinned(n) && n->type != TB_ROOT) {
                    TB_BasicBlock* bb = f->scheduled[n->gvn];
                    if (n->type == TB_MACH_FRAME_PTR) {
                        TB_ASSERT(n->gvn < f->scheduled_n);
                        f->scheduled[n->gvn] = bb0;
                        aarray_push(bb0->items, n);
                        aarray_push(pins, n);
                        // aarray_push(bb0->items, n);
                    } else if (is_proj(n) && !tb_node_is_pinned(n->inputs[0])) {
                        // projections are always pinned but they might refer to nodes which
                        // aren't (x86 division), we can skip these here as they're technically
                        // unpinned.
                    } else if (n->type != TB_DEAD) {
                        TB_Node* curr = n;
                        while (bb == NULL) {
                            bb = f->scheduled[curr->gvn];
                            if (curr->type == TB_DEAD || cfg_is_region(curr)) { // dead block? odd
                                break;
                            }
                            curr = curr->inputs[0];
                        }

                        if (bb == NULL || curr->type == TB_DEAD) {
                            continue;
                        }

                        TB_OPTDEBUG(GCM)(printf("%s: %%%u\n  PIN .bb%zu\n", f->super.name, n->gvn, bb - cfg.blocks));

                        TB_ASSERT(n->gvn < f->scheduled_n);
                        aarray_push(pins, n);

                        if (f->scheduled[n->gvn] == NULL) {
                            f->scheduled[n->gvn] = bb;
                            aarray_push(bb->items, n);
                        }
                    }
                }

                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }
        }

        // we don't care about anti-deps here, they'll be inserted during late scheduling
        CUIK_TIMED_BLOCK("early schedule") {
            worklist_clear(ws);

            aarray_for(i, pins) {
                TB_Node* pin_n = pins[i];

                Elem* top = tb_arena_alloc(&f->tmp_arena, sizeof(Elem));
                top->parent = NULL;
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
                            if (!tb_node_is_pinned(in) && !worklist_test_n_set(ws, in)) {
                                TB_ASSERT(in->type != TB_NULL);

                                /*#if TB_OPTDEBUG_GCM
                                for (Elem* e = top; e; e = e->parent) {
                                    printf("  ");
                                }
                                printf("PUSH %%%u\n", in->gvn);
                                #endif*/

                                Elem* new_top = tb_arena_alloc(&f->tmp_arena, sizeof(Elem));
                                new_top->parent = top;
                                new_top->n = in;
                                new_top->i = in->input_count;
                                top = new_top;
                            }
                        }
                        continue;
                    }

                    // only pinned node in this walk
                    if (n != pin_n) {
                        TB_OPTDEBUG(GCM)(printf("%s: %%%u\n", f->super.name, n->gvn));

                        // start at the entry point
                        int best_depth = 0;
                        TB_BasicBlock* best = &cfg.blocks[0];

                        // choose deepest block
                        FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
                            TB_ASSERT(n->inputs[i]->gvn < f->scheduled_n);
                            TB_BasicBlock* bb = f->scheduled[n->inputs[i]->gvn];
                            if (bb == NULL) {
                                // input has no scheduling... weird?
                                TB_OPTDEBUG(GCM)(printf("  IN %%%u @ dead\n", n->inputs[i]->gvn));
                                continue;
                            }

                            TB_OPTDEBUG(GCM)(printf("  IN %%%u @ bb%zu\n", n->inputs[i]->gvn, bb - cfg.blocks));
                            if (best_depth < bb->dom_depth) {
                                best_depth = bb->dom_depth;
                                best = bb;
                            }
                        }

                        TB_OPTDEBUG(GCM)(printf("  INTO .bb%zu\n", best - cfg.blocks));
                        TB_ASSERT(n->gvn < f->scheduled_n);
                        f->scheduled[n->gvn] = best;

                        // unpinned nodes getting moved means their users need to move too
                        FOR_USERS(u, n) if (is_proj(USERN(u))) {
                            TB_ASSERT(USERN(u)->type != TB_NULL);
                            f->scheduled[USERN(u)->gvn] = best;
                        }

                        dyn_array_put(ws->items, n);
                    }

                    Elem* parent = top->parent;
                    tb_arena_free(&f->tmp_arena, top, sizeof(Elem));
                    top = parent;

                    /*#if TB_OPTDEBUG_GCM
                    for (Elem* e = top; e; e = e->parent) {
                        printf("  ");
                    }
                    printf("POP %%%u\n", n->gvn);
                    #endif*/
                }
            }
        }

        if (early_only) {
            // only resolve anti-deps
            CUIK_TIMED_BLOCK("anti-dep schedule") {
                FOR_REV_N(i, 0, dyn_array_length(ws->items)) {
                    TB_Node* n = ws->items[i];
                    TB_ASSERT(n->type != TB_NULL);
                    TB_ASSERT(!tb_node_is_pinned(n));
                    TB_ASSERT(n->gvn < f->scheduled_n);

                    TB_BasicBlock* curr = f->scheduled[n->gvn];

                    // insert anti-deps
                    if (!tb_node_has_mem_out(n)) {
                        TB_Node* mem = tb_node_mem_in(n);
                        if (mem != NULL) {
                            add_anti_deps(f, &cfg, n, mem, curr);
                        }
                    }

                    // final schedule for a node is decided by this point so we place it into the correct bucket
                    FOR_USERS(u, n) {
                        TB_Node* un = USERN(u);
                        if (is_proj(un) && USERI(u) == 0) {
                            aarray_push(curr->items, un);
                        }
                    }
                    aarray_push(curr->items, n);
                }
            }
        } else {
            // move nodes closer to their usage site
            CUIK_TIMED_BLOCK("late schedule") {
                FOR_REV_N(i, 0, dyn_array_length(ws->items)) {
                    TB_Node* n = ws->items[i];
                    TB_ASSERT(n->type != TB_NULL);
                    TB_ASSERT(!tb_node_is_pinned(n));

                    TB_OPTDEBUG(GCM)(printf("%s: TRY LATE %%%u\n", f->super.name, n->gvn));

                    TB_BasicBlock* lca = NULL;
                    TB_BasicBlock* curr = f->scheduled[n->gvn];

                    // insert anti-deps
                    if (!tb_node_has_mem_out(n)) {
                        TB_Node* mem = tb_node_mem_in(n);
                        if (mem != NULL) {
                            add_anti_deps(f, &cfg, n, mem, curr);
                        }
                    }

                    lca = tb_late_sched(f, &cfg, lca, n);

                    if (lca != NULL) {
                        TB_ASSERT_MSG(curr, "we made it to late sched without an early sched?");

                        // replace old BB entry, also if old is a natural loop we might
                        // be better off hoisting the values if possible.
                        if (curr != lca && lca->dom_depth > curr->dom_depth) {
                            TB_BasicBlock* better = sched_into_good_block(f, get_lat, n, curr, lca);
                            if (curr != better) {
                                TB_OPTDEBUG(GCM)(
                                    printf("  LATE  %%%u into .bb%zu: ", n->gvn, better - cfg.blocks),
                                    tb_print_dumb_node(NULL, n),
                                    printf("\n")
                                );

                                f->scheduled[n->gvn] = better;

                                // unpinned nodes getting moved means their users need to move too
                                FOR_USERS(u, n) {
                                    TB_Node* un = USERN(u);
                                    if (is_proj(un) && USERI(u) == 0) {
                                        f->scheduled[un->gvn] = better;
                                    }
                                }
                                curr = better;
                            }
                        }
                    }

                    // final schedule for a node is decided by this point so we place it into the correct bucket
                    FOR_USERS(u, n) {
                        TB_Node* un = USERN(u);
                        if (is_proj(un) && USERI(u) == 0) {
                            aarray_push(curr->items, un);
                        }
                    }
                    aarray_push(curr->items, n);
                }
            }
        }

        worklist_clear(ws);
    }
}

////////////////////////////////
// Liveness analysis
////////////////////////////////
// we don't require local scheduling (only global) to run this since SSA implies the necessary info
void tb_dataflow(TB_Function* f, TB_Arena* arena, TB_CFG cfg) {
    size_t bb_count = aarray_length(cfg.blocks);
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
                aarray_for(j, bb->items) {
                    TB_Node* n = bb->items[j];
                    TB_ASSERT(n->gvn < node_count);

                    if (n->dt.type != TB_TAG_CONTROL && n->dt.type != TB_TAG_MEMORY) {
                        set_remove(&bb->gen, n->gvn);
                        set_put(&bb->kill, n->gvn);
                    }

                    if (n->type == TB_PHI) {
                        TB_Node* region = n->inputs[0];
                        FOR_N(k, 1, n->input_count) {
                            TB_Node* pred = cfg_get_pred(&cfg, region, k - 1);
                            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];

                            TB_Node* in = n->inputs[k];
                            if (!set_get(&pred_bb->kill, in->gvn)) {
                                TB_ASSERT(in->gvn < node_count);

                                // asked for from the previous block, but it should be "killed" in the PHI's block
                                // to avoid propagating it further
                                set_put(&pred_bb->gen, in->gvn);
                            }
                        }
                    } else {
                        FOR_N(k, 1, n->input_count) {
                            TB_Node* in = n->inputs[k];
                            if (in && !set_get(&bb->kill, in->gvn)) {
                                TB_ASSERT(in->gvn < node_count);
                                set_put(&bb->gen, in->gvn);
                            }
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
                        if (pred->input_count > 0 && pred->type != TB_DEAD) {
                            worklist_push(ws, pred);
                        }
                    }
                }
            }

            aarray_for(i, cfg.blocks) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                TB_Node* start = bb->start;
                if (cfg_is_region(start)) {
                    FOR_USERS(u, start) {
                        TB_Node* n = USERN(u);
                        if (n->type == TB_PHI && n->dt.type != TB_TAG_MEMORY) {
                            TB_ASSERT(USERI(u) == 0);
                            FOR_N(k, 1, n->input_count) {
                                TB_Node* pred = cfg_get_pred(&cfg, start, k - 1);
                                TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];

                                TB_ASSERT(n->inputs[k]->gvn < node_count);
                                set_put(&pred_bb->live_out, n->inputs[k]->gvn);
                            }
                        }
                    }
                }
            }
        }

        #if TB_OPTDEBUG_DATAFLOW
        // log live ins and outs
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];

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

