////////////////////////////////
// IPSCCP
////////////////////////////////
// we can run this solver on multiple threads so long as we
// have a concurrent queue to throw functions on when solving.
//
// we "seed" the function-local solvers with a root type defined
// by the different concurrent solvers which MEET their type with
// the existing root type, we keep running until we're no longer
// pushing new nodes onto the queue and have thus reached the maximal
// fixed point. At which point we can rewrite each graph.
enum {
    IPSCCP_STATUS_IDLE,

    // waiting to be processed
    IPSCCP_STATUS_IN_LIST,

    // it's in the middle of processing, we can't push it onto
    // the list at this moment else we'll have two tasks on the
    // same function but we still need to handle the race around
    // retrying.
    IPSCCP_STATUS_PROCESSING,

    // if we ask to process a function which is already
    // processing we'll just ask it to push the function
    // back onto the queue rather than completing.
    IPSCCP_STATUS_RETRY,
};

static Lattice* ipsccp_return_type(TB_Function* f, bool top_as_tuple) {
    Lattice* t = &TOP_IN_THE_SKY;
    if (f->types) {
        TB_Arena* arena = get_permanent_arena(f->super.module);
        FOR_N(i, 1, f->root_node->input_count) {
            TB_Node* ret = f->root_node->inputs[i];
            if (ret->type == TB_RETURN) {
                Lattice* ctrl = latuni_get(f, ret->inputs[0]);
                if (ctrl != &LIVE_IN_THE_SKY) {
                    continue;
                }

                size_t size = sizeof(Lattice) + ret->input_count*sizeof(Lattice*);
                Lattice* l = tb_arena_alloc(arena, size);
                *l = (Lattice){ LATTICE_TUPLE, ._elem_count = ret->input_count };

                l->elems[0] = ctrl;
                FOR_N(i, 1, ret->input_count) {
                    l->elems[i] = latuni_get(f, ret->inputs[i]);
                }

                // intern & free
                Lattice* k = latticehs_intern(&f->super.module->lattice_elements, l);
                if (k != l) { tb_arena_free(arena, l, size); }

                // meet over all paths
                t = lattice_meet(f, t, k);
            }
        }
    }

    if (t == &TOP_IN_THE_SKY) {
        if (top_as_tuple) {
            // dead function
            size_t ret_count = 3 + f->prototype->return_count;
            size_t size = sizeof(Lattice) + ret_count*sizeof(Lattice*);

            TB_Arena* arena = get_permanent_arena(f->super.module);
            Lattice* l = tb_arena_alloc(arena, size);
            *l = (Lattice){ LATTICE_TUPLE, ._elem_count = ret_count };

            l->elems[0] = &DEAD_IN_THE_SKY;
            l->elems[1] = &TOP_IN_THE_SKY;
            l->elems[2] = &XNULL_IN_THE_SKY; // RPC is at least not NULL
            FOR_N(i, 3, ret_count) {
                l->elems[i] = &TOP_IN_THE_SKY;
            }

            // intern & free
            Lattice* k = latticehs_intern(&f->super.module->lattice_elements, l);
            if (k != l) { tb_arena_free(arena, l, size); }

            return k;
        } else {
            return &TOP_IN_THE_SKY;
        }
    }

    return t;
}

static void func_sccp_task(TPool* tp, void** arg) {
    TB_Function* f = arg[0];
    TB_Module* m = f->super.module;

    if (ipo_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ipo_worklist = tb_worklist_alloc();
    }

    int old;
    do {
        // in_list | retrying -> processing
        f->ipsccp_status = IPSCCP_STATUS_PROCESSING;

        cuikperf_region_start("solve", f->super.name);
        Lattice* old_type = ipsccp_return_type(f, false);

        // the temp arena might've been freed, let's restore it
        if (f->tmp_arena.top == NULL) {
            tb_arena_create(&f->tmp_arena, "Tmp");
            TB_ASSERT(f->tmp_arena.top);
        }

        worklist_clear(ipo_worklist);
        f->worklist = ipo_worklist;

        if (f->cprop == NULL) {
            f->cprop = tb_arena_alloc(&f->tmp_arena, sizeof(CProp));
            *f->cprop = tb_opt_cprop_init(f);
        }

        tb_opt_cprop_analyze(f, f->cprop, true);
        f->worklist = NULL;

        // update the IPSCCP rets, this value has only one writer at any one time
        // so we don't need a CAS
        Lattice* new_type = ipsccp_return_type(f, true);
        if (old_type != new_type) {
            #if 0
            mtx_lock(&aaa);
            printf("%s: ", f->super.name);
            print_lattice(old_type);
            printf(" => ");
            print_lattice(new_type);
            printf("\n");
            mtx_unlock(&aaa);
            #endif

            atomic_store_explicit(&f->ipsccp_ret, new_type, memory_order_release);
            log_debug("OPT: %s: made progress, pushing users!", f->super.name);

            IPOSolver* ipo = m->ipo;
            for (CallGraphEdge* e = ipo->call_graph[f->uid]; e; e = e->next) {
                // if the function is unreachable, don't notify it
                Lattice* args = e->caller->ipsccp_args;
                if (args != NULL) {
                    push_ipsccp_job(m, e->caller);
                }
            }
        }
        cuikperf_region_end();

        old = IPSCCP_STATUS_PROCESSING;
    } while (!atomic_compare_exchange_strong(&f->ipsccp_status, &old, IPSCCP_STATUS_IDLE));

    // we're done with the solver now
    m->ipsccp_tracker[0] += 1;
    if (m->ipsccp_tracker[0] == m->ipsccp_tracker[1]) {
        futex_signal(&m->ipsccp_tracker[0]);
    }
}

static void func_sccp_rewrite_task(TPool* tp, void** arg) {
    TB_Function* f = arg[0];
    TB_Module* m = f->super.module;

    if (ipo_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ipo_worklist = tb_worklist_alloc();
    }

    cuikperf_region_start("rewrite", f->super.name);

    // push all the nodes back in, we didn't preserve the worklist since it's
    // cheap to recompute it and the memory cost wouldn't been annoying.
    TB_Worklist* ws = ipo_worklist;
    worklist_clear(ws);
    worklist_push(ws, f->root_node);
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];
        FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
    }

    f->worklist = ws;
    tb_opt_cprop_rewrite(f);
    tb_opt_cprop_deinit(f, f->cprop);
    f->cprop = NULL;
    f->worklist = NULL;

    if (worklist_count(ws)) {
        m->ipsccp_progress = true;
        worklist_clear(ws);
    }

    TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "IPSCCP"));

    // we've rewritten the graph now, let's get the size metrics
    // for inlining.
    IPOSolver* ipo = m->ipo;
    ipo->size_metric[f->uid] = compute_size(f, ws);

    tb_arena_destroy(&f->tmp_arena);
    cuikperf_region_end();

    // we're done with the solver now
    m->ipsccp_tracker[0] += 1;
    if (m->ipsccp_tracker[0] == m->ipsccp_tracker[1]) {
        futex_signal(&m->ipsccp_tracker[0]);
    }
}

void push_ipsccp_job(TB_Module* m, TB_Function* f) {
    if (m->ipsccp_pool) {
        // either...
        // * we're not on the list, push to list.
        // * we're processing, move to retry state.
        int status = f->ipsccp_status, next;
        do {
            if (status == IPSCCP_STATUS_IDLE) {
                next = IPSCCP_STATUS_IN_LIST;
            } else if (status == IPSCCP_STATUS_PROCESSING) {
                next = IPSCCP_STATUS_RETRY;
            } else {
                break;
            }
        } while (!atomic_compare_exchange_strong(&f->ipsccp_status, &status, next));

        // we've moved into the list? yay!
        if (status == IPSCCP_STATUS_IDLE && next == IPSCCP_STATUS_IN_LIST) {
            log_debug("OPT: %s: pushed to IPSCCP worklist", f->super.name);
            m->ipsccp_tracker[1] += 1;

            void* args[2] = { f };
            tpool_add_task2(m->ipsccp_pool, func_sccp_task, 2, args);
        }
    } else {
        IPOSolver* ipo = m->ipo;
        int status = atomic_load_explicit(&f->ipsccp_status, memory_order_relaxed);
        if (status == IPSCCP_STATUS_IDLE) {
            atomic_store_explicit(&f->ipsccp_status, IPSCCP_STATUS_IN_LIST, memory_order_relaxed);
            aarray_push(ipo->ipsccp_ws, f);
        }
    }
}

static void dump_ipsccp(TB_Module* m) {
    nbhs_for(entry, &m->symbols) {
        TB_Symbol* s = *entry;
        if (s->tag == TB_SYMBOL_FUNCTION) {
            TB_Function* f = (TB_Function*) s;
            Lattice* args = f->ipsccp_args ? f->ipsccp_args : &TOP_IN_THE_SKY;
            Lattice* ret  = f->ipsccp_ret  ? f->ipsccp_ret  : &TOP_IN_THE_SKY;
            if (args != &TOP_IN_THE_SKY) {
                printf("%s: ", f->super.name);
                print_lattice(args);
                printf(" -> ");
                print_lattice(ret);
                printf("\n");
            } else {
                printf("%s: DEAD\n", f->super.name);
            }
        }
    }
}

static bool run_ipsccp(TB_Module* m, TPool* pool) {
    m->during_ipsccp = true;
    m->ipsccp_pool = pool;
    m->ipsccp_tracker[0] = m->ipsccp_tracker[1] = 0;

    if (pool) {
        // any entry-point functions will be pushed
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION && is_function_root((TB_Function*) s)) {
                TB_Function* f = (TB_Function*) s;

                // since this is the lowest element, any meet should
                // yield this and thus should always win the race.
                f->ipsccp_args = lattice_tuple_from_node(f, f->root_node);
                push_ipsccp_job(m, f);
            }
        }

        int64_t old;
        while (old = m->ipsccp_tracker[0], old != m->ipsccp_tracker[1]) {
            futex_wait(&m->ipsccp_tracker[0], old);
        }
        m->during_ipsccp = false;
        m->ipsccp_tracker[0] = m->ipsccp_tracker[1] = 0;

        // dump_ipsccp(m);

        // Transform phase
        //   we can apply the rewrites now, in parallel
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION) {
                TB_Function* f = (TB_Function*) s;
                Lattice* args = f->ipsccp_args ? f->ipsccp_args : &TOP_IN_THE_SKY;
                Lattice* ret  = f->ipsccp_ret  ? f->ipsccp_ret  : &TOP_IN_THE_SKY;
                if (args != &TOP_IN_THE_SKY) {
                    log_debug("OPT: %s: pushed to IPSCCP worklist", f->super.name);
                    m->ipsccp_tracker[1] += 1;

                    void* args[2] = { f };
                    tpool_add_task2(m->ipsccp_pool, func_sccp_rewrite_task, 2, args);
                }
            }
        }

        while (old = m->ipsccp_tracker[0], old != m->ipsccp_tracker[1]) {
            futex_wait(&m->ipsccp_tracker[0], old);
        }
    } else {
        // any entry-point functions will be pushed
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION && is_function_root((TB_Function*) s)) {
                TB_Function* f = (TB_Function*) s;

                // since this is the lowest element, any meet should
                // yield this and thus should always win the race.
                f->ipsccp_args = lattice_tuple_from_node(f, f->root_node);
                push_ipsccp_job(m, f);
            }
        }

        // Analysis phase
        IPOSolver* ipo = m->ipo;
        while (aarray_length(ipo->ipsccp_ws)) {
            TB_Function* f = aarray_pop(ipo->ipsccp_ws);
            atomic_store_explicit(&f->ipsccp_status, IPSCCP_STATUS_IDLE, memory_order_relaxed);

            cuikperf_region_start("solve", f->super.name);
            Lattice* old_type = ipsccp_return_type(f, false);

            // the temp arena might've been freed, let's restore it
            if (f->tmp_arena.top == NULL) {
                tb_arena_create(&f->tmp_arena, "Tmp");
                TB_ASSERT(f->tmp_arena.top);
            }

            worklist_clear(ipo_worklist);
            f->worklist = ipo_worklist;

            if (f->cprop == NULL) {
                f->cprop = tb_arena_alloc(&f->tmp_arena, sizeof(CProp));
                *f->cprop = tb_opt_cprop_init(f);
            }

            tb_opt_cprop_analyze(f, f->cprop, true);
            f->worklist = NULL;

            Lattice* new_type = ipsccp_return_type(f, true);
            if (old_type != new_type) {
                atomic_store_explicit(&f->ipsccp_ret, new_type, memory_order_relaxed);
                log_debug("OPT: %s: made progress, pushing users!", f->super.name);

                IPOSolver* ipo = m->ipo;
                for (CallGraphEdge* e = ipo->call_graph[f->uid]; e; e = e->next) {
                    // if the function is unreachable, don't notify it
                    Lattice* args = e->caller->ipsccp_args;
                    if (args != NULL) {
                        push_ipsccp_job(m, e->caller);
                    }
                }
            }
            cuikperf_region_end();
        }

        m->during_ipsccp = false;

        // Transform phase
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION) {
                TB_Function* f = (TB_Function*) s;
                Lattice* args = f->ipsccp_args ? f->ipsccp_args : &TOP_IN_THE_SKY;
                Lattice* ret  = f->ipsccp_ret  ? f->ipsccp_ret  : &TOP_IN_THE_SKY;
                if (args != &TOP_IN_THE_SKY) {
                    log_debug("OPT: %s: pushed to IPSCCP worklist", f->super.name);

                    void* args[2] = { f };
                    func_sccp_rewrite_task(NULL, args);
                }
            }
        }
    }

    return true;
}
