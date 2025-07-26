// We walk the functions bottom-up:
// * find deepest function in SCCs
// * attempt to inline
// * optimize the function
// * update SCCs
enum {
    IPO_FUNC_IDLE,

    // we're in the process of optimizing
    IPO_FUNC_OPT,

    // we're done optimizing, we can be used as
    // part of an inlining attempt.
    IPO_FUNC_READY,
};

// we wanna be idle, READY -> IDLE or just IDLE
static void ipo_lock(TB_Function* f) {
    FutexV old = f->ipo_lock;
    do {
        if (old == IPO_FUNC_OPT) {
            futex_wait(&f->ipo_lock, IPO_FUNC_OPT);
            old = f->ipo_lock;
        }
    } while (old != IPO_FUNC_IDLE && !atomic_compare_exchange_strong(&f->ipo_lock, &(FutexV){ IPO_FUNC_READY }, IPO_FUNC_IDLE));
}

static bool ipo_try_lock(TB_Function* f) {
    FutexV old = f->ipo_lock;
    return old == IPO_FUNC_IDLE || atomic_compare_exchange_strong(&f->ipo_lock, &(FutexV){ IPO_FUNC_READY }, IPO_FUNC_IDLE);
}

static void ipo_opt_task(TPool* tp, void** arg) {
    TB_Function* f = arg[0];
    Futex* tracker = arg[1];

    cuikperf_region_start("optimize", f->super.name);
    if (ipo_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ipo_worklist = tb_worklist_alloc();
    }

    tb_opt(f, ipo_worklist, true);

    // notify the IPO thread to stop waiting
    f->ipo_lock = IPO_FUNC_READY;
    futex_signal(&f->ipo_lock);

    log_debug("%s: optimized!", f->super.name);

    tracker[0] += 1;
    if (tracker[0] == tracker[1]) { // might be done?
        futex_signal(&tracker[0]);
    }
    cuikperf_region_end();
}

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Worklist* ws, TB_Node* call_site, TB_Function* kid) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node** clones = tb_arena_alloc(arena, kid->node_count * sizeof(TB_Node*));
    memset(clones, 0, kid->node_count * sizeof(TB_Node*));

    // find nodes & alloc clones
    worklist_clear(ws);
    worklist_push(ws, kid->root_node);

    TB_Node* va_start_n = NULL;
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];
        FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }

        if (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT) {
            // this is a parameter, just hook it directly to the inputs of
            // the callsite.
            //
            // 0:ctrl, 1:mem, 2:rpc, 3... params
            int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            TB_ASSERT(call_site->inputs[index]);
            clones[n->gvn] = call_site->inputs[index];
        } else {
            // allocate new nodes here, we'll fill in the use-def edges later
            size_t extra = extra_bytes(n);
            TB_Node* cloned = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);

            // clone extra data (i hope it's that easy lol)
            memcpy(cloned->extra, n->extra, extra);
            clones[n->gvn] = cloned;

            // points to root? we can GVN immediately
            if (n->input_count == 1 && n->inputs[0] == kid->root_node) {
                cloned->inputs[0] = f->root_node;
                add_user(f, cloned, f->root_node, 0);

                clones[n->gvn] = tb__gvn(f, cloned, extra);
            }

            latuni_set(f, cloned, kid->types[n->gvn]);
            if (n->type == TB_VA_START) {
                TB_ASSERT(va_start_n == NULL);
                va_start_n = clones[n->gvn];
            }
        }
    }

    // fill in the use-def edges
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];
        TB_Node* k = clones[n->gvn];
        if (
            (n->input_count == 1 && n->inputs[0] == kid->root_node) ||
            (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT)
        ) {
            #if TB_OPTDEBUG_INLINE2
            printf("REDIRECT "), tb_print_dumb_node(NULL, n), printf(" => "), tb_print_dumb_node(NULL, k), printf("\n");
            #endif
        } else {
            // fill cloned edges
            FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
                TB_Node* in = clones[n->inputs[j]->gvn];
                k->inputs[j] = in;
                add_user(f, k, in, j);
            }

            #if TB_OPTDEBUG_INLINE2
            printf("CLONE "), tb_print_dumb_node(NULL, n), printf(" => "), tb_print_dumb_node(NULL, k), printf("\n");
            #endif
        }
    }

    // kill edge in callgraph
    TB_Node* callgraph = f->root_node->inputs[0];
    TB_ASSERT(callgraph->type == TB_CALLGRAPH);

    FOR_N(i, 1, callgraph->input_count) {
        if (callgraph->inputs[i] == call_site) {
            set_input(f, callgraph, callgraph->inputs[callgraph->input_count - 1], i);
            set_input(f, callgraph, NULL, callgraph->input_count - 1);
            callgraph->input_count--;
            break;
        }
    }

    // append all callee callgraph edges to caller
    TB_Node* kid_callgraph = clones[kid->root_node->inputs[0]->gvn];
    FOR_N(i, 1, kid_callgraph->input_count) {
        add_input_late(f, callgraph, kid_callgraph->inputs[i]);
    }
    tb_kill_node(f, kid_callgraph);

    TB_Node* kid_root = clones[kid->root_node->gvn];
    TB_ASSERT(kid_root->type == TB_ROOT);

    TB_Node** rets;
    size_t ret_cnt = 0;
    if (kid_root->input_count > 2) {
        size_t phi_len = 1;
        TB_Node* any_ret = NULL;
        FOR_N(i, 1, kid_root->input_count) {
            TB_Node* end = kid_root->inputs[i];
            if (end->type == TB_RETURN) {
                if (!any_ret) {
                    ret_cnt = end->input_count;
                    any_ret = end;
                } else {
                    TB_ASSERT(ret_cnt == end->input_count);
                }

                phi_len += 1;
            } else {
                add_input_late(f, f->root_node, end);
            }
        }

        rets = tb_arena_alloc(arena, ret_cnt * sizeof(TB_Node*));
        if (phi_len == 2) {
            // any_ret is the only ret
            memcpy(rets, any_ret->inputs, ret_cnt * sizeof(TB_Node*));
        } else if (phi_len == 1) {
            // no paths actually return...
            __debugbreak();
        } else {
            TB_Node* r = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, phi_len - 1, sizeof(TB_NodeRegion));
            rets[0] = r;
            FOR_N(i, 1, ret_cnt) {
                rets[i] = tb_alloc_node(f, TB_PHI, any_ret->inputs[i]->dt, phi_len, 0);
            }

            int k = 1;
            FOR_N(i, 1, kid_root->input_count) {
                TB_Node* end = kid_root->inputs[i];
                if (end->type == TB_RETURN) {
                    FOR_N(j, 1, end->input_count) {
                        TB_ASSERT(rets[k]->dt.raw == end->inputs[j]->dt.raw);
                        set_input(f, rets[k], end->inputs[j], j);
                    }
                    k++;
                }
            }
        }
    } else if (kid_root->inputs[1]->type == TB_RETURN) {
        TB_Node* ret = kid_root->inputs[1];

        ret_cnt = ret->input_count;
        rets = tb_arena_alloc(arena, ret_cnt * sizeof(TB_Node*));
        memcpy(rets, ret->inputs, ret_cnt * sizeof(TB_Node*));

        tb_kill_node(f, ret);
    } else {
        ret_cnt = 0;
        rets = NULL;
    }

    size_t va_list_size = (call_site->input_count - 3) - kid->prototype->param_count;
    for (size_t i = 0; i < call_site->user_count;) {
        TB_Node* un = USERN(&call_site->users[i]);
        if (is_proj(un)) {
            int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
            if (index >= 2) { index += 1; }

            TB_ASSERT(index < ret_cnt);
            subsume_node(f, un, rets[index]);
        } else {
            i += 1;
        }
    }
    subsume_node(f, kid_root, f->root_node);

    if (kid->prototype->has_varargs && va_start_n) {
        TB_Node* va_list_n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 1, sizeof(TB_NodeLocal));
        set_input(f, va_list_n, f->root_node, 0);
        TB_NODE_SET_EXTRA(va_list_n, TB_NodeLocal, .size = 8 * va_list_size, .align = 8);

        // we don't want this interfering with our "next_mem_user" call
        set_input(f, call_site, NULL, 1);

        // fill each arg into a 8B entry in our hypothetical array
        TB_Node* mem = next_mem_user(clones[kid->params[1]->gvn]);
        FOR_REV_N(i, 0, va_list_size) {
            TB_Node* arg = call_site->inputs[3 + kid->prototype->param_count + i];
            TB_ASSERT(arg);

            TB_Node* addr = va_list_n;
            if (i) {
                addr = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
                set_input(f, addr, va_list_n, 1);
                set_input(f, addr, make_int_node(f, TB_TYPE_I64, i*8), 2);
            }

            TB_Node* st = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
            set_input(f, st, mem->inputs[0], 0);
            set_input(f, st, mem->inputs[1], 1);
            set_input(f, st, addr, 2);
            set_input(f, st, arg,  3);
            set_input(f, mem, st, 1);
        }

        subsume_node(f, va_start_n, va_list_n);
    }
    tb_kill_node(f, call_site);
    tb_arena_restore(arena, sp);
}

static bool should_full_inline_by_size(TB_Module* m, IPOSolver* ipo, int caller_size, TB_Function* target) {
    SizeClass caller_class = classify_size(caller_size);
    int target_size = ipo->size_metric[target->uid];

    // if all callsites inlined, how big would that get?
    int rc = ipo->ref_count[target->uid] + is_function_root(target);

    SizeClass lower_class = classify_size(target_size);
    TB_OPTDEBUG(INLINE)(printf("  lower=%d,%s\n", target_size, size_strs[lower_class]));

    // large functions will stop inlining completely
    if (caller_class == SIZE_LARGE || lower_class == SIZE_LARGE) {
        return false;
    }

    // one caller? always inline
    if (rc == 1) {
        TB_OPTDEBUG(INLINE)(printf("  YES, always inline because there's one caller!\n"));
        return true;
    }

    // two callers? always inline small functions
    if (lower_class == SIZE_SMALL || (lower_class == SIZE_SMALL_MED && rc == 2)) {
        TB_OPTDEBUG(INLINE)(printf("  YES, the target is tiny!\n"));
        return true;
    }

    // TODO(NeGate): crazy heuristics
    return false;
}

static bool run_inliner(TB_Module* m, IPOSolver* ipo, SCC* scc, TB_Worklist* ws, TPool* pool) {
    bool progress = false;
    TB_OPTDEBUG(INLINE)(printf("BOTTOM-UP ORDER:\n"));

    Futex tracker[2] = { 0 };
    FOR_N(i, 0, ipo->ws_cnt) {
        TB_Function* f = ipo->ws[i];
        if (f->super.tag == TB_SYMBOL_DEAD) {
            continue;
        }

        cuikperf_region_start("process", f->super.name);
        TB_Node* callgraph = f->root_node->inputs[0];
        TB_ASSERT(callgraph->type == TB_CALLGRAPH);

        cuikperf_region_start("lock", f->super.name);
        // lock all the nodes necessary for the inlining attempt
        bool ready = true;
        for (size_t i = 1; i < callgraph->input_count;) {
            TB_Node* call = callgraph->inputs[i++];
            TB_Function* target = static_call_site(call);
            if (target == NULL) {
                continue;
            }

            if (!ipo_try_lock(target)) {
                log_debug("%s: waiting on %s to complete optimizing!", f->super.name, target->super.name);
                ready = false;
                break;
            }
        }

        // if we're not ready to process we have two options, one is to wait
        // for all the relevant functions to complete and the other is to process
        // a function on the same depth level as ourselves
        if (!ready) {
            // wait for all the deps
            for (size_t i = 1; i < callgraph->input_count;) {
                TB_Node* call = callgraph->inputs[i++];
                TB_Function* target = static_call_site(call);
                if (target == NULL) {
                    continue;
                }

                ipo_lock(target);
            }
            log_debug("%s: we're ready now!", f->super.name);
        }
        cuikperf_region_end();

        int size = ipo->size_metric[f->uid];
        TB_OPTDEBUG(INLINE)(printf("* FUNCTION: %s (%d, %s)\n", f->super.name, size, classify_size_str(size)));

        for (size_t i = 1; i < callgraph->input_count;) {
            TB_Node* call = callgraph->inputs[i++];
            TB_Function* target = static_call_site(call);
            if (target == NULL) {
                continue;
            }

            cuikperf_region_start("inline?", target->super.name);
            TB_OPTDEBUG(INLINE)(printf("  -> %s?", target->super.name));

            bool should = false;
            if (!target->no_inline) {
                // cannot inline functions which directly recurse for code size reasons
                should = should_full_inline_by_size(m, ipo, size, target);
                if (!should) {
                    TB_OPTDEBUG(INLINE)(printf("  NO, the upper bound on inlining was small enough!\n"));
                }
            } else {
                TB_OPTDEBUG(INLINE)(printf("  NO, inlining was banned!\n"));
            }

            if (should) {
                inline_into(scc->arena, f, ws, call, target);
                i -= 1;
                progress = true;

                TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Inlining '%s'", target->super.name));

                // remove edge on graph
                ipo->ref_count[target->uid] -= 1;
                if (ipo->ref_count[target->uid] == 0 && !is_function_root(target)) {
                    // function is now unreachable, we can kill it
                    TB_OPTDEBUG(INLINE)(printf("     KILL!\n"));
                    tb_function_destroy(target);
                }

                // re-evaluate our code size
                int new_size = compute_size(f, ws);
                if (classify_size(size) != classify_size(new_size)) {
                    TB_OPTDEBUG(INLINE)(printf("  -> grown to %d, %s\n", new_size, classify_size_str(new_size)));
                }
                ipo->size_metric[f->uid] = size = new_size;
            }
            cuikperf_region_end();
        }

        log_debug("%s: queue'd to optimize now that it has tried inlining!", f->super.name);
        f->ipo_lock = IPO_FUNC_OPT;

        // optimize the function now that we've inlined the callees we wanted
        void* args[2] = { f, tracker };
        // tracker[1] += 1;
        // tpool_add_task2(pool, ipo_opt_task, 2, args);
        ipo_opt_task(pool, args);
        cuikperf_region_end();

        // update the callgraph, this may change the shape of the SCC
    }

    // finish up our initial round of function-local optimizations
    /* int64_t old;
    while (old = tracker[0], old != tracker[1]) {
         futex_wait(&tracker[0], old);
    } */

    // we might have empty sections, we should prune those now

    return progress;
}
