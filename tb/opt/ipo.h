#include <pool.h>
#include <futex.h>

// INLINES ALLOWED:
//   SMALL     + SMALL  = TRUE
//   SMALL_MED + SMALL  = TRUE
//   SMALL_MED + MEDIUM = TRUE
//   MEDIUM    + SMALL  = TRUE
typedef enum {
    SIZE_SMALL,
    SIZE_SMALL_MED,
    SIZE_MEDIUM,
    SIZE_LARGE,
} SizeClass;

// functions track caller -> callee, we're tracking the opposite
// relationship for our IPSCCP solver.
typedef struct CallGraphEdge {
    struct CallGraphEdge* next;
    TB_Function* caller;
} CallGraphEdge;

struct IPOSolver {
    TB_Module* mod;
    NL_HashSet visited;

    size_t ws_cap;
    size_t ws_cnt;
    TB_Function** ws;

    SizeClass* size_classes;
    int* ref_count;

    CallGraphEdge** call_graph;
};

typedef struct {
    bool on_stack;
    int index, low_link;
} SCCNode;

typedef struct {
    TB_Arena* arena;
    size_t fn_count;
    NL_Table nodes;

    size_t stk_cnt;
    TB_Function** stk;

    int index;
} SCC;

static mtx_t aaa;

static int op_weights[256] = {
    // loops will increase size even if it doesn't generate ops
    [TB_NATURAL_LOOP]         = 5,
    [TB_AFFINE_LOOP]          = 5,
    // pointers
    [TB_PTR_OFFSET]           = 1,
    [TB_LOAD]                 = 1,
    [TB_STORE]                = 1,
    // integer binary ops
    [TB_AND ... TB_SMOD]      = 1,
    // float binary ops
    [TB_FADD ... TB_FMAX]     = 1,
    // comparisons
    [TB_CMP_EQ ... TB_CMP_NE] = 2,
};

static int bin_count;
static int type2bin[256];
static int bin_weights[256];

#ifndef TB_NO_THREADS
static once_flag bin_init = ONCE_FLAG_INIT;
#else
static bool bin_init;
#endif

static void init_bins(void) {
    FOR_N(i, 0, 256) {
        int w = op_weights[i];

        // if two nodes have the same weight, they can share a bin
        int found = -1;
        FOR_N(j, 0, bin_count) {
            if (bin_weights[j] == w) {
                found = j;
                break;
            }
        }

        if (found >= 0) {
            type2bin[i] = found;
        } else {
            bin_weights[bin_count] = w;
            type2bin[i] = bin_count++;
        }
    }
}

static SizeClass classify_size(TB_Function* f, TB_Worklist* ws) {
    cuikperf_region_start("classify_size", NULL);

    #ifndef TB_NO_THREADS
    call_once(&bin_init, init_bins);
    #else
    if (!bin_init) {
        bin_init = true;
        init_bins();
    }
    #endif

    // find all nodes
    CUIK_TIMED_BLOCK("push all") {
        worklist_clear(ws);
        worklist_push(ws, f->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }
    }

    int hist[256] = { 0 };

    // clone all nodes in kid into f (GVN while we're at it)
    TB_Node* n;
    while (n = worklist_pop(ws), n) {
        if (n->type < 256) { hist[type2bin[n->type]] += 1; }
    }

    int weighted_sum = 0;
    FOR_N(i, 0, bin_count) {
        weighted_sum += hist[i] * bin_weights[i];
    }

    SizeClass size_class = SIZE_SMALL;
    if (weighted_sum >= 1000) {
        size_class = SIZE_LARGE;
    } else if (weighted_sum >= 100) {
        size_class = SIZE_MEDIUM;
    } else if (weighted_sum >= 20) {
        size_class = SIZE_SMALL_MED;
    }

    TB_OPTDEBUG(INLINE3)(printf("classify_size(%s) = %d\n", f->super.name, weighted_sum));

    cuikperf_region_end();
    return size_class;
}

static TB_Function* static_call_site(TB_Node* n) {
    // is this call site a static function call
    TB_ASSERT(n->type == TB_CALL || n->type == TB_TAILCALL);
    if (n->inputs[2]->type != TB_SYMBOL) return NULL;

    TB_Symbol* target = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;
    return target->tag == TB_SYMBOL_FUNCTION ? (TB_Function*) target : NULL;
}

static SCCNode* scc_walk(SCC* restrict scc, IPOSolver* ipo, TB_Function* f, TB_Worklist* ws) {
    SCCNode* n = tb_arena_alloc(scc->arena, sizeof(SCCNode));
    n->index = scc->index;
    n->low_link = scc->index;
    n->on_stack = true;
    scc->index += 1;
    nl_table_put(&scc->nodes, f, n);

    scc->stk[scc->stk_cnt++] = f;

    // consider the successors
    TB_Node* callgraph = f->root_node->inputs[0];
    TB_ASSERT(callgraph->type == TB_CALLGRAPH);
    FOR_N(i, 1, callgraph->input_count) {
        TB_Node* call = callgraph->inputs[i];
        TB_Function* target = static_call_site(call);
        if (target != NULL) {
            SCCNode* succ = nl_table_get(&scc->nodes, target);
            if (succ == NULL) {
                succ = scc_walk(scc, ipo, target, ws);
                if (n->low_link > succ->low_link) { n->low_link = succ->low_link; }
            } else if (succ->on_stack) {
                if (n->low_link > succ->index) { n->low_link = succ->index; }
            }
        }
    }

    FOR_USERS(u, f->root_node) {
        if (USERN(u)->type == TB_SYMBOL) {
            TB_Symbol* target = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeSymbol)->sym;
            if (target->tag != TB_SYMBOL_FUNCTION) { continue; }

            FOR_USERS(u2, USERN(u)) {
                ipo->ref_count[((TB_Function*) target)->uid] += 1;
            }
        }
    }

    // we're the root, construct an SCC
    if (n->low_link == n->index) {
        TB_Function* kid_f;
        do {
            TB_ASSERT(scc->stk_cnt > 0);
            kid_f = scc->stk[--scc->stk_cnt];

            ipo->size_classes[kid_f->uid] = classify_size(f, ws);

            SCCNode* kid_n = nl_table_get(&scc->nodes, kid_f);
            kid_n->on_stack = false;
            ipo->ws[ipo->ws_cnt++] = kid_f;
        } while (kid_f != f);
    }

    return n;
}

static _Thread_local TB_Worklist* ipo_worklist;
static void func_opt_task(TPool* tp, void** arg) {
    TB_Function* f = arg[0];
    Futex* tracker = arg[1];

    cuikperf_region_start("optimize", f->super.name);
    if (ipo_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ipo_worklist = tb_worklist_alloc();
    }

    log_debug("OPT: %s: function local optimizer", f->super.name);
    tb_opt(f, ipo_worklist, false);

    tracker[0] += 1;
    if (tracker[0] == tracker[1]) { // might be done?
        futex_signal(&tracker[0]);
    }
    cuikperf_region_end();
}

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
    // processing we'll just ask it to retry immediately.
    IPSCCP_STATUS_RETRY,
};

static Lattice* ipsccp_return_type(TB_Function* f) {
    Lattice* t = &TOP_IN_THE_SKY;
    if (f->types == NULL) {
        return &TOP_IN_THE_SKY;
    }

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

    return t;
}

static void func_sccp_task(TPool* tp, void** arg) {
    TB_Function* f = arg[0];
    TB_Module* m = f->super.module;

    if (ipo_worklist == NULL) {
        // we just leak these btw, i don't care yet
        ipo_worklist = tb_worklist_alloc();
    }

    for (;;) {
        // retry | in_list -> processing
        f->ipsccp_status = IPSCCP_STATUS_PROCESSING;

        cuikperf_region_start("IPSCCP", f->super.name);
        Lattice* old_type = ipsccp_return_type(f);

        f->worklist = ipo_worklist;
        // the temp arena might've been freed, let's restore it
        if (f->tmp_arena.top == NULL) {
            tb_arena_create(&f->tmp_arena, "Tmp");
            TB_ASSERT(f->tmp_arena.top);
        }

        int progress = tb_opt_cprop(f, true, false);

        tb_arena_destroy(&f->tmp_arena);
        f->worklist = NULL;

        // update the IPSCCP rets, this value has only one writer at any one time
        // so we don't need a CAS
        Lattice* new_type = ipsccp_return_type(f);
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
                push_ipsccp_job(m, e->caller);
            }
        }
        cuikperf_region_end();

        // processing -> idle, if we lose the only other option is retrying so we do that
        if (atomic_compare_exchange_strong(&f->ipsccp_status, &(int){ IPSCCP_STATUS_PROCESSING }, IPSCCP_STATUS_IDLE)) {
            break;
        }
    }

    // we're done with the solver now
    m->ipsccp_tracker[0] += 1;
    if (m->ipsccp_tracker[0] == m->ipsccp_tracker[1]) {
        futex_signal(&m->ipsccp_tracker[0]);
    }
}

void push_ipsccp_job(TB_Module* m, TB_Function* f) {
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
    if (status != next && next == IPSCCP_STATUS_IN_LIST) {
        log_debug("OPT: %s: pushed to IPSCCP worklist", f->super.name);
        m->ipsccp_tracker[1] += 1;

        void* args[2] = { f };
        tpool_add_task2(m->ipsccp_pool, func_sccp_task, 2, args);
    }
}

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Worklist* ws, TB_Node* call_site, TB_Function* kid);
bool tb_module_ipo(TB_Module* m, TPool* pool) {
    cuikperf_region_start("IPO", NULL);
    mtx_init(&aaa, mtx_plain);

    CUIK_TIMED_BLOCK("resize barrier") {
        symbolhs_resize_barrier(&m->symbols);
    }

    // fill initial worklist with all external function calls :)
    //
    // two main things we wanna know are if something is alive and when to inline (eventually
    // we can incorporate IPSCCP)
    SCC scc = { 0 };
    scc.arena = get_temporary_arena(m);

    // Run function-local opts and
    CUIK_TIMED_BLOCK("initial optimize round") {
        Futex tracker[2] = { 0 };
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION) {
                ((TB_Function*) s)->uid = scc.fn_count++;

                void* args[2] = { s, tracker };
                tracker[1] += 1;

                tpool_add_task2(pool, func_opt_task, 2, args);
            }
        }

        // finish up our initial round of function-local optimizations
        int64_t old;
        while (old = tracker[0], old != tracker[1]) {
            futex_wait(&tracker[0], old);
        }

        log_debug("Starting IPO with %"PRId64" functions", tracker[1]);
    }

    IPOSolver ipo = { 0 };
    ipo.ws_cap = scc.fn_count;
    ipo.ws = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));
    ipo.size_classes = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(SizeClass));
    ipo.ref_count = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(int));
    ipo.call_graph = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(CallGraphEdge*));
    m->ipo = &ipo;

    FOR_N(i, 0, scc.fn_count) {
        ipo.ref_count[i] = 0;
        ipo.call_graph[i] = NULL;
    }

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, 500);

    CUIK_TIMED_BLOCK("build SCC") {
        TB_ArenaSavepoint sp = tb_arena_save(scc.arena);
        scc.stk      = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));
        scc.nodes    = nl_table_arena_alloc(scc.arena, scc.fn_count);

        // build strongly connected components
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION && nl_table_get(&scc.nodes, s) == NULL) {
                scc_walk(&scc, &ipo, (TB_Function*) s, &ws);
            }
        }

        tb_arena_restore(scc.arena, sp);
    }

    CUIK_TIMED_BLOCK("Call graph construction") {
        FOR_N(i, 0, ipo.ws_cnt) {
            TB_Function* f = ipo.ws[i];
            TB_ASSERT(f->super.tag == TB_SYMBOL_FUNCTION);

            // construct call graph
            TB_Node* callgraph = f->root_node->inputs[0];
            TB_ASSERT(callgraph->type == TB_CALLGRAPH);
            FOR_N(i, 1, callgraph->input_count) {
                TB_Node* call = callgraph->inputs[i];
                TB_Function* target = static_call_site(call);
                if (target != NULL) {
                    CallGraphEdge* edge = tb_arena_alloc(scc.arena, sizeof(CallGraphEdge));
                    edge->next   = ipo.call_graph[target->uid];
                    edge->caller = f;
                    ipo.call_graph[target->uid] = edge;
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("IPSCCP") {
        m->during_ipsccp = true;
        m->ipsccp_pool = pool;
        m->ipsccp_tracker[0] = m->ipsccp_tracker[1];

        // any entry-point functions will be pushed first
        nbhs_for(entry, &m->symbols) {
            TB_Symbol* s = *entry;
            if (s->tag == TB_SYMBOL_FUNCTION && s->linkage == TB_LINKAGE_PUBLIC) {
                TB_Function* f = (TB_Function*) s;
                f->ipsccp_args = lattice_tuple_from_node(f, f->root_node);

                push_ipsccp_job(m, f);
            }
        }

        int64_t old;
        while (old = m->ipsccp_tracker[0], old != m->ipsccp_tracker[1]) {
            futex_wait(&m->ipsccp_tracker[0], old);
        }
        m->during_ipsccp = false;
    }

    cuikperf_region_end();
    return true;

    // we've got our bottom up ordering on the worklist... start trying to inline callsites
    bool progress = false;

    // IPO {
    //   SCC
    //   forever {
    //     IPSCCP
    //
    //     bottom-up inlining (makes dirty functions)
    //     re-opt dirty functions
    //       edit SCC
    //     }
    //   }
    // }
    TB_OPTDEBUG(INLINE)(printf("BOTTOM-UP ORDER:\n"));
    FOR_N(i, 0, ipo.ws_cnt) {
        TB_Function* f = ipo.ws[i];
        if (f->super.tag == TB_SYMBOL_DEAD) {
            continue;
        }

        static const char* size_strs[] = { "small", "small-medium", "medium", "large" };
        SizeClass size = ipo.size_classes[f->uid];

        TB_OPTDEBUG(INLINE)(printf("* FUNCTION: %s (%s)\n", f->super.name, size_strs[size]));

        TB_Node* callgraph = f->root_node->inputs[0];
        TB_ASSERT(callgraph->type == TB_CALLGRAPH);

        size_t i = 1;
        while (i < callgraph->input_count) {
            TB_Node* call = callgraph->inputs[i++];
            TB_Function* target = static_call_site(call);
            if (target == NULL) {
                continue;
            }

            SizeClass target_size = ipo.size_classes[target->uid];

            bool should = false;
            // large functions will stop inlining completely
            if (size != SIZE_LARGE && target_size != SIZE_LARGE) {
                if (size == SIZE_SMALL || target_size == SIZE_SMALL) {
                    should = true;
                }
            }

            TB_OPTDEBUG(INLINE)(printf("  -> %s (from %%%u, %s)... %s\n", target->super.name, call->gvn, size_strs[target_size], should ? "YES" : "NO"));

            if (should) {
                inline_into(scc.arena, f, &ws, call, target);
                i -= 1;
                progress = true;

                // remove edge on graph
                ipo.ref_count[target->uid] -= 1;
                if (ipo.ref_count[target->uid] == 0 && target->super.linkage == TB_LINKAGE_PRIVATE) {
                    // function is now unreachable, we can kill it
                    TB_OPTDEBUG(INLINE)(printf("     KILL!\n"));
                    tb_function_destroy(target);
                }

                // re-evaluate our code size
                SizeClass new_size = classify_size(f, &ws);
                if (size != new_size) {
                    TB_OPTDEBUG(INLINE)(printf("  -> grown to %s\n", size_strs[new_size]));
                    ipo.size_classes[f->uid] = size = new_size;
                }
            }
        }
    }

    worklist_free(&ws);
    cuikperf_region_end();
    m->ipo = NULL;

    return progress;
}

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Worklist* ws, TB_Node* call_site, TB_Function* kid) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node** clones = tb_arena_alloc(arena, kid->node_count * sizeof(TB_Node*));
    memset(clones, 0, kid->node_count * sizeof(TB_Node*));

    // find nodes & alloc clones
    worklist_clear(ws);
    worklist_push(ws, kid->root_node);
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

    {
        // TODO(NeGate): region-ify the exit point
        TB_Node* kid_root = clones[kid->root_node->gvn];
        TB_ASSERT(kid_root->type == TB_ROOT);
        TB_ASSERT(kid_root->input_count == 2);

        TB_Node* ret = kid_root->inputs[1];
        TB_ASSERT(ret->type == TB_RETURN);

        for (size_t i = 0; i < call_site->user_count;) {
            TB_Node* un = USERN(&call_site->users[i]);
            if (is_proj(un)) {
                int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
                if (index >= 2) { index += 1; }

                subsume_node(f, un, ret->inputs[index]);
            } else {
                i += 1;
            }
        }
        tb_kill_node(f, ret);

        subsume_node(f, kid_root, f->root_node);
        tb_kill_node(f, call_site);
    }
    tb_arena_restore(arena, sp);
}
