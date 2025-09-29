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
    int refs;
} CallGraphEdge;

struct IPOSolver {
    TB_Module* mod;
    NL_HashSet visited;

    size_t ws_cap;
    size_t ws_cnt;
    TB_Function** ws;

    int* size_metric;
    int* ref_count;

    // IPSCCP worklist given that we don't multithread the solver
    ArenaArray(TB_Function*) ipsccp_ws;

    CallGraphEdge** call_graph;
};

typedef struct {
    bool on_stack;
    int index, low_link, depth;
    int group;
} SCCNode;

typedef struct {
    TB_Arena* arena;
    size_t fn_count;
    NL_Table nodes;

    size_t stk_cnt;
    TB_Function** stk;

    size_t group_count;
    int index;
} SCC;

static const char* size_strs[] = { "small", "small-medium", "medium", "large" };

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
    // function calls? idk what this should count as
    [TB_CALL]                 = 10,
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

static SizeClass classify_size(int metric) {
    if (metric >= 1000) {
        return SIZE_LARGE;
    } else if (metric >= 50) {
        return SIZE_MEDIUM;
    } else if (metric >= 10) {
        return SIZE_SMALL_MED;
    } else {
        return SIZE_SMALL;
    }
}

static const char* classify_size_str(int metric) {
    return size_strs[classify_size(metric)];
}

static int compute_size(TB_Function* f, TB_Worklist* ws) {
    cuikperf_region_start("compute_size", NULL);

    #ifndef TB_NO_THREADS
    call_once(&bin_init, init_bins);
    #else
    if (!bin_init) {
        bin_init = true;
        init_bins();
    }
    #endif

    // find all nodes
    // CUIK_TIMED_BLOCK("push all")
    {
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

    TB_OPTDEBUG(INLINE3)(printf("compute_size(%s) = %d\n", f->super.name, weighted_sum));

    cuikperf_region_end();
    return weighted_sum;
}

static TB_Function* static_call_site(TB_Node* n) {
    // is this call site a static function call
    TB_ASSERT(n->type == TB_CALL || n->type == TB_TAILCALL);
    if (n->inputs[2]->type != TB_SYMBOL) return NULL;

    TB_Symbol* target = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;
    return target->tag == TB_SYMBOL_FUNCTION ? (TB_Function*) target : NULL;
}

static bool is_function_root(TB_Function* f) {
    TB_Module* m = f->super.module;
    if (f->ipsccp_escape) {
        return true;
    }

    // COMDAT functions aren't by default alive, even if they're exported
    // since it's possible they're not the one that ends up picked.
    if (m->sections[f->section].comdat.type != TB_COMDAT_NONE) {
        return false;
    }

    return f->super.linkage == TB_LINKAGE_PUBLIC;
}

static void update_callgraph_edges(IPOSolver* ipo, TB_Function* f) {
    // search existing callgraph edges for any which have been removed
    for (CallGraphEdge* e = ipo->call_graph[f->uid]; e; e = e->next) {
        int refs = 0;
        TB_Function* caller = e->caller;

        TB_Node* callgraph = caller->root_node->inputs[0];
        TB_ASSERT(callgraph->type == TB_CALLGRAPH);
        FOR_N(i, 1, callgraph->input_count) {
            TB_Node* call = callgraph->inputs[i];
            TB_Function* target = static_call_site(call);
            if (target == caller) {
                refs += 1;
            }
        }
    }
}

static SCCNode* scc_walk(SCC* restrict scc, IPOSolver* ipo, TB_Function* f, TB_Worklist* ws, int depth) {
    SCCNode* n = tb_arena_alloc(scc->arena, sizeof(SCCNode));
    n->index = scc->index;
    n->low_link = scc->index;
    n->on_stack = true;
    scc->index += 1;
    nl_table_put(&scc->nodes, f, n);

    scc->stk[scc->stk_cnt++] = f;

    // consider the successors
    bool is_cycle = false;
    TB_Node* callgraph = f->root_node->inputs[0];
    TB_ASSERT(callgraph->type == TB_CALLGRAPH);
    FOR_N(i, 1, callgraph->input_count) {
        TB_Node* call = callgraph->inputs[i];
        TB_Function* target = static_call_site(call);

        // easy case of a cycle, the other case is an SCC
        // composed of more than one node.
        if (target == f) {
            is_cycle = true;
        }

        if (target != NULL) {
            SCCNode* succ = nl_table_get(&scc->nodes, target);
            if (succ == NULL) {
                succ = scc_walk(scc, ipo, target, ws, depth + 1);
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
        int head = scc->stk_cnt;
        while (scc->stk[--head] != f) {}

        if (is_cycle || scc->stk_cnt - head > 1) {
            // printf("Call cycle (depth=%d, %zu):\n", depth, scc->stk_cnt - head);

            is_cycle = true;
            f->attrs |= TB_FUNCTION_NOINLINE;
        }

        TB_Function* kid_f;
        do {
            TB_ASSERT(scc->stk_cnt > 0);
            kid_f = scc->stk[--scc->stk_cnt];

            if (is_cycle) {
                // printf("  %s\n", kid_f->super.name);
            }

            SCCNode* kid_n = nl_table_get(&scc->nodes, kid_f);
            kid_n->depth = depth;
            kid_n->on_stack = false;
            kid_n->group = scc->group_count;
            ipo->ws[ipo->ws_cnt++] = kid_f;
        } while (kid_f != f);

        scc->group_count++;
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

    // if (strcmp(f->super.name, "collectgarbage") == 0) {
    log_debug("OPT: %s: function local optimizer", f->super.name);
    tb_opt(f, ipo_worklist, false);
    // }

    if (tracker) {
        tracker[0] += 1;
        if (tracker[0] == tracker[1]) { // might be done?
            futex_signal(&tracker[0]);
        }
    }
    cuikperf_region_end();
}

#include "ipsccp.h"
#include "inline.h"

void tb_dump_stats(void) {
    uint64_t total_time = 0;
    FOR_N(i, 0, STATS_MAX) {
        total_time += tb_stats[i];
    }

    if (0) {
        // CSV dump
        FOR_N(i, 0, STATS_MAX) {
            printf("%s,%zu\n", stats_get_name(i), tb_stats[i]);
        }
    } else {
        printf("Total exec time: %f ms\n\n", total_time / 1000000.0);
        printf("  ----  Time  ----   --- Name ---\n");
        FOR_N(i, 0, STATS_MAX) {
            uint64_t t = tb_stats[i];
            double percent = total_time ? ((double)t / (double)total_time) * 100.0 : 0.0;
            int frac_p = fmodf(percent * 10.0, 10.0);
            int int_p  = floorf(percent);

            printf("  %10.4f (%3d.%1d%%)   %s\n", tb_stats[i] / 1000000.0, int_p, frac_p, stats_get_name(i));
        }
        printf("\n");
    }
}

bool tb_module_ipo(TB_Module* m, TPool* pool) {
    cuikperf_region_start("Optimizer", NULL);
    mtx_init(&aaa, mtx_plain);

    #if TB_OPTDEBUG_SERVER
    startup_server(m);
    #endif

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
        if (pool) {
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
        } else {
            nbhs_for(entry, &m->symbols) {
                TB_Symbol* s = *entry;
                if (s->tag == TB_SYMBOL_FUNCTION) {
                    ((TB_Function*) s)->uid = scc.fn_count++;

                    void* args[2] = { s, NULL };
                    func_opt_task(NULL, args);
                }
            }
        }

        log_debug("Initial optimization of %"PRId64" functions", scc.fn_count);
    }

    IPOSolver ipo = { 0 };
    ipo.ws_cap = scc.fn_count;
    ipo.ws = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));
    ipo.size_metric = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(int));
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
            if (s->tag == TB_SYMBOL_FUNCTION && is_function_root((TB_Function*) s) && nl_table_get(&scc.nodes, s) == NULL) {
                scc_walk(&scc, &ipo, (TB_Function*) s, &ws, 0);
            }
        }

        tb_arena_restore(scc.arena, sp);
    }

    CUIK_TIMED_BLOCK("Call graph construction") {
        FOR_N(i, 0, ipo.ws_cnt) {
            TB_Function* f = ipo.ws[i];
            TB_ASSERT(f->super.tag == TB_SYMBOL_FUNCTION);

            #if TB_OPTDEBUG_INLINE4
            const char* color = "white";
            switch (classify_size(ipo.size_metric[f->uid])) {
                case SIZE_SMALL: color = "green"; break;
                case SIZE_SMALL_MED: color = "yellow"; break;
                case SIZE_MEDIUM: color = "orange"; break;
                case SIZE_LARGE: color = "red"; break;
            }
            printf("%s [shape=box style=filled fillcolor=\"%s\"]\n", f->super.name, color);
            #endif

            // construct call graph
            TB_Node* callgraph = f->root_node->inputs[0];
            TB_ASSERT(callgraph->type == TB_CALLGRAPH);
            FOR_N(i, 1, callgraph->input_count) {
                TB_Node* call = callgraph->inputs[i];
                TB_Function* target = static_call_site(call);
                if (target != NULL) {
                    CallGraphEdge* edge = NULL;

                    // if we already refer to this function, we
                    // don't need two call graph edges to it
                    for (CallGraphEdge* e = ipo.call_graph[target->uid]; e; e = e->next) {
                        if (e->caller == f) {
                            edge = e;
                            edge->refs += 1;
                            break;
                        }
                    }

                    if (edge == NULL) {
                        edge = tb_arena_alloc(scc.arena, sizeof(CallGraphEdge));
                        edge->next   = ipo.call_graph[target->uid];
                        edge->caller = f;
                        edge->refs   = 1;
                        ipo.call_graph[target->uid] = edge;

                        #if TB_OPTDEBUG_INLINE4
                        printf("%s -> %s\n", f->super.name, target->super.name);
                        #endif
                    }
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("IPSCCP") {
        TB_ArenaSavepoint sp = tb_arena_save(scc.arena);
        ipo.ipsccp_ws = aarray_create(scc.arena, TB_Function*, scc.fn_count);

        STATS_ENTER(IPSCCP);
        run_ipsccp(m, pool);
        STATS_EXIT(IPSCCP);

        tb_arena_restore(scc.arena, sp);
    }

    bool progress;
    CUIK_TIMED_BLOCK("Inliner") {
        progress = run_inliner(m, &ipo, &scc, &ws, pool);
    }

    worklist_free(&ws);
    cuikperf_region_end();
    m->ipo = NULL;
    return progress;
}
