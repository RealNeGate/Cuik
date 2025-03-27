
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

typedef struct {
    TB_Module* mod;
    NL_HashSet visited;

    size_t ws_cap;
    size_t ws_cnt;
    TB_Function** ws;

    SizeClass* size_classes;
    int* ref_count;
} IPOSolver;

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
        worklist_push(ws, f->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }
    }

    int hist[256] = { 0 };
    // tb_print_dumb(f);

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

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Worklist* ws, TB_Node* call_site, TB_Function* kid);
bool tb_module_ipo(TB_Module* m) {
    CUIK_TIMED_BLOCK("resize barrier") {
        symbolhs_resize_barrier(&m->symbols);
    }

    // fill initial worklist with all external function calls :)
    //
    // two main things we wanna know are if something is alive and when to inline (eventually
    // we can incorporate IPSCCP)
    SCC scc = { 0 };
    scc.arena    = get_temporary_arena(m);
    nbhs_for(entry, &m->symbols) {
        TB_Symbol* s = *entry;
        if (s->tag == TB_SYMBOL_FUNCTION) {
            ((TB_Function*) s)->uid = scc.fn_count++;
        }
    }

    IPOSolver ipo = { 0 };
    ipo.ws_cap = scc.fn_count;
    ipo.ws = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));
    ipo.size_classes = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(SizeClass));
    ipo.ref_count = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(int));
    FOR_N(i, 0, scc.fn_count) {
        ipo.ref_count[i] = 0;
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

    // we've got our bottom up ordering on the worklist... start trying to inline callsites
    bool progress = false;

    TB_OPTDEBUG(INLINE)(printf("BOTTOM-UP ORDER:\n"));
    FOR_N(i, 0, ipo.ws_cnt) {
        TB_Function* f = ipo.ws[i];

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
                progress = true;
            }
        }
    }

    worklist_free(&ws);
    return progress;
}

static TB_Node* inline_clone_node(TB_Function* f, TB_Node* call_site, TB_Node** clones, TB_Node* n) {
    // special cases
    if (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT) {
        // this is a parameter, just hook it directly to the inputs of
        // the callsite.
        //
        // 0:ctrl, 1:mem, 2:rpc, 3... params
        int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
        clones[n->gvn] = call_site->inputs[index];

        TB_ASSERT(clones[n->gvn]);
        return clones[n->gvn];
    } else if (clones[n->gvn] != NULL) {
        return clones[n->gvn];
    }

    size_t extra = extra_bytes(n);
    TB_Node* cloned = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);

    // clone extra data (i hope it's that easy lol)
    memcpy(cloned->extra, n->extra, extra);
    clones[n->gvn] = cloned;

    // fill cloned edges
    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        TB_Node* in = inline_clone_node(f, call_site, clones, n->inputs[i]);

        cloned->inputs[i] = in;
        add_user(f, cloned, in, i);
    }

    #if TB_OPTDEBUG_INLINE
    // printf("CLONE "), tb_print_dumb_node(NULL, n), printf(" => "), tb_print_dumb_node(NULL, cloned), printf("\n");
    #endif

    return cloned;

    /* TB_Node* k = tb__gvn(f, cloned, extra);
    if (k != cloned) {
        #if TB_OPTDEBUG_INLINE
        printf(" => GVN");
        #endif
    }
    printf("\n");
    return  = cloned; */
}

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Worklist* ws, TB_Node* call_site, TB_Function* kid) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node** clones = tb_arena_alloc(arena, kid->node_count * sizeof(TB_Node*));
    memset(clones, 0, kid->node_count * sizeof(TB_Node*));

    // find all nodes
    {
        worklist_push(ws, kid->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }
    }

    // clone all nodes in kid into f (GVN while we're at it)
    TB_Node* n;
    while (n = worklist_pop(ws), n) {
        inline_clone_node(f, call_site, clones, n);
    }

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

        subsume_node(f, kid_root, f->root_node);
        tb_kill_node(f, call_site);
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
    tb_arena_restore(arena, sp);
}
