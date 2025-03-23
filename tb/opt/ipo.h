
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

static TB_Function* static_call_site(TB_Node* n) {
    // is this call site a static function call
    TB_ASSERT(n->type == TB_CALL || n->type == TB_TAILCALL);
    if (n->inputs[2]->type != TB_SYMBOL) return NULL;

    TB_Symbol* target = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;
    return target->tag == TB_SYMBOL_FUNCTION ? (TB_Function*) target : NULL;
}

static SCCNode* scc_walk(SCC* restrict scc, IPOSolver* ipo, TB_Function* f) {
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
                succ = scc_walk(scc, ipo, target);
                if (n->low_link > succ->low_link) { n->low_link = succ->low_link; }
            } else if (succ->on_stack) {
                if (n->low_link > succ->index) { n->low_link = succ->index; }
            }
        }
    }

    // we're the root, construct an SCC
    if (n->low_link == n->index) {
        TB_Function* kid_f;
        do {
            TB_ASSERT(scc->stk_cnt > 0);
            kid_f = scc->stk[--scc->stk_cnt];

            SCCNode* kid_n = nl_table_get(&scc->nodes, kid_f);
            kid_n->on_stack = false;
            ipo->ws[ipo->ws_cnt++] = kid_f;
        } while (kid_f != f);
    }

    return n;
}

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Node* call_site, TB_Function* kid);
bool tb_module_ipo(TB_Module* m) {
    // fill initial worklist with all external function calls :)
    //
    // two main things we wanna know are if something is alive and when to inline (eventually
    // we can incorporate IPSCCP)
    SCC scc = { 0 };
    scc.arena    = get_temporary_arena(m);
    // scc.fn_count = m->symbol_count[TB_SYMBOL_FUNCTION];

    IPOSolver ipo = { 0 };
    ipo.ws_cap = scc.fn_count;
    ipo.ws = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));

    #if 0
    CUIK_TIMED_BLOCK("build SCC") {
        TB_ArenaSavepoint sp = tb_arena_save(scc.arena);
        scc.stk      = tb_arena_alloc(scc.arena, scc.fn_count * sizeof(TB_Function*));
        scc.nodes    = nl_table_arena_alloc(scc.arena, scc.fn_count);

        // build strongly connected components
        TB_ThreadInfo* info = atomic_load_explicit(&m->first_info_in_module, memory_order_relaxed);
        while (info != NULL) {
            DynArray(TB_Symbol*) syms = info->symbols;
            dyn_array_for(i, syms) {
                TB_Symbol* s = syms[i];

                if (s->tag == TB_SYMBOL_FUNCTION && nl_table_get(&scc.nodes, s) == NULL) {
                    scc_walk(&scc, &ipo, (TB_Function*) s);
                }
            }
            info = info->next_in_module;
        }
        tb_arena_restore(scc.arena, sp);
    }
    #endif

    // we've got our bottom up ordering on the worklist... start trying to inline callsites
    bool progress = false;

    TB_OPTDEBUG(INLINE)(printf("BOTTOM-UP ORDER:\n"));
    FOR_N(i, 0, ipo.ws_cnt) {
        TB_Function* f = ipo.ws[i];

        TB_OPTDEBUG(INLINE)(printf("* FUNCTION: %s\n", f->super.name));

        TB_Node* callgraph = f->root_node->inputs[0];
        TB_ASSERT(callgraph->type == TB_CALLGRAPH);

        size_t i = 1;
        while (i < callgraph->input_count) {
            TB_Node* call = callgraph->inputs[i];
            TB_Function* target = static_call_site(call);

            // really simple getter/setter kind of heuristic
            if (target && target->node_count < 15) {
                TB_OPTDEBUG(INLINE)(printf("  -> %s (from v%u)\n", target->super.name, call->gvn));
                inline_into(scc.arena, f, call, target);
                progress = true;
            } else {
                i++;
            }
        }
    }

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
    printf("CLONE "), tb_print_dumb_node(NULL, n), printf(" => "), tb_print_dumb_node(NULL, cloned), printf("\n");
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

static void inline_into(TB_Arena* arena, TB_Function* f, TB_Node* call_site, TB_Function* kid) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node** clones = tb_arena_alloc(arena, kid->node_count * sizeof(TB_Node*));
    memset(clones, 0, kid->node_count * sizeof(TB_Node*));

    // find all nodes
    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, kid->node_count);
    {
        worklist_push(&ws, kid->root_node);
        for (size_t i = 0; i < dyn_array_length(ws.items); i++) {
            TB_Node* n = ws.items[i];
            FOR_USERS(u, n) { worklist_push(&ws, USERN(u)); }
        }
    }

    // clone all nodes in kid into f (GVN while we're at it)
    FOR_REV_N(i, 0, dyn_array_length(ws.items)) {
        inline_clone_node(f, call_site, clones, ws.items[i]);
    }
    worklist_free(&ws);

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
