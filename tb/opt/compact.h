
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
                    FOR_USERS(u, n) if (IS_PROJ(USERN(u))) {
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

                Lattice** new_types = cuik_malloc(f->type_cap * sizeof(Lattice*));
                FOR_N(i, 0, f->type_cap) { new_types[i] = NULL; }

                FOR_N(i, 0, dyn_array_length(ws->items)) {
                    TB_Node* n = ws->items[i];
                    TB_Node* k = fwd[n->gvn];
                    new_types[k->gvn] = f->types[n->gvn];
                }

                TB_ASSERT(f->root_node->gvn == 0);
                cuik_free(f->types);
                f->types = new_types;
            }

            nl_hashset_clear(&f->gvn_nodes);
            FOR_N(i, 0, dyn_array_length(ws->items)) {
                TB_Node* n = ws->items[i];
                TB_Node* k = fwd[n->gvn];

                // connect new node to new inputs
                FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
                    TB_Node* moved_k = fwd[n->inputs[j]->gvn];
                    TB_ASSERT(moved_k != NULL);

                    #ifndef NDEBUG
                    set_input(f, n, NULL, j);
                    #endif

                    set_input(f, k, moved_k, j);
                }

                // extra deps need special treatment
                FOR_N(j, n->input_count, n->input_cap) if (n->inputs[j]) {
                    TB_Node* moved_k = fwd[n->inputs[j]->gvn];
                    TB_ASSERT(moved_k != NULL);

                    tb_node_add_extra(f, k, moved_k);
                }

                // rediscover the GVN table
                if (can_gvn(k)) {
                    nl_hashset_put2(&f->gvn_nodes, k, gvn_hash, gvn_compare);
                }

                TB_OPTDEBUG(COMPACT)(printf("%s: %p (%u) -> %p (%u)\n", f->super.name, n, n->gvn, k, k->gvn));
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
                if (IS_PROJ(un)) {
                    TB_ASSERT(USERI(u) == 0);
                    int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
                    if (index < param_count) {
                        f->params[index] = un;
                    }
                }
            }
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
                    FOR_USERS(u, n) if (IS_PROJ(USERN(u))) {
                        worklist_push(ws, USERN(u));
                    }
                }

                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }
        }

        CUIK_TIMED_BLOCK("compact IDs") {
            f->node_count = dyn_array_length(ws->items);
            if (f->types) {
                size_t old_type_cap = f->type_cap;
                f->type_cap = tb_next_pow2(f->node_count + 16);

                Lattice** new_types = cuik_malloc(f->type_cap * sizeof(Lattice*));
                FOR_N(i, 0, f->type_cap) { new_types[i] = NULL; }

                FOR_N(i, 0, dyn_array_length(ws->items)) {
                    uint32_t old_gvn = ws->items[i]->gvn;
                    if (old_gvn >= old_type_cap) {
                        new_types[i] = lattice_from_dt(f, ws->items[i]->dt);
                    } else {
                        new_types[i] = f->types[old_gvn];
                    }
                    ws->items[i]->gvn = i;
                }

                TB_ASSERT(f->root_node->gvn == 0);
                cuik_free(f->types);
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
