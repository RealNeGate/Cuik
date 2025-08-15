// Based on the classic
//
// Exploiting Superword Level Parallelism with Multimedia Instruction Sets, 2000
//   Samuel Larsen and Saman Amarasinghe
//
// only type of PackSet we need
typedef struct Pair {
    struct Pair* next;
    int id;

    TB_Node* lhs;
    TB_Node* rhs;
} Pair;

typedef struct {
    int count;

    Pair* first;
    Pair* last;

    Pair** l_map;
    Pair** r_map;

    // depth[n->gvn]
    //   helps narrow node independence checks
    int* depth;
} PairSet;

typedef struct {
    TB_DataType v_dt;
    TB_Node* v_op;

    int width;
    TB_Node* ops[];
} VectorOp;

// int, float
static bool is_valid_vector_component(TB_DataType dt) {
    return TB_IS_INTEGER_TYPE(dt) || TB_IS_FLOAT_TYPE(dt);
}

static TB_DataType slp_node_data_type(TB_Node* n) {
    return n->type == TB_STORE ? n->inputs[3]->dt : n->dt;
}

static bool isomorphic(TB_Node* a, TB_Node* b) {
    if (a->type == b->type && a->dt.raw == b->dt.raw && a->input_count == b->input_count) {
        if (a->type >= TB_CMP_EQ && a->type <= TB_CMP_FLE) {
            return TB_NODE_GET_EXTRA_T(a, TB_NodeCompare)->cmp_dt.raw == TB_NODE_GET_EXTRA_T(b, TB_NodeCompare)->cmp_dt.raw;
        } else {
            return true;
        }
    }

    return false;
}

static bool independent(TB_Function* f, PairSet* pairs, TB_Node* a, TB_Node* b) {
    // siblings or equal?
    if (pairs->depth[a->gvn] == pairs->depth[b->gvn]) {
        return a != b;
    }

    cuikperf_region_start("independent", NULL);
    TB_Node* shallow = a;
    TB_Node* deep = b;
    if (pairs->depth[a->gvn] > pairs->depth[b->gvn]) {
        SWAP(TB_Node*, shallow, deep);
    }

    worklist_push(f->worklist, deep);
    int mark = pairs->depth[shallow->gvn];

    for (size_t i = 0; i < dyn_array_length(f->worklist->items); i++) {
        TB_Node* n = f->worklist->items[i];

        // printf("Checked... ");
        // tb_print_dumb_node(NULL, n);
        // printf(" depth=%d\n", pairs->depth[n->gvn]);

        FOR_N(i, 0, n->input_count) {
            if (n->inputs[i] == shallow) {
                cuikperf_region_end();
                return false;
            }

            // push inputs which are still below the mark
            if (n->inputs[i]) {
                int d = pairs->depth[n->inputs[i]->gvn];
                if (d >= 0 && d >= mark) {
                    worklist_push(f->worklist, n->inputs[i]);
                }
            }
        }
    }

    cuikperf_region_end();
    return true;
}

static bool is_adjacent_ref(MemRef a, MemRef b) {
    if (a.mem->type == TB_LOAD) {
        // load adjacency means they share mem deps
        if (a.mem->inputs[1] != b.mem->inputs[1]) { return false; }
    } else {
        // store adjacency means one will go into the other
        if (a.mem->inputs[1] != b.mem && a.mem != b.mem->inputs[1]) { return false; }
    }

    // one of them is touching starting the other's ass
    return a.base == b.base && a.size == b.size && (a.offset + a.size == b.offset || b.offset + b.size == a.offset);
}

static bool can_pack(TB_Function* f, PairSet* pairs, TB_Node* a, TB_Node* b, TB_Node* top) {
    if (pairs->l_map[a->gvn] || pairs->r_map[b->gvn]) {
        return false;
    }

    if (is_valid_vector_component(slp_node_data_type(a)) &&
        is_valid_vector_component(slp_node_data_type(b)) &&
        isomorphic(a, b) &&
        a->inputs[0] == b->inputs[0])
    {
        // stores don't do independence checks, packable ones are actually dependent
        // it's just treated as a special kind (adjacent and chained up nicely)
        if (a->type != TB_STORE && !independent(f, pairs, a, b)) {
            return false;
        }

        // if it's a memory op then it should share a base, we can't do sparse loads
        if (a->type == TB_LOAD || a->type == TB_STORE) {
            MemRef aa = compute_mem_ref(f, a);
            MemRef bb = compute_mem_ref(f, b);
            if (!is_adjacent_ref(aa, bb)) {
                return false;
            }
        }

        return true;
    }

    return false;
}

static int ref_cmp(const void* a, const void* b) {
    const MemRef* aa = a;
    const MemRef* bb = b;

    if (aa->mem->type != bb->mem->type) { return bb->mem->type - aa->mem->type; }
    if (aa->base->gvn != bb->base->gvn) { return aa->base->gvn - bb->base->gvn; }

    if (aa->index_count != bb->index_count) { return aa->index_count - bb->index_count; }
    FOR_REV_N(i, 0, aa->index_count) {
        if (aa->stride[i] != bb->stride[i]) { return aa->stride[i] - bb->stride[i]; }
        if (aa->index[i]->gvn != bb->index[i]->gvn) { return aa->index[i]->gvn - bb->index[i]->gvn; }
    }

    if (aa->size != bb->size) { return aa->size - bb->size; }
    if (aa->offset != bb->offset) { return aa->offset - bb->offset; }

    return 0;
}

static bool ref_same_indices(MemRef* a, MemRef* b) {
    if (a->index_count != b->index_count) {
        return false;
    }

    FOR_N(i, 0, a->index_count) {
        if (a->stride[i] != b->stride[i] || a->index[i] != b->index[i]) {
            return false;
        }
    }

    return true;
}

static void slp_add_pair(TB_Function* f, PairSet* pairs, TB_Node* lhs, TB_Node* rhs) {
    Pair* p = tb_arena_alloc(&f->tmp_arena, sizeof(Pair));
    p->next = NULL;
    p->id = pairs->count++;
    p->lhs = lhs;
    p->rhs = rhs;

    if (pairs->first == NULL) {
        pairs->first = pairs->last = p;
    } else {
        pairs->last->next = p;
        pairs->last = p;
    }

    pairs->l_map[lhs->gvn] = p;
    pairs->r_map[rhs->gvn] = p;
}

static void slp_walk(TB_Function* f, TB_Worklist* ws, TB_Node* n) {
    if (worklist_test_n_set(ws, n)) { return; }
    if (cfg_is_control(n)) {
        FOR_USERS(u, n) {
            if (cfg_is_control(USERN(u))) {
                slp_walk(f, ws, USERN(u));
            }
        }
    }
}

bool generate_pack(TB_Function* f, PairSet* pairs, TB_Worklist* ws, LoopOpt* ctx, TB_LoopTree* loop) {
    TB_Node* top = loop ? loop->header : f->root_node;
    ArenaArray(MemRef) refs = aarray_create(&f->tmp_arena, MemRef, 8);

    // compute schedule for "trace"
    CUIK_TIMED_BLOCK("trace") {
        // DFS walk
        worklist_clear(ws);
        worklist_push(ws, top);
        pairs->depth[top->gvn] = 0;

        if (loop != NULL) {
            FOR_USERS(u, top) {
                if (USERN(u)->type == TB_PHI && USERI(u) == 0) {
                    worklist_push(ws, USERN(u));
                }
            }
        }

        while (dyn_array_length(ws->items)) retry: {
            TB_Node* n = ws->items[dyn_array_length(ws->items) - 1];

            // process users before placing ourselves
            FOR_USERS(u, n) {
                TB_Node* un = USERN(u);
                if (!worklist_test_n_set(ws, un)) {
                    TB_Node* un_ctrl = ctx->ctrl[un->gvn];
                    TB_LoopTree* un_loop = un_ctrl ? nl_table_get(&ctx->loop_map, un_ctrl) : NULL;
                    if (loop == un_loop) {
                        dyn_array_put(ws->items, un);
                        goto retry;
                    }
                }
            }

            #if TB_OPTDEBUG_SLP
            tb_print_dumb_node(NULL, n);
            printf(" depth=%zu\n", dyn_array_length(ws->items));
            #endif

            dyn_array_pop(ws->items);
            pairs->depth[n->gvn] = dyn_array_length(ws->items);

            // find relevant memory ops
            if (n->type == TB_LOAD || n->type == TB_STORE) {
                aarray_push(refs, compute_mem_ref(f, n));
            }
        }
        worklist_clear(ws);
    }

    // sort for faster adjancency queries later on
    CUIK_TIMED_BLOCK("sort") {
        qsort(refs, aarray_length(refs), sizeof(MemRef), ref_cmp);
    }

    aarray_for(i, refs) {
        MemRef r = refs[i];

        #if TB_OPTDEBUG_SLP
        printf("AAA! ");
        tb_print_dumb_node(NULL, r.mem);
        printf(" (%%%u", r.base->gvn);
        FOR_N(j, 0, r.index_count) {
            printf(" + %%%u*%d", r.index[j]->gvn, r.stride[j]);
        }
        printf(" + %d, size=%d)\n", r.offset, r.size);
        #endif
    }
    TB_OPTDEBUG(SLP)(printf("\n"));

    CUIK_TIMED_BLOCK("SLP scan") {
        // scan for groups with the same base & element size
        int start = 0;
        while (start < aarray_length(refs)) {
            int end = start + 1;
            while (refs[end].base   == refs[start].base
                && ref_same_indices(&refs[end], &refs[start])
                && refs[end].size   == refs[start].size) {
                end++;
            }

            if (start + 1 != end) { // no group :(
                TB_Node* base = refs[start].base;
                int32_t size  = refs[start].size;
                TB_DataType elem_dt = refs[start].mem->type == TB_STORE ? refs[start].mem->inputs[3]->dt : refs[start].mem->dt;

                // find adjacent stores
                FOR_N(i, start, end) {
                    MemRef* a = &refs[i];
                    FOR_N(j, start+1, end) {
                        MemRef* b = &refs[j];

                        // there's a weird overlapping memory op
                        if (a->offset + size < b->offset) { break; }
                        if (a->offset + size == b->offset && can_pack(f, pairs, a->mem, b->mem, top)) {
                            slp_add_pair(f, pairs, a->mem, b->mem);
                        }
                    }
                }

                // extend packs based on use-def
                bool progress;
                do {
                    progress = false;

                    CUIK_TIMED_BLOCK("SLP extend") {
                        #if TB_OPTDEBUG_SLP
                        /*printf("=== PAIRS ===\n");
                        for (Pair* p = pairs->first; p; p = p->next) {
                            printf("\x1b[96m%%%-4u --   %%%-4u    %s\x1b[0m\n  ", p->lhs->gvn, p->rhs->gvn, tb_node_get_name(p->lhs->type));
                            tb_print_dumb_node(NULL, p->lhs);
                            printf("\n  ");
                            tb_print_dumb_node(NULL, p->rhs);
                            printf("\n\n");
                        }
                        printf("\n\n\n");*/
                        #endif

                        // check if all input edges do the same op
                        for (Pair* p = pairs->first; p; p = p->next) {
                            TB_Node* lhs = p->lhs;
                            TB_Node* rhs = p->rhs;
                            TB_ASSERT(lhs->type == rhs->type);

                            // loads always terminate the SIMD chains, they only have an address input and those are scalar
                            if (lhs->type == TB_LOAD) {
                                continue;
                            }

                            // can we pack the normal input ops (doesn't count the memory or address for a store)
                            FOR_N(j, lhs->type == TB_STORE ? 3 : 1, lhs->input_count) {
                                TB_Node* a = lhs->inputs[j];
                                TB_Node* b = rhs->inputs[j];
                                if (can_pack(f, pairs, a, b, top)) {
                                    slp_add_pair(f, pairs, a, b);
                                    progress = true;
                                } else {
                                    // printf("failed to pack... %%%u -- %%%u\n", a->gvn, b->gvn);
                                }
                            }
                        }
                    }
                } while (progress);
            }

            start = end;
        }
    }

    if (pairs->count == 0) {
        return false;
    }

    return true;
}

static int find_vector_index(VectorOp* op, TB_Node* src) {
    FOR_N(i, 0, op->width) {
        if (op->ops[i] == src) { return i; }
    }

    tb_panic("bad find_vector_index");
}

static bool viable_vector(TB_Function* f, NL_Table* ops, TB_DataType v_dt, VectorOp* op, int index) {
    TB_Node* leader = op->ops[0]->inputs[index];
    VectorOp* leader_op = nl_table_get(ops, leader);
    if (leader_op == NULL) {
        // might be a broadcasted value?
        FOR_N(i, 1, op->width) {
            TB_Node* src = op->ops[i]->inputs[index];
            if (src != leader) { return false; }
        }

        return true;
    }

    // all have to share the same vector source... for now
    FOR_N(i, 1, op->width) {
        TB_Node* src = op->ops[i]->inputs[index];
        VectorOp* src_op = nl_table_get(ops, src);
        if (src_op != leader_op) {
            return false;
        }
    }

    return true;
}

static TB_Node* gimme_vector(TB_Function* f, NL_Table* ops, VectorOp* op, int index) {
    TB_Node* leader = op->ops[0]->inputs[index];
    VectorOp* leader_op = nl_table_get(ops, leader);
    if (leader_op == NULL) {
        // might be a broadcasted value?
        FOR_N(i, 1, op->width) {
            TB_Node* src = op->ops[i]->inputs[index];
            if (src != leader) {
                TB_ASSERT_MSG(0, "shoulda rejected");
            }
        }

        TB_Node* n = tb_alloc_node(f, TB_VBROADCAST, op->v_dt, 2, 0);
        set_input(f, n, leader, 1);

        TB_Node* k = tb_opt_gvn_node(f, n);
        mark_node(f, k);
        return k;
    }

    // as long as all the ops come from the same leader, it's all good
    int* indices = tb_arena_alloc(&f->tmp_arena, op->width * sizeof(int));
    indices[0] = find_vector_index(leader_op, leader);

    FOR_N(i, 1, op->width) {
        TB_Node* src = op->ops[i]->inputs[index];
        VectorOp* src_op = nl_table_get(ops, src);
        if (src_op != leader_op) {
            TB_ASSERT_MSG(0, "shoulda rejected");
        }
        indices[i] = find_vector_index(src_op, src);
    }

    // sometimes we don't need shuffles
    bool ordered = true;
    FOR_N(i, 0, op->width) {
        if (i != indices[i]) { ordered = false; break; }
    }

    TB_ASSERT(leader_op->v_op);
    if (ordered) {
        tb_arena_free(&f->tmp_arena, indices, op->width * sizeof(int));
        return leader_op->v_op;
    }

    TB_Node* n = tb_alloc_node(f, TB_VSHUFFLE, op->v_dt, 2, sizeof(TB_NodeVShuffle) + op->width*sizeof(int));
    set_input(f, n, leader_op->v_op, 1);

    TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
    shuf->width = op->width;
    FOR_N(i, 0, op->width) {
        shuf->indices[i] = indices[i];
    }
    tb_arena_free(&f->tmp_arena, indices, op->width * sizeof(int));

    #if TB_OPTDEBUG_SLP
    printf("SHUFFLE: ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
    #endif

    TB_Node* k = tb_opt_gvn_node(f, n);
    mark_node(f, k);
    return k;
}

bool compile_packs(TB_Function* f, PairSet* pairs, TB_LoopTree* loop) {
    TB_Node* top = loop ? loop->header : f->root_node;
    DynArray(TB_Node*) combined = dyn_array_create(TB_Node*, 8);
    bool* visited  = tb_arena_alloc(&f->tmp_arena, pairs->count * sizeof(bool));
    FOR_N(i, 0, pairs->count) {
        visited[i] = false;
    }

    // Node -> VectorOp
    NL_Table ops = nl_table_alloc(64);
    DynArray(VectorOp*) schedule = dyn_array_create(VectorOp*, 32);

    // combine packs
    TB_OPTDEBUG(SLP)(printf("=== COMBINED ===\n"));

    ICodeGen* codegen = f->super.module->codegen;
    for (Pair* p = pairs->first; p; p = p->next) {
        if (visited[p->id]) { continue; }
        visited[p->id] = true;

        dyn_array_clear(combined);
        dyn_array_put(combined, p->lhs);
        dyn_array_put(combined, p->rhs);

        Pair* curr = p;
        while (pairs->l_map[curr->rhs->gvn]) {
            curr = pairs->l_map[curr->rhs->gvn];
            visited[curr->id] = true;
            dyn_array_put(combined, curr->rhs);
        }

        TB_DataType elem_dt = slp_node_data_type(p->lhs);
        int pack_limit = codegen->max_pack_width_for_op(f, elem_dt, p->lhs);
        if (pack_limit <= 1) {
            // operation cannot vectorize at all, filter out this pack
            continue;
        }

        VectorOp* op = tb_arena_alloc(&f->tmp_arena, sizeof(VectorOp) + pack_limit*sizeof(TB_Node*));
        op->v_op = NULL;
        op->width = 0;
        dyn_array_put(schedule, op);

        TB_OPTDEBUG(SLP)(printf("  PACK_LIMIT=%d\n", pack_limit));
        dyn_array_for(i, combined) {
            if (op->width >= pack_limit) {
                op = tb_arena_alloc(&f->tmp_arena, sizeof(VectorOp) + pack_limit*sizeof(TB_Node*));
                op->v_op = NULL;
                op->width = 0;
                dyn_array_put(schedule, op);

                TB_OPTDEBUG(SLP)(printf("  SPLIT\n"));
            }

            TB_Node* n = combined[i];
            op->ops[op->width++] = n;
            nl_table_put(&ops, n, op);

            #if TB_OPTDEBUG_SLP
            printf("  ");
            tb_print_dumb_node(NULL, n);
            printf("\n");
            #endif
        }
        TB_OPTDEBUG(SLP)(printf("\n"));
    }

    size_t i = dyn_array_length(schedule);
    while (i--) {
        VectorOp* op = schedule[i];
        TB_Node* first = op->ops[0];

        // classify the vector width and see if it's good
        bool bad = false;
        TB_DataType elem_dt = slp_node_data_type(first);
        if (!TB_IS_INTEGER_TYPE(elem_dt) && !TB_IS_FLOAT_TYPE(elem_dt)) {
            TB_OPTDEBUG(SLP)(printf("    SLP failed: can't vectorize this type. %%%u ", first->gvn), print_type(&OUT_STREAM_DEFAULT, elem_dt), printf("\n"));
            bad = true;
            goto done;
        }

        if (!codegen->is_pack_op_supported(f, elem_dt, op->ops[0], op->width)) {
            TB_OPTDEBUG(SLP)(printf("    SLP failed: can't vectorize this operation. %%%u [", first->gvn), print_type(&OUT_STREAM_DEFAULT, elem_dt), printf(" x %d]\n", op->width));
            bad = true;
            goto done;
        }

        int elem_bits = tb_data_type_bit_size(f->super.module, elem_dt.type);
        int vector_bits = elem_bits * op->width;
        TB_DataType v_dt = TB_TYPE_VOID;
        switch (vector_bits) {
            case 64:  v_dt = (TB_DataType){ { TB_TAG_V64,  .elem_or_addrspace = elem_dt.type } }; break;
            case 128: v_dt = (TB_DataType){ { TB_TAG_V128, .elem_or_addrspace = elem_dt.type } }; break;
            case 256: v_dt = (TB_DataType){ { TB_TAG_V256, .elem_or_addrspace = elem_dt.type } }; break;
            case 512: v_dt = (TB_DataType){ { TB_TAG_V512, .elem_or_addrspace = elem_dt.type } }; break;
            default: tb_todo();
        }
        op->v_dt = v_dt;

        // check if the vector ops can legally be connected up
        if (first->type == TB_STORE) {
            if (!viable_vector(f, &ops, v_dt, op, 3)) {
                bad = true;
            }
        } else {
            if (first->type != TB_LOAD) {
                if (!viable_vector(f, &ops, v_dt, op, 1)) {
                    bad = true;
                }

                if (!viable_vector(f, &ops, v_dt, op, 2)) {
                    bad = true;
                }
            }

            // all uses must also point to a pack... for now
            FOR_N(i, 0, op->width) {
                FOR_USERS(u, op->ops[i]) {
                    VectorOp* use_op = nl_table_get(&ops, USERN(u));
                    if (use_op == NULL) {
                        bad = true;
                        goto done;
                    }
                }
            }
        }

        // prune out
        done:;
        if (bad) {
            FOR_N(i, 0, op->width) {
                nl_table_remove(&ops, op->ops[i]);
            }

            size_t len = dyn_array_length(schedule);
            if (len - 1 != i) {
                i++;
                memmove(&schedule[i - 1], &schedule[i], (len - i) * sizeof(VectorOp*));
            }
            dyn_array_pop(schedule);
        }
    }

    // nothing left which is valid, bail out
    if (dyn_array_length(schedule) == 0) {
        return false;
    }

    TB_OPTDEBUG(SLP)(printf("=== COMPILED ===\n"));
    FOR_REV_N(i, 0, dyn_array_length(schedule)) {
        VectorOp* op = schedule[i];
        TB_Node* first = op->ops[0];

        // convert these ops into a packed op
        TB_DataType v_dt = op->v_dt;
        if (first->type == TB_LOAD) {
            int align = TB_NODE_GET_EXTRA_T(first, TB_NodeMemAccess)->align;
            TB_Node* n = tb_alloc_node(f, TB_LOAD, v_dt, 3, sizeof(TB_NodeMemAccess));
            set_input(f, n, first->inputs[0], 0);
            set_input(f, n, first->inputs[1], 1);
            set_input(f, n, first->inputs[2], 2);
            TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);

            op->v_op = tb_opt_gvn_node(f, n);
        } else if (first->type == TB_STORE) {
            int align = TB_NODE_GET_EXTRA_T(first, TB_NodeMemAccess)->align;
            TB_Node* src = gimme_vector(f, &ops, op, 3);

            TB_Node* n = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
            set_input(f, n, first->inputs[0], 0);
            set_input(f, n, first->inputs[1], 1);
            set_input(f, n, first->inputs[2], 2);
            set_input(f, n, src, 3);
            TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);

            op->v_op = n;

            TB_Node* last = op->ops[op->width - 1];
            subsume_node2(f, last, n);
        } else if (first->type == TB_FADD || first->type == TB_FMUL) {
            TB_Node* lhs = gimme_vector(f, &ops, op, 1);
            TB_Node* rhs = gimme_vector(f, &ops, op, 2);

            TB_Node* n = tb_alloc_node(f, first->type, v_dt, 3, 0);
            set_input(f, n, lhs, 1);
            set_input(f, n, rhs, 2);
            op->v_op = tb_opt_gvn_node(f, n);
        } else {
            __debugbreak();
        }

        mark_node(f, op->v_op);

        #if TB_OPTDEBUG_SLP
        FOR_N(j, 0, op->width) {
            tb_print_dumb_node(NULL, op->ops[j]);
            printf("\n");
        }

        printf("=> ");
        tb_print_dumb_node(NULL, op->v_op);
        printf("\n\n");
        #endif
    }

    // vaporization time, replace any effectful nodes tho
    FOR_REV_N(i, 0, dyn_array_length(schedule)) {
        VectorOp* op = schedule[i];

        // most ops can just have all their nodes killed
        FOR_N(j, 0, op->width) {
            tb_kill_node(f, op->ops[j]);
        }
    }

    tb_arena_free(&f->tmp_arena, visited, pairs->count * sizeof(bool));
    return true;
}

bool slp_transform(TB_Function* f, LoopOpt* ctx, TB_Worklist* ws, TB_LoopTree* loop) {
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    PairSet pairs = { 0 };
    pairs.l_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    pairs.r_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    pairs.depth = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));

    CUIK_TIMED_BLOCK("init") {
        FOR_N(i, 0, f->node_count) {
            pairs.l_map[i] = NULL;
            pairs.r_map[i] = NULL;
            pairs.depth[i] = -1;
        }
    }

    bool progress = false;
    if (generate_pack(f, &pairs, ws, ctx, loop)) {
        cuikperf_region_start("SLP xform", NULL);
        progress = compile_packs(f, &pairs, loop);
        cuikperf_region_end();
    }

    tb_arena_restore(&f->tmp_arena, sp);
    return progress;
}

