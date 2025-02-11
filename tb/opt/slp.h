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
} PairSet;

typedef struct {
    TB_Node* mem;
    TB_Node* base;
    int32_t offset;
    int32_t size;
} MemRef;

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

static bool independent(TB_Node* a, TB_Node* b) {
    // TODO(NeGate): check for real independence
    FOR_N(i, a->type == TB_STORE ? 3 : 1, a->input_count) {
        if (a->inputs[i] == b) {
            return false;
        }
    }

    FOR_N(i, b->type == TB_STORE ? 3 : 1, b->input_count) {
        if (b->inputs[i] == a) {
            return false;
        }
    }

    return a != b;
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

static MemRef compute_mem_ref(TB_Function* f, TB_Node* mem) {
    MemRef r = { mem, mem->inputs[2] };
    if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_ICONST) {
        r.offset = TB_NODE_GET_EXTRA_T(r.base->inputs[2], TB_NodeInt)->value;
        r.base   = r.base->inputs[1];
    }

    if (mem->type == TB_LOAD) {
        r.size = tb_data_type_byte_size(f->super.module, mem->dt.type);
    } else {
        r.size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
    }
    return r;
}

static bool can_pack(TB_Function* f, PairSet* pairs, TB_Node* a, TB_Node* b) {
    if (pairs->l_map[a->gvn] || pairs->r_map[b->gvn]) {
        return false;
    }

    if (is_valid_vector_component(slp_node_data_type(a)) &&
        is_valid_vector_component(slp_node_data_type(b)) &&
        isomorphic(a, b) &&
        independent(a, b) &&
        a->inputs[0] == b->inputs[0])
    {
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
    if (aa->size != bb->size) { return aa->size - bb->size; }
    if (aa->offset != bb->offset) { return aa->offset - bb->offset; }

    return 0;
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

bool generate_pack(TB_Function* f, PairSet* pairs) {
    ArenaArray(MemRef) refs = aarray_create(&f->tmp_arena, MemRef, 8);

    pairs->l_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    pairs->r_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    FOR_N(i, 0, f->node_count) {
        pairs->l_map[i] = NULL;
        pairs->r_map[i] = NULL;
    }

    TB_Node* root_mem = USERN(proj_with_index(f->root_node, 1));
    TB_ASSERT(root_mem->dt.type == TB_TAG_MEMORY);
    worklist_push(f->worklist, root_mem);

    for (size_t i = 0; i < dyn_array_length(f->worklist->items); i++) {
        TB_Node* mem = f->worklist->items[i];

        if (mem->type == TB_LOAD || mem->type == TB_STORE) {
            aarray_push(refs, compute_mem_ref(f, mem));
        }

        if (mem->dt.type == TB_TAG_MEMORY) {
            FOR_USERS(u, mem) {
                if (cfg_is_mproj(USERN(u))) {
                    mem = USERN(u);
                    break;
                }
            }
        }

        // walk all the memory users of the memory node
        if (mem->dt.type == TB_TAG_MEMORY) {
            FOR_USERS(u, mem) {
                if ((USERN(u)->type == TB_PHI && USERN(u)->dt.type == TB_TAG_MEMORY) ||
                    (USERI(u) == 1 && cfg_flags(USERN(u)) & NODE_MEMORY_IN)) {
                    worklist_push(f->worklist, USERN(u));
                }
            }
        }
    }
    worklist_clear(f->worklist);

    // sort for faster queries later on
    qsort(refs, aarray_length(refs), sizeof(MemRef), ref_cmp);

    aarray_for(i, refs) {
        MemRef r = refs[i];

        #if TB_OPTDEBUG_SLP
        printf("AAA! ");
        tb_print_dumb_node(NULL, r.mem);
        printf(" (%%%u + %d, size=%d)\n", r.base->gvn, r.offset, r.size);
        #endif
    }

    // scan for groups with the same base & element size
    int start = 0;
    while (start < aarray_length(refs)) {
        int end = start + 1;
        while (refs[end].base == refs[start].base
            && refs[end].size == refs[start].size) {
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
                    if (a->offset + size == b->offset && can_pack(f, pairs, a->mem, b->mem)) {
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
                    printf("=== PAIRS ===\n");
                    for (Pair* p = pairs->first; p; p = p->next) {
                        printf("\x1b[96m%%%-4u --   %%%-4u    %s\x1b[0m\n  ", p->lhs->gvn, p->rhs->gvn, tb_node_get_name(p->lhs->type));
                        tb_print_dumb_node(NULL, p->lhs);
                        printf("\n  ");
                        tb_print_dumb_node(NULL, p->rhs);
                        printf("\n\n");
                    }
                    printf("\n\n\n");
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
                            if (can_pack(f, pairs, a, b)) {
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

        TB_Node* n = tb_alloc_node(f, TB_VBROADCAST, v_dt, 2, 0);
        set_input(f, n, leader, 1);
        return n;
    }

    bool weird = false;
    FOR_N(i, 1, op->width) {
        TB_Node* src = op->ops[i]->inputs[index];
        VectorOp* src_op = nl_table_get(ops, src);
        // we can mix vectors until the same pool but we
        // can't have a scalar mixed with vectors.
        if (src_op == NULL) {
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
        return tb_opt_gvn_node(f, n);
    }

    // as long as all the ops come from the same leader, it's all good
    int* indices = tb_arena_alloc(&f->tmp_arena, op->width * sizeof(int));
    indices[0] = find_vector_index(leader_op, leader);

    bool weird = false;
    FOR_N(i, 1, op->width) {
        TB_Node* src = op->ops[i]->inputs[index];
        VectorOp* src_op = nl_table_get(ops, src);
        if (src_op != leader_op) {
            // TB_ASSERT_MSG(0, "shoulda rejected");
            weird = true;
        }
        indices[i] = find_vector_index(src_op, src);
    }

    if (weird) {
        printf("weird? ");
        FOR_N(i, 0, op->width) {
            if (i) { printf(", "); }
            VectorOp* src_op = nl_table_get(ops, op->ops[i]->inputs[index]);
            printf("%p:%d", src_op, indices[i]);
        }
        printf("\n");

        // chain of shuffles
        TB_Node* last = leader_op->v_op;
        FOR_N(i, 1, op->width) {
            TB_Node* src = op->ops[i]->inputs[index];
            VectorOp* src_op = nl_table_get(ops, src);
            if (src_op != leader_op) {
                last = tb_opt_gvn_node(f, last);

                TB_Node* n = tb_alloc_node(f, TB_VSHUFFLE, op->v_dt, 3, sizeof(TB_NodeVShuffle) + op->width*sizeof(int));
                set_input(f, n, last,         1);
                set_input(f, n, src_op->v_op, 2);

                TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
                shuf->width = op->width;
                FOR_N(j, 0, op->width) {
                    shuf->indices[j] = j;
                }
                last = n;
            }

            // overwrite the old entries
            TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(last);
            shuf->indices[i] = op->width + indices[i];
        }

        tb_arena_free(&f->tmp_arena, indices, op->width * sizeof(int));
        return tb_opt_gvn_node(f, last);
    }

    // sometimes we don't need shuffles
    bool ordered = true;
    FOR_N(i, 0, op->width) {
        if (i != indices[i]) { ordered = false; break; }
    }

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

    return tb_opt_gvn_node(f, n);
}

bool compile_packs(TB_Function* f, PairSet* pairs) {
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

            TB_OPTDEBUG(SLP)(printf("  "), tb_print_dumb_node(NULL, n));
            if (n->type == TB_LOAD || n->type == TB_STORE) {
                MemRef r = compute_mem_ref(f, n);
                TB_OPTDEBUG(SLP)(printf(" (%%%u + %d, size=%d)", r.base->gvn, r.offset, r.size));
            }
            TB_OPTDEBUG(SLP)(printf("\n"));
        }
        TB_OPTDEBUG(SLP)(printf("\n"));
    }

    FOR_REV_N(i, 0, dyn_array_length(schedule)) {
        VectorOp* op = schedule[i];
        TB_Node* first = op->ops[0];

        // classify the vector width and see if it's good
        bool bad = false;
        TB_DataType elem_dt = slp_node_data_type(first);
        if (!TB_IS_INTEGER_TYPE(elem_dt) && !TB_IS_FLOAT_TYPE(elem_dt)) {
            TB_OPTDEBUG(SLP)(printf("    SLP failed: can't vectorize this type. "), print_type(elem_dt), printf("\n"));
            bad = true;
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
        } else if (first->type != TB_LOAD) {
            if (!viable_vector(f, &ops, v_dt, op, 1)) {
                bad = true;
            }

            if (!viable_vector(f, &ops, v_dt, op, 2)) {
                bad = true;
            }
        }

        // prune out
        if (bad) {
            // nl_table_put(&ops, n, op);

            __debugbreak();
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

bool tb_opt_vectorize(TB_Function* f) {
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));
    bool progress;
    CUIK_TIMED_BLOCK("SLP") {
        PairSet pairs = { 0 };
        progress = generate_pack(f, &pairs);
        if (progress && !compile_packs(f, &pairs)) {
            // failed vibe check, no transformation performed
            progress = false;
        }

        tb_arena_clear(&f->tmp_arena);
    }
    return progress;
}
