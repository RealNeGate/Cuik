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
    return a != b;
}

static bool can_pack(PairSet* pairs, TB_Node* a, TB_Node* b) {
    if (pairs->l_map[a->gvn] || pairs->r_map[b->gvn]) {
        return false;
    }

    if (is_valid_vector_component(slp_node_data_type(a)) &&
        is_valid_vector_component(slp_node_data_type(b)) &&
        isomorphic(a, b) &&
        independent(a, b)) {
        // TODO(NeGate): check for independence
        return true;
    }

    return false;
}

static int ref_cmp(const void* a, const void* b) {
    const MemRef* aa = a;
    const MemRef* bb = b;

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

void generate_pack(TB_Function* f, PairSet* pairs, TB_Node* n) {
    ArenaArray(MemRef) refs = aarray_create(&f->tmp_arena, MemRef, 8);

    pairs->l_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    pairs->r_map = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(Pair*));
    FOR_N(i, 0, f->node_count) {
        pairs->l_map[i] = NULL;
        pairs->r_map[i] = NULL;
    }

    // find chains of stores
    TB_Node* mem = n;
    while (mem->type == TB_STORE) {
        MemRef r = { mem, mem->inputs[2] };
        if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_ICONST) {
            r.offset = TB_NODE_GET_EXTRA_T(r.base->inputs[2], TB_NodeInt)->value;
            r.base   = r.base->inputs[1];
        }

        r.size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
        aarray_push(refs, r);

        printf("AAA! ");
        tb_print_dumb_node(NULL, mem);
        printf(" (%%%u + %d, size=%d)\n", r.base->gvn, r.offset, r.size);

        mem = mem->inputs[1];
    }

    // sort for faster queries later on
    qsort(refs, aarray_length(refs), sizeof(MemRef), ref_cmp);

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
            TB_DataType elem_dt = refs[start].mem->inputs[3]->dt;

            // find adjacent stores
            FOR_N(i, start, end) {
                MemRef* a = &refs[i];
                FOR_N(j, start+1, end) {
                    MemRef* b = &refs[j];

                    // there's a weird overlapping memory op
                    if (a->offset + size < b->offset) { break; }
                    if (a->offset + size == b->offset && can_pack(pairs, a->mem, b->mem)) {
                        slp_add_pair(f, pairs, a->mem, b->mem);
                    }
                }
            }

            // extend packs based on use-def
            bool progress;
            do {
                progress = false;

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
                        if (can_pack(pairs, a, b)) {
                            slp_add_pair(f, pairs, a, b);
                            progress = true;
                        }
                    }
                }
            } while (progress);

            bool* visited = tb_arena_alloc(&f->tmp_arena, pairs->count * sizeof(bool));
            FOR_N(i, 0, pairs->count) {
                visited[i] = false;
            }

            // combine packs
            #if TB_OPTDEBUG_SLP
            printf("=== COMBINED ===\n");
            for (Pair* p = pairs->first; p; p = p->next) {
                if (visited[p->id]) { continue; }
                visited[p->id] = true;

                printf("  ");
                tb_print_dumb_node(NULL, p->lhs);
                printf("\n");

                printf("  ");
                tb_print_dumb_node(NULL, p->rhs);
                printf("\n");

                // if one pack ends where the other starts, we join them
                Pair* curr = p;
                while (pairs->l_map[curr->rhs->gvn]) {
                    curr = pairs->l_map[curr->rhs->gvn];
                    visited[curr->id] = true;

                    printf("  ");
                    tb_print_dumb_node(NULL, curr->rhs);
                    printf("\n");
                }

                printf("\n\n");
            }
            printf("\n");
            #endif

            visited = tb_arena_alloc(&f->tmp_arena, pairs->count * sizeof(bool));
        }

        start = end;
    }

    __debugbreak();
}

bool tb_opt_vectorize(TB_Function* f) {
    #if 0
    tb_print_dumb(f);

    // TODO(NeGate): search for reductions & stores throughout
    // the entire graph, for now we're only searching the exit
    // paths
    PairSet pairs = { 0 };
    FOR_N(i, 1, f->root_node->input_count) {
        TB_Node* exit = f->root_node->inputs[i];
        if (exit->type == TB_RETURN) {
            generate_pack(f, &pairs, exit->inputs[1]);
        }
    }

    __debugbreak();
    tb_arena_clear(&f->tmp_arena);
    #endif

    return false;
}
