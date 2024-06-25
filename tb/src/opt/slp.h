// Based on the classic
//
// Exploiting Superword Level Parallelism with Multimedia Instruction Sets, 2000
//   Samuel Larsen and Saman Amarasinghe
//
// only type of PackSet we need
typedef struct Pair {
    struct Pair* next;

    TB_Node* lhs;
    TB_Node* rhs;
} Pair;

typedef struct {
    Pair* first;
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

static bool can_pack(TB_Node* a, TB_Node* b) {
    if (is_valid_vector_component(a->inputs[3]->dt) &&
        is_valid_vector_component(b->inputs[3]->dt) &&
        isomorphic(a, b)) {
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

void generate_pack(TB_Function* f, PairSet* pairs, TB_Node* n) {
    if (n->type == TB_MERGEMEM) {
        // independent stores
        ArenaArray(MemRef) refs = aarray_create(&f->tmp_arena, MemRef, 8);
        FOR_N(i, 2, n->input_count) {
            TB_Node* mem = n->inputs[i];
            if (mem->type == TB_STORE) {
                MemRef r = { mem, mem->inputs[2] };
                if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_ICONST) {
                    r.offset = TB_NODE_GET_EXTRA_T(r.base->inputs[2], TB_NodeInt)->value;
                    r.base   = r.base->inputs[1];
                }

                r.size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
                aarray_push(refs, r);
            }
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
                int m = 1;
                FOR_N(i, start, end) {
                    MemRef* a = &refs[i];
                    FOR_N(j, start+1, end) {
                        MemRef* b = &refs[j];

                        // there's a weird overlapping memory op
                        if (a->offset + size < b->offset) { break; }
                        if (a->offset + size == b->offset && can_pack(a->mem, b->mem)) {
                            // add to pair
                            Pair* p = tb_arena_alloc(&f->tmp_arena, sizeof(Pair));
                            p->next = pairs->first;
                            p->lhs = a->mem;
                            p->rhs = b->mem;
                            pairs->first = p;
                            m++;
                        }
                    }
                }

                printf("=== VECTOR [");
                print_type(elem_dt);
                printf(" x %d] ===\n", m);

                for (Pair* p = pairs->first; p; p = p->next) {
                    printf("%%%u -- %%%u\n", p->lhs->gvn, p->rhs->gvn);
                }
                __debugbreak();
            }

            start = end;
        }

        __debugbreak();
    }
}

void tb_opt_vectorize(TB_Function* f) {
    /*tb_print_dumb(f);

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

    __debugbreak();*/
}
