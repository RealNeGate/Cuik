
enum { MEM_REF_MAX_INDICES = 2 };

typedef struct {
    TB_Node* base;
    int64_t stride;

    int32_t offset;
    int32_t size;
} ArrayAccess;

typedef struct {
    TB_Node* mem;
    TB_Node* base;
    int32_t offset;
    int32_t size;

    int index_count;
    TB_Node* index[MEM_REF_MAX_INDICES];
    int32_t stride[MEM_REF_MAX_INDICES];
} MemRef;

static ArrayAccess compute_array_access(TB_Function* f, TB_Node* mem) {
    ArrayAccess r = { mem->inputs[2] };

    if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_ICONST) {
        int64_t offset = TB_NODE_GET_EXTRA_T(r.base->inputs[2], TB_NodeInt)->value;
        if (offset == (int32_t) offset) {
            r.base = r.base->inputs[1];
            r.offset = offset;
        }
    }

    if (r.base->type == TB_PHI && cfg_is_natural_loop(r.base->inputs[0])) {
        TB_Node* step = r.base->inputs[2];
        if (step->type == TB_PTR_OFFSET && step->inputs[2]->type == TB_ICONST) {
            r.base = r.base->inputs[1];
            r.stride = TB_NODE_GET_EXTRA_T(step->inputs[2], TB_NodeInt)->value;
        }
    } else if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_MUL && r.base->inputs[2]->inputs[2]->type == TB_ICONST) {
        TB_Node* step = r.base->inputs[2]->inputs[2];
        r.base = r.base->inputs[1];
        r.stride = TB_NODE_GET_EXTRA_T(step, TB_NodeInt)->value;
    } else if (r.base->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_SHL && r.base->inputs[2]->inputs[2]->type == TB_ICONST) {
        TB_Node* step = r.base->inputs[2]->inputs[2];
        r.base = r.base->inputs[1];
        r.stride = 1ull << TB_NODE_GET_EXTRA_T(step, TB_NodeInt)->value;
    }

    if (mem->type == TB_LOAD) {
        r.size = tb_data_type_byte_size(f->super.module, mem->dt.type);
    } else {
        r.size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
    }
    return r;
}

static void dump_array_access(ArrayAccess a) {
    printf("%%%u [X*%"PRId64" + %d, %d]\n", a.base->gvn, a.stride, a.offset, a.size);
}

static MemRef compute_mem_ref(TB_Function* f, TB_Node* mem) {
    MemRef r = { mem, mem->inputs[2] };

    if (r.base->type == TB_PTR_OFFSET && r.base->inputs[1]->type == TB_PTR_OFFSET && r.base->inputs[2]->type == TB_ICONST) {
        r.offset = TB_NODE_GET_EXTRA_T(r.base->inputs[2], TB_NodeInt)->value;
        r.base = r.base->inputs[1];
    }

    if (r.base->type == TB_PTR_OFFSET) {
        TB_Node* curr = r.base->inputs[2];
        r.base = r.base->inputs[1];

        if (curr->type == TB_ICONST) {
            r.offset = TB_NODE_GET_EXTRA_T(curr, TB_NodeInt)->value;
        } else {
            TB_Node* index = NULL;
            uint64_t stride = 1;
            for (;;) {
                if (curr->type == TB_ADD && curr->inputs[2]->type == TB_ICONST) {
                    int64_t disp = TB_NODE_GET_EXTRA_T(curr->inputs[2], TB_NodeInt)->value;

                    curr = curr->inputs[1];
                    r.offset += disp * stride;
                }

                if (curr->type == TB_SHL && curr->inputs[2]->type == TB_ICONST) {
                    stride <<= TB_NODE_GET_EXTRA_T(curr->inputs[2], TB_NodeInt)->value;
                    curr = curr->inputs[1];
                }

                r.stride[r.index_count] = stride;
                r.index[r.index_count++] = curr;

                // combining indices (we limit the analysis to 2D because... works :p)
                if (r.index_count >= MEM_REF_MAX_INDICES || curr->type != TB_ADD) {
                    break;
                }

                r.index[r.index_count - 1] = curr->inputs[1];
                curr = curr->inputs[2];
            }

            // if there's two indices and one has a bigger stride, it should be placed first
            _Static_assert(MEM_REF_MAX_INDICES == 2, "this should be a sort if we do grow the max index count");
            if (r.index_count == MEM_REF_MAX_INDICES && (r.stride[0] < r.stride[1] || (r.stride[0] == r.stride[1] && r.index[0]->gvn > r.index[1]->gvn))) {
                SWAP(int32_t,  r.stride[0], r.stride[1]);
                SWAP(TB_Node*, r.index[0], r.index[1]);
            }
        }
    }

    if (mem->type == TB_LOAD) {
        r.size = tb_data_type_byte_size(f->super.module, mem->dt.type);
    } else {
        r.size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
    }
    return r;
}

typedef struct {
    TB_Node* base;
    int64_t offset;
} KnownPointer;

static KnownPointer known_pointer(TB_Node* n) {
    if (n->type == TB_PTR_OFFSET && n->inputs[2]->type == TB_ICONST) {
        return (KnownPointer){ n->inputs[1], TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value };
    } else {
        return (KnownPointer){ n, 0 };
    }
}

static Lattice* value_mem(TB_Function* f, TB_Node* n) {
    return &MEM_IN_THE_SKY;
}

static bool no_alias_with(TB_Module* m, KnownPointer us, int us_bytes, TB_Node* them, TB_DataType them_dt) {
    int them_bytes = tb_data_type_byte_size(m, them_dt.type);
    KnownPointer them_addr = known_pointer(them->inputs[2]);

    if (them_addr.base == us.base) {
        // if the stores don't intersect
        if (!(them_addr.offset < us.offset+us_bytes && us.offset < them_addr.offset+them_bytes)) {
            return true;
        }
    }

    return false;
}

static TB_Node* ideal_memcpy(TB_Function* f, TB_Node* n) {
    TB_Node* ctrl = n->inputs[0];
    TB_Node* mem = n->inputs[1];
    TB_Node* dst = n->inputs[2];
    TB_Node* src = n->inputs[3];

    // convert small memcpys into scalar stores (+ bitcasts), this
    // allows mem2reg to fold away the memory
    int align = TB_NODE_GET_EXTRA_T(n, TB_NodeMemAccess)->align;
    if (is_iconst(f, n->inputs[4])) {
        int64_t size = latuni_get(f, n->inputs[4])->_int.min;

        TB_DataType dt;
        switch (size) {
            case 1: dt = TB_TYPE_I8;  break;
            case 2: dt = TB_TYPE_I16; break;
            case 4: dt = TB_TYPE_I32; break;
            case 8: dt = TB_TYPE_I64; break;
            default: return NULL;
        }

        TB_Node* ld = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeMemAccess));
        set_input(f, ld, ctrl, 0);
        set_input(f, ld, mem,  1);
        set_input(f, ld, src,  2);
        TB_NODE_SET_EXTRA(ld, TB_NodeMemAccess, .align = align);

        TB_Node* st = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
        set_input(f, st, ctrl, 0);
        set_input(f, st, mem,  1);
        set_input(f, st, dst,  2);
        set_input(f, st, ld,   3);
        TB_NODE_SET_EXTRA(st, TB_NodeMemAccess, .align = align);
        return st;
    }

    return NULL;
}

static TB_Node* node_lookup(TB_Function* f, TB_Node* n) {
    return nl_hashset_get2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
}

static TB_Node* partially_redundant(TB_Function* f, TB_Node* n, TB_Node* phi, int phi_i, MemRef addr) {
    int64_t offset = addr.offset;
    TB_Node* path = phi->inputs[phi_i];

    // reverse the operation
    uint64_t amt;
    if (path->type == TB_ADD && get_int_const(path->inputs[2], &amt)) {
        offset += amt;
    }

    TB_DataType ptr_int_dt = TB_TYPE_I64;
    TB_Node* ptr = addr.mem->inputs[2];
    if (ptr->type == TB_PTR_OFFSET) {
        if (offset == 0) {
            ptr = ptr->inputs[1];
        } else {
            TB_Node* new_offset = make_int_node(f, ptr_int_dt, offset);
            TB_Node* add = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
            set_input(f, add, ptr->inputs[1], 1);
            set_input(f, add, new_offset,     2);

            TB_Node* k = node_lookup(f, add);
            if (k == NULL) {
                tb_kill_node(f, add);
                return NULL;
            }
            ptr = k;
        }
    }

    enum {
        MAX_STEPS = 5,
    };

    // any store users that are known to happen before the phi path?
    FOR_USERS(u, ptr) {
        TB_Node* st = USERN(u);
        if (st->type == TB_STORE && USERI(u) == 2) {
            TB_Node* mem = n->inputs[1];
            int i = 0;
            while (i++ < MAX_STEPS && mem != st) {
                // non-aliasing operations can be walked past
                if (IS_PROJ(mem)) {
                    mem = mem->inputs[1];
                }

                // we can walk past the region that got us here
                if (mem->type == TB_PHI && mem->inputs[0] == phi->inputs[0]) {
                    mem = mem->inputs[phi_i];
                } else if (mem->type != TB_DEBUG_LOCATION) {
                    // we assume anything that isn't a debug info annotation aliases us... for now
                    break;
                }
            }

            return mem == st ? mem->inputs[3] : NULL;
        }
    }

    return NULL;
}

static TB_Node* ideal_load(TB_Function* f, TB_Node* n) {
    TB_Node* ctrl = n->inputs[0];
    TB_Node* mem = n->inputs[1];
    TB_Node* addr = n->inputs[2];

    TB_NodeMemAccess* a = TB_NODE_GET_EXTRA(n);
    if (a->is_volatile) {
        return NULL;
    }

    if (ctrl != NULL) {
        // we've dependent on code which must always be run (ROOT.mem)
        if (n->inputs[0]->type == TB_PROJ && n->inputs[0]->inputs[0]->type == TB_ROOT) {
            set_input(f, n, NULL, 0);
            return n;
        } else {
            TB_Node* base = addr;
            while (base->type == TB_PTR_OFFSET) {
                base = base->inputs[1];
            }

            // loads based on LOCALs don't need control-dependence, it's actually kinda annoying
            if (base->type == TB_LOCAL) {
                set_input(f, n, NULL, 0);
                return n;
            }
        }

        // if all paths are dominated by a load of some address then it's safe
        // to relax ctrl deps.
        int bits_read = tb_data_type_bit_size(f->super.module, n->dt.type);

        // find other users of the address which read the same size (or more)
        FOR_USERS(u, addr) {
            TB_NodeTypeEnum type = 0;
            if (USERN(u) != n && USERI(u) == 2 && USERN(u)->type == TB_LOAD) {
                TB_DataType mem_dt = n->type == TB_LOAD ? n->dt : n->inputs[3]->dt;
                int other_bits_read = tb_data_type_bit_size(f->super.module, mem_dt.type);
                if (bits_read <= other_bits_read) {
                    TB_Node* other_ctrl = USERN(u)->inputs[0];
                    if (other_ctrl == NULL || (fast_dommy(other_ctrl, ctrl) && other_ctrl != ctrl)) {
                        set_input(f, n, other_ctrl, 0);
                        return n;
                    }
                }
            }
        }
    }

    // if we're dependent on memory we don't alias with, hoist the dep
    if (mem->type == TB_STORE) {
        int curr_bytes = tb_data_type_byte_size(f->super.module, n->dt.type);
        KnownPointer curr_ptr = known_pointer(addr);

        /* if (no_alias_with(f->super.module, curr_ptr, curr_bytes, mem, mem->inputs[3]->dt)) {
            set_input(f, n, mem->inputs[1], 1);

            // mark potential anti-deps
            mark_node_n_users(f, mem);
            return n;
        } */

        ArrayAccess A = compute_array_access(f, n);
        ArrayAccess B = compute_array_access(f, n->inputs[1]);

        if (A.base == B.base && A.stride == B.stride) {
            int64_t A_limit = A.offset+A.size;
            int64_t B_limit = B.offset+B.size;
            if (
                A.offset > 0 && A_limit <= A.stride &&
                B.offset > 0 && B_limit <= B.stride
            ) {
                if (!(B.offset < A_limit && A.offset < B_limit)) {
                    // dump_array_access(A);
                    // dump_array_access(B);

                    set_input(f, n, mem->inputs[1], 1);
                    // mark potential anti-deps
                    mark_node_n_users(f, mem);
                    return n;
                }
            }
        }
    }

    // we could push the load further up if we know
    // it's safe to access and partially redundant:
    // * address is a phi-dependent (even indirectly to a degree)
    if (0) {
        MemRef addr = compute_mem_ref(f, n);
        TB_Node* significant = addr.index_count ? addr.index[addr.index_count - 1] : addr.base;
        if (significant->type == TB_PHI) {
            tb_print(f);
            __debugbreak();

            TB_Node** paths = tb_arena_alloc(&f->tmp_arena, (significant->input_count - 1) * sizeof(TB_Node*));

            int redundant = 0;
            FOR_N(i, 1, significant->input_count) {
                TB_Node* k = partially_redundant(f, n, significant, i, addr);
                if (k != NULL) {
                    paths[i-1] = k;
                    redundant++;
                } else {
                    paths[i-1] = NULL;
                }
            }

            // if at least half of the paths compute the relevant value, we might
            // as well compute it for all paths
            if (redundant >= (significant->input_count/2)) {
                __debugbreak();
            }
            tb_arena_free(&f->tmp_arena, paths, (significant->input_count - 1) * sizeof(TB_Node*));
        }
    }

    return NULL;
}

static TB_Node* ideal_store(TB_Function* f, TB_Node* n) {
    TB_Node *mem = n->inputs[1], *addr = n->inputs[2], *val = n->inputs[3];
    TB_DataType dt = val->dt;

    #if 0
    // combining adjacent stores
    TB_Node* b = val;
    KnownPointer b_ptr = known_pointer(addr);
    if (mem->type == TB_STORE && mem->inputs[0] == n->inputs[0] && mem->user_count == 1) {
        TB_Node* a = mem->inputs[3];
        KnownPointer a_ptr = known_pointer(mem->inputs[2]);
        if (a_ptr.base == b_ptr.base) {
            if (a_ptr.offset > b_ptr.offset) {
                SWAP(TB_Node*, a, b);
                SWAP(KnownPointer, a_ptr, b_ptr);
            }

            int a_bytes = tb_data_type_byte_size(f->super.module, a->dt.type);
            int b_bytes = tb_data_type_byte_size(f->super.module, b->dt.type);
            int bytes = a_bytes + b_bytes;
            if (TB_IS_INTEGER_TYPE(a->dt) && TB_IS_INTEGER_TYPE(b->dt) && a_ptr.offset + a_bytes == b_ptr.offset) {
                bool valid_size = bytes == 2 || bytes == 4 || bytes == 8;
                __debugbreak();
            }
        }
    }
    #endif

    #if 0
    // store is next to a non-aliasing adjacent store (or merge to non-adjacent stores)
    int curr_bytes = tb_data_type_byte_size(f->super.module, val->dt.type);
    KnownPointer curr_ptr = known_pointer(addr);

    // split independent memory effects
    int split_count = 1;
    TB_Node* start_mem = mem;

    while (start_mem->type == TB_STORE
        && start_mem->inputs[0] == n->inputs[0]
        && start_mem->user_count == 1
        && no_alias_with(f->super.module, curr_ptr, curr_bytes, start_mem, start_mem->inputs[3]->dt))
    {
        start_mem = start_mem->inputs[1];
        split_count += 1;
    }

    if (split_count > 1) {
        TB_Node* split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_MEMORY, 2, 0);
        set_input(f, split, mem->inputs[1], 1);

        // merge node
        TB_Node* merge = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 2 + split_count, 0);
        set_input(f, merge, split, 1);

        TB_Node* curr = n;
        FOR_N(i, 0, split_count) {
            TB_Node* next = curr->inputs[1];
            set_input(f, curr,  split, 1);
            set_input(f, merge, curr, 2 + i);
            curr = next;
        }

        mark_node_n_users(f, split);
        return merge;
    }
    #endif

    // if a store has only one user in this chain it means it's only job was
    // to facilitate the creation of that user store... if we can detect that
    // user store is itself dead, everything in the middle is too.
    if (mem->type == TB_STORE && single_use(mem) && mem->inputs[2] == addr && mem->inputs[3]->dt.raw == dt.raw) {
        // choose the bigger alignment (we wanna keep this sort of info)
        TB_NodeMemAccess* a = TB_NODE_GET_EXTRA(mem);
        TB_NodeMemAccess* b = TB_NODE_GET_EXTRA(n);
        if (a->align > b->align) b->align = a->align;

        // make sure to kill the stores to avoid problems
        TB_Node* parent = mem->inputs[1];
        tb_kill_node(f, mem);

        set_input(f, n, parent, 1);
        return n;
    }

    return NULL;
}

////////////////////////////////
// Pointer math
////////////////////////////////
// There's literally one pointer math op, it's easier that way
static TB_Node* ideal_ptr_offset(TB_Function* f, TB_Node* n) {
    TB_Node* base   = n->inputs[1];
    TB_Node* offset = n->inputs[2];

    // reassociate:
    //   off(a, b + CON) => off(off(a, b), CON)
    if (offset->type == TB_ADD && offset->inputs[2]->type == TB_ICONST) {
        TB_Node* lhs = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
        set_input(f, lhs, base,              1);
        set_input(f, lhs, offset->inputs[1], 2);
        mark_node(f, lhs);

        // give it a good type to avoid monotonicity problems
        Lattice* lhs_type = value_of(f, lhs);
        latuni_set(f, lhs, lhs_type);

        set_input(f, n, lhs, 1);
        set_input(f, n, offset->inputs[2], 2);
        return n;
    }

    // reassociate:
    //   off(off(a, b), c) => off(a, b + c)
    if (base->type == TB_PTR_OFFSET) {
        if (offset->type != TB_ICONST) {
            TB_Node* rhs = tb_alloc_node(f, TB_ADD, offset->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, rhs, base->inputs[2], 1);
            set_input(f, rhs, offset,          2);
            mark_node(f, rhs);

            // give it a good type to avoid monotonicity problems
            Lattice* rhs_type = value_of(f, rhs);
            latuni_set(f, rhs, rhs_type);

            set_input(f, n, base->inputs[1], 1);
            set_input(f, n, rhs,             2);
            return n;
        } else if (base->inputs[2]->type == TB_ICONST) {
            Lattice* a = latuni_get(f, offset);
            Lattice* b = latuni_get(f, base->inputs[2]);
            TB_ASSERT(lattice_is_iconst(a) && lattice_is_iconst(b));

            TB_Node* con = make_int_node(f, offset->dt, a->_int.min + b->_int.min);
            set_input(f, n, base->inputs[1], 1);
            set_input(f, n, con,             2);
            return n;
        }
    }

    return NULL;
}

static TB_Node* identity_dead_store(TB_Function* f, TB_Node* n) {
    return n->inputs[1];
}

static TB_Node* identity_ptr_offset(TB_Function* f, TB_Node* n) {
    Lattice* off = latuni_get(f, n->inputs[2]);
    if (lattice_int_eq(off, 0)) {
        return n->inputs[1];
    }

    return n;
}

static TB_Node* ideal_split_mem(TB_Function* f, TB_Node* n) {
    int merge_count = 0;
    TB_Node* merge = NULL;
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (un->type == TB_MERGEMEM && USERI(u) == 1) {
            if (un->input_count == 3) { merge = un; }
            merge_count++;
        }
    }

    if (merge_count == 1 && merge) {
        // merge => only real memory path
        subsume_node(f, merge, merge->inputs[2]);
        // split => previous mem
        TB_Node* prev = n->inputs[1];
        tb_kill_node(f, merge);
        return prev;
    }

    return NULL;
}

static TB_Node* ideal_merge_mem(TB_Function* f, TB_Node* n) {
    bool progress = false;
    TB_Node* split_node = n->inputs[1];

    // remove useless split edges
    #if 0
    size_t i = 2;
    while (i < n->input_count) {
        if (n->inputs[i] == split_node) {
            progress = true;
            mark_node_n_users(f, split_node);

            // simple remove-swap
            set_input(f, n, n->inputs[n->input_count - 1], i);
            set_input(f, n, NULL, n->input_count - 1);
            n->input_count -= 1;
        } else {
            i += 1;
        }
    }

    skip:
    return progress ? n : NULL;
    #endif

    return NULL;
}

