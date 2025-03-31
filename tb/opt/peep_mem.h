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

        if (no_alias_with(f->super.module, curr_ptr, curr_bytes, mem, mem->inputs[3]->dt)) {
            set_input(f, n, mem->inputs[1], 1);

            // mark potential anti-deps
            mark_node_n_users(f, mem);
            return n;
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
    //   off(off(a, b), c) => off(a, b + c)
    if (base->type == TB_PTR_OFFSET) {
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
    }

    return NULL;
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

