
static bool is_cool(uint64_t x) { return x == 1 || x == 2 || x == 4 || x == 8; }
static TB_Node* ideal_memset(TB_Function* f, TB_Node* n) {
    // convert small memsets into stores
    uint64_t count, val;
    if (get_int_const(n->inputs[4], &count) && get_int_const(n->inputs[3], &val) && is_cool(count)) {
        // fill rest of the bytes
        FOR_N(i, 1, count) {
            val |= (val & 0xFF) << (i*8);
        }

        TB_DataType dt = TB_TYPE_INTN(count*8);
        set_input(f, n, make_int_node(f, dt, val), 3);
        set_input(f, n, NULL, 4);
        n->input_count = 4;
        n->type = TB_STORE;
        return n;
    }

    return NULL;
}

static TB_Node* ideal_memcpy(TB_Function* f, TB_Node* n) {
    // convert small memsets into ld+st pairs
    uint64_t count, val;
    if (get_int_const(n->inputs[4], &count) && is_cool(count)) {
        TB_Node* ctrl = n->inputs[0];
        TB_Node* mem  = n->inputs[1];
        TB_Node* src  = n->inputs[3];

        TB_DataType dt = TB_TYPE_INTN(count*8);
        TB_Node* ld = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, ld, ctrl, 0);
        set_input(f, ld, mem, 1);
        set_input(f, ld, src, 2);
        mark_node(f, ld);

        // convert to store, remove extra input
        n->type = TB_STORE;
        set_input(f, n, ld, 3);
        set_input(f, n, NULL, 4);
        n->input_count = 4;
        return n;
    }

    return NULL;
}

static Lattice* value_merge_mem(TB_Function* f, TB_Node* n) {
    return &ALLMEM_IN_THE_SKY;
}

static Lattice* value_mem(TB_Function* f, TB_Node* n) {
    // just inherit memory from parent
    return latuni_get(f, n->inputs[1]);
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

