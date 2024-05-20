
static bool get_int_const(TB_Node* n, uint64_t* imm) {
    if (n->type == TB_ICONST) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
        *imm = i->value;
        return true;
    } else {
        return false;
    }
}

////////////////////////////////
// Integer idealizations
////////////////////////////////
static TB_Node* ideal_bitcast(TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];

    if (n->dt.raw == src->dt.raw) {
        return src;
    } else if (src->type == TB_BITCAST) {
        set_input(f, n, src->inputs[1], 1);
        return n;
    }

    // single use load? ok replace the load
    if (src->type == TB_LOAD && src->user_count == 1) {
        src->dt = n->dt;
        latuni_set(f, src, NULL);
        return src;
    }

    // int -> smaller int means truncate
    if (src->dt.type == TB_TAG_INT && n->dt.type == TB_TAG_INT && src->dt.data > n->dt.data) {
        n->type = TB_TRUNCATE;
        return n;
    } else if (src->type == TB_ICONST) {
        return make_int_node(f, n->dt, TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value);
    }

    return NULL;
}

static bool inverted_cmp(TB_Node* n, TB_Node* n2) {
    switch (n->type) {
        case TB_CMP_EQ: return n2->type == TB_CMP_NE && n2->inputs[1] == n->inputs[1] && n2->inputs[2] == n->inputs[2];
        case TB_CMP_NE: return n2->type == TB_CMP_EQ && n2->inputs[1] == n->inputs[1] && n2->inputs[2] == n->inputs[2];
        // flipped inputs
        case TB_CMP_SLE: return n2->type == TB_CMP_SLT && n2->inputs[2] == n->inputs[1] && n2->inputs[1] == n->inputs[2];
        case TB_CMP_ULE: return n2->type == TB_CMP_ULT && n2->inputs[2] == n->inputs[1] && n2->inputs[1] == n->inputs[2];
        case TB_CMP_SLT: return n2->type == TB_CMP_SLE && n2->inputs[2] == n->inputs[1] && n2->inputs[1] == n->inputs[2];
        case TB_CMP_ULT: return n2->type == TB_CMP_ULE && n2->inputs[2] == n->inputs[1] && n2->inputs[1] == n->inputs[2];
        default: return false;
    }
}

static Lattice* value_sext(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }

    #if 1
    return a;
    #else
    if (a->_int.min == a->_int.max) { return a; }

    int64_t min    = a->_int.min;
    int64_t max    = a->_int.max;
    uint64_t zeros = a->_int.known_zeros;
    uint64_t ones  = a->_int.known_ones;
    int old_bits   = n->inputs[1]->dt.data;
    uint64_t mask  = tb__mask(n->dt.data) & ~tb__mask(old_bits);

    if (min >= 0 || (zeros >> (old_bits - 1))) { // known non-negative
        int64_t type_max = lattice_int_max(old_bits);

        zeros |= mask;
        if (min < 0) { min = 0; }
        if (max > type_max) { max = type_max; }
    } else if (max < 0 || (ones >> (old_bits - 1))) { // known non-positive
        int64_t type_min = lattice_int_min(old_bits);

        ones |= mask;
        if (min < type_min) { min = type_min; }
        if (max > -1) { max = -1; }
    }

    Lattice* this   = latuni_get(f, n);
    Lattice* narrow = lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
    return lattice_join(f, this, narrow);
    #endif
}

static Lattice* value_zext(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }

    int old_bits  = n->inputs[1]->dt.data;
    uint64_t mask = ~tb__mask(old_bits);
    Lattice* full_zxt_range = lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { 0, lattice_uint_max(old_bits), mask } });

    if (a->_int.min >= 0 || (a->_int.known_zeros >> (old_bits - 1))) { // known non-negative
        return lattice_join(f, full_zxt_range, a);
    }

    return full_zxt_range;
}

static Lattice* value_trunc(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    if (n->dt.type == TB_TAG_INT) {
        int64_t mask = tb__mask(n->dt.data);
        int64_t min = tb__sxt(a->_int.min & mask, n->dt.data, 64);
        int64_t max = tb__sxt(a->_int.max & mask, n->dt.data, 64);
        if (min > max) { return NULL; }

        uint64_t zeros = (a->_int.known_zeros & mask) | ~mask;
        uint64_t ones  =  a->_int.known_ones  & mask;
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
    } else {
        return NULL;
    }
}

static Lattice* value_bitcast(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    if (a->tag == LATTICE_INT && a->_int.min == a->_int.max && n->dt.type == TB_TAG_PTR) {
        // bitcast with a constant leads to fun cool stuff (usually we get constant zeros for NULL)
        return a->_int.min ? &XNULL_IN_THE_SKY : &NULL_IN_THE_SKY;
    }

    return NULL;
}

static Lattice* value_negate(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }
    if (a->tag != LATTICE_INT) { return NULL; }

    uint64_t mask = tb__mask(n->dt.data);
    uint64_t min = ~a->_int.min & mask;
    uint64_t max = ~a->_int.max & mask;
    if (min > max) { return NULL; }

    // -x => ~x + 1
    //   because of this addition we can technically
    //   overflow... umm? glhf?
    uint64_t min_inc = (min+1) & mask;
    uint64_t max_inc = (max+1) & mask;

    if (min_inc < min || max_inc < min) {
        return NULL;
    } else {
        min = min_inc;
        max = max_inc;
    }

    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, ~mask, 0, .widen = a->_int.widen } });
}

static void swap_edges(TB_Function* f, TB_Node* n, int i, int j) {
    TB_Node* a = n->inputs[i];
    TB_Node* b = n->inputs[j];
    set_input(f, n, b, i);
    set_input(f, n, a, j);
}

static TB_Node* ideal_select(TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];

    Lattice* key_truthy = lattice_truthy(latuni_get(f, src));
    if (key_truthy == &TRUE_IN_THE_SKY) {
        return n->inputs[2];
    } else if (key_truthy == &FALSE_IN_THE_SKY) {
        return n->inputs[3];
    }

    // ideally immediates are on the right side and i'd rather than over
    // having less-than operators
    if ((src->type == TB_CMP_SLT || src->type == TB_CMP_ULT) &&
        src->inputs[1]->type == TB_ICONST &&
        src->inputs[2]->type != TB_ICONST
    ) {
        TB_Node* new_cmp = tb_alloc_node(f, src->type == TB_CMP_SLT ? TB_CMP_SLE : TB_CMP_ULE, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
        set_input(f, new_cmp, src->inputs[2], 1);
        set_input(f, new_cmp, src->inputs[1], 2);
        TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(src, TB_NodeCompare)->cmp_dt);

        swap_edges(f, n, 2, 3);
        set_input(f, n, new_cmp, 1);
        mark_node(f, new_cmp);
        return n;
    }

    // select(y <= x, a, b) => select(x < y, b, a) flipped conditions
    if ((src->type == TB_CMP_SLE || src->type == TB_CMP_ULE) &&
        src->inputs[1]->type == TB_ICONST &&
        src->inputs[2]->type != TB_ICONST
    ) {
        TB_Node* new_cmp = tb_alloc_node(f, src->type == TB_CMP_SLE ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
        set_input(f, new_cmp, src->inputs[2], 1);
        set_input(f, new_cmp, src->inputs[1], 2);
        TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(src, TB_NodeCompare)->cmp_dt);

        swap_edges(f, n, 2, 3);
        set_input(f, n, new_cmp, 1);
        mark_node(f, new_cmp);
        return n;
    }

    // T(some_bool ? 1 : 0) => movzx(T, some_bool)
    if (src->dt.type == TB_TAG_INT && src->dt.data == 1) {
        uint64_t on_true, on_false;
        bool true_imm = get_int_const(n->inputs[2], &on_true);
        bool false_imm = get_int_const(n->inputs[3], &on_false);

        // A ? A : 0 => A (booleans)
        if (src == n->inputs[2] && false_imm && on_false == 0) {
            return src;
        }

        // A ? 0 : !A => A (booleans)
        if (inverted_cmp(src, n->inputs[3]) && true_imm && on_true == 0) {
            return src;
        }

        if (true_imm && false_imm && on_true == 1 && on_false == 0) {
            TB_Node* ext_node = tb_alloc_node(f, TB_ZERO_EXT, n->dt, 2, 0);
            set_input(f, ext_node, src, 1);
            mark_node(f, ext_node);
            return ext_node;
        }
    }

    if (TB_IS_FLOAT_TYPE(n->dt) && src->type == TB_CMP_FLT) {
        TB_Node* a = src->inputs[1];
        TB_Node* b = src->inputs[2];

        // (select (lt A B) A B) => (min A B)
        if (n->inputs[2] == a && n->inputs[3] == b) {
            TB_Node* new_node = tb_alloc_node(f, TB_FMIN, n->dt, 3, 0);
            set_input(f, new_node, a, 1);
            set_input(f, new_node, b, 2);
            return new_node;
        }

        // (select (lt A B) B A) => (max A B)
        if (n->inputs[2] == b && n->inputs[3] == a) {
            TB_Node* new_node = tb_alloc_node(f, TB_FMAX, n->dt, 3, 0);
            set_input(f, new_node, a, 1);
            set_input(f, new_node, b, 2);
            return new_node;
        }
    }

    return NULL;
}

static bool nice_ass_trunc(TB_NodeTypeEnum t) { return t == TB_AND || t == TB_XOR || t == TB_OR; }
static TB_Node* ideal_truncate(TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];

    if (src->type == TB_ZERO_EXT && src->inputs[1]->dt.type == TB_TAG_INT && n->dt.type == TB_TAG_INT) {
        int now = n->dt.data;
        int before = src->inputs[1]->dt.data;

        if (now != before) {
            // we're extending the original value
            TB_Node* ext = tb_alloc_node(f, now < before ? TB_TRUNCATE : src->type, n->dt, 2, 0);
            set_input(f, ext, src->inputs[1], 1);
            return ext;
        } else {
            return src->inputs[1];
        }
    }

    // Trunc(NiceAssBinop(a, b)) => NiceAssBinop(Trunc(a), Trunc(b))
    if (nice_ass_trunc(src->type)) {
        TB_Node* left = tb_alloc_node(f, TB_TRUNCATE, n->dt, 2, 0);
        set_input(f, left, src->inputs[1], 1);
        mark_node(f, left);

        TB_Node* right = tb_alloc_node(f, TB_TRUNCATE, n->dt, 2, 0);
        set_input(f, right, src->inputs[2], 1);
        mark_node(f, right);

        TB_Node* new_binop = tb_alloc_node(f, src->type, n->dt, 3, 0);
        set_input(f, new_binop, left, 1);
        set_input(f, new_binop, right, 2);
        return new_binop;
    }

    return NULL;
}

static TB_Node* ideal_extension(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum ext_type = n->type;
    TB_Node* src = n->inputs[1];

    if (src->type == ext_type) {
        do {
            src = src->inputs[1];
        } while (src->type == ext_type);
        set_input(f, n, src, 1);
        return n;
    }

    // we'd rather zero extends over sign extends when possible
    if (ext_type == TB_SIGN_EXT) {
        Lattice* src_t = latuni_get(f, src);
        if (lattice_int_ge(src_t, 0)) {
            n->type = TB_ZERO_EXT;
            return n;
        }
    } else if (ext_type == TB_ZERO_EXT && src->type == TB_TRUNCATE && src->inputs[1]->dt.raw == n->dt.raw) {
        TB_Node* con = make_int_node(f, n->dt, lattice_uint_max(src->dt.data));

        TB_Node* and = tb_alloc_node(f, TB_AND, n->dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, and, src->inputs[1], 1);
        set_input(f, and, con,            2);
        return and;
    }

    // Ext(phi(a: con, b: con)) => phi(Ext(a: con), Ext(b: con))
    if (src->type == TB_PHI) {
        FOR_N(i, 1, src->input_count) {
            if (src->inputs[i]->type != TB_ICONST) return NULL;
        }

        // generate extension nodes
        TB_DataType dt = n->dt;
        FOR_N(i, 1, src->input_count) {
            assert(src->inputs[i]->type == TB_ICONST);

            TB_Node* ext_node = tb_alloc_node(f, ext_type, dt, 2, 0);
            set_input(f, ext_node, src->inputs[i], 1);
            set_input(f, src, ext_node, i);
            mark_node(f, ext_node);
        }

        src->dt = dt;
        return src;
    }

    return NULL;
}

static int node_pos(TB_Node* n) {
    switch (n->type) {
        case TB_ICONST:
        case TB_F32CONST:
        case TB_F64CONST:
        return 1;

        case TB_SHR:
        return 2;

        case TB_SHL:
        return 3;

        default:
        return 4;

        case TB_PHI:
        return 5;
    }
}

static bool is_shift_op(TB_Node* n) {
    return n->type == TB_SHL || n->type == TB_SHR || n->type == TB_SAR;
}

static bool is_iconst(TB_Function* f, TB_Node* n) { return lattice_is_const(latuni_get(f, n)); }
static TB_Node* ideal_int_mod(TB_Function* f, TB_Node* n) {
    bool is_signed = n->type == TB_SMOD;

    TB_DataType dt = n->dt;
    TB_Node* x = n->inputs[1];

    uint64_t y = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
    uint64_t log2 = tb_ffs(y) - 1;
    if (!is_signed && y == (UINT64_C(1) << log2)) {
        TB_Node* and_node = tb_alloc_node(f, TB_AND, dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, and_node, x, 1);
        set_input(f, and_node, make_int_node(f, dt, y - 1), 2);
        return and_node;
    }

    return NULL;
}

static TB_Node* ideal_int_div(TB_Function* f, TB_Node* n) {
    bool is_signed = n->type == TB_SDIV;

    // if we have a constant denominator we may be able to reduce the division into a
    // multiply and shift-right
    if (n->inputs[2]->type != TB_ICONST) return NULL;

    // https://gist.github.com/B-Y-P/5872dbaaf768c204480109007f64a915
    TB_DataType dt = n->dt;
    TB_Node* x = n->inputs[1];

    uint64_t y = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
    if (y >= (1ull << 63ull)) {
        // we haven't implemented the large int case
        return NULL;
    } else if (y == 0) {
        return tb_alloc_node(f, TB_POISON, dt, 1, 0);
    } else if (y == 1) {
        return x;
    } else {
        // (udiv a N) => a >> log2(N) where N is a power of two
        uint64_t log2 = tb_ffs(y) - 1;
        if (!is_signed && y == (UINT64_C(1) << log2)) {
            TB_Node* shr_node = tb_alloc_node(f, TB_SHR, dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, shr_node, x, 1);
            set_input(f, shr_node, make_int_node(f, dt, log2), 2);
            return shr_node;
        }
    }

    // idk how to handle this yet
    if (is_signed) return NULL;

    uint64_t sh = (64 - tb_clz64(y)) - 1; // sh = ceil(log2(y)) + w - 64

    #ifndef NDEBUG
    uint64_t sh2 = 0;
    while(y > (1ull << sh2)){ sh2++; }    // sh' = ceil(log2(y))
    sh2 += 63 - 64;                       // sh  = ceil(log2(y)) + w - 64

    assert(sh == sh2);
    #endif

    // 128bit division here can't overflow
    uint64_t a = tb_div128(1ull << sh, y - 1, y);

    // now we can take a and sh and do:
    //   x / y  => mulhi(x, a) >> sh
    int bits = dt.data;
    if (bits > 32) {
        TB_Node* mul_node = tb_alloc_node(f, TB_MULPAIR, TB_TYPE_TUPLE, 3, 0);
        set_input(f, mul_node, x, 1);
        set_input(f, mul_node, make_int_node(f, dt, a), 2);

        TB_Node* lo = make_proj_node(f, dt, mul_node, 0);
        TB_Node* hi = make_proj_node(f, dt, mul_node, 1);

        mark_node(f, mul_node);
        mark_node(f, lo);
        mark_node(f, hi);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, sh_node, hi, 1);
        set_input(f, sh_node, make_int_node(f, dt, sh), 2);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        return sh_node;
    } else {
        TB_DataType big_dt = TB_TYPE_INTN(bits * 2);
        sh += bits; // chopping the low half

        a &= (1ull << bits) - 1;

        // extend x
        TB_Node* ext_node = tb_alloc_node(f, TB_ZERO_EXT, big_dt, 2, 0);
        set_input(f, ext_node, x, 1);

        TB_Node* mul_node = tb_alloc_node(f, TB_MUL, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, mul_node, ext_node, 1);
        set_input(f, mul_node, make_int_node(f, big_dt, a), 2);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, sh_node, mul_node, 1);
        set_input(f, sh_node, make_int_node(f, big_dt, sh), 2);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* trunc_node = tb_alloc_node(f, TB_TRUNCATE, dt, 2, 0);
        set_input(f, trunc_node, sh_node, 1);

        mark_node(f, mul_node);
        mark_node(f, sh_node);
        mark_node(f, ext_node);
        return trunc_node;
    }
}

////////////////////////////////
// Integer identities
////////////////////////////////
// a + 0 => a
// a - 0 => a
// a ^ 0 => a
// a * 0 => 0
// a / 0 => poison
static TB_Node* identity_int_binop(TB_Function* f, TB_Node* n) {
    if (n->type == TB_AND) {
        Lattice* aa = latuni_get(f, n->inputs[1]);
        Lattice* bb = latuni_get(f, n->inputs[2]);
        uint64_t mask = tb__mask(n->dt.data);

        if (aa != &TOP_IN_THE_SKY && bb->tag == LATTICE_INT) {
            uint64_t possible_ones = (aa->_int.known_ones | ~aa->_int.known_zeros) & mask;
            uint64_t affected      = (bb->_int.known_zeros | ~bb->_int.known_ones) & mask;

            // possible ones don't intersect with the mask? lmao
            if ((possible_ones & affected) == 0) {
                return n->inputs[1];
            }
        }
    } else if (n->type == TB_CMP_NE) {
        if (n->inputs[1] == n->inputs[2]) {
            return make_int_node(f, TB_TYPE_BOOL, 0);
        }
    } else if (n->type == TB_CMP_EQ) {
        if (n->inputs[1] == n->inputs[2]) {
            return make_int_node(f, TB_TYPE_BOOL, 1);
        }
    }

    uint64_t b;
    if (!get_int_const(n->inputs[2], &b)) {
        return n;
    }

    if (n->type == TB_MUL && b == 1) {
        return n->inputs[1];
    } else if (b == 0) {
        switch (n->type) {
            default: return n;

            case TB_SHL:
            case TB_SHR:
            case TB_ADD:
            case TB_SUB:
            case TB_XOR:
            case TB_PTR_OFFSET:
            return n->inputs[1];

            case TB_MUL:
            return make_int_node(f, n->dt, 0);

            case TB_UDIV:
            case TB_SDIV:
            case TB_UMOD:
            case TB_SMOD:
            return make_poison(f, n->dt);

            // (cmp.ne a 0) => a
            case TB_CMP_NE: {
                // walk up extension
                TB_Node* src = n->inputs[1];
                if (src->type == TB_ZERO_EXT || src->type == TB_SIGN_EXT) {
                    src = src->inputs[1];
                }

                if (src->dt.type == TB_TAG_INT && src->dt.data == 1) {
                    return src;
                }

                return n;
            }
        }
    } else {
        return n;
    }
}

