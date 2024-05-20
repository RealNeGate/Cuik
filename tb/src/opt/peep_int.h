
////////////////////////////////
// Arithmetic
////////////////////////////////
// division handled separately
static TB_Node* ideal_arith(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    assert(type == TB_ADD || type == TB_SUB || type == TB_MUL);

    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];

    // addition and multiplication are commutative & associative, subtraction isn't
    if (type == TB_SUB) {
        // a - immediate => a + -immediate
        if (is_iconst(f, b)) {
            Lattice* l = latuni_get(f, b);
            TB_Node* con = make_int_node(f, n->dt, -l->_int.min);

            n->type = TB_ADD;
            set_input(f, n, con, 2);
            return n;
        }
    } else {
        // commutativity opts (we want a canonical form).
        int ap = node_pos(a);
        int bp = node_pos(b);
        if (ap < bp || (ap == bp && a->gvn < b->gvn)) {
            set_input(f, n, b, 1);
            set_input(f, n, a, 2);
            return n;
        }

        // (aa + ab) + b => aa + (ab + b) where ab and b are constant
        if (a->type == type && is_iconst(f, a->inputs[2]) && is_iconst(f, b)) {
            TB_Node* abb = tb_alloc_node(f, type, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, abb, a->inputs[2], 1);
            set_input(f, abb, b, 2);

            Lattice* l = value_of(f, abb);
            assert(l->tag == LATTICE_INT && l->_int.min == l->_int.max);

            violent_kill(f, abb);

            TB_Node* con = make_int_node(f, n->dt, l->_int.min);
            set_input(f, n, a->inputs[1], 1);
            set_input(f, n, con,          2);
            return n;
        }
    }

    uint64_t rhs;
    if (type == TB_MUL && get_int_const(b, &rhs)) {
        uint64_t log2 = tb_ffs(rhs) - 1;
        if (rhs == (UINT64_C(1) << log2)) {
            TB_Node* shl_node = tb_alloc_node(f, TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, shl_node, a, 1);
            set_input(f, shl_node, make_int_node(f, n->dt, log2), 2);
            return shl_node;
        }
    }

    return NULL;
}

// im afraid of signed overflow UB
static int64_t sadd(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a + (uint64_t)b) & mask; }
static int64_t ssub(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a - (uint64_t)b) & mask; }
static Lattice* value_arith_raw(TB_Function* f, TB_NodeTypeEnum type, int bits, Lattice* a, Lattice* b) {
    int64_t mask = tb__mask(bits);
    int64_t imin = lattice_int_min(bits);
    int64_t imax = lattice_int_max(bits);
    int64_t amin = a->_int.min, amax = a->_int.max;
    int64_t bmin = b->_int.min, bmax = b->_int.max;

    assert(a->tag == LATTICE_INT && b->tag == LATTICE_INT);
    bool overflow = false;
    int64_t min, max;
    switch (type) {
        case TB_ADD:
        min = sadd(amin, bmin, mask);
        max = sadd(amax, bmax, mask);

        if (amin != amax || bmin != bmax) {
            // Ahh sweet, Hacker's delight horrors beyond my comprehension
            uint64_t u = amin & bmin & ~min;
            uint64_t v = ~(amax | bmax) & max;
            // just checking the sign bits
            if ((u | v) & imin) {
                overflow = true;
                min = imin, max = imax;
            }
        }
        break;

        case TB_SUB:
        min = ssub(amin, bmax, mask);
        max = ssub(amax, bmin, mask);
        bool overflow = false;
        if (amin != amax || bmin != bmax) {
            // Ahh sweet, Hacker's delight horrors beyond my comprehension
            uint64_t u = (amin ^ bmax) | (amin ^ min);
            uint64_t v = (amax ^ bmin) | (amax ^ max);
            if (~(u & v) & imin) {
                overflow = true;
                min = imin, max = imax;
            }
        }
        break;

        case TB_MUL:
        return NULL;

        default:
        TB_ASSERT(0);
    }

    // sign extend our integers now
    min |= min & imin ? ~mask : 0;
    max |= max & imin ? ~mask : 0;

    if (min == max) {
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, min, ~min, min } });
    } else {
        uint64_t min2 = min;
        uint64_t max2 = max;

        if (type == TB_SUB) {
            min2 = sadd(min, 1, mask);
            max2 = sadd(max, 1, mask);
        }

        // based on: https://llvm.org/doxygen/KnownBits_8cpp_source.html#l00021
        // known carry bits
        uint64_t known_carry_ones  = ~(max2 ^ a->_int.known_zeros ^ b->_int.known_zeros);
        uint64_t known_carry_zeros =   min2 ^ a->_int.known_ones  ^ b->_int.known_ones;
        // only trust which the bits in the intersection of all known bits
        uint64_t known = (a->_int.known_zeros | a->_int.known_ones)
            & (b->_int.known_zeros | b->_int.known_ones)
            & (known_carry_zeros | known_carry_ones);

        if (overflow) {
            known = 0;
        }

        uint64_t zeros = ~max2 & known;
        uint64_t ones  =  min2 & known;

        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones & mask } });
    }
}

static Lattice* value_arith(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    return value_arith_raw(f, n->type, n->dt.data, a, b);
}

////////////////////////////////
// Logical shifts
////////////////////////////////
static TB_Node* ideal_shift(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    assert(type == TB_SHL || type == TB_SHR);

    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];

    // (a << amt1) >> amt2 = (a << (amt1 - amt2)) & (ALL >> amt1)
    // (a >> amt1) << amt2 = (a >> (amt1 - amt2)) & ((1 << amt1) - 1)
    uint64_t amt1, amt2;
    if ((n->inputs[1]->type == TB_SHL || n->inputs[1]->type == TB_SHR) &&
        get_int_const(n->inputs[2], &amt2) && get_int_const(n->inputs[1]->inputs[2], &amt1)) {
        TB_NodeTypeEnum inner_shift = n->inputs[1]->type;

        // track how far we're shifting (and how many bits need chopping)
        int amt       = inner_shift == TB_SHL ? amt1               : -amt1;
        uint64_t mask = inner_shift == TB_SHL ? UINT64_MAX << amt1 : UINT64_MAX >> amt2;

        // apply outer shift
        amt  += type == TB_SHL ? amt2         : -amt2;
        mask  = type == TB_SHL ? mask << amt1 :  mask >> amt1;

        TB_Node* shift = n->inputs[1]->inputs[1];
        if (amt) {
            TB_Node* imm = make_int_node(f, n->dt, amt1 - amt2);

            // if we have a negative shift amount, that's a right shift
            shift = tb_alloc_node(f, amt < 0 ? TB_SHR : TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, shift, n->inputs[1]->inputs[1], 1);
            set_input(f, shift, imm, 2);

            mark_node(f, shift);
        }

        TB_Node* mask_node = make_int_node(f, n->dt, mask);
        TB_Node* and_node = tb_alloc_node(f, TB_AND, n->dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, and_node, shift,     1);
        set_input(f, and_node, mask_node, 2);
        return and_node;
    }

    // reassociate
    //   (x + y) << 2 => (x << 2) + (y << 2) where y is constant
    uint64_t ab_amt, amt;
    if (type == TB_SHL && a->type == TB_ADD &&
        get_int_const(a->inputs[2], &ab_amt) &&
        get_int_const(b, &amt) &&
        ab_amt + amt < n->dt.data)
    {
        TB_Node* lhs = tb_alloc_node(f, TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, lhs, a->inputs[1], 1);
        set_input(f, lhs, b,            2);
        mark_node(f, lhs);

        // give it a good type to avoid monotonicity problems
        Lattice* lhs_type = value_of(f, lhs);
        latuni_set(f, lhs, lhs_type);

        // constants start with a good type
        TB_Node* rhs = make_int_node(f, n->dt, ab_amt << amt);

        n->type = TB_ADD;
        set_input(f, n, lhs, 1);
        set_input(f, n, rhs, 2);
        return n;
    }

    return NULL;
}

static Lattice* value_shift(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    if (b->tag == LATTICE_INT && b->_int.max > b->_int.min) {
        return NULL;
    }

    uint64_t bits = n->dt.data;
    uint64_t mask = tb__mask(n->dt.data);

    // shift that's in-bounds can tell us quite a few nice details
    if (b->_int.max <= bits) {
        uint64_t zeros = 0, ones = 0;
        uint64_t min = a->_int.min & mask;
        uint64_t max = a->_int.max & mask;

        // convert the ranges into unsigned values
        if (min > max) { min = 0, max = mask; }

        uint64_t bmin = b->_int.min & mask;
        uint64_t bmax = b->_int.max & mask;
        if (bmin > bmax) { bmin = 0, bmax = mask; }

        switch (n->type) {
            case TB_SHL: {
                if (bmin == bmax) {
                    uint64_t new_min = (min << bmin) & mask;
                    uint64_t new_max = (max << bmin) & mask;

                    // check if we chop bits off the end (if we do we can't use
                    // the range info, we still have known bits tho)
                    if (((new_min >> bmin) & mask) != min ||
                        ((new_max >> bmin) & mask) != max) {
                        new_min = lattice_int_min(n->dt.data) | ~mask;
                        new_max = lattice_int_max(n->dt.data);
                    }

                    // we know exactly where the bits went
                    ones <<= b->_int.min;
                    min = new_min, max = new_max;
                }

                // we at least shifted this many bits therefore we
                // at least have this many zeros at the bottom
                zeros |= (1ull << b->_int.min) - 1ull;
                break;
            }

            case TB_SHR: {
                // the largest value is caused by the lowest shift amount
                min = (min >> b->_int.max) + 1;
                max = (max >> b->_int.min);

                // if we know how many bits we shifted then we know where
                // our known ones ones went
                if (b->_int.min == b->_int.max) {
                    ones  = a->_int.known_ones  >> b->_int.min;
                    zeros = a->_int.known_zeros >> b->_int.min;
                    zeros |= ~(mask >> b->_int.min) & mask;
                }
                break;
            }

            default: tb_todo();
        }

        int64_t lower = lattice_int_min(n->dt.data) | ~mask;
        int64_t upper = lattice_int_max(n->dt.data);

        if (min > max) { return NULL; }
        if (min < lower) { min = lower; }
        if (max > upper) { max = upper; }

        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones } });
    } else {
        return NULL;
    }
}

////////////////////////////////
// Bitwise ops
////////////////////////////////
static TB_Node* ideal_bits(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];

    assert(is_commutative(type));
    assert(is_associative(type));

    // commutativity
    int ap = node_pos(a);
    int bp = node_pos(b);
    if (ap < bp || (ap == bp && a->gvn < b->gvn)) {
        set_input(f, n, b, 1);
        set_input(f, n, a, 2);
        return n;
    }

    // reassociation
    if (a->type == type && is_iconst(f, a->inputs[2]) && is_iconst(f, b)) {
        TB_Node* abb = tb_alloc_node(f, type, n->dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, abb, a->inputs[2], 1);
        set_input(f, abb, b, 2);

        Lattice* l = value_of(f, abb);
        assert(l->tag == LATTICE_INT && l->_int.min == l->_int.max);

        violent_kill(f, abb);

        TB_Node* con = make_int_node(f, n->dt, l->_int.min);
        set_input(f, n, a->inputs[1], 1);
        set_input(f, n, con,          2);
        return n;
    }

    if (type == TB_OR) {
        assert(n->dt.type == TB_TAG_INT);
        int bits = n->dt.data;

        // (or (shl a 24) (shr a 40)) => (rol a 24)
        if (a->type == TB_SHL && b->type == TB_SHR) {
            uint64_t shl_amt, shr_amt;
            if (a->inputs[1] == b->inputs[1] &&
                get_int_const(a->inputs[2], &shl_amt) &&
                get_int_const(b->inputs[2], &shr_amt) &&
                shl_amt == bits - shr_amt) {
                // convert to rotate left
                n->type = TB_ROL;
                set_input(f, n, b->inputs[1], 1);
                set_input(f, n, b->inputs[2], 2);
                return n;
            }
        }
    }

    return NULL;
}

static Lattice* value_bits(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    int64_t min = a->_int.min;
    int64_t max = a->_int.max;

    uint64_t zeros, ones;
    switch (n->type) {
        case TB_AND:
        // 0 if either is zero, 1 if both are 1
        zeros = a->_int.known_zeros | b->_int.known_zeros;
        ones  = a->_int.known_ones  & b->_int.known_ones;

        min = (min & ~zeros) | ones;
        max = (max & ~zeros) | ones;
        if (min > max) {
            min = lattice_int_min(n->dt.data);
            max = lattice_int_max(n->dt.data);
        }
        break;

        case TB_OR:
        // 0 if both are 0, 1 if either is 1
        zeros = a->_int.known_zeros & b->_int.known_zeros;
        ones  = a->_int.known_ones  | b->_int.known_ones;
        break;

        case TB_XOR:
        // 0 if both bits are 0 or 1
        // 1 if both bits aren't the same
        zeros = (a->_int.known_zeros & b->_int.known_zeros) | (a->_int.known_ones & b->_int.known_ones);
        ones  = (a->_int.known_zeros & b->_int.known_ones)  | (a->_int.known_ones & b->_int.known_zeros);
        break;

        default: tb_todo();
    }

    uint64_t mask = tb__mask(n->dt.data);
    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones & mask } });
}

////////////////////////////////
// Comparison
////////////////////////////////
static TB_Node* ideal_cmp(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    if (type == TB_CMP_EQ) {
        // (a == 0) is !a
        TB_Node* cmp = n->inputs[1];

        uint64_t rhs;
        if (get_int_const(n->inputs[2], &rhs) && rhs == 0) {
            // !(a < b) is (b <= a)
            switch (cmp->type) {
                case TB_CMP_EQ: n->type = TB_CMP_NE; break;
                case TB_CMP_NE: n->type = TB_CMP_EQ; break;
                case TB_CMP_SLT: n->type = TB_CMP_SLE; break;
                case TB_CMP_SLE: n->type = TB_CMP_SLT; break;
                case TB_CMP_ULT: n->type = TB_CMP_ULE; break;
                case TB_CMP_ULE: n->type = TB_CMP_ULT; break;
                default: return NULL;
            }

            TB_DataType cmp_dt = TB_NODE_GET_EXTRA_T(cmp, TB_NodeCompare)->cmp_dt;
            TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = cmp_dt);

            set_input(f, n, cmp->inputs[2], 1);
            set_input(f, n, cmp->inputs[1], 2);
            return n;
        }
    }

    if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
        // (Cmp Sxt(a) Sxt(b)) => (Cmp a b)
        // (Cmp Zxt(a) Zxt(b)) => (Cmp a b)
        if ((n->inputs[1]->type == TB_SIGN_EXT && n->inputs[2]->type == TB_SIGN_EXT) ||
            (n->inputs[1]->type == TB_ZERO_EXT && n->inputs[2]->type == TB_ZERO_EXT)) {
            TB_DataType dt = n->inputs[1]->inputs[1]->dt;
            set_input(f, n, n->inputs[1]->inputs[1], 1);
            set_input(f, n, n->inputs[2]->inputs[1], 2);
            TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = dt);
            return n;
        }
    }

    return NULL;
}

static Lattice* value_cmp(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    TB_DataType dt = n->inputs[1]->dt;
    if (dt.type == TB_TAG_INT) {
        Lattice* cmp = value_arith_raw(f, TB_SUB, dt.data, a, b);

        // ok it's really annoying that i have to deal with the "idk bro" case in the middle
        // of each but that's why it looks like this... if you were curious (which you aren't,
        // nobody's reading this code lmao)
        switch (n->type) {
            case TB_CMP_EQ:  // cmp == 0
            if (lattice_int_eq(cmp, 0)) { return lattice_int_const(f,-1); }
            if (lattice_int_ne(cmp, 0)) { return lattice_int_const(f, 0); }
            break;
            case TB_CMP_NE:  // cmp != 0
            if (lattice_int_eq(cmp, 0)) { return lattice_int_const(f, 0); }
            if (lattice_int_ne(cmp, 0)) { return lattice_int_const(f,-1); }
            break;
            case TB_CMP_SLT: // cmp < 0
            if (lattice_int_lt(cmp, 0)) { return lattice_int_const(f,-1); }
            if (lattice_int_ge(cmp, 0)) { return lattice_int_const(f, 0); }
            break;
            case TB_CMP_SLE: // cmp <= 0
            if (lattice_int_le(cmp, 0)) { return lattice_int_const(f,-1); }
            if (lattice_int_gt(cmp, 0)) { return lattice_int_const(f, 0); }
            break;

            case TB_CMP_ULT:
            if (lattice_is_unsigned(a) && lattice_is_unsigned(b)) {
                if (lattice_int_lt(cmp, 0)) { return lattice_int_const(f,-1); }
                if (lattice_int_ge(cmp, 0)) { return lattice_int_const(f, 0); }
            }
            break;

            case TB_CMP_ULE:
            if (lattice_is_unsigned(a) && lattice_is_unsigned(b)) {
                if (lattice_int_le(cmp, 0)) { return lattice_int_const(f,-1); }
                if (lattice_int_gt(cmp, 0)) { return lattice_int_const(f, 0); }
            }
            break;
        }
    } else if (dt.type == TB_TAG_PTR && (n->type == TB_CMP_EQ || n->type == TB_CMP_NE)) {
        if (n->type == TB_CMP_EQ) {
            return a == b ? &TRUE_IN_THE_SKY : &BOOL_IN_THE_SKY;
        } else {
            return a == b ? &FALSE_IN_THE_SKY : &BOOL_IN_THE_SKY;
        }
    }

    return NULL;
}
