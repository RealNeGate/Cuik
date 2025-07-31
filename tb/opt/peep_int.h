
// im afraid of signed overflow UB
static int64_t sadd(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a + (uint64_t)b) & mask; }
static int64_t ssub(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a - (uint64_t)b) & mask; }
static int64_t smul(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a * (uint64_t)b) & mask; }

static int node_pos(TB_Node* n) {
    switch (n->type) {
        case TB_ICONST:
        case TB_F32CONST:
        case TB_F64CONST:
        return 1;

        case TB_ADD:
        return 2;

        default:
        return 4;

        case TB_PHI:
        return 5;

        case TB_SHR:
        return 6;

        case TB_SHL:
        return 7;
    }
}

////////////////////////////////
// Arithmetic
////////////////////////////////
// division handled separately
static TB_Node* ideal_arith(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    TB_ASSERT(type == TB_ADD || type == TB_SUB || type == TB_MUL);

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

        // (a + con1) - (b + con2) => (a - b) + (con1 - con2)
        if (a->type == TB_ADD && is_iconst(f, a->inputs[2]) &&
            b->type == TB_ADD && is_iconst(f, b->inputs[2])) {
            TB_Node* con1 = a->inputs[2];
            TB_Node* con2 = b->inputs[2];

            // x = a - b
            a->type = TB_SUB;
            // set_input(f, a, a->inputs[1], 1);
            set_input(f, a, b->inputs[1], 2);

            // y = con1 - con2
            b->type = TB_SUB;
            set_input(f, b, con1, 1);
            set_input(f, b, con2, 2);

            n->type = TB_ADD;
            return n;
        }
    } else {
        // commutativity opts (we want a canonical form).
        int ap = node_pos(a);
        int bp = node_pos(b);
        if (ap < bp || (ap == bp && a->gvn > b->gvn)) {
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

        // a + (b + c) => (a + b) + c where c is constant and b is not
        if (b->type == type && is_iconst(f, b->inputs[2])) {
            TB_Node* ab = tb_alloc_node(f, type, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, ab, a, 1);
            set_input(f, ab, b->inputs[1], 2);
            latuni_set(f, ab, value_of(f, ab));

            set_input(f, n, ab,           1);
            set_input(f, n, b->inputs[2], 2);
            return n;
        }

        // (a + b) + c => (a + c) + b where b is constant
        if (a->type == type && is_iconst(f, a->inputs[2])) {
            TB_Node* ab = tb_alloc_node(f, type, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, ab, a->inputs[1], 1);
            set_input(f, ab, b, 2);
            latuni_set(f, ab, value_of(f, ab));

            set_input(f, n, ab,           1);
            set_input(f, n, a->inputs[2], 2);
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

// Ripped, straight, from, Hacker's delight... mmm delight
static uint64_t mulhi64(uint64_t u, uint64_t v) {
    uint64_t u0 = u & 0xFFFFFFFF;
    uint64_t u1 = u >> 32;
    uint64_t v0 = v & 0xFFFFFFFF;
    uint64_t v1 = v >> 32;
    uint64_t w0 = u0*v0;
    uint64_t t = u1*v0 + (w0 >> 32);
    uint64_t w1 = (u0*v1) + (t & 0xFFFFFFFF);
    uint64_t w2 = (u1*v1) + (t >> 32);
    return w2 + (w1 >> 32);
}

static uint64_t mulhs(uint64_t u, uint64_t v, int bits) {
    if (bits == 64) {
        return mulhi64(u, v);
    }

    TB_ASSERT((bits & (bits - 1)) == 0);
    return (u * v) >> bits;
}

static bool mul_overflow(uint64_t u, uint64_t v, int bits, int64_t* out) {
    *out = u * v;
    uint64_t high = mulhs(u, v, bits);
    return (*out < 0 ? high + 1 : high) != 0;
}

static bool add_overflow(uint64_t u, uint64_t v, int bits, int64_t* out) {
    uint64_t res = u + v;

    u = (u >> 63) + LONG_MAX;
    return ((int64_t) ((u ^ v) | ~(v ^ res)) >= 0);
}

static bool will_mul_overflow(TB_Function* f, TB_DataType dt, Lattice* a, Lattice* b) {
    int bits = tb_data_type_bit_size(NULL, dt.type);
    TB_ASSERT(a->tag == LATTICE_INT && b->tag == LATTICE_INT);

    bool overflow = false;
    int64_t prods[4];
    overflow |= mul_overflow(a->_int.min, b->_int.min, bits, &prods[0]);
    overflow |= mul_overflow(a->_int.max, b->_int.min, bits, &prods[1]);
    overflow |= mul_overflow(a->_int.min, b->_int.max, bits, &prods[2]);
    overflow |= mul_overflow(a->_int.max, b->_int.max, bits, &prods[3]);
    return overflow;
}

static bool will_add_overflow(TB_Function* f, TB_DataType dt, Lattice* a, Lattice* b) {
    int bits = tb_data_type_bit_size(NULL, dt.type);

    int64_t mask = tb__mask(bits);
    int64_t imin = lattice_int_min(bits);
    int64_t imax = lattice_int_max(bits);
    int64_t amin = a->_int.min, amax = a->_int.max;
    int64_t bmin = b->_int.min, bmax = b->_int.max;

    int64_t min = sadd(amin, bmin, mask);
    int64_t max = sadd(amax, bmax, mask);
    if (amin != amax || bmin != bmax) {
        // Ahh sweet, Hacker's delight horrors beyond my comprehension
        uint64_t u = amin & bmin & ~min;
        uint64_t v = ~(amax | bmax) & max;
        // just checking the sign bits
        if ((((u | v) >> (bits - 1)) & 1) != 0) {
            return true;
        }
    }

    return false;
}

static void known_bits_add(uint64_t a_zeros, uint64_t a_ones, uint64_t b_zeros, uint64_t b_ones, uint64_t mask, bool sub, uint64_t* out_known) {
    uint64_t possible_sum_zeros = ~a_zeros + ~b_zeros;
    uint64_t possible_sum_ones  =  a_ones  +  b_ones;
    if (sub) {
        possible_sum_zeros += 1;
        possible_sum_ones  += 1;
    }

    // based on: https://llvm.org/doxygen/KnownBits_8cpp_source.html#l00021
    // known carry bits
    uint64_t known_carry_ones  = ~(possible_sum_zeros ^ a_zeros ^ b_zeros);
    uint64_t known_carry_zeros =   possible_sum_ones  ^ a_ones  ^ b_ones;
    // only trust which the bits in the intersection of all known bits
    uint64_t known = (a_zeros | a_ones)
        & (b_zeros | b_ones)
        & (known_carry_zeros | known_carry_ones)
        & mask;

    out_known[0] = ~possible_sum_zeros & known;
    out_known[1] =  possible_sum_ones  & known;
}

static Lattice* value_arith_raw(TB_Function* f, TB_NodeTypeEnum type, TB_DataType dt, Lattice* a, Lattice* b, bool congruent) {
    int bits = tb_data_type_bit_size(NULL, dt.type);
    int64_t mask = tb__mask(bits);
    int64_t imin = lattice_int_min(bits);
    int64_t imax = lattice_int_max(bits);
    int64_t amin = a->_int.min, amax = a->_int.max;
    int64_t bmin = b->_int.min, bmax = b->_int.max;

    TB_ASSERT(a->tag == LATTICE_INT && b->tag == LATTICE_INT);
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
            if ((((u | v) >> (bits - 1)) & 1) != 0) {
                overflow = true;
                min = imin, max = imax;
            }
        }
        break;

        case TB_SUB:
        min = ssub(amin, bmax, mask);
        max = ssub(amax, bmin, mask);
        if (amin != amax || bmin != bmax) {
            // Ahh sweet, Hacker's delight horrors beyond my comprehension
            uint64_t u = ~(amin ^ bmax) | ~(amin ^ min);
            uint64_t v = ~(amax ^ bmin) | ~(amax ^ max);

            if ((((u & v) >> (bits - 1)) & 1) == 0) {
                overflow = true;
                min = imin, max = imax;
            }
        }
        break;

        case TB_MUL: {
            int64_t prods[4];
            overflow |= mul_overflow(amin, bmin, bits, &prods[0]);
            overflow |= mul_overflow(amax, bmin, bits, &prods[1]);
            overflow |= mul_overflow(amin, bmax, bits, &prods[2]);
            overflow |= mul_overflow(amax, bmax, bits, &prods[3]);
            if (overflow) {
                min = imin, max = imax;
                break;
            }

            min = prods[0], max = prods[0];
            for (int i = 1; i < 4; i++) {
                min = TB_MIN(min, prods[i]);
                max = TB_MAX(max, prods[i]);
            }
            break;
        }

        default:
        TB_ASSERT(0);
    }

    // sign extend our integers now
    min |= min & imin ? ~mask : 0;
    max |= max & imin ? ~mask : 0;

    if (min == max) {
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, min, ~min, min } });
    } else if (type == TB_ADD || type == TB_SUB) {
        uint64_t known[2];
        if (type == TB_SUB) {
            // a - b = a + ~b + 1
            known_bits_add(a->_int.known_zeros, a->_int.known_ones, b->_int.known_ones, b->_int.known_zeros, mask, true, known);
        } else {
            known_bits_add(a->_int.known_zeros, a->_int.known_ones, b->_int.known_zeros, b->_int.known_ones, mask, false, known);
        }

        // during the optimistic solver, this will be evaluated before the type-based splitting
        // and since congruences can never exist from nodes which produce different types we should
        // prune based on that fact to avoid an ambiguity.
        Lattice* t = lattice_gimme_int2(f, min, max, known[0], known[1], 64);
        if (congruent && type == TB_SUB) {
            return lattice_int_const(f, 0);
        } else {
            return t;
        }
    } else {
        return lattice_gimme_int(f, min, max, bits);
    }
}

static Lattice* value_arith(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    bool c = gcf_is_congruent(f, n->inputs[1], n->inputs[2]);
    return value_arith_raw(f, n->type, n->dt, a, b, c);
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
            TB_Node* imm = make_int_node(f, n->dt, amt < 0 ? -amt : amt);

            // if we have a negative shift amount, that's a right shift
            shift = tb_alloc_node(f, amt < 0 ? TB_SHR : TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, shift, n->inputs[1]->inputs[1], 1);
            set_input(f, shift, imm, 2);

            mark_node(f, shift);
        }

        if (inner_shift == type) {
            return shift;
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
        get_int_const(b, &amt))
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

        Lattice* aaa = value_of(f, n);
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

    int bits = tb_data_type_bit_size(NULL, n->dt.type);
    uint64_t mask = tb__mask(bits);

    // shift that's in-bounds can tell us quite a few nice details
    if (b->_int.max <= bits) {
        uint64_t zeros = 0, ones = 0;

        int64_t min  = a->_int.min;
        int64_t max  = a->_int.max;
        int64_t bmin = b->_int.min & mask;
        int64_t bmax = b->_int.max & mask;

        switch (n->type) {
            case TB_SHL: {
                if (bmin == bmax) {
                    uint64_t new_min = (min << bmin) & mask;
                    uint64_t new_max = (max << bmin) & mask;

                    // we know exactly where the bits went
                    zeros <<= bmin;
                    ones  <<= bmin;

                    zeros |= (1ull << bmin) - 1ull;

                    // overflow check
                    if ((new_min >> bmin) == min &&
                        (new_max >> bmin) == max) {
                        return lattice_gimme_int2(f, new_min, new_max, zeros, ones, bits);
                    }
                } else {
                    // we at least shifted this many bits therefore we
                    // at least have this many zeros at the bottom
                    zeros |= (1ull << bmin) - 1ull;
                }

                min = lattice_int_min(bits);
                max = lattice_int_max(bits);
                return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
            }

            case TB_SHR: {
                // convert to unsigned range
                uint64_t new_min = min, new_max = max;
                if (((new_min >> (bits - 1)) & 1) || (new_max >> (bits - 1)) & 1) {
                    new_min = 0, new_max = mask;
                }

                // the largest value is caused by the lowest shift amount
                new_min = (new_min >> b->_int.max) & mask;
                new_max = (new_max >> b->_int.min) & mask;

                // if we know how many bits we shifted then we know where
                // our known ones ones went
                if (b->_int.min == b->_int.max) {
                    ones  = (a->_int.known_ones  & mask) >> b->_int.min;
                    zeros = (a->_int.known_zeros & mask) >> b->_int.min;
                    zeros |= ~(mask >> b->_int.min);
                }

                return lattice_gimme_uint2(f, new_min, new_max, zeros, ones, bits);
            }

            case TB_SAR: {
                // idk yet
                zeros = 0, ones = 0;

                if (b->_int.min == b->_int.max) {
                    min = tb__sxt(min >> b->_int.min, bits - b->_int.min, 64);
                    max = tb__sxt(max >> b->_int.min, bits - b->_int.min, 64);

                    return lattice_gimme_int2(f, min, max, zeros, ones, bits);
                }

                break;
            }

            default: tb_todo();
        }
    }

    return NULL;
}

////////////////////////////////
// Bitwise ops
////////////////////////////////
static TB_Node* ideal_bits(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];

    TB_ASSERT(is_commutative(type));
    TB_ASSERT(is_associative(type));

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
        TB_ASSERT(l->tag == LATTICE_INT && l->_int.min == l->_int.max);

        violent_kill(f, abb);

        TB_Node* con = make_int_node(f, n->dt, l->_int.min);
        set_input(f, n, a->inputs[1], 1);
        set_input(f, n, con,          2);
        return n;
    }

    if (type == TB_OR) {
        TB_ASSERT(TB_IS_INTEGER_TYPE(n->dt));
        int bits = tb_data_type_bit_size(NULL, n->dt.type);

        // (or (shl a 24) (shr a 40)) => (rol a 24)
        if (a->type == TB_SHL && b->type == TB_SHR) {
            uint64_t shl_amt, shr_amt;
            if (a->inputs[1] == b->inputs[1] &&
                get_int_const(a->inputs[2], &shl_amt) &&
                get_int_const(b->inputs[2], &shr_amt) &&
                shl_amt == bits - shr_amt) {
                // convert to rotate left
                n->type = TB_ROL;
                set_input(f, n, a->inputs[1], 1);
                set_input(f, n, a->inputs[2], 2);
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

    if (n->type == TB_XOR && gcf_is_congruent(f, n->inputs[1], n->inputs[2])) {
        return lattice_int_const(f, 0);
    }

    int bits = tb_data_type_bit_size(NULL, n->dt.type);
    int64_t min = lattice_int_min(bits);
    int64_t max = lattice_int_max(bits);

    uint64_t zeros, ones;
    switch (n->type) {
        case TB_AND: {
            // 0 if either is zero, 1 if both are 1
            zeros = a->_int.known_zeros | b->_int.known_zeros;
            ones  = a->_int.known_ones  & b->_int.known_ones;

            // if the range doesn't flip itself around we're safe
            if (b->_int.min == b->_int.max) {
                int64_t mmin = a->_int.min & b->_int.min;
                int64_t mmax = a->_int.max & b->_int.min;
                if (mmin < mmax) {
                    min = mmin, max = mmax;
                }
            }
            break;
        }

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

    uint64_t mask = tb__mask(bits);
    if (((ones ^ zeros) & mask) == mask) {
        return lattice_int_const(f, ones);
    }

    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
}

static TB_Node* identity_bits(TB_Function* f, TB_Node* n) {
    TB_ASSERT(n->type == TB_AND || n->type == TB_OR || n->type == TB_XOR);

    // a | a = a
    // a & a = a
    if (n->type != TB_XOR && gcf_is_congruent(f, n->inputs[1], n->inputs[2])) {
        return n->inputs[1];
    }

    Lattice* b = latuni_get(f, n->inputs[2]);
    if (lattice_int_eq(b, 0)) {
        // a & 0 = 0
        // a | 0 = a
        // a ^ 0 = a
        return n->type == TB_AND ? n->inputs[2] : n->inputs[1];
    }

    if (n->type == TB_AND) {
        Lattice* aa = latuni_get(f, n->inputs[1]);
        Lattice* bb = latuni_get(f, n->inputs[2]);

        int bits = tb_data_type_bit_size(NULL, n->dt.type);
        uint64_t mask = tb__mask(bits);

        if (aa != &TOP_IN_THE_SKY && bb->tag == LATTICE_INT) {
            uint64_t possible_ones = (aa->_int.known_ones | ~aa->_int.known_zeros) & mask;
            uint64_t affected      = (bb->_int.known_zeros | ~bb->_int.known_ones) & mask;

            // possible ones don't intersect with the mask? lmao
            if ((possible_ones & affected) == 0) {
                return n->inputs[1];
            }
        }
    }

    return n;
}

////////////////////////////////
// Division
////////////////////////////////
typedef struct {
    uint64_t a;
    uint8_t sh;
    bool add;
} IntRecip;

static IntRecip compute_recip_64(bool is_signed, uint64_t y) {
    IntRecip r;

    uint64_t rem;
    r.sh = (63 - tb_clz64(y));
    r.a = negate__div128(1ull << (r.sh - is_signed), 0, y, &rem);
    r.add = false;

    if ((y - rem) >= (1ull << r.sh)) {
        r.a *= 2;

        uint64_t rem2 = rem*2;
        if (rem2 >= y || rem2 < rem) {
            r.a += 1;
        }
        r.add = true;
    } else if (is_signed) {
        r.sh -= 1;
    }
    r.a += 1;
    return r;
}

static void sample_div(uint64_t x, IntRecip r, uint64_t y) {
    uint64_t expected = x / y;
    uint64_t got = mulhi64(x, r.a);
    if (r.add) {
        got = ((x - got) >> 1) + got;
    }
    got >>= r.sh;

    if (expected != got) {
        printf("BAD!!! %#"PRIx64" / %#"PRIx64" = %#"PRIx64" (we got %#"PRIx64")\n", x, y, expected, got);
        abort();
    }
}

static void tb_test_int_division(void) {
    uint64_t cases = 0;
    FOR_N(i, 0, 262144) {
        uint64_t y = i + ((i>>14) * (1ull << 59ull));
        if (y < 2 || (y & (y - 1)) == 0) {
            printf("Skipped %zu...\n", y);
            continue;
        }

        if (i == 262143) {
            y = (1ull << 63ull) - 1;
        }

        IntRecip r = compute_recip_64(false, y);
        uint64_t hi = 1ull << 63ull;
        FOR_N(j, 0, 500) {
            FOR_N(k, 0, 100) {
                sample_div(j * (y-k), r, y);
            }

            FOR_N(k, 0, 100) {
                sample_div(hi + (j * (y-k)), r, y);
            }

            sample_div(j,    r, y);
            sample_div(hi+j, r, y);
            cases += 202;
        }
    }
    printf("Tried: %"PRIu64" cases\n", cases);
    __debugbreak();
}

static TB_Node* int_binop(TB_Function* f, int type, TB_Node* lhs, TB_Node* rhs) {
    TB_Node* n = tb_alloc_node(f, type, lhs->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, lhs, 1);
    set_input(f, n, rhs, 2);
    mark_node(f, n);
    return n;
}

static TB_Node* ideal_int_div(TB_Function* f, TB_Node* n) {
    bool is_signed = n->type == TB_SDIV;
    // tb_test_int_division();

    // if we have a constant denominator we may be able to reduce the division into a
    // multiply and shift-right
    if (n->inputs[2]->type != TB_ICONST) return NULL;

    // https://gist.github.com/B-Y-P/5872dbaaf768c204480109007f64a915
    TB_DataType dt = n->dt;
    TB_Node* x = n->inputs[1];

    uint64_t y = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
    if (y >= (1ull << 63ull) && !is_signed) {
        // if the top bit is set in the divisor then there's literally only two quotients
        // left, from there we can reduce it to a range check.
        TB_Node* cmp_node = tb_alloc_node(f, TB_CMP_ULE, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
        set_input(f, cmp_node, n->inputs[2], 1);
        set_input(f, cmp_node, x, 2);
        TB_NODE_SET_EXTRA(cmp_node, TB_NodeCompare, .cmp_dt = dt);

        TB_Node* ext_node = tb_alloc_node(f, TB_ZERO_EXT, dt, 2, 0);
        set_input(f, ext_node, cmp_node, 1);
        return ext_node;
    } else if (y == 0) {
        return tb_alloc_node(f, TB_POISON, dt, 1, 0);
    } else if (y == 1) {
        return x;
    } else if (y == -1 && is_signed) {
        // just negate it
        TB_Node* neg = tb_alloc_node(f, TB_SUB, dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, neg, make_int_node(f, dt, 0), 1);
        set_input(f, neg, x, 2);
        return neg;
    } else {
        // (udiv a N) => a >> log2(N) where N is a power of two
        uint64_t log2 = tb_ffs(y) - 1;
        if (y == (UINT64_C(1) << log2)) {
            if (is_signed) {
                // z := x + (y - 1)
                TB_Node* add = make_int_binop(f, TB_ADD, x, make_int_node(f, dt, y - 1));

                // x < 0
                TB_Node* sign = tb_alloc_node(f, TB_CMP_SLT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
                set_input(f, sign, x, 1);
                set_input(f, sign, make_int_node(f, dt, 0), 2);
                TB_NODE_SET_EXTRA(sign, TB_NodeCompare, .cmp_dt = dt);

                // x < 0 ? z : x
                TB_Node* select = tb_alloc_node(f, TB_SELECT, dt, 4, 0);
                set_input(f, select, sign, 1);
                set_input(f, select, add,  2);
                set_input(f, select, x,    3);

                return make_int_binop(f, TB_SAR, select, make_int_node(f, dt, log2));
            } else {
                TB_Node* shr_node = tb_alloc_node(f, TB_SHR, dt, 3, sizeof(TB_NodeBinopInt));
                set_input(f, shr_node, x, 1);
                set_input(f, shr_node, make_int_node(f, dt, log2), 2);
                return shr_node;
            }
        }
    }

    bool neg = false;
    if (is_signed) {
        if (y >> 63ull) {
            y   = -y;
            neg = true;
        }
    }

    TB_ASSERT(y != 0 && y != 1 && (y >> 63ull) == 0);
    IntRecip r = compute_recip_64(is_signed, y);
    if (neg) {
        r.a = -r.a;
    }

    // now we can take a and sh and do:
    //   x / y  => mulhi(x, a) >> sh
    int bits = tb_data_type_bit_size(NULL, n->dt.type);
    if (dt.type != TB_TAG_I64) {
        TB_DataType big_dt;
        switch (dt.type) {
            case TB_TAG_I8:  big_dt = TB_TYPE_I16; break;
            case TB_TAG_I16: big_dt = TB_TYPE_I32; break;
            case TB_TAG_I32: big_dt = TB_TYPE_I64; break;
            default: tb_todo();
        }
        r.sh += bits; // chopping the low half
        r.a &= (1ull << bits) - 1;

        // extend x
        TB_Node* ext_node = tb_alloc_node(f, TB_ZERO_EXT, big_dt, 2, 0);
        set_input(f, ext_node, x, 1);

        TB_Node* mul_node = tb_alloc_node(f, TB_MUL, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, mul_node, ext_node, 1);
        set_input(f, mul_node, make_int_node(f, big_dt, r.a), 2);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, sh_node, mul_node, 1);
        set_input(f, sh_node, make_int_node(f, big_dt, r.sh), 2);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* trunc_node = tb_alloc_node(f, TB_TRUNCATE, dt, 2, 0);
        set_input(f, trunc_node, sh_node, 1);

        mark_node(f, mul_node);
        mark_node(f, sh_node);
        mark_node(f, ext_node);
        return trunc_node;
    } else {
        // hi = mulhi(x, a)
        TB_Node* mul_node = tb_alloc_node(f, is_signed ? TB_SMULPAIR : TB_UMULPAIR, TB_TYPE_TUPLE, 3, 0);
        set_input(f, mul_node, x, 1);
        set_input(f, mul_node, make_int_node(f, dt, r.a), 2);

        TB_Node* lo = make_proj_node(f, dt, mul_node, 0);
        TB_Node* hi = make_proj_node(f, dt, mul_node, 1);

        mark_node(f, mul_node);
        mark_node(f, lo);
        mark_node(f, hi);

        // Granlund and Montgomery winning rn
        if (is_signed) {
            if (r.add && neg) {
                TB_Node* d = int_binop(f, TB_SUB, make_int_node(f, dt, 0), hi);
                hi = int_binop(f, TB_ADD, hi, d);
            }

            TB_Node* q = int_binop(f, TB_SAR, hi, make_int_node(f, dt, r.sh));
            TB_Node* t = int_binop(f, TB_SHR, hi, make_int_node(f, dt, 63));
            return int_binop(f, TB_ADD, q, t);
        } else {
            if (r.add) {
                // hi = ((x - hi) >> 1) + hi;
                TB_Node* q = int_binop(f, TB_SUB, x, hi);
                TB_Node* r = int_binop(f, TB_SHR, q, make_int_node(f, dt, 1));
                hi = int_binop(f, TB_ADD, r, hi);
            }

            return int_binop(f, TB_SHR, hi, make_int_node(f, dt, r.sh));
        }
    }
}

////////////////////////////////
// Comparison
////////////////////////////////
static TB_Node* ideal_cmp(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;

    // commutativity ops
    if (type == TB_CMP_EQ || type == TB_CMP_NE) {
        TB_Node* a = n->inputs[1];
        TB_Node* b = n->inputs[2];

        // commutativity opts (we want a canonical form).
        int ap = node_pos(a);
        int bp = node_pos(b);
        if (ap < bp || (ap == bp && a->gvn < b->gvn)) {
            set_input(f, n, b, 1);
            set_input(f, n, a, 2);
            return n;
        }
    }

    TB_DataType dt = n->inputs[1]->dt;
    if (type == TB_CMP_EQ && TB_IS_FLOAT_TYPE(dt)) {
        // (a == 0) is !a
        TB_Node* cmp = n->inputs[1];

        uint64_t rhs;
        if (get_int_const(n->inputs[2], &rhs) && rhs == 0) {
            // !(a < b) is (b <= a)
            switch (cmp->type) {
                case TB_CMP_EQ:  n->type = TB_CMP_NE; break;
                case TB_CMP_NE:  n->type = TB_CMP_EQ; break;
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
    if (TB_IS_INTEGER_TYPE(dt)) {
        bool c = false; // gcf_is_congruent(f, n->inputs[1], n->inputs[2]);
        Lattice* cmp = value_arith_raw(f, TB_SUB, dt, a, b, c);

        // ok it's really annoying that i have to deal with the "idk bro" case in the middle
        // of each but that's why it looks like this... if you were curious (which you aren't :p)
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
        Lattice* meet = lattice_meet(f, a, b);
        if (meet->tag == LATTICE_NULL || meet->tag == LATTICE_PTRCON) {
            if (n->type == TB_CMP_EQ) {
                return a == b ? &TRUE_IN_THE_SKY : &BOOL_IN_THE_SKY;
            } else {
                return a == b ? &FALSE_IN_THE_SKY : &BOOL_IN_THE_SKY;
            }
        }
    }

    bool eq_id = true;

    // float oddities: regardless of the comparison, they'll always be false if a NaN is involved.
    if (dt.type == TB_TAG_F32) {
        if (a->tag == LATTICE_FLTCON32 && b->tag == LATTICE_FLTCON32) {
            if (n->type == TB_CMP_EQ) { return a->_f32 == b->_f32 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_NE) { return a->_f32 != b->_f32 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_FLT) { return a->_f32 < b->_f32 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_FLE) { return a->_f32 <= b->_f32 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
        }

        if (lattice_at_least(f, a, &NAN32_IN_THE_SKY)) { return &FALSE_IN_THE_SKY; }
        if (lattice_at_least(f, b, &NAN32_IN_THE_SKY)) { return &FALSE_IN_THE_SKY; }

        if (!lattice_at_least(f, a, &XNAN32_IN_THE_SKY) || !lattice_at_least(f, b, &XNAN32_IN_THE_SKY)) {
            eq_id = false;
        }
    } else if (dt.type == TB_TAG_F64) {
        if (a->tag == LATTICE_FLTCON64 && b->tag == LATTICE_FLTCON64) {
            if (n->type == TB_CMP_EQ) { return a->_f64 == b->_f64 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_NE) { return a->_f64 != b->_f64 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_FLT) { return a->_f64 < b->_f64 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
            if (n->type == TB_CMP_FLE) { return a->_f64 <= b->_f64 ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY; }
        }

        if (lattice_at_least(f, a, &NAN64_IN_THE_SKY)) { return &FALSE_IN_THE_SKY; }
        if (lattice_at_least(f, b, &NAN64_IN_THE_SKY)) { return &FALSE_IN_THE_SKY; }

        if (!lattice_at_least(f, a, &XNAN64_IN_THE_SKY) || !lattice_at_least(f, b, &XNAN64_IN_THE_SKY)) {
            eq_id = false;
        }
    }

    if (eq_id && gcf_is_congruent(f, n->inputs[1], n->inputs[2])) {
        if (n->type == TB_CMP_EQ) {
            return &TRUE_IN_THE_SKY;
        } else if (n->type == TB_CMP_NE) {
            return &FALSE_IN_THE_SKY;
        }
    }

    return NULL;
}

////////////////////////////////
// Extension
////////////////////////////////
static Lattice* value_zext(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }

    int dst_bits = tb_data_type_bit_size(NULL, n->dt.type);
    int src_bits = tb_data_type_bit_size(NULL, n->inputs[1]->dt.type);
    TB_ASSERT(src_bits != 64);

    if (a->_int.min == a->_int.max) {
        uint64_t mask = tb__mask(src_bits);
        int64_t min = tb__sxt(a->_int.min & mask, dst_bits, 64);
        return lattice_gimme_uint(f, min, min, dst_bits);
    }

    Lattice* full_zxt_range = lattice_gimme_int(f, 0, lattice_uint_max(src_bits), src_bits);
    if (a->_int.min >= 0 || (a->_int.known_zeros >> (src_bits - 1)) & 1) { // known non-negative
        return lattice_join(f, full_zxt_range, a);
    } else {
        return full_zxt_range;
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

    int old_bits = tb_data_type_bit_size(NULL, n->inputs[1]->dt);
    int bits     = tb_data_type_bit_size(NULL, n->dt);
    uint64_t mask  = tb__mask(bits) & ~tb__mask(old_bits);

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
        int bits = tb_data_type_bit_size(NULL, n->dt.type);
        TB_Node* con = make_int_node(f, n->dt, lattice_uint_max(bits));

        TB_Node* and = tb_alloc_node(f, TB_AND, n->dt, 3, sizeof(TB_NodeBinopInt));
        set_input(f, and, src->inputs[1], 1);
        set_input(f, and, con,            2);
        return and;
    }

    // so long as it doesn't overflow:
    //   cast(add(a, con)) => add(cast(a), con)
    if (src->type == TB_ADD && lattice_is_iconst(value_of(f, src->inputs[2]))) {
        Lattice* lhs = value_of(f, src->inputs[1]);
        Lattice* rhs = value_of(f, src->inputs[2]);
        if (!will_add_overflow(f, src->dt, lhs, rhs)) {
            TB_Node* aa = tb_alloc_node(f, ext_type, n->dt, 2, 0);
            set_input(f, aa, src->inputs[1], 1);
            mark_node(f, aa);
            latuni_set(f, aa, value_of(f, aa));

            TB_Node* bb = make_int_node(f, n->dt, rhs->_int.min);
            TB_Node* binop = tb_alloc_node(f, TB_ADD, n->dt, 3, sizeof(TB_NodeBinopInt));
            set_input(f, binop, aa, 1);
            set_input(f, binop, bb, 2);
            TB_NODE_SET_EXTRA(binop, TB_NodeBinopInt, .ab = TB_NODE_GET_EXTRA_T(src, TB_NodeBinopInt)->ab);
            return binop;
        }
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
