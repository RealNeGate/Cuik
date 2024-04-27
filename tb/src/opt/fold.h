
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

// cmp.slt(a, 0) => is_sign(a)
static bool sign_check(TB_Node* n) {
    uint64_t x;
    return n->type == TB_CMP_SLT && get_int_const(n->inputs[2], &x) && x == 0;
}

static bool is_non_zero(TB_Node* n) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return n->type == TB_ICONST && i->value != 0;
}

static bool is_zero(TB_Node* n) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return n->type == TB_ICONST && i->value == 0;
}

static bool is_one(TB_Node* n) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    return n->type == TB_ICONST && i->value == 1;
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

// im afraid of signed overflow UB
static int64_t sadd(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a + (uint64_t)b) & mask; }
static int64_t ssub(int64_t a, int64_t b, uint64_t mask) { return ((uint64_t)a - (uint64_t)b) & mask; }
static Lattice* value_sub(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    TB_DataType dt = n->inputs[1]->dt;
    int64_t mask = tb__mask(dt.data);
    int64_t imin = lattice_int_min(dt.data);
    int64_t imax = lattice_int_max(dt.data);
    int64_t amin = a->_int.min, amax = a->_int.max;
    int64_t bmin = b->_int.min, bmax = b->_int.max;

    assert(a->tag == LATTICE_INT && b->tag == LATTICE_INT);
    int64_t min = ssub(amin, bmax, mask);
    int64_t max = ssub(amax, bmin, mask);
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

    // sign extend our integers now
    min |= min & imin ? ~mask : 0;
    max |= max & imin ? ~mask : 0;

    if (min == max) {
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, min, ~min, min } });
    } else {
        // based on: https://llvm.org/doxygen/KnownBits_8cpp_source.html#l00021
        // known carry bits
        uint64_t known_carry_ones  = ~(sadd(max, 1, mask) ^ a->_int.known_zeros ^ b->_int.known_ones);
        uint64_t known_carry_zeros =   sadd(min, 1, mask) ^ a->_int.known_ones  ^ b->_int.known_zeros;
        // only trust which the bits in the intersection of all known bits
        uint64_t known = (a->_int.known_zeros | a->_int.known_ones)
            & (b->_int.known_zeros | b->_int.known_ones)
            & (known_carry_zeros | known_carry_ones);

        if (overflow) {
            known = 0;
        }

        uint64_t zeros = ~sadd(max, 1, mask) & known;
        uint64_t ones  =  sadd(min, 1, mask) & known;

        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
    }
}

static Lattice* value_add_mul(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    int64_t mask = tb__mask(n->dt.data);
    int64_t imin = lattice_int_min(n->dt.data);
    int64_t imax = lattice_int_max(n->dt.data);
    int64_t amin = a->_int.min, amax = a->_int.max;
    int64_t bmin = b->_int.min, bmax = b->_int.max;

    assert(a->tag == LATTICE_INT && b->tag == LATTICE_INT);
    bool overflow = false;
    int64_t min, max;
    switch (n->type) {
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

                #if 0
                if (cant_signed_overflow(n)) {
                    if (lattice_int_gt(b, 0)) {
                        min = TB_MAX(a->_int.min, b->_int.min);
                        max = imax;
                    } else if (lattice_int_lt(b, 0)) {
                        min = imin;
                        max = TB_MIN(a->_int.max, b->_int.max);
                    }
                }
                #endif
            }
        }
        break;

        case TB_MUL:
        return NULL;
    }

    // sign extend our integers now
    min |= min & imin ? ~mask : 0;
    max |= max & imin ? ~mask : 0;

    if (min == max) {
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, min, ~min, min } });
    } else {
        // based on: https://llvm.org/doxygen/KnownBits_8cpp_source.html#l00021
        // known carry bits
        uint64_t known_carry_ones  = ~(max ^ a->_int.known_zeros ^ b->_int.known_zeros);
        uint64_t known_carry_zeros =   min ^ a->_int.known_ones  ^ b->_int.known_ones;
        // only trust which the bits in the intersection of all known bits
        uint64_t known = (a->_int.known_zeros | a->_int.known_ones)
            & (b->_int.known_zeros | b->_int.known_ones)
            & (known_carry_zeros | known_carry_ones);

        if (overflow) {
            known = 0;
        }

        uint64_t zeros = ~max & known;
        uint64_t ones  =  min & known;

        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones & mask } });
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
    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones } });
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

        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros | ~mask, ones } });
    } else {
        return NULL;
    }
}

static bool ucmple(uint64_t a, uint64_t b) { return a <= b; }
static Lattice* value_cmp(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }
    if (a == &BOT_IN_THE_SKY || b == &BOT_IN_THE_SKY) { return &BOT_IN_THE_SKY; }

    TB_DataType dt = n->inputs[1]->dt;
    if (dt.type == TB_TAG_INT) {
        Lattice* cmp = value_sub(f, n);

        // ok it's really annoying that i have to deal with the "idk bro" case in the middle
        // of each but that's why it looks like this... if you were curious (which you aren't
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

    // (select.f32 (v43: cmp.lt.f32 ...) (v41: load.f32 ...) (v42: load.f32 ...))
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

    // Cast(NiceAssBinop(a, b)) => NiceAssBinop(Cast(a), Cast(b))
    /*if (nice_ass_trunc(src->type)) {
        TB_Node* left = tb_alloc_node(f, ext_type, n->dt, 2, 0);
        set_input(f, left, src->inputs[1], 1);
        mark_node(f, left);
        latuni_set(f, left, value_of(f, left));

        TB_Node* right = tb_alloc_node(f, ext_type, n->dt, 2, 0);
        set_input(f, right, src->inputs[2], 1);
        mark_node(f, right);
        latuni_set(f, right, value_of(f, right));

        TB_Node* new_binop = tb_alloc_node(f, src->type, n->dt, 3, 0);
        set_input(f, new_binop, left, 1);
        set_input(f, new_binop, right, 2);
        return new_binop;
    }*/

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
static TB_Node* ideal_int_binop(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];

    // if it's commutative: we wanna have a canonical form.
    if (is_commutative(type)) {
        // if they're the same rank, then we'll just shuffle for smaller gvn on the right
        int ap = node_pos(a);
        int bp = node_pos(b);
        if (ap < bp || (ap == bp && a->gvn < b->gvn)) {
            set_input(f, n, b, 1);
            set_input(f, n, a, 2);
            return n;
        }
    }

    // a - immediate => a + -immediate
    if (type == TB_SUB && is_iconst(f, b)) {
        Lattice* l = latuni_get(f, b);
        TB_Node* con = make_int_node(f, n->dt, -l->_int.min);

        n->type = TB_ADD;
        set_input(f, n, con, 2);
        return n;
    }

    // (aa + ab) + b => aa + (ab + b) where ab and b are constant
    if (is_associative(type) && a->type == type && is_iconst(f, a->inputs[2]) && is_iconst(f, b)) {
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

    /* if (a->type == TB_TRUNCATE) {
        if (cool_ass_binop(n)) {
            TB_Node* ext = tb_alloc_node(f, TB_ZERO_EXT, a->inputs[1]->dt, 2, 0);
            set_input(f, ext, b, 1);
            latuni_set(f, ext, value_of(f, ext));
            mark_node(f, ext);

            set_input(f, n, ext, 2);
            set_input(f, n, a->inputs[1], 1);

            TB_Node* trunc = tb_alloc_node(f, TB_TRUNCATE, n->dt, 2, 0);
            subsume_node2(f, n, trunc);
            mark_users(f, trunc);
            set_input(f, trunc, n, 1);

            n->dt = a->inputs[1]->dt;

            latuni_set(f, n, value_of(f, n));
            latuni_set(f, trunc, value_of(f, trunc));
            return n;
        } else if (type >= TB_CMP_EQ && type <= TB_CMP_SLE) {
            TB_Node* ext = tb_alloc_node(f, TB_ZERO_EXT, a->inputs[1]->dt, 2, 0);
            set_input(f, ext, b, 1);
            latuni_set(f, ext, value_of(f, ext));
            mark_node(f, ext);

            set_input(f, n, ext, 2);
            set_input(f, n, a->inputs[1], 1);

            TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->inputs[1]->dt);
            return n;
        }
    } */

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
    } else if (type == TB_MUL) {
        uint64_t rhs;
        if (get_int_const(b, &rhs)) {
            uint64_t log2 = tb_ffs(rhs) - 1;
            if (rhs == (UINT64_C(1) << log2)) {
                TB_Node* shl_node = tb_alloc_node(f, TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
                set_input(f, shl_node, a, 1);
                set_input(f, shl_node, make_int_node(f, n->dt, log2), 2);

                mark_node(f, shl_node->inputs[1]);
                mark_node(f, shl_node->inputs[2]);
                return shl_node;
            }
        }
    } else if (type == TB_CMP_EQ) {
        // (a == 0) is !a
        TB_Node* cmp = n->inputs[1];

        uint64_t rhs;
        if (get_int_const(n->inputs[2], &rhs) && rhs == 0) {
            // !(a <  b) is (b <= a)
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
    } else if (type == TB_SHL || type == TB_SHR) {
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
                mark_node(f, imm);

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
    }

    if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
        // (Cmp Sxt(a) Sxt(b)) => (Cmp a b)
        if (n->inputs[1]->type == TB_SIGN_EXT && n->inputs[2]->type == TB_SIGN_EXT) {
            TB_DataType dt = n->inputs[1]->inputs[1]->dt;
            set_input(f, n, n->inputs[1]->inputs[1], 1);
            set_input(f, n, n->inputs[2]->inputs[1], 2);
            TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = dt);
            return n;
        }
    }

    return NULL;
}

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

////////////////////////////////
// Pointer idealizations
////////////////////////////////
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

#if 0
static TB_Node* ideal_array_ptr(TB_Function* f, TB_Node* n) {
    int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;
    TB_Node* base  = n->inputs[1];
    TB_Node* index = n->inputs[2];

    // (array A B 4) => (member A B*4) where B is constant
    if (index->type == TB_ICONST) {
        int64_t src_i = TB_NODE_GET_EXTRA_T(index, TB_NodeInt)->value;

        int64_t offset = src_i * stride;
        TB_Node* new_n = tb_alloc_node(f, TB_MEMBER_ACCESS, n->dt, 2, sizeof(TB_NodeMember));
        set_input(f, new_n, base, 1);
        TB_NODE_SET_EXTRA(new_n, TB_NodeMember, .offset = offset);
        return new_n;
    }

    // (array A (shl B C) D) => (array A B C<<D)
    if (index->type == TB_SHL && index->inputs[2]->type == TB_ICONST) {
        uint64_t scale = TB_NODE_GET_EXTRA_T(index->inputs[2], TB_NodeInt)->value;
        set_input(f, n, index->inputs[1], 2);
        TB_NODE_SET_EXTRA(n, TB_NodeArray, .stride = stride << scale);
        return n;
    }

    // (array A (mul B C) D) => (array A B C*D)
    if (index->type == TB_MUL && index->inputs[2]->type == TB_ICONST) {
        uint64_t factor = TB_NODE_GET_EXTRA_T(index->inputs[2], TB_NodeInt)->value;
        set_input(f, n, index->inputs[1], 2);
        TB_NODE_SET_EXTRA(n, TB_NodeArray, .stride = stride * factor);
        return n;
    }

    if (index->type == TB_ADD) {
        TB_Node* new_index = index->inputs[1];
        TB_Node* add_rhs   = index->inputs[2];

        uint64_t offset;
        if (get_int_const(add_rhs, &offset)) {
            // (array A (add B C) D) => (member (array A B D) C*D)
            offset *= stride;

            TB_Node* new_n = tb_alloc_node(f, TB_ARRAY_ACCESS, TB_TYPE_PTR, 3, sizeof(TB_NodeArray));
            set_input(f, new_n, base, 1);
            set_input(f, new_n, new_index, 2);
            TB_NODE_SET_EXTRA(new_n, TB_NodeArray, .stride = stride);

            TB_Node* new_member = tb_alloc_node(f, TB_MEMBER_ACCESS, TB_TYPE_PTR, 2, sizeof(TB_NodeMember));
            set_input(f, new_member, new_n, 1);
            TB_NODE_SET_EXTRA(new_member, TB_NodeMember, .offset = offset);

            mark_node(f, new_n);
            mark_node(f, new_member);
            return new_member;
        } else if (add_rhs->type == TB_SHL && add_rhs->inputs[2]->type == TB_ICONST) {
            // (array A (add B (shl C D)) E) => (array (array A B 1<<D) B E)
            TB_Node* second_index = add_rhs->inputs[1];
            uint64_t amt = 1ull << TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value;

            TB_Node* new_n = tb_alloc_node(f, TB_ARRAY_ACCESS, TB_TYPE_PTR, 3, sizeof(TB_NodeArray));
            set_input(f, new_n, base, 1);
            set_input(f, new_n, second_index, 2);
            TB_NODE_SET_EXTRA(new_n, TB_NodeArray, .stride = amt);

            mark_node(f, new_n);
            set_input(f, n, new_n,     1);
            set_input(f, n, new_index, 2);
            return n;
        }
    }

    return NULL;
}
#endif


