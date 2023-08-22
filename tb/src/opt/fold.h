
#define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
#define BEXTR(src,pos) (((src) >> (pos)) & 1)
uint64_t tb__sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits) {
    uint64_t sign_bit = BEXTR(src, src_bits-1);
    uint64_t mask = MASK_UPTO(dst_bits) & ~MASK_UPTO(src_bits);

    uint64_t dst = src & ~mask;
    return dst | (sign_bit ? mask : 0);
}

static bool is_associative(TB_NodeTypeEnum type) {
    switch (type) {
        case TB_ADD: case TB_MUL:
        case TB_AND: case TB_XOR: case TB_OR:
        return true;

        default:
        return false;
    }
}

static bool is_commutative(TB_NodeTypeEnum type) {
    switch (type) {
        case TB_ADD: case TB_MUL:
        case TB_AND: case TB_XOR: case TB_OR:
        case TB_CMP_NE: case TB_CMP_EQ:
        return true;

        default:
        return false;
    }
}

static bool get_int_const(TB_Node* n, uint64_t* imm) {
    if (n->type != TB_INTEGER_CONST) return false;

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (i->num_words != 1) return false;

    *imm = i->words[0];
    return true;
}

////////////////////////////////
// Integer idealizations
////////////////////////////////
static TB_Node* ideal_truncate(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];
    if (src->type != TB_INTEGER_CONST || n->dt.type != TB_INT) {
        return NULL;
    }

    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);
    size_t src_num_words = src_i->num_words;

    TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, src_i->num_words);
    BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

    BigInt_copy(src_i->num_words, words, src_i->words);

    // mask bits on the top word
    uint64_t top_mask = (1ull << (src->dt.data & 63)) - 1;
    words[src_i->num_words - 1] &= ~top_mask;

    return new_n;
}

static TB_Node* ideal_int2ptr(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];
    if (src->type != TB_INTEGER_CONST) {
        return NULL;
    }

    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);

    TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, src_i->num_words);
    BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

    BigInt_copy(src_i->num_words, words, src_i->words);
    return new_n;
}

static TB_Node* ideal_extension(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[1];

    // Ext(phi(a: con, b: con)) => phi(Ext(a: con), Ext(b: con))
    if (src->type == TB_PHI) {
        FOREACH_N(i, 1, src->input_count) {
            if (src->inputs[i]->type != TB_INTEGER_CONST) return NULL;
        }

        // generate extension nodes
        TB_NodeTypeEnum ext_type = n->type;
        TB_DataType dt = n->dt;
        FOREACH_N(i, 1, src->input_count) {
            assert(src->inputs[i]->type == TB_INTEGER_CONST);

            TB_Node* ext_node = tb_alloc_node(f, ext_type, dt, 2, 0);
            set_input(opt, ext_node, src->inputs[i], 1);
            set_input(opt, src, ext_node, i);
            tb_pass_mark(opt, ext_node);
        }

        src->dt = dt;
        return src;
    }

    if (src->type != TB_INTEGER_CONST) {
        return NULL;
    }

    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);

    size_t src_num_words = src_i->num_words;
    size_t dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
    bool is_signed = false;
    if (n->type == TB_SIGN_EXT) {
        is_signed = BigInt_bextr(src_i->num_words, src_i->words, src->dt.data-1);
    }

    TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, dst_num_words);
    BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

    BigInt_copy(src_i->num_words, words, src_i->words);

    FOREACH_N(i, src_i->num_words, dst_num_words) {
        words[i] = is_signed ? ~UINT64_C(0) : 0;
    }

    // fixup the bits here
    uint64_t shift = (64 - (src->dt.data % 64));
    uint64_t mask = (~UINT64_C(0) >> shift) << shift;

    if (is_signed) words[src_num_words - 1] |= mask;
    else words[src_num_words - 1] &= ~mask;

    return new_n;
}

static TB_Node* ideal_int_unary(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    assert(n->type == TB_NOT || n->type == TB_NEG);
    TB_Node* src = n->inputs[1];

    if (src->type != TB_INTEGER_CONST) {
        return NULL;
    }

    assert(src->dt.type == TB_INT && src->dt.data > 0);
    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);

    TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, src_i->num_words);
    BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

    BigInt_copy(src_i->num_words, words, src_i->words);
    BigInt_not(src_i->num_words, words);

    if (n->type == TB_NEG) {
        // -x => ~x + 1
        BigInt_inc(src_i->num_words, words);
    }

    return new_n;
}

static TB_Node* ideal_int_binop(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;
    if (is_commutative(type)) {
        // if it's commutative: we wanna have a canonical form.
        // lower types to the right (constants are basically the lowest things)
        if (n->inputs[1]->type < n->inputs[2]->type) {
            TB_Node* tmp = n->inputs[1];
            set_input(opt, n, n->inputs[2], 1);
            set_input(opt, n, tmp, 2);
            return n;
        }
    }

    TB_Node* a = n->inputs[1];
    TB_Node* b = n->inputs[2];
    if (type == TB_OR) {
        assert(n->dt.type == TB_INT);
        int bits = n->dt.data;

        // (or (shr a 40) (shl a 24)) => (rol a 24)
        if (a->type == TB_SHR && b->type == TB_SHL) {
            uint64_t shl_amt, shr_amt;
            if (a->inputs[1] == b->inputs[1] &&
                get_int_const(a->inputs[2], &shr_amt) &&
                get_int_const(b->inputs[2], &shl_amt) &&
                shl_amt == bits - shr_amt) {
                // convert to rotate left
                n->type = TB_ROL;
                set_input(opt, n, b->inputs[1], 1);
                set_input(opt, n, b->inputs[2], 2);
                return n;
            }
        }
    } else if (type == TB_MUL) {
        uint64_t rhs;
        if (get_int_const(b, &rhs)) {
            uint64_t log2 = tb_ffs(rhs) - 1;
            if (rhs == (UINT64_C(1) << log2)) {
                TB_Node* shl_node = tb_alloc_node(f, TB_SHL, n->dt, 3, sizeof(TB_NodeBinopInt));
                set_input(opt, shl_node, a, 1);
                set_input(opt, shl_node, make_int_node(f, opt, n->dt, log2), 2);
                return shl_node;
            }
        }
    }

    if (a->type != TB_INTEGER_CONST || b->type != TB_INTEGER_CONST) {
        return NULL;
    }

    // fully fold
    TB_NodeInt* ai = TB_NODE_GET_EXTRA(a);
    TB_NodeInt* bi = TB_NODE_GET_EXTRA(b);

    assert(ai->num_words == bi->num_words);
    size_t num_words = ai->num_words;
    BigInt_t *a_words = ai->words, *b_words = bi->words;

    if (type >= TB_CMP_EQ && type <= TB_CMP_ULE) {
        TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, ai->num_words);
        BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

        bool result = false;
        switch (type) {
            case TB_CMP_EQ:  result = BigInt_cmp(num_words, a_words, b_words) == 0; break;
            case TB_CMP_NE:  result = BigInt_cmp(num_words, a_words, b_words) != 0; break;
            case TB_CMP_ULT: result = BigInt_cmp(num_words, a_words, b_words) <  0; break;
            case TB_CMP_ULE: result = BigInt_cmp(num_words, a_words, b_words) <= 0; break;
            default: tb_unreachable();
        }

        words[0] = result;
        return new_n;
    } else if (type >= TB_AND && type <= TB_MUL) {
        TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, ai->num_words);
        BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;
        switch (type) {
            case TB_AND: BigInt_and(num_words, a_words, b_words, words); break;
            case TB_OR:  BigInt_or(num_words, a_words, b_words, words); break;
            case TB_XOR: BigInt_xor(num_words, a_words, b_words, words); break;
            case TB_ADD: BigInt_add(num_words, a_words, num_words, b_words, num_words, words); break;
            case TB_SUB: BigInt_sub(num_words, a_words, num_words, b_words, num_words, words); break;
            case TB_MUL: BigInt_mul_basic(num_words, a_words, b_words, words); break;
            default: tb_unreachable();
        }

        // fixup the bits here
        uint64_t shift = (64 - (n->dt.data % 64)), mask = (~UINT64_C(0) >> shift) << shift;
        words[num_words-1] &= ~mask;
        return new_n;
    } else {
        return NULL;
    }
}

static TB_Node* ideal_int_div(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    bool is_signed = n->type == TB_SDIV;

    // if we have a constant denominator we may be able to reduce the division into a
    // multiply and shift-right
    if (n->inputs[2]->type != TB_INTEGER_CONST) return NULL;

    // https://gist.github.com/B-Y-P/5872dbaaf768c204480109007f64a915
    TB_DataType dt = n->dt;
    TB_Node* x = n->inputs[1];

    // we haven't implemented the large int case
    TB_NodeInt* bi = TB_NODE_GET_EXTRA(n->inputs[2]);
    if (bi->num_words != 1 || bi->words[0] >= (1ull << 63ull)) return NULL;

    uint64_t y = bi->words[0];

    // handle simpler cases
    if (y == 0) {
        return tb_alloc_node(f, TB_POISON, dt, 1, 0);
    } else if (y == 1) {
        return x;
    } else {
        // (udiv a N) => a >> log2(N) where N is a power of two
        uint64_t log2 = tb_ffs(y) - 1;
        if (!is_signed && y == (UINT64_C(1) << log2)) {
            TB_Node* shr_node = tb_alloc_node(f, TB_SHR, dt, 3, sizeof(TB_NodeBinopInt));
            set_input(opt, shr_node, x, 1);
            set_input(opt, shr_node, make_int_node(f, opt, dt, log2), 2);
            return shr_node;
        }
    }

    // idk how to handle this yet
    if (is_signed) return NULL;

    uint64_t sh = (64 - tb_clz64(y)) - 1; // sh = ceil(log2(y)) + w - 64

    #ifndef NDEBUG
    uint64_t sh2 = 0;
    while(y > (1ull << sh2)){ sh2++; }        // sh' = ceil(log2(y))
    sh2 += 63 - 64;                           // sh  = ceil(log2(y)) + w - 64

    assert(sh == sh2);
    #endif

    // 128bit division here can overflow so we need to handle that case
    uint64_t a = tb_div128(1ull << sh, y - 1, y);

    // now we can take a and sh and do:
    //   x / y  => mulhi(x, a) >> sh
    int bits = dt.data;
    if (bits > 32) {
        TB_Node* mul_node = tb_alloc_node(f, TB_MULPAIR, dt, 3, sizeof(TB_NodeMulPair));
        set_input(opt, mul_node, x, 1);
        set_input(opt, mul_node, make_int_node(f, opt, dt, a), 2);

        TB_Node* lo = make_proj_node(f, opt, dt, mul_node, 1);
        TB_Node* hi = make_proj_node(f, opt, dt, mul_node, 2);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeMulPair, .lo = lo, .hi = hi);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, dt, 3, sizeof(TB_NodeBinopInt));
        set_input(opt, sh_node, hi, 1);
        set_input(opt, sh_node, make_int_node(f, opt, dt, sh), 2);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        return sh_node;
    } else {
        TB_DataType big_dt = TB_TYPE_INTN(bits * 2);
        sh += bits; // chopping the low half

        a &= (1ull << bits) - 1;

        TB_Node* mul_node = tb_alloc_node(f, TB_MUL, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(opt, mul_node, x, 1);
        set_input(opt, mul_node, make_int_node(f, opt, big_dt, a), 2);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, big_dt, 3, sizeof(TB_NodeBinopInt));
        set_input(opt, sh_node, mul_node, 1);
        set_input(opt, sh_node, make_int_node(f, opt, big_dt, sh), 2);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* trunc_node = tb_alloc_node(f, TB_TRUNCATE, dt, 2, 0);
        set_input(opt, trunc_node, sh_node, 1);
        return trunc_node;
    }
}

////////////////////////////////
// Integer identities
////////////////////////////////
// a + 0 => a
// a - 0 => a
// a * 0 => 0
// a / 0 => poison
static TB_Node* identity_int_binop(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    if (!tb_node_is_constant_zero(n->inputs[2])) return n;

    switch (n->type) {
        default: return n;

        case TB_SHL:
        case TB_SHR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        return n->inputs[1];

        case TB_UDIV:
        case TB_SDIV:
        return tb_inst_poison(f);
    }
}

////////////////////////////////
// Pointer idealizations
////////////////////////////////
static TB_Node* ideal_array_ptr(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;
    TB_Node* base  = n->inputs[1];
    TB_Node* index = n->inputs[2];

    // (array A B 4) => (member A B*4) where B is constant
    if (index->type == TB_INTEGER_CONST) {
        TB_NodeInt* src_i = TB_NODE_GET_EXTRA(index);
        if (src_i->num_words != 1) return NULL;

        int64_t offset = src_i->words[0] * stride;
        TB_Node* new_n = tb_alloc_node(f, TB_MEMBER_ACCESS, n->dt, 2, sizeof(TB_NodeMember));
        set_input(opt, new_n, base, 1);
        TB_NODE_SET_EXTRA(new_n, TB_NodeMember, .offset = offset);
        return new_n;
    }

    // (array A (add B C) D) => (member (array A B D) C*D)
    if (index->type == TB_ADD) {
        TB_Node* new_index = index->inputs[1];
        TB_Node* add_rhs   = index->inputs[2];

        uint64_t offset;
        if (get_int_const(add_rhs, &offset)) {
            offset *= stride;

            TB_Node* new_n = tb_alloc_node(f, TB_ARRAY_ACCESS, TB_TYPE_PTR, 3, sizeof(TB_NodeArray));
            set_input(opt, new_n, base, 1);
            set_input(opt, new_n, new_index, 2);
            TB_NODE_SET_EXTRA(new_n, TB_NodeArray, .stride = stride);

            TB_Node* new_member = tb_alloc_node(f, TB_MEMBER_ACCESS, TB_TYPE_PTR, 2, sizeof(TB_NodeMember));
            set_input(opt, new_member, new_n, 1);
            TB_NODE_SET_EXTRA(new_member, TB_NodeMember, .offset = offset);

            tb_pass_mark(opt, new_n);
            tb_pass_mark(opt, new_member);
            return new_member;
        }
    }

    return NULL;
}

#if 0
static bool const_fold(TB_Function* f, TB_Label bb, TB_Node* n) {
    TB_DataType dt = n->dt;

    switch (n->type) {
        case TB_TRUNCATE: {
            TB_Node* src = n->inputs[0];
            if (src->type == TB_INTEGER_CONST) {
                TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);

                size_t dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                uint64_t* words = tb_transmute_to_int(f, opt, n, dst_num_words);
                BigInt_copy(dst_num_words, words, src_i->words);

                // fixup the bits here
                uint64_t shift = (64 - (dt.data % 64)), mask = (~UINT64_C(0) >> shift) << shift;
                words[dst_num_words-1] &= ~mask;
                return true;
            }
        }
    }
}
#endif
