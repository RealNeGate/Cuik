
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

////////////////////////////////
// Integer idealizations
////////////////////////////////
static TB_Node* ideal_truncate(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[0];
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

static TB_Node* ideal_branch(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    // "this is what graph rewriting looks like, you may not like it but this is peak optimizer"
    // "CPU caches hate this trick"
    // "billions must stall"
    if (n->input_count == 1 &&
        n->inputs[0]->type == TB_REGION &&
        n->inputs[0]->input_count == 1 &&
        n->inputs[0]->inputs[0]->type == TB_PROJ &&
        n->inputs[0]->inputs[0]->inputs[0]->type == TB_BRANCH &&
        n->inputs[0]->inputs[0]->inputs[0]->input_count == 1) {
        return n->inputs[0]->inputs[0]->inputs[0];
    }

    return NULL;
}

static TB_Node* ideal_int2ptr(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[0];
    if (src->type != TB_INTEGER_CONST) {
        return NULL;
    }

    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(src);

    TB_Node* new_n = tb_transmute_to_int(f, opt, n->dt, src_i->num_words);
    BigInt_t* words = TB_NODE_GET_EXTRA_T(new_n, TB_NodeInt)->words;

    BigInt_copy(src_i->num_words, words, src_i->words);
    return new_n;
}

static TB_Node* ideal_extension(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* src = n->inputs[0];
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

static TB_Node* ideal_int_unary(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    assert(n->type == TB_NOT || n->type == TB_NEG);
    TB_Node* src = n->inputs[0];

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

static TB_Node* ideal_int_binop(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    // if it's commutative: move constants to the right
    if (is_commutative(n->type) && n->inputs[0]->type == TB_INTEGER_CONST && n->inputs[1]->type != TB_INTEGER_CONST) {
        TB_Node* tmp = n->inputs[0];
        set_input(opt, n, n->inputs[1], 0);
        set_input(opt, n, tmp, 1);
        return n;
    }

    TB_Node* a = n->inputs[0];
    TB_Node* b = n->inputs[1];
    if (a->type != TB_INTEGER_CONST || b->type != TB_INTEGER_CONST) {
        return NULL;
    }

    // fully fold
    TB_NodeInt* ai = TB_NODE_GET_EXTRA(a);
    TB_NodeInt* bi = TB_NODE_GET_EXTRA(b);

    assert(ai->num_words == bi->num_words);
    size_t num_words = ai->num_words;
    BigInt_t *a_words = ai->words, *b_words = bi->words;

    TB_NodeTypeEnum type = n->type;
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

static TB_Node* ideal_int_div(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    bool is_signed = n->type == TB_SDIV;

    // if we have a constant denominator we may be able to reduce the division into a
    // multiply and shift-right
    if (n->inputs[1]->type != TB_INTEGER_CONST) return NULL;

    // https://gist.github.com/B-Y-P/5872dbaaf768c204480109007f64a915
    TB_DataType dt = n->dt;
    TB_Node* x = n->inputs[0];

    // we haven't implemented the large int case
    TB_NodeInt* bi = TB_NODE_GET_EXTRA(n->inputs[1]);
    if (bi->num_words != 1 || bi->words[0] >= (1ull << 63ull)) return NULL;

    uint64_t y = bi->words[0];

    // handle simpler cases
    if (y == 0) {
        return tb_alloc_node(f, TB_POISON, dt, 0, 0);
    } else if (y == 1) {
        return x;
    } else {
        // (udiv a N) => a >> log2(N) where N is a power of two
        uint64_t log2 = tb_ffs(y) - 1;
        if (!is_signed && y == (UINT64_C(1) << log2)) {
            TB_Node* shr_node = tb_alloc_node(f, TB_SHR, dt, 2, sizeof(TB_NodeMulPair));
            set_input(opt, shr_node, x, 0);
            set_input(opt, shr_node, make_int_node(f, opt, dt, log2), 1);
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
        TB_Node* mul_node = tb_alloc_node(f, TB_MULPAIR, dt, 2, sizeof(TB_NodeMulPair));
        set_input(opt, mul_node, x, 0);
        set_input(opt, mul_node, make_int_node(f, opt, dt, a), 1);

        TB_Node* lo = make_proj_node(f, opt, dt, mul_node, 0);
        TB_Node* hi = make_proj_node(f, opt, dt, mul_node, 1);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeMulPair, .lo = lo, .hi = hi);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, dt, 2, sizeof(TB_NodeBinopInt));
        set_input(opt, sh_node, hi, 0);
        set_input(opt, sh_node, make_int_node(f, opt, dt, sh), 1);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        return sh_node;
    } else {
        TB_DataType big_dt = TB_TYPE_INTN(bits * 2);
        sh += bits; // chopping the low half

        a &= (1ull << bits) - 1;

        TB_Node* mul_node = tb_alloc_node(f, TB_MUL, big_dt, 2, sizeof(TB_NodeBinopInt));
        set_input(opt, mul_node, x, 0);
        set_input(opt, mul_node, make_int_node(f, opt, big_dt, a), 1);
        TB_NODE_SET_EXTRA(mul_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* sh_node = tb_alloc_node(f, TB_SHR, big_dt, 2, sizeof(TB_NodeBinopInt));
        set_input(opt, sh_node, mul_node, 0);
        set_input(opt, sh_node, make_int_node(f, opt, big_dt, sh), 1);
        TB_NODE_SET_EXTRA(sh_node, TB_NodeBinopInt, .ab = 0);

        TB_Node* trunc_node = tb_alloc_node(f, TB_TRUNCATE, dt, 1, 0);
        set_input(opt, trunc_node, sh_node, 0);
        return trunc_node;
    }
}

////////////////////////////////
// Integer identities
////////////////////////////////
// a + 0 => a
// a * 0 => 0
// a / 0 => poison
static TB_Node* identity_int_binop(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    if (!tb_node_is_constant_zero(n->inputs[1])) return n;

    switch (n->type) {
        default: return n;

        case TB_ADD: return n->inputs[0];
        case TB_MUL: return n->inputs[0];
        case TB_UDIV: return tb_alloc_node(f, TB_POISON, n->dt, 0, 0);
        case TB_SDIV: return tb_alloc_node(f, TB_POISON, n->dt, 0, 0);
    }
}

////////////////////////////////
// Pointer idealizations
////////////////////////////////
static TB_Node* ideal_array_ptr(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    if (n->inputs[1]->type != TB_INTEGER_CONST) return NULL;

    TB_NodeInt* src_i = TB_NODE_GET_EXTRA(n->inputs[1]);
    if (src_i->num_words != 1) return NULL;

    int64_t offset = src_i->words[0] * TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;
    TB_Node* new_n = tb_alloc_node(f, TB_MEMBER_ACCESS, n->dt, 1, sizeof(TB_NodeMember));
    set_input(opt, new_n, n->inputs[0], 0);
    TB_NODE_SET_EXTRA(new_n, TB_NodeMember, .offset = offset);
    return new_n;
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
