#define MASK_UPTO(pos) (~UINT64_C(0) >> (64 - pos))
#define BEXTR(src,pos) (((src) >> (pos)) & 1)
uint64_t tb__sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits) {
    uint64_t sign_bit = BEXTR(src, src_bits-1);
    uint64_t mask = MASK_UPTO(dst_bits) & ~MASK_UPTO(src_bits);

    uint64_t dst = src & ~mask;
    return dst | (sign_bit ? mask : 0);
}

static bool single_word_compare_fold(TB_NodeTypeEnum node_type, TB_DataType dt, uint64_t ai, uint64_t bi) {
    uint64_t diff;
    bool overflow = tb_sub_overflow(ai, bi, &diff);
    bool sign = diff & (1u << (dt.data - 1));

    switch (node_type) {
        case TB_CMP_EQ:  return (diff == 0);
        case TB_CMP_NE:  return (diff != 0);
        case TB_CMP_SLT: return (sign != overflow);
        case TB_CMP_SLE: return (diff == 0) || (sign != overflow);
        case TB_CMP_ULT: return (overflow);
        case TB_CMP_ULE: return (diff == 0) || overflow;
        default: tb_unreachable(); return false;
    }
}

typedef struct {
    uint64_t result;
    bool poison;
} ArithResult;

static ArithResult single_word_arith_fold(TB_NodeTypeEnum node_type, TB_DataType dt, uint64_t ai, uint64_t bi, TB_ArithmaticBehavior ab) {
    uint64_t shift = 64-dt.data;
    uint64_t mask = ~UINT64_C(0) >> shift;

    switch (node_type) {
        case TB_AND: return (ArithResult){ ai & bi };
        case TB_XOR: return (ArithResult){ ai ^ bi };
        case TB_OR:  return (ArithResult){ ai | bi };
        case TB_ADD: {
            uint64_t result;
            bool ovr = tb_add_overflow(ai << shift, bi << shift, &result);

            if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ (result >> shift) & mask };
            }
            break;
        }
        case TB_SUB: {
            uint64_t result;
            bool ovr = tb_sub_overflow(ai << shift, bi << shift, &result);

            if ((ab & TB_ARITHMATIC_NUW) && ovr) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ (result >> shift) & mask };
            }
        }
        case TB_MUL: {
            TB_MultiplyResult res = tb_mul64x128(ai, bi);

            if ((ab & TB_ARITHMATIC_NUW) && (res.hi || res.lo & ~mask)) {
                return (ArithResult){ 0, true };
            } else if ((ab & TB_ARITHMATIC_NSW) && res.hi != res.lo >> 63) {
                return (ArithResult){ 0, true };
            } else {
                return (ArithResult){ res.lo & mask };
            }
        }
        case TB_UDIV:
        case TB_SDIV: {
            if (bi == 0) {
                return (ArithResult){ 0, true };
            }

            if (node_type == TB_SDIV) {
                return (ArithResult){ ((int64_t)ai / (int64_t)bi) & mask };
            } else {
                return (ArithResult){ (ai / bi) & mask };
            }
        }
        case TB_SHL: {
            return (ArithResult){ (ai << bi) };
        }
        case TB_SHR: {
            return (ArithResult){ (ai >> bi) };
        }
        case TB_SAR: {
            tb_assert_once("Idk if this works");

            bool sign_bit = BEXTR(ai, dt.data - 1);
            uint64_t mask = (~UINT64_C(0) >> (64 - dt.data)) << dt.data;

            return (ArithResult){ (ai >> bi) | (sign_bit ? mask : 0) };
        }
        default: tb_todo();
    }
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

static void const_fold(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Reg r) {
    TB_Node* n = &f->nodes[r];
    TB_DataType dt = n->dt;

    switch (n->type) {
        ////////////////////////////////
        // Unary operator folding
        ////////////////////////////////
        // This is merely true
        //   -x => ~x + 1
        case TB_NEG: {
            TB_Node* src = &f->nodes[n->unary.src];

            if (src->type == TB_INTEGER_CONST) {
                assert(src->dt.type == TB_INT && src->dt.data > 0);

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = src->integer.num_words;

                if (src->integer.num_words == 1) {
                    n->integer.single_word = ~src->integer.single_word + 1;
                } else {
                    BigInt_t* words = tb_platform_heap_alloc(BigIntWordSize * src->integer.num_words);
                    BigInt_copy(src->integer.num_words, words, src->integer.words);
                    BigInt_not(src->integer.num_words, words);
                    BigInt_inc(src->integer.num_words, words);
                    n->integer.words = words;
                }

                MARK(r);
            }

            break;
        }

        case TB_NOT: {
            TB_Node* src = &f->nodes[n->unary.src];

            if (src->type == TB_INTEGER_CONST) {
                assert(src->dt.type == TB_INT && src->dt.data > 0);

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = src->integer.num_words;

                if (src->integer.num_words == 1) {
                    n->integer.single_word = ~src->integer.single_word;
                } else {
                    BigInt_t* words = tb_platform_heap_alloc(BigIntWordSize * src->integer.num_words);
                    BigInt_copy(src->integer.num_words, words, src->integer.words);
                    BigInt_not(src->integer.num_words, words);
                    n->integer.words = words;
                }

                MARK(r);
            }

            break;
        }

        case TB_ZERO_EXT:
        case TB_SIGN_EXT: {
            TB_Node* src = &f->nodes[n->unary.src];
            if (src->type == TB_INTEGER_CONST) {
                size_t src_num_words = src->integer.num_words;
                BigInt_t* src_words = src->integer.num_words == 1 ? &src->integer.single_word : src->integer.words;

                size_t dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                bool is_signed = false;
                if (n->type == TB_SIGN_EXT) {
                    is_signed = BigInt_bextr(src_num_words, src_words, src->dt.data-1);
                }

                BigInt_t temp;
                BigInt_t* words = dst_num_words == 1 ? &temp : tb_platform_heap_alloc(BigIntWordSize * dst_num_words);
                BigInt_copy(src_num_words, words, src_words);

                FOREACH_N(i, src_num_words, dst_num_words) {
                    words[i] = is_signed ? ~UINT64_C(0) : 0;
                }

                // fixup the bits here
                uint64_t shift = (64 - (src->dt.data % 64)), mask = (~UINT64_C(0) >> shift) << shift;
                if (is_signed) words[src_num_words - 1] |= mask;
                else words[src_num_words - 1] &= ~mask;

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = dst_num_words;
                if (dst_num_words == 1) {
                    n->integer.single_word = words[0];
                } else {
                    n->integer.words = words;
                }

                MARK(r);
            }
            break;
        }

        case TB_TRUNCATE: {
            TB_Node* src = &f->nodes[n->unary.src];
            if (src->type == TB_INTEGER_CONST) {
                size_t dst_num_words = (n->dt.data + (BigIntWordSize*8) - 1) / (BigIntWordSize*8);
                BigInt_t* src_words = src->integer.num_words == 1 ? &src->integer.single_word : src->integer.words;

                BigInt_t temp;
                BigInt_t* words = dst_num_words == 1 ? &temp : tb_platform_heap_alloc(BigIntWordSize * dst_num_words);
                BigInt_copy(dst_num_words, words, src_words);

                // fixup the bits here
                uint64_t shift = (64 - (dt.data % 64)), mask = (~UINT64_C(0) >> shift) << shift;
                words[dst_num_words-1] &= ~mask;

                n->type = TB_INTEGER_CONST;
                n->integer.num_words = dst_num_words;
                if (dst_num_words == 1) {
                    n->integer.single_word = words[0];
                } else {
                    n->integer.words = words;
                }

                MARK(r);
            }
            break;
        }

        ////////////////////////////////
        // Binary operator folding
        ////////////////////////////////
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD:
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE: {
            // if it's commutative: move constants to the right
            if (is_commutative(n->type) && f->nodes[n->i_arith.a].type == TB_INTEGER_CONST && f->nodes[n->i_arith.b].type != TB_INTEGER_CONST) {
                SWAP_BINOP_REGS(n);
            }

            TB_Node* a = &f->nodes[n->i_arith.a];
            TB_Node* b = &f->nodes[n->i_arith.b];

            if (n->dt.type == TB_FLOAT && n->dt.data == TB_FLT_32 && a->type == TB_FLOAT32_CONST && b->type == TB_FLOAT32_CONST) {
                // comparisons
                if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
                    bool result = false;
                    switch (n->type) {
                        case TB_CMP_EQ: result = (a->flt32.value == b->flt32.value); break;
                        case TB_CMP_NE: result = (a->flt32.value != b->flt32.value); break;
                        case TB_CMP_FLT: result = (a->flt32.value < b->flt32.value); break;
                        case TB_CMP_FLE: result = (a->flt32.value <= b->flt32.value); break;
                        default: tb_todo();
                    }

                    TRANSMUTE_TO_INT(n, result);
                } else if (n->type >= TB_FADD && n->type <= TB_FDIV) {
                    float result = 0.0f;
                    switch (n->type) {
                        case TB_FADD: result = a->flt32.value + b->flt32.value; break;
                        case TB_FSUB: result = a->flt32.value - b->flt32.value; break;
                        case TB_FMUL: result = a->flt32.value * b->flt32.value; break;
                        case TB_FDIV: result = a->flt32.value / b->flt32.value; break;
                        default: tb_todo();
                    }

                    TRANSMUTE_TO_FLOAT32(n, result);
                }
            } else if (n->dt.type == TB_INT && b->type == TB_INTEGER_CONST) {
                if (a->type == TB_INTEGER_CONST) {
                    // fully fold
                    int num_a_words = a->integer.num_words;
                    BigInt_t* a_words = num_a_words == 1 ? &a->integer.single_word : a->integer.words;

                    int num_b_words = b->integer.num_words;
                    BigInt_t* b_words = num_b_words == 1 ? &b->integer.single_word : b->integer.words;
                    assert(num_a_words == num_b_words);

                    BigInt_t temp;
                    BigInt_t* words = num_a_words == 1 ? &temp : tb_platform_heap_alloc(BigIntWordSize * num_a_words);

                    switch (n->type) {
                        case TB_ADD: BigInt_add(num_a_words, a_words, num_b_words, b_words, num_a_words, words); break;
                        case TB_SUB: BigInt_sub(num_a_words, a_words, num_b_words, b_words, num_a_words, words); break;
                        case TB_MUL: BigInt_mul_basic(num_a_words, a_words, b_words, words); break;
                        case TB_AND: BigInt_and(num_a_words, a_words, b_words, words); break;
                        case TB_OR:  BigInt_or(num_a_words, a_words, b_words, words); break;
                        case TB_XOR: BigInt_xor(num_a_words, a_words, b_words, words); break;
                        default: goto fail;
                    }

                    // fixup the bits here
                    uint64_t shift = (64 - (n->dt.data % 64)), mask = (~UINT64_C(0) >> shift) << shift;
                    words[num_a_words-1] &= ~mask;

                    // convert into integer
                    remove_refs(f, ctx, n);
                    n->type = TB_INTEGER_CONST;
                    n->integer.num_words = num_a_words;
                    if (num_a_words == 1) {
                        n->integer.single_word = words[0];
                    } else {
                        n->integer.words = words;
                    }
                    MARK(r);

                    // if we fail, delete our allocation, we probably should avoid failing in the future
                    fail:
                    if (num_a_words > 1) tb_platform_heap_free(words);
                } else {
                    // partial binary operations e.g.
                    //   a * 0 = 0
                    //   a + 0 = a
                    int num_src_words = b->integer.num_words;
                    BigInt_t* src_words = num_src_words == 1 ? &b->integer.single_word : b->integer.words;

                    TB_Reg ar = n->i_arith.a;
                    if (BigInt_is_zero(num_src_words, src_words)) {
                        switch (n->type) {
                            case TB_ADD: case TB_SUB:
                            case TB_XOR: case TB_OR:
                            case TB_SHL: case TB_SHR:
                            case TB_SAR:
                            TRANSMUTE_TO_PASS(n, ar);
                            break;

                            case TB_MUL: case TB_AND:
                            TRANSMUTE_TO_INT(n, 0);
                            break;

                            case TB_SDIV: case TB_UDIV:
                            TRANSMUTE_TO_POISON(n);
                            break;

                            default: break;
                        }
                    } else if (BigInt_is_small_num(num_src_words, src_words, 1)) {
                        switch (n->type) {
                            case TB_MUL: case TB_SDIV: case TB_UDIV:
                            TRANSMUTE_TO_PASS(n, ar);
                            break;

                            default: break;
                        }
                    }

                    TB_Reg r = n - f->nodes;
                    TB_Label bb = tb_find_label_from_reg(f, ar);
                    if (b->integer.num_words == 1) {
                        if (n->type == TB_MUL) {
                            // (a * b) => (a << log2(b)) where b is a power of two
                            uint64_t log2 = tb_ffs(b->integer.single_word) - 1;
                            if (b->integer.single_word == (UINT64_C(1) << log2)) {
                                OPTIMIZER_LOG(r, "converted power-of-two multiply into left shift");

                                // It's a power of two, swap in a left-shift
                                // just slap it right after the label
                                TB_Reg new_op = tb_function_insert_after(f, bb, ar);

                                f->nodes[new_op].type = TB_INTEGER_CONST;
                                f->nodes[new_op].dt = dt;
                                f->nodes[new_op].integer.num_words = 1;
                                f->nodes[new_op].integer.single_word = log2;

                                tb_todo();
                                // remove_refs(f, ctx, n);
                                /* n->type = TB_SHL;
                                n->dt = dt;
                                n->i_arith = (struct TB_NodeIArith) { .a = a - f->nodes, .b = new_op };
                                changes = true; */
                                break;
                            }
                        } else if (n->type == TB_UMOD || n->type == TB_SMOD) {
                            // (mod a N) => (and a N-1) where N is a power of two
                            uint64_t mask = b->integer.single_word;
                            if (tb_is_power_of_two(mask)) {
                                OPTIMIZER_LOG(r, "converted modulo into AND with constant mask");

                                // generate mask
                                TB_Reg extra_reg = tb_function_insert_after(f, bb, ar);
                                TB_Node* extra = &f->nodes[extra_reg];
                                extra->type = TB_INTEGER_CONST;
                                extra->dt = n->dt;
                                extra->integer.num_words = 1;
                                extra->integer.single_word = mask - 1;
                                MARK(extra_reg);

                                // new AND operation to replace old MOD
                                tb_todo();
                                /* n = &f->nodes[r];
                                n->type = TB_AND;
                                n->i_arith.b = extra_reg;
                                changes = true; */
                                break;
                            }
                        }
                    }
                }
            }
            break;
        }

        default:
        break;
    }
}
