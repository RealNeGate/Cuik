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
        Lattice* old_type = latuni_get(f, n);
        bool c = gcf_is_congruent(f, n->inputs[1], n->inputs[2]);
        Lattice* cmp = value_arith_raw(f, TB_SUB, dt, a, b, c, old_type == &TOP_IN_THE_SKY);

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
            if (lattice_int_gt(cmp, 0)) { return lattice_int_const(f, 0); }
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
