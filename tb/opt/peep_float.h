
static TB_Node* ideal_farith(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;

    if (type == TB_FADD || type == TB_FMUL) {
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

    return NULL;
}

static TB_Node* identity_flt_binop(TB_Function* f, TB_Node* n) {
    Lattice* b = latuni_get(f, n->inputs[2]);

    // additive identity
    Lattice* add_id = lattice_f32_const(f, -0.0f);
    if (n->type == TB_FADD && lattice_meet(f, b, add_id) == add_id) {
        return n->inputs[1];
    }

    return n;
}

static Lattice* value_farith(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    Lattice* b = latuni_get(f, n->inputs[2]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }
    if (b == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }

    if (a->tag == LATTICE_FLTCON32 && b->tag == LATTICE_FLTCON32) {
        // float32 folding
        if (n->type == TB_FADD) { return lattice_f32_const(f, a->_f32 + b->_f32); }
        if (n->type == TB_FSUB) { return lattice_f32_const(f, a->_f32 - b->_f32); }
        if (n->type == TB_FMUL) { return lattice_f32_const(f, a->_f32 * b->_f32); }
        if (n->type == TB_FDIV) { return lattice_f32_const(f, a->_f32 / b->_f32); }
    } else if (a->tag == LATTICE_FLTCON64 && b->tag == LATTICE_FLTCON64) {
        // float64 folding
        if (n->type == TB_FADD) { return lattice_f64_const(f, a->_f64 + b->_f64); }
        if (n->type == TB_FSUB) { return lattice_f64_const(f, a->_f64 - b->_f64); }
        if (n->type == TB_FMUL) { return lattice_f64_const(f, a->_f64 * b->_f64); }
        if (n->type == TB_FDIV) { return lattice_f64_const(f, a->_f64 / b->_f64); }
    }

    if (n->type != TB_FDIV) {
        // binop(~NaN, ~NaN) => ~NaN
        // binop(x,     NaN) =>  NaN
        if (n->dt.type == TB_TAG_F32) {
            if (lattice_at_least(f, a, &NAN32_IN_THE_SKY))  { return &NAN32_IN_THE_SKY; }
            if (lattice_at_least(f, b, &NAN32_IN_THE_SKY))  { return &NAN32_IN_THE_SKY; }

            if (lattice_at_least(f, a, &XNAN32_IN_THE_SKY) && lattice_at_least(f, b, &XNAN32_IN_THE_SKY)) {
                return &XNAN32_IN_THE_SKY;
            }
        } else if (n->dt.type == TB_TAG_F64) {
            if (lattice_at_least(f, a, &NAN64_IN_THE_SKY))  { return &NAN64_IN_THE_SKY; }
            if (lattice_at_least(f, b, &NAN64_IN_THE_SKY))  { return &NAN64_IN_THE_SKY; }

            if (lattice_at_least(f, a, &XNAN64_IN_THE_SKY) && lattice_at_least(f, b, &XNAN64_IN_THE_SKY)) {
                return &XNAN64_IN_THE_SKY;
            }
        }
    }

    return NULL;
}

static Lattice* value_fpneg(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }

    if (a->tag == LATTICE_FLTCON32) {
        union { float f; uint32_t i; } x;
        x.f = a->_f32;
        x.i ^= 0x80000000;
        return lattice_f32_const(f, x.f);
    }

    return NULL;
}

static Lattice* value_fpext(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }
    if (a->tag == LATTICE_FLTCON32) {
        TB_ASSERT(n->dt.type == TB_TAG_F64);
        return lattice_f64_const(f, (double) a->_f32);
    }

    return NULL;
}

static Lattice* value_int2float(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[1]);
    if (a == &TOP_IN_THE_SKY) { return &TOP_IN_THE_SKY; }
    if (a->tag != LATTICE_INT || a->_int.min != a->_int.max) { return NULL; }

    switch (n->dt.type) {
        case TB_TAG_F32: return lattice_f32_const(f, (int64_t) a->_int.min);
        case TB_TAG_F64: return lattice_f64_const(f, (int64_t) a->_int.min);
        default: tb_todo();
    }
}

