
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

