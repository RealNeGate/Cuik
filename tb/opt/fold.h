
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
    if (src->dt.type == TB_TAG_BOOL) {
        uint64_t on_true, on_false;
        bool true_imm  = get_int_const(n->inputs[2], &on_true);
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
            if (n->dt.type == TB_TAG_BOOL) {
                return src;
            }

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
