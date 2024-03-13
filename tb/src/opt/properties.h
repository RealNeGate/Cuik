// Keeping track of all kinds of TB node properties
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
        case TB_FADD: case TB_FMUL:
        return true;

        default:
        return false;
    }
}

static bool is_mem_access(TB_Node* n) {
    switch (n->type) {
        case TB_LOAD:
        case TB_STORE:
        case TB_MEMCPY:
        case TB_MEMSET:
        return true;

        default:
        return false;
    }
}
