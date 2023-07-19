TB_API bool tb_uses_effects(TB_Node* n) {
    switch (n->type) {
        case TB_LOAD:
        return true;

        // memory effects
        case TB_STORE:
        case TB_MEMCPY:
        case TB_MEMSET:
        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_CLEAR:
        case TB_ATOMIC_TEST_AND_SET:
        return true;

        case TB_PROJ:
        return n->dt.type == TB_CONTROL;

        // control flow
        case TB_PHI:
        case TB_START:
        case TB_REGION:
        case TB_BRANCH:
        case TB_RET:
        case TB_UNREACHABLE:
        case TB_DEBUGBREAK:
        case TB_TRAP:
        case TB_SCALL:
        case TB_CALL:
        return true;

        default:
        return false;
    }
}

TB_API bool tb_has_effects(TB_Node* n) {
    switch (n->type) {
        case TB_LOAD:
        return TB_NODE_GET_EXTRA_T(n, TB_NodeMemAccess)->is_volatile;

        // memory effects
        case TB_STORE:
        case TB_MEMCPY:
        case TB_MEMSET:
        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_CLEAR:
        case TB_ATOMIC_TEST_AND_SET:
        return true;

        case TB_PROJ:
        return n->dt.type == TB_CONTROL;

        // control flow
        case TB_START:
        case TB_REGION:
        case TB_BRANCH:
        case TB_RET:
        case TB_UNREACHABLE:
        case TB_DEBUGBREAK:
        case TB_TRAP:
        case TB_SCALL:
        case TB_CALL:
        return true;

        default:
        return false;
    }
}
