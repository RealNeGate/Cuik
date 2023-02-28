static bool is_expr_like(TB_Function* f, TB_Reg r) {
    TB_Node* n = &f->nodes[r];
    switch (n->type) {
        case TB_GET_SYMBOL_ADDRESS:
        case TB_INTEGER_CONST:
        case TB_ARRAY_ACCESS:
        case TB_MEMBER_ACCESS:
        case TB_BITCAST:
        case TB_FLOAT32_CONST:
        case TB_FLOAT64_CONST:
        case TB_INT2PTR:
        case TB_PTR2INT:
        case TB_INT2FLOAT:
        case TB_FLOAT2INT:
        case TB_FLOAT_EXT:
        case TB_SIGN_EXT:
        case TB_ZERO_EXT:
        case TB_TRUNCATE:
        case TB_LOCAL:
        case TB_PASS:
        case TB_NOT:
        case TB_NEG:
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SDIV:
        case TB_UDIV:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        return true;

        case TB_LOAD:
        if (!n->load.is_volatile) {
            return true;
        }
        return false;

        default:
        return false;
    }
}

static bool dce(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Reg r) {
    if (ctx->users[r] == NULL && is_expr_like(f, r)) {
        tb_kill_op(f, r);
        return true;
    }
    return false;
}