#define R_PUSH(next)        ((1u<<30u) | (next))
#define R_POP(next)         ((2u<<30u) | (next))
#define R_CAPTURE(next, id) ((3u<<30u) | ((id)<<16u) | (next))
static uint32_t x86_grammar[][10] = {
    [0][0] = R_CAPTURE(1, 0),
    [0][1] = R_CAPTURE(2, 1),
    [0][4] = R_CAPTURE(5, 2),
    [0][6] = R_CAPTURE(7, 3),

    [$imm+1][5] = R_PUSH(6),

    [TB_LOAD+1][0] = R_PUSH(0),

    [TB_NULL+1][3] = 4,
    [TB_NULL+1][7] = R_POP(8),
    [TB_NULL+1][8] = R_POP(9),
    [TB_NULL+1][9] = R_POP(9),

    [TB_PTR_OFFSET+1][2] = R_PUSH(3),

};

static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node** captures, int state) {
    switch (state) {
        case 9: {
            do {
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, captures[3]))) {
                    break;
                }
            
                TB_Node* k0 = tb_alloc_node(f, x86_mov, $dt, 3, sizeof(X86MemOp));
                set_input(f, k0, captures[0], 0);
                set_input(f, k0, captures[1], 1);
                set_input(f, k0, captures[2], 2);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = as_int32(captures[3]);
                k0_extra->mode = MODE_LD;
            
                return k0;
            } while (0);
        } return NULL;
        // no match?
        default: return NULL;
    }
}
