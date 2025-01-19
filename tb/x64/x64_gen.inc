#define R_PUSH(next)        ((1u<<30u) | (next))
#define R_POP(next)         ((2u<<30u) | (next))
#define R_CAPTURE(next, id) ((3u<<30u) | ((id)<<16u) | (next))
static uint32_t x86_grammar[][10] = {
    [0][0] = R_CAPTURE(8, 0),
    [0][1] = R_CAPTURE(2, 1),
    [0][4] = R_CAPTURE(5, 2),
    [0][5] = R_CAPTURE(6, 3),

    [TB_PTR_OFFSET+1][2] = R_PUSH(3),

    [TB_LOAD+1][0] = R_PUSH(0),

    [TB_NULL+1][3] = 4,
    [TB_NULL+1][6] = R_POP(6),
    [TB_NULL+1][7] = R_POP(7),
    [TB_NULL+1][8] = R_POP(8),

};

static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node** captures, int state) {
    switch (state) {
        case 7: {
            TB_Node* k0 = tb_alloc_node(f, x86_mov, TB_TYPE_PTR, 1, sizeof(nil));
        }
        // no match?
        default: return NULL;
    }
}
