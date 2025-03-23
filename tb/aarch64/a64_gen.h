typedef enum A64NodeType {
    a64_add_imm12 = TB_MACH_A64 + 0,
} A64NodeType;
static const char* node_name(int n_type) {
    switch (n_type) {
        case a64_add_imm12: return "a64_add_imm12";
        default: return NULL;
    }
}

static TB_Node* mach_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {
    switch (state) {
        case 2: {
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
                float $value = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, TB_ICONST, TB_TYPE_I32, 1, sizeof(TB_NodeInt));
                set_input(f, k0, $root, k0_i++);
                TB_NodeInt* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->value = *(uint32_t*) &$value;
            
                return k0;
            } while (0);
        } return NULL;
        case 4: {
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
                double $value = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, TB_ICONST, TB_TYPE_I32, 1, sizeof(TB_NodeInt));
                set_input(f, k0, $root, k0_i++);
                TB_NodeInt* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->value = *(uint64_t*) &$value;
            
                return k0;
            } while (0);
        } return NULL;
        case 9: {
            do {
                uint64_t $b = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value;
                TB_Node* $a = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(($b == ($b & 07777)) || ($b == ($b & 077770000)))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, a64_add_imm12, $dt, 2, sizeof(A64Op));
                k0_i++;
                set_input(f, k0, $a, k0_i++);
                A64Op* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = $b;
            
                return k0;
            } while (0);
        } return NULL;
        // no match?
        default: return NULL;
    }
}

static bool mach_is_operand[512] = {
};
static bool mach_is_subpat[512] = {
};
#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static void global_init(void) {
    static const uint32_t edges[] = {
        (0)<<16 | (TB_ADD+1), R_PUSH(5),
        (0)<<16 | (TB_F32CONST+1), R_PUSH(1),
        (0)<<16 | (TB_F64CONST+1), R_PUSH(3),
        (1)<<16 | (0), 2,
        (2)<<16 | (TB_NULL+1), R_POP(2, 2),
        (3)<<16 | (0), 4,
        (4)<<16 | (TB_NULL+1), R_POP(2, 4),
        (5)<<16 | (TB_NULL+1), 6,
        (6)<<16 | (0), 7,
        (7)<<16 | (TB_ICONST+1), R_PUSH(8),
        (8)<<16 | (0), 8,
        (8)<<16 | (TB_NULL+1), R_POP(2, 9),
        (9)<<16 | (TB_NULL+1), R_POP(2, 9),
    };
    // transitions: 13
    size_t count = sizeof(edges) / (2*sizeof(uint32_t));
    node_grammar_alloc(count);
    FOR_N(i, 0, count) {
        node_grammar_put(edges[i*2], edges[i*2 + 1]);
    }
}
