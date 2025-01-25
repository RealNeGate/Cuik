#define R_PUSH(next)        ((1u<<30u) | (next))
#define R_POP(next)         ((2u<<30u) | (next))
static bool x86_can_memory[512] = {
    [TB_PTR_OFFSET] = true,
};
static uint32_t x86_grammar[117][512] = {
    [2][0] = 3,
    [3][0] = 4,
    [5][0] = 5,
    [8][0] = 9,
    [10][0] = 10,
    [14][0] = 15,
    [16][0] = 16,
    [19][0] = 19,
    [23][0] = 24,
    [25][0] = 25,
    [30][0] = 30,
    [33][0] = 33,
    [34][0] = 35,
    [35][0] = 36,
    [36][0] = 37,
    [38][0] = 38,
    [41][0] = 42,
    [42][0] = 43,
    [44][0] = 45,
    [45][0] = 46,
    [47][0] = 47,
    [49][0] = 50,
    [53][0] = 53,
    [56][0] = 57,
    [57][0] = 58,
    [59][0] = 60,
    [60][0] = 61,
    [62][0] = 62,
    [64][0] = 65,
    [68][0] = 68,
    [71][0] = 72,
    [72][0] = 73,
    [74][0] = 75,
    [75][0] = 76,
    [77][0] = 77,
    [79][0] = 80,
    [83][0] = 83,
    [86][0] = 87,
    [87][0] = 88,
    [89][0] = 90,
    [90][0] = 91,
    [92][0] = 92,
    [94][0] = 95,
    [98][0] = 98,
    [101][0] = 102,
    [102][0] = 103,
    [104][0] = 105,
    [105][0] = 106,
    [107][0] = 107,
    [109][0] = 110,
    [113][0] = 113,

    [0][TB_OR+1] = R_PUSH(55),

    [0][TB_PTR_OFFSET+1] = R_PUSH(1),

    [0][TB_ADD+1] = R_PUSH(40),
    [3][TB_ADD+1] = R_PUSH(7),

    [0][TB_LOAD+1] = R_PUSH(34),
    [56][TB_LOAD+1] = R_PUSH(59),
    [86][TB_LOAD+1] = R_PUSH(89),
    [101][TB_LOAD+1] = R_PUSH(104),
    [41][TB_LOAD+1] = R_PUSH(44),
    [71][TB_LOAD+1] = R_PUSH(74),

    [67][TB_ICONST+1] = R_PUSH(68),
    [29][TB_ICONST+1] = R_PUSH(30),
    [24][TB_ICONST+1] = R_PUSH(25),
    [82][TB_ICONST+1] = R_PUSH(83),
    [52][TB_ICONST+1] = R_PUSH(53),
    [97][TB_ICONST+1] = R_PUSH(98),
    [18][TB_ICONST+1] = R_PUSH(19),
    [112][TB_ICONST+1] = R_PUSH(113),
    [9][TB_ICONST+1] = R_PUSH(10),
    [15][TB_ICONST+1] = R_PUSH(16),
    [3][TB_ICONST+1] = R_PUSH(5),

    [3][TB_SHL+1] = R_PUSH(22),
    [8][TB_SHL+1] = R_PUSH(13),

    [1][TB_NULL+1] = 2,
    [4][TB_NULL+1] = R_POP(4),
    [5][TB_NULL+1] = R_POP(6),
    [6][TB_NULL+1] = R_POP(6),
    [7][TB_NULL+1] = 8,
    [10][TB_NULL+1] = R_POP(115),
    [11][TB_NULL+1] = R_POP(12),
    [12][TB_NULL+1] = R_POP(12),
    [13][TB_NULL+1] = 14,
    [16][TB_NULL+1] = R_POP(28),
    [17][TB_NULL+1] = R_POP(18),
    [19][TB_NULL+1] = R_POP(20),
    [20][TB_NULL+1] = R_POP(21),
    [21][TB_NULL+1] = R_POP(21),
    [22][TB_NULL+1] = 23,
    [25][TB_NULL+1] = R_POP(26),
    [26][TB_NULL+1] = R_POP(27),
    [27][TB_NULL+1] = R_POP(27),
    [28][TB_NULL+1] = R_POP(29),
    [30][TB_NULL+1] = R_POP(31),
    [31][TB_NULL+1] = R_POP(32),
    [32][TB_NULL+1] = R_POP(32),
    [33][TB_NULL+1] = R_POP(33),
    [37][TB_NULL+1] = R_POP(37),
    [38][TB_NULL+1] = R_POP(39),
    [39][TB_NULL+1] = R_POP(39),
    [40][TB_NULL+1] = 41,
    [43][TB_NULL+1] = R_POP(43),
    [47][TB_NULL+1] = R_POP(51),
    [48][TB_NULL+1] = R_POP(49),
    [50][TB_NULL+1] = R_POP(50),
    [51][TB_NULL+1] = R_POP(52),
    [53][TB_NULL+1] = R_POP(54),
    [54][TB_NULL+1] = R_POP(54),
    [55][TB_NULL+1] = 56,
    [58][TB_NULL+1] = R_POP(58),
    [62][TB_NULL+1] = R_POP(66),
    [63][TB_NULL+1] = R_POP(64),
    [65][TB_NULL+1] = R_POP(65),
    [66][TB_NULL+1] = R_POP(67),
    [68][TB_NULL+1] = R_POP(69),
    [69][TB_NULL+1] = R_POP(69),
    [70][TB_NULL+1] = 71,
    [73][TB_NULL+1] = R_POP(73),
    [77][TB_NULL+1] = R_POP(81),
    [78][TB_NULL+1] = R_POP(79),
    [80][TB_NULL+1] = R_POP(80),
    [81][TB_NULL+1] = R_POP(82),
    [83][TB_NULL+1] = R_POP(84),
    [84][TB_NULL+1] = R_POP(84),
    [85][TB_NULL+1] = 86,
    [88][TB_NULL+1] = R_POP(88),
    [92][TB_NULL+1] = R_POP(96),
    [93][TB_NULL+1] = R_POP(94),
    [95][TB_NULL+1] = R_POP(95),
    [96][TB_NULL+1] = R_POP(97),
    [98][TB_NULL+1] = R_POP(99),
    [99][TB_NULL+1] = R_POP(99),
    [100][TB_NULL+1] = 101,
    [103][TB_NULL+1] = R_POP(103),
    [107][TB_NULL+1] = R_POP(111),
    [108][TB_NULL+1] = R_POP(109),
    [110][TB_NULL+1] = R_POP(110),
    [111][TB_NULL+1] = R_POP(112),
    [113][TB_NULL+1] = R_POP(114),
    [114][TB_NULL+1] = R_POP(114),
    [115][TB_NULL+1] = R_POP(116),
    [116][TB_NULL+1] = R_POP(116),

    [0][TB_SUB+1] = R_PUSH(85),

    [0][TB_XOR+1] = R_PUSH(100),

    [0][x86_MEMORY+1] = R_PUSH(33),
    [91][x86_MEMORY+1] = R_PUSH(92),
    [76][x86_MEMORY+1] = R_PUSH(77),
    [46][x86_MEMORY+1] = R_PUSH(47),
    [106][x86_MEMORY+1] = R_PUSH(107),
    [61][x86_MEMORY+1] = R_PUSH(62),
    [36][x86_MEMORY+1] = R_PUSH(38),

    [0][TB_AND+1] = R_PUSH(70),

};

static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {
    switch (state) {
        case 88: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 80: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 12: {
            do {
                TB_Node* $imm = n->inputs[2]->inputs[2];
                TB_Node* $index = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_int32(TB_TYPE_I64, $imm))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = as_int32($imm);
            
                return k0;
            } while (0);
        } return NULL;
        case 27: {
            do {
                TB_Node* $scale = n->inputs[2]->inputs[2];
                TB_Node* $index = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_scale($scale))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_PTR, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_INDEXED;
                k0_extra->scale = as_int32($scale);
            
                return k0;
            } while (0);
        } return NULL;
        case 37: {
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, $dt, 3, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $addr, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->mode = MODE_LD;
            
                return k0;
            } while (0);
        } return NULL;
        case 65: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 21: {
            do {
                TB_Node* $scale = n->inputs[2]->inputs[1]->inputs[2];
                TB_Node* $index = 1 < n->inputs[2]->inputs[1]->input_count ? n->inputs[2]->inputs[1]->inputs[1] : NULL;
                TB_Node* $disp = n->inputs[2]->inputs[2];
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_scale($scale) && fits_into_int32(TB_TYPE_I64, $disp))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_PTR, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = as_int32($disp);
                k0_extra->scale = as_int32($scale);
            
                return k0;
            } while (0);
        } return NULL;
        case 116: {
            do {
                TB_Node* $imm = n->inputs[2]->inputs[2];
                TB_Node* $index = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_int32(TB_TYPE_I64, $imm))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = as_int32($imm);
            
                return k0;
            } while (0);
        } return NULL;
        case 6: {
            do {
                TB_Node* $imm = n->inputs[2];
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_int32(TB_TYPE_I64, $imm))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = as_int32($imm);
            
                return k0;
            } while (0);
        } return NULL;
        case 114: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32(TB_TYPE_I64, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
        } return NULL;
        case 32: {
            do {
                TB_Node* $scale = n->inputs[2]->inputs[1]->inputs[2];
                TB_Node* $index = 1 < n->inputs[2]->inputs[1]->input_count ? n->inputs[2]->inputs[1]->inputs[1] : NULL;
                TB_Node* $disp = n->inputs[2]->inputs[2];
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(fits_into_scale($scale) && fits_into_int32(TB_TYPE_I64, $disp))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = as_int32($disp);
                k0_extra->scale = as_int32($scale);
            
                return k0;
            } while (0);
        } return NULL;
        case 103: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 110: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 95: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 4: {
            do {
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $index = 2 < n->input_count ? n->inputs[2] : NULL;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_INDEXED;
                k0_extra->scale = 1;
            
                return k0;
            } while (0);
        } return NULL;
        case 99: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32(TB_TYPE_I64, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
        } return NULL;
        case 58: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 69: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32(TB_TYPE_I64, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
        } return NULL;
        case 54: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32(TB_TYPE_I64, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
        } return NULL;
        case 50: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 84: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32(TB_TYPE_I64, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
        } return NULL;
        case 39: {
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 43: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 33: {
            do {
                TB_Node* $lhs = n;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 73: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        // no match?
        default: return NULL;
    }
}
