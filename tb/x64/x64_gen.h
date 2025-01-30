#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static bool x86_can_memory[512] = {
    [TB_PTR_OFFSET] = true,
};
static uint32_t x86_grammar[155][512] = {
    [0][TB_ADD+1] = R_PUSH(35),
    [0][TB_OR+1] = R_PUSH(50),
    [0][TB_PTR_OFFSET+1] = R_PUSH(1),
    [0][TB_MUL+1] = R_PUSH(110),
    [0][TB_ROL+1] = R_PUSH(143),
    [0][TB_LOAD+1] = R_PUSH(29),
    [0][x86_MEMORY+1] = R_PUSH(28),
    [0][TB_SUB+1] = R_PUSH(80),
    [0][TB_SAR+1] = R_PUSH(137),
    [0][TB_SHR+1] = R_PUSH(131),
    [0][TB_SHL+1] = R_PUSH(125),
    [0][TB_ROR+1] = R_PUSH(149),
    [0][TB_AND+1] = R_PUSH(65),
    [0][TB_XOR+1] = R_PUSH(95),

    [1][TB_NULL+1] = 2,

    [2][0] = 3,

    [3][0] = 4,
    [3][TB_ADD+1] = R_PUSH(7),
    [3][TB_ICONST+1] = R_PUSH(5),
    [3][TB_SHL+1] = R_PUSH(22),

    [4][TB_NULL+1] = R_POP(2, 4),

    [5][0] = 5,
    [5][TB_NULL+1] = R_POP(2, 6),

    [6][TB_NULL+1] = R_POP(2, 6),

    [7][TB_NULL+1] = 8,
    [7][0] = R_POP(2, 4),

    [8][0] = 9,
    [8][TB_SHL+1] = R_PUSH(13),

    [9][TB_ICONST+1] = R_PUSH(10),
    [9][0] = R_POP(2, 4),

    [10][0] = 10,
    [10][TB_NULL+1] = R_POP(2, 11),

    [11][TB_NULL+1] = R_POP(2, 12),
    [11][0] = R_POP(2, 4),

    [12][TB_NULL+1] = R_POP(2, 12),

    [13][TB_NULL+1] = 14,
    [13][0] = R_POP(3, 4),

    [14][0] = 15,

    [15][TB_ICONST+1] = R_PUSH(16),
    [15][0] = R_POP(3, 4),

    [16][0] = 16,
    [16][TB_NULL+1] = R_POP(2, 17),

    [17][TB_NULL+1] = R_POP(2, 18),
    [17][0] = R_POP(3, 4),

    [18][TB_ICONST+1] = R_PUSH(19),
    [18][0] = R_POP(2, 4),

    [19][0] = 19,
    [19][TB_NULL+1] = R_POP(2, 20),

    [20][TB_NULL+1] = R_POP(2, 21),
    [20][0] = R_POP(2, 4),

    [21][TB_NULL+1] = R_POP(2, 21),

    [22][TB_NULL+1] = 23,
    [22][0] = R_POP(2, 4),

    [23][0] = 24,

    [24][TB_ICONST+1] = R_PUSH(25),
    [24][0] = R_POP(2, 4),

    [25][0] = 25,
    [25][TB_NULL+1] = R_POP(2, 26),

    [26][TB_NULL+1] = R_POP(2, 27),
    [26][0] = R_POP(2, 4),

    [27][TB_NULL+1] = R_POP(2, 27),

    [28][0] = 28,
    [28][TB_NULL+1] = R_POP(2, 28),

    [29][0] = 30,

    [30][0] = 31,

    [31][0] = 32,
    [31][x86_MEMORY+1] = R_PUSH(33),

    [32][TB_NULL+1] = R_POP(2, 32),

    [33][0] = 33,
    [33][TB_NULL+1] = R_POP(2, 34),

    [34][TB_NULL+1] = R_POP(2, 34),

    [35][TB_NULL+1] = 36,

    [36][0] = 37,
    [36][TB_LOAD+1] = R_PUSH(41),

    [37][0] = 38,
    [37][TB_ICONST+1] = R_PUSH(39),

    [38][TB_NULL+1] = R_POP(2, 38),

    [39][0] = 39,
    [39][TB_NULL+1] = R_POP(2, 40),

    [40][TB_NULL+1] = R_POP(2, 40),

    [41][0] = 42,

    [42][0] = 43,

    [43][x86_MEMORY+1] = R_PUSH(44),
    [43][0] = R_POP(2, 37),

    [44][0] = 44,
    [44][TB_NULL+1] = R_POP(2, 45),

    [45][TB_NULL+1] = R_POP(2, 46),
    [45][0] = R_POP(2, 37),

    [46][TB_ICONST+1] = R_PUSH(47),
    [46][0] = 49,

    [47][0] = 47,
    [47][TB_NULL+1] = R_POP(2, 48),

    [48][TB_NULL+1] = R_POP(2, 48),

    [49][TB_NULL+1] = R_POP(2, 49),

    [50][TB_NULL+1] = 51,

    [51][0] = 52,
    [51][TB_LOAD+1] = R_PUSH(56),

    [52][0] = 53,
    [52][TB_ICONST+1] = R_PUSH(54),

    [53][TB_NULL+1] = R_POP(2, 53),

    [54][0] = 54,
    [54][TB_NULL+1] = R_POP(2, 55),

    [55][TB_NULL+1] = R_POP(2, 55),

    [56][0] = 57,

    [57][0] = 58,

    [58][x86_MEMORY+1] = R_PUSH(59),
    [58][0] = R_POP(2, 52),

    [59][0] = 59,
    [59][TB_NULL+1] = R_POP(2, 60),

    [60][TB_NULL+1] = R_POP(2, 61),
    [60][0] = R_POP(2, 52),

    [61][TB_ICONST+1] = R_PUSH(62),
    [61][0] = 64,

    [62][0] = 62,
    [62][TB_NULL+1] = R_POP(2, 63),

    [63][TB_NULL+1] = R_POP(2, 63),

    [64][TB_NULL+1] = R_POP(2, 64),

    [65][TB_NULL+1] = 66,

    [66][0] = 67,
    [66][TB_LOAD+1] = R_PUSH(71),

    [67][0] = 68,
    [67][TB_ICONST+1] = R_PUSH(69),

    [68][TB_NULL+1] = R_POP(2, 68),

    [69][0] = 69,
    [69][TB_NULL+1] = R_POP(2, 70),

    [70][TB_NULL+1] = R_POP(2, 70),

    [71][0] = 72,

    [72][0] = 73,

    [73][x86_MEMORY+1] = R_PUSH(74),
    [73][0] = R_POP(2, 67),

    [74][0] = 74,
    [74][TB_NULL+1] = R_POP(2, 75),

    [75][TB_NULL+1] = R_POP(2, 76),
    [75][0] = R_POP(2, 67),

    [76][TB_ICONST+1] = R_PUSH(77),
    [76][0] = 79,

    [77][0] = 77,
    [77][TB_NULL+1] = R_POP(2, 78),

    [78][TB_NULL+1] = R_POP(2, 78),

    [79][TB_NULL+1] = R_POP(2, 79),

    [80][TB_NULL+1] = 81,

    [81][0] = 82,
    [81][TB_LOAD+1] = R_PUSH(86),

    [82][0] = 83,
    [82][TB_ICONST+1] = R_PUSH(84),

    [83][TB_NULL+1] = R_POP(2, 83),

    [84][0] = 84,
    [84][TB_NULL+1] = R_POP(2, 85),

    [85][TB_NULL+1] = R_POP(2, 85),

    [86][0] = 87,

    [87][0] = 88,

    [88][x86_MEMORY+1] = R_PUSH(89),
    [88][0] = R_POP(2, 82),

    [89][0] = 89,
    [89][TB_NULL+1] = R_POP(2, 90),

    [90][TB_NULL+1] = R_POP(2, 91),
    [90][0] = R_POP(2, 82),

    [91][TB_ICONST+1] = R_PUSH(92),
    [91][0] = 94,

    [92][0] = 92,
    [92][TB_NULL+1] = R_POP(2, 93),

    [93][TB_NULL+1] = R_POP(2, 93),

    [94][TB_NULL+1] = R_POP(2, 94),

    [95][TB_NULL+1] = 96,

    [96][0] = 97,
    [96][TB_LOAD+1] = R_PUSH(101),

    [97][0] = 98,
    [97][TB_ICONST+1] = R_PUSH(99),

    [98][TB_NULL+1] = R_POP(2, 98),

    [99][0] = 99,
    [99][TB_NULL+1] = R_POP(2, 100),

    [100][TB_NULL+1] = R_POP(2, 100),

    [101][0] = 102,

    [102][0] = 103,

    [103][x86_MEMORY+1] = R_PUSH(104),
    [103][0] = R_POP(2, 97),

    [104][0] = 104,
    [104][TB_NULL+1] = R_POP(2, 105),

    [105][TB_NULL+1] = R_POP(2, 106),
    [105][0] = R_POP(2, 97),

    [106][TB_ICONST+1] = R_PUSH(107),
    [106][0] = 109,

    [107][0] = 107,
    [107][TB_NULL+1] = R_POP(2, 108),

    [108][TB_NULL+1] = R_POP(2, 108),

    [109][TB_NULL+1] = R_POP(2, 109),

    [110][TB_NULL+1] = 111,

    [111][0] = 112,
    [111][TB_LOAD+1] = R_PUSH(116),

    [112][0] = 113,
    [112][TB_ICONST+1] = R_PUSH(114),

    [113][TB_NULL+1] = R_POP(2, 113),

    [114][0] = 114,
    [114][TB_NULL+1] = R_POP(2, 115),

    [115][TB_NULL+1] = R_POP(2, 115),

    [116][0] = 117,

    [117][0] = 118,

    [118][x86_MEMORY+1] = R_PUSH(119),
    [118][0] = R_POP(2, 112),

    [119][0] = 119,
    [119][TB_NULL+1] = R_POP(2, 120),

    [120][TB_NULL+1] = R_POP(2, 121),
    [120][0] = R_POP(2, 112),

    [121][TB_ICONST+1] = R_PUSH(122),
    [121][0] = 124,

    [122][0] = 122,
    [122][TB_NULL+1] = R_POP(2, 123),

    [123][TB_NULL+1] = R_POP(2, 123),

    [124][TB_NULL+1] = R_POP(2, 124),

    [125][TB_NULL+1] = 126,

    [126][0] = 127,

    [127][0] = 128,
    [127][TB_ICONST+1] = R_PUSH(129),

    [128][TB_NULL+1] = R_POP(2, 128),

    [129][0] = 129,
    [129][TB_NULL+1] = R_POP(2, 130),

    [130][TB_NULL+1] = R_POP(2, 130),

    [131][TB_NULL+1] = 132,

    [132][0] = 133,

    [133][0] = 134,
    [133][TB_ICONST+1] = R_PUSH(135),

    [134][TB_NULL+1] = R_POP(2, 134),

    [135][0] = 135,
    [135][TB_NULL+1] = R_POP(2, 136),

    [136][TB_NULL+1] = R_POP(2, 136),

    [137][TB_NULL+1] = 138,

    [138][0] = 139,

    [139][0] = 140,
    [139][TB_ICONST+1] = R_PUSH(141),

    [140][TB_NULL+1] = R_POP(2, 140),

    [141][0] = 141,
    [141][TB_NULL+1] = R_POP(2, 142),

    [142][TB_NULL+1] = R_POP(2, 142),

    [143][TB_NULL+1] = 144,

    [144][0] = 145,

    [145][0] = 146,
    [145][TB_ICONST+1] = R_PUSH(147),

    [146][TB_NULL+1] = R_POP(2, 146),

    [147][0] = 147,
    [147][TB_NULL+1] = R_POP(2, 148),

    [148][TB_NULL+1] = R_POP(2, 148),

    [149][TB_NULL+1] = 150,

    [150][0] = 151,

    [151][0] = 152,
    [151][TB_ICONST+1] = R_PUSH(153),

    [152][TB_NULL+1] = R_POP(2, 152),

    [153][0] = 153,
    [153][TB_NULL+1] = R_POP(2, 154),

    [154][TB_NULL+1] = R_POP(2, 154),

};

static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {
    switch (state) {
        case 4: {
            do {
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $index = 2 < n->input_count ? n->inputs[2] : NULL;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_lea, TB_TYPE_PTR, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->scale = 1;
                k0_extra->flags = OP_INDEXED;
            
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
                k0_extra->scale = as_int32($scale);
                k0_extra->disp = as_int32($disp);
                k0_extra->flags = OP_INDEXED;
            
                return k0;
            } while (0);
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
                k0_extra->scale = as_int32($scale);
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = as_int32($disp);
            
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
                k0_extra->scale = as_int32($scale);
                k0_extra->flags = OP_INDEXED;
            
                return k0;
            } while (0);
        } return NULL;
        case 28: {
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
        case 32: {
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
        case 34: {
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
        case 38: {
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
        case 40: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 48: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 49: {
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
        case 53: {
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
        case 55: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 63: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 64: {
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
        case 68: {
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
        case 70: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 78: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 79: {
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
        case 83: {
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
        case 85: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 93: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 94: {
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
        case 98: {
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
        case 100: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 108: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 109: {
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
        case 113: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 115: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 123: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
                TB_DataType $dt = n->dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->imm = as_int32($rhs);
                k0_extra->mode = MODE_LD;
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 124: {
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 5, sizeof(X86MemOp));
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
        case 128: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shl, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 140: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sar, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 146: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_rol, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 142: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_uint8($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sar, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 152: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ror, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 136: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_uint8($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shr, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 130: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_uint8($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shl, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 154: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_uint8($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ror, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        case 134: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shr, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 148: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(fits_into_uint8($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_rol, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                return k0;
            } while (0);
        } return NULL;
        // no match?
        default: return NULL;
    }
}
