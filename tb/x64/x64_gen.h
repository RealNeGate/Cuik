typedef enum X86NodeType {
    x86_xor = TB_MACH_X86 + 11,
    x86_or = TB_MACH_X86 + 8,
    x86_cmp = TB_MACH_X86 + 2,
    x86_sar = TB_MACH_X86 + 15,
    x86_test = TB_MACH_X86 + 3,
    x86_add = TB_MACH_X86 + 7,
    x86_mov = TB_MACH_X86 + 5,
    x86_and = TB_MACH_X86 + 9,
    x86_ror = TB_MACH_X86 + 17,
    x86_rol = TB_MACH_X86 + 16,
    x86_sub = TB_MACH_X86 + 10,
    x86_imul = TB_MACH_X86 + 12,
    x86_MEMORY = TB_MACH_X86 + 0,
    x86_jcc = TB_MACH_X86 + 4,
    x86_shl = TB_MACH_X86 + 13,
    x86_COND = TB_MACH_X86 + 1,
    x86_shr = TB_MACH_X86 + 14,
    x86_lea = TB_MACH_X86 + 6,
} X86NodeType;
static const char* node_name(int n_type) {
    switch (n_type) {
        case x86_xor: return "x86_xor";
        case x86_or: return "x86_or";
        case x86_cmp: return "x86_cmp";
        case x86_sar: return "x86_sar";
        case x86_test: return "x86_test";
        case x86_add: return "x86_add";
        case x86_mov: return "x86_mov";
        case x86_and: return "x86_and";
        case x86_ror: return "x86_ror";
        case x86_rol: return "x86_rol";
        case x86_sub: return "x86_sub";
        case x86_imul: return "x86_imul";
        case x86_MEMORY: return "x86_MEMORY";
        case x86_jcc: return "x86_jcc";
        case x86_shl: return "x86_shl";
        case x86_COND: return "x86_COND";
        case x86_shr: return "x86_shr";
        case x86_lea: return "x86_lea";
        default: return NULL;
    }
}

#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static bool x86_is_operand[512] = {
    [TB_CMP_NE] = true,
    [TB_CMP_EQ] = true,
    [TB_PTR_OFFSET] = true,
    [TB_CMP_ULE] = true,
    [TB_CMP_ULT] = true,
    [TB_CMP_SLE] = true,
    [TB_CMP_SLT] = true,
};
static uint32_t x86_grammar[196][512] = {
    [0][TB_BRANCH+1] = R_PUSH(64),
    [0][TB_CMP_ULE+1] = R_PUSH(40),
    [0][TB_ADD+1] = R_PUSH(76),
    [0][TB_LOAD+1] = R_PUSH(70),
    [0][TB_CMP_ULT+1] = R_PUSH(46),
    [0][TB_CMP_EQ+1] = R_PUSH(52),
    [0][TB_SHL+1] = R_PUSH(166),
    [0][TB_SAR+1] = R_PUSH(178),
    [0][TB_MUL+1] = R_PUSH(151),
    [0][TB_CMP_NE+1] = R_PUSH(58),
    [0][TB_ROR+1] = R_PUSH(190),
    [0][TB_PTR_OFFSET+1] = R_PUSH(1),
    [0][TB_AND+1] = R_PUSH(106),
    [0][TB_SHR+1] = R_PUSH(172),
    [0][TB_ROL+1] = R_PUSH(184),
    [0][TB_CMP_SLE+1] = R_PUSH(28),
    [0][TB_CMP_SLT+1] = R_PUSH(34),
    [0][x86_MEMORY+1] = R_PUSH(69),
    [0][TB_OR+1] = R_PUSH(91),
    [0][TB_SUB+1] = R_PUSH(121),
    [0][TB_XOR+1] = R_PUSH(136),

    [1][TB_NULL+1] = 2,

    [2][0] = 3,

    [3][0] = 4,
    [3][TB_SHL+1] = R_PUSH(22),
    [3][TB_ICONST+1] = R_PUSH(5),
    [3][TB_ADD+1] = R_PUSH(7),

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

    [28][TB_NULL+1] = 29,

    [29][0] = 30,

    [30][0] = 31,
    [30][TB_ICONST+1] = R_PUSH(32),

    [31][TB_NULL+1] = R_POP(2, 31),

    [32][0] = 32,
    [32][TB_NULL+1] = R_POP(2, 33),

    [33][TB_NULL+1] = R_POP(2, 33),

    [34][TB_NULL+1] = 35,

    [35][0] = 36,

    [36][0] = 37,
    [36][TB_ICONST+1] = R_PUSH(38),

    [37][TB_NULL+1] = R_POP(2, 37),

    [38][0] = 38,
    [38][TB_NULL+1] = R_POP(2, 39),

    [39][TB_NULL+1] = R_POP(2, 39),

    [40][TB_NULL+1] = 41,

    [41][0] = 42,

    [42][0] = 43,
    [42][TB_ICONST+1] = R_PUSH(44),

    [43][TB_NULL+1] = R_POP(2, 43),

    [44][0] = 44,
    [44][TB_NULL+1] = R_POP(2, 45),

    [45][TB_NULL+1] = R_POP(2, 45),

    [46][TB_NULL+1] = 47,

    [47][0] = 48,

    [48][0] = 49,
    [48][TB_ICONST+1] = R_PUSH(50),

    [49][TB_NULL+1] = R_POP(2, 49),

    [50][0] = 50,
    [50][TB_NULL+1] = R_POP(2, 51),

    [51][TB_NULL+1] = R_POP(2, 51),

    [52][TB_NULL+1] = 53,

    [53][0] = 54,

    [54][0] = 55,
    [54][TB_ICONST+1] = R_PUSH(56),

    [55][TB_NULL+1] = R_POP(2, 55),

    [56][0] = 56,
    [56][TB_NULL+1] = R_POP(2, 57),

    [57][TB_NULL+1] = R_POP(2, 57),

    [58][TB_NULL+1] = 59,

    [59][0] = 60,

    [60][0] = 61,
    [60][TB_ICONST+1] = R_PUSH(62),

    [61][TB_NULL+1] = R_POP(2, 61),

    [62][0] = 62,
    [62][TB_NULL+1] = R_POP(2, 63),

    [63][TB_NULL+1] = R_POP(2, 63),

    [64][0] = 65,

    [65][x86_COND+1] = R_PUSH(66),

    [66][0] = 67,

    [67][TB_NULL+1] = R_POP(2, 68),

    [68][TB_NULL+1] = R_POP(2, 68),

    [69][0] = 69,
    [69][TB_NULL+1] = R_POP(2, 69),

    [70][0] = 71,

    [71][0] = 72,

    [72][0] = 73,
    [72][x86_MEMORY+1] = R_PUSH(74),

    [73][TB_NULL+1] = R_POP(2, 73),

    [74][0] = 74,
    [74][TB_NULL+1] = R_POP(2, 75),

    [75][TB_NULL+1] = R_POP(2, 75),

    [76][TB_NULL+1] = 77,

    [77][0] = 78,
    [77][TB_LOAD+1] = R_PUSH(82),

    [78][0] = 79,
    [78][TB_ICONST+1] = R_PUSH(80),

    [79][TB_NULL+1] = R_POP(2, 79),

    [80][0] = 80,
    [80][TB_NULL+1] = R_POP(2, 81),

    [81][TB_NULL+1] = R_POP(2, 81),

    [82][0] = 83,

    [83][0] = 84,

    [84][x86_MEMORY+1] = R_PUSH(85),
    [84][0] = R_POP(2, 78),

    [85][0] = 85,
    [85][TB_NULL+1] = R_POP(2, 86),

    [86][TB_NULL+1] = R_POP(2, 87),
    [86][0] = R_POP(2, 78),

    [87][TB_ICONST+1] = R_PUSH(88),
    [87][0] = 90,

    [88][0] = 88,
    [88][TB_NULL+1] = R_POP(2, 89),

    [89][TB_NULL+1] = R_POP(2, 89),

    [90][TB_NULL+1] = R_POP(2, 90),

    [91][TB_NULL+1] = 92,

    [92][0] = 93,
    [92][TB_LOAD+1] = R_PUSH(97),

    [93][0] = 94,
    [93][TB_ICONST+1] = R_PUSH(95),

    [94][TB_NULL+1] = R_POP(2, 94),

    [95][0] = 95,
    [95][TB_NULL+1] = R_POP(2, 96),

    [96][TB_NULL+1] = R_POP(2, 96),

    [97][0] = 98,

    [98][0] = 99,

    [99][x86_MEMORY+1] = R_PUSH(100),
    [99][0] = R_POP(2, 93),

    [100][0] = 100,
    [100][TB_NULL+1] = R_POP(2, 101),

    [101][TB_NULL+1] = R_POP(2, 102),
    [101][0] = R_POP(2, 93),

    [102][TB_ICONST+1] = R_PUSH(103),
    [102][0] = 105,

    [103][0] = 103,
    [103][TB_NULL+1] = R_POP(2, 104),

    [104][TB_NULL+1] = R_POP(2, 104),

    [105][TB_NULL+1] = R_POP(2, 105),

    [106][TB_NULL+1] = 107,

    [107][0] = 108,
    [107][TB_LOAD+1] = R_PUSH(112),

    [108][0] = 109,
    [108][TB_ICONST+1] = R_PUSH(110),

    [109][TB_NULL+1] = R_POP(2, 109),

    [110][0] = 110,
    [110][TB_NULL+1] = R_POP(2, 111),

    [111][TB_NULL+1] = R_POP(2, 111),

    [112][0] = 113,

    [113][0] = 114,

    [114][x86_MEMORY+1] = R_PUSH(115),
    [114][0] = R_POP(2, 108),

    [115][0] = 115,
    [115][TB_NULL+1] = R_POP(2, 116),

    [116][TB_NULL+1] = R_POP(2, 117),
    [116][0] = R_POP(2, 108),

    [117][TB_ICONST+1] = R_PUSH(118),
    [117][0] = 120,

    [118][0] = 118,
    [118][TB_NULL+1] = R_POP(2, 119),

    [119][TB_NULL+1] = R_POP(2, 119),

    [120][TB_NULL+1] = R_POP(2, 120),

    [121][TB_NULL+1] = 122,

    [122][0] = 123,
    [122][TB_LOAD+1] = R_PUSH(127),

    [123][0] = 124,
    [123][TB_ICONST+1] = R_PUSH(125),

    [124][TB_NULL+1] = R_POP(2, 124),

    [125][0] = 125,
    [125][TB_NULL+1] = R_POP(2, 126),

    [126][TB_NULL+1] = R_POP(2, 126),

    [127][0] = 128,

    [128][0] = 129,

    [129][x86_MEMORY+1] = R_PUSH(130),
    [129][0] = R_POP(2, 123),

    [130][0] = 130,
    [130][TB_NULL+1] = R_POP(2, 131),

    [131][TB_NULL+1] = R_POP(2, 132),
    [131][0] = R_POP(2, 123),

    [132][TB_ICONST+1] = R_PUSH(133),
    [132][0] = 135,

    [133][0] = 133,
    [133][TB_NULL+1] = R_POP(2, 134),

    [134][TB_NULL+1] = R_POP(2, 134),

    [135][TB_NULL+1] = R_POP(2, 135),

    [136][TB_NULL+1] = 137,

    [137][0] = 138,
    [137][TB_LOAD+1] = R_PUSH(142),

    [138][0] = 139,
    [138][TB_ICONST+1] = R_PUSH(140),

    [139][TB_NULL+1] = R_POP(2, 139),

    [140][0] = 140,
    [140][TB_NULL+1] = R_POP(2, 141),

    [141][TB_NULL+1] = R_POP(2, 141),

    [142][0] = 143,

    [143][0] = 144,

    [144][x86_MEMORY+1] = R_PUSH(145),
    [144][0] = R_POP(2, 138),

    [145][0] = 145,
    [145][TB_NULL+1] = R_POP(2, 146),

    [146][TB_NULL+1] = R_POP(2, 147),
    [146][0] = R_POP(2, 138),

    [147][TB_ICONST+1] = R_PUSH(148),
    [147][0] = 150,

    [148][0] = 148,
    [148][TB_NULL+1] = R_POP(2, 149),

    [149][TB_NULL+1] = R_POP(2, 149),

    [150][TB_NULL+1] = R_POP(2, 150),

    [151][TB_NULL+1] = 152,

    [152][0] = 153,
    [152][TB_LOAD+1] = R_PUSH(157),

    [153][0] = 154,
    [153][TB_ICONST+1] = R_PUSH(155),

    [154][TB_NULL+1] = R_POP(2, 154),

    [155][0] = 155,
    [155][TB_NULL+1] = R_POP(2, 156),

    [156][TB_NULL+1] = R_POP(2, 156),

    [157][0] = 158,

    [158][0] = 159,

    [159][x86_MEMORY+1] = R_PUSH(160),
    [159][0] = R_POP(2, 153),

    [160][0] = 160,
    [160][TB_NULL+1] = R_POP(2, 161),

    [161][TB_NULL+1] = R_POP(2, 162),
    [161][0] = R_POP(2, 153),

    [162][TB_ICONST+1] = R_PUSH(163),
    [162][0] = 165,

    [163][0] = 163,
    [163][TB_NULL+1] = R_POP(2, 164),

    [164][TB_NULL+1] = R_POP(2, 164),

    [165][TB_NULL+1] = R_POP(2, 165),

    [166][TB_NULL+1] = 167,

    [167][0] = 168,

    [168][0] = 169,
    [168][TB_ICONST+1] = R_PUSH(170),

    [169][TB_NULL+1] = R_POP(2, 169),

    [170][0] = 170,
    [170][TB_NULL+1] = R_POP(2, 171),

    [171][TB_NULL+1] = R_POP(2, 171),

    [172][TB_NULL+1] = 173,

    [173][0] = 174,

    [174][0] = 175,
    [174][TB_ICONST+1] = R_PUSH(176),

    [175][TB_NULL+1] = R_POP(2, 175),

    [176][0] = 176,
    [176][TB_NULL+1] = R_POP(2, 177),

    [177][TB_NULL+1] = R_POP(2, 177),

    [178][TB_NULL+1] = 179,

    [179][0] = 180,

    [180][0] = 181,
    [180][TB_ICONST+1] = R_PUSH(182),

    [181][TB_NULL+1] = R_POP(2, 181),

    [182][0] = 182,
    [182][TB_NULL+1] = R_POP(2, 183),

    [183][TB_NULL+1] = R_POP(2, 183),

    [184][TB_NULL+1] = 185,

    [185][0] = 186,

    [186][0] = 187,
    [186][TB_ICONST+1] = R_PUSH(188),

    [187][TB_NULL+1] = R_POP(2, 187),

    [188][0] = 188,
    [188][TB_NULL+1] = R_POP(2, 189),

    [189][TB_NULL+1] = R_POP(2, 189),

    [190][TB_NULL+1] = 191,

    [191][0] = 192,

    [192][0] = 193,
    [192][TB_ICONST+1] = R_PUSH(194),

    [193][TB_NULL+1] = R_POP(2, 193),

    [194][0] = 194,
    [194][TB_NULL+1] = R_POP(2, 195),

    [195][TB_NULL+1] = R_POP(2, 195),

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
                k0_extra->flags = OP_INDEXED;
                k0_extra->scale = 1;
            
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
                k0_extra->disp = as_int32($disp);
                k0_extra->scale = as_int32($scale);
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
                k0_extra->disp = as_int32($disp);
                k0_extra->scale = as_int32($scale);
                k0_extra->flags = OP_INDEXED;
            
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
        case 31: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 33: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = LE;
            
                return k1;
            } while (0);
        } return NULL;
        case 37: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 39: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = L;
            
                return k1;
            } while (0);
        } return NULL;
        case 43: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 45: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = BE;
            
                return k1;
            } while (0);
        } return NULL;
        case 49: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 51: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = B;
            
                return k1;
            } while (0);
        } return NULL;
        case 55: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 57: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
        } return NULL;
        case 61: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $rhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 63: {
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(fits_into_int32($dt, $rhs))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->imm = as_int32($rhs);
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
        } return NULL;
        case 68: {
            do {
                TB_Node* $cmp = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                int $cond = TB_NODE_GET_EXTRA_T(n->inputs[1], X86MemOp)->cond;
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_jcc, TB_TYPE_TUPLE, 2, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $cmp, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->cond = $cond;
            
                return k0;
            } while (0);
        } return NULL;
        case 69: {
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
        case 75: {
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
        case 79: {
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
        case 81: {
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
        case 89: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 90: {
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
        case 94: {
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
        case 96: {
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
        case 104: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 105: {
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
        case 109: {
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
        case 111: {
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
        case 119: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 120: {
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
        case 124: {
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
        case 126: {
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
        case 187: {
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
        case 154: {
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
        case 134: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 135: {
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
        case 183: {
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
        case 165: {
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
        case 164: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 181: {
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
        case 195: {
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
        case 141: {
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
        case 193: {
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
        case 177: {
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
        case 175: {
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
        case 169: {
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
        case 156: {
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
        case 139: {
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
        case 149: {
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
                k0_extra->mode = MODE_LD;
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = $flags | OP_IMMEDIATE;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 150: {
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
        case 189: {
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
        case 171: {
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
        // no match?
        default: return NULL;
    }
}

static void global_init(void) {
     // transitions: 309
}
