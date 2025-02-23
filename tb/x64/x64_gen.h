typedef enum X86NodeType {
    x86_MEMORY = TB_MACH_X86 + 0,
    x86_COND = TB_MACH_X86 + 1,
    x86_cmp = TB_MACH_X86 + 2,
    x86_test = TB_MACH_X86 + 3,
    x86_ucomi = TB_MACH_X86 + 4,
    x86_jcc = TB_MACH_X86 + 5,
    x86_setcc = TB_MACH_X86 + 6,
    x86_call = TB_MACH_X86 + 7,
    x86_mov = TB_MACH_X86 + 8,
    x86_lea = TB_MACH_X86 + 9,
    x86_add = TB_MACH_X86 + 10,
    x86_or = TB_MACH_X86 + 11,
    x86_and = TB_MACH_X86 + 12,
    x86_sub = TB_MACH_X86 + 13,
    x86_xor = TB_MACH_X86 + 14,
    x86_imul = TB_MACH_X86 + 15,
    x86_shl = TB_MACH_X86 + 16,
    x86_shr = TB_MACH_X86 + 17,
    x86_sar = TB_MACH_X86 + 18,
    x86_rol = TB_MACH_X86 + 19,
    x86_ror = TB_MACH_X86 + 20,
    x86_vmov = TB_MACH_X86 + 21,
    x86_vzero = TB_MACH_X86 + 22,
    x86_vadd = TB_MACH_X86 + 23,
    x86_vsub = TB_MACH_X86 + 24,
    x86_vmul = TB_MACH_X86 + 25,
    x86_vdiv = TB_MACH_X86 + 26,
    x86_movsx8 = TB_MACH_X86 + 27,
    x86_movzx8 = TB_MACH_X86 + 28,
    x86_movsx16 = TB_MACH_X86 + 29,
    x86_movzx16 = TB_MACH_X86 + 30,
    x86_movsx32 = TB_MACH_X86 + 31,
} X86NodeType;
static const char* node_name(int n_type) {
    switch (n_type) {
        case x86_MEMORY: return "x86_MEMORY";
        case x86_COND: return "x86_COND";
        case x86_cmp: return "x86_cmp";
        case x86_test: return "x86_test";
        case x86_ucomi: return "x86_ucomi";
        case x86_jcc: return "x86_jcc";
        case x86_setcc: return "x86_setcc";
        case x86_call: return "x86_call";
        case x86_mov: return "x86_mov";
        case x86_lea: return "x86_lea";
        case x86_add: return "x86_add";
        case x86_or: return "x86_or";
        case x86_and: return "x86_and";
        case x86_sub: return "x86_sub";
        case x86_xor: return "x86_xor";
        case x86_imul: return "x86_imul";
        case x86_shl: return "x86_shl";
        case x86_shr: return "x86_shr";
        case x86_sar: return "x86_sar";
        case x86_rol: return "x86_rol";
        case x86_ror: return "x86_ror";
        case x86_vmov: return "x86_vmov";
        case x86_vzero: return "x86_vzero";
        case x86_vadd: return "x86_vadd";
        case x86_vsub: return "x86_vsub";
        case x86_vmul: return "x86_vmul";
        case x86_vdiv: return "x86_vdiv";
        case x86_movsx8: return "x86_movsx8";
        case x86_movzx8: return "x86_movzx8";
        case x86_movsx16: return "x86_movsx16";
        case x86_movzx16: return "x86_movzx16";
        case x86_movsx32: return "x86_movsx32";
        default: return NULL;
    }
}

static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {
    switch (state) {
        case 4: {
            do {
                TB_Node* $base = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $index = 2 < n->input_count ? n->inputs[2] : NULL;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 2, sizeof(X86MemOp));
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
                k0_extra->flags = OP_INDEXED;
                k0_extra->scale = as_int32($scale);
            
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
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = as_int32($disp);
                k0_extra->scale = as_int32($scale);
            
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
        case 29: {
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
                int $disp = TB_NODE_GET_EXTRA_T(n, TB_NodeLocal)->stack_pos;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k0, ctx->frame_ptr, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = $disp;
            
                return k0;
            } while (0);
        } return NULL;
        case 31: {
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Symbol* $sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_VOID, 1, sizeof(TB_NodeMachSymbol));
                set_input(f, k0, $root, k0_i++);
                TB_NodeMachSymbol* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->sym = $sym;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
            
                return k1;
            } while (0);
        } return NULL;
        case 36: {
            do {
                TB_Node* $root = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Symbol* $sym = TB_NODE_GET_EXTRA_T(n->inputs[1], TB_NodeSymbol)->sym;
                TB_Node* $disp = n->inputs[2];
                if (!(fits_into_int32(TB_TYPE_I64, $disp))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_VOID, 1, sizeof(TB_NodeMachSymbol));
                set_input(f, k0, $root, k0_i++);
                TB_NodeMachSymbol* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->sym = $sym;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->disp = as_int32($disp);
            
                return k1;
            } while (0);
        } return NULL;
        case 40: {
            do {
                TB_Node* $base = 2 < n->input_count ? n->inputs[2] : NULL;
                int $disp = TB_NODE_GET_EXTRA_T(n, X86MemOp)->disp;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->disp = $disp;
            
                return k0;
            } while (0);
        } return NULL;
        case 41: {
            do {
                TB_Node* $base = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_Node* $index = 3 < n->input_count ? n->inputs[3] : NULL;
                int $flags = TB_NODE_GET_EXTRA_T(n, X86MemOp)->flags;
                int $scale = TB_NODE_GET_EXTRA_T(n, X86MemOp)->scale;
                int $disp = TB_NODE_GET_EXTRA_T(n, X86MemOp)->disp;
                if (!($flags==OP_INDEXED)) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 2, sizeof(X86MemOp));
                set_input(f, k0, $base, k0_i++);
                set_input(f, k0, $index, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = $disp;
                k0_extra->scale = $scale;
            
                return k0;
            } while (0);
        } return NULL;
        case 45: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = LE;
            
                return k1;
            } while (0);
        } return NULL;
        case 47: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = LE;
            
                return k1;
            } while (0);
        } return NULL;
        case 51: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = L;
            
                return k1;
            } while (0);
        } return NULL;
        case 53: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = L;
            
                return k1;
            } while (0);
        } return NULL;
        case 57: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = BE;
            
                return k1;
            } while (0);
        } return NULL;
        case 59: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = BE;
            
                return k1;
            } while (0);
        } return NULL;
        case 63: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = B;
            
                return k1;
            } while (0);
        } return NULL;
        case 65: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = B;
            
                return k1;
            } while (0);
        } return NULL;
        case 69: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_IS_FLOAT_TYPE($dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ucomi, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
        } return NULL;
        case 71: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_IS_INT_OR_PTR($dt) && TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0)) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_test, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
        } return NULL;
        case 75: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_cmp, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_IS_FLOAT_TYPE($dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ucomi, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
        } return NULL;
        case 77: {
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
                k0_extra->extra_dt = $dt;
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_IS_INT_OR_PTR($dt) && TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0)) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_test, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
        } return NULL;
        case 81: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ucomi, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = B;
            
                return k1;
            } while (0);
        } return NULL;
        case 85: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ucomi, TB_TYPE_I64, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = $dt;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_I8, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = BE;
            
                return k1;
            } while (0);
        } return NULL;
        case 90: {
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
        case 95: {
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
        case 97: {
            do {
                TB_Node* $cmp = 0 < n->input_count ? n->inputs[0] : NULL;
                int $cond = TB_NODE_GET_EXTRA_T(n, X86MemOp)->cond;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_setcc, TB_TYPE_I8, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $cmp, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->cond = $cond;
            
                return k0;
            } while (0);
        } return NULL;
        case 104: {
            do {
                TB_Node* $sym = n->inputs[2]->inputs[0];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                size_t $REST_LEN = n->input_count - 3;
                TB_Node** $REST  = &n->inputs[3];
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_call, TB_TYPE_TUPLE, 3 + $REST_LEN, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $sym, k0_i++);
                FOR_N(i, 0, $REST_LEN) { set_input(f, k0, $REST[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 105: {
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $target = 2 < n->input_count ? n->inputs[2] : NULL;
                size_t $REST_LEN = n->input_count - 3;
                TB_Node** $REST  = &n->inputs[3];
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_call, TB_TYPE_TUPLE, 3 + $REST_LEN, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $target, k0_i++);
                FOR_N(i, 0, $REST_LEN) { set_input(f, k0, $REST[i], k0_i++); }
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->mode = MODE_LD;
            
                return k0;
            } while (0);
        } return NULL;
        case 106: {
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
        case 110: {
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
                if (!(TB_IS_INT_OR_PTR($dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, $dt, 3, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $addr, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->mode = MODE_LD;
            
                return k0;
            } while (0);
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
                if (!(TB_IS_FLOAT_TYPE($dt) || TB_IS_VECTOR_TYPE($dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vmov, $dt, 3, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $addr, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->mode = MODE_LD;
            
                return k0;
            } while (0);
        } return NULL;
        case 112: {
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(TB_IS_INT_OR_PTR($dt))) {
                    break;
                }
            
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
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
                if (!(TB_IS_FLOAT_TYPE($dt) || TB_IS_VECTOR_TYPE($dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vmov, $dt, 4, sizeof(X86MemOp));
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
        case 117: {
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
                if (!(TB_IS_INT_OR_PTR(n->inputs[3]->dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, TB_TYPE_MEMORY, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $addr, k0_i++);
                set_input(f, k0, $val, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = n->inputs[3]->dt;
                k0_extra->mode = MODE_ST;
            
                return k0;
            } while (0);
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
                if (!(TB_IS_FLOAT_TYPE(n->inputs[3]->dt) || TB_IS_VECTOR_TYPE(n->inputs[3]->dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vmov, TB_TYPE_MEMORY, 4, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                set_input(f, k0, $addr, k0_i++);
                set_input(f, k0, $val, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->extra_dt = n->inputs[3]->dt;
                k0_extra->mode = MODE_ST;
            
                return k0;
            } while (0);
        } return NULL;
        case 120: {
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
                if (!(TB_IS_INT_OR_PTR(n->inputs[3]->dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, TB_TYPE_MEMORY, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $val, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->extra_dt = n->inputs[3]->dt;
                k0_extra->mode = MODE_ST;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
                if (!(TB_IS_FLOAT_TYPE(n->inputs[3]->dt) || TB_IS_VECTOR_TYPE(n->inputs[3]->dt))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vmov, TB_TYPE_MEMORY, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $lhs->input_count) { set_input(f, k0, $lhs->inputs[i], k0_i++); }
                set_input(f, k0, $val, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $lhs->extra, sizeof(X86MemOp));
                k0_extra->extra_dt = n->inputs[3]->dt;
                k0_extra->mode = MODE_ST;
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
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 130: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 132: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_add, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 136: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 142: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 144: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_or, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 148: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 154: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
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
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_and, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 160: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 166: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 168: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sub, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 172: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 178: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 180: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_xor, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 184: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 190: {
            do {
                TB_Node* $rhs = n->inputs[2]->inputs[2];
                TB_Node* $ctrl = 0 < n->inputs[2]->input_count ? n->inputs[2]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[2]->input_count ? n->inputs[2]->inputs[1] : NULL;
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 5, sizeof(X86MemOp));
                set_input(f, k0, $ctrl, k0_i++);
                set_input(f, k0, $mem, k0_i++);
                FOR_N(i, 0, $rhs->input_count) { set_input(f, k0, $rhs->inputs[i], k0_i++); }
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
                memcpy(k0_extra, $rhs->extra, sizeof(X86MemOp));
                k0_extra->mode = MODE_LD;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 192: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_imul, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 196: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shl, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 198: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shl, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 202: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shr, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 204: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_shr, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 208: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sar, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 210: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_sar, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 214: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_rol, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 216: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_rol, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 220: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ror, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 222: {
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
                k0_extra->flags = OP_IMMEDIATE;
                k0_extra->imm = as_int32($rhs);
            
                return k0;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_ror, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 224: {
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
                if (!(is_float32_zero(n))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vzero, TB_TYPE_F32, 1, sizeof(X86MemOp));
                set_input(f, k0, $root, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
            do {
                TB_Node* $root = 0 < n->input_count ? n->inputs[0] : NULL;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, TB_MACH_SYMBOL, TB_TYPE_VOID, 1, sizeof(TB_NodeMachSymbol));
                set_input(f, k0, $root, k0_i++);
                TB_NodeMachSymbol* k0_extra = TB_NODE_GET_EXTRA(k0);
                k0_extra->sym = gimme_float32_sym(ctx, n);
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_vmov, TB_TYPE_F32, 3, sizeof(X86MemOp));
                k1_i++;
                k1_i++;
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->mode = MODE_LD;
            
                return k1;
            } while (0);
        } return NULL;
        case 229: {
            do {
                TB_Node* $x = n->inputs[1];
                TB_Node* $root = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_DataType $dt = n->dt;
                if (!(is_float32_zero($x))) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vzero, $dt, 1, sizeof(X86MemOp));
                set_input(f, k0, $root, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 233: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vadd, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 237: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vsub, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 241: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vmul, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 245: {
            do {
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $rhs = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_DataType $dt = n->dt;
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_vdiv, $dt, 4, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $rhs, k0_i++);
                set_input(f, k0, $lhs, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        case 248: {
            do {
                TB_DataType $dt = n->dt;
                TB_Node* $src = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!($dt.type == TB_TAG_I64 && n->inputs[1]->dt.type == TB_TAG_I32)) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_mov, TB_TYPE_I64, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $src, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
            do {
                TB_DataType $dt = n->dt;
                TB_Node* $src = 1 < n->input_count ? n->inputs[1] : NULL;
                if (!(($dt.type == TB_TAG_I32 || $dt.type == TB_TAG_I64) && n->inputs[1]->dt.type == TB_TAG_I8)) {
                    break;
                }
            
                size_t k0_i = 0;
                TB_Node* k0 = tb_alloc_node(f, x86_movzx8, $dt, 3, sizeof(X86MemOp));
                k0_i++;
                k0_i++;
                set_input(f, k0, $src, k0_i++);
                X86MemOp* k0_extra = TB_NODE_GET_EXTRA(k0);
            
                return k0;
            } while (0);
        } return NULL;
        // no match?
        default: return NULL;
    }
}

static bool x86_is_operand[512] = {
    [TB_SYMBOL] = true,
    [TB_LOCAL] = true,
    [TB_CMP_ULE] = true,
    [x86_lea] = true,
    [TB_PTR_OFFSET] = true,
    [TB_CMP_ULT] = true,
    [TB_CMP_SLE] = true,
    [TB_CMP_SLT] = true,
    [TB_CMP_FLE] = true,
    [TB_CMP_FLT] = true,
    [TB_CMP_EQ] = true,
    [TB_CMP_NE] = true,
};
#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static void global_init(void) {
    static const uint32_t edges[] = {
        (0)<<16 | (TB_SUB+1), R_PUSH(157),
        (0)<<16 | (TB_CMP_ULE+1), R_PUSH(54),
        (0)<<16 | (TB_CMP_FLE+1), R_PUSH(82),
        (0)<<16 | (TB_CMP_ULT+1), R_PUSH(60),
        (0)<<16 | (TB_CMP_EQ+1), R_PUSH(66),
        (0)<<16 | (TB_VBROADCAST+1), R_PUSH(225),
        (0)<<16 | (x86_lea+1), R_PUSH(37),
        (0)<<16 | (TB_PTR_OFFSET+1), R_PUSH(1),
        (0)<<16 | (TB_STORE+1), R_PUSH(113),
        (0)<<16 | (TB_MUL+1), R_PUSH(181),
        (0)<<16 | (TB_BRANCH+1), R_PUSH(86),
        (0)<<16 | (x86_COND+1), R_PUSH(96),
        (0)<<16 | (TB_FADD+1), R_PUSH(230),
        (0)<<16 | (TB_FSUB+1), R_PUSH(234),
        (0)<<16 | (TB_LOCAL+1), R_PUSH(28),
        (0)<<16 | (TB_AFFINE_LATCH+1), R_PUSH(91),
        (0)<<16 | (TB_SHR+1), R_PUSH(199),
        (0)<<16 | (TB_FDIV+1), R_PUSH(242),
        (0)<<16 | (TB_CMP_SLE+1), R_PUSH(42),
        (0)<<16 | (TB_SAR+1), R_PUSH(205),
        (0)<<16 | (TB_CALL+1), R_PUSH(98),
        (0)<<16 | (x86_MEMORY+1), R_PUSH(106),
        (0)<<16 | (TB_ROL+1), R_PUSH(211),
        (0)<<16 | (TB_ROR+1), R_PUSH(217),
        (0)<<16 | (TB_OR+1), R_PUSH(133),
        (0)<<16 | (TB_ZERO_EXT+1), R_PUSH(246),
        (0)<<16 | (TB_AND+1), R_PUSH(145),
        (0)<<16 | (TB_SHL+1), R_PUSH(193),
        (0)<<16 | (TB_SYMBOL+1), R_PUSH(30),
        (0)<<16 | (TB_FMUL+1), R_PUSH(238),
        (0)<<16 | (TB_CMP_NE+1), R_PUSH(72),
        (0)<<16 | (TB_ADD+1), R_PUSH(121),
        (0)<<16 | (TB_XOR+1), R_PUSH(169),
        (0)<<16 | (TB_LOAD+1), R_PUSH(107),
        (0)<<16 | (TB_F32CONST+1), R_PUSH(223),
        (0)<<16 | (TB_CMP_SLT+1), R_PUSH(48),
        (0)<<16 | (TB_CMP_FLT+1), R_PUSH(78),
        (1)<<16 | (TB_NULL+1), 2,
        (2)<<16 | (0), 3,
        (2)<<16 | (TB_SYMBOL+1), R_PUSH(32),
        (3)<<16 | (0), 4,
        (3)<<16 | (TB_ICONST+1), R_PUSH(5),
        (3)<<16 | (TB_SHL+1), R_PUSH(22),
        (3)<<16 | (TB_ADD+1), R_PUSH(7),
        (4)<<16 | (TB_NULL+1), R_POP(2, 4),
        (5)<<16 | (0), 5,
        (5)<<16 | (TB_NULL+1), R_POP(2, 6),
        (6)<<16 | (TB_NULL+1), R_POP(2, 6),
        (7)<<16 | (TB_NULL+1), 8,
        (7)<<16 | (0), R_POP(2, 4),
        (8)<<16 | (0), 9,
        (8)<<16 | (TB_SHL+1), R_PUSH(13),
        (9)<<16 | (0), R_POP(2, 4),
        (9)<<16 | (TB_ICONST+1), R_PUSH(10),
        (10)<<16 | (0), 10,
        (10)<<16 | (TB_NULL+1), R_POP(2, 11),
        (11)<<16 | (TB_NULL+1), R_POP(2, 12),
        (11)<<16 | (0), R_POP(2, 4),
        (12)<<16 | (TB_NULL+1), R_POP(2, 12),
        (13)<<16 | (TB_NULL+1), 14,
        (13)<<16 | (0), R_POP(3, 4),
        (14)<<16 | (0), 15,
        (15)<<16 | (0), R_POP(3, 4),
        (15)<<16 | (TB_ICONST+1), R_PUSH(16),
        (16)<<16 | (0), 16,
        (16)<<16 | (TB_NULL+1), R_POP(2, 17),
        (17)<<16 | (TB_NULL+1), R_POP(2, 18),
        (17)<<16 | (0), R_POP(3, 4),
        (18)<<16 | (0), R_POP(2, 4),
        (18)<<16 | (TB_ICONST+1), R_PUSH(19),
        (19)<<16 | (0), 19,
        (19)<<16 | (TB_NULL+1), R_POP(2, 20),
        (20)<<16 | (TB_NULL+1), R_POP(2, 21),
        (20)<<16 | (0), R_POP(2, 4),
        (21)<<16 | (TB_NULL+1), R_POP(2, 21),
        (22)<<16 | (TB_NULL+1), 23,
        (22)<<16 | (0), R_POP(2, 4),
        (23)<<16 | (0), 24,
        (24)<<16 | (0), R_POP(2, 4),
        (24)<<16 | (TB_ICONST+1), R_PUSH(25),
        (25)<<16 | (0), 25,
        (25)<<16 | (TB_NULL+1), R_POP(2, 26),
        (26)<<16 | (TB_NULL+1), R_POP(2, 27),
        (26)<<16 | (0), R_POP(2, 4),
        (27)<<16 | (TB_NULL+1), R_POP(2, 27),
        (28)<<16 | (0), 29,
        (29)<<16 | (TB_NULL+1), R_POP(2, 29),
        (30)<<16 | (0), 31,
        (31)<<16 | (TB_NULL+1), R_POP(2, 31),
        (32)<<16 | (0), 33,
        (33)<<16 | (TB_NULL+1), R_POP(2, 34),
        (33)<<16 | (0), R_POP(2, 3),
        (34)<<16 | (TB_ICONST+1), R_PUSH(35),
        (35)<<16 | (0), 35,
        (35)<<16 | (TB_NULL+1), R_POP(2, 36),
        (36)<<16 | (TB_NULL+1), R_POP(2, 36),
        (37)<<16 | (TB_NULL+1), 38,
        (38)<<16 | (TB_NULL+1), 39,
        (39)<<16 | (0), 40,
        (40)<<16 | (TB_NULL+1), R_POP(2, 40),
        (40)<<16 | (0), 41,
        (41)<<16 | (TB_NULL+1), R_POP(2, 41),
        (42)<<16 | (TB_NULL+1), 43,
        (43)<<16 | (0), 44,
        (44)<<16 | (0), 45,
        (44)<<16 | (TB_ICONST+1), R_PUSH(46),
        (45)<<16 | (TB_NULL+1), R_POP(2, 45),
        (46)<<16 | (0), 46,
        (46)<<16 | (TB_NULL+1), R_POP(2, 47),
        (47)<<16 | (TB_NULL+1), R_POP(2, 47),
        (48)<<16 | (TB_NULL+1), 49,
        (49)<<16 | (0), 50,
        (50)<<16 | (0), 51,
        (50)<<16 | (TB_ICONST+1), R_PUSH(52),
        (51)<<16 | (TB_NULL+1), R_POP(2, 51),
        (52)<<16 | (0), 52,
        (52)<<16 | (TB_NULL+1), R_POP(2, 53),
        (53)<<16 | (TB_NULL+1), R_POP(2, 53),
        (54)<<16 | (TB_NULL+1), 55,
        (55)<<16 | (0), 56,
        (56)<<16 | (0), 57,
        (56)<<16 | (TB_ICONST+1), R_PUSH(58),
        (57)<<16 | (TB_NULL+1), R_POP(2, 57),
        (58)<<16 | (0), 58,
        (58)<<16 | (TB_NULL+1), R_POP(2, 59),
        (59)<<16 | (TB_NULL+1), R_POP(2, 59),
        (60)<<16 | (TB_NULL+1), 61,
        (61)<<16 | (0), 62,
        (62)<<16 | (0), 63,
        (62)<<16 | (TB_ICONST+1), R_PUSH(64),
        (63)<<16 | (TB_NULL+1), R_POP(2, 63),
        (64)<<16 | (0), 64,
        (64)<<16 | (TB_NULL+1), R_POP(2, 65),
        (65)<<16 | (TB_NULL+1), R_POP(2, 65),
        (66)<<16 | (TB_NULL+1), 67,
        (67)<<16 | (0), 68,
        (68)<<16 | (0), 69,
        (68)<<16 | (TB_ICONST+1), R_PUSH(70),
        (69)<<16 | (TB_NULL+1), R_POP(2, 69),
        (70)<<16 | (0), 70,
        (70)<<16 | (TB_NULL+1), R_POP(2, 71),
        (71)<<16 | (TB_NULL+1), R_POP(2, 71),
        (72)<<16 | (TB_NULL+1), 73,
        (73)<<16 | (0), 74,
        (74)<<16 | (0), 75,
        (74)<<16 | (TB_ICONST+1), R_PUSH(76),
        (75)<<16 | (TB_NULL+1), R_POP(2, 75),
        (76)<<16 | (0), 76,
        (76)<<16 | (TB_NULL+1), R_POP(2, 77),
        (77)<<16 | (TB_NULL+1), R_POP(2, 77),
        (78)<<16 | (TB_NULL+1), 79,
        (79)<<16 | (0), 80,
        (80)<<16 | (0), 81,
        (81)<<16 | (TB_NULL+1), R_POP(2, 81),
        (82)<<16 | (TB_NULL+1), 83,
        (83)<<16 | (0), 84,
        (84)<<16 | (0), 85,
        (85)<<16 | (TB_NULL+1), R_POP(2, 85),
        (86)<<16 | (0), 87,
        (87)<<16 | (x86_COND+1), R_PUSH(88),
        (88)<<16 | (0), 89,
        (89)<<16 | (TB_NULL+1), R_POP(2, 90),
        (90)<<16 | (TB_NULL+1), R_POP(2, 90),
        (91)<<16 | (0), 92,
        (92)<<16 | (x86_COND+1), R_PUSH(93),
        (93)<<16 | (0), 94,
        (94)<<16 | (TB_NULL+1), R_POP(2, 95),
        (95)<<16 | (TB_NULL+1), R_POP(2, 95),
        (96)<<16 | (0), 97,
        (97)<<16 | (TB_NULL+1), R_POP(2, 97),
        (98)<<16 | (0), 99,
        (99)<<16 | (0), 100,
        (100)<<16 | (x86_MEMORY+1), R_PUSH(101),
        (100)<<16 | (0), 105,
        (101)<<16 | (0), R_POP(2, 105),
        (101)<<16 | (TB_MACH_SYMBOL+1), R_PUSH(102),
        (102)<<16 | (0), 102,
        (102)<<16 | (TB_NULL+1), R_POP(2, 103),
        (103)<<16 | (TB_NULL+1), R_POP(2, 104),
        (103)<<16 | (0), R_POP(2, 105),
        (104)<<16 | (0), 104,
        (104)<<16 | (TB_NULL+1), R_POP(2, 104),
        (105)<<16 | (0), 105,
        (105)<<16 | (TB_NULL+1), R_POP(2, 105),
        (106)<<16 | (0), 106,
        (106)<<16 | (TB_NULL+1), R_POP(2, 106),
        (107)<<16 | (0), 108,
        (108)<<16 | (0), 109,
        (109)<<16 | (0), 110,
        (109)<<16 | (x86_MEMORY+1), R_PUSH(111),
        (110)<<16 | (TB_NULL+1), R_POP(2, 110),
        (111)<<16 | (0), 111,
        (111)<<16 | (TB_NULL+1), R_POP(2, 112),
        (112)<<16 | (TB_NULL+1), R_POP(2, 112),
        (113)<<16 | (0), 114,
        (114)<<16 | (0), 115,
        (115)<<16 | (0), 116,
        (115)<<16 | (x86_MEMORY+1), R_PUSH(118),
        (116)<<16 | (0), 117,
        (117)<<16 | (TB_NULL+1), R_POP(2, 117),
        (118)<<16 | (0), 118,
        (118)<<16 | (TB_NULL+1), R_POP(2, 119),
        (119)<<16 | (0), 120,
        (120)<<16 | (TB_NULL+1), R_POP(2, 120),
        (121)<<16 | (TB_NULL+1), 122,
        (122)<<16 | (0), 123,
        (123)<<16 | (0), 124,
        (123)<<16 | (TB_ICONST+1), R_PUSH(131),
        (123)<<16 | (TB_LOAD+1), R_PUSH(125),
        (124)<<16 | (TB_NULL+1), R_POP(2, 124),
        (125)<<16 | (0), 126,
        (126)<<16 | (0), 127,
        (127)<<16 | (x86_MEMORY+1), R_PUSH(128),
        (127)<<16 | (0), R_POP(2, 124),
        (128)<<16 | (0), 128,
        (128)<<16 | (TB_NULL+1), R_POP(2, 129),
        (129)<<16 | (TB_NULL+1), R_POP(2, 130),
        (129)<<16 | (0), R_POP(2, 124),
        (130)<<16 | (TB_NULL+1), R_POP(2, 130),
        (131)<<16 | (0), 131,
        (131)<<16 | (TB_NULL+1), R_POP(2, 132),
        (132)<<16 | (TB_NULL+1), R_POP(2, 132),
        (133)<<16 | (TB_NULL+1), 134,
        (134)<<16 | (0), 135,
        (135)<<16 | (0), 136,
        (135)<<16 | (TB_ICONST+1), R_PUSH(143),
        (135)<<16 | (TB_LOAD+1), R_PUSH(137),
        (136)<<16 | (TB_NULL+1), R_POP(2, 136),
        (137)<<16 | (0), 138,
        (138)<<16 | (0), 139,
        (139)<<16 | (x86_MEMORY+1), R_PUSH(140),
        (139)<<16 | (0), R_POP(2, 136),
        (140)<<16 | (0), 140,
        (140)<<16 | (TB_NULL+1), R_POP(2, 141),
        (141)<<16 | (TB_NULL+1), R_POP(2, 142),
        (141)<<16 | (0), R_POP(2, 136),
        (142)<<16 | (TB_NULL+1), R_POP(2, 142),
        (143)<<16 | (0), 143,
        (143)<<16 | (TB_NULL+1), R_POP(2, 144),
        (144)<<16 | (TB_NULL+1), R_POP(2, 144),
        (145)<<16 | (TB_NULL+1), 146,
        (146)<<16 | (0), 147,
        (147)<<16 | (0), 148,
        (147)<<16 | (TB_ICONST+1), R_PUSH(155),
        (147)<<16 | (TB_LOAD+1), R_PUSH(149),
        (148)<<16 | (TB_NULL+1), R_POP(2, 148),
        (149)<<16 | (0), 150,
        (150)<<16 | (0), 151,
        (151)<<16 | (x86_MEMORY+1), R_PUSH(152),
        (151)<<16 | (0), R_POP(2, 148),
        (152)<<16 | (0), 152,
        (152)<<16 | (TB_NULL+1), R_POP(2, 153),
        (153)<<16 | (TB_NULL+1), R_POP(2, 154),
        (153)<<16 | (0), R_POP(2, 148),
        (154)<<16 | (TB_NULL+1), R_POP(2, 154),
        (155)<<16 | (0), 155,
        (155)<<16 | (TB_NULL+1), R_POP(2, 156),
        (156)<<16 | (TB_NULL+1), R_POP(2, 156),
        (157)<<16 | (TB_NULL+1), 158,
        (158)<<16 | (0), 159,
        (159)<<16 | (0), 160,
        (159)<<16 | (TB_ICONST+1), R_PUSH(167),
        (159)<<16 | (TB_LOAD+1), R_PUSH(161),
        (160)<<16 | (TB_NULL+1), R_POP(2, 160),
        (161)<<16 | (0), 162,
        (162)<<16 | (0), 163,
        (163)<<16 | (x86_MEMORY+1), R_PUSH(164),
        (163)<<16 | (0), R_POP(2, 160),
        (164)<<16 | (0), 164,
        (164)<<16 | (TB_NULL+1), R_POP(2, 165),
        (165)<<16 | (TB_NULL+1), R_POP(2, 166),
        (165)<<16 | (0), R_POP(2, 160),
        (166)<<16 | (TB_NULL+1), R_POP(2, 166),
        (167)<<16 | (0), 167,
        (167)<<16 | (TB_NULL+1), R_POP(2, 168),
        (168)<<16 | (TB_NULL+1), R_POP(2, 168),
        (169)<<16 | (TB_NULL+1), 170,
        (170)<<16 | (0), 171,
        (171)<<16 | (0), 172,
        (171)<<16 | (TB_ICONST+1), R_PUSH(179),
        (171)<<16 | (TB_LOAD+1), R_PUSH(173),
        (172)<<16 | (TB_NULL+1), R_POP(2, 172),
        (173)<<16 | (0), 174,
        (174)<<16 | (0), 175,
        (175)<<16 | (x86_MEMORY+1), R_PUSH(176),
        (175)<<16 | (0), R_POP(2, 172),
        (176)<<16 | (0), 176,
        (176)<<16 | (TB_NULL+1), R_POP(2, 177),
        (177)<<16 | (TB_NULL+1), R_POP(2, 178),
        (177)<<16 | (0), R_POP(2, 172),
        (178)<<16 | (TB_NULL+1), R_POP(2, 178),
        (179)<<16 | (0), 179,
        (179)<<16 | (TB_NULL+1), R_POP(2, 180),
        (180)<<16 | (TB_NULL+1), R_POP(2, 180),
        (181)<<16 | (TB_NULL+1), 182,
        (182)<<16 | (0), 183,
        (183)<<16 | (0), 184,
        (183)<<16 | (TB_ICONST+1), R_PUSH(191),
        (183)<<16 | (TB_LOAD+1), R_PUSH(185),
        (184)<<16 | (TB_NULL+1), R_POP(2, 184),
        (185)<<16 | (0), 186,
        (186)<<16 | (0), 187,
        (187)<<16 | (x86_MEMORY+1), R_PUSH(188),
        (187)<<16 | (0), R_POP(2, 184),
        (188)<<16 | (0), 188,
        (188)<<16 | (TB_NULL+1), R_POP(2, 189),
        (189)<<16 | (TB_NULL+1), R_POP(2, 190),
        (189)<<16 | (0), R_POP(2, 184),
        (190)<<16 | (TB_NULL+1), R_POP(2, 190),
        (191)<<16 | (0), 191,
        (191)<<16 | (TB_NULL+1), R_POP(2, 192),
        (192)<<16 | (TB_NULL+1), R_POP(2, 192),
        (193)<<16 | (TB_NULL+1), 194,
        (194)<<16 | (0), 195,
        (195)<<16 | (0), 196,
        (195)<<16 | (TB_ICONST+1), R_PUSH(197),
        (196)<<16 | (TB_NULL+1), R_POP(2, 196),
        (197)<<16 | (0), 197,
        (197)<<16 | (TB_NULL+1), R_POP(2, 198),
        (198)<<16 | (TB_NULL+1), R_POP(2, 198),
        (199)<<16 | (TB_NULL+1), 200,
        (200)<<16 | (0), 201,
        (201)<<16 | (0), 202,
        (201)<<16 | (TB_ICONST+1), R_PUSH(203),
        (202)<<16 | (TB_NULL+1), R_POP(2, 202),
        (203)<<16 | (0), 203,
        (203)<<16 | (TB_NULL+1), R_POP(2, 204),
        (204)<<16 | (TB_NULL+1), R_POP(2, 204),
        (205)<<16 | (TB_NULL+1), 206,
        (206)<<16 | (0), 207,
        (207)<<16 | (0), 208,
        (207)<<16 | (TB_ICONST+1), R_PUSH(209),
        (208)<<16 | (TB_NULL+1), R_POP(2, 208),
        (209)<<16 | (0), 209,
        (209)<<16 | (TB_NULL+1), R_POP(2, 210),
        (210)<<16 | (TB_NULL+1), R_POP(2, 210),
        (211)<<16 | (TB_NULL+1), 212,
        (212)<<16 | (0), 213,
        (213)<<16 | (0), 214,
        (213)<<16 | (TB_ICONST+1), R_PUSH(215),
        (214)<<16 | (TB_NULL+1), R_POP(2, 214),
        (215)<<16 | (0), 215,
        (215)<<16 | (TB_NULL+1), R_POP(2, 216),
        (216)<<16 | (TB_NULL+1), R_POP(2, 216),
        (217)<<16 | (TB_NULL+1), 218,
        (218)<<16 | (0), 219,
        (219)<<16 | (0), 220,
        (219)<<16 | (TB_ICONST+1), R_PUSH(221),
        (220)<<16 | (TB_NULL+1), R_POP(2, 220),
        (221)<<16 | (0), 221,
        (221)<<16 | (TB_NULL+1), R_POP(2, 222),
        (222)<<16 | (TB_NULL+1), R_POP(2, 222),
        (223)<<16 | (0), 224,
        (224)<<16 | (TB_NULL+1), R_POP(2, 224),
        (225)<<16 | (TB_NULL+1), 226,
        (226)<<16 | (TB_F32CONST+1), R_PUSH(227),
        (227)<<16 | (0), 228,
        (228)<<16 | (TB_NULL+1), R_POP(2, 229),
        (229)<<16 | (TB_NULL+1), R_POP(2, 229),
        (230)<<16 | (TB_NULL+1), 231,
        (231)<<16 | (0), 232,
        (232)<<16 | (0), 233,
        (233)<<16 | (TB_NULL+1), R_POP(2, 233),
        (234)<<16 | (TB_NULL+1), 235,
        (235)<<16 | (0), 236,
        (236)<<16 | (0), 237,
        (237)<<16 | (TB_NULL+1), R_POP(2, 237),
        (238)<<16 | (TB_NULL+1), 239,
        (239)<<16 | (0), 240,
        (240)<<16 | (0), 241,
        (241)<<16 | (TB_NULL+1), R_POP(2, 241),
        (242)<<16 | (TB_NULL+1), 243,
        (243)<<16 | (0), 244,
        (244)<<16 | (0), 245,
        (245)<<16 | (TB_NULL+1), R_POP(2, 245),
        (246)<<16 | (TB_NULL+1), 247,
        (247)<<16 | (0), 248,
        (248)<<16 | (TB_NULL+1), R_POP(2, 248),
    };
    // transitions: 378
    size_t count = sizeof(edges) / (2*sizeof(uint32_t));
    node_grammar_alloc(count);
    FOR_N(i, 0, count) {
        node_grammar_put(edges[i*2], edges[i*2 + 1]);
    }
}
