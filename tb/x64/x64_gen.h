typedef enum X86NodeType {
    x86_cmp = TB_MACH_X86 + 2,
    x86_MEMORY = TB_MACH_X86 + 0,
    x86_and = TB_MACH_X86 + 9,
    x86_shl = TB_MACH_X86 + 13,
    x86_sar = TB_MACH_X86 + 15,
    x86_jcc = TB_MACH_X86 + 4,
    x86_shr = TB_MACH_X86 + 14,
    x86_rol = TB_MACH_X86 + 16,
    x86_sub = TB_MACH_X86 + 10,
    x86_movsx32 = TB_MACH_X86 + 22,
    x86_ror = TB_MACH_X86 + 17,
    x86_movzx16 = TB_MACH_X86 + 21,
    x86_COND = TB_MACH_X86 + 1,
    x86_movzx8 = TB_MACH_X86 + 19,
    x86_mov = TB_MACH_X86 + 5,
    x86_xor = TB_MACH_X86 + 11,
    x86_movsx8 = TB_MACH_X86 + 18,
    x86_test = TB_MACH_X86 + 3,
    x86_or = TB_MACH_X86 + 8,
    x86_lea = TB_MACH_X86 + 6,
    x86_movsx16 = TB_MACH_X86 + 20,
    x86_add = TB_MACH_X86 + 7,
    x86_imul = TB_MACH_X86 + 12,
} X86NodeType;
static const char* node_name(int n_type) {
    switch (n_type) {
        case x86_cmp: return "x86_cmp";
        case x86_MEMORY: return "x86_MEMORY";
        case x86_and: return "x86_and";
        case x86_shl: return "x86_shl";
        case x86_sar: return "x86_sar";
        case x86_jcc: return "x86_jcc";
        case x86_shr: return "x86_shr";
        case x86_rol: return "x86_rol";
        case x86_sub: return "x86_sub";
        case x86_movsx32: return "x86_movsx32";
        case x86_ror: return "x86_ror";
        case x86_movzx16: return "x86_movzx16";
        case x86_COND: return "x86_COND";
        case x86_movzx8: return "x86_movzx8";
        case x86_mov: return "x86_mov";
        case x86_xor: return "x86_xor";
        case x86_movsx8: return "x86_movsx8";
        case x86_test: return "x86_test";
        case x86_or: return "x86_or";
        case x86_lea: return "x86_lea";
        case x86_movsx16: return "x86_movsx16";
        case x86_add: return "x86_add";
        case x86_imul: return "x86_imul";
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
                k0_extra->disp = as_int32($disp);
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
        case 29: {
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
        case 34: {
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
        case 38: {
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
        case 40: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = LE;
            
                return k1;
            } while (0);
        } return NULL;
        case 44: {
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
        case 46: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = L;
            
                return k1;
            } while (0);
        } return NULL;
        case 50: {
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
        case 52: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = BE;
            
                return k1;
            } while (0);
        } return NULL;
        case 56: {
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
        case 58: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = B;
            
                return k1;
            } while (0);
        } return NULL;
        case 62: {
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
        case 64: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0)) {
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
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = E;
            
                return k1;
            } while (0);
        } return NULL;
        case 68: {
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
        case 70: {
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
                k0_extra->imm = as_int32($rhs);
                k0_extra->flags = OP_IMMEDIATE;
            
                size_t k1_i = 0;
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
            do {
                TB_Node* $rhs = n->inputs[2];
                TB_Node* $lhs = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_DataType $dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
                if (!(TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0)) {
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
                TB_Node* k1 = tb_alloc_node(f, x86_COND, TB_TYPE_VOID, 1, sizeof(X86MemOp));
                set_input(f, k1, k0, k1_i++);
                X86MemOp* k1_extra = TB_NODE_GET_EXTRA(k1);
                k1_extra->cond = NE;
            
                return k1;
            } while (0);
        } return NULL;
        case 75: {
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
        case 80: {
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
        case 81: {
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
        case 85: {
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
        case 87: {
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
        case 92: {
            do {
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $addr = 2 < n->input_count ? n->inputs[2] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
            
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
        } return NULL;
        case 95: {
            do {
                TB_Node* $lhs = n->inputs[2];
                TB_Node* $ctrl = 0 < n->input_count ? n->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->input_count ? n->inputs[1] : NULL;
                TB_Node* $val = 3 < n->input_count ? n->inputs[3] : NULL;
            
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
        } return NULL;
        case 99: {
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
        case 101: {
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
        case 109: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 113: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 117: {
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
        case 119: {
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
        case 127: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 203: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 155: {
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
        case 219: {
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
        case 153: {
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
        case 185: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 167: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 199: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 231: {
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
        case 137: {
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
        case 215: {
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
        case 149: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 181: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 163: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 227: {
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
        case 236: {
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
        } return NULL;
        case 135: {
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
        case 225: {
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
        case 145: {
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
            do {
                TB_Node* $lhs = n->inputs[1]->inputs[2];
                int $flags = TB_NODE_GET_EXTRA_T(n->inputs[1]->inputs[2], X86MemOp)->flags;
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $rhs = n->inputs[2];
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
                k0_extra->flags = $flags;
                k0->input_count = k0_i;
            
                return k0;
            } while (0);
        } return NULL;
        case 213: {
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
        case 221: {
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
        case 207: {
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
        case 209: {
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
        case 191: {
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
        case 173: {
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
        case 131: {
            do {
                TB_Node* $rhs = n->inputs[1]->inputs[3];
                TB_Node* $ctrl = 0 < n->inputs[1]->input_count ? n->inputs[1]->inputs[0] : NULL;
                TB_Node* $mem = 1 < n->inputs[1]->input_count ? n->inputs[1]->inputs[1] : NULL;
                TB_Node* $lhs = 2 < n->inputs[1]->input_count ? n->inputs[1]->inputs[2] : NULL;
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
        case 233: {
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
        case 189: {
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
        case 171: {
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
        // no match?
        default: return NULL;
    }
}

static bool x86_is_operand[512] = {
    [TB_CMP_NE] = true,
    [TB_SYMBOL] = true,
    [TB_CMP_ULT] = true,
    [TB_CMP_SLE] = true,
    [TB_CMP_SLT] = true,
    [TB_PTR_OFFSET] = true,
    [TB_CMP_ULE] = true,
    [TB_CMP_EQ] = true,
};
#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static void global_init(void) {
    static const uint32_t edges[] = {
        (0)<<16 | (TB_ADD+1), R_PUSH(96),
        (0)<<16 | (TB_CMP_EQ+1), R_PUSH(59),
        (0)<<16 | (TB_SHR+1), R_PUSH(210),
        (0)<<16 | (TB_ZERO_EXT+1), R_PUSH(234),
        (0)<<16 | (TB_CMP_NE+1), R_PUSH(65),
        (0)<<16 | (TB_PTR_OFFSET+1), R_PUSH(1),
        (0)<<16 | (TB_AND+1), R_PUSH(132),
        (0)<<16 | (TB_SHL+1), R_PUSH(204),
        (0)<<16 | (TB_SAR+1), R_PUSH(216),
        (0)<<16 | (TB_CMP_ULE+1), R_PUSH(47),
        (0)<<16 | (TB_SYMBOL+1), R_PUSH(28),
        (0)<<16 | (TB_LOAD+1), R_PUSH(82),
        (0)<<16 | (TB_ROL+1), R_PUSH(222),
        (0)<<16 | (TB_BRANCH+1), R_PUSH(71),
        (0)<<16 | (TB_SUB+1), R_PUSH(150),
        (0)<<16 | (TB_ROR+1), R_PUSH(228),
        (0)<<16 | (TB_MUL+1), R_PUSH(186),
        (0)<<16 | (TB_STORE+1), R_PUSH(88),
        (0)<<16 | (TB_CMP_SLE+1), R_PUSH(35),
        (0)<<16 | (TB_XOR+1), R_PUSH(168),
        (0)<<16 | (TB_CMP_ULT+1), R_PUSH(53),
        (0)<<16 | (TB_AFFINE_LATCH+1), R_PUSH(76),
        (0)<<16 | (TB_OR+1), R_PUSH(114),
        (0)<<16 | (x86_MEMORY+1), R_PUSH(81),
        (0)<<16 | (TB_CMP_SLT+1), R_PUSH(41),
        (1)<<16 | (TB_NULL+1), 2,
        (2)<<16 | (0), 3,
        (2)<<16 | (TB_SYMBOL+1), R_PUSH(30),
        (3)<<16 | (0), 4,
        (3)<<16 | (TB_SHL+1), R_PUSH(22),
        (3)<<16 | (TB_ADD+1), R_PUSH(7),
        (3)<<16 | (TB_ICONST+1), R_PUSH(5),
        (4)<<16 | (TB_NULL+1), R_POP(2, 4),
        (5)<<16 | (0), 5,
        (5)<<16 | (TB_NULL+1), R_POP(2, 6),
        (6)<<16 | (TB_NULL+1), R_POP(2, 6),
        (7)<<16 | (0), R_POP(2, 4),
        (7)<<16 | (TB_NULL+1), 8,
        (8)<<16 | (0), 9,
        (8)<<16 | (TB_SHL+1), R_PUSH(13),
        (9)<<16 | (0), R_POP(2, 4),
        (9)<<16 | (TB_ICONST+1), R_PUSH(10),
        (10)<<16 | (0), R_POP(3, 4),
        (10)<<16 | (TB_NULL+1), R_POP(2, 11),
        (11)<<16 | (0), R_POP(2, 4),
        (11)<<16 | (TB_NULL+1), R_POP(2, 12),
        (12)<<16 | (TB_NULL+1), R_POP(2, 12),
        (13)<<16 | (0), R_POP(3, 4),
        (13)<<16 | (TB_NULL+1), 14,
        (14)<<16 | (0), 15,
        (15)<<16 | (0), R_POP(3, 4),
        (15)<<16 | (TB_ICONST+1), R_PUSH(16),
        (16)<<16 | (0), R_POP(4, 4),
        (16)<<16 | (TB_NULL+1), R_POP(2, 17),
        (17)<<16 | (0), R_POP(3, 4),
        (17)<<16 | (TB_NULL+1), R_POP(2, 18),
        (18)<<16 | (0), R_POP(2, 4),
        (18)<<16 | (TB_ICONST+1), R_PUSH(19),
        (19)<<16 | (0), R_POP(3, 4),
        (19)<<16 | (TB_NULL+1), R_POP(2, 20),
        (20)<<16 | (0), R_POP(2, 4),
        (20)<<16 | (TB_NULL+1), R_POP(2, 21),
        (21)<<16 | (TB_NULL+1), R_POP(2, 21),
        (22)<<16 | (0), R_POP(2, 4),
        (22)<<16 | (TB_NULL+1), 23,
        (23)<<16 | (0), 24,
        (24)<<16 | (0), R_POP(2, 4),
        (24)<<16 | (TB_ICONST+1), R_PUSH(25),
        (25)<<16 | (0), R_POP(3, 4),
        (25)<<16 | (TB_NULL+1), R_POP(2, 26),
        (26)<<16 | (0), R_POP(2, 4),
        (26)<<16 | (TB_NULL+1), R_POP(2, 27),
        (27)<<16 | (TB_NULL+1), R_POP(2, 27),
        (28)<<16 | (0), 29,
        (29)<<16 | (TB_NULL+1), R_POP(2, 29),
        (30)<<16 | (0), 31,
        (31)<<16 | (0), R_POP(2, 3),
        (31)<<16 | (TB_NULL+1), R_POP(2, 32),
        (32)<<16 | (TB_ICONST+1), R_PUSH(33),
        (33)<<16 | (0), 33,
        (33)<<16 | (TB_NULL+1), R_POP(2, 34),
        (34)<<16 | (TB_NULL+1), R_POP(2, 34),
        (35)<<16 | (TB_NULL+1), 36,
        (36)<<16 | (0), 37,
        (37)<<16 | (0), 38,
        (37)<<16 | (TB_ICONST+1), R_PUSH(39),
        (38)<<16 | (TB_NULL+1), R_POP(2, 38),
        (39)<<16 | (0), 39,
        (39)<<16 | (TB_NULL+1), R_POP(2, 40),
        (40)<<16 | (TB_NULL+1), R_POP(2, 40),
        (41)<<16 | (TB_NULL+1), 42,
        (42)<<16 | (0), 43,
        (43)<<16 | (0), 44,
        (43)<<16 | (TB_ICONST+1), R_PUSH(45),
        (44)<<16 | (TB_NULL+1), R_POP(2, 44),
        (45)<<16 | (0), 45,
        (45)<<16 | (TB_NULL+1), R_POP(2, 46),
        (46)<<16 | (TB_NULL+1), R_POP(2, 46),
        (47)<<16 | (TB_NULL+1), 48,
        (48)<<16 | (0), 49,
        (49)<<16 | (0), 50,
        (49)<<16 | (TB_ICONST+1), R_PUSH(51),
        (50)<<16 | (TB_NULL+1), R_POP(2, 50),
        (51)<<16 | (0), 51,
        (51)<<16 | (TB_NULL+1), R_POP(2, 52),
        (52)<<16 | (TB_NULL+1), R_POP(2, 52),
        (53)<<16 | (TB_NULL+1), 54,
        (54)<<16 | (0), 55,
        (55)<<16 | (0), 56,
        (55)<<16 | (TB_ICONST+1), R_PUSH(57),
        (56)<<16 | (TB_NULL+1), R_POP(2, 56),
        (57)<<16 | (0), 57,
        (57)<<16 | (TB_NULL+1), R_POP(2, 58),
        (58)<<16 | (TB_NULL+1), R_POP(2, 58),
        (59)<<16 | (TB_NULL+1), 60,
        (60)<<16 | (0), 61,
        (61)<<16 | (0), 62,
        (61)<<16 | (TB_ICONST+1), R_PUSH(63),
        (62)<<16 | (TB_NULL+1), R_POP(2, 62),
        (63)<<16 | (0), 63,
        (63)<<16 | (TB_NULL+1), R_POP(2, 64),
        (64)<<16 | (TB_NULL+1), R_POP(2, 64),
        (65)<<16 | (TB_NULL+1), 66,
        (66)<<16 | (0), 67,
        (67)<<16 | (0), 68,
        (67)<<16 | (TB_ICONST+1), R_PUSH(69),
        (68)<<16 | (TB_NULL+1), R_POP(2, 68),
        (69)<<16 | (0), 69,
        (69)<<16 | (TB_NULL+1), R_POP(2, 70),
        (70)<<16 | (TB_NULL+1), R_POP(2, 70),
        (71)<<16 | (0), 72,
        (72)<<16 | (x86_COND+1), R_PUSH(73),
        (73)<<16 | (0), 74,
        (74)<<16 | (TB_NULL+1), R_POP(2, 75),
        (75)<<16 | (TB_NULL+1), R_POP(2, 75),
        (76)<<16 | (0), 77,
        (77)<<16 | (x86_COND+1), R_PUSH(78),
        (78)<<16 | (0), 79,
        (79)<<16 | (TB_NULL+1), R_POP(2, 80),
        (80)<<16 | (TB_NULL+1), R_POP(2, 80),
        (81)<<16 | (0), 81,
        (81)<<16 | (TB_NULL+1), R_POP(2, 81),
        (82)<<16 | (0), 83,
        (83)<<16 | (0), 84,
        (84)<<16 | (0), 85,
        (84)<<16 | (x86_MEMORY+1), R_PUSH(86),
        (85)<<16 | (TB_NULL+1), R_POP(2, 85),
        (86)<<16 | (0), 86,
        (86)<<16 | (TB_NULL+1), R_POP(2, 87),
        (87)<<16 | (TB_NULL+1), R_POP(2, 87),
        (88)<<16 | (0), 89,
        (89)<<16 | (0), 90,
        (90)<<16 | (0), 91,
        (90)<<16 | (x86_MEMORY+1), R_PUSH(93),
        (91)<<16 | (0), 92,
        (92)<<16 | (TB_NULL+1), R_POP(2, 92),
        (93)<<16 | (0), 93,
        (93)<<16 | (TB_NULL+1), R_POP(2, 94),
        (94)<<16 | (0), 95,
        (95)<<16 | (TB_NULL+1), R_POP(2, 95),
        (96)<<16 | (TB_NULL+1), 97,
        (97)<<16 | (0), 98,
        (97)<<16 | (TB_LOAD+1), R_PUSH(102),
        (98)<<16 | (0), 99,
        (98)<<16 | (TB_ICONST+1), R_PUSH(100),
        (99)<<16 | (TB_NULL+1), R_POP(2, 99),
        (100)<<16 | (0), 100,
        (100)<<16 | (TB_NULL+1), R_POP(2, 101),
        (101)<<16 | (TB_NULL+1), R_POP(2, 101),
        (102)<<16 | (0), 103,
        (103)<<16 | (0), 104,
        (104)<<16 | (x86_MEMORY+1), R_PUSH(105),
        (104)<<16 | (0), 110,
        (105)<<16 | (0), R_POP(3, 98),
        (105)<<16 | (TB_NULL+1), R_POP(2, 106),
        (106)<<16 | (0), R_POP(2, 98),
        (106)<<16 | (TB_NULL+1), R_POP(2, 107),
        (107)<<16 | (TB_ICONST+1), R_PUSH(108),
        (108)<<16 | (0), 108,
        (108)<<16 | (TB_NULL+1), R_POP(2, 109),
        (109)<<16 | (TB_NULL+1), R_POP(2, 109),
        (110)<<16 | (x86_MEMORY+1), R_PUSH(111),
        (110)<<16 | (0), R_POP(2, 98),
        (111)<<16 | (0), R_POP(3, 98),
        (111)<<16 | (TB_NULL+1), R_POP(2, 112),
        (112)<<16 | (0), R_POP(2, 98),
        (112)<<16 | (TB_NULL+1), R_POP(2, 113),
        (113)<<16 | (TB_NULL+1), R_POP(2, 113),
        (114)<<16 | (TB_NULL+1), 115,
        (115)<<16 | (0), 116,
        (115)<<16 | (TB_LOAD+1), R_PUSH(120),
        (116)<<16 | (0), 117,
        (116)<<16 | (TB_ICONST+1), R_PUSH(118),
        (117)<<16 | (TB_NULL+1), R_POP(2, 117),
        (118)<<16 | (0), 118,
        (118)<<16 | (TB_NULL+1), R_POP(2, 119),
        (119)<<16 | (TB_NULL+1), R_POP(2, 119),
        (120)<<16 | (0), 121,
        (121)<<16 | (0), 122,
        (122)<<16 | (x86_MEMORY+1), R_PUSH(123),
        (122)<<16 | (0), 128,
        (123)<<16 | (0), R_POP(3, 116),
        (123)<<16 | (TB_NULL+1), R_POP(2, 124),
        (124)<<16 | (0), R_POP(2, 116),
        (124)<<16 | (TB_NULL+1), R_POP(2, 125),
        (125)<<16 | (TB_ICONST+1), R_PUSH(126),
        (126)<<16 | (0), 126,
        (126)<<16 | (TB_NULL+1), R_POP(2, 127),
        (127)<<16 | (TB_NULL+1), R_POP(2, 127),
        (128)<<16 | (x86_MEMORY+1), R_PUSH(129),
        (128)<<16 | (0), R_POP(2, 116),
        (129)<<16 | (0), R_POP(3, 116),
        (129)<<16 | (TB_NULL+1), R_POP(2, 130),
        (130)<<16 | (0), R_POP(2, 116),
        (130)<<16 | (TB_NULL+1), R_POP(2, 131),
        (131)<<16 | (TB_NULL+1), R_POP(2, 131),
        (132)<<16 | (TB_NULL+1), 133,
        (133)<<16 | (0), 134,
        (133)<<16 | (TB_LOAD+1), R_PUSH(138),
        (134)<<16 | (0), 135,
        (134)<<16 | (TB_ICONST+1), R_PUSH(136),
        (135)<<16 | (TB_NULL+1), R_POP(2, 135),
        (136)<<16 | (0), 136,
        (136)<<16 | (TB_NULL+1), R_POP(2, 137),
        (137)<<16 | (TB_NULL+1), R_POP(2, 137),
        (138)<<16 | (0), 139,
        (139)<<16 | (0), 140,
        (140)<<16 | (x86_MEMORY+1), R_PUSH(141),
        (140)<<16 | (0), 146,
        (141)<<16 | (0), R_POP(3, 134),
        (141)<<16 | (TB_NULL+1), R_POP(2, 142),
        (142)<<16 | (0), R_POP(2, 134),
        (142)<<16 | (TB_NULL+1), R_POP(2, 143),
        (143)<<16 | (TB_ICONST+1), R_PUSH(144),
        (144)<<16 | (0), 144,
        (144)<<16 | (TB_NULL+1), R_POP(2, 145),
        (145)<<16 | (TB_NULL+1), R_POP(2, 145),
        (146)<<16 | (x86_MEMORY+1), R_PUSH(147),
        (146)<<16 | (0), R_POP(2, 134),
        (147)<<16 | (0), R_POP(3, 134),
        (147)<<16 | (TB_NULL+1), R_POP(2, 148),
        (148)<<16 | (0), R_POP(2, 134),
        (148)<<16 | (TB_NULL+1), R_POP(2, 149),
        (149)<<16 | (TB_NULL+1), R_POP(2, 149),
        (150)<<16 | (TB_NULL+1), 151,
        (151)<<16 | (0), 152,
        (151)<<16 | (TB_LOAD+1), R_PUSH(156),
        (152)<<16 | (0), 153,
        (152)<<16 | (TB_ICONST+1), R_PUSH(154),
        (153)<<16 | (TB_NULL+1), R_POP(2, 153),
        (154)<<16 | (0), 154,
        (154)<<16 | (TB_NULL+1), R_POP(2, 155),
        (155)<<16 | (TB_NULL+1), R_POP(2, 155),
        (156)<<16 | (0), 157,
        (157)<<16 | (0), 158,
        (158)<<16 | (x86_MEMORY+1), R_PUSH(159),
        (158)<<16 | (0), 164,
        (159)<<16 | (0), R_POP(3, 152),
        (159)<<16 | (TB_NULL+1), R_POP(2, 160),
        (160)<<16 | (0), R_POP(2, 152),
        (160)<<16 | (TB_NULL+1), R_POP(2, 161),
        (161)<<16 | (TB_ICONST+1), R_PUSH(162),
        (162)<<16 | (0), 162,
        (162)<<16 | (TB_NULL+1), R_POP(2, 163),
        (163)<<16 | (TB_NULL+1), R_POP(2, 163),
        (164)<<16 | (x86_MEMORY+1), R_PUSH(165),
        (164)<<16 | (0), R_POP(2, 152),
        (165)<<16 | (0), R_POP(3, 152),
        (165)<<16 | (TB_NULL+1), R_POP(2, 166),
        (166)<<16 | (0), R_POP(2, 152),
        (166)<<16 | (TB_NULL+1), R_POP(2, 167),
        (167)<<16 | (TB_NULL+1), R_POP(2, 167),
        (168)<<16 | (TB_NULL+1), 169,
        (169)<<16 | (0), 170,
        (169)<<16 | (TB_LOAD+1), R_PUSH(174),
        (170)<<16 | (0), 171,
        (170)<<16 | (TB_ICONST+1), R_PUSH(172),
        (171)<<16 | (TB_NULL+1), R_POP(2, 171),
        (172)<<16 | (0), 172,
        (172)<<16 | (TB_NULL+1), R_POP(2, 173),
        (173)<<16 | (TB_NULL+1), R_POP(2, 173),
        (174)<<16 | (0), 175,
        (175)<<16 | (0), 176,
        (176)<<16 | (x86_MEMORY+1), R_PUSH(177),
        (176)<<16 | (0), 182,
        (177)<<16 | (0), R_POP(3, 170),
        (177)<<16 | (TB_NULL+1), R_POP(2, 178),
        (178)<<16 | (0), R_POP(2, 170),
        (178)<<16 | (TB_NULL+1), R_POP(2, 179),
        (179)<<16 | (TB_ICONST+1), R_PUSH(180),
        (180)<<16 | (0), 180,
        (180)<<16 | (TB_NULL+1), R_POP(2, 181),
        (181)<<16 | (TB_NULL+1), R_POP(2, 181),
        (182)<<16 | (x86_MEMORY+1), R_PUSH(183),
        (182)<<16 | (0), R_POP(2, 170),
        (183)<<16 | (0), R_POP(3, 170),
        (183)<<16 | (TB_NULL+1), R_POP(2, 184),
        (184)<<16 | (0), R_POP(2, 170),
        (184)<<16 | (TB_NULL+1), R_POP(2, 185),
        (185)<<16 | (TB_NULL+1), R_POP(2, 185),
        (186)<<16 | (TB_NULL+1), 187,
        (187)<<16 | (0), 188,
        (187)<<16 | (TB_LOAD+1), R_PUSH(192),
        (188)<<16 | (0), 189,
        (188)<<16 | (TB_ICONST+1), R_PUSH(190),
        (189)<<16 | (TB_NULL+1), R_POP(2, 189),
        (190)<<16 | (0), 190,
        (190)<<16 | (TB_NULL+1), R_POP(2, 191),
        (191)<<16 | (TB_NULL+1), R_POP(2, 191),
        (192)<<16 | (0), 193,
        (193)<<16 | (0), 194,
        (194)<<16 | (x86_MEMORY+1), R_PUSH(195),
        (194)<<16 | (0), 200,
        (195)<<16 | (0), R_POP(3, 188),
        (195)<<16 | (TB_NULL+1), R_POP(2, 196),
        (196)<<16 | (0), R_POP(2, 188),
        (196)<<16 | (TB_NULL+1), R_POP(2, 197),
        (197)<<16 | (TB_ICONST+1), R_PUSH(198),
        (198)<<16 | (0), 198,
        (198)<<16 | (TB_NULL+1), R_POP(2, 199),
        (199)<<16 | (TB_NULL+1), R_POP(2, 199),
        (200)<<16 | (x86_MEMORY+1), R_PUSH(201),
        (200)<<16 | (0), R_POP(2, 188),
        (201)<<16 | (0), R_POP(3, 188),
        (201)<<16 | (TB_NULL+1), R_POP(2, 202),
        (202)<<16 | (0), R_POP(2, 188),
        (202)<<16 | (TB_NULL+1), R_POP(2, 203),
        (203)<<16 | (TB_NULL+1), R_POP(2, 203),
        (204)<<16 | (TB_NULL+1), 205,
        (205)<<16 | (0), 206,
        (206)<<16 | (0), 207,
        (206)<<16 | (TB_ICONST+1), R_PUSH(208),
        (207)<<16 | (TB_NULL+1), R_POP(2, 207),
        (208)<<16 | (0), 208,
        (208)<<16 | (TB_NULL+1), R_POP(2, 209),
        (209)<<16 | (TB_NULL+1), R_POP(2, 209),
        (210)<<16 | (TB_NULL+1), 211,
        (211)<<16 | (0), 212,
        (212)<<16 | (0), 213,
        (212)<<16 | (TB_ICONST+1), R_PUSH(214),
        (213)<<16 | (TB_NULL+1), R_POP(2, 213),
        (214)<<16 | (0), 214,
        (214)<<16 | (TB_NULL+1), R_POP(2, 215),
        (215)<<16 | (TB_NULL+1), R_POP(2, 215),
        (216)<<16 | (TB_NULL+1), 217,
        (217)<<16 | (0), 218,
        (218)<<16 | (0), 219,
        (218)<<16 | (TB_ICONST+1), R_PUSH(220),
        (219)<<16 | (TB_NULL+1), R_POP(2, 219),
        (220)<<16 | (0), 220,
        (220)<<16 | (TB_NULL+1), R_POP(2, 221),
        (221)<<16 | (TB_NULL+1), R_POP(2, 221),
        (222)<<16 | (TB_NULL+1), 223,
        (223)<<16 | (0), 224,
        (224)<<16 | (0), 225,
        (224)<<16 | (TB_ICONST+1), R_PUSH(226),
        (225)<<16 | (TB_NULL+1), R_POP(2, 225),
        (226)<<16 | (0), 226,
        (226)<<16 | (TB_NULL+1), R_POP(2, 227),
        (227)<<16 | (TB_NULL+1), R_POP(2, 227),
        (228)<<16 | (TB_NULL+1), 229,
        (229)<<16 | (0), 230,
        (230)<<16 | (0), 231,
        (230)<<16 | (TB_ICONST+1), R_PUSH(232),
        (231)<<16 | (TB_NULL+1), R_POP(2, 231),
        (232)<<16 | (0), 232,
        (232)<<16 | (TB_NULL+1), R_POP(2, 233),
        (233)<<16 | (TB_NULL+1), R_POP(2, 233),
        (234)<<16 | (TB_NULL+1), 235,
        (235)<<16 | (0), 236,
        (236)<<16 | (TB_NULL+1), R_POP(2, 236),
    };
    // transitions: 371
    size_t count = sizeof(edges) / (2*sizeof(uint32_t));
    node_grammar_alloc(count);
    FOR_N(i, 0, count) {
        node_grammar_put(edges[i*2], edges[i*2 + 1]);
    }
}
