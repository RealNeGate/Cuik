typedef enum X86NodeType {
    x86_mov = TB_MACH_X86 + 5,
    x86_shl = TB_MACH_X86 + 13,
    x86_lea = TB_MACH_X86 + 6,
    x86_MEMORY = TB_MACH_X86 + 0,
    x86_shr = TB_MACH_X86 + 14,
    x86_cmp = TB_MACH_X86 + 2,
    x86_COND = TB_MACH_X86 + 1,
    x86_test = TB_MACH_X86 + 3,
    x86_or = TB_MACH_X86 + 8,
    x86_sar = TB_MACH_X86 + 15,
    x86_xor = TB_MACH_X86 + 11,
    x86_rol = TB_MACH_X86 + 16,
    x86_and = TB_MACH_X86 + 9,
    x86_add = TB_MACH_X86 + 7,
    x86_imul = TB_MACH_X86 + 12,
    x86_ror = TB_MACH_X86 + 17,
    x86_sub = TB_MACH_X86 + 10,
    x86_jcc = TB_MACH_X86 + 4,
} X86NodeType;
static const char* node_name(int n_type) {
    switch (n_type) {
        case x86_mov: return "x86_mov";
        case x86_shl: return "x86_shl";
        case x86_lea: return "x86_lea";
        case x86_MEMORY: return "x86_MEMORY";
        case x86_shr: return "x86_shr";
        case x86_cmp: return "x86_cmp";
        case x86_COND: return "x86_COND";
        case x86_test: return "x86_test";
        case x86_or: return "x86_or";
        case x86_sar: return "x86_sar";
        case x86_xor: return "x86_xor";
        case x86_rol: return "x86_rol";
        case x86_and: return "x86_and";
        case x86_add: return "x86_add";
        case x86_imul: return "x86_imul";
        case x86_ror: return "x86_ror";
        case x86_sub: return "x86_sub";
        case x86_jcc: return "x86_jcc";
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
                k0_extra->flags = OP_INDEXED;
                k0_extra->disp = as_int32($disp);
            
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
        case 99: {
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
        case 169: {
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
        case 201: {
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
        case 135: {
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
        case 115: {
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
        case 151: {
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
        case 183: {
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
        case 93: {
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
        case 125: {
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
        case 147: {
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
        case 179: {
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
        case 211: {
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
        case 79: {
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
        case 111: {
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
        case 189: {
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
        case 187: {
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
        case 153: {
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
        case 199: {
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
        case 107: {
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
        case 165: {
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
        case 117: {
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
        case 195: {
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
        case 161: {
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
        case 193: {
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
        case 213: {
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
        case 143: {
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
        case 207: {
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
        case 97: {
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
        case 129: {
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
        case 133: {
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
        case 205: {
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
        case 171: {
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
        // no match?
        default: return NULL;
    }
}

static bool x86_is_operand[512] = {
    [TB_CMP_SLT] = true,
    [TB_CMP_NE] = true,
    [TB_CMP_EQ] = true,
    [TB_PTR_OFFSET] = true,
    [TB_CMP_ULE] = true,
    [TB_CMP_ULT] = true,
    [TB_CMP_SLE] = true,
};
#define R_PUSH(next)        ((1u  << 16u) | (next))
#define R_POP(n, next)      (((n) << 16u) | (next))
static void global_init(void) {
    static const uint32_t edges[] = {
        (0)<<16 | (x86_MEMORY+1), R_PUSH(69),
        (0)<<16 | (TB_SUB+1), R_PUSH(130),
        (0)<<16 | (TB_CMP_ULT+1), R_PUSH(46),
        (0)<<16 | (TB_CMP_NE+1), R_PUSH(58),
        (0)<<16 | (TB_SHR+1), R_PUSH(190),
        (0)<<16 | (TB_ADD+1), R_PUSH(76),
        (0)<<16 | (TB_OR+1), R_PUSH(94),
        (0)<<16 | (TB_SAR+1), R_PUSH(196),
        (0)<<16 | (TB_XOR+1), R_PUSH(148),
        (0)<<16 | (TB_BRANCH+1), R_PUSH(64),
        (0)<<16 | (TB_ROL+1), R_PUSH(202),
        (0)<<16 | (TB_ROR+1), R_PUSH(208),
        (0)<<16 | (TB_AND+1), R_PUSH(112),
        (0)<<16 | (TB_SHL+1), R_PUSH(184),
        (0)<<16 | (TB_PTR_OFFSET+1), R_PUSH(1),
        (0)<<16 | (TB_CMP_SLT+1), R_PUSH(34),
        (0)<<16 | (TB_CMP_EQ+1), R_PUSH(52),
        (0)<<16 | (TB_MUL+1), R_PUSH(166),
        (0)<<16 | (TB_CMP_ULE+1), R_PUSH(40),
        (0)<<16 | (TB_CMP_SLE+1), R_PUSH(28),
        (0)<<16 | (TB_LOAD+1), R_PUSH(70),
        (1)<<16 | (TB_NULL+1), 2,
        (2)<<16 | (0), 3,
        (3)<<16 | (0), 4,
        (3)<<16 | (TB_ICONST+1), R_PUSH(5),
        (3)<<16 | (TB_SHL+1), R_PUSH(22),
        (3)<<16 | (TB_ADD+1), R_PUSH(7),
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
        (10)<<16 | (0), 10,
        (10)<<16 | (TB_NULL+1), R_POP(2, 11),
        (11)<<16 | (0), R_POP(2, 4),
        (11)<<16 | (TB_NULL+1), R_POP(2, 12),
        (12)<<16 | (TB_NULL+1), R_POP(2, 12),
        (13)<<16 | (0), R_POP(3, 4),
        (13)<<16 | (TB_NULL+1), 14,
        (14)<<16 | (0), 15,
        (15)<<16 | (0), R_POP(3, 4),
        (15)<<16 | (TB_ICONST+1), R_PUSH(16),
        (16)<<16 | (0), 16,
        (16)<<16 | (TB_NULL+1), R_POP(2, 17),
        (17)<<16 | (0), R_POP(3, 4),
        (17)<<16 | (TB_NULL+1), R_POP(2, 18),
        (18)<<16 | (0), R_POP(2, 4),
        (18)<<16 | (TB_ICONST+1), R_PUSH(19),
        (19)<<16 | (0), 19,
        (19)<<16 | (TB_NULL+1), R_POP(2, 20),
        (20)<<16 | (0), R_POP(2, 4),
        (20)<<16 | (TB_NULL+1), R_POP(2, 21),
        (21)<<16 | (TB_NULL+1), R_POP(2, 21),
        (22)<<16 | (0), R_POP(2, 4),
        (22)<<16 | (TB_NULL+1), 23,
        (23)<<16 | (0), 24,
        (24)<<16 | (0), R_POP(2, 4),
        (24)<<16 | (TB_ICONST+1), R_PUSH(25),
        (25)<<16 | (0), 25,
        (25)<<16 | (TB_NULL+1), R_POP(2, 26),
        (26)<<16 | (0), R_POP(2, 4),
        (26)<<16 | (TB_NULL+1), R_POP(2, 27),
        (27)<<16 | (TB_NULL+1), R_POP(2, 27),
        (28)<<16 | (TB_NULL+1), 29,
        (29)<<16 | (0), 30,
        (30)<<16 | (0), 31,
        (30)<<16 | (TB_ICONST+1), R_PUSH(32),
        (31)<<16 | (TB_NULL+1), R_POP(2, 31),
        (32)<<16 | (0), 32,
        (32)<<16 | (TB_NULL+1), R_POP(2, 33),
        (33)<<16 | (TB_NULL+1), R_POP(2, 33),
        (34)<<16 | (TB_NULL+1), 35,
        (35)<<16 | (0), 36,
        (36)<<16 | (0), 37,
        (36)<<16 | (TB_ICONST+1), R_PUSH(38),
        (37)<<16 | (TB_NULL+1), R_POP(2, 37),
        (38)<<16 | (0), 38,
        (38)<<16 | (TB_NULL+1), R_POP(2, 39),
        (39)<<16 | (TB_NULL+1), R_POP(2, 39),
        (40)<<16 | (TB_NULL+1), 41,
        (41)<<16 | (0), 42,
        (42)<<16 | (0), 43,
        (42)<<16 | (TB_ICONST+1), R_PUSH(44),
        (43)<<16 | (TB_NULL+1), R_POP(2, 43),
        (44)<<16 | (0), 44,
        (44)<<16 | (TB_NULL+1), R_POP(2, 45),
        (45)<<16 | (TB_NULL+1), R_POP(2, 45),
        (46)<<16 | (TB_NULL+1), 47,
        (47)<<16 | (0), 48,
        (48)<<16 | (0), 49,
        (48)<<16 | (TB_ICONST+1), R_PUSH(50),
        (49)<<16 | (TB_NULL+1), R_POP(2, 49),
        (50)<<16 | (0), 50,
        (50)<<16 | (TB_NULL+1), R_POP(2, 51),
        (51)<<16 | (TB_NULL+1), R_POP(2, 51),
        (52)<<16 | (TB_NULL+1), 53,
        (53)<<16 | (0), 54,
        (54)<<16 | (0), 55,
        (54)<<16 | (TB_ICONST+1), R_PUSH(56),
        (55)<<16 | (TB_NULL+1), R_POP(2, 55),
        (56)<<16 | (0), 56,
        (56)<<16 | (TB_NULL+1), R_POP(2, 57),
        (57)<<16 | (TB_NULL+1), R_POP(2, 57),
        (58)<<16 | (TB_NULL+1), 59,
        (59)<<16 | (0), 60,
        (60)<<16 | (0), 61,
        (60)<<16 | (TB_ICONST+1), R_PUSH(62),
        (61)<<16 | (TB_NULL+1), R_POP(2, 61),
        (62)<<16 | (0), 62,
        (62)<<16 | (TB_NULL+1), R_POP(2, 63),
        (63)<<16 | (TB_NULL+1), R_POP(2, 63),
        (64)<<16 | (0), 65,
        (65)<<16 | (x86_COND+1), R_PUSH(66),
        (66)<<16 | (0), 67,
        (67)<<16 | (TB_NULL+1), R_POP(2, 68),
        (68)<<16 | (TB_NULL+1), R_POP(2, 68),
        (69)<<16 | (0), 69,
        (69)<<16 | (TB_NULL+1), R_POP(2, 69),
        (70)<<16 | (0), 71,
        (71)<<16 | (0), 72,
        (72)<<16 | (0), 73,
        (72)<<16 | (x86_MEMORY+1), R_PUSH(74),
        (73)<<16 | (TB_NULL+1), R_POP(2, 73),
        (74)<<16 | (0), 74,
        (74)<<16 | (TB_NULL+1), R_POP(2, 75),
        (75)<<16 | (TB_NULL+1), R_POP(2, 75),
        (76)<<16 | (TB_NULL+1), 77,
        (77)<<16 | (0), 78,
        (77)<<16 | (TB_LOAD+1), R_PUSH(82),
        (78)<<16 | (0), 79,
        (78)<<16 | (TB_ICONST+1), R_PUSH(80),
        (79)<<16 | (TB_NULL+1), R_POP(2, 79),
        (80)<<16 | (0), 80,
        (80)<<16 | (TB_NULL+1), R_POP(2, 81),
        (81)<<16 | (TB_NULL+1), R_POP(2, 81),
        (82)<<16 | (0), 83,
        (83)<<16 | (0), 84,
        (84)<<16 | (0), 90,
        (84)<<16 | (x86_MEMORY+1), R_PUSH(85),
        (85)<<16 | (0), 85,
        (85)<<16 | (TB_NULL+1), R_POP(2, 86),
        (86)<<16 | (0), R_POP(2, 78),
        (86)<<16 | (TB_NULL+1), R_POP(2, 87),
        (87)<<16 | (TB_ICONST+1), R_PUSH(88),
        (88)<<16 | (0), 88,
        (88)<<16 | (TB_NULL+1), R_POP(2, 89),
        (89)<<16 | (TB_NULL+1), R_POP(2, 89),
        (90)<<16 | (0), R_POP(2, 78),
        (90)<<16 | (x86_MEMORY+1), R_PUSH(91),
        (91)<<16 | (0), 91,
        (91)<<16 | (TB_NULL+1), R_POP(2, 92),
        (92)<<16 | (0), R_POP(2, 78),
        (92)<<16 | (TB_NULL+1), R_POP(2, 93),
        (93)<<16 | (TB_NULL+1), R_POP(2, 93),
        (94)<<16 | (TB_NULL+1), 95,
        (95)<<16 | (0), 96,
        (95)<<16 | (TB_LOAD+1), R_PUSH(100),
        (96)<<16 | (0), 97,
        (96)<<16 | (TB_ICONST+1), R_PUSH(98),
        (97)<<16 | (TB_NULL+1), R_POP(2, 97),
        (98)<<16 | (0), 98,
        (98)<<16 | (TB_NULL+1), R_POP(2, 99),
        (99)<<16 | (TB_NULL+1), R_POP(2, 99),
        (100)<<16 | (0), 101,
        (101)<<16 | (0), 102,
        (102)<<16 | (0), 108,
        (102)<<16 | (x86_MEMORY+1), R_PUSH(103),
        (103)<<16 | (0), 103,
        (103)<<16 | (TB_NULL+1), R_POP(2, 104),
        (104)<<16 | (0), R_POP(2, 96),
        (104)<<16 | (TB_NULL+1), R_POP(2, 105),
        (105)<<16 | (TB_ICONST+1), R_PUSH(106),
        (106)<<16 | (0), 106,
        (106)<<16 | (TB_NULL+1), R_POP(2, 107),
        (107)<<16 | (TB_NULL+1), R_POP(2, 107),
        (108)<<16 | (0), R_POP(2, 96),
        (108)<<16 | (x86_MEMORY+1), R_PUSH(109),
        (109)<<16 | (0), 109,
        (109)<<16 | (TB_NULL+1), R_POP(2, 110),
        (110)<<16 | (0), R_POP(2, 96),
        (110)<<16 | (TB_NULL+1), R_POP(2, 111),
        (111)<<16 | (TB_NULL+1), R_POP(2, 111),
        (112)<<16 | (TB_NULL+1), 113,
        (113)<<16 | (0), 114,
        (113)<<16 | (TB_LOAD+1), R_PUSH(118),
        (114)<<16 | (0), 115,
        (114)<<16 | (TB_ICONST+1), R_PUSH(116),
        (115)<<16 | (TB_NULL+1), R_POP(2, 115),
        (116)<<16 | (0), 116,
        (116)<<16 | (TB_NULL+1), R_POP(2, 117),
        (117)<<16 | (TB_NULL+1), R_POP(2, 117),
        (118)<<16 | (0), 119,
        (119)<<16 | (0), 120,
        (120)<<16 | (0), 126,
        (120)<<16 | (x86_MEMORY+1), R_PUSH(121),
        (121)<<16 | (0), 121,
        (121)<<16 | (TB_NULL+1), R_POP(2, 122),
        (122)<<16 | (0), R_POP(2, 114),
        (122)<<16 | (TB_NULL+1), R_POP(2, 123),
        (123)<<16 | (TB_ICONST+1), R_PUSH(124),
        (124)<<16 | (0), 124,
        (124)<<16 | (TB_NULL+1), R_POP(2, 125),
        (125)<<16 | (TB_NULL+1), R_POP(2, 125),
        (126)<<16 | (0), R_POP(2, 114),
        (126)<<16 | (x86_MEMORY+1), R_PUSH(127),
        (127)<<16 | (0), 127,
        (127)<<16 | (TB_NULL+1), R_POP(2, 128),
        (128)<<16 | (0), R_POP(2, 114),
        (128)<<16 | (TB_NULL+1), R_POP(2, 129),
        (129)<<16 | (TB_NULL+1), R_POP(2, 129),
        (130)<<16 | (TB_NULL+1), 131,
        (131)<<16 | (0), 132,
        (131)<<16 | (TB_LOAD+1), R_PUSH(136),
        (132)<<16 | (0), 133,
        (132)<<16 | (TB_ICONST+1), R_PUSH(134),
        (133)<<16 | (TB_NULL+1), R_POP(2, 133),
        (134)<<16 | (0), 134,
        (134)<<16 | (TB_NULL+1), R_POP(2, 135),
        (135)<<16 | (TB_NULL+1), R_POP(2, 135),
        (136)<<16 | (0), 137,
        (137)<<16 | (0), 138,
        (138)<<16 | (0), 144,
        (138)<<16 | (x86_MEMORY+1), R_PUSH(139),
        (139)<<16 | (0), 139,
        (139)<<16 | (TB_NULL+1), R_POP(2, 140),
        (140)<<16 | (0), R_POP(2, 132),
        (140)<<16 | (TB_NULL+1), R_POP(2, 141),
        (141)<<16 | (TB_ICONST+1), R_PUSH(142),
        (142)<<16 | (0), 142,
        (142)<<16 | (TB_NULL+1), R_POP(2, 143),
        (143)<<16 | (TB_NULL+1), R_POP(2, 143),
        (144)<<16 | (0), R_POP(2, 132),
        (144)<<16 | (x86_MEMORY+1), R_PUSH(145),
        (145)<<16 | (0), 145,
        (145)<<16 | (TB_NULL+1), R_POP(2, 146),
        (146)<<16 | (0), R_POP(2, 132),
        (146)<<16 | (TB_NULL+1), R_POP(2, 147),
        (147)<<16 | (TB_NULL+1), R_POP(2, 147),
        (148)<<16 | (TB_NULL+1), 149,
        (149)<<16 | (0), 150,
        (149)<<16 | (TB_LOAD+1), R_PUSH(154),
        (150)<<16 | (0), 151,
        (150)<<16 | (TB_ICONST+1), R_PUSH(152),
        (151)<<16 | (TB_NULL+1), R_POP(2, 151),
        (152)<<16 | (0), 152,
        (152)<<16 | (TB_NULL+1), R_POP(2, 153),
        (153)<<16 | (TB_NULL+1), R_POP(2, 153),
        (154)<<16 | (0), 155,
        (155)<<16 | (0), 156,
        (156)<<16 | (0), 162,
        (156)<<16 | (x86_MEMORY+1), R_PUSH(157),
        (157)<<16 | (0), 157,
        (157)<<16 | (TB_NULL+1), R_POP(2, 158),
        (158)<<16 | (0), R_POP(2, 150),
        (158)<<16 | (TB_NULL+1), R_POP(2, 159),
        (159)<<16 | (TB_ICONST+1), R_PUSH(160),
        (160)<<16 | (0), 160,
        (160)<<16 | (TB_NULL+1), R_POP(2, 161),
        (161)<<16 | (TB_NULL+1), R_POP(2, 161),
        (162)<<16 | (0), R_POP(2, 150),
        (162)<<16 | (x86_MEMORY+1), R_PUSH(163),
        (163)<<16 | (0), 163,
        (163)<<16 | (TB_NULL+1), R_POP(2, 164),
        (164)<<16 | (0), R_POP(2, 150),
        (164)<<16 | (TB_NULL+1), R_POP(2, 165),
        (165)<<16 | (TB_NULL+1), R_POP(2, 165),
        (166)<<16 | (TB_NULL+1), 167,
        (167)<<16 | (0), 168,
        (167)<<16 | (TB_LOAD+1), R_PUSH(172),
        (168)<<16 | (0), 169,
        (168)<<16 | (TB_ICONST+1), R_PUSH(170),
        (169)<<16 | (TB_NULL+1), R_POP(2, 169),
        (170)<<16 | (0), 170,
        (170)<<16 | (TB_NULL+1), R_POP(2, 171),
        (171)<<16 | (TB_NULL+1), R_POP(2, 171),
        (172)<<16 | (0), 173,
        (173)<<16 | (0), 174,
        (174)<<16 | (0), 180,
        (174)<<16 | (x86_MEMORY+1), R_PUSH(175),
        (175)<<16 | (0), 175,
        (175)<<16 | (TB_NULL+1), R_POP(2, 176),
        (176)<<16 | (0), R_POP(2, 168),
        (176)<<16 | (TB_NULL+1), R_POP(2, 177),
        (177)<<16 | (TB_ICONST+1), R_PUSH(178),
        (178)<<16 | (0), 178,
        (178)<<16 | (TB_NULL+1), R_POP(2, 179),
        (179)<<16 | (TB_NULL+1), R_POP(2, 179),
        (180)<<16 | (0), R_POP(2, 168),
        (180)<<16 | (x86_MEMORY+1), R_PUSH(181),
        (181)<<16 | (0), 181,
        (181)<<16 | (TB_NULL+1), R_POP(2, 182),
        (182)<<16 | (0), R_POP(2, 168),
        (182)<<16 | (TB_NULL+1), R_POP(2, 183),
        (183)<<16 | (TB_NULL+1), R_POP(2, 183),
        (184)<<16 | (TB_NULL+1), 185,
        (185)<<16 | (0), 186,
        (186)<<16 | (0), 187,
        (186)<<16 | (TB_ICONST+1), R_PUSH(188),
        (187)<<16 | (TB_NULL+1), R_POP(2, 187),
        (188)<<16 | (0), 188,
        (188)<<16 | (TB_NULL+1), R_POP(2, 189),
        (189)<<16 | (TB_NULL+1), R_POP(2, 189),
        (190)<<16 | (TB_NULL+1), 191,
        (191)<<16 | (0), 192,
        (192)<<16 | (0), 193,
        (192)<<16 | (TB_ICONST+1), R_PUSH(194),
        (193)<<16 | (TB_NULL+1), R_POP(2, 193),
        (194)<<16 | (0), 194,
        (194)<<16 | (TB_NULL+1), R_POP(2, 195),
        (195)<<16 | (TB_NULL+1), R_POP(2, 195),
        (196)<<16 | (TB_NULL+1), 197,
        (197)<<16 | (0), 198,
        (198)<<16 | (0), 199,
        (198)<<16 | (TB_ICONST+1), R_PUSH(200),
        (199)<<16 | (TB_NULL+1), R_POP(2, 199),
        (200)<<16 | (0), 200,
        (200)<<16 | (TB_NULL+1), R_POP(2, 201),
        (201)<<16 | (TB_NULL+1), R_POP(2, 201),
        (202)<<16 | (TB_NULL+1), 203,
        (203)<<16 | (0), 204,
        (204)<<16 | (0), 205,
        (204)<<16 | (TB_ICONST+1), R_PUSH(206),
        (205)<<16 | (TB_NULL+1), R_POP(2, 205),
        (206)<<16 | (0), 206,
        (206)<<16 | (TB_NULL+1), R_POP(2, 207),
        (207)<<16 | (TB_NULL+1), R_POP(2, 207),
        (208)<<16 | (TB_NULL+1), 209,
        (209)<<16 | (0), 210,
        (210)<<16 | (0), 211,
        (210)<<16 | (TB_ICONST+1), R_PUSH(212),
        (211)<<16 | (TB_NULL+1), R_POP(2, 211),
        (212)<<16 | (0), 212,
        (212)<<16 | (TB_NULL+1), R_POP(2, 213),
        (213)<<16 | (TB_NULL+1), R_POP(2, 213),
    };
    // transitions: 339
    size_t count = sizeof(edges) / (2*sizeof(uint32_t));
    node_grammar_alloc(count);
    FOR_N(i, 0, count) {
        node_grammar_put(edges[i*2], edges[i*2 + 1]);
    }
}
