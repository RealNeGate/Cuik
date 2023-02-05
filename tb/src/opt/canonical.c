#include "../tb_internal.h"
#include "cse.h"
#include "fold.h"

#define NL_MAP_IMPL
#include "../hash_map.h"

static void replace_label(TB_Function* f, TB_Label old, TB_Label new) {
    f->bbs[old] = (TB_BasicBlock){ 0 };

    #define X(l) if (l == old) l = new
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (tb_node_is_phi_node(f, r)) {
                size_t count = tb_node_get_phi_width(f, r);
                TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

                FOREACH_N(i, 0, count) {
                    X(inputs[i].label);
                }
            } else if (n->type == TB_IF) {
                X(n->if_.if_true);
                X(n->if_.if_false);
            } else if (n->type == TB_GOTO) {
                X(n->goto_.label);
            } else if (n->type == TB_SWITCH) {
                size_t entry_start = n->switch_.entries_start;
                size_t entry_count = (n->switch_.entries_end - n->switch_.entries_start) / 2;

                for (size_t j = 0; j < entry_count; j++) {
                    TB_SwitchEntry* e = (TB_SwitchEntry*)&f->vla.data[entry_start + (j * 2)];
                    X(e->value);
                }

                X(n->switch_.default_label);
            }
        }
    }
    #undef X
}

static bool compact_regs(TB_Function* f) {
    int changes = 0;
    TB_Node* nodes = f->nodes;

    // Find a NULL, skip over any NULLs until a valid node is found and cut out the middle men
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_Reg prev = f->bbs[bb].start;

        for (TB_Reg r = prev; r != 0; prev = r, r = nodes[r].next) {
            if (nodes[r].type == TB_NULL) {
                bool start_of_bb = (prev == r);

                // check for however many sequencial NOPs
                do {
                    TB_Reg next = nodes[r].next;
                    if (next == 0) break;

                    r = next;
                } while (nodes[r].type == TB_NULL);

                if (start_of_bb) {
                    // this is the start of the basic block, changed the starting point in it instead
                    f->bbs[bb].start = r;
                } else {
                    f->nodes[prev].next = r;
                }
                changes++;
            }
        }
    }

    return changes;
}

static bool remove_passes(TB_Function* f) {
    int changes = 0;

    if (f->node_count > 300) {
        NL_Map(TB_Reg, TB_Reg) def_table = { 0 };

        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(r, f, bb) {
                TB_Node* n = &f->nodes[r];

                #define X(reg)                                     \
                do {                                               \
                    ptrdiff_t search = nl_map_get(def_table, reg); \
                    if (search >= 0) {                             \
                        reg = def_table[search].v;                 \
                    }                                              \
                } while (0)

                switch (n->type) {
                    case TB_NULL:
                    case TB_INTEGER_CONST:
                    case TB_FLOAT32_CONST:
                    case TB_FLOAT64_CONST:
                    case TB_STRING_CONST:
                    case TB_LOCAL:
                    case TB_PARAM:
                    case TB_GOTO:
                    case TB_LINE_INFO:
                    case TB_GET_SYMBOL_ADDRESS:
                    case TB_X86INTRIN_STMXCSR:
                    case TB_UNREACHABLE:
                    case TB_DEBUGBREAK:
                    case TB_TRAP:
                    case TB_POISON:
                    break;

                    case TB_INITIALIZE:
                    X(n->init.addr);
                    break;

                    case TB_KEEPALIVE:
                    case TB_VA_START:
                    case TB_NOT:
                    case TB_NEG:
                    case TB_X86INTRIN_SQRT:
                    case TB_X86INTRIN_RSQRT:
                    case TB_INT2PTR:
                    case TB_PTR2INT:
                    case TB_UINT2FLOAT:
                    case TB_FLOAT2UINT:
                    case TB_INT2FLOAT:
                    case TB_FLOAT2INT:
                    case TB_TRUNCATE:
                    case TB_X86INTRIN_LDMXCSR:
                    case TB_BITCAST:
                    X(n->unary.src);
                    break;

                    case TB_ATOMIC_LOAD:
                    case TB_ATOMIC_XCHG:
                    case TB_ATOMIC_ADD:
                    case TB_ATOMIC_SUB:
                    case TB_ATOMIC_AND:
                    case TB_ATOMIC_XOR:
                    case TB_ATOMIC_OR:
                    case TB_ATOMIC_CMPXCHG:
                    X(n->atomic.addr);
                    X(n->atomic.src);
                    break;

                    case TB_ATOMIC_CMPXCHG2:
                    X(n->atomic.src);
                    break;

                    case TB_MEMCPY:
                    case TB_MEMSET:
                    X(n->mem_op.dst);
                    X(n->mem_op.src);
                    X(n->mem_op.size);
                    break;

                    case TB_MEMBER_ACCESS:
                    X(n->member_access.base);
                    break;

                    case TB_ARRAY_ACCESS:
                    X(n->array_access.base);
                    X(n->array_access.index);
                    break;

                    case TB_PARAM_ADDR:
                    X(n->param_addr.param);
                    break;

                    case TB_PASS:
                    X(n->pass.value);
                    break;

                    case TB_PHI1:
                    X(n->phi1.inputs[0].val);
                    break;

                    case TB_PHI2:
                    FOREACH_N(it, 0, 2) {
                        X(n->phi2.inputs[it].val);
                    }
                    break;

                    case TB_PHIN:
                    FOREACH_N(it, 0, n->phi.count) {
                        X(n->phi.inputs[it].val);
                    }
                    break;

                    case TB_LOAD:
                    X(n->load.address);
                    break;

                    case TB_STORE:
                    X(n->store.address);
                    X(n->store.value);
                    break;

                    case TB_ZERO_EXT:
                    case TB_SIGN_EXT:
                    case TB_FLOAT_EXT:
                    X(n->unary.src);
                    break;

                    case TB_AND:
                    case TB_OR:
                    case TB_XOR:
                    case TB_ADD:
                    case TB_SUB:
                    case TB_MUL:
                    case TB_UDIV:
                    case TB_SDIV:
                    case TB_UMOD:
                    case TB_SMOD:
                    case TB_SAR:
                    case TB_SHL:
                    case TB_SHR:
                    X(n->i_arith.a);
                    X(n->i_arith.b);
                    break;

                    case TB_FADD:
                    case TB_FSUB:
                    case TB_FMUL:
                    case TB_FDIV:
                    X(n->f_arith.a);
                    X(n->f_arith.b);
                    break;

                    case TB_CMP_EQ:
                    case TB_CMP_NE:
                    case TB_CMP_SLT:
                    case TB_CMP_SLE:
                    case TB_CMP_ULT:
                    case TB_CMP_ULE:
                    case TB_CMP_FLT:
                    case TB_CMP_FLE:
                    X(n->cmp.a);
                    X(n->cmp.b);
                    break;

                    case TB_SCALL: {
                        X(n->scall.target);

                        FOREACH_N(it, n->scall.param_start, n->scall.param_end) {
                            X(f->vla.data[it]);
                        }
                        break;
                    }

                    case TB_VCALL: {
                        X(n->vcall.target);

                        FOREACH_N(it, n->vcall.param_start, n->vcall.param_end) {
                            X(f->vla.data[it]);
                        }
                        break;
                    }

                    case TB_CALL:
                    case TB_ICALL: {
                        FOREACH_N(it, n->call.param_start, n->call.param_end) {
                            X(f->vla.data[it]);
                        }
                        break;
                    }

                    case TB_SWITCH: X(n->switch_.key); break;
                    case TB_IF: X(n->if_.cond); break;
                    case TB_RET: X(n->ret.value); break;

                    default: tb_todo();
                }
                #undef X

                if (f->nodes[r].type == TB_PASS) {
                    OPTIMIZER_LOG(r, "Replacing PASS with r%d", f->nodes[r].pass.value);

                    // if the node we're pointing to is also in the map then we look at it's parent
                    TB_Reg pointee = f->nodes[r].pass.value;
                    ptrdiff_t search;
                    while (search = nl_map_get(def_table, pointee), search >= 0) {
                        pointee = def_table[search].v;
                    }

                    nl_map_put(def_table, r, pointee);

                    // if it matches find, then remove find from the basic block
                    if (f->bbs[bb].start == r) {
                        f->bbs[bb].start = f->nodes[f->bbs[bb].start].next;
                    }

                    if (f->bbs[bb].end == r) {
                        f->bbs[bb].end = tb_node_get_previous(f, f->bbs[bb].end);
                    }

                    f->nodes[r].type = TB_NULL;
                    changes++;
                }
            }
        }

        nl_map_free(def_table);
    } else {
        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(r, f, bb) {
                if (f->nodes[r].type == TB_PASS) {
                    OPTIMIZER_LOG(r, "Replacing PASS with r%d", f->nodes[r].pass.value);

                    tb_function_find_replace_reg(f, r, f->nodes[r].pass.value);
                    tb_murder_reg(f, r);
                    changes++;
                }
            }
        }
    }

    return changes;
}

static bool inst_combine(TB_Function* f) {
    int changes = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_ULE) {
                TB_Node* a = &f->nodes[n->cmp.a];
                TB_Node* b = &f->nodes[n->cmp.b];

                if (n->type == TB_CMP_EQ && tb_node_is_constant_zero(f, n->cmp.b) &&
                    a->type == TB_CMP_EQ && tb_node_is_constant_zero(f, a->cmp.b)) {
                    // (cmpeq (cmpeq a 0) 0) => (cmpeq a 0)
                    OPTIMIZER_LOG(r, "removed redundant comparisons");

                    n->type = TB_PASS;
                    n->pass.value = a->cmp.a;
                } else if (n->type == TB_CMP_NE && tb_node_is_constant_zero(f, n->cmp.b) &&
                    a->type == TB_CMP_EQ && tb_node_is_constant_zero(f, a->cmp.b)) {
                    // (cmpeq (cmpeq a 0) 0) => (cmpne a 0)
                    OPTIMIZER_LOG(r, "removed redundant comparisons");

                    n->type = TB_PASS;
                    n->pass.value = a->cmp.a;
                    a->type = TB_CMP_NE;
                } else {
                    // Sometimes we promote some types up when we don't need to
                    // (cmp (sxt/zxt A) (int B))
                    // VVV
                    // (cmp A (int B))
                    if (a->type == TB_SIGN_EXT && b->type == TB_SIGN_EXT) {
                        OPTIMIZER_LOG(r, "removed unnecessary sign extension");
                        TB_DataType dt = f->nodes[a->unary.src].dt;

                        n->cmp.dt = dt;
                        n->cmp.a = a->unary.src;
                        n->cmp.b = b->unary.src;
                        changes++;
                    } else if (a->type == TB_ZERO_EXT && b->type == TB_ZERO_EXT) {
                        OPTIMIZER_LOG(r, "removed unnecessary zero extension");
                        TB_DataType dt = f->nodes[a->unary.src].dt;

                        n->cmp.dt = dt;
                        n->cmp.a = a->unary.src;
                        n->cmp.b = b->unary.src;
                        changes++;
                    } else if (a->type == TB_SIGN_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(f->nodes[a->unary.src].dt, b->dt)) {
                        OPTIMIZER_LOG(r, "removed unnecessary sign extension for compare against constants");

                        n->cmp.a = a->unary.src;
                        changes++;
                    } else if (a->type == TB_ZERO_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(f->nodes[a->unary.src].dt, b->dt)) {
                        OPTIMIZER_LOG(r, "removed unnecessary zero extension for compare against constants");

                        n->cmp.a = a->unary.src;
                        changes++;
                    }
                }
            }

            if (phi_motion(f, n)) {
                n = &f->nodes[r];
                changes++;
            }

            if (reassoc(f, n)) {
                n = &f->nodes[r];
                changes++;
            }

            if (const_fold(f, n)) {
                n = &f->nodes[r];
                changes++;
            }

            TB_NodeTypeEnum type = n->type;
            if (n->type == TB_PASS) {
                OPTIMIZER_LOG(r, "Replacing PASS with r%d", n->unary.src);
                tb_function_find_replace_reg(f, r, n->unary.src);

                n->type = TB_NULL;
                changes++;
            } else if (type == TB_INITIALIZE) {
                TB_Reg addr = n->init.addr;
                TB_Initializer* init = n->init.src;

                if (init->obj_count == 0) {
                    OPTIMIZER_LOG(r, "Replaced complex initializer with memset");

                    TB_Label bb2 = tb_find_label_from_reg(f, addr);
                    TB_Reg imm_reg = tb_function_insert_after(f, bb2, addr);
                    f->nodes[imm_reg].type = TB_INTEGER_CONST;
                    f->nodes[imm_reg].dt = TB_TYPE_I8;
                    f->nodes[imm_reg].integer = (struct TB_NodeInt) {
                        .num_words = 1,
                        .single_word = 0
                    };

                    TB_Reg size_reg = tb_function_insert_after(f, bb2, imm_reg);
                    f->nodes[size_reg].type = TB_INTEGER_CONST;
                    f->nodes[size_reg].dt = TB_TYPE_PTR;
                    f->nodes[size_reg].integer = (struct TB_NodeInt) {
                        .num_words = 1,
                        .single_word = init->size
                    };

                    n = &f->nodes[r];
                    n->type = TB_MEMSET;
                    n->mem_op.dst = addr;
                    n->mem_op.src = imm_reg;
                    n->mem_op.size = size_reg;
                    n->mem_op.align = init->align;
                    changes++;
                }
            } else if (type == TB_MEMBER_ACCESS) {
                TB_Node* base = &f->nodes[n->member_access.base];

                if (base->type == TB_MEMBER_ACCESS) {
                    uint32_t offset = n->member_access.offset;
                    offset += base->member_access.offset;

                    if (!TB_FITS_INTO(int32_t, offset)) {
                        OPTIMIZER_LOG(r, "FAILURE cannot fold into member access without overflow");
                    } else {
                        TB_Reg base_base = base->member_access.base;

                        n->member_access.base = base_base;
                        n->member_access.offset = offset;
                        changes++;
                    }
                } else {
                    int32_t offset = n->member_access.offset;

                    if (offset == 0) {
                        OPTIMIZER_LOG(r, "elided member access to first element");

                        n->type = TB_PASS;
                        n->pass.value = n->member_access.base;
                        changes++;
                    }
                }
            } else if (type == TB_ARRAY_ACCESS) {
                TB_Node* index = &f->nodes[n->array_access.index];

                if (index->type == TB_INTEGER_CONST && index->integer.num_words == 1) {
                    uint64_t index_imm = index->integer.single_word;

                    uint64_t res = n->array_access.stride * index_imm;
                    if (!TB_FITS_INTO(int32_t, res)) {
                        OPTIMIZER_LOG(r, "FAILURE cannot fold into array access without overflow");
                    } else {
                        // success!
                        OPTIMIZER_LOG(r, "folded constant array access");
                        TB_Reg base_reg = n->array_access.base;

                        n->type = TB_MEMBER_ACCESS;
                        n->member_access.base = base_reg;
                        n->member_access.offset = res;
                        changes++;
                    }
                } else if (tb_node_is_constant_zero(f, n->array_access.index)) {
                    OPTIMIZER_LOG(r, "elided array access to first element");

                    n->type = TB_PASS;
                    n->pass.value = n->array_access.base;
                    changes++;
                } else if (index->type == TB_MUL) {
                    TB_Node* potential_constant = &f->nodes[index->i_arith.b];

                    if (potential_constant->type == TB_INTEGER_CONST && potential_constant->integer.num_words == 1) {
                        // don't worry it doesn't loop i just needed to have 'break' support
                        do {
                            uint64_t factor = potential_constant->integer.single_word;
                            if (!TB_FITS_INTO(int32_t, factor)) {
                                OPTIMIZER_LOG(r, "FAILURE multiply cannot fold into array access because too big");
                                break;
                            }

                            uint64_t res = n->array_access.stride * factor;
                            if (!TB_FITS_INTO(int32_t, res)) {
                                OPTIMIZER_LOG(r, "FAILURE multiply cannot fold into array access without overflow");
                                break;
                            }

                            // success!
                            OPTIMIZER_LOG(r, "folded multiply into array access");
                            n->array_access.index = index->i_arith.a;
                            n->array_access.stride = res;
                            changes++;
                        } while (0);
                    }
                } else if (index->type == TB_ADD) {
                    // (array A (add B O) C) => (member (array A B C) O*C)
                    TB_Node* potential_constant = &f->nodes[index->i_arith.b];

                    if (potential_constant->type == TB_INTEGER_CONST && potential_constant->integer.num_words == 1) {
                        TB_CharUnits c = n->array_access.stride;
                        uint64_t res = potential_constant->integer.single_word * c;

                        if (res < UINT32_MAX) {
                            OPTIMIZER_LOG(r, "converted add into member access");
                            TB_Label bb2 = tb_find_label_from_reg(f, n->array_access.index);
                            TB_Reg new_array_reg = tb_function_insert_after(f, bb2, n->array_access.index);

                            TB_Reg a = n->array_access.base;
                            TB_Reg b = index->i_arith.a;

                            n = &f->nodes[r];
                            n->type = TB_MEMBER_ACCESS;
                            n->dt = TB_TYPE_PTR;
                            n->member_access.base = new_array_reg;
                            n->member_access.offset = potential_constant->integer.single_word * c;

                            TB_Node* new_array = &f->nodes[new_array_reg];
                            new_array->type = TB_ARRAY_ACCESS;
                            new_array->dt = TB_TYPE_PTR;
                            new_array->array_access.base = a;
                            new_array->array_access.index = b;
                            new_array->array_access.stride = c;
                            changes++;
                        }
                    }
                }
            } else if (type == TB_INT2PTR) {
                TB_Node* src = &f->nodes[n->unary.src];

                if (src->type == TB_INTEGER_CONST && src->integer.num_words == 1) {
                    OPTIMIZER_LOG(r, "constant int2ptr removed.");

                    uint64_t imm = src->integer.single_word;

                    n->type = TB_INTEGER_CONST;
                    // preserve the int2ptr's pointer type
                    n->integer.num_words = 1;
                    n->integer.single_word = imm;
                    changes++;
                }
            } else if (type == TB_IF) {
                TB_Node* cond = &f->nodes[n->if_.cond];

                if (cond->type == TB_STRING_CONST) {
                    // (if str B C) => (goto B)
                    TB_Label new_target = n->if_.if_true;

                    n->type = TB_GOTO;
                    n->dt = TB_TYPE_VOID;
                    n->goto_.label = new_target;
                    changes++;
                } else if (cond->type == TB_INTEGER_CONST) {
                    // (if A B C) => (goto X) where X = A ? B : C
                    TB_Label new_target = !tb_node_is_constant_zero(f, n->if_.cond) ?
                        n->if_.if_true : n->if_.if_false;

                    n->type = TB_GOTO;
                    n->dt = TB_TYPE_VOID;
                    n->goto_.label = new_target;
                    changes++;
                } else if (cond->type == TB_CMP_NE && tb_node_is_constant_zero(f, cond->cmp.b)) {
                    // (if (cmpne A 0) B C) => (if A B C)
                    OPTIMIZER_LOG(r, "removed redundant compare-to-zero on if node");

                    TB_DataType dt = f->nodes[cond->cmp.a].dt;

                    n->dt = dt;
                    n->if_.cond = cond->cmp.a;
                    changes++;
                } else if (cond->type == TB_CMP_EQ && tb_node_is_constant_zero(f, cond->cmp.b)) {
                    // (if (cmpeq A 0) B C) => (if A C B)
                    OPTIMIZER_LOG(r, "removed redundant compare-to-zero on if node");

                    TB_DataType dt = f->nodes[cond->cmp.a].dt;

                    n->dt = dt;
                    n->if_.cond = cond->cmp.a;
                    tb_swap(TB_Label, n->if_.if_true, n->if_.if_false);
                    changes++;
                }
            }
        }
    }

    return (changes > 0);
}

TB_API TB_Pass tb_opt_remove_pass_nodes(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "RemovePassNodes",
        .func_run = remove_passes,
    };
}

TB_API TB_Pass tb_opt_subexpr_elim(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "CommonSubexprElim",
        .func_run = cse,
    };
}

TB_API TB_Pass tb_opt_instcombine(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "InstCombine",
        .func_run = inst_combine,
    };
}

TB_API TB_Pass tb_opt_compact_dead_regs(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "CompactDeadRegs",
        .func_run = compact_regs,
    };
}
