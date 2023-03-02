#define NL_MAP_IMPL
#include "../hash_map.h"

typedef struct {
    NL_Map(TB_Node*, TB_Node*) def_table;
} PassCtx;

static void handle_pass(TB_Function* f, PassCtx* ctx, TB_Label bb, TB_Node* n) {
    TB_FOR_INPUT_IN_NODE(in, n) {
        ptrdiff_t search = nl_map_get(ctx->def_table, *in);
        if (search >= 0) {
            *in = ctx->def_table[search].v;
        }
    }

    if (n->type == TB_PASS) {
        OPTIMIZER_LOG(n, "Replacing PASS with r%d", n->inputs[0]);

        // if the node we're pointing to is also in the map then we look at it's parent
        TB_Node* pointee = n->inputs[0];
        ptrdiff_t search;
        while (search = nl_map_get(ctx->def_table, pointee), search >= 0) {
            pointee = ctx->def_table[search].v;
        }

        nl_map_put(ctx->def_table, n, pointee);

        // if it matches find, then remove find from the basic block
        if (f->bbs[bb].start == n) {
            f->bbs[bb].start = f->bbs[bb].start->next;
        }

        if (f->bbs[bb].end == n) {
            f->bbs[bb].end = tb_node_get_previous(f, f->bbs[bb].end);
        }

        TB_KILL_NODE(n);
    }
}

static bool simplify_cmp(TB_Function* f, TB_Node* n) {
    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_ULE) {
        TB_Node* a = n->inputs[0];
        TB_Node* b = n->inputs[1];

        if (n->type == TB_CMP_EQ && tb_node_is_constant_zero(b) &&
            a->type == TB_CMP_EQ && tb_node_is_constant_zero(a->inputs[1])) {
            // (cmpeq (cmpeq a 0) 0) => (cmpeq a 0)
            OPTIMIZER_LOG(n, "removed redundant comparisons");

            tb_transmute_to_pass(n, a->inputs[0]);
            return true;
        } else if (n->type == TB_CMP_NE && tb_node_is_constant_zero(b) &&
            a->type == TB_CMP_EQ && tb_node_is_constant_zero(a->inputs[1])) {
            // (cmpeq (cmpeq a 0) 0) => (cmpne a 0)
            OPTIMIZER_LOG(n, "removed redundant comparisons");

            a->type = TB_CMP_NE;
            tb_transmute_to_pass(n, a->inputs[0]);
            return true;
        } else {
            // Sometimes we promote some types up when we don't need to
            // (cmp (sxt/zxt A) (int B))
            // VVV
            // (cmp A (int B))
            if (a->type == TB_SIGN_EXT && b->type == TB_SIGN_EXT) {
                OPTIMIZER_LOG(n, "removed unnecessary sign extension");
                TB_DataType dt = a->inputs[0]->dt;

                n->inputs[0] = a->inputs[0];
                n->inputs[1] = b->inputs[0];
                TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = dt);
                return true;
            } else if (a->type == TB_ZERO_EXT && b->type == TB_ZERO_EXT) {
                OPTIMIZER_LOG(n, "removed unnecessary zero extension");
                TB_DataType dt = a->inputs[0]->dt;

                n->inputs[0] = a->inputs[0];
                n->inputs[1] = b->inputs[0];
                TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = dt);
                return true;
            } else if (a->type == TB_SIGN_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(a->inputs[0]->dt, b->dt)) {
                OPTIMIZER_LOG(n, "removed unnecessary sign extension for compare against constants");

                n->inputs[0] = a->inputs[0];
                return true;
            } else if (a->type == TB_ZERO_EXT && b->type == TB_INTEGER_CONST && TB_DATA_TYPE_EQUALS(a->inputs[0]->dt, b->dt)) {
                OPTIMIZER_LOG(n, "removed unnecessary zero extension for compare against constants");

                n->inputs[0] = a->inputs[0];
                return true;
            }
        }
    }

    return false;
}

static bool simplify_pointers(TB_Function* f, TB_Node* n) {
    TB_NodeTypeEnum type = n->type;

    if (type == TB_MEMBER_ACCESS) {
        TB_Node* base = n->inputs[0];
        int64_t offset = TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset;

        if (base->type == TB_MEMBER_ACCESS) {
            offset += TB_NODE_GET_EXTRA_T(base, TB_NodeMember)->offset;

            if (!TB_FITS_INTO(int32_t, offset)) {
                OPTIMIZER_LOG(n, "FAILURE cannot fold into member access without overflow");
            } else {
                TB_Node* base_base = base->inputs[0];

                n->inputs[0] = base_base;
                TB_NODE_SET_EXTRA(n, TB_NodeMember, .offset = offset);
                return true;
            }
        } else if (offset == 0) {
            OPTIMIZER_LOG(n, "elided member access to first element");

            tb_transmute_to_pass(n, base);
            return true;
        }
    } else if (type == TB_ARRAY_ACCESS) {
        TB_Node* base = n->inputs[0];
        TB_Node* index = n->inputs[1];
        int64_t stride = TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;

        if (index->type == TB_INTEGER_CONST && TB_NODE_GET_EXTRA_T(index, TB_NodeInt)->num_words == 1) {
            int64_t index_imm = TB_NODE_GET_EXTRA_T(index, TB_NodeInt)->words[0];

            int64_t res = stride * index_imm;
            if (!TB_FITS_INTO(int32_t, res)) {
                OPTIMIZER_LOG(n, "FAILURE cannot fold into array access without overflow");
            } else {
                // success!
                OPTIMIZER_LOG(n, "folded constant array access");

                tb_todo();
                /* n->type = TB_MEMBER_ACCESS;
                n->inputs[0] = base;
                n->member_access.base = base_reg;
                n->member_access.offset = res; */
                return true;
            }
        } else if (tb_node_is_constant_zero(index)) {
            OPTIMIZER_LOG(n, "elided array access to first element");

            tb_transmute_to_pass(n, base);
            return true;
        } else if (index->type == TB_MUL) {
            TB_Node* potential_constant = index->inputs[1];

            if (potential_constant->type == TB_INTEGER_CONST && TB_NODE_GET_EXTRA_T(potential_constant, TB_NodeInt)->num_words == 1) {
                // don't worry it doesn't loop i just needed to have 'break' support
                int64_t factor = TB_NODE_GET_EXTRA_T(potential_constant, TB_NodeInt)->words[0];
                if (!TB_FITS_INTO(int32_t, factor)) {
                    OPTIMIZER_LOG(n, "FAILURE multiply cannot fold into array access because too big");
                    goto fail;
                }

                int64_t res = stride * factor;
                if (!TB_FITS_INTO(int32_t, res)) {
                    OPTIMIZER_LOG(n, "FAILURE multiply cannot fold into array access without overflow");
                    goto fail;
                }

                // success!
                OPTIMIZER_LOG(n, "folded multiply into array access");
                n->inputs[1] = index->inputs[0];
                TB_NODE_SET_EXTRA(n, TB_NodeArray, .stride = res);
                return true;

                fail:;
            }
        } else if (index->type == TB_ADD) {
            // (array A (add B O) C) => (member (array A B C) O*C)
            TB_Node* potential_constant = index->inputs[1];

            if (potential_constant->type == TB_INTEGER_CONST && TB_NODE_GET_EXTRA_T(potential_constant, TB_NodeInt)->num_words == 1) {
                int64_t res = TB_NODE_GET_EXTRA_T(potential_constant, TB_NodeInt)->words[0];
                res *= stride;

                if (res < UINT32_MAX) {
                    OPTIMIZER_LOG(n, "converted add into member access");
                    tb_todo();

                    #if 0
                    TB_Label bb2 = tb_find_label_from_reg(f, n->inputs[1]);
                    TB_Reg new_array_reg = tb_insert_node(f, bb2, n->inputs[1]);

                    TB_Reg a = n->inputs[0];
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
                    return true;
                    #endif
                }
            }
        }
    } else if (type == TB_INT2PTR) {
        #if 0
        TB_Node* src = &f->nodes[n->unary.src];

        if (src->type == TB_INTEGER_CONST && src->integer.num_words == 1) {
            OPTIMIZER_LOG(n, "constant int2ptr removed.");

            uint64_t imm = src->integer.single_word;

            n->type = TB_INTEGER_CONST;
            // preserve the int2ptr's pointer type
            n->integer.num_words = 1;
            n->integer.single_word = imm;
            return true;
        }
        #endif
    }

    return false;
}















#if 0
#include "../tb_internal.h"
#include "cse.h"
#include "fold.h"

#define NL_MAP_IMPL
#include "../hash_map.h"

static bool compact_regs(TB_Function* f) {
    int changes = 0;
    TB_Node* nodes = f->nodes;

    // Find a NULL, skip over any NULLs until a valid node is found and cut out the middle men
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_Reg prev = f->bbs[bb].start;

        for (TB_Reg r = prev; r != 0; prev = n, r = nodes[r].next) {
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

static bool inst_combine(TB_Function* f) {
    int changes = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            TB_Node* n = &f->nodes[r];



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
            else if (type == TB_IF) {
                TB_Node* cond = &f->nodes[n->if_.cond];

                if (cond->type == TB_GET_SYMBOL_ADDRESS) {
                    // (if (sym B) 0) => (goto B)
                    TB_Label new_target = n->if_.if_true;

                    n->type = TB_GOTO;
                    n->dt = TB_TYPE_VOID;
                    n->goto_.label = new_target;
                    changes++;
                } else if (cond->type == TB_INTEGER_CONST) {
                    // (if A B C) => (goto X) where X = A ? B : C
                    TB_Label new_target = !tb_node_is_constant_zero(n->if_.cond) ?
                        n->if_.if_true : n->if_.if_false;

                    n->type = TB_GOTO;
                    n->dt = TB_TYPE_VOID;
                    n->goto_.label = new_target;
                    changes++;
                } else if (cond->type == TB_CMP_NE && tb_node_is_constant_zero(f, cond->cmp.b)) {
                    // (if (cmpne A 0) B C) => (if A B C)
                    OPTIMIZER_LOG(n, "removed redundant compare-to-zero on if node");

                    TB_DataType dt = f->nodes[cond->cmp.a].dt;

                    n->dt = dt;
                    n->if_.cond = cond->cmp.a;
                    changes++;
                } else if (cond->type == TB_CMP_EQ && tb_node_is_constant_zero(f, cond->cmp.b)) {
                    // (if (cmpeq A 0) B C) => (if A C B)
                    OPTIMIZER_LOG(n, "removed redundant compare-to-zero on if node");

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
#endif
