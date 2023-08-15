
static TB_Node* ideal_phi(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    return NULL;
}

static TB_Node* ideal_branch(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_Node* bb = tb_get_parent_region(n);
    TB_NodeRegion* region = TB_NODE_GET_EXTRA(bb);
    if (region->succ_count != 2) return NULL;

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    if (n->input_count == 2 && br->keys[0] == 0) {
        TB_Node* cmp_node = n->inputs[1];
        TB_NodeTypeEnum cmp_type = cmp_node->type;

        // br ((y <= x)) => br (x < y) flipped conditions
        if (cmp_type == TB_CMP_SLE || cmp_type == TB_CMP_ULE) {
            TB_Node* new_cmp = tb_alloc_node(f, cmp_type == TB_CMP_SLE ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
            set_input(opt, new_cmp, cmp_node->inputs[2], 1);
            set_input(opt, new_cmp, cmp_node->inputs[1], 2);
            TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(cmp_node, TB_NodeCompare)->cmp_dt);

            SWAP(TB_Node*, region->succ[0], region->succ[1]);
            set_input(opt, n, new_cmp, 1);
            return n;
        }

        // br ((x != y) != 0) => br (x != y)
        if ((cmp_type == TB_CMP_NE || cmp_type == TB_CMP_EQ) && cmp_node->inputs[2]->type == TB_INTEGER_CONST) {
            TB_NodeInt* i = TB_NODE_GET_EXTRA(cmp_node->inputs[2]);
            if (i->num_words == 1) {
                set_input(opt, n, cmp_node->inputs[1], 1);
                br->keys[0] = i->words[0];

                // flip successors
                if (cmp_type == TB_CMP_EQ) {
                    SWAP(TB_Node*, region->succ[0], region->succ[1]);
                }
                return n;
            }
        }
    }

    return NULL;
}

bool tb_pass_cfg(TB_Passes* opt) {
    #if 0
    // walk dominators to see if we've already checked this condition
    TB_Node* other_bb = bb;
    while (0 && other_bb != f->start_node) retry: {
        other_bb = idom(opt->doms, other_bb);

        TB_NodeRegion* other_region = TB_NODE_GET_EXTRA(other_bb);
        TB_Node* latch = other_region->end;
        if (latch->type != TB_BRANCH) break;
        if (latch->input_count != 2) continue;

        bool hit = false;
        ptrdiff_t j = -1;
        FOREACH_N(i, 0, other_region->succ_count) {
            if (tb_is_dominated_by(opt->doms, other_region->succ[i], bb)) {
                // duplicates means ambiguous which we can't know much about
                if (j >= 0) goto retry;
                j = i;
            }
        }

        TB_NodeBranch* latch_br = TB_NODE_GET_EXTRA(latch);
        if (j >= 0 && latch->inputs[1] == n->inputs[1] && latch_br->keys[0] == br->keys[0]) {
            TB_Node* dead_block = region->succ[1 - j];

            if (dead_block->input_count == 1) {
                // convert conditional into goto
                set_input(opt, n, NULL, 1);
                n->input_count = 1;
                region->succ_count = 1;
                region->succ[0] = region->succ[j];

                set_input(opt, dead_block, NULL, 0);
                dead_block->input_count = 0;

                // remove predecessor from dead_block's successors
                TB_NodeRegion* dead_region = TB_NODE_GET_EXTRA(dead_block);
                FOREACH_N(k, 0, dead_region->succ_count) {
                    remove_pred(opt, f, dead_block, dead_region->succ[k]);
                }

                return n;
            }
        }
    }
    #endif

    return false;
}
