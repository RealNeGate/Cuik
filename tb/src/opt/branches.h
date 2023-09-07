
static TB_Node* ideal_region(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

    // if there's one predecessor and it's to an unconditional branch, merge them.
    if (n->input_count == 1 && n->inputs[0]->type == TB_PROJ &&
        n->inputs[0]->inputs[0]->type == TB_BRANCH &&
        n->inputs[0]->inputs[0]->input_count == 1) {
        TB_Node* top_node = unsafe_get_region(n->inputs[0]);
        TB_NodeRegion* top_region = TB_NODE_GET_EXTRA(top_node);

        // set new terminator
        top_region->end = r->end;
        TB_Node* parent = n->inputs[0]->inputs[0]->inputs[0];

        tb_pass_kill_node(p, n->inputs[0]->inputs[0]);
        tb_pass_kill_node(p, n->inputs[0]);

        return parent;
    }

    // if a region is dead, dettach it's succesors
    if (n->input_count == 0 && r->end->type == TB_BRANCH) {
        TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
        FOREACH_N(i, 0, br->succ_count) {
            TB_Node* succ = br->succ[i];
            if (succ->input_count > 0) {
                remove_pred(p, f, n, succ);

                tb_pass_mark(p, succ);
                tb_pass_mark_users(p, succ);
            }
        }

        br->succ_count = 0;
        return br->succ_count ? n : NULL;
    }

    return NULL;
}

static void transmute_goto(TB_Passes* restrict opt, TB_Function* f, TB_Node* br, TB_Node* dst) {
    assert(br->type == TB_BRANCH && dst->input_count >= 1);

    // convert to unconditional branch
    set_input(opt, br, NULL, 1);
    br->input_count = 1;

    // remove predecessor from other branches
    TB_Node* bb = unsafe_get_region(br);
    TB_NodeBranch* br_info = TB_NODE_GET_EXTRA(br);
    FOREACH_N(i, 0, br_info->succ_count) {
        remove_pred(opt, f, bb, br_info->succ[i]);
    }
    br_info->succ_count = 1;

    // construct new projection for the branch
    TB_Node* proj = tb_alloc_node(f, TB_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeProj));
    set_input(opt, proj, br, 0);
    TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = 0);

    dst->input_count = 1;
    set_input(opt, dst, proj, 0);

    // set new successor
    br_info->succ_count = 1;
    br_info->succ[0] = dst;

    // we need to mark the changes to that jump
    // threading can clean it up
    tb_pass_mark(opt, bb);
    tb_pass_mark(opt, proj);
    tb_pass_mark(opt, dst);
    tb_pass_mark_users(opt, bb);
}

static TB_Node* ideal_phi(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    // degenerate PHI, poison it
    if (n->input_count == 1) {
        log_warn("%s: ir: generated poison due to PHI with no edges", f->super.name);
        return make_poison(f, opt, n->dt);
    }

    // if branch, both paths are empty => select(cond, t, f)
    TB_DataType dt = n->dt;
    TB_Node* region = n->inputs[0];
    if (region->input_count == 2) {
        // for now we'll leave multi-phi scenarios alone, we need
        // to come up with a cost-model around this stuff.
        int phi_count = 0;
        for (User* use = find_users(opt, region); use; use = use->next) {
            if (use->n->type == TB_PHI) {
                if (use->n->dt.type == TB_MEMORY) return NULL;
                phi_count++;
            }
            if (phi_count > 1) return NULL;
        }

        // guarentee paths are effectless
        if (!is_empty_bb(opt, region->inputs[0]->inputs[0])) { return NULL; }
        if (!is_empty_bb(opt, region->inputs[1]->inputs[0])) { return NULL; }

        // these don't have directions, i just need names
        TB_Node* left  = region->inputs[0]->inputs[0]->inputs[0];
        TB_Node* right = region->inputs[1]->inputs[0]->inputs[0];

        // is it a proper if-diamond?
        if (left->input_count == 1 && right->input_count == 1 &&
            left->inputs[0]->inputs[0]->type == TB_BRANCH &&
            left->inputs[0]->inputs[0] == right->inputs[0]->inputs[0]) {
            TB_Node* branch = left->inputs[0]->inputs[0];
            TB_NodeBranch* header_br = TB_NODE_GET_EXTRA(branch);

            if (header_br->succ_count == 2) {
                assert(left->inputs[0]->inputs[0]->input_count == 2);
                TB_Node* cond    = branch->inputs[1];
                TB_Node* left_v  = n->inputs[1];
                TB_Node* right_v = n->inputs[2];

                bool right_false = header_br->succ[0] == right;
                uint64_t falsey = TB_NODE_GET_EXTRA_T(branch, TB_NodeBranch)->keys[0];

                // TODO(NeGate): handle non-zero falseys
                if (falsey == 0) {
                    // header -> merge
                    transmute_goto(opt, f, branch, region);

                    TB_Node* selector = tb_alloc_node(f, TB_SELECT, dt, 4, 0);
                    set_input(opt, selector, cond, 1);
                    set_input(opt, selector, left_v, 2 + right_false);
                    set_input(opt, selector, right_v, 2 + !right_false);

                    return selector;
                }
            }
        }
    }

    return NULL;
}

static TB_Node* ideal_branch(TB_Passes* restrict opt, TB_Function* f, TB_Node* n) {
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

    if (br->succ_count == 2) {
        if (n->input_count == 2 && br->keys[0] == 0) {
            TB_Node* cmp_node = n->inputs[1];
            TB_NodeTypeEnum cmp_type = cmp_node->type;

            // empty BB, just does if branch but the condition is effect-less
            // if (a && b) A else B => if (a ? b : 0) A else B
            //
            // TODO(NeGate): implement form which works on an arbitrary falsey
            if (is_empty_bb(opt, n)) {
                TB_Node* bb = n->inputs[0];
                assert(bb->type == TB_REGION || bb->type == TB_START);

                uint64_t falsey = br->keys[0];
                TB_Node* pred_branch = bb->inputs[0]->inputs[0];

                // needs one pred
                uint64_t pred_falsey;
                if (bb->input_count == 1 && is_if_branch(pred_branch, &pred_falsey)) {
                    TB_NodeBranch* pred_br_info = TB_NODE_GET_EXTRA(pred_branch);

                    bool bb_on_false = pred_br_info->succ[0] != bb;
                    TB_Node* shared_edge = pred_br_info->succ[!bb_on_false];

                    int shared_i = -1;
                    if (shared_edge == pred_br_info->succ[0]) shared_i = 0;
                    if (shared_edge == pred_br_info->succ[1]) shared_i = 1;

                    if (shared_i >= 0) {
                        TB_Node* pred_cmp = pred_branch->inputs[1];

                        // convert first branch into an unconditional into bb
                        transmute_goto(opt, f, pred_branch, bb);

                        // we wanna normalize into a comparison (not a boolean -> boolean)
                        if (!(pred_cmp->dt.type == TB_INT && pred_cmp->dt.data == 1)) {
                            assert(pred_cmp->dt.type != TB_FLOAT && "TODO");
                            TB_Node* imm = make_int_node(f, opt, pred_cmp->dt, pred_falsey);
                            tb_pass_mark(opt, imm);

                            TB_Node* new_node = tb_alloc_node(f, TB_CMP_NE, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
                            set_input(opt, new_node, pred_cmp, 1);
                            set_input(opt, new_node, imm, 2);
                            TB_NODE_SET_EXTRA(new_node, TB_NodeCompare, .cmp_dt = pred_cmp->dt);

                            tb_pass_mark(opt, new_node);
                            pred_cmp = new_node;
                        }

                        TB_Node* false_node = make_int_node(f, opt, n->inputs[1]->dt, falsey);
                        tb_pass_mark(opt, false_node);

                        // a ? b : 0
                        TB_Node* selector = tb_alloc_node(f, TB_SELECT, n->inputs[1]->dt, 4, 0);
                        set_input(opt, selector, pred_cmp, 1);
                        set_input(opt, selector, n->inputs[1], 2 + bb_on_false);
                        set_input(opt, selector, false_node, 2 + !bb_on_false);

                        set_input(opt, n, selector, 1);
                        tb_pass_mark(opt, selector);
                        return n;
                    }
                }
            }

            // br ((y <= x)) => br (x < y) flipped conditions
            if (cmp_type == TB_CMP_SLE || cmp_type == TB_CMP_ULE) {
                TB_Node* new_cmp = tb_alloc_node(f, cmp_type == TB_CMP_SLE ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
                set_input(opt, new_cmp, cmp_node->inputs[2], 1);
                set_input(opt, new_cmp, cmp_node->inputs[1], 2);
                TB_NODE_SET_EXTRA(new_cmp, TB_NodeCompare, .cmp_dt = TB_NODE_GET_EXTRA_T(cmp_node, TB_NodeCompare)->cmp_dt);

                SWAP(TB_Node*, br->succ[0], br->succ[1]);
                set_input(opt, n, new_cmp, 1);
                tb_pass_mark(opt, new_cmp);
                return n;
            }

            // br ((x != y) != 0) => br (x != y)
            if ((cmp_type == TB_CMP_NE || cmp_type == TB_CMP_EQ) && cmp_node->inputs[2]->type == TB_INTEGER_CONST) {
                uint64_t imm = TB_NODE_GET_EXTRA_T(cmp_node->inputs[2], TB_NodeInt)->value;
                set_input(opt, n, cmp_node->inputs[1], 1);
                br->keys[0] = imm;

                // flip successors
                if (cmp_type == TB_CMP_EQ) {
                    SWAP(TB_Node*, br->succ[0], br->succ[1]);
                }
                return n;
            }
        }
    }

    // constant fold branch
    /*if (n->input_count == 2) {
        uint64_t key;
        if (get_int_const(n->inputs[1], &key)) {
            size_t taken = 0;
            FOREACH_N(i, 0, br->succ_count - 1) {
                uint64_t case_key = br->keys[i];
                if (key == case_key) { taken = i + 1; break; }
            }

            TB_Node* dead = make_dead(f, opt);

            // convert dead projections into DEAD and convert live projection into index 0
            for (User* use = find_users(opt, n); use; use = use->next) {
                if (use->n->type == TB_PROJ) {
                    int index = TB_NODE_GET_EXTRA_T(use->n, TB_NodeProj)->index;
                    if (index != taken) {
                        subsume_node(opt, f, use->n, dead);
                    } else {
                        TB_NODE_GET_EXTRA_T(use->n, TB_NodeProj)->index = 0;

                        User* proj_use = find_users(opt, use->n);
                        assert(proj_use->next == NULL && "control projection has conflicts?");
                        assert(proj_use->n->type == TB_REGION);

                        br->succ_count = 1;
                        br->succ[0] = proj_use->n;
                    }
                }
            }
            assert(br->succ_count == 1);

            // remove condition
            set_input(opt, n, NULL, 1);
            n->input_count = 1;
            return n;
        }
    }

    // check if it's a dead region
    TB_Node* parent = unsafe_get_region(n);
    if (parent->input_count == 0 && br->succ_count != 0) {
        // remove predecessor from successors
        TB_Node* dead = make_dead(f, opt);
        for (User* use = find_users(opt, n); use; use = use->next) {
            if (use->n->type == TB_PROJ) {
                subsume_node(opt, f, use->n, dead);
            }
        }

        br->succ_count = 0;
        return n;
    }*/

    return NULL;
}
