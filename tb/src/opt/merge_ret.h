
static bool merge_rets(TB_Function* f, TB_TemporaryStorage* tls) {
    // array of all returns
    size_t count = 0;
    TB_PhiInput* arr = tb_tls_push(tls, 0);
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_RET) {
                tb_tls_push(tls, sizeof(TB_PhiInput));
                arr[count++] = (TB_PhiInput){ bb, n };
            }
        }
    }

    if (count > 1) {
        TB_Label bb = tb_basic_block_create(f);

        TB_Node* phi = tb_alloc_node(f, TB_PHI, arr[0].val->dt, count, count * sizeof(TB_Label));
        OPTIMIZER_LOG(phi, "Insert new PHI node");

        // fill inputs
        TB_NodePhi* extra = TB_NODE_GET_EXTRA(phi);
        FOREACH_N(i, 0, count) {
            phi->inputs[i] = arr[i].val;
            extra->labels[i] = arr[i].label;

            // convert returns into GOTOs (return is smaller than GOTO so we can't transmute)
            TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_VOID, 0, sizeof(TB_NodeBranch));
            TB_NODE_SET_EXTRA(n, TB_NodeBranch, .default_label = bb);

            // kill old return
            TB_KILL_NODE(arr[i].val);

            // attach as new BB end
            arr[i].val->next = n;
            f->bbs[arr[i].label].end = n;
        }

        TB_Node* ret = tb_alloc_node(f, TB_RET, phi->dt, 1, 0);
        ret->inputs[0] = phi;

        // attach together
        phi->next = ret;
        f->bbs[bb].start = phi;
        f->bbs[bb].end = ret;
        return true;
    }

    return false;
}

const TB_Pass tb_opt_merge_rets = {
    .name = "MergeReturns",
    .func_run = merge_rets,
};
