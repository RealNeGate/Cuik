// Basic block placement, we've got two options:
// * RPO-style: just uses the simple RPO walk except we
//              move the main return block to the bottom.
//
// * Trace-style: expand blocks into traces based on branch
//                frequency to reduce the number of static
//                misprediction.
void bb_placement_rpo(TB_Arena* arena, TB_CFG* cfg, int* dst_order) {
    size_t bb_count = aarray_length(cfg->blocks);

    size_t j = 0;
    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].end->type != TB_RETURN) {
            dst_order[j++] = i;
        }
    }

    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].end->type == TB_RETURN) {
            dst_order[j++] = i;
        }
    }
    TB_ASSERT_MSG(bb_count == j, "did we forget to schedule a BB?");
}

#if 0
static TB_Node* best_successor_of(TB_Node* n, float* confidence, float* payment) {
    if (cfg_is_endpoint(n)) {
        return NULL;
    } else if (!cfg_is_fork(n)) {
        // straightline and always trying to combine
        TB_Node* succ = cfg_next_control(n);

        *confidence = 1.0f;
        *payment = (succ->input_count - 1) * 0.25f;
        return succ;
    }

    TB_Node* best = NULL;
    float best_score = 0;

    if (cfg_is_branch(n)) {
        int total_hits = 0;
        FOR_USERS(u, n) {
            if (cfg_is_cproj(USERN(u))) {
                int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                TB_Node* succ = USERN(u);
                if () {

                }
            }
        }
    } else {
        // pick CProj0 is there's no other info
        best = USERN(proj_with_index(n, 0));
    }

    *payment = (best->input_count - 1) * 0.25f;
    return best;
}

void bb_placement_trace(TB_Arena* arena, TB_CFG* cfg, int* dst_order) {
    size_t bb_count = aarray_length(cfg->blocks);
    bool* visited = tb_arena_alloc(arena, bb_count);
    FOR_N(i, 0, bb_count) { visited[i] = false; }

    int j = 0;

    // start from the entry block, we only expand trace via successors
    // but there's reasons why expanding by preds would be good, we also
    // don't try to do fallthru for backedges (succ < bb with our RPOrdering)
    FOR_N(i, 0, bb_count) {
        // if we're in a block that's unvisited we can expand out a trace
        // until we don't think there's a strong case for the trace expanding or
        // we hit a visited block.
        int curr = i;
        float cost = 0.5f;

        while (!visited[curr]) {
            visited[curr] = true;
            dst_order[j++] = curr;

            // find best successor
            bb = &cfg->blocks[curr];

            float confidence, payment;
            TB_Node* succ = best_successor_of(bb->end, &confidence, &payment);

            cost += payment;
            if (confidence - cost > 0.0f) {
                continue;
            }

            curr = nl_map_get_checked(cfg->node_to_bb, succ) - cfg->blocks;
        }
    }
}
#endif
