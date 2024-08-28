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

#if 1
static TB_BasicBlock* best_successor_of(TB_CFG* cfg, TB_Node* n) {
    if (cfg_is_endpoint(n)) {
        return NULL;
    } else if (!cfg_is_fork(n)) {
        // straightline and always trying to combine
        TB_Node* succ = cfg_next_control(n);
        return nl_map_get_checked(cfg->node_to_block, succ);
    } else {
        // find highest freq successor (it might make sense to search in a consistent order?)
        if (cfg_is_branch(n)) {
            TB_BasicBlock* best = NULL;
            float best_freq = 0.0f;

            FOR_USERS(u, n) {
                TB_Node* succ = USERN(u);
                if (cfg_is_cproj(succ)) {
                    TB_ASSERT(succ->type == TB_BRANCH_PROJ);

                    TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, succ);
                    if (succ_bb->freq > best_freq) {
                        best = succ_bb;
                        best_freq = succ_bb->freq;
                    }
                }
            }

            return best;
        } else {
            // pick CProj0 is there's no other info
            TB_Node* succ = USERN(proj_with_index(n, 0));
            return nl_map_get_checked(cfg->node_to_block, succ);
        }
    }
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
        if (!visited[i]) {
            TB_OPTDEBUG(PLACEMENT)(printf("Trace:\n"));

            int curr = i;
            do {
                TB_BasicBlock* bb = &cfg->blocks[curr];

                #if TB_OPTDEBUG_PLACEMENT
                printf("  BB%-3d (freq = %8.4f)  =>", curr, bb->freq);
                FOR_SUCC(it, bb->end) {
                    TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);
                    printf("  BB%-3zu", succ_bb - cfg->blocks);
                }
                printf("\n");
                #endif

                visited[curr] = true;
                dst_order[j++] = curr;

                // find best successor
                TB_BasicBlock* succ = best_successor_of(cfg, bb->end);
                if (succ == NULL) { break; }

                // it's not worth growing the trace if the next block is really low-freq
                if (succ->freq < 1e-3) { break; }

                // can't revisit, but also can't do backedges in traces
                int succ_bb = succ - cfg->blocks;
                if (succ_bb < curr || visited[succ_bb]) { break; }

                curr = succ_bb;
            } while (!visited[curr]);

            // end of trace (might've literally been a single block)
        }
    }
}
#endif

