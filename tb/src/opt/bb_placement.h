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
typedef struct Trace {
    int id;
    int first_bb;
    int last_bb;
} Trace;

typedef struct {
    TB_CFG* cfg;
    Trace** block_to_trace;

    // linked list
    int* next_block;
} TraceScheduler;

static Trace* trace_of(TraceScheduler* traces, TB_BasicBlock* bb) {
    return traces->block_to_trace[bb - traces->cfg->blocks];
}

static void trace_append(TraceScheduler* traces, int a, int b) {
    TB_ASSERT(traces->block_to_trace[a]->last_bb == a);
    TB_ASSERT(traces->block_to_trace[b]->first_bb == b);

    traces->next_block[a] =  b;
    traces->next_block[b] = -1;
    traces->block_to_trace[b] = traces->block_to_trace[a];
    traces->block_to_trace[b]->last_bb = b;
}

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

    TraceScheduler traces = { .cfg = cfg };
    traces.block_to_trace = tb_arena_alloc(arena, bb_count * sizeof(Trace*));
    traces.next_block = tb_arena_alloc(arena, bb_count * sizeof(int));

    // initialize the most degen traces
    FOR_N(i, 0, bb_count) {
        Trace* trace = tb_arena_alloc(arena, bb_count * sizeof(Trace));
        trace->id = trace->first_bb = trace->last_bb = i;

        traces.next_block[i] = -1;
        traces.block_to_trace[i] = trace;
    }

    // grow traces by placing likely traces next to them
    FOR_N(i, 0, bb_count) { visited[i] = false; }
    FOR_N(i, 0, bb_count) {
        Trace* trace = traces.block_to_trace[i];
        // if we've already tried to grow this trace, don't :p
        if (visited[trace->id]) { continue; }
        visited[trace->id] = true;

        TB_OPTDEBUG(PLACEMENT)(printf("Grow Trace %d?\n", trace->id));

        // extend the end of the trace until we hit an unlikely case or a backedge
        int curr = trace->last_bb;
        for (;;) {
            TB_BasicBlock* bb = &cfg->blocks[trace->last_bb];

            #if TB_OPTDEBUG_PLACEMENT
            printf("  BB%-3d (freq = %8.4f)  =>", curr, bb->freq);
            FOR_SUCC(it, bb->end) {
                TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);
                printf("  BB%-3zu", succ_bb - cfg->blocks);
            }
            printf("\n");
            #endif

            // find best successor
            TB_BasicBlock* succ = best_successor_of(cfg, bb->end);
            if (succ == NULL) { break; }
            // it's not worth growing the trace if the next block is really low-freq
            if (succ->freq < 1e-3) { break; }
            // can't do backedges in traces
            int succ_bb = succ - cfg->blocks;
            if (succ_bb <= curr) { break; }
            // it's only an option if the block is the start of it's trace
            if (traces.block_to_trace[succ_bb]->first_bb != succ_bb) { break; }

            trace_append(&traces, curr, succ_bb);
            curr = succ_bb;
        }
    }

    // final placement
    int j = 0;
    FOR_N(i, 0, bb_count) { visited[i] = false; }
    FOR_N(i, 0, bb_count) {
        Trace* trace = traces.block_to_trace[i];
        // if we've already tried to grow this trace, don't :p
        if (visited[trace->id]) { continue; }
        visited[trace->id] = true;

        int curr = trace->first_bb;
        do {
            dst_order[j++] = curr;
            curr = traces.next_block[curr];
        } while (curr >= 0);
    }
}
#endif

