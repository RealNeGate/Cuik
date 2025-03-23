// Basic block placement, we've got two options:
//
// * RPO-style: just uses the simple RPO walk except we
//              move the main return block to the bottom.
//
// * Trace-style: expand blocks into traces based on branch
//                frequency to reduce the number of static
//                misprediction.
//
// We don't place the "forwarded" blocks because they don't actually appear in the final schedule.
int bb_placement_rpo(TB_Arena* arena, TB_CFG* cfg, int* dst_order) {
    size_t bb_count = aarray_length(cfg->blocks);

    size_t j = 0;
    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].fwd != i) { continue; }
        if (cfg->blocks[i].end->type != TB_RETURN) {
            dst_order[j++] = i;
        }
    }

    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].fwd != i) { continue; }
        if (cfg->blocks[i].end->type == TB_RETURN) {
            dst_order[j++] = i;
        }
    }
    return j;
}

#if 1
typedef struct Trace {
    int id;
    int first_bb;
    int last_bb;
    int dom_depth;

    bool complete;

    // I'm gonna assume the first
    // block's frequency is a good metric
    float freq;
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

static int trace_cmp(const void* a, const void* b) {
    const Trace* aa = *(const Trace**) a;
    const Trace* bb = *(const Trace**) b;

    // can't reorder the entry block
    if (aa->first_bb != 0 && bb->first_bb != 0) {
        // it's not unsurprising to get the exact same
        // frequency, synthetic frequencies do it all the
        // time and it which case we'll order by the RPO.
        /*if (aa->freq != bb->freq) {
            return aa->freq > bb->freq ? -1 : 1;
        }*/
    }

    return aa->first_bb - bb->first_bb;
}

int bb_placement_trace(TB_Arena* arena, TB_CFG* cfg, int* dst_order) {
    size_t bb_count = aarray_length(cfg->blocks);

    TraceScheduler traces = { .cfg = cfg };
    traces.block_to_trace = tb_arena_alloc(arena, bb_count * sizeof(Trace*));
    traces.next_block = tb_arena_alloc(arena, bb_count * sizeof(int));

    // initialize the most degen traces
    size_t trace_count = 0;
    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].fwd != i) {
            // forwarding blocks don't go into traces
            traces.block_to_trace[i] = NULL;
        } else {
            Trace* trace = tb_arena_alloc(arena, bb_count * sizeof(Trace));
            trace->id = trace_count++;
            trace->first_bb = trace->last_bb = i;
            traces.block_to_trace[i] = trace;
        }
        traces.next_block[i] = -1;
    }
    TB_ASSERT(trace_count <= bb_count);

    // place starting point and any missed BBs will be placed on a worklist to attempt
    // trace growing when we can't grow the main trace anymore.
    int ws_cnt = 0;
    int* ws = tb_arena_alloc(arena, bb_count * sizeof(int));
    bool* visited = tb_arena_alloc(arena, bb_count);
    FOR_N(i, 0, bb_count) { visited[i] = false; }

    // place entry
    ws[ws_cnt++] = 0;

    int order_cnt = 0;
    Trace** order = tb_arena_alloc(arena, bb_count * sizeof(Trace*));

    // grow traces by placing likely traces next to them
    while (ws_cnt > 0) {
        int first_bb = ws[--ws_cnt];
        Trace* trace = traces.block_to_trace[first_bb];
        // if the block was already stitched somewhere else we can't start a trace here
        if (trace->first_bb != first_bb) { continue; }

        TB_OPTDEBUG(PLACEMENT)(printf("Grow Trace %d?\n", trace->id));

        trace->freq = cfg->blocks[first_bb].freq;
        order[order_cnt++] = trace;

        // extend the end of the trace until we hit an unlikely case or a backedge
        int curr = trace->last_bb;
        TB_BasicBlock* succ;
        do {
            TB_BasicBlock* bb = &cfg->blocks[trace->last_bb];

            #if TB_OPTDEBUG_PLACEMENT
            printf("  BB%-3d (freq = %8.4f)  =>", curr, bb->freq);
            FOR_SUCC(it, bb->end) {
                TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);
                printf("  BB%-3d", succ_bb->fwd);
            }
            printf("\n");
            #endif

            // find best successor
            succ = best_successor_of(cfg, bb->end);
            if (succ != NULL &&
                // it's not worth growing the trace if the next block is really low-freq
                succ->freq >= 1e-3 &&
                // can't do backedges in traces
                succ->fwd > curr &&
                // it's only an option if the block is the start of it's trace
                traces.block_to_trace[succ->fwd]->first_bb == succ->fwd &&
                // and it's not part of a completed trace
                !traces.block_to_trace[succ->fwd]->complete
            ) {
                trace_append(&traces, curr, succ->fwd);
                curr = succ->fwd;
            } else {
                succ = NULL;
            }

            // place the missed blocks in the worklist
            FOR_SUCC(it, bb->end) {
                TB_BasicBlock* bb = nl_map_get_checked(cfg->node_to_block, it.succ);
                if (bb == succ) { continue; }
                Trace* t = traces.block_to_trace[bb->fwd];
                // it's only an option if the block is the start of it's trace
                if (t->first_bb != bb->fwd) { continue; }
                // if it's already in the worklist, we don't need to add it again
                if (visited[bb->fwd]) { continue; }

                TB_OPTDEBUG(PLACEMENT)(printf("    Postpone BB%d\n", bb->fwd));

                TB_ASSERT(ws_cnt < bb_count);
                ws[ws_cnt++] = bb->fwd;
                visited[bb->fwd] = true;
            }
        } while (succ != NULL);

        trace->complete = true;
    }

    qsort(order, order_cnt, sizeof(Trace*), trace_cmp);

    // final placement
    int j = 0;
    FOR_N(i, 0, order_cnt) {
        Trace* trace = order[i];
        int curr = trace->first_bb;
        do {
            TB_ASSERT(cfg->blocks[curr].fwd == curr);
            dst_order[j++] = curr;
            curr = traces.next_block[curr];
        } while (curr >= 0);
    }
    return j;
}
#endif

