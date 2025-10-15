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
typedef struct Edge {
    int start_bb, end_bb;
    float freq;
} Edge;

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
    int* uf;
    int* next_block;

    ArenaArray(Edge) edges;
} TraceScheduler;

static Trace* trace_of(TraceScheduler* traces, int bb) {
    bb = uf_find(traces->uf, aarray_length(traces->cfg->blocks), bb);
    return traces->block_to_trace[bb];
}

static void trace_join(TraceScheduler* traces, int a, int b) {
    TB_ASSERT(traces->block_to_trace[a]->last_bb == a);
    TB_ASSERT(traces->block_to_trace[b]->first_bb == b);

    int first = trace_of(traces, a)->first_bb;
    int last  = trace_of(traces, b)->last_bb;
    traces->next_block[a] = b;

    int head = uf_union(traces->uf, a, b);
    traces->block_to_trace[head]->first_bb = first;
    traces->block_to_trace[head]->last_bb = last;
}

static int edge_cmp(const void* a, const void* b) {
    const Edge* aa = (const Edge*) a;
    const Edge* bb = (const Edge*) b;
    if (aa->freq != bb->freq) {
        return aa->freq > bb->freq ? -1 : 1;
    }

    if (aa->start_bb != bb->start_bb) {
        return aa->start_bb > bb->start_bb ? -1 : 1;
    }

    return aa->end_bb - bb->end_bb;
}

static int trace_cmp(const void* a, const void* b) {
    const Trace* aa = *(const Trace**) a;
    const Trace* bb = *(const Trace**) b;

    // can't reorder the entry block
    if (aa->first_bb != 0 && bb->first_bb != 0) {
        // it's not unsurprising to get the exact same
        // frequency, synthetic frequencies do it all the
        // time and it which case we'll order by the RPO.
        if (fabsf(aa->freq - bb->freq) > 0.04f) {
            return aa->freq > bb->freq ? -1 : 1;
        }
    }

    return aa->first_bb - bb->first_bb;
}

int bb_placement_trace(TB_Arena* arena, TB_CFG* cfg, int* dst_order) {
    size_t bb_count = aarray_length(cfg->blocks);

    TraceScheduler traces = { .cfg = cfg };
    traces.block_to_trace = tb_arena_alloc(arena, bb_count * sizeof(Trace*));
    traces.next_block = tb_arena_alloc(arena, bb_count * sizeof(int));
    traces.edges = aarray_create(arena, Edge, bb_count);
    traces.uf = tb_arena_alloc(arena, bb_count * sizeof(int));

    // initialize the most degen traces
    size_t trace_count = 0;
    FOR_N(i, 0, bb_count) {
        if (cfg->blocks[i].fwd != i) {
            // forwarding blocks don't go into traces
            traces.uf[i] = cfg->blocks[i].fwd;
            traces.block_to_trace[i] = NULL;
        } else {
            TB_BasicBlock* bb = &cfg->blocks[i];

            Trace* trace = tb_arena_alloc(arena, bb_count * sizeof(Trace));
            trace->id = trace_count++;
            trace->first_bb = trace->last_bb = i;
            trace->freq = bb->freq;

            traces.uf[i] = i;
            traces.block_to_trace[i] = trace;

            // add edges
            float freq = bb->freq;
            if (tb_node_is_fork_ctrl(bb->end)) {
                FOR_SUCC(it, bb->end) {
                    TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);

                    Edge edge;
                    edge.start_bb = i;
                    edge.end_bb   = succ_bb->fwd;
                    edge.freq     = freq * tb_edge_prob(it.succ);
                    aarray_push(traces.edges, edge);
                }
            } else if (!cfg_is_endpoint(bb->end)) {
                TB_Node* succ = USERN(cfg_next_user(bb->end));
                TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, succ);

                Edge edge;
                edge.start_bb = i;
                edge.end_bb   = succ_bb->fwd;
                edge.freq     = freq;
                aarray_push(traces.edges, edge);
            }
        }
        traces.next_block[i] = -1;
    }
    TB_ASSERT(trace_count <= bb_count);

    // sort by hottest edge
    qsort(traces.edges, aarray_length(traces.edges), sizeof(Edge), edge_cmp);

    #if TB_OPTDEBUG_PLACEMENT
    printf("== EDGES ==\n");
    for (int i = 0; i < aarray_length(traces.edges); i++) {
        Edge edge = traces.edges[i];
        printf("  BB%-3d -> BB%-3d (%f)\n", edge.start_bb, edge.end_bb, edge.freq);
    }
    #endif

    for (int i = 0; i < aarray_length(traces.edges); i++) {
        Edge e = traces.edges[i];

        // ignore backedges
        if (e.start_bb >= e.end_bb) {
            continue;
        }

        Trace* curr = trace_of(&traces, e.start_bb);
        Trace* next = trace_of(&traces, e.end_bb);
        // we've already been placed in the middle of a trace
        if (curr->last_bb != e.start_bb || next->first_bb != e.end_bb) {
            continue;
        }

        // try to join these two traces
        TB_BasicBlock* start_bb = &cfg->blocks[curr->last_bb];
        TB_BasicBlock* end_bb   = &cfg->blocks[next->first_bb];
        trace_join(&traces, curr->last_bb, next->first_bb);
    }

    int order_cnt = 0;
    Trace** order = tb_arena_alloc(arena, bb_count * sizeof(Trace*));

    FOR_N(i, 0, bb_count) {
        if (traces.uf[i] == i) {
            order[order_cnt++] = traces.block_to_trace[i];
        }
    }

    qsort(order, order_cnt, sizeof(Trace*), trace_cmp);

    // final placement
    int j = 0;
    FOR_N(i, 0, order_cnt) {
        Trace* trace = order[i];
        TB_OPTDEBUG(PLACEMENT)(printf("Trace:\n"));

        int curr = trace->first_bb;
        do {
            TB_ASSERT(cfg->blocks[curr].fwd == curr);
            TB_OPTDEBUG(PLACEMENT)(printf("  BB%-3d (freq=%f)\n", curr, cfg->blocks[curr].freq));

            dst_order[j++] = curr;
            curr = traces.next_block[curr];
        } while (curr >= 0);
    }
    return j;
}
#endif

