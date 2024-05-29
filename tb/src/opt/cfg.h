
typedef struct Block {
    struct Block* parent;
    TB_ArenaSavepoint sp;
    int bb_id;
    int succ_i;
    TB_Node* succ[];
} Block;

void tb_free_cfg(TB_CFG* cfg) {
    aarray_for(i, cfg->blocks) {
        nl_hashset_free(cfg->blocks[i].items);
    }
    nl_map_free(cfg->node_to_block);
}

static TB_Node* cfg_next_control0(TB_Node* n) {
    FOR_USERS(u, n) {
        if (USERI(u) == 0 && cfg_is_control(USERN(u))) {
            return USERN(u);
        }
    }

    return NULL;
}

// walks until the terminator or other critical edge
static TB_Node* end_of_bb(TB_Node* n) {
    while (!cfg_is_terminator(n)) {
        TB_Node* next = cfg_next_control0(n);
        if (next == NULL || cfg_is_region(next)) {
            break;
        }
        n = next;
    }

    return n;
}

static Block* create_block(TB_Arena* arena, TB_Node* end, int id) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    size_t succ_count = 0;
    if (end->type == TB_BRANCH) {
        succ_count = TB_NODE_GET_EXTRA_T(end, TB_NodeBranch)->succ_count;
    } else if (end->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, end) if (cfg_is_cproj(USERN(u))) {
            succ_count += 1;
        }
    } else if (!cfg_is_endpoint(end)) {
        succ_count = 1;
    }

    Block* top = tb_arena_alloc(arena, sizeof(Block) + succ_count*sizeof(TB_Node*));
    *top = (Block){
        .sp  = sp,
        .bb_id = id,
        .succ_i = succ_count,
    };

    if (cfg_is_fork(end)) {
        // this does imply the successors take up the bottom indices on the tuple... idk if thats
        // necessarily a problem tho.
        FOR_USERS(u, end) {
            if (cfg_is_cproj(USERN(u))) {
                int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                top->succ[index] = cfg_next_bb_after_cproj(USERN(u));
            }
        }
    } else if (!cfg_is_endpoint(end)) {
        top->succ[0] = USERN(cfg_next_user(end));
    }

    return top;
}

TB_CFG tb_compute_cfg(TB_Function* f, TB_Worklist* ws, TB_Arena* arena) {
    cuikperf_region_start("CFG", NULL);
    assert(dyn_array_length(ws->items) == 0);

    ////////////////////////////////
    // pre-order DFS
    ////////////////////////////////
    TB_CFG cfg = { 0 };
    worklist_push(ws, f->params[0]);

    TB_Node* n;
    while (n = worklist_pop(ws), n) {
        TB_Node* end = end_of_bb(n);

        if (cfg_is_fork(end)) {
            // this does imply the successors take up the bottom indices on the tuple... idk if thats
            // necessarily a problem tho.
            FOR_USERS(u, end) {
                if (cfg_is_cproj(USERN(u))) {
                    worklist_push(ws, cfg_next_bb_after_cproj(USERN(u)));
                }
            }
        } else if (!cfg_is_endpoint(end)) {
            worklist_push(ws, USERN(cfg_next_user(end)));
        }

        TB_BasicBlock bb = { .start = n, .end = end, .dom_depth = -1 };
        if (aarray_length(cfg.blocks) == 0) {
            bb.dom_depth = 0;
        }
        aarray_push(cfg.blocks, bb);
    }

    size_t block_count = aarray_length(cfg.blocks);
    nl_map_create(cfg.node_to_block, block_count);
    aarray_for(i, cfg.blocks) {
        nl_map_put(cfg.node_to_block, cfg.blocks[i].start, &cfg.blocks[i]);
    }

    __debugbreak();

    ////////////////////////////////
    // RPO walk
    ////////////////////////////////
    size_t block_i = block_count;
    cfg.rpo_walk = tb_arena_alloc(arena, block_count * sizeof(int));

    // push initial block
    Block* top = create_block(f->tmp_arena, blocks[0].end, 0);
    worklist_test_n_set(ws, f->params[0]);

    while (top != NULL) {
        cuikperf_region_start("rpo_iter", NULL);
        if (top->succ_i > 0) {
            // push next unvisited succ
            TB_Node* succ = top->succ[--top->succ_i];
            if (!worklist_test_n_set(ws, succ)) {
                Block* new_top = create_block(f->tmp_arena, succ);
                new_top->parent = top;
                top = new_top;
            }
        } else {
            Block* parent = top->parent;
            Block b = *top;

            assert(block_i > 0);
            cfg.rpo_walk[--block_i] = top->bb_id;

            tb_arena_restore(f->tmp_arena, top->sp);
            top = parent; // off to wherever we left off
        }
        cuikperf_region_end();
    }

    CUIK_TIMED_BLOCK("dom depths") {
        FOR_N(i, 0, cfg.block_count) {
            TB_BasicBlock* bb = &nl_map_get_checked(cfg.node_to_block, ws->items[i]);
            if (i == 0) {
                bb->dom_depth = 0;
            }
            bb->id = i;
        }
    }

    cuikperf_region_end();
    return cfg;
}

static int resolve_dom_depth(TB_CFG* cfg, TB_Node* bb) {
    if (dom_depth(cfg, bb) >= 0) {
        return dom_depth(cfg, bb);
    }

    int parent = resolve_dom_depth(cfg, idom(cfg, bb));

    // it's one more than it's parent
    nl_map_get_checked(cfg->node_to_block, bb).dom_depth = parent + 1;
    return parent + 1;
}

// Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm." (2006)
//   https://repository.rice.edu/items/99a574c3-90fe-4a00-adf9-ce73a21df2ed
void tb_compute_dominators(TB_Function* f, TB_Worklist* ws, TB_CFG cfg) {
    size_t block_count = aarray_length(cfg.blocks);
    TB_BasicBlock* blocks = &cfg.blocks[0];

    // entry block dominates itself
    blocks[0].dom = &blocks[0];

    bool changed = true;
    while (changed) {
        changed = false;

        // for all nodes, b, in reverse postorder (except entry block)
        FOR_N(i, 1, cfg.block_count) {
            TB_BasicBlock* bb = &blocks[cfg.rpo_walk[i]];
            TB_BasicBlock* new_idom = NULL;
            TB_Node* b = bb->start;

            // for all predecessors, p, of b
            FOR_N(j, 0, b->input_count) {
                TB_BasicBlock* p = cfg_get_pred_bb(&cfg, b, j);
                if (p == NULL) { continue; }

                // not computed, we'll wait for now
                TB_BasicBlock* idom_p = p->dom;
                if (idom_p == NULL) { continue; }

                if (new_idom == NULL) {
                    new_idom = idom_p;
                } else {
                    TB_ASSERT(p->input_count > 0);
                    int a = p->rpo_i;
                    if (a >= 0) {
                        int b = new_idom->rpo_i;
                        TB_ASSERT(b >= 0);

                        while (a != b) {
                            // while (finger1 < finger2)
                            //   finger1 = doms[finger1]
                            while (a > b) { a = blocks[cfg.rpo_walk[a]].dom->rpo_i; }

                            // while (finger2 < finger1)
                            //   finger2 = doms[finger2]
                            while (b > a) { a = blocks[cfg.rpo_walk[b]].dom->rpo_i; }
                        }

                        new_idom = &blocks[a];
                    }
                }
            }

            assert(new_idom != NULL);
            if (b->dom != new_idom) {
                b->dom  = new_idom;
                changed = true;
            }
        }
    }

    // generate depth values
    CUIK_TIMED_BLOCK("generate dom tree") {
        FOR_REV_N(i, 1, cfg.block_count) {
            resolve_dom_depth(&cfg, blocks[i]);
        }
    }
}

bool tb_is_dominated_by(TB_CFG cfg, TB_Node* expected_dom, TB_Node* n) {
    TB_BasicBlock* expected = nl_map_get_checked(cfg.node_to_block, expected_dom);
    TB_BasicBlock* bb = nl_map_get_checked(cfg.node_to_block, n);

    while (bb != expected) {
        if (bb->dom == bb) {
            return false;
        }
        bb = bb->dom;
    }

    return true;
}
