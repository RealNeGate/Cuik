
static void compute_dominators(TB_Function* f, TB_Worklist* ws, TB_CFG cfg);

typedef struct Block {
    struct Block* parent;
    TB_ArenaSavepoint sp;
    TB_Node* start;
    TB_Node* end;
    int succ_i;
    TB_Node* succ[];
} Block;

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

static Block* create_block(TB_Arena* arena, TB_Node* n) {
    TB_ArenaSavepoint sp = tb_arena_save(arena);
    TB_Node* end = end_of_bb(n);

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
        .sp     = sp,
        .start  = n,
        .end    = end,
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

TB_CFG tb_compute_cfg(TB_Function* f, TB_Worklist* ws, TB_Arena* arena, bool dominators) {
    cuikperf_region_start("CFG", NULL);
    assert(dyn_array_length(ws->items) == 0);

    TB_CFG cfg = { 0 };
    cfg.blocks = aarray_create(arena, TB_BasicBlock, 32);

    ////////////////////////////////
    // post-order DFS
    ////////////////////////////////
    // push initial block
    Block* top = create_block(&f->tmp_arena, f->params[0]);
    worklist_test_n_set(ws, f->params[0]);

    while (top != NULL) {
        cuikperf_region_start("rpo_iter", NULL);
        if (top->succ_i > 0) {
            // push next unvisited succ
            TB_Node* succ = top->succ[--top->succ_i];
            if (!worklist_test_n_set(ws, succ)) {
                Block* new_top = create_block(&f->tmp_arena, succ);
                new_top->parent = top;
                top = new_top;
            }
        } else {
            Block* parent = top->parent;

            TB_BasicBlock bb = { .start = top->start, .end = top->end, .dom_depth = -1, .freq = 1.0f };
            aarray_push(cfg.blocks, bb);

            tb_arena_restore(&f->tmp_arena, top->sp);
            top = parent; // off to wherever we left off
        }
        cuikperf_region_end();
    }
    worklist_clear_visited(ws);

    // just reverse the items here... im too lazy to flip all my uses
    cuikperf_region_start("reverse", NULL);
    size_t block_count = aarray_length(cfg.blocks);
    FOR_N(i, 0, block_count / 2) {
        SWAP(TB_BasicBlock, cfg.blocks[i], cfg.blocks[(block_count - 1) - i]);
    }
    cuikperf_region_end();

    // by this point, the cfg.blocks array is fixed in address (no more insertions thus no more resize)
    nl_map_create(cfg.node_to_block, block_count);
    aarray_for(i, cfg.blocks) {
        nl_map_put(cfg.node_to_block, cfg.blocks[i].start, &cfg.blocks[i]);
    }

    if (dominators) {
        cuikperf_region_start("doms", NULL);
        compute_dominators(f, ws, cfg);
        cuikperf_region_end();

        #if TB_OPTDEBUG_LOOP
        printf("\n%s: Doms:\n", f->super.name);
        FOR_N(i, 0, block_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            printf("  BB%zu(%%%u - %%%u, dom: BB%zu)\n", i, bb->start->gvn, bb->end->gvn, bb->dom - cfg.blocks);
        }
        #endif
    }

    cuikperf_region_end();
    return cfg;
}

void tb_free_cfg(TB_CFG* cfg) {
    nl_map_free(cfg->node_to_block);
}

////////////////////////////////
// Dominators
////////////////////////////////
// Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken. "A simple, fast dominance algorithm." (2006)
//   https://repository.rice.edu/items/99a574c3-90fe-4a00-adf9-ce73a21df2ed
static void compute_dominators(TB_Function* f, TB_Worklist* ws, TB_CFG cfg) {
    size_t block_count = aarray_length(cfg.blocks);
    TB_BasicBlock* blocks = &cfg.blocks[0];

    // entry block dominates itself
    blocks[0].dom_depth = 0;
    blocks[0].dom = &blocks[0];

    bool changed = true;
    while (changed) {
        changed = false;

        // for all nodes, b, in reverse postorder (except entry block)
        FOR_N(i, 1, block_count) {
            TB_BasicBlock* bb = &blocks[i];
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
                    new_idom = p;
                } else {
                    TB_ASSERT(p->start->input_count > 0);
                    int a = p - blocks;
                    if (a >= 0) {
                        int b = new_idom - blocks;
                        TB_ASSERT(b >= 0);

                        while (a != b) {
                            // while (finger1 < finger2)
                            //   finger1 = doms[finger1]
                            while (a > b) { a = blocks[a].dom - blocks; }

                            // while (finger2 < finger1)
                            //   finger2 = doms[finger2]
                            while (b > a) { b = blocks[b].dom - blocks; }
                        }

                        new_idom = &blocks[a];
                    }
                }
            }

            assert(new_idom != NULL);
            if (bb->dom != new_idom) {
                bb->dom = new_idom;
                changed = true;
            }
        }
    }

    // generate depth values
    CUIK_TIMED_BLOCK("generate dom tree") {
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            TB_BasicBlock* curr = bb;
            int depth = 0;
            while (curr->dom_depth < 0) {
                curr = curr->dom, depth++;
            }
            bb->dom_depth = depth + curr->dom_depth;
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

////////////////////////////////
// SoN dominance
////////////////////////////////
// we access and operate on dominators in the SoN form because it's nicer
// when modifying the IR (since we don't maintain a CFG).
//
//    | |  |
//    Region     [Region] = dominating terminator node
//      |
//     ...       [...]    = NULL
//      |
//    Branch     [Branch] = Region
//    |    |
//
static void tb_set_idom(TB_Function* f, TB_Node* n, TB_Node* new_dom) {
    TB_ASSERT_MSG(n != new_dom, "probably a mistake (the entry dom was defined without this function)");
    if (n->gvn >= f->doms_n) {
        size_t new_cap = tb_next_pow2(n->gvn + 16);

        f->doms = tb_arena_realloc(&f->arena, f->doms, f->doms_n * sizeof(TB_Node*), new_cap * sizeof(TB_Node*));
        FOR_N(i, f->doms_n, new_cap) { f->doms[i] = NULL; }

        f->doms_n = new_cap;
    }
    f->doms[n->gvn] = new_dom;
}

static TB_Node* tb_get_idom(TB_Function* f, TB_Node* n) {
    // if dom[n] is NULL we just follow the inputs[0]
    while (f->doms[n->gvn] == NULL) {
        TB_ASSERT(!cfg_is_region(n));
        n = n->inputs[0];
    }
    return f->doms[n->gvn];
}

// walks up until the node has a dom entry (it's a start or end node)
static TB_Node* tb_walk_to_bb_bounds(TB_Function* f, TB_Node* n) {
    // if dom[n] is NULL we just follow the inputs[0]
    while (f->doms[n->gvn] == NULL) {
        TB_ASSERT(!cfg_is_region(n));
        n = n->inputs[0];
    }
    return n;
}

static bool tb_is_dom_of(TB_Function* f, TB_Node* expected_dom, TB_Node* n) {
    n = tb_walk_to_bb_bounds(f, n);
    while (n != expected_dom && n != f->doms[n->gvn]) {
        n = f->doms[n->gvn];
    }
    return n == expected_dom;
}
