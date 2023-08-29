// This file contains generic analysis functions for operating on the TBIR
#include "tb_internal.h"

typedef struct {
    TB_Function* f;
    TB_PostorderWalk order;
} DomContext;

// we'll be walking backwards from the end node
static void postorder(TB_Function* f, TB_PostorderWalk* ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->visited, n);
    if (search >= 0) {
        return;
    }

    nl_map_put(ctx->visited, n, 0);

    // walk control edges (aka predecessors)
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
    if (r->end->type == TB_BRANCH) {
        TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
        FOREACH_REVERSE_N(i, 0, br->succ_count) {
            postorder(f, ctx, br->succ[i]);
        }
    }

    assert(ctx->count < f->control_node_count);
    ctx->traversal[ctx->count++] = n;
}

TB_API TB_PostorderWalk tb_function_get_postorder(TB_Function* f) {
    TB_PostorderWalk walk = {
        .traversal = tb_platform_heap_alloc(f->control_node_count * sizeof(TB_Node*))
    };

    nl_map_create(walk.visited, f->control_node_count);
    postorder(f, &walk, f->start_node);
    nl_map_free(walk.visited);
    return walk;
}

TB_API void tb_function_free_postorder(TB_PostorderWalk* walk) {
    tb_platform_heap_free(walk->traversal);
}

static int find_traversal_index(DomContext* ctx, TB_Node* bb) {
    FOREACH_N(i, 0, ctx->order.count) {
        if (ctx->order.traversal[i] == bb) return i;
    }

    tb_todo();
}

static int try_find_traversal_index(DomContext* ctx, TB_Node* bb) {
    FOREACH_N(i, 0, ctx->order.count) {
        if (ctx->order.traversal[i] == bb) return i;
    }

    return -1;
}

static TB_Node* find_region(TB_Node* n) {
    while (n->type != TB_REGION && n->type != TB_START) n = n->inputs[0];
    return n;
}

static int resolve_dom_depth(TB_Node* bb) {
    if (dom_depth(bb) >= 0) {
        return dom_depth(bb);
    }

    int parent = resolve_dom_depth(idom(bb));

    // it's one more than it's parent
    TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->dom_depth = parent + 1;
    return parent + 1;
}

TB_API TB_DominanceFrontiers tb_get_dominance_frontiers(TB_Function* f, const TB_PostorderWalk* order) {
    TB_DominanceFrontiers df = NULL;

    FOREACH_REVERSE_N(i, 0, order->count) {
        TB_Node* bb = order->traversal[i];

        if (bb->input_count >= 2) {
            FOREACH_N(k, 0, bb->input_count) {
                TB_Node* runner = find_region(bb->inputs[k]);

                while (runner->input_count > 0 && runner != idom(bb)) {
                    // add to frontier set
                    ptrdiff_t search = nl_map_get(df, runner);
                    TB_FrontierSet* set = NULL;
                    if (search < 0) {
                        nl_map_puti(df, runner, search);
                        df[search].v = nl_hashset_alloc(2); // these won't be big
                    }

                    nl_hashset_put(&df[search].v, bb);
                    runner = idom(runner);
                }
            }
        }
    }

    return df;
}

TB_API void tb_free_dominance_frontiers(TB_Function* f, TB_DominanceFrontiers frontiers, const TB_PostorderWalk* order) {
    nl_map_for(i, frontiers) {
        nl_hashset_free(frontiers[i].v);
    }
    nl_map_free(frontiers);
}

// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
void tb_compute_dominators(TB_Function* f, TB_PostorderWalk order) {
    // entrypoint dominates itself
    DomContext ctx = { .f = f, .order = order };

    // identify post order traversal order
    int entry_dom = ctx.order.count - 1;

    bool changed = true;
    while (changed) {
        changed = false;

        // for all nodes, b, in reverse postorder (except start node)
        FOREACH_REVERSE_N(i, 0, ctx.order.count - 1) {
            TB_Node* b = ctx.order.traversal[i];
            TB_Node* new_idom = find_region(b->inputs[0]);

            // for all other predecessors, p, of b
            FOREACH_N(j, 1, b->input_count) {
                TB_Node* p = find_region(b->inputs[j]);

                // if doms[p] already calculated
                TB_Node* idom_p = TB_NODE_GET_EXTRA_T(p, TB_NodeRegion)->dom;
                if (idom_p == NULL && p->input_count > 0) {
                    int a = try_find_traversal_index(&ctx, p);
                    if (a >= 0) {
                        int b = find_traversal_index(&ctx, new_idom);
                        while (a != b) {
                            // while (finger1 < finger2)
                            //   finger1 = doms[finger1]
                            while (a < b) {
                                TB_Node* d = idom(ctx.order.traversal[a]);
                                a = d ? find_traversal_index(&ctx, d) : entry_dom;
                            }

                            // while (finger2 < finger1)
                            //   finger2 = doms[finger2]
                            while (b < a) {
                                TB_Node* d = idom(ctx.order.traversal[b]);
                                b = d ? find_traversal_index(&ctx, d) : entry_dom;
                            }
                        }
                    }

                    new_idom = ctx.order.traversal[a];
                }
            }

            assert(new_idom != NULL);
            TB_NodeRegion* region_b = TB_NODE_GET_EXTRA_T(b, TB_NodeRegion);
            if (region_b->dom != new_idom) {
                region_b->dom = new_idom;
                changed = true;
            }
        }
    }

    // generate depth values
    CUIK_TIMED_BLOCK("generate dom tree") {
        FOREACH_N(i, 0, ctx.order.count - 1) {
            resolve_dom_depth(ctx.order.traversal[i]);
        }
    }
}

TB_API TB_Node* tb_get_parent_region(TB_Node* n) {
    while (n->type != TB_REGION && n->type != TB_START) {
        tb_assert(n->inputs[0], "node has no have a control edge");
        n = n->inputs[0];
    }

    return n;
}

TB_API bool tb_is_dominated_by(TB_Node* expected_dom, TB_Node* bb) {
    while (expected_dom != bb) {
        TB_Node* new_bb = idom(bb);
        if (bb == new_bb) {
            return false;
        }

        bb = new_bb;
    }

    return true;
}
