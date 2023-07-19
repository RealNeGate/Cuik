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
    FOREACH_REVERSE_N(i, 0, r->succ_count) {
        postorder(f, ctx, r->succ[i]);
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

static TB_Node* find_region(TB_Node* n) {
    while (n->type != TB_REGION && n->type != TB_START) n = n->inputs[0];
    return n;
}

TB_API TB_DominanceFrontiers tb_get_dominance_frontiers(TB_Function* f, TB_Dominators doms, const TB_PostorderWalk* order) {
    TB_DominanceFrontiers df = NULL;

    FOREACH_REVERSE_N(i, 0, order->count) {
        TB_Node* bb = order->traversal[i];

        if (bb->input_count >= 2) {
            FOREACH_N(k, 0, bb->input_count) {
                TB_Node* runner = find_region(bb->inputs[k]);

                while (runner->input_count > 0 && runner != nl_map_get_checked(doms, bb)) {
                    // add to frontier set
                    ptrdiff_t search = nl_map_get(df, runner);
                    TB_FrontierSet* set = NULL;
                    if (search < 0) {
                        nl_map_puti(df, runner, search);
                        df[search].v = nl_hashset_alloc(2); // these won't be big
                    }

                    nl_hashset_put(&df[search].v, bb);
                    runner = nl_map_get_checked(doms, runner);
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
TB_API TB_Dominators tb_get_dominators(TB_Function* f, TB_PostorderWalk order) {
    // entrypoint dominates itself
    DomContext ctx = { .f = f, .order = order };

    TB_Dominators doms = NULL;
    nl_map_create(doms, f->control_node_count);
    nl_map_put(doms, f->start_node, f->start_node);

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
                ptrdiff_t search_p = nl_map_get(doms, p);
                if (search_p < 0 && p->input_count > 0) {
                    int a = find_traversal_index(&ctx, p);
                    int b = find_traversal_index(&ctx, new_idom);

                    while (a != b) {
                        // while (finger1 < finger2)
                        //   finger1 = doms[finger1]
                        while (a < b) {
                            ptrdiff_t search = nl_map_get(doms, ctx.order.traversal[a]);
                            a = search >= 0 ? find_traversal_index(&ctx, doms[search].v) : entry_dom;
                        }

                        // while (finger2 < finger1)
                        //   finger2 = doms[finger2]
                        while (b < a) {
                            ptrdiff_t search = nl_map_get(doms, ctx.order.traversal[b]);
                            b = search >= 0 ? find_traversal_index(&ctx, doms[search].v) : entry_dom;
                        }
                    }

                    new_idom = ctx.order.traversal[a];
                }
            }

            ptrdiff_t search_b = nl_map_get(doms, b);
            if (search_b < 0) {
                nl_map_put(doms, b, new_idom);
                changed = true;
            } else if (doms[search_b].v != new_idom) {
                doms[search_b].v = new_idom;
                changed = true;
            }
        }
    }

    return doms;
}

TB_API TB_Node* tb_get_parent_region(TB_Node* n) {
    if (!tb_has_effects(n)) {
        return NULL;
    }

    while (n->type != TB_REGION && n->type != TB_START) {
        assert(tb_has_effects(n));
        n = n->inputs[0];
    }

    return n;
}

TB_API bool tb_is_dominated_by(TB_Dominators doms, TB_Node* expected_dom, TB_Node* bb) {
    while (expected_dom != bb) {
        ptrdiff_t search = nl_map_get(doms, bb);
        assert(search >= 0);

        if (bb == doms[search].v) {
            return false;
        }
        bb = doms[search].v;
    }

    return true;
}
