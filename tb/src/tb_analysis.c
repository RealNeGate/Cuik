// This file contains generic analysis functions for operating on the TBIR
#include "tb_internal.h"

typedef struct {
    TB_Function* f;
    int* doms;
    TB_PostorderWalk order;
} DomContext;

// ignores the start node when doing the traversal
static void postorder(TB_Function* f, TB_PostorderWalk* ctx, TB_Label bb) {
    if (ctx->visited[bb]) {
        return;
    }

    ctx->visited[bb] = true;

    TB_Node* end = &f->nodes[f->bbs[bb].end];
    if (end->type == TB_NULL || end->type == TB_RET || end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
        /* RET can't do shit in this context */
    } else if (end->type == TB_GOTO) {
        postorder(f, ctx, end->goto_.label);
    } else if (end->type == TB_IF) {
        postorder(f, ctx, end->if_.if_false);
        postorder(f, ctx, end->if_.if_true);
    } else if (end->type == TB_SWITCH) {
        // each entry takes up two slots in the VLA storage (i just put random crap in there like arguments for function calls)
        size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
        TB_SwitchEntry* entries = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start];

        postorder(f, ctx, end->switch_.default_label);

        FOREACH_REVERSE_N(i, 0, entry_count) {
            postorder(f, ctx, entries[i].value);
        }
    } else {
        tb_function_print(f, tb_default_print_callback, stdout, true);
        tb_panic("Invalid IR :v(\n");
    }

    ctx->traversal[ctx->count++] = bb;
}

TB_API TB_PostorderWalk tb_function_get_postorder(TB_Function* f) {
    TB_PostorderWalk walk = {
        .traversal = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Reg)),
        .visited = tb_platform_heap_alloc(f->bb_count * sizeof(bool))
    };

    tb_function_get_postorder_explicit(f, &walk);
    return walk;
}

TB_API void tb_function_free_postorder(TB_PostorderWalk* walk) {
    tb_platform_heap_free(walk->visited);
    tb_platform_heap_free(walk->traversal);
}

TB_API void tb_function_get_postorder_explicit(TB_Function* f, TB_PostorderWalk* walk) {
    memset(walk->visited, 0, f->bb_count * sizeof(bool));
    postorder(f, walk, 0);
}

static int find_traversal_index(DomContext* ctx, TB_Label l) {
    FOREACH_N(i, 0, ctx->order.count) {
        if (ctx->order.traversal[i] == l) return i;
    }

    tb_todo();
}

static int get_idom_in_postorder_index(DomContext* ctx, int x) {
    return find_traversal_index(ctx, ctx->doms[ctx->order.traversal[x]]);
}

// this takes in postorder indices
static TB_Label intersect(DomContext* ctx, int a, int b) {
    while (a != b) {
        // while (finger1 < finger2)
        //   finger1 = doms[finger1]
        while (a < b) {
            a = get_idom_in_postorder_index(ctx, a);
        }

        // while (finger2 < finger1)
        //   finger2 = doms[finger2]
        while (b < a) {
            b = get_idom_in_postorder_index(ctx, b);
        }
    }

    return a;
}

TB_Predeccesors tb_get_temp_predeccesors(TB_Function* f, TB_TemporaryStorage* tls) {
    TB_Predeccesors p = { 0 };
    p.count = tb_tls_push(tls, f->bb_count * sizeof(int));
    p.preds = tb_tls_push(tls, f->bb_count * sizeof(TB_Label*));

    // entry label has no predecessors
    p.count[0] = 0;
    p.preds[0] = NULL;

    FOREACH_N(j, 1, f->bb_count) {
        p.preds[j] = tb_tls_push(tls, 0);
        tb_calculate_immediate_predeccessors(f, tls, j, &p.count[j]);
    }

    return p;
}

void tb_free_temp_predeccesors(TB_TemporaryStorage* tls, TB_Predeccesors preds) {
    tb_tls_restore(tls, preds.count);
}

TB_API TB_Predeccesors tb_get_predeccesors(TB_Function* f) {
    TB_Predeccesors p = { 0 };
    p.count = tb_platform_heap_alloc(f->bb_count * sizeof(int));
    p.preds = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Label*));

    // entry label has no predecessors
    p.count[0] = 0;
    p.preds[0] = NULL;

    FOREACH_N(j, 1, f->bb_count) {
        p.preds[j] = tb_calculate_immediate_predeccessors(f, NULL, j, &p.count[j]);
    }

    return p;
}

TB_API TB_DominanceFrontiers tb_get_dominance_frontiers(TB_Function* f, TB_Predeccesors p, const TB_Label* doms) {
    TB_DominanceFrontiers df = { 0 };
    df.count = tb_platform_heap_alloc(f->bb_count * sizeof(int));
    df._ = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Label*));

    memset(df.count, 0, f->bb_count * sizeof(int));
    memset(df._, 0, f->bb_count * sizeof(TB_Label*));

    FOREACH_N(bb, 0, f->bb_count) {
        if (p.count[bb] >= 2) {
            FOREACH_N(k, 0, p.count[bb]) {
                TB_Label runner = p.preds[bb][k];
                while (runner != doms[bb]) {
                    // add to frontier
                    size_t i = df.count[runner]++;
                    df._[runner] = tb_platform_heap_realloc(df._[runner], (i + 1) * sizeof(TB_Label));
                    df._[runner][i] = bb;

                    runner = doms[runner];
                }
            }
        }
    }

    #if 0
    // GraphViz output
    printf("digraph %s {\n", f->super.name);
    printf("  subgraph CFG {\n");
    FOREACH_N(bb, 0, f->bb_count) {
        FOREACH_N(j, 0, p.count[bb]) {
            printf("    L%d -> L%td;\n", p.preds[bb][j], bb);
        }
        printf("\n");
    }
    printf("  }\n\n");
    printf("  subgraph Doms {\n");
    FOREACH_N(bb, 0, f->bb_count) {
        printf("    D%d -> D%td;\n", doms[bb], bb);
    }
    printf("  }\n\n");
    printf("  subgraph DomFrontier {\n");
    FOREACH_N(bb, 0, f->bb_count) {
        FOREACH_N(j, 0, df.count[bb]) {
            printf("    F%d -> F%td;\n", df._[bb][j], bb);
        }
        printf("\n");
    }
    printf("  }\n\n");
    printf("}\n");
    #endif

    return df;
}

TB_API void tb_free_dominance_frontiers(TB_Function* f, TB_DominanceFrontiers* frontiers) {
    FOREACH_N(bb, 0, f->bb_count) {
        tb_platform_heap_free(frontiers->_[bb]);
    }

    tb_platform_heap_free(frontiers->_);
    tb_platform_heap_free(frontiers->count);
}

// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
TB_API size_t tb_get_dominators(TB_Function* f, TB_Predeccesors preds, TB_Label* doms) {
    if (doms == NULL) {
        return f->bb_count;
    }

    // entrypoint dominates itself
    DomContext ctx = { .f = f, .doms = doms };
    doms[0] = 0;

    // undef all dominator entries
    FOREACH_N(i, 1, f->bb_count) doms[i] = -1;

    // identify post order traversal order
    {
        TB_TemporaryStorage* tls = tb_tls_steal();

        ctx.order.count = 0;
        ctx.order.traversal = tb_tls_push(tls, f->bb_count * sizeof(TB_Reg));

        // we only need the visited array for this scope, it's just to avoid
        // recursing forever on post order traversal stuff
        ctx.order.visited = tb_tls_push(tls, f->bb_count * sizeof(bool));
        memset(ctx.order.visited, 0, f->bb_count * sizeof(bool));

        tb_function_get_postorder_explicit(f, &ctx.order);

        // NOTE: free ctx.order.visited but keep ctx.traversal
        tb_tls_restore(tls, ctx.order.visited);
        ctx.order.visited = NULL;
    }

    bool changed = true;
    while (changed) {
        changed = false;

        // for all nodes, b, in reverse postorder (except start node)
        FOREACH_REVERSE_N(i, 0, ctx.order.count - 1) {
            TB_Label b = ctx.order.traversal[i];
            TB_Label new_idom = preds.preds[b][0];

            // for all other predecessors, p, of b
            FOREACH_N(j, 1, preds.count[b]) {
                TB_Label p = preds.preds[b][j];

                // i.e., if doms[p] already calculated
                if (doms[p] != -1) {
                    new_idom = ctx.order.traversal[intersect(
                            &ctx,
                            find_traversal_index(&ctx, p),
                            find_traversal_index(&ctx, new_idom)
                        )];
                }
            }

            if (doms[b] != new_idom) {
                doms[b] = new_idom;
                changed = true;
            }
        }
    }

    // if it's still undefined it's unreachable but for now we'll make
    // it map to the entrypoint to avoid array bounds issues and such
    FOREACH_N(i, 1, f->bb_count){
        if (doms[i] == -1) doms[i] = 0;
    }

    return f->bb_count;
}

TB_API bool tb_is_dominated_by(TB_Label* doms, TB_Label expected_dom, TB_Label bb) {
    while (bb != 0 && expected_dom != bb) {
        bb = doms[bb];
    }

    return (expected_dom == bb);
}

TB_API TB_LoopInfo tb_get_loop_info(TB_Function* f, TB_Predeccesors preds, TB_Label* doms) {
    // Find loops
    DynArray(TB_Loop) loops = dyn_array_create(TB_Loop, 64);
    FOREACH_N(bb, 0, f->bb_count) {
        TB_Label backedge = 0;
        FOREACH_N(j, 0, preds.count[bb]) {
            if (tb_is_dominated_by(doms, bb, preds.preds[bb][j])) {
                backedge = preds.preds[bb][j];
                break;
            }
        }

        if (backedge) {
            printf("L%zu is a loop (backedge: L%d)\n    ", bb, backedge);

            TB_Loop l = { .parent_loop = -1, .header = bb, .backedge = backedge };
            l.body = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Label));

            FOREACH_N(j, 0, f->bb_count) {
                if (tb_is_dominated_by(doms, bb, j)) {
                    printf("L%zu ", j);

                    l.body[l.body_count++] = j;
                }
            }

            l.body = tb_platform_heap_realloc(l.body, l.body_count * sizeof(TB_Label));

            // check if we have a parent...
            FOREACH_REVERSE_N(o, 0, dyn_array_length(loops)) {
                FOREACH_N(j, 0, loops[o].body_count) {
                    if (bb == loops[o].body[j]) {
                        l.parent_loop = o;
                        goto fatherfull_behavior;
                    }
                }
            }

            fatherfull_behavior:
            dyn_array_put(loops, l);

            printf("\n\n");
        }
    }

    return (TB_LoopInfo){ .count = dyn_array_length(loops), .loops = &loops[0] };
}

TB_API void tb_free_loop_info(TB_LoopInfo l) {
    dyn_array_destroy(l.loops);
}
