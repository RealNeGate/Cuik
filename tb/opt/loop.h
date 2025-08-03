
typedef struct {
    TB_Function* f;
    TB_CFG cfg;

    size_t ctrl_n;
    TB_Node** ctrl;

    NL_Table loop_map;
} LoopOpt;

static TB_Node* branch_cproj(TB_Function* f, TB_Node* n, uint64_t taken, int64_t key, int index) {
    TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
    set_input(f, cproj, n, 0);
    TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = index, .taken = taken, .key = key);
    return cproj;
}

////////////////////////////////
// Loop finding
////////////////////////////////
enum { NOT_ON_STK, ON_STK, IN_SCC };
typedef struct {
    int on_stack;
    int index, low_link;
} LoopSCCNode;

typedef struct {
    TB_Function* f;
    NL_Table* loop_map;
    TB_Arena* arena;

    TB_LoopTree* outer_loop;

    LoopSCCNode* nodes;
    int index;

    int stk_cnt;
    int* stk;
} LoopSCC;

bool slp_transform(TB_Function* f, LoopOpt* ctx, TB_Worklist* ws, TB_LoopTree* loop);

static void loop_add_kid(TB_LoopTree* kid, TB_LoopTree* mom) {
    kid->parent = mom;
    kid->next   = mom->kid;
    mom->kid    = kid;
}

static void loop_compute_depth(TB_LoopTree* loop, int d) {
    while (loop) {
        #if TB_OPTDEBUG_LOOP
        FOR_N(i, 0, d) { printf("  "); }
        printf("  Loop%d(depth: %d", loop->id, d);
        if (loop->header) {
            printf(", header: %%%u", loop->header->gvn);
        }
        printf(")\n");
        #endif

        loop->depth = d;
        if (loop->kid) {
            loop_compute_depth(loop->kid, d + 1);
        }
        loop = loop->next;
    }
}

static TB_LoopTree* loop_add_backedge(LoopSCC* scc, TB_CFG* cfg, TB_BasicBlock* header, TB_BasicBlock* backedge) {
    bool is_natural = slow_dommy2(header, backedge);
    TB_OPTDEBUG(LOOP)(printf("  * found backedge BB%zu -> BB%zu (%s)\n", header - cfg->blocks, backedge - cfg->blocks, is_natural ? "reducible" : "irreducible"));

    TB_LoopTree* loop = nl_table_get(scc->loop_map, header->start);
    if (loop == NULL) {
        loop = tb_arena_alloc(scc->arena, sizeof(TB_LoopTree));
        *loop = (TB_LoopTree){
            .parent      = NULL,
            .header      = header->start,
            .backedge    = backedge->start,
            .is_natural  = is_natural,
            .id          = 1 + aarray_length(cfg->loops),
        };

        nl_table_put(scc->loop_map, header->start, loop);
        aarray_push(cfg->loops, loop);
    }

    nl_table_put(scc->loop_map, backedge->start, loop);
    return loop;
}

// strongly connected components used to find loops & their bodies, if the backedge is dominated by the
// header we also mark it as natural.
static void loop_scc_walk(LoopSCC* restrict scc, TB_CFG* restrict cfg, TB_BasicBlock* bb) {
    LoopSCCNode* n = &scc->nodes[bb - cfg->blocks];
    n->index = n->low_link = scc->index++;

    TB_ASSERT(scc->stk_cnt < aarray_length(cfg->blocks));
    scc->stk[scc->stk_cnt++] = bb - cfg->blocks;
    n->on_stack = ON_STK;

    // walk successors
    // TODO(NeGate): in order to avoid weird nondeterminism
    FOR_SUCC(it, bb->end) {
        TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);
        LoopSCCNode* succ_n    = &scc->nodes[succ_bb - cfg->blocks];

        // found a backedge, this will also place a loop info entry
        // whose body will be filled once the SCC for the header is built.
        if (succ_bb <= bb) {
            TB_LoopTree* new_loop = loop_add_backedge(scc, cfg, succ_bb, bb);

            if (cfg->root_loop == NULL) {
                cfg->root_loop = new_loop;
            } else if (new_loop != cfg->root_loop) {
                // if we're not dominated by the loop header, it means we should bump out of this loop nest
                TB_LoopTree* last = NULL;
                while (cfg->root_loop && !slow_dommy(cfg, cfg->root_loop->header, succ_bb->start)) {
                    last = cfg->root_loop;
                    cfg->root_loop = cfg->root_loop->parent;
                }

                if (cfg->root_loop == NULL) {
                    TB_OPTDEBUG(LOOP)(printf("    * Loop%d is next to Loop%d\n", new_loop->id, last->id));
                    new_loop->next = last;
                } else {
                    TB_OPTDEBUG(LOOP)(printf("    * Loop%d is inside Loop%d\n", new_loop->id, cfg->root_loop->id));
                    loop_add_kid(new_loop, cfg->root_loop);
                }
                cfg->root_loop = new_loop;
            }
        }

        if (succ_n->index < 0) {
            loop_scc_walk(scc, cfg, succ_bb);
            n->low_link = TB_MIN(n->low_link, succ_n->low_link);
        } else if (succ_n->index < n->index && succ_n->on_stack == ON_STK) {
            n->low_link = TB_MIN(n->low_link, succ_n->index);
        }
    }

    // we're the root, construct an SCC. note that an SCC isn't a loop, it might encapsulate
    // multiple loops so we use the existing notation of headers to associate each BB in the
    // SCC into the nearest (tightest) loops.
    if (n->low_link == n->index) {
        int scc_top = scc->stk_cnt;
        int root_bb_id = bb - cfg->blocks;

        do {
            scc->nodes[scc->stk[scc->stk_cnt - 1]].on_stack = IN_SCC;
            scc->stk_cnt -= 1;
        } while (root_bb_id != scc->stk[scc->stk_cnt]);

        TB_ASSERT(scc->stk_cnt >= 0);
        TB_OPTDEBUG(LOOP)(printf("  SCC(header: %%%u)\n", bb->start->gvn));

        // this is also the highest loop in the SCC
        TB_LoopTree* top_loop = nl_table_get(scc->loop_map, bb->start);
        if (top_loop == NULL) {
            // if the SCC doesn't have a loop header it HAS to be
            // a single BB that's not a cycle
            TB_ASSERT(scc_top == scc->stk_cnt+1);
            TB_OPTDEBUG(LOOP)(printf("  * BB%zu\n", bb - cfg->blocks));
        } else {
            FOR_N(i, scc->stk_cnt, scc_top) {
                int bb_rpo = scc->stk[i];
                TB_BasicBlock* kid_bb = &cfg->blocks[bb_rpo];

                // walk dom to nearest loop
                TB_BasicBlock* curr = kid_bb;
                TB_LoopTree* loop = nl_table_get(scc->loop_map, curr->start);
                while (loop == NULL) {
                    curr = curr->dom;
                    loop = nl_table_get(scc->loop_map, curr->start);
                }

                if (loop != top_loop) {
                    if (loop->is_natural) {
                        // we can only be in this natural loop if we're between the RPO indices
                        int head_rpo = nl_map_get_checked(cfg->node_to_block, loop->header) - cfg->blocks;
                        int tail_rpo = nl_map_get_checked(cfg->node_to_block, loop->backedge) - cfg->blocks;
                        if (bb_rpo < head_rpo || bb_rpo > tail_rpo) {
                            loop = loop->parent;
                        }
                    } else {
                        loop = loop->parent;
                    }
                }

                nl_table_put(scc->loop_map, kid_bb->start, loop);
                TB_OPTDEBUG(LOOP)(printf("  * BB%zu (Loop%d)\n", kid_bb - cfg->blocks, loop->id));
            }

            // if there's no nodes in the SCC which leave it, we're left with an infinite loop.
            int exits = 0;
            FOR_N(i, scc->stk_cnt, scc_top) {
                TB_BasicBlock* kid_bb = &cfg->blocks[scc->stk[i]];
                FOR_SUCC(it, kid_bb->end) {
                    TB_BasicBlock* succ_bb = nl_map_get_checked(cfg->node_to_block, it.succ);
                    LoopSCCNode* succ_n    = &scc->nodes[succ_bb - cfg->blocks];
                    if (succ_n->on_stack != IN_SCC) {
                        TB_OPTDEBUG(LOOP)(printf("  * BB%zu exits via BB%zu\n", kid_bb - cfg->blocks, succ_bb - cfg->blocks));
                        exits += 1;
                    }
                }
            }

            if (exits == 0) {
                TB_Function* f = scc->f;
                mark_node_n_users(f, bb->start);

                // insert NEVER_BRANCH right after the header
                TB_Node* branch = tb_alloc_node(f, TB_NEVER_BRANCH, TB_TYPE_TUPLE, 1, 0);
                TB_Node* into_loop = make_proj_node(f, TB_TYPE_CONTROL, branch, 0);
                TB_Node* exit_loop = make_proj_node(f, TB_TYPE_CONTROL, branch, 1);

                subsume_node_without_phis(f, bb->start, into_loop);
                set_input(f, branch, bb->start, 0);

                // it doesn't matter what the memory input is
                TB_Node* unreach = tb_alloc_node(f, TB_UNREACHABLE, TB_TYPE_CONTROL, 2, 0);
                set_input(f, unreach, exit_loop, 0);

                // replace an existing DEAD node or add to potential exits
                bool replace = false;
                FOR_N(i, 1, f->root_node->input_count) {
                    if (f->root_node->inputs[i]->type == TB_DEAD) {
                        set_input(f, f->root_node, unreach, i);
                        replace = true;
                        break;
                    }
                }

                if (!replace) {
                    add_input_late(f, f->root_node, unreach);
                }

                mark_node(f, unreach);
                mark_node(f, f->root_node);
                mark_node_n_users(f, branch);
            }
        }

        FOR_N(i, scc->stk_cnt, scc_top) {
            scc->nodes[scc->stk[i]].on_stack = NOT_ON_STK;
        }

        TB_OPTDEBUG(LOOP)(printf("\n"));
    }
}

static void loop_find(TB_Function* f, TB_CFG* restrict cfg, LoopOpt* restrict ctx) {
    TB_OPTDEBUG(LOOP)(printf("\n%s: Loop Finding:\n", f->super.name));
    cfg->loops = aarray_create(&f->tmp_arena, TB_LoopTree*, 32);

    // SCC
    size_t block_count = aarray_length(cfg->blocks);
    {
        LoopSCC scc = { 0 };
        scc.f        = f;
        scc.arena    = &f->tmp_arena;
        scc.loop_map = &ctx->loop_map;
        scc.nodes    = tb_arena_alloc(&f->tmp_arena, block_count * sizeof(LoopSCCNode));
        scc.stk      = tb_arena_alloc(&f->tmp_arena, block_count * sizeof(int));

        FOR_N(i, 0, block_count) {
            scc.nodes[i] = (LoopSCCNode){ .index = -1, .low_link = -1 };
        }

        loop_scc_walk(&scc, cfg, &cfg->blocks[0]);

        if (cfg->root_loop) {
            while (cfg->root_loop->parent != NULL) {
                cfg->root_loop = cfg->root_loop->parent;
            }
        }

        TB_OPTDEBUG(LOOP)(printf("\n%s: Final analysis:\n", f->super.name));
        loop_compute_depth(cfg->root_loop, 1);
    }
}

////////////////////////////////
// Loop rotation
////////////////////////////////
static void loop_set_ctrl(LoopOpt* ctx, TB_Node* n, TB_Node* new_ctrl) {
    if (n->gvn >= ctx->ctrl_n) {
        ctx->ctrl = tb_arena_realloc(&ctx->f->tmp_arena, ctx->ctrl, ctx->ctrl_n * sizeof(TB_Node*), 2 * ctx->ctrl_n * sizeof(TB_Node*));
        FOR_N(i, ctx->ctrl_n, 2 * ctx->ctrl_n) {
            ctx->ctrl[i] = NULL;
        }
        ctx->ctrl_n *= 2;
    }

    TB_ASSERT(new_ctrl);
    ctx->ctrl[n->gvn] = new_ctrl;
}

// control dependence on the loop body can be moved to the ZTC, this allows for the hoisting of
// loads since there's now a safe location outside of the loop where they can land.
static void loop_hoist_ops(TB_Function* f, TB_Node* ctrl, TB_Node* earlier) {
    for (size_t i = 0; i < ctrl->user_count;) {
        TB_Node* un = USERN(&ctrl->users[i]);
        int ui      = USERI(&ctrl->users[i]);
        if (ui == 0 && !cfg_is_control(un) && un->type != TB_PHI) {
            #if TB_OPTDEBUG_LOOP
            printf("   HOIST(");
            tb_print_dumb_node(NULL, un);
            printf(") => %%%u\n", earlier->gvn);
            #endif

            set_input(f, un, earlier, 0);
        } else {
            i += 1;
        }
    }
}

static bool loop_inside(TB_LoopTree* a, TB_LoopTree* b) {
    while (b && a != b) {
        b = b->parent;
    }
    return a == b;
}

// returns the cloned loop header (with no preds)
static ArenaArray(TB_Node*) loop_clone_ztc(LoopOpt* ctx, TB_Worklist* ws, size_t cloned_n, TB_Node** cloned, TB_Node* header, TB_Node* latch) {
    TB_Function* f = ctx->f;
    ArenaArray(TB_Node*) cloned_list = aarray_create(&f->tmp_arena, TB_Node*, 32);

    worklist_clear(ws);
    worklist_push(ws, header);
    FOR_USERS(u, header) {
        if (USERN(u)->type == TB_PHI) {
            TB_ASSERT(USERI(u) == 0);
            worklist_push(ws, USERN(u));
        }
    }

    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];

        // loop variants are just those which are dominated by the loop header
        if (ctx->ctrl[n->gvn] == header) {
            if (n->input_count == 1 && n->inputs[0] == f->root_node) {
                cloned[n->gvn] = n;
            } else {
                // we generate the cloned node now, the edges get connected later on
                size_t extra = extra_bytes(n);
                TB_Node* k = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
                memcpy(k->extra, n->extra, extra);

                cloned[n->gvn] = k;
                aarray_push(cloned_list, n);

                // avoid forward progress problems by cloning the lattice position
                latuni_set(f, k, latuni_get(f, n));
                mark_node(f, n);

                FOR_N(j, 0, n->input_count) if (n->inputs[j]) { worklist_push(ws, n->inputs[j]); }
                FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
            }
        }
    }

    TB_Node* cloned_header = cloned[header->gvn];
    TB_ASSERT(cloned_list[0] == header);

    // walk all nodes to clone and redirect any cloned inputs
    FOR_N(i, 0, aarray_length(cloned_list)) {
        TB_Node* n = cloned_list[i];
        TB_Node* k = cloned[n->gvn];

        loop_set_ctrl(ctx, k, cloned_header);
        if (i == 0) {
            k->type = TB_REGION;
            k->input_count = 0;
        } else if (n != k) {
            FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
                TB_Node* new_in = n->inputs[j];
                if (cloned[new_in->gvn] != NULL) {
                    new_in = cloned[new_in->gvn];
                }
                set_input(f, k, new_in, j);
            }
        }
        mark_node(f, n);
        mark_node(f, k);

        #if TB_OPTDEBUG_LOOP
        printf("CLONE: ");
        tb_print_dumb_node(NULL, n);
        printf(" => ");
        tb_print_dumb_node(NULL, k);
        printf("\n");
        #endif
    }

    TB_LoopTree* loop = nl_table_get(&ctx->loop_map, header);

    // anyone in the body who used to refer to the header nodes should now refer directly
    // to a new rotated phi
    aarray_for(i, cloned_list) {
        TB_Node* n = cloned_list[i];
        if (n == header || (n->type == TB_PHI && n->inputs[0] == header)) {
            continue;
        }

        #if TB_OPTDEBUG_LOOP
        printf("   ROOF(");
        tb_print_dumb_node(NULL, n);
        printf(")\n");
        #endif

        TB_Node* p = NULL;
        for (size_t i = 0; i < n->user_count;) {
            TB_Node* un = USERN(&n->users[i]);
            int ui      = USERI(&n->users[i]);

            if (cloned[un->gvn] == NULL && !is_proj(un)) {
                TB_Node* bb = ctx->ctrl[un->gvn];
                TB_LoopTree* use_loop = nl_table_get(&ctx->loop_map, bb);
                if (loop_inside(loop, use_loop)) {
                    if (p == NULL) {
                        p = tb_alloc_node(f, TB_PHI, n->dt, 3, 0);
                        set_input(f, p, header, 0);
                        set_input(f, p, cloned[n->gvn], 1);
                        set_input(f, p, n, 2);

                        #if TB_OPTDEBUG_LOOP
                        printf("   PHI(");
                        tb_print_dumb_node(NULL, p);
                        printf(")\n");
                        #endif
                    }

                    #if TB_OPTDEBUG_LOOP
                    printf("   USSR(");
                    tb_print_dumb_node(NULL, un);
                    printf(", %d)\n", ui);
                    #endif

                    set_input(f, un, p, ui);
                    continue;
                }
            }
            i++;
        }
        TB_OPTDEBUG(LOOP)(printf("\n"));
    }

    // ZTC doesn't need its cloned phis, replace
    // them with a single value.
    FOR_USERS(u, header) {
        TB_Node* old_phi = USERN(u);
        if (old_phi->type == TB_PHI && old_phi->gvn < cloned_n) {
            TB_ASSERT(USERI(u) == 0);
            TB_Node* new_phi = cloned[old_phi->gvn];

            #if TB_OPTDEBUG_LOOP
            printf("INIT: ");
            tb_print_dumb_node(NULL, new_phi);
            printf(" => ");
            tb_print_dumb_node(NULL, old_phi->inputs[1]);
            printf("\n");
            #endif

            subsume_node(f, new_phi, old_phi->inputs[1]);
            cloned[old_phi->gvn] = old_phi->inputs[1];

            TB_Node* next_val = old_phi->inputs[2];
            for (size_t j = 0; j < old_phi->user_count;) {
                TB_Node* un = USERN(&old_phi->users[j]);

                if (cloned[un->gvn] && un != old_phi) {
                    #if TB_OPTDEBUG_LOOP
                    printf("   USER(");
                    tb_print_dumb_node(NULL, un);
                    printf(", %d, %%%u)\n", USERI(&old_phi->users[j]), next_val->gvn);
                    #endif

                    set_input(f, un, next_val, USERI(&old_phi->users[j]));
                } else {
                    j++;
                }
            }

            if (old_phi->user_count == 0) {
                #if TB_OPTDEBUG_LOOP
                printf("   KILL(");
                tb_print_dumb_node(NULL, old_phi);
                printf(")\n");
                #endif

                tb_kill_node(f, old_phi);
            }
        }
    }

    // if the "pre_latch_ctrl" is not cloned that means there's at least
    // one control node in the body of the loop and we're now hooking the
    // rotated control nodes to it.
    TB_Node* pre_latch_ctrl = latch->inputs[1];
    if (!cloned[pre_latch_ctrl->gvn]) {
        for (size_t j = 0; j < header->user_count;) {
            TB_Node* un = USERN(&header->users[j]);

            if (cloned[un->gvn] && un->type != TB_PHI) {
                #if TB_OPTDEBUG_LOOP
                printf("   USER(");
                tb_print_dumb_node(NULL, un);
                printf(", %d, %%%u)\n", USERI(&header->users[j]), pre_latch_ctrl->gvn);
                #endif

                set_input(f, un, pre_latch_ctrl, USERI(&header->users[j]));
            } else {
                j++;
            }
        }
    }

    return cloned_list;
}

// Affine loop accessors
static TB_Node* affine_loop_latch(TB_Node* header) {
    if (header->type == TB_AFFINE_LOOP &&
        header->inputs[1]->type == TB_BRANCH_PROJ &&
        header->inputs[1]->inputs[0]->type == TB_AFFINE_LATCH) {
        return header->inputs[1]->inputs[0];
    }

    return NULL;
}

static uint64_t* iconst(TB_Node* n) {
    return n->type == TB_ICONST ? &TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value : NULL;
}

static void swap_nodes(TB_Function* f, TB_Node* n, int i, int j) {
    TB_Node* old = n->inputs[i];
    set_input(f, n, n->inputs[j], i);
    set_input(f, n, old, j);
}

static TB_Node* get_simple_loop_exit(LoopOpt* opt, TB_LoopTree* loop, TB_Node* header, TB_Node* latch) {
    TB_ASSERT(nl_table_get(&opt->loop_map, header) == loop);
    if ((latch->type != TB_BRANCH && latch->type != TB_AFFINE_LATCH) || TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
        return NULL;
    }

    // for this latch to count it needs to exit
    TB_Node* exit = NULL;
    TB_Node* backedge_bb = header->inputs[1];
    FOR_USERS(u, latch) {
        if (USERN(u)->type != TB_BRANCH_PROJ) { continue; }
        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
        assert(index < 2);

        TB_Node* succ = USERN(u);
        TB_LoopTree* succ_loop = nl_table_get(&opt->loop_map, succ);
        if (succ_loop == loop && succ->user_count == 1) {
            // we didn't travel far enough
            TB_Node* next_succ = USERN(&succ->users[0]);
            TB_LoopTree* next_loop = nl_table_get(&opt->loop_map, succ);
            if (next_loop) {
                succ = next_succ;
                succ_loop = next_loop;
            }
        }

        if (!loop_inside(loop, succ_loop)) {
            // successor leaves the loop, we want only one
            if (exit) { return NULL; }
            else { exit = USERN(u);  }
        }
    }

    return exit;
}

static const char* ind_pred_names[] = { "ne", "slt", "sle", "ult", "ule" };

// since we're looking at the rotated form:
//
//   i = phi(init, i2)
//   n = i + step where step is constant
static TB_Node* affine_indvar(TB_Node* n, TB_Node* header) {
    if (n->type == TB_PHI) {
        n = n->inputs[2];
    }

    return (n->type == TB_ADD || n->type == TB_PTR_OFFSET)
        && TB_IS_INT_OR_PTR(n->dt)
        && n->inputs[1]->type == TB_PHI
        && n->inputs[1]->inputs[0] == header
        && n->inputs[1]->inputs[2] == n
        && n->inputs[2]->type == TB_ICONST ? n->inputs[1] : NULL;
}

static uint64_t* find_affine_indvar(TB_Node* n, TB_Node* header) {
    if (n->type == TB_PHI &&
        n->inputs[0] == header &&
        n->inputs[2]->type == TB_ADD &&
        n->inputs[2]->inputs[1] == n &&
        n->inputs[2]->inputs[2]->type == TB_ICONST) {
        return &TB_NODE_GET_EXTRA_T(n->inputs[2]->inputs[2], TB_NodeInt)->value;
    }

    return NULL;
}

static bool find_latch_indvar(TB_Node* header, TB_Node* latch, TB_InductionVar* restrict var) {
    bool exit_when_key = !TB_NODE_GET_EXTRA_T(header->inputs[1], TB_NodeProj)->index;
    TB_NodeBranchProj* if_br = cfg_if_branch(latch);
    assert(if_br);

    TB_Node* cond = latch->inputs[1];
    if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_SLE) {
        // canonicalize compare, this way it's always shaped such that "true" means continue
        TB_NodeTypeEnum type = cond->type;
        TB_Node* a = cond->inputs[1];
        TB_Node* b = cond->inputs[2];

        // flip condition
        if ((!exit_when_key && if_br->key == 0) ||
            (exit_when_key && if_br->key == 1)) {
            if (type == TB_CMP_EQ) { type = TB_CMP_NE; }
            else if (type == TB_CMP_NE) { type = TB_CMP_EQ; }
            else {
                SWAP(TB_Node*, a, b);
                switch (type) {
                    case TB_CMP_ULT: type = TB_CMP_ULE; break;
                    case TB_CMP_ULE: type = TB_CMP_ULT; break;
                    case TB_CMP_SLT: type = TB_CMP_SLE; break;
                    case TB_CMP_SLE: type = TB_CMP_SLT; break;
                    default: tb_todo();
                }
            }
        }

        // you're washed if you made it here with an equal... equal? that means it's only
        // looping if it's one specific value, idk go canonicalize that loop elsewhere wtf
        if (cond->type == TB_CMP_EQ) { return false; }

        // shit's scary if both are indvars, it's not "illegal" but also wtf
        bool backwards = false;
        TB_Node *indvar = NULL, *limit = NULL;
        if (indvar = affine_indvar(a, header), indvar) {
            limit = b;
        } else if (indvar = affine_indvar(b, header), indvar) {
            limit = a;
            backwards = true;
            tb_todo();
        }

        if (indvar) {
            // we're a real affine loop now!
            TB_Node* op = indvar->inputs[2];
            TB_ASSERT(indvar->inputs[2]->type == TB_ADD || indvar->inputs[2]->type == TB_PTR_OFFSET);

            *var = (TB_InductionVar){
                .cond = cond,
                .end_cond = limit,
                .phi  = indvar,
                .step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value,
                .backwards = backwards
            };

            switch (type) {
                case TB_CMP_NE:  var->pred = IND_NE;  break;
                case TB_CMP_ULE: var->pred = IND_ULE; break;
                case TB_CMP_ULT: var->pred = IND_ULT; break;
                case TB_CMP_SLE: var->pred = IND_SLE; break;
                case TB_CMP_SLT: var->pred = IND_SLT; break;
                default: tb_todo();
            }
            return true;
        }
    } else if (affine_indvar(cond, header)) {
        if (exit_when_key || if_br->key == 0) {
            TB_Node* stepper = cond;
            if (cond->type == TB_PHI) {
                stepper = cond->inputs[2];
            }

            TB_ASSERT(stepper->type == TB_ADD || stepper->type == TB_SUB);
            int64_t step = TB_NODE_GET_EXTRA_T(stepper->inputs[2], TB_NodeInt)->value;
            if (stepper->type == TB_SUB) {
                step = -step;
            }

            *var = (TB_InductionVar){
                .cond = cond,
                .phi  = cond,
                .step = step,
                .end_const = if_br->key,
                .pred = IND_NE,
                .backwards = false
            };
            return true;
        }
    }

    return false;
}

// no clue what to call this function but if all the preds have the same dom, we can use that
// if not, we'll just pick the "easy" provided answer.
static TB_Node* easy_dom(TB_Function* f, TB_Node* n, TB_Node* easy) {
    FOR_N(i, 1, n->input_count) {
        if (f->doms[n->inputs[i]->gvn] != f->doms[n->inputs[0]->gvn]) {
            return easy;
        }
    }
    return n->inputs[0];
}

////////////////////////////////
// Loop IV optimizations
////////////////////////////////
// if we do any IV reshaping, we'll replace the latch IV with
// a simpler shape:
//
// int i = n;
// do {
//   ...
//   i -= 1;
// } while (i);
//
// the iterator should take one reg, a "i < n" check would necessitate two.

// loop strength reduction value:
//   i = phi(init, OP(i, con))
typedef struct {
    TB_NodeTypeEnum op;
    TB_Node* init;
    Lattice* step;

    TB_Node* node;
    int uses;

    bool good;
} LSRVar;

static TB_Node* generate_loop_trip_count(TB_Function* f, TB_InductionVar var) {
    // construct the exact trip count, we'll integrate all our closed-form equations based
    // on this:
    //
    // range = end - start
    Lattice* start = latuni_get(f, var.phi->inputs[1]);
    TB_Node* range = NULL;
    if (var.end_cond) {
        if (lattice_is_izero(start)) {
            range = var.end_cond;
        } else {
            range = make_int_binop(f, TB_SUB, var.end_cond, var.phi->inputs[1]);
        }
    } else {
        // we can probably fold this case
        Lattice* end = lattice_int_const(f, var.end_const);
        Lattice* sub = value_arith_raw(f, TB_SUB, var.phi->dt, end, start, false, false);
        if (lattice_is_iconst(sub)) {
            range = make_int_node(f, var.phi->dt, sub->_int.min);
        } else {
            TB_Node* end_cond = make_int_node(f, var.phi->dt, end->_int.min);
            range = make_int_binop(f, TB_SUB, end_cond, var.phi->inputs[1]);
        }
    }

    // if we're dealing with "<" then we wanna round our range up
    Lattice* pad = lattice_int_const(f, var.step - (var.pred == IND_SLT || var.pred == IND_ULT ? 1 : 0));
    if (!lattice_is_izero(pad)) {
        Lattice* range_ty = latuni_get(f, range);
        Lattice* add = value_arith_raw(f, TB_ADD, var.phi->dt, range_ty, pad, false, false);
        if (lattice_is_iconst(add)) {
            range = make_int_node(f, var.phi->dt, add->_int.min);
        } else {
            TB_Node* addend = make_int_node(f, var.phi->dt, pad->_int.min);
            range = make_int_binop(f, TB_ADD, range, addend);
        }
    }

    bool is_signed = var.pred == IND_SLT || var.pred == IND_SLE;
    if (var.step != 1) {
        TB_Node* step = make_int_node(f, var.phi->dt, var.step);
        return make_int_binop(f, is_signed ? TB_SDIV : TB_UDIV, range, step);
    }

    return range;
}

static bool loop_strength_reduce(TB_Function* f, TB_Node* header) {
    TB_Node* latch = affine_loop_latch(header);
    if (latch == NULL) {
        return false;
    }

    TB_InductionVar latch_var;
    if (!find_latch_indvar(header, latch, &latch_var)) {
        return false;
    }

    // find any IV-users to strength reduce
    ArenaArray(LSRVar) vars = aarray_create(&f->tmp_arena, LSRVar, 4);
    FOR_USERS(u, header) if (USERN(u)->type == TB_PHI) {
        TB_Node* n = USERN(u);
        if (!TB_IS_BOOL_INT_PTR(n->dt)) {
            continue;
        }

        uint64_t* step_ptr = find_affine_indvar(n, header);
        if (step_ptr == NULL) {
            continue;
        }

        #if TB_OPTDEBUG_LOOP
        printf("IV: ");
        tb_print_dumb_node(NULL, n);
        printf("\n");
        FOR_USERS(u2, n) {
            printf("  ");
            tb_print_dumb_node(NULL, USERN(u2));
            printf("\n");
        }
        tb_print_dumb_node(NULL, n->inputs[2]);
        printf("\n");
        FOR_USERS(u2, n->inputs[2]) {
            printf("  ");
            tb_print_dumb_node(NULL, USERN(u2));
            printf("\n");
        }
        #endif

        if (n->user_count <= 1) {
            continue;
        }

        LSRVar var = { 0 };
        var.op = n->inputs[2]->type;
        var.init = n->inputs[1];
        var.step = lattice_int_const(f, *step_ptr);
        var.node = n;
        var.uses = n->user_count - 1; // one of these users is just the stepping op itself, ignore it
        aarray_push(vars, var);
    }

    // if non-NULL, the new value is the accurate trip count
    TB_Node* replace_latch_iv = NULL;

    // specialize IV based on the use
    bool rewrite = false;
    for (size_t i = 0; i < aarray_length(vars); i++) {
        LSRVar* var = &vars[i];
        TB_Node* n = var->node;

        #if TB_OPTDEBUG_LOOP
        printf("[%zu] %%%u: ", i, var->node->gvn);
        print_type(&OUT_STREAM_DEFAULT, var->node->dt);

        Lattice* init = value_of(f, var->init);
        if (init->_int.min == init->_int.max) {
            printf(" = %"PRId64" ", init->_int.min);
        } else {
            printf(" = %%%u ", var->init->gvn);
        }

        printf(" + %"PRId64"*x\n", var->step->_int.min);
        #endif

        TB_Node* stepper = n->type == TB_PHI ? n->inputs[2] : NULL;
        TB_Node* reduce = NULL;
        FOR_USERS(u2, n) {
            if (USERN(u2) == stepper) {
                continue;
            } else if (reduce != NULL) {
                // can't have multiple uses of the IV
                reduce = NULL;
                break;
            } else {
                reduce = USERN(u2);
            }
        }

        TB_Node* cmp = NULL;
        if (stepper) {
            FOR_USERS(u2, stepper) {
                if (USERN(u2) == latch->inputs[1] && latch->inputs[1]->user_count == 1) {
                    cmp = latch->inputs[1];
                } else if (USERN(u2) == latch) {
                    cmp = latch;
                } else if (USERN(u2) != n) {
                    reduce = NULL;
                    break;
                }
            }
        }

        /*printf("HHH:\n");
        print_lattice(value_of(f, var->node));
        printf("\n");
        print_lattice(var->step);
        printf("\n");*/

        if (reduce != NULL && var->op == TB_ADD) {
            TB_ASSERT(lattice_is_iconst(var->step));

            uint64_t scale = 1;
            if (reduce->type == TB_SHL && lattice_is_iconst(latuni_get(f, reduce->inputs[2]))) {
                TB_ASSERT(reduce->inputs[1] == n);

                Lattice* sh_amt = latuni_get(f, reduce->inputs[2]);
                scale = 1ull << sh_amt->_int.min;
            } else if (reduce->type == TB_MUL && lattice_is_iconst(latuni_get(f, reduce->inputs[2]))) {
                // if the compare's limit can be bumped up without overflow then we'll do that too
                Lattice* l = latuni_get(f, reduce->inputs[2]);
                scale = l->_int.min;
            }

            int64_t prod;
            int bits = tb_data_type_bit_size(NULL, n->dt.type);
            if (scale != 1 && !mul_overflow(var->step->_int.min, scale, bits, &prod)) {
                prod = tb__sxt(prod, bits, 64);

                // make a simple trip counter and replace this IV
                // with a better fit.
                if (cmp && !replace_latch_iv) {
                    replace_latch_iv = generate_loop_trip_count(f, latch_var);
                }

                LSRVar new_var = *var;
                new_var.init = make_int_binop(f, TB_MUL, var->init, make_int_node(f, n->dt, scale));
                new_var.step = lattice_int_const(f, prod);
                new_var.node = reduce;
                new_var.uses = reduce->user_count; // one of these users is just the stepping op itself, ignore it
                new_var.good = true;

                var->uses -= new_var.uses;
                aarray_push(vars, new_var);

                rewrite = true;
                continue;
            }

            // NOTE(NeGate): we can safely extend values which don't overflow, maybe
            // then we shouldn't replace the latch IV
            if (reduce->type == TB_ZERO_EXT || reduce->type == TB_SIGN_EXT) {
                if (cmp && !replace_latch_iv) {
                    replace_latch_iv = generate_loop_trip_count(f, latch_var);
                }

                LSRVar new_var = *var;
                new_var.init = make_int_unary(f, reduce->dt, reduce->type, var->init);
                new_var.step = var->step;
                new_var.node = reduce;
                new_var.uses = reduce->user_count; // one of these users is just the stepping op itself, ignore it
                new_var.good = true;

                var->uses -= new_var.uses;
                aarray_push(vars, new_var);

                rewrite = true;
                continue;
            }

            // if the IV is always used as the index to some pointer, it might
            // make sense to just make a pointer IV.
            if (reduce->type == TB_PTR_OFFSET && reduce->inputs[2] == n) {
                if (cmp && !replace_latch_iv) {
                    replace_latch_iv = generate_loop_trip_count(f, latch_var);
                }

                LSRVar new_var = *var;
                new_var.init = make_ptr_offset(f, reduce->inputs[1], var->init);
                new_var.op = TB_PTR_OFFSET;
                new_var.step = var->step;
                new_var.node = reduce;
                new_var.uses = reduce->user_count; // one of these users is just the stepping op itself, ignore it
                new_var.good = true;

                var->uses -= new_var.uses;
                aarray_push(vars, new_var);

                rewrite = true;
                continue;
            }
        }
    }

    if (!rewrite) {
        return false;
    }

    if (replace_latch_iv) {
        TB_DataType dt = replace_latch_iv->dt;

        // i = phi(limit, i2);
        TB_Node* trip_inc = tb_alloc_node(f, TB_ADD, dt, 3, sizeof(TB_NodeBinopInt));
        TB_Node* trip_iv = tb_alloc_node(f, TB_PHI, dt, 3, 0);
        set_input(f, trip_iv, header, 0);
        set_input(f, trip_iv, replace_latch_iv, 1);
        set_input(f, trip_iv, trip_inc, 2);
        // i2 = i - 1;
        set_input(f, trip_inc, trip_iv, 1);
        set_input(f, trip_inc, make_int_node(f, dt, -1), 2);
        // { ... } while (i2);
        TB_Node* cond = latch->inputs[1];
        if (cond->type >= TB_CMP_EQ && cond->type <= TB_CMP_FLE) {
            // if there's more than one user, it means we couldn't have replaced this IV
            TB_ASSERT(cond->user_count == 1);
            tb_kill_node(f, cond);
        }
        set_input(f, latch, trip_inc, 1);

        mark_node(f, trip_iv);
        mark_node(f, trip_inc);
        mark_node_n_users(f, latch);

        TB_OPTDEBUG(LOOP)(printf("  Created new trip-counter IV: %%%u (limit=%%%u)\n", trip_iv->gvn, replace_latch_iv->gvn));
        TB_OPTDEBUG(PASSES)(printf("        * Added new trip-counter IV: %%%u\n", trip_iv->gvn));
    }

    // replace IVs
    TB_DataType ptr_int_dt = tb_data_type_ptr_int(f->super.module);
    for (size_t i = aarray_length(vars); i--;) {
        LSRVar* var = &vars[i];
        // kill any dead IVs now
        if (var->uses == 0) {
            if (var->node->type == TB_PHI) {
                TB_Node* stepper = var->node->inputs[2];
                TB_OPTDEBUG(LOOP)(printf("  Deleted dead IV: %%%u (step=%%%u)\n", var->node->gvn, stepper->gvn));

                set_input(f, var->node, NULL, 2);
                tb_kill_node(f, var->node);
                tb_kill_node(f, stepper);
            }
            continue;
        } else if (!var->good) {
            continue;
        }

        TB_DataType dt = var->node->dt;
        TB_Node *new_inc;
        if (var->op == TB_PTR_OFFSET) {
            new_inc = tb_alloc_node(f, TB_PTR_OFFSET, dt, 3, 0);
        } else {
            TB_ASSERT(var->op == TB_ADD);
            new_inc = tb_alloc_node(f, TB_ADD, dt, 3, sizeof(TB_NodeBinopInt));
        }
        // i = phi(n, i2);
        TB_Node* new_iv = tb_alloc_node(f, TB_PHI, dt, 3, 0);
        set_input(f, new_iv, header,    0);
        set_input(f, new_iv, var->init, 1);
        set_input(f, new_iv, new_inc,   2);
        // i2 = OP(i, step);
        set_input(f, new_inc, new_iv, 1);
        set_input(f, new_inc, make_int_node(f, var->op == TB_PTR_OFFSET ? ptr_int_dt : dt, var->step->_int.min), 2);

        mark_node(f, new_iv);
        mark_node(f, new_inc);

        TB_OPTDEBUG(PASSES)(printf("        * Added new IV %%%u (replacing %%%u)\n", new_iv->gvn, var->node->gvn));
        TB_OPTDEBUG(LOOP)(printf("  Replaced IV: %%%u => %%%u (new_step=%%%u)\n", var->node->gvn, new_iv->gvn, new_inc->gvn));

        subsume_node(f, var->node, new_iv);
        mark_node_n_users(f, latch);
    }

    return true;
}

void tb_compute_synthetic_loop_freq(TB_Function* f, TB_CFG* cfg) {
    LoopOpt ctx = { 0 };
    ctx.f      = f;
    ctx.cfg    = *cfg;
    ctx.loop_map = nl_table_alloc((f->node_count / 16) + 4);

    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
    CUIK_TIMED_BLOCK("find loops") {
        loop_find(f, cfg, &ctx);
    }

    aarray_for(i, cfg->blocks) {
        TB_BasicBlock* bb = &cfg->blocks[i];
        TB_LoopTree* loop = nl_table_get(&ctx.loop_map, bb->start);

        // 8^depth
        int depth = loop ? loop->depth : 0;
        bb->freq = 1 << (depth * 3);

        TB_Node* fallthru = bb->end;
        if (!cfg_is_terminator(fallthru)) {
            // if we're forced to jump to a different block, then we
            // inherit it's frequency if it's low.
            fallthru = cfg_next_control(fallthru);
            fallthru = nl_map_get_checked(cfg->node_to_block, fallthru)->end;
        }

        // return/trap/unreachable paths are always marked as statically unlikely
        if (fallthru->type == TB_RETURN) {
            bb->freq = 1e-1;
        } else if (fallthru->type == TB_TRAP || fallthru->type == TB_UNREACHABLE) {
            bb->freq = 1e-4;
        }
    }

    tb_arena_restore(&f->tmp_arena, sp);
    nl_table_free(ctx.loop_map);
}

static LoopOpt loop_opt_begin(TB_Function* f) {
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));
    TB_CFG cfg = tb_compute_cfg(f, f->worklist, &f->tmp_arena, true);

    LoopOpt ctx;
    ctx.f      = f;
    ctx.cfg    = cfg;
    ctx.ctrl_n = tb_next_pow2(f->node_count + 16);
    ctx.ctrl   = tb_arena_alloc(&f->tmp_arena, ctx.ctrl_n * sizeof(TB_Node*));
    ctx.loop_map = nl_table_alloc((f->node_count / 16) + 4);

    // loop analysis does dominators on the SoN directly, we do this so that we can mutate
    // the graph while maintaining doms but without the CFG (since it becomes less relevant
    // once we've spotted the loops).
    //
    // i don't fill the doms for the BB's body since it's trivial stuff
    CUIK_TIMED_BLOCK("SoN doms") {
        f->doms_n = tb_next_pow2(f->node_count + 16);
        f->doms   = tb_arena_alloc(&f->arena, f->doms_n * sizeof(TB_Node*));
        FOR_N(i, 0, f->doms_n) { f->doms[i] = NULL; }

        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            // sometimes the start and end nodes are the same, that's fine we're writing
            // the start info last for that reason
            f->doms[bb->end->gvn] = bb->start;
            f->doms[bb->start->gvn] = bb->dom->start;
        }
    }
    // we use this early scheduling to discover if nodes are loop invariant or not
    CUIK_TIMED_BLOCK("early sched") {
        TB_ArenaSavepoint sp2 = tb_arena_save(&f->tmp_arena);
        tb_global_schedule(f, f->worklist, cfg, false, NULL);
        tb_clear_anti_deps(f, f->worklist);

        FOR_N(i, 0, f->node_count) { ctx.ctrl[i] = f->scheduled[i] ? f->scheduled[i]->start : NULL; }
        FOR_N(i, f->node_count, ctx.ctrl_n) { ctx.ctrl[i] = NULL; }

        tb_arena_restore(&f->tmp_arena, sp2);
        worklist_clear(f->worklist);

        // we don't need this variant of it around
        f->scheduled = NULL;
        f->scheduled_n = 0;
    }
    CUIK_TIMED_BLOCK("find loops") {
        loop_find(f, &ctx.cfg, &ctx);
    }
    return ctx;
}

static bool loop_opt_remove_safepoints(TB_Function* f, LoopOpt* ctx) {
    #if 0
    bool progress = false;
    CUIK_TIMED_BLOCK("remove safepoints") {
        TB_ArenaSavepoint sp2 = tb_arena_save(&f->tmp_arena);

        // dominating safepoint of a block
        TB_Node** safepoints = tb_arena_alloc(&f->tmp_arena, aarray_length(cfg.blocks) * sizeof(TB_Node*));
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            // scan for the earliest safepoint
            TB_Node* curr = bb->end;
            TB_Node* sfpt = NULL;
            while (curr != bb->start) {
                if (tb_node_is_safepoint(curr)) {
                    sfpt = curr;
                }
                curr = curr->inputs[0];
            }
            safepoints[i] = sfpt;
        }

        // note that only safepoint polls can be removed, even if we find a redundant
        // function call safepoint we can't erase the call (rest of explanation left as
        // an exercise to the reader).
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            TB_Node* sfpt = safepoints[i];
            if (sfpt == NULL) {
                // walk doms (stopping at loop headers) to find a dominating safepoint, since
                // any blocks before i are resolved already we can early-exit in those cases.
                TB_BasicBlock* dom = bb->dom;
                while (dom > bb && safepoints[dom - cfg.blocks] == NULL) {
                    // unresolved so we keep walking up
                    dom = dom->dom;
                }

                sfpt = safepoints[dom - cfg.blocks];
            }

            TB_Node* curr = bb->end;
            while (curr != bb->start) {
                TB_Node* prev = curr->inputs[0];
                if (curr != sfpt && curr->type == TB_SAFEPOINT) {
                    subsume_node(f, curr, prev);
                    mark_node_n_users(f, prev);
                    progress = true;
                }
                curr = prev;
            }
        }
        tb_arena_restore(&f->tmp_arena, sp2);
    }
    return progress;
    #else
    return false;
    #endif
}

// canonicalize regions into natural loop headers (or affine loops) and
// join all backedges into one pred path on the header
static bool loop_opt_canonicalize(TB_Function* f, LoopOpt* ctx, TB_Worklist* tmp_ws) {
    bool progress = false;
    DynArray(ptrdiff_t) backedges = NULL;
    aarray_for(i, ctx->cfg.loops) {
        TB_LoopTree* loop = ctx->cfg.loops[i];
        if (!loop->is_natural) { continue; }

        TB_Node* header = loop->header;
        if (!cfg_is_region(header) && header->input_count >= 2) { continue; }

        // find all backedges
        dyn_array_clear(backedges);
        FOR_N(j, 0, header->input_count) {
            if (tb_is_dom_of(f, header, header->inputs[j])) {
                dyn_array_put(backedges, j);
            }
        }

        TB_ASSERT(dyn_array_length(backedges) >= 1);
        TB_OPTDEBUG(LOOP)(printf("found natural loop on .bb%zu (%%%u)\n", i, header->gvn));

        // as part of loop simplification we convert backedges (and entries) into one.
        ptrdiff_t single_backedge = backedges[0];
        if (dyn_array_length(backedges) > 1 || header->input_count > 2) {
            // we're splitting regions, usually the peeps would join these together but that doesn't apply
            // to loop regions.
            TB_OPTDEBUG(LOOP)(printf("region split on loop header %%%u\n", header->gvn));
            progress = true;

            // we'll make both regions regardless, the peeps will clean it up.
            int back_path_count = dyn_array_length(backedges);
            int entry_path_count = header->input_count - back_path_count;
            TB_Node* back_region  = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, back_path_count, sizeof(TB_NodeRegion));
            TB_Node* entry_region = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, entry_path_count, sizeof(TB_NodeRegion));

            size_t phi_count = 0;
            FOR_USERS(u, header) {
                if (USERN(u)->type == TB_PHI) { phi_count++; }
            }

            TB_ArenaSavepoint sp2 = tb_arena_save(&f->tmp_arena);

            // find out which are the entry paths
            bool* is_backedge = tb_arena_alloc(&f->tmp_arena, back_path_count * sizeof(bool));
            memset(is_backedge, 0, back_path_count * sizeof(bool));
            FOR_N(j, 0, back_path_count) {
                is_backedge[backedges[j]] = true;
            }

            // clone & sort phi ins into either entry or backedge phi ins
            int phi_i = 0;
            FOR_USERS(u, header) {
                if (USERN(u)->type != TB_PHI) { continue; }

                TB_Node* un = USERN(u);
                TB_Node* entry_phi = tb_alloc_node(f, TB_PHI, un->dt, 1 + entry_path_count, 0);
                TB_Node* back_phi = tb_alloc_node(f, TB_PHI, un->dt, 1 + back_path_count, 0);
                set_input(f, entry_phi, entry_region, 0);
                set_input(f, back_phi, back_region, 0);

                size_t back_i = 1, entry_i = 1;
                FOR_N(j, 1, un->input_count) {
                    if (is_backedge[j - 1]) {
                        set_input(f, back_phi, un->inputs[j], back_i);
                        back_i += 1;
                    } else {
                        set_input(f, entry_phi, un->inputs[j], entry_i);
                        entry_i += 1;
                    }
                    set_input(f, un, NULL, j);
                }

                // header phis need to get truncated now
                set_input(f, un, entry_phi, 1);
                set_input(f, un, back_phi, 2);
                un->input_count = 3;
            }

            // sort preds into regions
            size_t back_i = 0, entry_i = 0;
            FOR_N(j, 0, header->input_count) {
                if (is_backedge[j]) {
                    set_input(f, back_region, header->inputs[j], back_i);
                    back_i += 1;
                } else {
                    set_input(f, entry_region, header->inputs[j], entry_i);
                    entry_i += 1;
                }
            }

            // hook to loop region
            set_input(f, header, entry_region, 0);
            set_input(f, header, back_region,  1);
            FOR_N(i, 2, header->input_count) {
                set_input(f, header, NULL, i);
            }
            header->input_count = 2;

            // we need to make sure the back region is still dom'd by the header but if we can
            // get more accurate than that we're better off.
            tb_set_idom(f, back_region, easy_dom(f, back_region, header));
            tb_set_idom(f, entry_region, tb_get_idom_RAW(f, header));
            tb_set_idom(f, header, entry_region);

            mark_node_n_users(f, entry_region);
            mark_node_n_users(f, back_region);
            mark_node_n_users(f, header);

            tb_arena_restore(&f->tmp_arena, sp2);
            single_backedge = 1;
        }

        // somehow we couldn't simplify the loop? welp
        if (single_backedge >= 0) {
            // guarentee that the dominator is inputs[0]
            if (single_backedge == 0) {
                swap_nodes(f, header, 0, 1);
                FOR_USERS(phi, header) {
                    if (USERN(phi)->type == TB_PHI) { swap_nodes(f, USERN(phi), 1, 2); }
                }
                single_backedge = 1;
            }

            TB_NodeTypeEnum type = TB_NATURAL_LOOP;

            TB_InductionVar var;
            TB_Node* latch = NULL;

            // if there's a latch on the header, move it to the backedge. also not properly
            // rotated if there's things attached to the backedge cproj, they should've been moved above it.
            TB_Node* header_end = end_of_bb(header);
            TB_Node* exit_proj = get_simple_loop_exit(ctx, loop, header, header_end);
            // if (exit_proj && (!cfg_is_cproj(exit_proj) || exit_proj->inputs[0] != header->inputs[1]->inputs[0] || header->inputs[1]->user_count != 1)) {
            if (exit_proj && (!cfg_is_cproj(exit_proj) || header->inputs[1]->user_count != 1)) {
                #if 1
                TB_OPTDEBUG(PASSES)(printf("      * Rotating loop %%%u %p\n", header->gvn, exit_proj));

                latch = header_end;
                int exit_loop_i = TB_NODE_GET_EXTRA_T(exit_proj, TB_NodeProj)->index;
                TB_ArenaSavepoint sp2 = tb_arena_save(&f->tmp_arena);

                TB_Node** cloned;
                size_t cloned_n = f->node_count;
                CUIK_TIMED_BLOCK("alloc cloned table") {
                    cloned = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(TB_Node*));
                    memset(cloned, 0, f->node_count * sizeof(TB_Node*));
                }

                // let's rotate the loop:
                //
                //     header:                    ztc:
                //       i = phi(init, next)        ...
                //       ...                        if (A) header else exit
                //       if (A) body else exit    header:
                //     body:                        i = phi(init, next)
                //       ...                        ...
                //       jmp body                   if (A) header else exit
                //     exit:                      exit:
                //
                TB_Node* ztc_start = header->inputs[0];
                // construct the ZTC's version of the branch (same as the original latch but
                // uses the phi's inputs[1] edge instead of the phis directly)
                ArenaArray(TB_Node*) cloned_list = loop_clone_ztc(ctx, tmp_ws, cloned_n, cloned, header, latch);
                TB_Node* top_cloned = cloned[header->gvn];
                TB_Node* bot_cloned = cloned[latch->gvn];
                // make a ZTC branch
                TB_ASSERT(top_cloned->input_cap >= 1);
                top_cloned->input_count = 1;
                set_input(f, top_cloned, ztc_start, 0);
                TB_Node* into_loop = branch_cproj(f, bot_cloned, 90, 0, 1 - exit_loop_i);
                TB_Node* exit_loop = branch_cproj(f, bot_cloned, 10, 0, exit_loop_i);
                mark_node(f, into_loop), mark_node(f, exit_loop);
                // connect up to the loop
                set_input(f, header, into_loop, 0);
                // intercept exit path and place a region (merging the ZTC & rotated loop)
                TB_User after_exit = *cfg_next_user(exit_proj);
                mark_node_n_users(f, USERN(&after_exit));
                TB_Node* join = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
                set_input(f, USERN(&after_exit), join, USERI(&after_exit));
                set_input(f, join, exit_loop, 0);
                set_input(f, join, exit_proj, 1);
                // any non-CFG nodes attached to this exit projection
                // should be moved down to the new shared join point.
                loop_hoist_ops(f, exit_proj, join);
                loop_set_ctrl(ctx, join, join);
                mark_node(f, join);
                // fill in the doms
                if (bot_cloned != top_cloned) {
                    tb_set_idom(f, bot_cloned, top_cloned);
                }
                tb_set_idom(f, top_cloned, tb_walk_to_bb_bounds(f, ztc_start));
                tb_set_idom(f, header, top_cloned);
                tb_set_idom(f, join, top_cloned);
                tb_set_idom(f, exit_loop, bot_cloned);
                tb_set_idom(f, into_loop, bot_cloned);
                // latch gets moved down to the bottom (backedge)
                TB_Node* into_loop2 = USERN(proj_with_index(latch, 1 - exit_loop_i));
                {
                    // latch no longer connected to the top
                    TB_User* after_into_loop2 = cfg_next_user(into_loop2);
                    mark_node_n_users(f, USERN(after_into_loop2));
                    set_input(f, USERN(after_into_loop2), latch->inputs[0], USERI(after_into_loop2));
                    // backedge has a lovely "new" latch
                    set_input(f, latch,  header->inputs[1], 0);
                    set_input(f, header, into_loop2,        1);

                    mark_node_n_users(f, latch);
                    mark_node_n_users(f, header);
                }
                // if we were ctrl-dependent on successfully entering the
                // loop body, we need to be hooked to the header now
                loop_hoist_ops(f, into_loop2, header);

                // insert phis at the join site since we've now got two sets
                // of definitions for any nodes which are referenced outside the loop
                size_t snapshot_count = f->node_count;
                aarray_for(i, cloned_list) {
                    TB_Node* n = cloned_list[i];
                    if (n->dt.type == TB_TAG_CONTROL || cfg_is_fork(n)) {
                        continue;
                    }

                    // any nodes created during this loop close fixup shouldn't themselves need fixup btw.
                    bool is_loop_phi = n->type == TB_PHI && n->inputs[0] == header;

                    // lazily constructed
                    TB_Node* p = NULL;
                    for (size_t i = 0; i < n->user_count;) {
                        TB_Node* un = USERN(&n->users[i]);
                        int ui      = USERI(&n->users[i]);

                        // uses by our own loop phi cannot be considered "escapes"
                        if (un->type == TB_PHI && un->inputs[0] == header) {
                            i += 1;
                            continue;
                        }

                        if (un->gvn < snapshot_count && un->type != TB_CALLGRAPH) {
                            // if it's not within the loop (including the kids), it's used
                            // after the loop which means it should refer to the "new_phi"
                            TB_Node* bb = ctx->ctrl[un->gvn];
                            TB_LoopTree* use_loop = nl_table_get(&ctx->loop_map, bb);
                            if (loop != use_loop && (use_loop == NULL || bb != loop->header)) {
                                if (!loop_inside(loop, use_loop)) {
                                    if (p == NULL) {
                                        #if TB_OPTDEBUG_LOOP
                                        printf("ESCAPE! ");
                                        tb_print_dumb_node(NULL, n);
                                        printf("\n");
                                        #endif

                                        // cloned form is the init-case
                                        TB_ASSERT(n->gvn < cloned_n);
                                        TB_Node* init_path = cloned[n->gvn];
                                        TB_ASSERT(join->input_count == 2);

                                        p = tb_alloc_node(f, TB_PHI, n->dt, 3, 0);
                                        set_input(f, p, join,      0);
                                        set_input(f, p, init_path, 1);
                                        if (is_loop_phi) {
                                            set_input(f, p, n->inputs[2], 2);
                                        } else {
                                            set_input(f, p, n, 2);
                                        }
                                        loop_set_ctrl(ctx, p, join);
                                        mark_node(f, p);

                                        #if TB_OPTDEBUG_LOOP
                                        printf("   ");
                                        tb_print_dumb_node(NULL, p);
                                        printf("\n");
                                        #endif

                                        latuni_set(f, p, latuni_get(f, n));
                                    }

                                    #if TB_OPTDEBUG_LOOP
                                    printf("   USER(");
                                    tb_print_dumb_node(NULL, un);
                                    printf(", %d)\n", ui);
                                    #endif

                                    set_input(f, un, p, ui);
                                    continue;
                                }
                            }
                        }

                        i += 1;
                    }

                    if (p != NULL) { mark_node_n_users(f, p); }
                }

                progress = true;
                tb_arena_restore(&f->tmp_arena, sp2);

                TB_OPTDEBUG(PASSES)(printf("        * Added extra latch %%%u\n", latch->gvn));
                TB_OPTDEBUG(PASSES)(printf("        * Added extra join %%%u\n", join->gvn));

                #ifndef NDEBUG
                TB_Node* exit_proj2 = get_simple_loop_exit(ctx, loop, header, header_end);
                TB_ASSERT(exit_proj2->inputs[0] == latch);
                TB_ASSERT(into_loop2->inputs[0] == latch);
                TB_ASSERT(header->inputs[1] == into_loop2 && header->inputs[1]->user_count == 1);
                #endif
                #endif
            } else {
                // the loop is already rotated if there's a latch at the bottom, maybe
                // it's marked, maybe it's not.
                if (cfg_is_cproj(header->inputs[1])) {
                    TB_Node* exit_proj = get_simple_loop_exit(ctx, loop, header, header->inputs[1]->inputs[0]);
                    if (exit_proj) {
                        TB_OPTDEBUG(PASSES)(printf("      * Found rotated loop %%%u\n", header->gvn));
                        latch = exit_proj->inputs[0];
                    }
                }
            }

            if (latch != NULL) {
                TB_InductionVar var;
                if (find_latch_indvar(header, latch, &var)) {
                    type = TB_AFFINE_LOOP;

                    uint64_t* init = iconst(var.phi->inputs[1]);
                    uint64_t* end  = var.end_cond ? iconst(var.end_cond) : &var.end_const;

                    #if TB_OPTDEBUG_LOOP
                    TB_Node *phi = var.phi, *cond = var.cond;
                    int64_t step = var.step;

                    if (init) {
                        printf("  affine loop: %%%u = %"PRId64"*x + %"PRId64"\n", phi->gvn, step, *init);
                    } else {
                        printf("  affine loop: %%%u = %"PRId64"*x + %%%u\n", phi->gvn, step, phi->inputs[1]->gvn);
                    }

                    if (end) {
                        printf("        latch: %s(%%%u, %"PRId64, ind_pred_names[var.pred], phi->gvn, *end);
                    } else {
                        printf("        latch: %s(%%%u, %%%u", ind_pred_names[var.pred], phi->gvn, var.end_cond->gvn);
                    }

                    if (var.backwards) {
                        printf(", flipped");
                    }
                    printf(")\n");

                    // fixed trip count loops aren't *uncommon*
                    if (init && end) {
                        int64_t pad = step - (var.pred == IND_SLT || var.pred == IND_ULT ? 1 : 0);
                        uint64_t trips = (*end - *init + pad) / step;
                        uint64_t rem   = (*end - *init + pad) % step;
                        if (rem != 0 && var.pred == IND_NE) {
                            printf("        trips: overshoot\n");
                        } else {
                            printf("        trips: %"PRId64" (%"PRId64" ... %"PRId64")\n", trips, *init, *end);
                        }
                    }
                    #endif
                }
            }

            // first time we've seen this loop
            if (header->type != type) {
                progress = true;
                header->type = type;
                mark_node_n_users(f, header);

                // all affine loops have an affine latch
                if (type == TB_AFFINE_LOOP) {
                    header->inputs[1]->inputs[0]->type = TB_AFFINE_LATCH;
                    mark_node_n_users(f, header->inputs[1]->inputs[0]);
                }
            }
        }
    }

    return progress;
}
