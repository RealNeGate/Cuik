
// walks up until the node has a dom entry (it's a start or end node)
static TB_Node* walk_to_bb_bounds(TB_Function* f, TB_Node* n) {
    // if dom[n] is NULL we just follow the inputs[0]
    while (f->doms[n->gvn] == NULL) {
        TB_ASSERT(!cfg_is_region(n));
        n = n->inputs[0];
    }
    return n;
}

static TB_Node* get_idom(TB_Function* f, TB_Node* n) {
    // if dom[n] is NULL we just follow the inputs[0]
    while (f->doms[n->gvn] == NULL) {
        TB_ASSERT(!cfg_is_region(n));
        n = n->inputs[0];
    }
    return f->doms[n->gvn];
}

static bool is_dom(TB_Function* f, TB_Node* expected_dom, TB_Node* n) {
    n = walk_to_bb_bounds(f, n);
    while (n != expected_dom && n != f->doms[n->gvn]) {
        n = f->doms[n->gvn];
    }
    return n == expected_dom;
}

static void set_idom(TB_Function* f, TB_Node* n, TB_Node* new_dom) {
    TB_ASSERT_MSG(n != new_dom, "probably a mistake (the entry dom was defined without this function)");
    if (n->gvn >= f->doms_n) {
        size_t new_cap = tb_next_pow2(n->gvn + 16);

        f->doms = tb_arena_realloc(&f->arena, f->doms, f->doms_n * sizeof(TB_Node*), new_cap * sizeof(TB_Node*));
        FOR_N(i, f->doms_n, new_cap) { f->doms[i] = NULL; }

        f->doms_n = new_cap;
    }
    f->doms[n->gvn] = new_dom;
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

// clone anything except control edges and phis to region
static TB_Node* loop_clone_node(TB_Function* f, TB_Node** cloned, size_t pre_clone_index, TB_Node* header, TB_Node* n) {
    if (n->gvn >= pre_clone_index) {
        return n;
    } else if (cloned[n->gvn]) {
        return cloned[n->gvn];
    }

    TB_Node* k = n;
    if (n->type == TB_PHI) {
        if (n->inputs[0] == header) {
            // replace OG with loop phi's initial value
            k = n->inputs[1];
        }
    } else if (cfg_is_region(n) || n->type == TB_ROOT || (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT)) {
        // doesn't clone
    } else {
        size_t extra = extra_bytes(n);
        k = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);

        // clone extra data (i hope it's that easy lol)
        memcpy(k->extra, n->extra, extra);

        // fill cloned edges
        FOR_N(i, 1, n->input_count) if (n->inputs[i]) {
            TB_Node* in = loop_clone_node(f, cloned, pre_clone_index, header, n->inputs[i]);
            k->inputs[i] = in;
            add_user(f, k, in, i);

            // uncloned form should refer to the "next" edge on the phi
            if (n->inputs[i]->type == TB_PHI && n->inputs[i]->inputs[0] == header && n != n->inputs[i]->inputs[2]) {
                set_input(f, n, n->inputs[i]->inputs[2], i);
            }
        }

        // keep original ctrl edge, we may replace it later tho
        if (n->inputs[0]) {
            k->inputs[0] = n->inputs[0];
            add_user(f, k, n->inputs[0], 0);
        }

        // mark new node
        mark_node(f, k);
    }
    cloned[n->gvn] = k;

    #if TB_OPTDEBUG_LOOP
    printf("CLONE: ");
    tb_print_dumb_node(NULL, n);
    printf(" => ");
    tb_print_dumb_node(NULL, k);
    printf("\n");
    #endif

    return k;
}

static uint64_t* iconst(TB_Node* n) {
    return n->type == TB_ICONST ? &TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value : NULL;
}

static void swap_nodes(TB_Function* f, TB_Node* n, int i, int j) {
    TB_Node* old = n->inputs[i];
    set_input(f, n, n->inputs[j], i);
    set_input(f, n, old, j);
}

static TB_Node* branch_cproj(TB_Function* f, TB_Node* n, uint64_t taken, int64_t key, int index) {
    TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
    set_input(f, cproj, n, 0);
    TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = index, .taken = taken, .key = key);
    return cproj;
}

static void hoist_ops(TB_Function* f, TB_Node* ctrl, TB_Node* earlier) {
    for (size_t i = 0; i < ctrl->user_count;) {
        TB_Node* un = USERN(&ctrl->users[i]);
        int ui      = USERI(&ctrl->users[i]);
        if (un->type == TB_LOAD && ui == 0) {
            set_input(f, un, earlier, 0);
        } else {
            i += 1;
        }
    }
}

static void mark_reachable(TB_Function* f, uint32_t* before, TB_Node* n) {
    assert(n->gvn < f->node_count);
    if (before[n->gvn / 32] & (1u << (n->gvn % 32))) {
        return;
    } else if (n->type == TB_ROOT) {
        // don't go past root, you'll wrap around and it'll be
        // nasty
        return;
    } else if (cfg_is_region(n)) {
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            mark_reachable(f, before, USERN(u));
        }
    }

    #if TB_OPTDEBUG_LOOP
    printf("REACH: ");
    tb_print_dumb_node(NULL, n);
    printf("\n");
    #endif

    before[n->gvn / 32] |= (1u << (n->gvn % 32));
    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        mark_reachable(f, before, n->inputs[i]);
    }
}

static void replace_phis(TB_Function* f, uint32_t* before, TB_Node* phi, TB_Node* new_phi) {
    for (size_t i = 0; i < phi->user_count;) {
        TB_Node* un = USERN(&phi->users[i]);
        int ui      = USERI(&phi->users[i]);
        if ((before[un->gvn / 32] & (1u << (un->gvn % 32))) == 0) {
            // printf("PHI %%%-3u was used past the loop at %%%-3u (%%%-3u -> %%%-3u)\n", phi->gvn, un->gvn, un->inputs[ui]->gvn, new_phi->gvn);
            set_input(f, un, new_phi, ui);
        } else {
            i += 1;
        }
    }
}

static TB_Node* get_simple_loop_exit(TB_Function* f, TB_Node* header, TB_Node* latch) {
    if ((latch->type != TB_BRANCH && latch->type != TB_AFFINE_LATCH) || TB_NODE_GET_EXTRA_T(latch, TB_NodeBranch)->succ_count != 2) {
        return NULL;
    }

    // for this latch to count it needs to exit (have a path not dominated
    // by the loop's backedge)
    TB_Node* exit = NULL;
    TB_Node* backedge_bb = header->inputs[1];
    FOR_USERS(u, latch) {
        if (USERN(u)->type != TB_BRANCH_PROJ) { continue; }
        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
        assert(index < 2);

        TB_Node* succ = cfg_next_bb_after_cproj(USERN(u));
        if (!is_dom(f, succ, backedge_bb)) {
            // successor doesn't dom backedge, we're leaving the loop then
            if (exit) { return NULL; }
            else { exit = USERN(u);  }
        }
    }

    return exit;
}

static const char* ind_pred_names[] = { "ne", "slt", "sle", "ult", "ule" };

// since we're looking at the rotated form:
//
//   i  = phi(init, i2)
//   i2 = i + step where step is constant
static bool affine_indvar(TB_Node* n, TB_Node* header) {
    return n->type == TB_ADD && n->dt.type == TB_TAG_INT
        && n->inputs[1]->type == TB_PHI
        && n->inputs[1]->inputs[0] == header
        && n->inputs[1]->inputs[2] == n
        && n->inputs[2]->type == TB_ICONST;
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
        if ((exit_when_key && if_br->key == 0) ||
            (!exit_when_key && if_br->key == 1)) {
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
        if (affine_indvar(a, header))      { indvar = a->inputs[1], limit = b; }
        else if (affine_indvar(b, header)) { indvar = b->inputs[1], limit = a, backwards = true; }

        if (indvar) {
            // we're a real affine loop now!
            TB_Node* op = indvar->inputs[2];
            assert(indvar->inputs[2]->type == TB_ADD);

            *var = (TB_InductionVar){
                .cond = cond,
                .end_cond = limit,
                .phi  = indvar,
                .step = TB_NODE_GET_EXTRA_T(op->inputs[2], TB_NodeInt)->value,
                .backwards = false
            };

            switch (cond->type) {
                case TB_CMP_NE:  var->pred = IND_NE;  break;
                case TB_CMP_ULE: var->pred = IND_ULE; break;
                case TB_CMP_ULT: var->pred = IND_ULT; break;
                case TB_CMP_SLE: var->pred = IND_SLE; break;
                case TB_CMP_SLT: var->pred = IND_SLT; break;
                default: tb_todo();
            }
            return true;
        }
    } else if (affine_indvar(cond, header) && exit_when_key) {
        assert(cond->type == TB_ADD);
        *var = (TB_InductionVar){
            .cond = cond,
            .phi  = cond,
            .step = TB_NODE_GET_EXTRA_T(cond->inputs[2], TB_NodeInt)->value,
            .end_const = if_br->key,
            .pred = IND_NE,
            .backwards = false
        };
        return true;
    }

    return false;
}

static void compute_loop_depth(TB_LoopTree* loop, int d) {
    while (loop) {
        #if TB_OPTDEBUG_LOOP
        FOR_N(i, 0, d) { printf("  "); }
        printf("  Loop(depth: %d", d);
        if (loop->header) {
            printf(", header: %%%u", loop->header->start->gvn);
        }
        printf(")\n");
        #endif

        loop->depth = d;
        if (loop->kid) {
            compute_loop_depth(loop->kid, d + 1);
        }
        loop = loop->next;
    }
}

static TB_LoopTree* loop_uf_find(TB_LoopTree* a) {
    while (a->parent != a) { a = a->parent; }
    return a;
}

static void loop_add_kid(TB_LoopTree* kid, TB_LoopTree* mom) {
    kid->parent = mom;
    kid->next   = mom->kid;
    mom->kid    = kid;
}

static void add_loop_backedge(TB_CFG* cfg, TB_Arena* arena, TB_BasicBlock* header, TB_BasicBlock* backedge) {
    bool is_natural = slow_dommy2(header, backedge);
    TB_OPTDEBUG(LOOP)(printf("  * found backedge BB%zu -> BB%zu (%s)\n", header - cfg->blocks, backedge - cfg->blocks, is_natural ? "reducible" : "irreducible"));
    if (!is_natural) {
        return;
    }

    // blocks in the loop body must appear in this range
    // but also dominate the backedge block.
    size_t start_i = header - cfg->blocks, end_i = backedge - cfg->blocks;
    TB_ASSERT_MSG(end_i > 0, "entry block cannot be a loop... how would it?");

    int new_body_min = start_i / 64;
    int new_body_max = (end_i + 63) / 64;

    TB_LoopTree* loop = header->loop;
    if (loop) {
        int old_body_min = loop->body_offset;
        int old_body_max = loop->body_offset + loop->body_count;

        // resize loop tree
        int new_body_count = TB_MAX(new_body_max, old_body_max) - TB_MIN(new_body_min, old_body_min);
        if (old_body_min != new_body_min || loop->body_count != new_body_count) {
            tb_todo();
        }
    } else {
        loop = header->loop = tb_arena_alloc(arena, sizeof(TB_LoopTree) + (new_body_max - new_body_min)*sizeof(uint64_t));
        *loop = (TB_LoopTree){
            // this means it's a top-level loop, it'll get positioned correctly later
            .parent      = loop,
            .header      = header,
            .is_natural  = is_natural,
            .id          = 1 + aarray_length(cfg->loops),
            .body_offset = new_body_min,
            .body_count  = new_body_max - new_body_min
        };
        FOR_N(i, 0, loop->body_count) { loop->body[i] = 0; }

        aarray_push(cfg->loops, loop);
    }

    TB_OPTDEBUG(LOOP)(printf("  body: "));
    int body_offset = loop->body_offset;
    if (is_natural) {
        TB_OPTDEBUG(LOOP)(printf("BB%zu  ", start_i));

        int local_i = start_i - body_offset*64;
        loop->body[local_i / 64] |= 1ull << (local_i % 64);
    }

    FOR_N(i, start_i + 1, end_i) {
        TB_BasicBlock* bb = &cfg->blocks[i];
        if (slow_dommy2(bb, backedge)) {
            TB_OPTDEBUG(LOOP)(printf("BB%zu  ", i));

            int local_i = i - body_offset*64;
            loop->body[local_i / 64] |= 1ull << (local_i % 64);
        }
    }
    TB_OPTDEBUG(LOOP)(printf("\n"));
}

bool tb_opt_loops(TB_Function* f) {
    cuikperf_region_start("loop opts", NULL);
    bool progress = false;

    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));
    TB_CFG cfg = tb_compute_cfg(f, f->worklist, &f->tmp_arena, TB_CFG_DOMS);

    CUIK_TIMED_BLOCK("find loops") {
        TB_OPTDEBUG(LOOP)(printf("\n%s: Loop Finding:\n", f->super.name));
        cfg.loops = aarray_create(&f->tmp_arena, TB_LoopTree*, 32);
        // succ <= i means we've got a cycle edge, if i doms succ it's a natural loop.
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            TB_Node* end = bb->end;

            if (cfg_is_fork(end)) {
                FOR_USERS(u, end) {
                    if (cfg_is_cproj(USERN(u))) {
                        TB_Node* succ = cfg_next_bb_after_cproj(USERN(u));
                        TB_BasicBlock* succ_bb = nl_map_get_checked(cfg.node_to_block, succ);
                        if (succ_bb <= bb) { add_loop_backedge(&cfg, &f->tmp_arena, succ_bb, bb); }
                    }
                }
            } else if (!cfg_is_endpoint(end)) {
                TB_Node* succ = USERN(cfg_next_user(end));
                TB_BasicBlock* succ_bb = nl_map_get_checked(cfg.node_to_block, succ);
                if (succ_bb <= bb) { add_loop_backedge(&cfg, &f->tmp_arena, succ_bb, bb); }
            }
        }
    }
    // union-find to organize the loop tree, any intersecting loops will
    // be sorted by who doms who
    CUIK_TIMED_BLOCK("union-find") {
        TB_OPTDEBUG(LOOP)(printf("\n%s: Union-Find:\n", f->super.name));
        FOR_N(i, 0, aarray_length(cfg.loops)) {
            TB_LoopTree* restrict loop = cfg.loops[i];

            TB_OPTDEBUG(LOOP)(printf("  Loop%zu(header: %%%u, body:", i, loop->header->start->gvn));
            FOR_N(j, 0, loop->body_count) {
                FOR_BIT(k, j*64, loop->body[j]) {
                    TB_OPTDEBUG(LOOP)(printf(" BB%zu", loop->body_offset*64 + k));
                }
            }
            TB_OPTDEBUG(LOOP)(printf(")\n"));

            FOR_N(j, i + 1, aarray_length(cfg.loops)) {
                TB_LoopTree* restrict other = cfg.loops[j];
                TB_OPTDEBUG(LOOP)(printf("    * Intersect with Loop%zu?\n", j));

                // broad-phase intersection (they're at least in the same 64 block area)
                int start = TB_MAX(loop->body_offset, other->body_offset);
                int end   = TB_MIN(loop->body_offset + loop->body_count, other->body_offset + other->body_count);
                if (start >= end) { continue; }

                // narrow-phase intersection
                bool intersect = false;
                FOR_N(k, start, end) {
                    TB_ASSERT(k >= loop->body_offset && k < loop->body_offset + loop->body_count);
                    TB_ASSERT(k >= other->body_offset && k < other->body_offset + other->body_count);

                    uint64_t x = loop->body[k - loop->body_offset] & other->body[k - other->body_offset];
                    if (x) { // intersection? ok time to union
                        intersect = true;
                        break;
                    }
                }

                if (intersect) {
                    TB_LoopTree* a = loop_uf_find(loop);
                    TB_LoopTree* b = loop_uf_find(other);

                    if (a != b) { // not already in the same set
                        TB_ASSERT(a->header != b->header);
                        if (slow_dommy2(a->header, b->header)) {
                            TB_OPTDEBUG(LOOP)(printf("    * Loop%d is inside Loop%d\n", b->id, a->id));
                            TB_ASSERT(b->parent == b);
                            loop_add_kid(b, a);
                        } else if (slow_dommy2(b->header, a->header)) {
                            TB_OPTDEBUG(LOOP)(printf("    * Loop%d is inside Loop%d\n", a->id, b->id));
                            TB_ASSERT(a->parent == a);
                            loop_add_kid(a, b);
                        }
                    }
                }
            }
        }

        // anything without a parent will be stitched to the root "loop" (not a real loop, just the function itself)
        cfg.root_loop = tb_arena_alloc(&f->tmp_arena, sizeof(TB_LoopTree));
        *cfg.root_loop = (TB_LoopTree){ 0 };
        FOR_N(i, 0, aarray_length(cfg.loops)) {
            TB_LoopTree* restrict loop = cfg.loops[i];
            if (loop->parent == loop) {
                loop_add_kid(loop, cfg.root_loop);
            }
        }

        // loop depths are sometimes used for synthetic frequencies... and that's about it for now?
        TB_OPTDEBUG(LOOP)(printf("\n%s: Final analysis:\n", f->super.name));
        compute_loop_depth(cfg.root_loop, 0);
    }
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

            // if end is a branch, we'll just fill in the doms of the branch projs
            if (cfg_is_fork(bb->end)) {
                FOR_USERS(u, bb->end) {
                    if (cfg_is_cproj(USERN(u))) {
                        f->doms[USERN(u)->gvn] = bb->end;
                    }
                }
            }

            f->doms[bb->end->gvn] = bb->start;
            f->doms[bb->start->gvn] = bb->dom->start;
        }
    }

    // canonicalize regions into natural loop headers (or affine loops)
    DynArray(ptrdiff_t) backedges = NULL;
    aarray_for(i, cfg.loops) {
        TB_LoopTree* loop = cfg.loops[i];
        if (!loop->is_natural) { continue; }

        TB_Node* header = loop->header->start;
        if (!cfg_is_region(header) || header->input_count < 2) { continue; }

        // find all backedges
        dyn_array_clear(backedges);
        FOR_N(j, 0, header->input_count) {
            if (is_dom(f, header, header->inputs[j])) {
                dyn_array_put(backedges, j);
            }
        }

        TB_ASSERT(dyn_array_length(backedges) >= 1);
        TB_OPTDEBUG(LOOP)(printf("found natural loop on .bb%zu (v%u)\n", i, header->gvn));

        // as part of loop simplification we convert backedges into one, this
        // makes it easier to analyze the exit condition.
        ptrdiff_t single_backedge = backedges[0];
        if (dyn_array_length(backedges) > 1 || header->input_count > 2) {
            // TODO(NeGate): i haven't thought about if we care much yet...
            single_backedge = -1;
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

            // if we don't have the latch in the header
            TB_BasicBlock* header_info = nl_map_get_checked(cfg.node_to_block, header);

            TB_InductionVar var;
            TB_Node* latch = NULL;

            // if there's a latch on the header, move it to the backedge. also not properly
            // rotated if there's things attached to the backedge cproj, they should've been moved above it.
            TB_Node* exit_proj = get_simple_loop_exit(f, header, header_info->end);
            if (exit_proj && (exit_proj->type != TB_BRANCH_PROJ || exit_proj->inputs[0] != header->inputs[1]->inputs[0] || header->inputs[1]->user_count != 1)) {
                TB_OPTDEBUG(PASSES)(printf("      * Rotating loop %%%u\n", header->gvn));

                latch = header_info->end;
                int exit_loop_i = TB_NODE_GET_EXTRA_T(exit_proj, TB_NodeProj)->index;
                TB_ArenaSavepoint sp2 = tb_arena_save(&f->tmp_arena);

                TB_Node** cloned;
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
                // construct the ZTC's version of the branch (same as the original latch but
                // uses the phi's inputs[1] edge instead of the phis directly)
                size_t pre_clone_index = f->node_count;
                TB_Node* ztc_start = header->inputs[0];
                TB_Node *bot_cloned = NULL, *top_cloned = NULL;
                for (TB_Node* curr = latch; curr != header; curr = curr->inputs[0]) {
                    TB_Node* k = loop_clone_node(f, cloned, pre_clone_index, header, curr);
                    mark_users(f, k);

                    // attach control edge
                    if (top_cloned) {
                        set_input(f, top_cloned, k, 0);
                    } else {
                        bot_cloned = k;
                    }
                    top_cloned = k;
                }
                // make a ZTC branch
                set_input(f, top_cloned, ztc_start, 0);
                TB_Node* into_loop = branch_cproj(f, bot_cloned, 90, 0, 1 - exit_loop_i);
                TB_Node* exit_loop = branch_cproj(f, bot_cloned, 10, 0, exit_loop_i);
                mark_node(f, into_loop), mark_node(f, exit_loop);
                // connect up to the loop
                set_input(f, header, into_loop, 0);
                // intercept exit path and place a region (merging the ZTC & rotated loop)
                TB_User* after_exit = cfg_next_user(exit_proj);
                mark_node_n_users(f, USERN(after_exit));
                TB_Node* join = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
                set_input(f, join, exit_loop, 0);
                set_input(f, join, exit_proj, 1);
                set_input(f, USERN(after_exit), join, USERI(after_exit));
                mark_node(f, join);
                // fill in the doms
                if (bot_cloned != top_cloned) {
                    set_idom(f, bot_cloned, top_cloned);
                }
                set_idom(f, top_cloned, walk_to_bb_bounds(f, ztc_start));
                set_idom(f, header, top_cloned);
                set_idom(f, join, top_cloned);
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
                // loads with control deps on the loop's body can also
                // safe once you're guarenteed to run at least once (ZTC)
                hoist_ops(f, into_loop2, into_loop);
                // sometimes there's things leftover which are still dependent on the latch's backedge
                for (size_t i = 0; i < into_loop2->user_count;) {
                    TB_Node* un = USERN(&into_loop2->users[i]);
                    int ui      = USERI(&into_loop2->users[i]);
                    if (ui == 0 && un != header) {
                        set_input(f, un, into_loop, ui);
                    } else {
                        i += 1;
                    }
                }
                // some loop phis escape the loop, we wanna tie these to the exit phis not the
                // loop body phis (since we've constructed two exit paths now).
                CUIK_TIMED_BLOCK("discover loop reachability") {
                    uint32_t* before = tb_arena_alloc(&f->tmp_arena, ((f->node_count + 31) / 32) * sizeof(uint32_t));
                    memset(before, 0, ((f->node_count + 31) / 32) * sizeof(uint32_t));

                    // TODO(NeGate): there's probably a faster way to solve for this btw
                    mark_reachable(f, before, header);

                    FOR_USERS(u, header) if (USERN(u)->type == TB_PHI) {
                        assert(USERI(u) == 0);
                        TB_Node* phi = USERN(u);
                        TB_Node* new_phi = tb_alloc_node(f, TB_PHI, phi->dt, 3, 0);
                        set_input(f, new_phi, join,           0);
                        set_input(f, new_phi, phi->inputs[1], 1);
                        set_input(f, new_phi, phi->inputs[2], 2);

                        replace_phis(f, before, phi, new_phi);
                        mark_node(f, new_phi);
                    }
                }
                progress = true;
                tb_arena_restore(&f->tmp_arena, sp2);

                TB_OPTDEBUG(PASSES)(printf("        * Added extra latch %%%u\n", latch->gvn));
                TB_OPTDEBUG(PASSES)(printf("        * Added extra join %%%u\n", join->gvn));
            } else {
                // the loop is already rotated if there's a latch at the bottom, maybe
                // it's marked, maybe it's not.
                if (header->inputs[1]->type == TB_BRANCH_PROJ) {
                    TB_Node* exit_proj = get_simple_loop_exit(f, header, header->inputs[1]->inputs[0]);
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
                        int64_t trips = (*end - *init) / step;
                        int64_t rem   = (*end - *init) % step;
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
    tb_free_cfg(&cfg);

    f->doms = NULL;
    f->doms_n = 0;

    tb_arena_clear(&f->tmp_arena);
    cuikperf_region_end();
    return progress;
}
