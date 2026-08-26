// This is where shared RA stuff goes.
#include "ra.h"

// *ra = (RABase){ .ctx = ctx, .arena = arena };

static TB_Node* tb__ra_hard_split(Ctx* ctx, RABase* ra, TB_Node* n, TB_Node* in, RegMask* rm, int vreg_id);
static void tb__ra_remat(Ctx* ctx, RABase* ra, TB_Node* n, bool kill_node);

void tb__ra_init(RABase* ra, TB_Arena* arena) {
    Ctx* ctx = ra->ctx;
    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;

    TB_Worklist* ws = f->worklist;
    worklist_clear(ws);
    ra->uf_len = f->node_count;

    int max_regs_in_class = 0;
    FOR_N(i, 0, ctx->num_classes) {
        size_t count = ctx->num_regs[i];
        if (max_regs_in_class < count) {
            max_regs_in_class = count;
        }
    }
    ra->num_regs = ctx->num_regs;
    ra->max_regs_in_class = max_regs_in_class;

    // used for hard-list list at the very start
    ra->new_vregs = dyn_array_create(int, 32);
    ra->dead_vregs = dyn_array_create(int, 32);
    bool cisc_stuff = false;

    // create timeline & insert moves
    CUIK_TIMED_BLOCK("insert legalizing moves") {
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                RegMask** ins = ctx->ins;
                ctx->constraint(ctx, n, ins);

                // insert input copies (temporaries & clobbers never introduce
                // these so we're safe don't check those)
                size_t in_count = n->input_count;
                FOR_N(k, 1, in_count) if (n->inputs[k]) {
                    TB_Node* in = n->inputs[k];
                    RegMask* in_mask = ins[k];
                    if (in_mask == &TB_REG_EMPTY) { continue; }

                    VReg* in_vreg = node_vreg(ctx, in);

                    #ifndef NDEBUG
                    // common enough error that i figure i should make a proper error
                    if (in_vreg == NULL) {
                        printf("RA ERROR in %s (%s:%d):\n  ", f->super.name, __FILE__, __LINE__);
                        tb_print_dumb_node(NULL, in);
                        printf("\n  ^^^^^  this node has no vreg even though it's used by %%%u[%zu] in BB%zu:\n  ", n->gvn, k, i);
                        tb_print_dumb_node(NULL, n);
                        printf("\n");
                        tb_integrated_dbg(f, n);
                    }
                    #endif

                    /* int hint = fixed_reg_mask(in_mask);
                    if (hint >= 0 && in_vreg->mask->class == in_mask->class) {
                    in_vreg->hint_vreg = ra->fixed[in_mask->class] + hint;
                    } */

                    // intersect use masks with the vreg's mask, if it becomes empty we've
                    // got a hard-split (not necessarily spilling to the stack)
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[k]);
                    if (in_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                        TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs));
                        dyn_array_put(ra->new_vregs, in_vreg - ctx->vregs);
                    }

                    in_vreg->mask = new_mask;
                }

                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id > 0) {
                    VReg* vreg = &ctx->vregs[vreg_id];
                    RegMask* def_mask = vreg->mask;

                    vreg->spill_cost = NAN;
                    if (n->type == TB_PHI) {
                        ra->uf_len += n->input_count;
                        worklist_push(ws, n);
                    } else if (ctx->node_2addr(n) >= 0) {
                        ra->uf_len += 1;
                        cisc_stuff = true;
                    }
                }
            }
        }
    }

    // this avoids the subsume_node calls adding nodes to the list, they'd
    // do this if you remove nodes such that they get DCE'd
    f->worklist = NULL;

    // resolving hard-splits
    if (dyn_array_length(ra->new_vregs) > 0) {
        cuikperf_region_start("hard splits", NULL);
        // insert hard split code
        FOR_N(i, 0, dyn_array_length(ra->new_vregs)) {
            VReg* vreg = &ctx->vregs[ra->new_vregs[i]];
            RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
            vreg->mask = mask;

            if (can_remat(ctx, vreg->n)) {
                tb__ra_remat(ctx, ra, vreg->n, true);
            } else {
                spill_entire_lifetime(ctx, vreg, mask, vreg->n, true);
            }

            ra->uf_len += 1;
        }
        dyn_array_clear(ra->new_vregs);
        cuikperf_region_end();

        f->worklist = ws;
        redo_dataflow(ctx, arena);
        f->worklist = NULL;
    }

    ra->coalesce_set = nl_table_alloc(100);
    ra->uf = cuik_malloc(ra->uf_len * sizeof(int));
    ra->uf_size = cuik_malloc(ra->uf_len * sizeof(int));
    FOR_N(i, 0, ra->uf_len) {
        ra->uf[i] = i;
        ra->uf_size[i] = 1;
    }

    bool changes = false;
    if (cisc_stuff || dyn_array_length(ws->items) > 0) {
        cuikperf_region_start("aggro coalesce", NULL);

        TB_ArenaSavepoint sp = tb_arena_save(arena);
        ra->rebuild_intr(ctx, ra);

        // CISC ops will coalesce with their shared edge, if not they'll insert a copy node
        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            FOR_REV_N(j, 0, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];
                if (ctx->vreg_map[n->gvn] == 0) { continue; }

                int shared_edge = ctx->node_2addr(n);
                if (shared_edge >= 0 && n->inputs[shared_edge]) {
                    int x = uf_find(ra->uf, ra->uf_len, n->gvn);
                    int vreg_id = ctx->vreg_map[x];
                    TB_Node* in = n->inputs[shared_edge];

                    // this is a necessary copy
                    if (n->type == TB_MACH_COPY) {
                        continue;
                    }

                    int y = uf_find(ra->uf, ra->uf_len, in->gvn);

                    RegMask* in_mask = constraint_in(ctx, n, shared_edge);
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_mask, ctx->vregs[ctx->vreg_map[y]].mask);
                    new_mask = tb__reg_mask_meet(ctx, new_mask, ctx->vregs[vreg_id].mask);

                    if (!tb__ra_can_coalesce(ra, n, in) || new_mask == &TB_REG_EMPTY) {
                        // insert a copy
                        TB_OPTDEBUG(REGALLOC)(printf("CISC OP %%%u (-> %%%u) has conflict\n", n->gvn, in->gvn));

                        TB_Node* copy = tb__ra_hard_split(ctx, ra, n, in, in_mask, vreg_id);
                        // any other inputs which use the original edge could use the copy for simplicity
                        FOR_N(k, 0, n->input_count) {
                            if (n->inputs[k] == in) {
                                set_input(f, n, copy, k);
                            }
                        }
                        tb__insert_before(ctx, ctx->f, copy, n);

                        if (in->user_count == 0) {
                            // delete the original def
                            ctx->vregs[ctx->vreg_map[in->gvn]].uses -= 1;
                            ctx->vreg_map[in->gvn] = 0;
                            tb__remove_node(ctx, ctx->f, in);
                            tb_kill_node(ctx->f, in);
                        }

                        in = n->inputs[shared_edge];
                        changes = true;
                    } else {
                        ctx->vregs[vreg_id].mask = new_mask;
                    }

                    // hard coalesce with direct input
                    y = uf_find(ra->uf, ra->uf_len, in->gvn);
                    tb__ra_coalesce(ra, x, y, n, in);
                }
            }
        }

        FOR_N(i, 0, dyn_array_length(ws->items)) {
            TB_Node* n = ws->items[i];
            TB_ASSERT(n->type == TB_PHI);

            // join all these into one lifetime, make n the leader
            int x = uf_find(ra->uf, ra->uf_len, n->gvn);
            int vreg_id = ctx->vreg_map[x];

            RegMask* rm = ctx->vregs[vreg_id].mask;
            FOR_N(k, 1, n->input_count) {
                // interfere against everything in the set
                TB_Node* in = n->inputs[k];
                int y = uf_find(ra->uf, ra->uf_len, in->gvn);

                RegMask* new_mask = tb__reg_mask_meet(ctx, rm, ctx->vregs[ctx->vreg_map[y]].mask);
                if (!tb__ra_can_coalesce(ra, n, in) || new_mask == &TB_REG_EMPTY) {
                    TB_OPTDEBUG(REGALLOC)(printf("PHI %%%u (-> %%%u) has self-conflict\n", n->gvn, in->gvn));

                    TB_Node* move = tb__ra_hard_split(ctx, ra, n, in, rm, vreg_id);
                    set_input(f, n, move, k);

                    rm = tb__reg_mask_meet(ctx, rm, ctx->constraint(ctx, move, NULL));
                    TB_ASSERT(rm != &TB_REG_EMPTY);

                    TB_Node* pred = cfg_get_pred(&ctx->cfg, n->inputs[0], k - 1);
                    TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
                    TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

                    if (in->user_count == 0) {
                        // delete the original def
                        ctx->vregs[ctx->vreg_map[in->gvn]].uses -= 1;
                        ctx->vreg_map[in->gvn] = 0;
                        tb__remove_node(ctx, ctx->f, in);
                        tb_kill_node(ctx->f, in);
                    }

                    // place at the end of the pred BB to the phi, basically the latest point
                    insert_op_at_end(ctx, NULL, pred_bb, move);
                    changes = true;
                } else {
                    rm = new_mask;
                }

                // hard coalesce with direct input
                y = uf_find(ra->uf, ra->uf_len, n->inputs[k]->gvn);
                tb__ra_coalesce(ra, x, y, n, n->inputs[k]);
            }

            TB_ASSERT(x == ra->uf[x]); // must've stayed the head
            ctx->vregs[ctx->vreg_map[x]].mask = rm;
        }

        // compute lists of coalesced nodes
        FOR_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            aarray_for(j, bb->items) {
                TB_Node* n = bb->items[j];
                int vreg_id = ctx->vreg_map[n->gvn];
                if (vreg_id == 0) { continue; }

                // remap the vregs
                int leader = uf_find(ra->uf, ra->uf_len, n->gvn);
                if (leader != n->gvn) {
                    ctx->vreg_map[n->gvn] = ctx->vreg_map[leader];
                }
            }
        }
        ra->interfere_dirty = true;

        tb_arena_restore(&f->tmp_arena, sp);
        cuikperf_region_end();
    }
    // RA calls might add dead nodes but we don't care
    f->worklist = ws;

    if (changes) {
        // recompute liveness
        redo_dataflow(ctx, arena);
    }

    ra->num_spills = ctx->num_regs[REG_CLASS_STK];
    ra->hrp = tb_arena_alloc(arena, ctx->bb_count * sizeof(HRPRegion));
    FOR_N(i, 0, ctx->bb_count) {
        ra->hrp[i].start[0] = -1;
        ra->hrp[i].end[0]   = -1;
    }
}

void tb__ra_deinit(RABase* ra) {
    dyn_array_destroy(ra->new_vregs);
    dyn_array_destroy(ra->dead_vregs);
    cuik_free(ra->uf);
    cuik_free(ra->uf_size);
    nl_table_free(ra->coalesce_set);
}

static TB_Node* tb__ra_hard_split(Ctx* ctx, RABase* ra, TB_Node* n, TB_Node* in, RegMask* rm, int vreg_id) {
    TB_Function* f = ctx->f;

    TB_Node* move;
    if (can_remat(ctx, in)) {
        size_t extra = extra_bytes(in);
        move = tb_alloc_node(f, in->type, in->dt, in->input_count, extra);
        memcpy(move->extra, in->extra, extra);
        FOR_N(j, 0, in->input_count) if (in->inputs[j]) {
            move->inputs[j] = in->inputs[j];
            add_user(f, move, in->inputs[j], j);
        }
    } else {
        move = tb_alloc_node(f, TB_MACH_COPY, in->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, move, in, 1);
        TB_NODE_SET_EXTRA(move, TB_NodeMachCopy, .def = rm, .use = ctx->normie_mask[rm->class]);
    }
    aarray_insert(ctx->vreg_map, move->gvn, vreg_id);
    return move;
}

static void tb__ra_remat(Ctx* ctx, RABase* ra, TB_Node* n, bool kill_node) {
    size_t extra = extra_bytes(n);
    TB_Function* f = ctx->f;
    TB_Node* root = f->root_node;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    RegMask* src_mask = NULL;
    if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
        src_mask = cpy->use;
    }

    RegMask* def_mask = ctx->constraint(ctx, n, NULL);

    // aggressive reload
    double base_bias = ctx->vregs[ctx->vreg_map[n->gvn]].spill_bias;
    for (size_t i = 0; i < n->user_count;) {
        TB_Node* use_n = USERN(&n->users[i]);
        int use_i      = USERI(&n->users[i]);

        // it's never in[0] lmao
        assert(use_i != 0);

        VReg* reload_vreg;
        TB_Node* remat = NULL;
        if (use_n->type == TB_MACH_COPY && src_mask) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (!reg_mask_is_stack(cpy->def) || !reg_mask_is_stack(src_mask)) {
                // gotta be in separate coalesced groups
                int leader = uf_find(ra->uf, ra->uf_len, use_n->gvn);
                if (uf_find(ra->uf, ra->uf_len, n->gvn) != leader) {
                    remat    = use_n;
                    cpy->use = src_mask;

                    // schedule the split right before use
                    set_input(f, use_n, n->inputs[1], use_i);
                    continue;
                }
            }
        }

        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        // remat per use site
        remat = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
        memcpy(remat->extra, n->extra, extra);
        FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
            remat->inputs[j] = n->inputs[j];
            add_user(f, remat, n->inputs[j], j);
        }

        if (use_n->type == TB_PHI) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, use_n->inputs[0], use_i - 1);
            TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];
            TB_ASSERT(pred->input_count != 0 && pred->type != TB_DEAD);

            int pos = aarray_length(pred_bb->items);
            TB_Node* last = pred_bb->items[pos - 1];
            if (IS_PROJ(last)) { last = last->inputs[0]; }
            if (tb_node_is_terminator(last)) {
                pos--;

                while (pos > 0 && pred_bb->items[pos] != pred_bb->start && IS_PROJ(pred_bb->items[pos])) {
                    pos--;
                }
            }

            // place at the end of the pred BB to the phi, basically the latest point
            rogers_insert_op(ctx, pred_bb - ctx->cfg.blocks, remat, pos);

            // phis hard coalesce
            int vreg_id = ctx->vreg_map[use_n->gvn];
            reload_vreg = &ctx->vregs[vreg_id];
            aarray_insert(ctx->vreg_map, remat->gvn, vreg_id);
        } else {
            // schedule the split right before use
            tb__insert_before(ctx, ctx->f, remat, use_n);
            reload_vreg = tb__set_node_vreg(ctx, remat);
            reload_vreg->mask = def_mask;
        }

        // insert copy because the mask can't be represented
        RegMask* mask = tb__reg_mask_meet(ctx, in_mask, reload_vreg->mask);
        if (mask == &TB_REG_EMPTY) {
            reload_vreg->reg_width = tb__reg_width_from_dt(reload_vreg->mask->class, remat->dt);
            reload_vreg->spill_bias = 1e6;

            TB_Node* copy = tb_alloc_node(f, TB_MACH_COPY, remat->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, copy, remat, 1);
            TB_NODE_SET_EXTRA(copy, TB_NodeMachCopy, .def = in_mask, .use = def_mask);

            tb__insert_before(ctx, ctx->f, copy, use_n);
            mask = in_mask;
            remat = copy;

            VReg* new_vreg = tb__set_node_vreg(ctx, copy);
            new_vreg->mask = in_mask;
            reload_vreg = new_vreg;
        }

        set_input(f, use_n, remat, use_i);

        // reload_vreg->hint_vreg = ctx->vreg_map[use_n->gvn];
        reload_vreg->mask = mask;
        reload_vreg->reg_width = tb__reg_width_from_dt(mask->class, remat->dt);
        reload_vreg->spill_bias = 1e6;
        TB_ASSERT(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu:    use (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(&f->tmp_arena, sp);

    if (kill_node) {
        // delete the original def
        ctx->vregs[ctx->vreg_map[n->gvn]].uses -= 1;
        ctx->vreg_map[n->gvn] = 0;
        tb__remove_node(ctx, f, n);
        tb_kill_node(f, n);
    }
}

void insert_op_at_end(Ctx* ctx, RABase* ra, TB_BasicBlock* bb, TB_Node* n) {
    int pos = aarray_length(bb->items);
    TB_Node* last = bb->items[pos - 1];
    if (IS_PROJ(last)) { last = last->inputs[0]; }
    if (tb_node_is_terminator(last)) {
        pos--;

        while (pos > 0 && bb->items[pos] != bb->start && IS_PROJ(bb->items[pos])) {
            pos--;
        }
    }

    size_t bb_id = bb - ctx->cfg.blocks;
    rogers_insert_op(ctx, bb_id, n, pos);

    if (ra != NULL) {
        // move up if necessary
        FOR_N(class, 1, ctx->num_classes) {
            ra->hrp[bb_id].start[class] += pos <= ra->hrp[bb_id].start[class];
            ra->hrp[bb_id].end[class] += pos <= ra->hrp[bb_id].end[class];
        }
    }
}

void tb__ra_resize_uf(RABase* ra, size_t new_len) {
    ra->uf = cuik_realloc(ra->uf, new_len * sizeof(int));
    ra->uf_size = cuik_realloc(ra->uf_size, new_len * sizeof(int));
    FOR_N(i, ra->uf_len, new_len) {
        ra->uf[i] = i;
        ra->uf_size[i] = 1;
    }
    ra->uf_len = new_len;
}

void tb__ra_coalesce(RABase* ra, int x, int y, TB_Node* xn, TB_Node* yn) {
    if (x == y) {
        return;
    }

    int max = TB_MAX(x, y) + 1;
    if (max >= ra->uf_len) {
        tb__ra_resize_uf(ra, max);
    }

    // hard coalesce with direct input
    TB_ASSERT(x < ra->uf_len && y < ra->uf_len);
    ra->uf[y] = x;
    ra->uf_size[x] += ra->uf_size[y];

    TB_ASSERT(ra->uf[x] == x);
    ArenaArray(TB_Node*)* new_set = (ArenaArray(TB_Node*)*) nl_table_getp(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
    if (new_set == NULL) {
        ArenaArray(TB_Node*) set = aarray_create(&ra->ctx->f->arena, TB_Node*, 4);
        nl_table_put(&ra->coalesce_set, (void*) (uintptr_t) (x + 1), set);

        // lazy? yes
        new_set = (ArenaArray(TB_Node*)*) nl_table_getp(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
        aarray_push(*new_set, xn);
    }

    ArenaArray(TB_Node*) old_set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (y + 1));
    if (old_set == NULL) {
        aarray_push(*new_set, yn);
    } else {
        aarray_for(i, old_set) {
            aarray_push(*new_set, old_set[i]);
        }
    }
}

bool tb__ra_can_coalesce(RABase* ra, TB_Node* xn, TB_Node* yn) {
    if (ra->uf[yn->gvn] != yn->gvn || ra->uf_size[yn->gvn] != 1) {
        return false;
    }

    int x = uf_find(ra->uf, ra->uf_len, xn->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (x + 1));
    if (set == NULL) {
        return !ra->interfere(ra->ctx, ra, xn, yn);
    } else {
        aarray_for(i, set) {
            if (ra->interfere(ra->ctx, ra, set[i], yn)) {
                return false;
            }
        }

        return true;
    }
}

void tb__ra_update_mask(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id) {
    size_t cnt;
    TB_Node** arr = coalesce_set_array(&ra->base, &ctx->vregs[vreg_id].n, &cnt);

    RegMask* mask = NULL;
    FOR_N(i, 0, cnt) {
        TB_Node* y = arr[i];

        ctx->vreg_map[y->gvn] = vreg_id;
        mask = tb__reg_mask_meet(ctx, mask, ctx->constraint(ctx, y, NULL));
        FOR_USERS(u, y) {
            if (USERI(u) > 0 && USERI(u) < USERN(u)->input_count) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                mask = tb__reg_mask_meet(ctx, mask, in_mask);
            }
        }
    }
    TB_ASSERT(mask != &TB_REG_EMPTY);

    VReg* vreg = &ctx->vregs[vreg_id];
    vreg->mask = mask;
    // vreg->reg_width = tb__reg_width_from_dt(mask->class, n->dt);
}

double tb__ra_get_spill_cost(RABase* ra, VReg* vreg) {
    if (isnan(vreg->spill_cost)) {
        size_t cnt;
        TB_Node** arr = coalesce_set_array(ra, &vreg->n, &cnt);

        double c = 0.0;
        FOR_N(i, 0, cnt) {
            c += get_node_spill_cost(ra->ctx, arr[i]);
        }
        vreg->spill_cost = c + vreg->spill_bias;
    }

    // no area? this means it's used right after def
    if (vreg->area == 0) {
        return INFINITY;
    }

    return vreg->spill_cost - vreg->area*0.2;
}

#if 0
static void rogers_uncoalesce(Ctx* restrict ctx, Rogers* restrict ra, int x) {
    int leader = uf_find(ra->uf, ra->uf_len, x);

    if (x >= ra->uf_len) {
        rogers_resize_uf(ctx, ra, x + 1);
    }

    // make them all their own roots
    ra->uf[x] = x;
    ra->uf_size[x] = 1;

    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set == NULL) {
        return;
    }

    // if the leader is the node being removed from the set, pick a new leader
    int new_leader = -1;
    for (int i = 0; i < aarray_length(set); i++) {
        int y = set[i]->gvn;
        if (y == x) {
            ra->uf[y] = y;
            aarray_remove(set, i);
            i--;
        } else if (leader == x) {
            if (new_leader < 0) {
                new_leader = y;
            }

            ra->uf[y] = new_leader;
        }
    }

    if (new_leader >= 0) {
        nl_table_remove(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
        nl_table_put(&ra->coalesce_set, (void*) (uintptr_t) (new_leader + 1), set);
    }
}
#endif
