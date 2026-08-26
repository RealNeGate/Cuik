// This is a local register allocator, it'll spill basically anything
// that's used across multiple blocks for the purpose of achieving a
// nearly single-pass allocation.
typedef struct {
    int pos;
    TB_Node* n;
} FastRAInsert;

typedef struct {
    RABase base;
    size_t old_node_count;

    int order_cap;
    int* order;

    size_t spill_cap;
    TB_Node** spills;

    // These are special pinned regs like the frameptr or
    // non-volatile saves
    TB_Node** precolor[MAX_REG_CLASSES];

    int* refs[MAX_REG_CLASSES];
    TB_Node** active[MAX_REG_CLASSES];

    DynArray(FastRAInsert) inserts;
} FastRA;

enum {
    FRA_LOG_LOW  = 1,
    FRA_LOG_MED  = 2,
    FRA_LOG_HIGH = 3,

    FRA_LOG_CURR = FRA_LOG_LOW,
};

static bool fra_interfere(Ctx* restrict ctx, RABase* ra_base, TB_Node* lhs, TB_Node* rhs) {
    return true;
}

static void fra_rebuild_intr(Ctx* ctx, RABase* ra) {
}

static TB_Node* fra_mach_copy(Ctx* ctx, RABase* ra, RegMask* def, RegMask* use, TB_Node* src) {
    TB_Function* f = ctx->f;
    TB_Node* cpy = tb_alloc_node(f, TB_MACH_COPY, src->dt, 2, sizeof(TB_NodeMachCopy));
    set_input(f, cpy, src, 1);
    TB_NODE_SET_EXTRA(cpy, TB_NodeMachCopy, .def = def, .use = use);
    return cpy;
}

static TB_Node* fra_allocate_reg(Ctx* restrict ctx, FastRA* ra, TB_Node* n, int vreg_id, RegMask* use_mask, TB_BasicBlock* bb, int pos) {
    if (FRA_LOG_CURR >= FRA_LOG_MED) {
        printf("    ");
        tb_print_dumb_node(NULL, n);
        printf("\n");
    }

    VReg* v = &ctx->vregs[vreg_id];
    int class = v->mask->class;
    if (v->assigned >= 0) {
        return n;
    } else if (class == REG_CLASS_STK) {
        if (v->assigned < 0) {
            if (FRA_LOG_CURR >= FRA_LOG_MED) {
                printf("    SPILL! ");
                tb_print_dumb_node(NULL, n);
                printf("\n");
            }

            v->class = 0;
            v->assigned = ra->base.num_spills;
            ra->base.num_spills += v->reg_width;
        }
        return n;
    }

    size_t cnt = 0;
    TB_Node** arr = coalesce_set_array(&ra->base, &n, &cnt);

    TB_BasicBlock* leader = NULL;
    if (n->gvn >= ra->old_node_count) {
        leader = bb;
    } else {
        FOR_N(j, 0, cnt) {
            TB_BasicBlock* bb = ctx->f->scheduled[arr[j]->gvn];
            if (leader == NULL) { leader = bb; }
            else if (leader != bb) { leader = NULL; break; }

            FOR_USERS(u, arr[j]) {
                TB_BasicBlock* bb = ctx->f->scheduled[USERN(u)->gvn];
                if (leader == NULL) { leader = bb; }
                else if (leader != bb) { leader = NULL; goto done; }
            }
        }
    }

    done:;
    TB_Node* spill = vreg_id < ra->spill_cap ? ra->spills[vreg_id] : NULL;
    RegMask* mask = v->mask;
    if (leader == NULL) {
        // we need to spill, although at this point in time we only mark
        // as spilled and handle the reload case.
        if (mask->class == 0 || mask->may_spill) {
            // we can still point to the spill slot
            return n;
        }

        // we need to insert a reload
        RegMask* spill_mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);

        TB_Node* spill  = fra_mach_copy(ctx, &ra->base, spill_mask, mask,     n);
        TB_Node* reload = fra_mach_copy(ctx, &ra->base, use_mask, spill_mask, spill);

        if (FRA_LOG_CURR >= FRA_LOG_MED) {
            printf("    RELOAD! ");
            tb_print_dumb_node(NULL, reload);
            printf("\n");

            printf("    SPILL!  ");
            tb_print_dumb_node(NULL, spill);
            printf("\n");
        }

        VReg* spill_vreg = tb__set_node_vreg(ctx, spill);
        size_t spill_vreg_id  = spill_vreg - ctx->vregs;
        spill_vreg->mask      = spill_mask;
        spill_vreg->class     = REG_CLASS_STK;
        spill_vreg->assigned  = ra->base.num_spills;
        spill_vreg->reg_width = tb__reg_width_from_dt(mask->class, n->dt);
        ra->base.num_spills += spill_vreg->reg_width;

        VReg* reload_vreg = tb__set_node_vreg(ctx, reload);
        size_t reload_vreg_id = reload_vreg - ctx->vregs;
        reload_vreg->mask = use_mask;
        reload_vreg->reg_width = 1;

        TB_ASSERT(vreg_id < ra->spill_cap);
        ra->spills[vreg_id] = spill;

        FastRAInsert ins = { pos, reload };
        dyn_array_put(ra->inserts, ins);
        return fra_allocate_reg(ctx, ra, reload, reload_vreg_id, use_mask, bb, pos);
    }
    TB_ASSERT(cnt > 0);

    size_t nr = ctx->num_regs[mask->class];
    uint64_t* ra_mask = ra->base.mask;
    {
        FOR_N(j, 0, mask->count) { ra_mask[j] = ~mask->mask[j]; }
        FOR_N(j, mask->count, (nr + 63) / 64) { ra_mask[j] = UINT64_MAX; }
        if (nr % 64) {
            ra_mask[nr / 64] &= UINT64_MAX >> (64ull - (nr % 64));
        }
    }
    FOR_N(i, 0, ctx->num_regs[class]) {
        if (ra->active[class][i] != NULL) {
            ra_mask[i / 64] |= 1ull << (i % 64);
        }
    }

    if (!reg_assign(ctx, v, ra_mask, ctx->num_regs[mask->class])) {
        // track next use
        int best_vreg_id = 0;
        int best_dist = 0;
        TB_Node* best_n = NULL;
        FOR_N(i, 0, ctx->num_regs[class]) {
            if (ra->active[class][i] == NULL) { continue; }
            if (((mask->mask[i / 64] >> (i % 64)) & 1) == 0) { continue; }

            // Scan up for the same VReg, we might benefit from just
            // tracking ordinals.
            TB_Node* k = ra->active[class][i];
            int other_vreg_id = ctx->vreg_map[k->gvn];
            size_t cnt2;
            TB_Node** arr2 = coalesce_set_array(&ra->base, &ctx->vregs[other_vreg_id].n, &cnt2);

            int prev_use = pos;
            TB_ASSERT(ra->old_node_count <= ra->order_cap);
            FOR_N(j, 0, cnt2) {
                FOR_USERS(u, arr2[j]) {
                    if (ctx->f->scheduled[USERN(u)->gvn] != bb || USERN(u)->gvn >= ra->old_node_count) {
                        continue;
                    }
                    int use_pos = ra->order[USERN(u)->gvn] - 1;
                    prev_use = TB_MIN(prev_use, use_pos);
                }

                if (arr2[j]->gvn < ra->old_node_count) {
                    int def_pos = ra->order[arr2[j]->gvn] - 1;
                    prev_use = TB_MIN(prev_use, def_pos);
                }
            }

            if (prev_use > best_dist) {
                best_n = k;
                best_dist = prev_use;
                best_vreg_id = other_vreg_id;
            }
        }
        TB_ASSERT(best_vreg_id != 0);

        if (FRA_LOG_CURR >= FRA_LOG_MED) {
            printf("      EVICT ");
            print_reg_name(ctx->vregs[best_vreg_id].class, ctx->vregs[best_vreg_id].assigned);
            printf(" (V%d)\n", best_vreg_id);
        }

        RegMask* normie_mask = ctx->normie_mask[class];
        RegMask* spill_mask  = intern_regmask(ctx, REG_CLASS_STK, true, 0);

        // insert a reload *after* pos for the evicted reg, remap
        // all uses below this point to a new vreg
        {
            bool remat = can_remat(ctx, best_n);
            // it's already allocated we don't really care to change it
            RegMask* old_mask = ctx->vregs[best_vreg_id].mask;
            // we will compute a fresh "new mask" because there's now less restrictions on it
            RegMask* new_mask = NULL;

            TB_Node* reload;
            TB_Node* spill = NULL;
            if (remat) {
                // Clone node
                size_t extra = extra_bytes(best_n);
                reload = tb_alloc_node(ctx->f, best_n->type, best_n->dt, best_n->input_count, extra);
                memcpy(reload->extra, best_n->extra, extra);
                FOR_N(j, 0, best_n->input_count) if (best_n->inputs[j]) {
                    reload->inputs[j] = best_n->inputs[j];
                    add_user(ctx->f, reload, best_n->inputs[j], j);
                }
            } else {
                // cannot reload a newly created node
                TB_ASSERT(best_vreg_id < ra->spill_cap);
                spill  = fra_mach_copy(ctx, &ra->base, spill_mask, old_mask, best_n);
                reload = fra_mach_copy(ctx, &ra->base, old_mask, spill_mask, spill);
            }

            if (FRA_LOG_CURR >= FRA_LOG_MED) {
                printf("    RELOAD! ");
                tb_print_dumb_node(NULL, reload);
                printf("\n");
            }

            if (FRA_LOG_CURR >= FRA_LOG_MED && spill) {
                printf("    SPILL!  ");
                tb_print_dumb_node(NULL, spill);
                printf("\n");
            }

            // define new coalescing set
            ArenaArray(TB_Node*) old_set = aarray_create(&ctx->f->arena, TB_Node*, 4);
            ArenaArray(TB_Node*) new_set = aarray_create(&ctx->f->arena, TB_Node*, 4);
            aarray_push(new_set, reload);

            if (ctx->f->node_count >= ra->base.uf_len) {
                tb__ra_resize_uf(&ra->base, ctx->f->node_count + 1);
            }
            if (spill != NULL) {
                // spill is alone
                ra->base.uf[spill->gvn] = spill->gvn;
                ra->base.uf_size[spill->gvn] = 1;
            }
            // reload
            ra->base.uf[reload->gvn] = reload->gvn;
            ra->base.uf_size[reload->gvn] = 1;

            if (spill != NULL) {
                VReg* spill_vreg = tb__set_node_vreg(ctx, spill);
                spill_vreg->mask      = spill_mask;
                spill_vreg->class     = REG_CLASS_STK;
                spill_vreg->assigned  = ra->base.num_spills;
                spill_vreg->reg_width = tb__reg_width_from_dt(mask->class, n->dt);
                ra->base.num_spills += spill_vreg->reg_width;
            }

            VReg* reload_vreg = tb__set_node_vreg(ctx, reload);
            size_t reload_vreg_id = reload_vreg - ctx->vregs;

            // reload due to pointer invalidation
            v = &ctx->vregs[vreg_id];
            reload_vreg = &ctx->vregs[reload_vreg_id];
            VReg* evict_vreg = &ctx->vregs[best_vreg_id];
            // inherit assignment
            reload_vreg->mask     = old_mask;
            reload_vreg->class    = evict_vreg->class;
            reload_vreg->assigned = evict_vreg->assigned;
            // give away evicted assignment too
            v->class    = evict_vreg->class;
            v->assigned = evict_vreg->assigned;
            // reset core assignment
            ra->active[evict_vreg->class][evict_vreg->assigned] = 0;
            evict_vreg->class = 0;
            evict_vreg->assigned = -1;

            // leader of the pre-reload set
            TB_Node* new_leader = NULL;
            int old_leader = uf_find(ra->base.uf, ra->base.uf_len, n->gvn);

            // point all uses below the reload to the reload
            size_t cnt2;
            TB_Node** arr2 = coalesce_set_array(&ra->base, &ctx->vregs[best_vreg_id].n, &cnt2);
            FOR_N(i, 0, cnt2) {
                TB_Node* k = arr2[i];
                for (size_t j = 0; j < k->user_count;) {
                    TB_Node* un = USERN(&k->users[j]);
                    int ui      = USERI(&k->users[j]);

                    int t = ra->order[un->gvn] - 1;
                    if (NODE_ISA(un, PROJ) || ctx->f->scheduled[un->gvn] != bb || t <= pos) {
                        j += 1;
                        continue;
                    }
                    set_input(ctx->f, un, reload, ui);

                    if (FRA_LOG_CURR >= FRA_LOG_HIGH) {
                        printf("    UPDATE! ");
                        tb_print_dumb_node(NULL, un);
                        printf("\n");
                    }
                }

                // k is defined below the reload, add to new coalesce set
                int t = ra->order[k->gvn] - 1;
                if (t > pos) {
                    ctx->vreg_map[k->gvn] = reload_vreg_id;
                    aarray_push(new_set, reload);
                } else if (new_leader == NULL) {
                    new_leader = k;

                    // reset
                    ra->base.uf[k->gvn] = k->gvn;
                    ctx->vreg_map[k->gvn] = best_vreg_id;
                    aarray_push(old_set, reload);
                } else {
                    ra->base.uf[k->gvn] = new_leader->gvn;
                    ctx->vreg_map[k->gvn] = best_vreg_id;
                }
            }
            TB_ASSERT(aarray_length(new_set) > 0 && aarray_length(old_set) > 0);
            ra->base.uf_size[reload->gvn] = aarray_length(new_set);
            ra->base.uf_size[new_leader->gvn] = aarray_length(old_set);

            nl_table_remove(&ra->base.coalesce_set, (void*) (uintptr_t) (old_leader + 1));
            nl_table_put(&ra->base.coalesce_set, (void*) (uintptr_t) (reload->gvn + 1), new_set);
            nl_table_put(&ra->base.coalesce_set, (void*) (uintptr_t) (new_leader->gvn + 1), old_set);

            if (spill != NULL) {
                TB_ASSERT(best_vreg_id < ra->spill_cap);
                ra->spills[best_vreg_id] = spill;
            }

            FastRAInsert ins = { pos + 1, reload };
            dyn_array_put(ra->inserts, ins);
        }
    }

    if (FRA_LOG_CURR >= FRA_LOG_MED) {
        printf("      ASSIGN TO ");
        print_reg_name(v->class, v->assigned);
        printf(" (V%d, %%%u)\n", vreg_id, n->gvn);
    }

    TB_ASSERT(v->assigned >= 0);
    TB_ASSERT(ra->active[v->class][v->assigned] == 0);
    ra->refs[v->class][v->assigned]   = cnt;
    ra->active[v->class][v->assigned] = n;
    return n;
}

void tb__ra_fast(Ctx* restrict ctx, TB_Arena* arena) {
    FastRA ra = { { .ctx = ctx, .arena = arena, .rebuild_intr = fra_rebuild_intr, .interfere = fra_interfere } };
    tb__ra_init(&ra.base, arena);
    // ra.splits = dyn_array_create(SplitDecision, 32);

    TB_Function* f = ctx->f;
    ra.old_node_count = f->node_count;
    int starting_spills = ra.base.num_spills;

    size_t new_cap = tb_next_pow2(ctx->f->node_count + 16);
    ra.order = tb_arena_alloc(arena, new_cap * sizeof(int));
    ra.order_cap = new_cap;

    ra.active[0] = NULL;
    FOR_N(i, 1, ctx->num_classes) {
        int nr = (ctx->num_regs[i] + 15) & -16;

        ra.precolor[i] = tb_arena_alloc(arena, nr * sizeof(TB_Node*));
        memset(ra.precolor[i], 0, nr * sizeof(TB_Node*));

        ra.active[i] = tb_arena_alloc(arena, nr * sizeof(TB_Node*));
        memset(ra.active[i], 0, nr * sizeof(TB_Node*));

        ra.refs[i] = tb_arena_alloc(arena, nr * sizeof(int));
        memset(ra.refs[i], 0, nr * sizeof(int));
    }

    ra.base.mask_cap = ra.base.max_regs_in_class;
    ra.base.mask = tb_arena_alloc(arena, ((ra.base.mask_cap+63)/64) * sizeof(uint64_t));

    ra.spill_cap = aarray_length(ctx->vregs);
    ra.spills = tb_arena_alloc(arena, ra.spill_cap * sizeof(TB_Node*));
    FOR_N(i, 0, ra.spill_cap) {
        ra.spills[i] = NULL;
    }

    cuikperf_region_start("ordinals", NULL);
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        // We technically only need them locally since they're used
        // for "next use" heuristics and vregs are only ever allocated
        // locally.
        aarray_for(j, bb->items) {
            TB_Node* n = bb->items[j];
            ra.order[n->gvn] = j + 1;

            printf("%-5zu  ", j);
            tb_print_dumb_node(NULL, n);
            printf("\n");
        }
        printf("\n");
    }
    cuikperf_region_end();

    // Pre-coloring, certain regs
    FOR_USERS(u, ctx->f->root_node) {
        TB_Node* un = USERN(u);

        bool pin = false;
        if (un->type == TB_MACH_FRAME_PTR) {
            // 1-def on entry, N-use anywhere, we just reserve this reg
            pin = true;
        } else if (un->type == TB_MACH_PROJ) {
            // 1-def on entry, 1-use on ret
            TB_Node* ret = ctx->f->root_node->inputs[1];
            if (ret->type == TB_RETURN &&
                un->user_count == 1    &&
                USERN(&un->users[0]) == ret &&
                USERI(&un->users[0]) > 3) {
                pin = true;
            }
        }

        if (pin && ctx->vreg_map[un->gvn]) {
            RegMask* def_mask = ctx->constraint(ctx, un, NULL);
            int fixed = fixed_reg_mask(def_mask);
            TB_ASSERT(fixed >= 0);

            ra.precolor[def_mask->class][fixed] = un;

            VReg* v = &ctx->vregs[ctx->vreg_map[un->gvn]];
            v->class    = def_mask->class;
            v->assigned = fixed;

            if (FRA_LOG_CURR >= FRA_LOG_LOW) {
                printf("      PRECOLOR ");
                print_reg_name(v->class, v->assigned);
                printf(" (V%d, %%%u)\n", ctx->vreg_map[un->gvn], un->gvn);
            }
        }
    }

    cuikperf_region_start("alloc regs", NULL);
    FOR_REV_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        dyn_array_clear(ra.inserts);

        // reset active, this is all local RA
        FOR_N(class, 1, ctx->num_classes) {
            FOR_N(reg, 0, ctx->num_regs[class]) {
                ra.active[class][reg] = 0;
            }
        }

        int last_phi = 0;
        while (last_phi < aarray_length(bb->items) && (bb->items[last_phi]->type == TB_PHI || NODE_ISA(bb->items[last_phi], PROJ))) {
            last_phi++;
        }

        printf("BB%zu (freq=%f, %%%u):\n", i, bb->freq, bb->start->gvn);
        FOR_REV_N(j, last_phi, aarray_length(bb->items)) {
            TB_Node* n = bb->items[j];

            if (FRA_LOG_CURR >= FRA_LOG_LOW) {
                printf("  ");
                FOR_N(k, 0, ctx->num_regs[1]) {
                    if (ra.active[1][k]) {
                        printf("R%-2zu ", k);
                    } else {
                        printf("    ");
                    }
                }
                printf("  ");
                tb_print_dumb_node(NULL, n);
                printf("\n");
            }

            // expire intervals
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id != 0) {
                VReg* vreg = &ctx->vregs[vreg_id];
                if (ra.spills[vreg_id] != NULL) {
                    // insert spill here
                    FastRAInsert ins = { j, ra.spills[vreg_id] };
                    dyn_array_put(ra.inserts, ins);
                    ra.spills[vreg_id] = NULL;
                }

                if (vreg->assigned >= 0 && vreg->class > 0) {
                    TB_ASSERT(ra.active[vreg->class][vreg->assigned] == n);
                    int refs = --ra.refs[vreg->class][vreg->assigned];
                    if (refs == 0) {
                        ra.active[vreg->class][vreg->assigned] = NULL;

                        if (FRA_LOG_CURR >= FRA_LOG_MED) {
                            printf("    EXPIRE ");
                            print_reg_name(vreg->class, vreg->assigned);
                            printf("\n");
                        }
                    } else {
                        // walk to the next node in the coalescing
                        // set, mostly by checking the 2addr
                        int k = ctx->node_2addr(n);
                        TB_ASSERT(k >= 0);
                        TB_ASSERT(ctx->vreg_map[n->inputs[k]->gvn] == vreg_id);

                        ra.active[vreg->class][vreg->assigned] = n->inputs[k];

                        if (FRA_LOG_CURR >= FRA_LOG_MED) {
                            printf("    ADVANCE ");
                            print_reg_name(vreg->class, vreg->assigned);
                            printf(" (%%%u -> %%%u)\n", n->gvn, n->inputs[k]->gvn);
                        }
                    }
                }
            }

            // start intervals
            if (n->type != TB_PHI) {
                RegMask* def_mask = ctx->constraint(ctx, n, ctx->ins);
                FOR_N(k, 1, n->input_count) {
                    TB_Node* in = n->inputs[k];
                    if (in == NULL || in->gvn >= ra.old_node_count || ctx->vreg_map[in->gvn] == 0) {
                        continue;
                    }

                    TB_Node* new_in = fra_allocate_reg(ctx, &ra, in, ctx->vreg_map[in->gvn], ctx->ins[k], bb, j);
                    if (in != new_in) {
                        set_input(f, n, new_in, k);

                        FOR_N(l, k + 1, n->input_count) {
                            if (n->inputs[l] == in) {
                                set_input(f, n, new_in, l);
                            }
                        }
                    }
                }
            }
        }

        // merge insert
        DynArray(FastRAInsert) inserts = ra.inserts;
        size_t cnt = aarray_length(bb->items);
        aarray_reserve(bb->items, cnt + dyn_array_length(inserts));

        dyn_array_for(j, inserts) {
            TB_Node* n = inserts[j].n;
            size_t pos = inserts[j].pos;
            while (pos < cnt && NODE_ISA(bb->items[pos], PROJ)) {
                pos++;
            }

            // skip phis and projections so that they stay nice and snug
            if (cnt > pos) {
                memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
            }
            bb->items[pos] = n;
            cnt += 1;
            tb__insert(ctx, ctx->f, bb, n);
        }

        printf("BB%zu (freq=%f, %%%u):\n", i, bb->freq, bb->start->gvn);
        aarray_for(j, bb->items) {
            printf("%-5zu  ", j);
            tb_print_dumb_node(NULL, bb->items[j]);
            if (bb->items[j]->gvn >= ra.old_node_count) {
                printf(" # NEW!!!");
            }
            printf("\n");
        }
        printf("\n");
    }

    __builtin_debugtrap();
    ctx->num_spills += ra.base.num_spills - starting_spills;
    cuikperf_region_end();

    tb__ra_deinit(&ra.base);
    cuikperf_region_end();
}
