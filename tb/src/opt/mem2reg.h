
typedef enum {
    // we had some escape so we can't rewrite
    RENAME_NONE,

    RENAME_MEMORY,
    RENAME_
} RenameMode;

// Region -> Value
typedef NL_Map(TB_Node*, TB_Node*) Mem2Reg_Def;

typedef struct Mem2Reg_Ctx {
    TB_Function* f;
    TB_Passes* p;
    TB_Node** blocks;

    // Stack slots we're going to convert into
    // SSA form
    size_t to_promote_count;
    TB_Node** to_promote;

    // [to_promote_count]
    Mem2Reg_Def* defs;
} Mem2Reg_Ctx;

static int bits_in_data_type(int pointer_size, TB_DataType dt);
static Coherency tb_get_stack_slot_coherency(TB_Passes* p, TB_Function* f, TB_Node* address, TB_DataType* dt);

static bool good_mem_op(TB_Passes* p, TB_Node* n) { // ld, st, memcpy, memset
    if (n->type == TB_LOAD) {
        return true;
    } else if (n->type >= TB_STORE && n->type <= TB_MEMSET) {
        Lattice* l = lattice_universe_get(p, n);
        return l == &TOP_IN_THE_SKY || l == p->root_mem;
    } else {
        return false;
    }
}

typedef struct {
    TB_Node* n;
    int slot;
    int reason;
} MemOp;

typedef struct {
    int alias_cnt;
    TB_Node** projs;
    TB_Node** addrs;
    TB_Node* merge;
    NL_Table phi2alias;
} LocalSplitter;

static bool same_base(TB_Node* a, TB_Node* b) {
    while (b->type == TB_MEMBER_ACCESS || b->type == TB_ARRAY_ACCESS) {
        if (a == b) return true;
        b = b->inputs[1];
    }

    return a == b;
}

static TB_Node* next_mem_user(TB_Node* n) {
    FOR_USERS(u, n) {
        if (is_mem_out_op(u->n)) {
            return u->n;
        }
    }

    return NULL;
}

static int categorize_alias_idx(LocalSplitter* restrict ctx, TB_Node* n) {
    // skip any member or array accesses
    while (n->type == TB_ARRAY_ACCESS || n->type == TB_MEMBER_ACCESS) {
        n = n->inputs[1];
    }

    FOREACH_N(i, 1, ctx->alias_cnt) {
        if (ctx->addrs[i] == n) return i;
    }

    return 0;
}

enum {
    MEM_FORK, MEM_JOIN, MEM_END, MEM_USE
};

static void fixup_mem_node(TB_Function* f, TB_Passes* restrict p, LocalSplitter* restrict ctx, TB_Node* curr, TB_Node** latest) {
    int user_cnt = 0;
    MemOp* users = NULL;
    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);

    // walk past each memory effect and categorize it
    while (curr) {
        // skip past projections
        if (curr->type != TB_PROJ && curr->type != TB_SPLITMEM && curr->type != TB_MERGEMEM) {
            int cat = -1;
            if (curr->type == TB_CALL || curr->type == TB_SYSCALL) {
                cat = 0;
            } else if (curr->type == TB_PHI) {
                uintptr_t v = (uintptr_t) nl_table_get(&ctx->phi2alias, curr);
                assert(v);
                cat = v - 1;
            } else {
                cat = categorize_alias_idx(ctx, curr->inputs[2]);
            }

            assert(cat >= 0);
            if (latest[cat] != curr) {
                set_input(f, curr, latest[cat], 1);

                if (curr->dt.type == TB_TUPLE) {
                    curr = next_mem_user(curr);
                }

                assert(curr->dt.type == TB_MEMORY);
                latest[cat] = curr;
            }
        }

        int user_cap = 0;
        FOR_USERS(u, curr) { user_cap++; }

        sp = tb_arena_save(tmp_arena);
        user_cnt = 0;
        users = tb_arena_alloc(tmp_arena, user_cap * sizeof(MemOp));
        FOR_USERS(u, curr) {
            TB_Node* use_n = u->n;
            int use_i = u->slot;

            // we can either be followed by:
            // * merge: we're done now, stitch in the latest ops
            //
            // * phi: whoever makes it here first follows it through and generates the
            //        parallel memory phis, everyone else just fills in the existing nodes.
            //
            // * normie memory op: just advances (maybe forking)
            int reason = -1;

            if (0) {}
            else if (use_n == ctx->merge)        { reason = MEM_END;  }
            else if (use_n->type == TB_PHI)      { reason = MEM_JOIN; }
            else if (is_mem_only_in_op(u->n))    { reason = MEM_USE;  }
            else if (cfg_is_mproj(use_n) || (use_i == 1 && is_mem_out_op(use_n))) {
                reason = MEM_FORK;
            }

            if (reason >= 0) {
                users[user_cnt].n    = u->n;
                users[user_cnt].slot = u->slot;
                users[user_cnt].reason = reason;
                user_cnt += 1;
            }
        }

        // fixup any connected loads
        FOREACH_N(i, 0, user_cnt) {
            TB_Node* use_n = users[i].n;
            int use_i = users[i].slot;
            int reason = users[i].reason;

            if (reason == MEM_USE) {
                int op_cat = categorize_alias_idx(ctx, use_n->inputs[2]);
                set_input(f, use_n, latest[op_cat], 1);

                if (use_n->type == TB_LOAD) {
                    tb_pass_mark(p, use_n);
                }
            }
        }

        lattice_universe_map(p, curr, &TOP_IN_THE_SKY);
        tb_pass_mark(p, curr);

        // "tail" such that we don't make another stack frame and more
        // importantly another "latest" array
        if (user_cnt == 1 && users[0].reason == MEM_FORK) {
            curr = users[0].n;
            tb_arena_restore(tmp_arena, sp);
        } else {
            break;
        }
    }

    FOREACH_N(i, 0, user_cnt) {
        TB_Node* use_n = users[i].n;
        int use_i = users[i].slot;
        int reason = users[i].reason;

        switch (reason) {
            case MEM_FORK: {
                TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);
                TB_Node** new_latest = tb_arena_alloc(tmp_arena, ctx->alias_cnt * sizeof(TB_Node*));
                FOREACH_N(i, 0, ctx->alias_cnt) {
                    new_latest[i] = latest[i];
                }

                fixup_mem_node(f, p, ctx, use_n, new_latest);
                tb_arena_restore(tmp_arena, sp);
                break;
            }

            case MEM_JOIN: {
                // stitch latest state to phis
                TB_Node* region = use_n->inputs[0];

                uintptr_t v = (uintptr_t) nl_table_get(&ctx->phi2alias, use_n);
                if (v == 0) {
                    TB_Node** new_latest = tb_arena_alloc(tmp_arena, ctx->alias_cnt * sizeof(TB_Node*));

                    // convert single phi into multiple parallel phis (first one will be replaced
                    // with the root mem)
                    set_input(f, use_n, latest[0], use_i);
                    lattice_universe_map(p, use_n, &TOP_IN_THE_SKY);

                    nl_table_put(&ctx->phi2alias, use_n, (void*) 1);

                    assert(use_n->dt.type == TB_MEMORY);
                    new_latest[0] = use_n;

                    // make extra alias phis
                    FOREACH_N(i, 1, ctx->alias_cnt) {
                        TB_Node* new_phi = tb_alloc_node(f, TB_PHI, TB_TYPE_MEMORY, use_n->input_count, 0);
                        set_input(f, new_phi, region, 0);
                        set_input(f, new_phi, latest[i], use_i);
                        new_latest[i] = new_phi;
                        tb_pass_mark(p, new_phi);

                        nl_table_put(&ctx->phi2alias, new_phi, (void*) (i+1));
                        lattice_universe_map(p, new_phi, &TOP_IN_THE_SKY);
                    }

                    if (!worklist_test_n_set(&p->worklist, use_n)) {
                        dyn_array_put(p->worklist.items, use_n);
                        fixup_mem_node(f, p, ctx, use_n, new_latest);
                    }
                } else {
                    FOR_USERS(phi, region) {
                        if (phi->n->type == TB_PHI && phi->n->dt.type == TB_MEMORY) {
                            uintptr_t alias_idx = (uintptr_t) nl_table_get(&ctx->phi2alias, phi->n);
                            assert(alias_idx != 0);
                            set_input(f, phi->n, latest[alias_idx - 1], use_i);
                        }
                    }
                }
                break;
            }

            case MEM_END: {
                // stitch the latest nodes to the merge or phis
                FOREACH_N(i, 0, ctx->alias_cnt) {
                    set_input(f, use_n, latest[i], 2+i);
                }
                break;
            }
        }
    }

    tb_arena_restore(tmp_arena, sp);
}

static bool all_stores_dead(TB_Node* n) {
    FOR_USERS(u, n) {
        if (u->slot != 2 || u->n->type != TB_STORE) {
            return false;
        }
    }

    return true;
}

static void expunge(TB_Function* f, TB_Node* n) {
    // delete users without weird iteration invalidation issues
    while (n->users) {
        TB_Node* use_n = n->users->n;
        int use_i      = n->users->slot;

        assert(use_i == 2);
        n->users = n->users->next;
        use_n->inputs[2] = NULL;

        FOR_USERS(u, use_n) {
            // kill connected phis
            if (u->n->type == TB_PHI) {
                set_input(f, u->n, NULL, 0);
            }
        }

        tb_pass_kill_node(f, use_n);
    }
    tb_pass_kill_node(f, n);
}

bool tb_pass_split_locals(TB_Passes* p) {
    TB_Function* f = p->f;

    for (;;) {
        assert(dyn_array_length(p->worklist.items) == 0);
        worklist_clear(&p->worklist);

        tb_pass_print(p);
        tb_dumb_print(f, p);

        // which locals can never alias (no ptr uses)
        FOR_USERS(u, f->root_node) {
            if (u->n->type != TB_LOCAL) continue;
            TB_Node* addr = u->n;

            // let's make sure the local never escapes
            bool good = true;
            FOR_USERS(mem, addr) {
                if (mem->slot == 1 && (mem->n->type == TB_MEMBER_ACCESS || mem->n->type == TB_ARRAY_ACCESS)) {
                    // pointer arith are also fair game, since they'd stay in bounds (given no UB)
                } else if (mem->slot != 2 || !good_mem_op(p, mem->n)) {
                    good = false;
                    break;
                }
            }

            if (good) {
                printf("WOOYEA v%u\n", addr->gvn);
                tb_pass_mark(p, addr);
            }
        }

        if (dyn_array_length(p->worklist.items) == 0) {
            break;
        }

        // not including the "ANYMEM" option
        LocalSplitter ctx = { .alias_cnt = dyn_array_length(p->worklist.items) };
        int new_aliases = p->alias_n;
        int base = 1;

        TB_Node* split = NULL;
        TB_Node* merge = f->ret_node->inputs[1];
        if (merge->type == TB_MERGEMEM &&
            merge->inputs[1]->type == TB_SPLITMEM &&
            merge->inputs[1]->inputs[0] == f->params[0]) {
            // ok let's append onto the existing split/merge (we need to resize the split now)
            split = merge->inputs[1];

            int old_alias_cnt = TB_NODE_GET_EXTRA_T(split, TB_NodeMemSplit)->alias_cnt;
            int* old_alias_idx = TB_NODE_GET_EXTRA_T(split, TB_NodeMemSplit)->alias_idx;

            base = old_alias_cnt;
            p->alias_n += ctx.alias_cnt;
            ctx.alias_cnt += old_alias_cnt;

            TB_Node* new_split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_TUPLE, 2, sizeof(TB_NodeMemSplit) + ctx.alias_cnt*sizeof(int));
            set_input(f, new_split, split->inputs[0], 0);
            set_input(f, new_split, split->inputs[1], 1);
            lattice_universe_map(p, new_split, &TOP_IN_THE_SKY);

            TB_NodeMemSplit* split_extra = TB_NODE_GET_EXTRA(new_split);
            split_extra->alias_cnt = ctx.alias_cnt;
            FOREACH_N(i, 0, old_alias_cnt) {
                split_extra->alias_idx[i] = old_alias_idx[i];
            }

            // move split
            subsume_node(f, split, new_split);
            split = new_split;

            // find old projections
            ctx.projs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
            ctx.addrs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
            FOR_USERS(u, split) {
                if (u->n->type != TB_PROJ) continue;
                int index = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
                ctx.addrs[index] = NULL;
                ctx.projs[index] = u->n;
            }
        } else {
            ctx.alias_cnt += 1;

            // new final memory node
            merge = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 2 + ctx.alias_cnt, 0);
            set_input(f, merge, f->ret_node->inputs[0], 0);
            lattice_universe_map(p, merge, &TOP_IN_THE_SKY);

            // new initial memory node
            split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_TUPLE, 2, sizeof(TB_NodeMemSplit) + ctx.alias_cnt*sizeof(int));
            set_input(f, split, f->params[0], 0);
            set_input(f, merge, split, 1);

            p->alias_n += ctx.alias_cnt-1;

            TB_NodeMemSplit* split_extra = TB_NODE_GET_EXTRA(split);
            split_extra->alias_idx[0] = 0;
            split_extra->alias_cnt = ctx.alias_cnt;

            ctx.projs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
            ctx.addrs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
            ctx.projs[0] = make_proj_node(f, TB_TYPE_MEMORY, split, 0);

            // stitch memory effects
            subsume_node2(f, f->params[1], ctx.projs[0]);
            set_input(f, split, f->params[1], 1);
            set_input(f, merge, f->ret_node->inputs[1], 2);
            set_input(f, f->ret_node, merge, 1);
        }
        ctx.merge = merge;
        ctx.addrs[0] = NULL;

        lattice_universe_map(p, split, &TOP_IN_THE_SKY);

        tb_pass_mark(p, merge);
        tb_pass_mark_users(p, ctx.projs[0]);

        // append new projections
        TB_NodeMemSplit* split_extra = TB_NODE_GET_EXTRA(split);
        FOREACH_N(i, base, ctx.alias_cnt) {
            split_extra->alias_idx[i] = new_aliases+i-1;
            ctx.projs[i] = make_proj_node(f, TB_TYPE_MEMORY, split, i);
            ctx.addrs[i] = p->worklist.items[i-base];
            tb_pass_mark(p, ctx.projs[i]);

            if (merge->input_count == 2+i) {
                add_input_late(f, merge, ctx.projs[i]);
            } else {
                set_input(f, merge, ctx.projs[i], 2+i);
            }
        }

        ctx.phi2alias = nl_table_alloc(200);

        // walk from the last memory effect until the next phi node.
        TB_Node* first_mem = next_mem_user(ctx.projs[0]);
        if (first_mem) {
            TB_Node** latest = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
            FOREACH_N(i, 0, ctx.alias_cnt) { latest[i] = ctx.projs[i]; }

            fixup_mem_node(f, p, &ctx, first_mem, latest);
        }

        nl_table_free(ctx.phi2alias);
        tb_pass_mark(p, split);

        // GVN the loads first (then we'll phi so we avoid redundants since they'll fuck us over)
        Worklist* ws = &p->worklist;
        for (size_t i = 0; i < dyn_array_length(ws->items);) {
            TB_Node* n = ws->items[i];

            if (n->type == TB_LOAD) {
                TB_Node* k = nl_hashset_put2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
                if (k && (k != n)) {
                    DO_IF(TB_OPTDEBUG_PEEP)(printf("GVN t=%d? ", ++p->stats.time), print_node_sexpr(n, 0));
                    DO_IF(TB_OPTDEBUG_STATS)(p->stats.gvn_hit++);
                    DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[95mGVN v%u\x1b[0m\n", k->gvn));

                    // remove-swap
                    dyn_array_remove(ws->items, i);

                    subsume_node(f, n, k);
                    tb_pass_mark_users(p, k);
                    continue;
                } else {
                    DO_IF(TB_OPTDEBUG_STATS)(p->stats.gvn_miss++);
                }
            }

            i++;
        }

        // fold away the loads
        tb_pass_peephole(p);

        // let's dead code some of these:
        //   the peepholes might've changed the ordering of alias indices, that's fine
        //   we can construct it based on the merge edges
        assert(merge->type == TB_MERGEMEM);
        FOREACH_N(i, 2, merge->input_count) {
            Lattice* ty = lattice_universe_get(p, merge->inputs[i]);
            if (ty->tag != LATTICE_MEM) continue;

            TB_Node* addr = NULL;
            size_t j = ty->_mem.alias_idx - new_aliases;
            if (j >= 0 && j < ctx.alias_cnt) {
                addr = ctx.addrs[base + j];
            }

            if (addr && all_stores_dead(addr)) {
                // delete users without weird iteration invalidation issues
                expunge(f, addr);
                set_input(f, merge, ctx.projs[base + j], i);
                tb_pass_mark(p, merge);
            }
        }

        tb_pass_peephole(p);

        // if it's only one split, just get rid of it
        if (merge->input_count == 3) {
            User* proj = proj_with_index(split, 0);
            assert(proj->n);

            tb_pass_mark_users(p, proj->n);
            set_input(f, proj->n, NULL, 0);
            subsume_node(f, proj->n, split->inputs[1]);
            tb_pass_kill_node(f, split);

            TB_Node* prev = merge->inputs[2];
            subsume_node(f, merge, prev);
            tb_pass_mark_users(p, prev);
        }

        tb_pass_peephole(p);
    }
    return true;
}
