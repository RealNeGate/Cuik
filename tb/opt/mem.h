// Memory Pass, like every good pass this is a combined solver:
// * Load elimination.
//
// * Store elimination.
//
// * SSA construction from locals, if it's not possible to lower into phis we'll split the
//   locals' memory effects if it never escapes.
typedef enum {
    // we had some escape so we can't rewrite
    RENAME_NONE,

    // the value has no escapes but requires pointer
    // arithmetic, we can at least make equivalence
    // classes for them & split edges.
    RENAME_MEMORY,

    // best stuff, simple names + phis
    RENAME_VALUE,
} RenameMode;

typedef struct {
    TB_Node* addr;
    bool is_mem;
    bool is_alive;
} Rename;

typedef struct {
    TB_Node* n;
    int slot;
    int reason;
} MemOp;

typedef struct {
    TB_Node* mem;
    int64_t offset;
    int64_t size;
} SimpleMemRef;

// tracks a set of stores with a shared base which aren't aliasing
typedef struct {
    TB_Node* base;
    uint32_t cnt, cap;
    SimpleMemRef stores[];
} MemorySet;

typedef struct {
    int local_count;
    Rename* renames;
    NL_Table phi2local;

    // (shared base) Node -> MemorySet*
    NL_Table non_aliasing;

    bool progress;
} LocalSplitter;

// used by phi2local whenever a memory phi is marked as split (it's not bound to any
// new name but shouldn't count as NULL)
static Rename RENAME_DUMMY = { 0 };

static int bits_in_data_type(int pointer_size, TB_DataType dt);

static bool good_mem_op(TB_Function* f, TB_Node* n) { // ld, st, memcpy, memset
    if (n->type == TB_LOAD) {
        return true;
    } else if (n->type == TB_STORE) { // && n->type <= TB_MEMSET) {
        TB_Node* base = n;
        while (base->type == TB_PTR_OFFSET) {
            base = base->inputs[1];
        }

        return base->type != TB_LOCAL || !TB_NODE_GET_EXTRA_T(base, TB_NodeLocal)->has_split;
    } else {
        return false;
    }
}

static TB_Node* next_mem_user(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_mproj(USERN(u)) || tb_node_has_mem_out(USERN(u))) {
            return USERN(u);
        }
    }

    return NULL;
}

static int find_local_idx(LocalSplitter* restrict ctx, TB_Node* n) {
    if (n->type == TB_PTR_OFFSET) {
        n = n->inputs[1];
    }

    FOR_N(i, 0, ctx->local_count) {
        if (ctx->renames[i].addr == n) {
            return 1 + i;
        }
    }

    return 0;
}

enum {
    MEM_FORK, MEM_JOIN, MEM_END, MEM_USE
};

static TB_Node* node_or_poison(TB_Function* f, TB_Node* n, TB_DataType dt) {
    return n ? n : make_poison(f, dt);
}

static SimpleMemRef find_simple_mem_ref(TB_Function* f, LocalSplitter* ctx, TB_Node* mem, TB_Node** out_base) {
    TB_Node* base = mem->inputs[2];
    int64_t offset = 0;
    if (base->type == TB_PTR_OFFSET) {
        TB_Node* curr = base->inputs[2];
        if (curr->type == TB_ICONST) {
            offset = TB_NODE_GET_EXTRA_T(curr, TB_NodeInt)->value;
            base   = base->inputs[1];
        }
    }

    int64_t size;
    if (mem->type == TB_LOAD) {
        size = tb_data_type_byte_size(f->super.module, mem->dt.type);
    } else {
        size = tb_data_type_byte_size(f->super.module, mem->inputs[3]->dt.type);
    }

    *out_base = base;
    return (SimpleMemRef){ mem, offset, size };
}

static int find_aliasing_store(LocalSplitter* restrict ctx, MemorySet* set, int64_t offset) {
    size_t left = 0, right = aarray_length(set->stores);
    while (left != right) {
        size_t i = (left + right) / 2;
        if (set->stores[i].offset >= offset) { right = i; }
        else { left = i + 1; }
    }
    return left;
}

static MemorySet* memory_set_find(TB_Function* f, LocalSplitter* restrict ctx, TB_Node* base) {
    MemorySet* set = nl_table_get(&ctx->non_aliasing, base);
    if (set == NULL) {
        set = cuik_malloc(sizeof(MemorySet) + 4*sizeof(SimpleMemRef));
        set->base = base;
        set->cap = 4;
        set->cnt = 0;
        nl_table_put(&ctx->non_aliasing, base, set);
    }
    return set;
}

static void memory_set_insert(TB_Function* f, LocalSplitter* restrict ctx, MemorySet* set, int i, SimpleMemRef v) {
    if (set->cnt == set->cap) {
        set = cuik_realloc(set, sizeof(MemorySet) + 2*set->cap*sizeof(SimpleMemRef));
        set->cap *= 2;
        nl_table_put(&ctx->non_aliasing, set->base, set);
    }

    memmove(&set->stores[i + 1], &set->stores[i], (set->cnt - i) * sizeof(SimpleMemRef));
    set->stores[i] = v;
    set->cnt += 1;
}

static void memory_set_clear_except(TB_Function* f, LocalSplitter* restrict ctx, MemorySet* ignore) {
    nl_table_for(e, &ctx->non_aliasing) {
        if (e->v != ignore) {
            cuik_free(e->v);
            nl_table_remove(&ctx->non_aliasing, e->k);
        }
    }
}

static void fixup_mem_node(TB_Function* f, LocalSplitter* restrict ctx, TB_Node* curr, TB_Node** latest) {
    int user_cnt = 0;
    MemOp* users = NULL;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    // walk past each memory effect and categorize it
    while (curr) {
        // printf("WALK v%u\n", curr->gvn);

        int user_cap = curr->user_count;
        sp = tb_arena_save(&f->tmp_arena);
        user_cnt = 0;
        users = tb_arena_alloc(&f->tmp_arena, user_cap * sizeof(MemOp));
        FOR_USERS(u, curr) {
            TB_Node* use_n = USERN(u);
            int use_i = USERI(u);

            // not a real memory use
            if (curr->type == TB_SPLITMEM && use_n->type == TB_MERGEMEM && use_i == 1) {
                continue;
            }

            // we can either be followed by:
            // * merge: we're done now, stitch in the latest ops
            //
            // * phi: whoever makes it here first follows it through and generates the
            //        parallel memory phis, everyone else just fills in the existing nodes.
            //
            // * normie memory op: just advances (maybe forking)
            int reason = -1;

            if (0) {}
            else if (is_mem_end_op(use_n))         { reason = MEM_END;  }
            else if (use_n->type == TB_MERGEMEM)   { reason = MEM_END;  }
            else if (use_n->type == TB_PHI)        { reason = MEM_JOIN; }
            else if (tb_node_mem_read_only(use_n)) { reason = MEM_USE;  }
            else if (cfg_is_mproj(use_n) || (use_i == 1 && tb_node_has_mem_out(use_n))) {
                reason = MEM_FORK;
            }

            if (reason >= 0) {
                users[user_cnt].n    = use_n;
                users[user_cnt].slot = use_i;
                users[user_cnt].reason = reason;
                user_cnt += 1;
            }
        }

        TB_Node* st_val = NULL;
        int cat = 0;
        if (curr->type == TB_STORE) {
            cat = find_local_idx(ctx, curr->inputs[2]);
            if (cat == 0) {
                // normal store
                TB_Node* base;
                SimpleMemRef ref = find_simple_mem_ref(f, ctx, curr, &base);

                MemorySet* set = memory_set_find(f, ctx, base);
                if (set->cnt == 0) {
                    // first time constructing this set, previous writes to different bases
                    // might alias so they should be cleared.
                    //
                    // if we know the "object" that the pointer is referring to then we can strictly
                    // talk about aliasing.
                    memory_set_clear_except(f, ctx, set);
                    memory_set_insert(f, ctx, set, 0, ref);
                } else {
                    bool clobber = false;

                    // check if we clobbered any stores
                    int idx = find_aliasing_store(ctx, set, ref.offset);
                    if (idx > 0 && set->stores[idx - 1].offset + set->stores[idx - 1].size > ref.offset) {
                        // lower address store overlaps
                        clobber = true;
                    }

                    if (idx < set->cnt && ref.offset + ref.size > set->stores[idx].offset) {
                        // higher address store overlaps
                        clobber = true;
                    }

                    // clobber all stores if there's any partial writes
                    // TODO(NeGate): allow for some partial clobbering.
                    if (idx >= 0) {
                        SimpleMemRef* st = &set->stores[idx];

                        // the entire previous write is eaten up by this write
                        if (idx < set->cnt &&
                            st->mem->user_count == 1 &&
                            ref.offset >= st->offset &&
                            st->offset + st->size <= ref.offset + ref.size
                        ) {
                            subsume_node(f, st->mem, st->mem->inputs[1]);
                        }

                        memory_set_insert(f, ctx, set, idx, ref);
                    } else {
                        memory_set_clear_except(f, ctx, set);
                        memory_set_insert(f, ctx, set, 0, ref);
                    }
                }
            }

            if (cat == 0 || ctx->renames[cat - 1].is_mem) {
                // rewrite memory edge
                if (curr->inputs[1] != latest[cat]) {
                    set_input(f, curr, latest[cat], 1);

                    // invalidate old memory type
                    mark_node_n_users(f, curr);
                    latuni_set(f, curr, NULL);
                }
                latest[cat] = curr;
            } else {
                // get rid of the store, we don't need it
                latest[cat] = curr->inputs[3];
                tb_kill_node(f, curr);
            }
        } else if (curr->type != TB_PROJ && curr->type != TB_PHI && tb_node_has_mem_out(curr)) {
            // unknown memory access, clobber everything
            memory_set_clear_except(f, ctx, NULL);

            if (curr != latest[0] && curr->inputs[1] != latest[0]) {
                set_input(f, curr, latest[0], 1);
                mark_node_n_users(f, curr);
            }
            latest[0] = curr;
        }

        if (curr->dt.type == TB_TAG_TUPLE) {
            // skip to mproj
            TB_ASSERT(curr->type != TB_SPLITMEM);
            curr = next_mem_user(curr);
            latest[0] = curr;
        }

        #if 0
        printf("[");
        nl_table_for(e, &ctx->non_aliasing) {
            MemorySet* set = e->v;
            FOR_N(i, 0, set->cnt) {
                printf(" %%%u+%"PRId64"=%%%u", set->base->gvn, set->stores[i].offset, set->stores[i].mem->gvn);
            }
        }
        printf(" ]\n");
        #endif

        // fixup any connected loads
        int mem_use_count = 0;
        FOR_N(i, 0, user_cnt) {
            TB_Node* use_n = users[i].n;
            int use_i = users[i].slot;
            int reason = users[i].reason;

            if (reason == MEM_USE) {
                TB_Node* val = NULL;
                int cat = find_local_idx(ctx, use_n->inputs[2]);
                if (cat == 0) {
                    // normal load, try to elim
                    TB_Node* base;
                    SimpleMemRef ref = find_simple_mem_ref(f, ctx, use_n, &base);

                    MemorySet* set = memory_set_find(f, ctx, base);
                    int idx = find_aliasing_store(ctx, set, ref.offset);
                    if (idx < set->cnt && set->stores[idx].offset == ref.offset && set->stores[idx].size == ref.size) {
                        TB_ASSERT(set->stores[idx].mem->type == TB_STORE);
                        val = set->stores[idx].mem->inputs[3];
                    }
                } else if (!ctx->renames[cat - 1].is_mem) {
                    TB_ASSERT(use_n->type == TB_LOAD);
                    val = node_or_poison(f, latest[cat], use_n->dt);
                }

                if (val != NULL) {
                    if (use_n->dt.raw != val->dt.raw) {
                        // insert bitcast
                        TB_Node* cast = tb_alloc_node(f, TB_BITCAST, use_n->dt, 2, 0);
                        set_input(f, cast, val, 1);
                        val = cast;
                    }

                    subsume_node(f, use_n, val);
                    mark_node_n_users(f, val);
                    ctx->progress = true;
                } else {
                    if (cat > 0) {
                        ctx->renames[cat - 1].is_alive = true;
                    }

                    // rewrite memory edge
                    if (use_n->inputs[1] != latest[cat]) {
                        set_input(f, use_n, latest[cat], 1);
                        mark_node_n_users(f, use_n);
                    }
                }
                mem_use_count += 1;
            }
        }

        // "tail" such that we don't make another stack frame and more
        // importantly another "latest" array
        if (user_cnt == 1+mem_use_count) {
            int fork_i = -1;
            FOR_N(i, 0, user_cnt) {
                if (users[i].reason == MEM_FORK) { fork_i = i; break; }
            }

            if (fork_i < 0) {
                break;
            }

            curr = users[fork_i].n;
            tb_arena_restore(&f->tmp_arena, sp);
        } else {
            break;
        }
    }

    FOR_N(i, 0, user_cnt) {
        TB_Node* use_n = users[i].n;
        int use_i = users[i].slot;
        int reason = users[i].reason;

        switch (reason) {
            case MEM_FORK: {
                if (curr->type == TB_SPLITMEM) {
                    fixup_mem_node(f, ctx, use_n, latest);
                } else {
                    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
                    TB_Node** new_latest = tb_arena_alloc(&f->tmp_arena, (1 + ctx->local_count) * sizeof(TB_Node*));
                    FOR_N(i, 0, 1 + ctx->local_count) {
                        new_latest[i] = latest[i];
                    }

                    fixup_mem_node(f, ctx, use_n, new_latest);
                    tb_arena_restore(&f->tmp_arena, sp);
                }
                break;
            }

            case MEM_JOIN: {
                // stitch latest state to phis
                TB_Node* region = use_n->inputs[0];

                Rename* v = nl_table_get(&ctx->phi2local, region);
                if (v == NULL) {
                    nl_table_put(&ctx->phi2local, region, &RENAME_DUMMY);
                    nl_table_put(&ctx->phi2local, use_n,  &RENAME_DUMMY);

                    // convert single phi into parallel phis (use_n will become the leftovers mem)
                    TB_Node** new_latest = tb_arena_alloc(&f->tmp_arena, (1 + ctx->local_count) * sizeof(TB_Node*));

                    set_input(f, use_n, latest[0], use_i);
                    latuni_set(f, use_n, NULL);

                    TB_ASSERT(use_n->dt.type == TB_TAG_MEMORY);
                    new_latest[0] = use_n;

                    // make extra alias phis
                    FOR_N(i, 0, ctx->local_count) {
                        // let's hope the first datatype we get from the phi is decent, if not the
                        // peepholes will ideally fix it.
                        TB_Node* val = latest[1 + i];
                        TB_DataType dt = TB_TYPE_MEMORY;

                        if (val != NULL) {
                            if (ctx->renames[i].is_mem) {
                                dt = val->dt;
                            }

                            TB_Node* new_phi = tb_alloc_node(f, TB_PHI, dt, use_n->input_count, 0);
                            set_input(f, new_phi, region, 0);
                            set_input(f, new_phi, val, use_i);
                            new_latest[1 + i] = new_phi;
                            mark_node(f, new_phi);

                            nl_table_put(&ctx->phi2local, new_phi, &ctx->renames[i]);
                            latuni_set(f, new_phi, NULL);
                        } else {
                            new_latest[1 + i] = NULL;
                        }
                    }

                    // first entry, every other time we'll just be stitching phis
                    mark_node(f, use_n);
                    fixup_mem_node(f, ctx, use_n, new_latest);
                } else if (v == &RENAME_DUMMY) {
                    for (size_t i = 0; i < region->user_count; i++) {
                        TB_Node* un = USERN(&region->users[i]);
                        int ui      = USERI(&region->users[i]);
                        if (un->type == TB_PHI) {
                            Rename* name = nl_table_get(&ctx->phi2local, un);
                            if (name == &RENAME_DUMMY) {
                                assert(latest[0]);
                                set_input(f, un, latest[0], use_i);
                            } else if (name) {
                                if (name->is_mem) {
                                    TB_Node* mem = latest[1 + (name - ctx->renames)];
                                    assert(mem);
                                    set_input(f, un, mem, use_i);
                                } else {
                                    TB_Node* val = latest[1 + (name - ctx->renames)];
                                    if (val == NULL) {
                                        // let's just insert poison, maybe it'll be
                                        // completely unreferenced later (ideally)
                                        val = make_poison(f, un->dt);
                                    } else if (val->dt.raw != un->dt.raw) {
                                        // insert bitcast
                                        TB_Node* cast = tb_alloc_node(f, TB_BITCAST, un->dt, 2, 0);
                                        set_input(f, cast, val, 1);
                                        val = cast;
                                    }

                                    set_input(f, un, val, use_i);
                                }
                            }
                        }
                    }
                }
                break;
            }

            case MEM_END: {
                // stitch the latest nodes to the merge or return
                if (use_n->type == TB_MERGEMEM) {
                    if (nl_table_get(&ctx->phi2local, use_n) == &RENAME_DUMMY) {
                        size_t j = 1;
                        set_input(f, use_n, latest[0], 2);
                        FOR_N(i, 0, ctx->local_count) if (ctx->renames[i].is_mem) {
                            assert(latest[1 + i] != NULL && "TODO we should place a poison?");
                            set_input(f, use_n, latest[1 + i], 2+j);
                            j += 1;
                        }
                        break;
                    }
                } else {
                    TB_ASSERT(use_i == 1);
                }

                if (use_n->inputs[use_i] != latest[0]) {
                    set_input(f, use_n, latest[0], use_i);
                }
                break;
            }
        }
    }

    tb_arena_restore(&f->tmp_arena, sp);
}

int tb_opt_locals(TB_Function* f) {
    cuikperf_region_start("locals", NULL);
    assert(dyn_array_length(f->worklist->items) == 0);

    /*CUIK_TIMED_BLOCK("sroa") {
        TB_Worklist* ws = f->worklist;
        int pointer_size = f->super.module->codegen->pointer_size;
        TB_Node* root = f->root_node;

        // write initial locals
        FOR_USERS(u, root) {
            if (USERN(u)->type == TB_LOCAL) { worklist_push(ws, USERN(u)); }
        }

        // i think the SROA'd pieces can't themselves split more? that should something we check
        size_t local_count = dyn_array_length(ws->items);
        for (size_t i = 0; i < local_count; i++) {
            TB_ASSERT(ws->items[i]->type == TB_LOCAL);
            sroa_rewrite(f, root, ws->items[i]);
        }
    }*/

    // find all locals
    LocalSplitter ctx = { 0 };
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
    ArenaArray(TB_Node*) locals = aarray_create(&f->tmp_arena, TB_Node*, 32);

    FOR_USERS(u, f->root_node) {
        if (USERN(u)->type == TB_LOCAL && !TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeLocal)->has_split) {
            aarray_push(locals, USERN(u));
            ctx.local_count++;
        }
    }

    // find reasons for renaming
    ctx.renames = tb_arena_alloc(&f->tmp_arena, ctx.local_count * sizeof(Rename));
    int splits_needed = 1;

    size_t j = 0;
    bool needs_to_rewrite = false;
    aarray_for(i, locals) {
        TB_Node* addr = locals[i];
        RenameMode mode = RENAME_VALUE;

        FOR_USERS(mem, addr) {
            if (USERI(mem) == 1 && USERN(mem)->type == TB_PTR_OFFSET) {
                // pointer arith are also fair game, since they'd stay in bounds (given no UB)
                mode = RENAME_MEMORY;
            } else if (USERI(mem) != 2 || !good_mem_op(f, USERN(mem))) {
                mode = RENAME_NONE;
                break;
            }
        }

        done:
        if (mode != RENAME_NONE) {
            // allocate new alias index
            if (mode == RENAME_MEMORY) {
                TB_NodeLocal* local = TB_NODE_GET_EXTRA(addr);
                local->has_split = true;
                splits_needed += 1;
                needs_to_rewrite = true;
            } else if (mode == RENAME_VALUE) {
                needs_to_rewrite = true;
            }

            ctx.renames[j].is_mem = mode == RENAME_MEMORY;
            ctx.renames[j].is_alive = false;
            ctx.renames[j].addr = addr;
            j += 1;
        }
    }

    /*if (!needs_to_rewrite) {
        tb_arena_restore(&f->tmp_arena, sp);
        cuikperf_region_end();
        return 0;
    }*/

    ctx.local_count = j;
    ctx.phi2local = nl_table_alloc(20);
    ctx.non_aliasing = nl_table_alloc(20);

    // let's rewrite values & memory
    TB_Node* first_mem = next_mem_user(f->params[1]);
    if (first_mem) {
        TB_Node** latest = tb_arena_alloc(&f->tmp_arena, (1 + ctx.local_count) * sizeof(TB_Node*));
        FOR_N(i, 1, 1 + ctx.local_count) { latest[i] = NULL; }

        // if there's already a capable split just use that
        if (first_mem->type == TB_SPLITMEM && f->params[1]->user_count == 1) {
            latest[0] = first_mem;
            FOR_N(i, 0, ctx.local_count) if (ctx.renames[i].is_mem) {
                latest[1 + i] = first_mem;
            }
        } else if (splits_needed > 1) {
            TB_Node* split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_MEMORY, 2, 0);

            // move initial effect to split's proj0
            subsume_node2(f, f->params[1], split);

            set_input(f, split, f->params[0], 0);
            set_input(f, split, f->params[1], 1);

            FOR_N(i, 0, ctx.local_count) if (ctx.renames[i].is_mem) {
                latest[1 + i] = split;
                mark_node(f, latest[1 + i]);
            }
            mark_node(f, f->params[1]);
            mark_node(f, split);

            // each terminator has a separate merge, we can have multiple merges
            // per split which is a bit wacky.
            FOR_N(i, 1, f->root_node->input_count) {
                TB_Node* end = f->root_node->inputs[i];
                if (end->type != TB_RETURN && end->type != TB_TRAP && end->type != TB_UNREACHABLE) {
                    continue;
                }

                // merge node
                TB_Node* merge = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 2 + splits_needed, 0);
                set_input(f, merge, f->params[0], 0);
                set_input(f, merge, split, 1);

                j = 1;
                FOR_N(i, 0, ctx.local_count) if (ctx.renames[i].is_mem) {
                    set_input(f, merge, latest[1 + i], 2 + j);
                    j += 1;
                }
                set_input(f, merge, end->inputs[1], 2);
                set_input(f, end, merge, 1);

                mark_node(f, merge);
                mark_node(f, end);

                nl_table_put(&ctx.phi2local, merge, &RENAME_DUMMY);
            }
            latest[0] = split;
        } else {
            latest[0] = f->params[1];
        }

        fixup_mem_node(f, &ctx, first_mem, latest);

        first_mem = next_mem_user(f->params[1]);
        if (first_mem->type == TB_SPLITMEM) {
            first_mem = f->params[1];
        }

        FOR_N(i, 0, ctx.local_count) if (!ctx.renames[i].is_alive) {
            if (ctx.renames[i].is_mem) {
                FOR_USERS(u, ctx.renames[i].addr) {
                    if (USERN(u)->type == TB_PTR_OFFSET) {
                        FOR_USERS(u2, USERN(u)) {
                            subsume_node(f, USERN(u2), first_mem);
                        }
                        tb_kill_node(f, USERN(u));
                    } else {
                        subsume_node(f, USERN(u), first_mem);
                    }
                }
            }
            tb_kill_node(f, ctx.renames[i].addr);
        }
    }

    nl_table_free(ctx.phi2local);
    tb_arena_restore(&f->tmp_arena, sp);
    cuikperf_region_end();

    return needs_to_rewrite;
}
