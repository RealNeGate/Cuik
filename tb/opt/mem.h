// Memory Pass, like every good pass this is a combined solver:
//
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
    TB_Node* mem;
    TB_Node* index;
    int64_t offset;
    int64_t size;
} SimpleMemRef;

// tracks a set of stores with a shared base which aren't aliasing
typedef struct {
    // not speculative, we can use for load and store elim
    bool ready;

    TB_Node* base;
    uint32_t cnt, cap;
    SimpleMemRef stores[];
} MemorySet;

typedef struct MemoryState {
    int refs;

    int order;
    TB_Node* start;
    struct MemoryState* loop_head;

    bool walked_once;
    bool sealed;
    bool is_loop;
    bool is_dead;
    TB_Node** phis;

    NL_Table non_aliasing;
    TB_Node* latest[];
} MemoryState;

typedef struct {
    int local_count;
    Rename* renames;

    bool insert_merge;
    bool progress;

    TB_Worklist* dead_worklist;

    size_t postorder_len;
    MemoryState** postorder_states;
} LocalSplitter;

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
    TB_Node* index = NULL;

    int64_t offset = 0;
    if (base->type == TB_PTR_OFFSET) {
        TB_Node* curr = base->inputs[2];
        if (curr->type == TB_ICONST) {
            offset = TB_NODE_GET_EXTRA_T(curr, TB_NodeInt)->value;
            base   = base->inputs[1];
        } else if (curr->type == TB_ADD && curr->inputs[2]->type == TB_ICONST) {
            offset = TB_NODE_GET_EXTRA_T(curr->inputs[2], TB_NodeInt)->value;
            index  = curr->inputs[1];
            base   = base->inputs[1];
        } else {
            index  = base->inputs[2];
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
    return (SimpleMemRef){ mem, index, offset, size };
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

static MemorySet* memory_set_find_or_create(TB_Function* f, NL_Table* non_aliasing, TB_Node* base) {
    MemorySet* set = nl_table_get(non_aliasing, base);
    if (set == NULL) {
        set = cuik_malloc(sizeof(MemorySet) + 4*sizeof(SimpleMemRef));
        set->ready = true;
        set->base = base;
        set->cap = 4;
        set->cnt = 0;
        nl_table_put(non_aliasing, base, set);
    }
    return set;
}

static void memory_set_insert(TB_Function* f, NL_Table* non_aliasing, MemorySet* set, int i, SimpleMemRef v) {
    if (set->cnt == set->cap) {
        size_t new_cap = set->cap*2;
        if (new_cap < 4) { new_cap = 4; }

        set = cuik_realloc(set, sizeof(MemorySet) + new_cap*sizeof(SimpleMemRef));
        set->cap = new_cap;
        nl_table_put(non_aliasing, set->base, set);
    }

    TB_ASSERT((i + 1) + (set->cnt - i) <= set->cap);
    memmove(&set->stores[i + 1], &set->stores[i], (set->cnt - i) * sizeof(SimpleMemRef));
    set->stores[i] = v;
    set->cnt += 1;
}

static void memory_set_clear_except(NL_Table* non_aliasing, MemorySet* ignore) {
    nl_table_for(e, non_aliasing) {
        if (e->v != ignore) {
            nl_table_remove(non_aliasing, e->k);
        }
    }
}

static NL_Table memory_set_clone(TB_Function* f, NL_Table* non_aliasing, bool speculative) {
    NL_Table clone = nl_table_alloc(non_aliasing->count);
    nl_table_for(e, non_aliasing) {
        nl_table_put(&clone, e->k, e->v);
        if (speculative) {
            ((MemorySet*) e->v)->ready = false;
        }
    }
    return clone;
}

static void print_memory_state(NL_Table* non_aliasing) {
    nl_table_for(e, non_aliasing) {
        MemorySet* set = e->v;
        FOR_N(i, 0, set->cnt) {
            printf("%%%u", set->base->gvn);
            if (set->stores[i].index) {
                printf("+%%%u", set->stores[i].index->gvn);
            }
            printf("+%"PRId64"=%%%u ", set->stores[i].offset, set->stores[i].mem->gvn);
        }
    }
}

static void print_var_state(LocalSplitter* restrict ctx, MemoryState* state) {
    printf("[");
    FOR_N(i, 0, 1 + ctx->local_count) {
        if (i == 0) {
            printf("ROOT:");
        } else {
            printf("%%%u:", ctx->renames[i - 1].addr->gvn);
        }

        if (state->latest[i]) {
            printf("%%%u ", state->latest[i]->gvn);
        } else {
            printf("___ ");
        }
    }
    printf("]\n");
}

static void merge_memory(TB_Function* f, NL_Table* non_aliasing, NL_Table* other) {
    /*printf("  ");
    print_memory_state(non_aliasing);
    printf("\n+ ");
    print_memory_state(other);
    printf("\n==================\n");*/

    // since all the aliasing info we're working with is global, any two memory sets
    // with different bases regardless of the table, will not alias.
    nl_table_for(e, non_aliasing) {
        MemorySet* this = e->v;
        MemorySet* that = nl_table_get(other, e->k);

        if (that == NULL) {
            continue;
        }

        TB_ASSERT(this->base == that->base);
        MemorySet* set = cuik_malloc(sizeof(MemorySet) + (this->cnt + that->cnt)*sizeof(SimpleMemRef));
        set->ready = true;
        set->base = this->base;
        set->cap = this->cnt + that->cnt;
        set->cnt = 0;

        // two sorted lists, we can walk each head
        int x = 0, y = 0;
        while (x < this->cnt && y < that->cnt) {
            // either this[x] and that[x] overlap (in which case neither are placed),
            // or they're well ordered and thus placed as such.
            int l1 = this->stores[x].offset;
            int r1 = that->stores[y].offset;
            int l2 = this->stores[x].offset + this->stores[x].size;
            int r2 = that->stores[y].offset + that->stores[y].size;
            if (r1 < l2 || r2 < l1) {
                x++, y++;
                continue;
            }

            if (this->stores[x].offset < that->stores[y].offset) {
                set->stores[set->cnt++] = this->stores[x++];
                set->stores[set->cnt++] = that->stores[y++];
            } else {
                set->stores[set->cnt++] = that->stores[y++];
                set->stores[set->cnt++] = this->stores[x++];
            }
        }

        // leftover this
        while (x < this->cnt) { set->stores[set->cnt++] = this->stores[x++]; }
        // leftover that
        while (y < that->cnt) { set->stores[set->cnt++] = that->stores[y++]; }

        e->v = set;
    }

    nl_table_for(e, other) {
        MemorySet* this = e->v;
        MemorySet* that = nl_table_get(non_aliasing, e->k);
        if (that == NULL) {
            // clone it
            MemorySet* set = cuik_malloc(sizeof(MemorySet) + this->cnt * sizeof(SimpleMemRef));
            set->ready = true;
            set->base = this->base;
            set->cap = this->cnt;
            set->cnt = this->cnt;
            memcpy(set->stores, this->stores, this->cnt * sizeof(SimpleMemRef));

            nl_table_put(non_aliasing, e->k, set);
        }
    }

    /* printf("  ");
    print_memory_state(non_aliasing);
    printf("\n"); */
}

static TB_Node* next_of_memory_sese(TB_Node* curr) {
    // the next memory output, NULL if there's multiple
    TB_Node* next = NULL;
    FOR_USERS(u, curr) {
        TB_Node* use_n = USERN(u);
        int use_i = USERI(u);

        // not a real memory use
        if (curr->type == TB_SPLITMEM && use_n->type == TB_MERGEMEM && use_i == 1) {
            continue;
        }

        // memory end points
        if (use_n->type == TB_PHI || is_mem_end_op(use_n) || use_n->type == TB_MERGEMEM) {
            return NULL;
        }

        if (cfg_is_mproj(use_n) || (use_i == 1 && tb_node_has_mem_out(use_n))) {
            if (next == NULL) { next = use_n; }
            else { return NULL; }
        }
    }
    return next;
}

static TB_Node* end_of_memory_sese(TB_Node* curr) {
    TB_OPTDEBUG(MEM2REG)(printf("SESE:\n"));
    for (;;) {
        TB_OPTDEBUG(MEM2REG)(printf("  WALK %%%u\n", curr->gvn));

        // the next memory output, NULL if there's multiple
        TB_Node* next = next_of_memory_sese(curr);
        if (next == NULL) {
            return curr;
        }
        curr = next;
    }
}

static MemoryState* start_of_memory_sese(NL_Table* sese2set, TB_Node* n) {
    for (;;) {
        MemoryState* state = nl_table_get(sese2set, n);
        if (state != NULL) {
            return state;
        }

        // can't walk past phis, that means
        // we missed a "block"
        TB_ASSERT(n->type != TB_PHI);

        int mem_slot = 1;
        if (is_proj(n)) { mem_slot = 0; }
        else if (n->type == TB_MERGEMEM) { mem_slot = 2; }

        n = n->inputs[mem_slot];
    }
}

// Perform renaming until we reach a fork
static TB_Node* process_sese(TB_Function* f, NL_Table* sese2set, LocalSplitter* restrict ctx, TB_Node* curr, MemoryState* state, NL_Table* non_aliasing, bool* out_progress) {
    TB_Node** latest = state->walked_once ? NULL : state->latest;
    TB_OPTDEBUG(MEM2REG)(printf("\n\n"));

    // current aliasing set is a meet over all preds
    nl_table_clear(non_aliasing);
    if (curr->type == TB_PHI) {
        // first time we walk, we associate the latest array members with phis
        if (!state->walked_once) {
            latest[0] = curr;

            MemoryState* dom_pred = NULL;
            if (state->is_loop && curr->input_count == 3) {
                FOR_N(j, 1, curr->input_count) {
                    MemoryState* pred = start_of_memory_sese(sese2set, curr->inputs[j]);
                    if (pred->walked_once) {
                        dom_pred = pred;
                        break;
                    }
                }
            }

            TB_Node** phis = tb_arena_alloc(&f->tmp_arena, ctx->local_count * sizeof(TB_Node*));
            TB_Node* region = curr->inputs[0];
            FOR_N(i, 0, ctx->local_count) {
                // only values which are live-in can be alive "around", so
                // if we know what those are we're good.
                if (dom_pred && dom_pred->latest[1 + i] == NULL) {
                    phis[i] = latest[1 + i] = NULL;
                    continue;
                }

                TB_Node* new_phi = tb_alloc_node(f, TB_PHI, TB_TYPE_VOID, curr->input_count, 0);
                set_input(f, new_phi, region, 0);
                mark_node(f, new_phi);

                phis[i] = latest[1 + i] = new_phi;
                TB_OPTDEBUG(MEM2REG)(printf("  PHI %%%u\n", new_phi->gvn));

                if (ctx->renames[i].is_mem) {
                    // they're all part of the same set
                    state->refs += 1;
                    nl_table_put(sese2set, new_phi, state);
                }
            }

            state->phis = phis;
        }

        // fill in the phis (run identity rules on them too)
        if (!state->is_loop || (state->walked_once && !state->sealed)) {
            FOR_N(j, 1, curr->input_count) {
                MemoryState* pred = start_of_memory_sese(sese2set, curr->inputs[j]);
                FOR_N(i, 0, ctx->local_count) {
                    if (state->phis[i]) {
                        if (state->phis[i]->dt.type == TB_TAG_VOID && pred->latest[1 + i]) {
                            state->phis[i]->dt = pred->latest[1 + i]->dt;
                        }

                        if (pred->latest[1 + i] == NULL) {
                            TB_ASSERT(state->phis[i]->dt.type != TB_TAG_VOID);
                            pred->latest[1 + i] = make_poison(f, state->phis[i]->dt);
                        }

                        set_input(f, state->phis[i], pred->latest[1 + i], j);
                    }
                }

                if (curr->inputs[j] != pred->latest[0]) {
                    set_input(f, curr, pred->latest[0], j);
                    mark_node_n_users(f, curr);
                }
            }

            FOR_REV_N(i, 0, ctx->local_count) {
                TB_Node* n = state->phis[i];
                if (n == NULL) {
                    continue;
                }

                if (n->dt.type == TB_TAG_VOID && n->user_count == 0) {
                    TB_OPTDEBUG(MEM2REG)(printf("  KILL %%%u\n", n->gvn));

                    violent_kill(f, state->phis[i]);
                    state->phis[i] = NULL;
                    if (latest) {
                        latest[1 + i] = NULL;
                    }
                    continue;
                }

                /* TB_Node* same = identity_phi(f, n);
                if (n != same) {
                    TB_OPTDEBUG(MEM2REG)(printf("  RENAME %%%u => %%%u\n", n->gvn, same->gvn));

                    // rename memory set
                    FOR_N(j, 0, ctx->postorder_len) {
                        MemoryState* kid_state = ctx->postorder_states[j];
                        MemorySet* set = nl_table_get(&kid_state->non_aliasing, state->phis[i]);
                        if (set) {
                            set->base = same;

                            nl_table_put(&kid_state->non_aliasing, same, set);
                            nl_table_remove(&kid_state->non_aliasing, state->phis[i]);
                        }
                    }

                    TB_ASSERT(same->type != TB_NULL);
                    subsume_node(f, n, same);
                    state->latest[1 + i] = same;
                } else {
                    TB_OPTDEBUG(MEM2REG)(printf("  SEALED %%%u\n", n->gvn));
                }*/
                TB_OPTDEBUG(MEM2REG)(printf("  SEALED %%%u\n", n->gvn));
            }

            state->sealed = true;
        }

        CUIK_TIMED_BLOCK("merge states") {
            // first time around a loop we process it without the preds since they're incomplete
            if (!state->is_loop || state->walked_once) {
                FOR_N(j, 1, curr->input_count) {
                    MemoryState* pred = start_of_memory_sese(sese2set, curr->inputs[j]);
                    merge_memory(f, non_aliasing, &pred->non_aliasing);
                }
            }
        }
    } else {
        nl_table_for(e, &state->non_aliasing) {
            nl_table_put(non_aliasing, e->k, e->v);
        }

        if (latest && (curr->type != TB_PROJ || curr->inputs[0]->type != TB_ROOT)) {
            TB_Node* old_mem = curr->inputs[curr->type == TB_MERGEMEM ? 2 : 1];
            MemoryState* pred = start_of_memory_sese(sese2set, old_mem);
            memcpy(latest, pred->latest, (1 + ctx->local_count) * sizeof(TB_Node*));
        }
    }
    state->walked_once = true;

    #if TB_OPTDEBUG_MEM2REG
    printf("\nSESE %d   [ ", state->order);
    print_memory_state(non_aliasing);
    printf("]\n");
    print_var_state(ctx, state);
    #endif

    for (;;) {
        int load_count = 0;
        TB_Node** loads = tb_arena_alloc(&f->tmp_arena, curr->user_count * sizeof(TB_Node*));

        // the next memory output, NULL if there's multiple
        TB_Node* next = NULL;
        FOR_USERS(u, curr) {
            TB_Node* use_n = USERN(u);
            int use_i = USERI(u);
            if (use_n->type == TB_LOAD && use_i == 1) {
                loads[load_count++] = use_n;
            }
        }

        FOR_USERS(u, curr) {
            TB_Node* use_n = USERN(u);
            int use_i = USERI(u);

            // not a real memory use
            if (curr->type == TB_SPLITMEM && use_n->type == TB_MERGEMEM && use_i == 1) {
                continue;
            }

            if (use_n->type == TB_PHI || is_mem_end_op(use_n) || use_n->type == TB_MERGEMEM) {
                next = NULL;
                break;
            } else if (cfg_is_mproj(use_n) || (use_i == 1 && tb_node_has_mem_out(use_n))) {
                if (next == NULL) { next = use_n; }
                else { next = NULL; break; }
            }
        }

        #if TB_OPTDEBUG_MEM2REG
        printf("  WALK %%%u [ ", curr->gvn);
        print_memory_state(non_aliasing);
        printf("]\n");
        #endif

        // TODO(NeGate): classify the effect
        int cat = 0;
        if (curr->type == TB_STORE) {
            cat = find_local_idx(ctx, curr->inputs[2]);
            if (cat == 0) {
                // normal store
                TB_Node* base;
                SimpleMemRef ref = find_simple_mem_ref(f, ctx, curr, &base);

                MemorySet* set = memory_set_find_or_create(f, non_aliasing, base);
                if (set->cnt == 0) {
                    TB_OPTDEBUG(MEM2REG)(printf("    INVALIDATE ALL!\n"));

                    // first time constructing this set, previous writes to different bases
                    // might alias so they should be cleared.
                    //
                    // if we know the "object" that the pointer is referring to then we can strictly
                    // talk about aliasing.
                    memory_set_clear_except(non_aliasing, set);
                    memory_set_insert(f, non_aliasing, set, 0, ref);
                } else {
                    // check if we clobbered any stores
                    int idx = find_aliasing_store(ctx, set, ref.offset);
                    if (
                        idx >= 0 && idx < set->cnt &&
                        !state->walked_once &&
                        set->stores[idx].mem->user_count == 1 &&
                        ref.offset >= set->stores[idx].offset &&
                        set->stores[idx].offset + set->stores[idx].size <= ref.offset + ref.size
                    ) {
                        TB_Node* st_mem = set->stores[idx].mem;

                        // the entire previous write is eaten up by this write
                        TB_OPTDEBUG(MEM2REG)(printf("    DEAD-STORE %%%u\n", st_mem->gvn));
                        st_mem->type = TB_DEAD_STORE;
                        worklist_push(ctx->dead_worklist, st_mem);
                    } else {
                        bool clobber = false;
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
                        if (!clobber && idx >= 0) {
                            memory_set_insert(f, non_aliasing, set, idx, ref);
                        } else {
                            TB_OPTDEBUG(MEM2REG)(printf("    CLOBBER\n"));

                            memory_set_clear_except(non_aliasing, set);
                            memory_set_insert(f, non_aliasing, set, 0, ref);
                        }
                    }
                }
            }

            if (latest) {
                TB_Node* old_mem = curr->inputs[1];
                if (cat == 0 || ctx->renames[cat - 1].is_mem) {
                    // rewrite memory edge
                    TB_ASSERT(latest[cat]);
                    if (old_mem != latest[cat]) {
                        set_input(f, curr, latest[cat], 1);

                        // invalidate old memory type
                        mark_node_n_users(f, curr);
                        latuni_set(f, curr, NULL);
                    }
                    latest[cat] = curr;
                } else {
                    // get rid of the store, we don't need it
                    latest[cat] = curr->inputs[3];

                    #if TB_OPTDEBUG_MEM2REG
                    printf("      STORE [cat=%d] = %%%u (KILLED %%%u)\n", cat, latest[cat]->gvn, curr->gvn);
                    #endif

                    curr->type = TB_DEAD_STORE;
                    set_input(f, curr, NULL, 2);
                    set_input(f, curr, NULL, 3);
                    worklist_push(ctx->dead_worklist, curr);
                }
            }
        } else if (curr->type != TB_PROJ && curr->type != TB_PHI && tb_node_has_mem_out(curr)) {
            // unknown memory access, clobber everything
            nl_table_clear(non_aliasing);

            if (latest) {
                int mem = curr->type == TB_MERGEMEM ? 2 : 1;
                if (curr != latest[0] && curr->inputs[mem] != latest[0]) {
                    TB_ASSERT(curr->inputs[mem] != curr);
                    set_input(f, curr, latest[0], mem);
                    mark_node_n_users(f, curr);
                }

                if (curr->type == TB_MERGEMEM && ctx->insert_merge) {
                    FOR_N(i, 1, f->root_node->input_count) {
                        TB_Node* end = f->root_node->inputs[i];
                        if (end->type != TB_RETURN && end->type != TB_TRAP && end->type != TB_UNREACHABLE) {
                            continue;
                        }

                        if (end->inputs[1] == curr) {
                            size_t j = 3;
                            FOR_N(i, 0, ctx->local_count) if (ctx->renames[i].is_mem) {
                                TB_ASSERT(latest[1 + i] != NULL && "TODO we should place a poison?");
                                set_input(f, curr, latest[1 + i], j++);
                            }
                            break;
                        }
                    }
                }
                latest[0] = curr;
            }
        }

        // TODO(NeGate): renaming loads
        if (curr->dt.type == TB_TAG_TUPLE) {
            // skip to mproj
            TB_ASSERT(curr->type != TB_SPLITMEM);
            curr = next_mem_user(curr);
            if (latest) {
                latest[0] = curr;
            }
        }

        // fixup any connected loads
        FOR_N(i, 0, load_count) {
            TB_Node* use_n = loads[i];
            int cat = find_local_idx(ctx, use_n->inputs[2]);

            #if TB_OPTDEBUG_MEM2REG
            printf("    LOAD %%%u (cat=%d)\n", use_n->gvn, cat);
            #endif

            TB_Node* val = NULL;
            if (cat == 0) {
                // normal load, try to elim
                TB_Node* base;
                SimpleMemRef ref = find_simple_mem_ref(f, ctx, use_n, &base);

                #if TB_OPTDEBUG_MEM2REG
                printf("      REF [%%%u + %"PRId64"]\n", base->gvn, ref.offset);
                #endif

                MemorySet* set = nl_table_get(non_aliasing, base);
                if (set) {
                    int idx = find_aliasing_store(ctx, set, ref.offset);
                    if (idx < set->cnt && set->stores[idx].offset == ref.offset && set->stores[idx].size == ref.size) {
                        TB_ASSERT(set->stores[idx].mem->type == TB_STORE);
                        val = set->stores[idx].mem->inputs[3];
                    }
                }
            } else if (!ctx->renames[cat - 1].is_mem) {
                TB_ASSERT(use_n->type == TB_LOAD);
                val = node_or_poison(f, latest[cat], use_n->dt);
            }

            if (val != NULL) {
                if (latest) {
                    #if TB_OPTDEBUG_MEM2REG
                    printf("      ELIM %%%u => %%%u\n", use_n->gvn, val->gvn);
                    #endif

                    if (use_n->dt.raw != val->dt.raw) {
                        // it's one of the half constructed phis we've got laying around
                        if (val->type == TB_PHI && val->dt.type == TB_TAG_VOID) {
                            val->dt = use_n->dt;
                        } else {
                            // insert bitcast
                            TB_Node* cast = tb_alloc_node(f, TB_BITCAST, use_n->dt, 2, 0);
                            set_input(f, cast, val, 1);
                            val = cast;
                        }
                    }

                    int old = val->user_count;
                    subsume_node(f, use_n, val);
                    if (val->user_count != old) {
                        mark_node_n_users(f, val);
                    }
                    ctx->progress = true;
                }
            } else {
                if (cat > 0) {
                    ctx->renames[cat - 1].is_alive = true;
                }

                // rewrite memory edge
                if (latest && use_n->inputs[1] != latest[cat]) {
                    set_input(f, use_n, latest[cat], 1);
                    mark_node_n_users(f, use_n);
                }
            }
        }
        tb_arena_free(&f->tmp_arena, loads, curr->user_count * sizeof(TB_Node*));

        // advance memory node
        if (next == NULL) {
            break;
        }
        curr = next;
    }

    #if TB_OPTDEBUG_MEM2REG
    printf("  FINAL [ ");
    print_memory_state(non_aliasing);
    printf("]\n");
    print_var_state(ctx, state);
    #endif

    // Copy out the memory state
    nl_table_clear(&state->non_aliasing);
    nl_table_for(e, non_aliasing) {
        nl_table_put(&state->non_aliasing, e->k, e->v);
    }

    if (out_progress) {
        *out_progress = false;
    }
    return curr;
}

static void postorder_memory(TB_Function* f, NL_Table* sese2set, TB_Worklist* sese_worklist, TB_Node* start, int local_count) {
    if (worklist_test_n_set(sese_worklist, start)) {
        return;
    }

    // walk successors first
    TB_Node* end = end_of_memory_sese(start);
    FOR_USERS(u, end) {
        TB_Node* use_n = USERN(u);
        int use_i = USERI(u);

        if (use_n->type == TB_MERGEMEM && use_i == 1) {
            // not a real memory use
        } else if (is_mem_end_op(use_n)) {
            // don't traverse past it
        } else if (use_n->type == TB_MERGEMEM || use_n->type == TB_PHI || cfg_is_mproj(use_n) || (use_i == 1 && tb_node_has_mem_out(use_n))) {
            postorder_memory(f, sese2set, sese_worklist, use_n, local_count);
        }
    }

    MemoryState* state = tb_arena_alloc(&f->tmp_arena, sizeof(MemoryState) + ((1 + local_count) * sizeof(TB_Node*)));
    *state = (MemoryState){ .refs = 1, .order = dyn_array_length(sese_worklist->items), .start = start };
    state->non_aliasing = nl_table_alloc(4);
    FOR_N(i, 0, 1 + local_count) {
        state->latest[i] = NULL;
    }
    nl_table_put(sese2set, start, state);

    if (start != end) {
        state->refs += 1;
        nl_table_put(sese2set, end, state);
    }

    // printf("SESE %d, %%%u -- %%%u\n", state->order, start->gvn, end->gvn);
    dyn_array_put(sese_worklist->items, start);
}

static TB_Node* node_uf_find(TB_Node** uf, TB_Node* n) {
    // find UF leader
    TB_Node* leader = n;
    while (uf[leader->gvn]) {
        leader = uf[leader->gvn];
    }

    // path compaction
    while (uf[n->gvn]) {
        TB_Node* next = uf[n->gvn];
        uf[n->gvn] = leader, n = next;
    }

    return leader;
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

        bool blackhole = false;
        FOR_USERS(mem, addr) {
            if (USERN(mem)->type == TB_BLACKHOLE) {
                blackhole = true;
            } else if (USERI(mem) == 1 && USERN(mem)->type == TB_PTR_OFFSET) {
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
            ctx.renames[j].is_alive = blackhole;
            ctx.renames[j].addr = addr;
            j += 1;
        }
    }

    ctx.local_count = j;

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

                ctx.insert_merge = true;
            }
            latest[0] = split;
        } else {
            latest[0] = f->params[1];
        }

        // tb_print(f);
        // tb_print_dumb(f);

        TB_Worklist sese_worklist;
        NL_Table sese2set = nl_table_alloc(20);
        worklist_alloc(&sese_worklist, 20);

        CUIK_TIMED_BLOCK("postorder_memory") {
            postorder_memory(f, &sese2set, &sese_worklist, f->params[1], ctx.local_count);
        }

        // printf("\n\n");

        MemoryState* initial_state = nl_table_get(&sese2set, f->params[1]);
        initial_state->non_aliasing = nl_table_alloc(20);
        memcpy(initial_state->latest, latest, (1 + ctx.local_count) * sizeof(TB_Node*));

        // basic loop finding
        ctx.postorder_len = dyn_array_length(sese_worklist.items);
        ctx.postorder_states = tb_arena_alloc(&f->tmp_arena, ctx.postorder_len * sizeof(MemoryState*));
        FOR_REV_N(i, 0, ctx.postorder_len) {
            TB_Node* start = sese_worklist.items[i];
            MemoryState* state = nl_table_get(&sese2set, start);

            MemoryState* loop_tail = NULL;
            if (start->type == TB_PHI) {
                FOR_N(j, 1, start->input_count) {
                    MemoryState* pred = start_of_memory_sese(&sese2set, start->inputs[j]);
                    if (state->order >= pred->order &&
                        // we wanna make sure the loop_tail is the "last" loop pred we see
                        (loop_tail == NULL || pred->order < loop_tail->order)
                    ) {
                        loop_tail = pred;
                    }
                }
            }

            if (loop_tail) {
                state->is_loop = true;
                loop_tail->loop_head = state;
            }
            ctx.postorder_states[i] = state;
        }

        // we don't need it anymore for the postorder walk, we
        // use it for tracking dead stores now.
        worklist_clear(&sese_worklist);
        ctx.dead_worklist = &sese_worklist;

        // RPO walk, any time we hit a loop tail, we re-eval the
        // loop head and if it made progress we revert back to the
        // loop head for processing.
        NL_Table non_aliasing = nl_table_alloc(16);
        int i = ctx.postorder_len;
        while (i--) {
            MemoryState* state = ctx.postorder_states[i];
            TB_ASSERT(state->order == i);

            cuikperf_region_start("transfer", NULL);
            TB_Node* end = process_sese(f, &sese2set, &ctx, state->start, state, &non_aliasing, NULL);
            cuikperf_region_end();

            if (state->loop_head) {
                // revisit loop header, if it makes progress we revisit the loop body
                bool progress;
                process_sese(f, &sese2set, &ctx, state->loop_head->start, state->loop_head, &non_aliasing, &progress);
                if (progress && i + 1 > state->loop_head->order) {
                    i = state->loop_head->order + 1;
                }
            }
        }
        tb_arena_free(&f->tmp_arena, ctx.postorder_states, dyn_array_length(sese_worklist.items) * sizeof(MemoryState*));

        // memory teardown
        CUIK_TIMED_BLOCK("teardown") {
            nl_table_for(set, &sese2set) {
                MemoryState* state = set->v;
                if (--state->refs == 0) {
                    nl_table_for(e, &state->non_aliasing) {
                        cuik_free(e->v);
                    }
                    nl_table_free(state->non_aliasing);
                }
            }
            nl_table_free(non_aliasing);
            nl_table_free(sese2set);
        }

        CUIK_TIMED_BLOCK("phi identities") {
            // from the newly marked phis, build up a disjoint-set for the
            // equivalence relations, then replace them all at once before
            // advancing to the peeps
            TB_Node** uf = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(TB_Node*));
            FOR_N(i, 0, f->node_count) { uf[i] = NULL; }

            TB_Worklist* ws = f->worklist;
            int id = 0;
            for (size_t i = dyn_array_length(ws->items); i--;) {
                TB_Node* n = ws->items[i];
                // if we're not the leader, we've already been processed... somehow?
                if (n->type != TB_PHI || uf[n->gvn]) {
                    continue;
                }

                TB_Node* same = NULL;
                FOR_N(i, 1, n->input_count) {
                    TB_Node* in = node_uf_find(uf, n->inputs[i]);

                    if (in == n) continue;
                    if (same && same != in) { same = n; break; }
                    same = in;
                }

                if (same != n) {
                    TB_ASSERT(same);
                    TB_ASSERT(uf[same->gvn] == NULL);
                    uf[n->gvn] = same;
                    subsume_node(f, n, same);
                    id++;
                }
            }

            /* for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
                TB_Node* n = ws->items[i];
                if (n->type == TB_PHI && uf[n->gvn]) {
                    TB_Node* same = node_uf_find(uf, n);
                    TB_ASSERT(same == uf[n->gvn]);

                    tb_print_dumb_node(NULL, n);
                    printf(" => ");
                    tb_print_dumb_node(NULL, same);
                    printf("\n");

                }
            } */

            /* TB_Node* n;
            while (n = worklist_pop(&sese_worklist), n) {
                if (n->type == TB_PHI || n->type == TB_DEAD_STORE) {
                    subsume_node(f, n, n->inputs[1]);
                } else {
                    tb_kill_node(f, n);
                }
            }
            __debugbreak();*/
        }

        CUIK_TIMED_BLOCK("kill nodes") {
            FOR_N(i, 0, ctx.local_count) if (!ctx.renames[i].is_alive) {
                // discover all dead stores and addresses
                worklist_push(&sese_worklist, ctx.renames[i].addr);

                FOR_USERS(u, ctx.renames[i].addr) {
                    if (USERN(u)->type == TB_PTR_OFFSET) {
                        FOR_USERS(u2, USERN(u)) {
                            if (USERN(u2)->type == TB_STORE) {
                                worklist_push(&sese_worklist, USERN(u2));
                            }
                        }
                    } else if (USERN(u)->type == TB_STORE || USERN(u)->type == TB_DEAD_STORE) {
                        worklist_push(&sese_worklist, USERN(u));
                    }
                }
            }

            TB_Node* n;
            while (n = worklist_pop(&sese_worklist), n) {
                if (n->type == TB_STORE || n->type == TB_DEAD_STORE) {
                    subsume_node(f, n, n->inputs[1]);
                } else {
                    tb_kill_node(f, n);
                }
            }
        }

        // tb_print_dumb(f);
        // tb_print(f);
        // __debugbreak();

        worklist_free(&sese_worklist);
    }

    tb_arena_restore(&f->tmp_arena, sp);
    cuikperf_region_end();

    return needs_to_rewrite;
}
