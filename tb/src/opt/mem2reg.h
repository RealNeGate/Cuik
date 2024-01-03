
typedef enum {
    COHERENCY_DEAD,
    COHERENCY_GOOD,

    // failure states
    COHERENCY_USES_ADDRESS,
    COHERENCY_VOLATILE
} Coherency;

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

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Node* r) {
    // TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
    FOREACH_N(i, 0, c->to_promote_count) {
        if (c->to_promote[i] == r) return i;
    }

    return -1;
}

// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Node* new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, int var, TB_Node* block, TB_DataType dt) {
    TB_Node* n = tb_alloc_node(f, TB_PHI, dt, 1 + block->input_count, 0);
    set_input(f, n, block, 0);

    // append variable attrib
    /*for (TB_Attrib* a = c->to_promote[var]->first_attrib; a; a = a->next) if (a->type == TB_ATTRIB_VARIABLE) {
        tb_node_append_attrib(n, a);
        break;
    }*/

    DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("v%u: insert new PHI node (in v%u)", n->gvn, block->gvn));
    tb_pass_mark(c->p, n);
    return n;
}

static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Node* phi_node, TB_Node* bb, TB_Node* node) {
    // we're using NULL nodes as the baseline PHI0
    if (phi_node == node) {
        return;
    }

    phi_node->dt = node->dt;
    if (phi_node->type == TB_POISON) {
        return;
    }

    assert(phi_node->type == TB_PHI);
    TB_Node* phi_region = phi_node->inputs[0];
    DO_IF(TB_OPTDEBUG_MEM2REG)(printf("v%u: adding v%u to PHI\n", phi_node->gvn, node->gvn));

    // the slot to fill is based on the predecessor list of the region
    FOREACH_N(i, 0, phi_region->input_count) {
        TB_Node* pred = get_pred_cfg(&c->p->cfg, phi_region, i);
        if (pred == bb) {
            set_input(f, phi_node, node, i+1);
            break;
        }
    }

    tb_pass_mark_users(c->p, phi_node);
}

static void write_variable(Mem2Reg_Ctx* c, int var, TB_Node* block, TB_Node* value) {
    if (c->defs[var] == NULL) {
        nl_map_create(c->defs[var], 16);
    }

    nl_map_put(c->defs[var], block, value);
}

static void ssa_replace_phi_arg(Mem2Reg_Ctx* c, TB_Function* f, TB_Node* bb, TB_Node* dst, DynArray(TB_Node*)* stack) {
    FOREACH_N(var, 0, c->to_promote_count) {
        ptrdiff_t search = nl_map_get(c->defs[var], dst);
        if (search < 0) continue;

        TB_Node* phi_reg = c->defs[var][search].v;
        if (phi_reg->type != TB_PHI) continue;

        TB_Node* top;
        if (dyn_array_length(stack[var]) == 0) {
            // this is UB land, insert poison
            top = make_poison(f, phi_reg->dt);
            log_warn("%s: v%u: generated poison due to read of uninitialized local", f->super.name, top->gvn);
        } else {
            top = stack[var][dyn_array_length(stack[var]) - 1];
        }

        DO_IF(TB_OPTDEBUG_MEM2REG)(printf("v%u: replace v%u to PHI\n", phi_reg->gvn, top->gvn));

        bool found = false;
        FOREACH_N(j, 0, dst->input_count) {
            TB_Node* pred = get_pred_cfg(&c->p->cfg, dst, j);
            if (pred == bb) {
                // try to replace
                set_input(f, phi_reg, top, j + 1);
                found = true;
                break;
            }
        }

        if (!found) {
            add_phi_operand(c, f, phi_reg, bb, top);
        }
    }
}

static void ssa_rename(Mem2Reg_Ctx* c, TB_Function* f, TB_Node* bb, DynArray(TB_Node*)* stack) {
    assert(bb);
    TB_Passes* p = c->p;

    // push phi nodes
    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);
    size_t* old_len = tb_arena_alloc(tmp_arena, sizeof(size_t) * c->to_promote_count);
    FOREACH_N(var, 0, c->to_promote_count) {
        old_len[var] = dyn_array_length(stack[var]);

        ptrdiff_t search = nl_map_get(c->defs[var], bb);
        if (search >= 0 && c->defs[var][search].v->type == TB_PHI) {
            dyn_array_put(stack[var], c->defs[var][search].v);
        }
    }

    // rewrite operations
    TB_BasicBlock* bb_info = &nl_map_get_checked(c->p->cfg.node_to_block, bb);
    TB_Node* end = bb_info->end;

    DO_IF(TB_OPTDEBUG_MEM2REG)(
        printf("  FORST %u: ", bb->gvn),
        print_node_sexpr(bb, 0),
        printf("\n")
    );

    // go through all uses and replace their accessors
    TB_Node* n = bb_info->mem_in;
    if (n != NULL) {
        do {
            DO_IF(TB_OPTDEBUG_MEM2REG)(
                printf("  SIGMA %u: ", n->gvn),
                print_node_sexpr(n, 0),
                printf("\n")
            );

            // if we spot a store, we push to the stack
            bool kill = false;
            if (n->type == TB_STORE) {
                int var = get_variable_id(c, n->inputs[2]);
                if (var >= 0) {
                    DO_IF(TB_OPTDEBUG_MEM2REG)(printf("    ASSIGN %d -> %u\n", var, n->inputs[3]->gvn));

                    // push new store value onto the stack
                    dyn_array_put(stack[var], n->inputs[3]);
                    kill = true;
                }
            }

            // check for any loads and replace them
            FOR_USERS(u, n) {
                TB_Node* use = u->n;

                if (u->slot == 1 && use->type == TB_LOAD) {
                    int var = get_variable_id(c, use->inputs[2]);
                    if (var >= 0) {
                        TB_Node* val;
                        if (dyn_array_length(stack[var]) == 0) {
                            // this is UB since it implies we've read before initializing the
                            // stack slot.
                            val = make_poison(f, use->dt);
                            log_warn("v%u: found load-before-init in mem2reg, this is UB", use->gvn);
                        } else {
                            val = stack[var][dyn_array_length(stack[var]) - 1];
                        }

                        // make sure it's the right type
                        if (use->dt.raw != val->dt.raw) {
                            TB_Node* cast = tb_alloc_node(c->f, TB_BITCAST, use->dt, 2, 0);
                            tb_pass_mark(p, cast);
                            set_input(f, cast, val, 1);

                            val = cast;
                        }

                        set_input(f, use, NULL, 1); // unlink first
                        subsume_node(f, use, val);

                        tb_pass_mark(p, val);
                        tb_pass_mark_users(p, val);
                    }
                }
            }

            // next memory has to be decided before we kill the node since
            // murder will dettach the users.
            TB_Node* next = mem_user(p, n, 1);

            // we can remove the effect now
            if (kill) {
                TB_Node* into = n->inputs[1];
                subsume_node(c->f, n, into);

                tb_pass_mark(c->p, into);
                tb_pass_mark_users(p, into);
            }

            n = next;
        } while (n != NULL && n->type != TB_PHI && cfg_underneath(&c->p->cfg, n, bb_info));
    }

    // replace phi arguments on successor
    if (end->type == TB_BRANCH) {
        // fill successors
        FOR_USERS(u, end) {
            if (!cfg_is_control(u->n)) continue;

            TB_Node* succ = cfg_next_bb_after_cproj(u->n);
            if (succ->type != TB_REGION) continue;

            // for p in succ's phis:
            //   if p is a var v, replace edge with stack[v]
            ssa_replace_phi_arg(c, f, bb, succ, stack);
        }
    } else if (end->type != TB_ROOT && end->type != TB_UNREACHABLE) {
        // fallthrough case
        ssa_replace_phi_arg(c, f, bb, cfg_next_control(end), stack);
    }

    // for each successor s of the BB in the dominator
    //    rename(s)
    //
    // TODO(NeGate): maybe we want a data structure for this because it'll
    // be "kinda" slow.
    FOREACH_N(i, 0, c->p->cfg.block_count) {
        TB_Node* k = c->blocks[i];
        TB_Node* v = idom(&c->p->cfg, k);

        if (v == bb && k != bb) {
            ssa_rename(c, f, k, stack);
        }
    }

    FOREACH_N(var, 0, c->to_promote_count) {
        dyn_array_set_length(stack[var], old_len[var]);
    }
    tb_arena_restore(tmp_arena, sp);
}

static void insert_phis(Mem2Reg_Ctx* restrict ctx, TB_Node* bb, TB_Node* n, TB_BasicBlock* bb_info) {
    DO_IF(TB_OPTDEBUG_MEM2REG)(
        printf("  FORST %u: ", bb->gvn),
        print_node_sexpr(bb, 0),
        printf("\n")
    );

    do {
        DO_IF(TB_OPTDEBUG_MEM2REG)(
            printf("  OMEGA %u: ", n->gvn),
            print_node_sexpr(n, 0),
            printf("\n")
        );

        if (n->type == TB_STORE) {
            int var = get_variable_id(ctx, n->inputs[2]);
            if (var >= 0) {
                write_variable(ctx, var, bb, n->inputs[3]);
            }
        }

        // next memory
        n = mem_user(ctx->p, n, 1);
    } while (n != NULL && n->type != TB_PHI && cfg_underneath(&ctx->p->cfg, n, bb_info));
}

void tb_pass_mem2reg(TB_Passes* p) {
    cuikperf_region_start("mem2reg", NULL);

    TB_Function* f = p->f;

    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    FOR_USERS(u, f->root_node) {
    }

    size_t to_promote_count = dyn_array_length(p->worklist.items);
    if (to_promote_count == 0) {
        // doesn't need to mem2reg
        cuikperf_region_end();
        return;
    }

    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);

    Mem2Reg_Ctx c = {
        .f = f,
        .p = p,
        .to_promote_count = to_promote_count,
        .to_promote = tb_arena_alloc(tmp_arena, to_promote_count * sizeof(TB_Node*)),
    };

    memcpy(c.to_promote, p->worklist.items, to_promote_count * sizeof(TB_Node*));
    dyn_array_clear(p->worklist.items);

    c.defs = tb_arena_alloc(tmp_arena, to_promote_count * sizeof(Mem2Reg_Def));
    memset(c.defs, 0, to_promote_count * sizeof(Mem2Reg_Def));

    tb_pass_update_cfg(p, &p->worklist, true);
    c.blocks = &p->worklist.items[0];

    worklist_clear_visited(&p->worklist);
    TB_DominanceFrontiers* df = tb_get_dominance_frontiers(f, p, c.p->cfg, c.blocks);

    ////////////////////////////////
    // Phase 1: Insert phi functions
    ////////////////////////////////
    // Identify the final value of all the variables in the function per basic block
    FOREACH_N(i, 0, c.p->cfg.block_count) {
        TB_Node* bb = c.blocks[i];
        TB_BasicBlock* bb_info = &nl_map_get_checked(c.p->cfg.node_to_block, bb);

        // mark into worklist
        worklist_test_n_set(&p->worklist, bb);
        tb_pass_mark_users(p, bb);

        if (i == 0) {
            // start block can use the input memory as the earliest point
            insert_phis(&c, bb, f->params[1], bb_info);
            bb_info->mem_in = f->params[1];
            continue;
        }

        TB_Node* end = bb_info->end;

        // find memory phi
        TB_Node* n = bb;
        TB_Node* mem = NULL;
        while (n != NULL) {
            FOR_USERS(u, n) {
                if (is_mem_out_op(u->n)) {
                    mem = u->n;
                    goto done;
                }
            }

            if (n == end) break;
            n = cfg_next_control(n);
        }

        done:
        // find earliest memory in the BB:
        //   note this doesn't account for multiple memory streams
        //   but that's fine for now...
        if (mem && !(mem->type == TB_PROJ && mem->inputs[0]->type == TB_ROOT)) {
            while (mem->type != TB_PHI && cfg_underneath(&c.p->cfg, mem->inputs[1], bb_info)) {
                mem = mem->inputs[1];
            }

            insert_phis(&c, bb, mem, bb_info);
        }

        bb_info->mem_in = mem;
    }

    // for each global name we'll insert phi nodes
    TB_Node** phi_p = tb_arena_alloc(tmp_arena, c.p->cfg.block_count * sizeof(TB_Node*));

    NL_HashSet ever_worked = nl_hashset_alloc(c.p->cfg.block_count);
    NL_HashSet has_already = nl_hashset_alloc(c.p->cfg.block_count);
    FOREACH_N(var, 0, c.to_promote_count) {
        nl_hashset_clear(&ever_worked);
        nl_hashset_clear(&has_already);

        size_t p_count = 0;
        FOREACH_N(i, 0, c.p->cfg.block_count) {
            TB_Node* bb = c.blocks[i];

            ptrdiff_t search = nl_map_get(c.defs[var], bb);
            if (search >= 0) {
                nl_hashset_put(&ever_worked, bb);
                phi_p[p_count++] = bb;
            }
        }

        // it's a global name
        if (p_count > 1) {
            // insert phi per dominance of the blocks it's defined in
            for (size_t i = 0; i < p_count; i++) {
                TB_Node* bb = phi_p[i];
                TB_Node* value = nl_map_get_checked(c.defs[var], bb);
                TB_DataType dt = value->dt;

                // for all DFs of BB, insert PHI
                int bb_id = nl_map_get_checked(c.p->cfg.node_to_block, bb).id;
                uint64_t* frontier = &df->arr[bb_id * df->stride];
                FOREACH_N(j, 0, df->stride) FOREACH_BIT(k, j*64, frontier[j]) {
                    TB_Node* l = c.blocks[k];
                    if (!nl_hashset_put(&has_already, l)) continue;

                    ptrdiff_t search = nl_map_get(c.defs[var], l);

                    TB_Node* phi_reg = NULL;
                    if (search < 0) {
                        phi_reg = new_phi(&c, f, var, l, dt);
                        nl_map_put(c.defs[var], l, phi_reg);
                    } else {
                        phi_reg = c.defs[var][search].v;

                        if (phi_reg->type != TB_PHI) {
                            TB_Node* old_reg = phi_reg;
                            phi_reg = new_phi(&c, f, var, l, dt);
                            add_phi_operand(&c, f, phi_reg, l, old_reg);

                            nl_map_put(c.defs[var], l, phi_reg);
                        }
                    }

                    add_phi_operand(&c, f, phi_reg, bb, value);

                    if (nl_hashset_put(&ever_worked, l)) {
                        phi_p[p_count++] = l;
                    }
                }
            }
        }
    }
    tb_platform_heap_free(df);

    ////////////////////////////////
    // Phase 2: Rename loads and stores
    ////////////////////////////////
    DynArray(TB_Node*)* stack = tb_arena_alloc(tmp_arena, c.to_promote_count * sizeof(DynArray(TB_Node*)));
    FOREACH_N(var, 0, c.to_promote_count) {
        stack[var] = dyn_array_create(TB_Node*, 16);
    }

    ssa_rename(&c, f, c.blocks[0], stack);

    // don't need these anymore
    FOREACH_N(var, 0, c.to_promote_count) {
        assert(c.to_promote[var]->users == NULL);
        tb_pass_kill_node(f, c.to_promote[var]);
    }
    tb_arena_restore(tmp_arena, sp);
    tb_free_cfg(&p->cfg);
    cuikperf_region_end();
}

static bool good_mem_op(TB_Node* n) { // ld, st, memcpy, memset
    return n->type >= TB_LOAD && n->type <= TB_MEMSET;
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

    NL_Table phi2alias;
    // DynArray(TB_Node*) phis;
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
        int cat = -1;
        if (curr->type == TB_PHI) {
            uintptr_t v = (uintptr_t) nl_table_get(&ctx->phi2alias, curr);
            assert(v);
            cat = v - 1;
        } else {
            cat = categorize_alias_idx(ctx, curr->inputs[2]);
        }

        assert(cat >= 0);
        if (latest[cat] != curr) {
            set_input(f, curr, latest[cat], 1);
            latest[cat] = curr;
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
            else if (use_n->type == TB_MERGEMEM) { reason = MEM_END;  }
            else if (use_n->type == TB_PHI)      { reason = MEM_JOIN; }
            else if (is_mem_only_in_op(u->n))    { reason = MEM_USE; }
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
                    TB_Node* k = identity_load(p, f, use_n);
                    if (k != use_n) {
                        subsume_node(f, use_n, k);
                        tb_pass_mark_users(p, k);
                    } else {
                        tb_pass_mark(p, use_n);
                    }
                }
            }
        }
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

                    // convert single phi into multiple parallel phis
                    set_input(f, use_n, latest[0], use_i);
                    nl_table_put(&ctx->phi2alias, use_n, (void*) 1);
                    new_latest[0] = use_n;

                    // make extra alias phis
                    FOREACH_N(i, 1, ctx->alias_cnt) {
                        TB_Node* new_phi = tb_alloc_node(f, TB_PHI, TB_TYPE_MEMORY, use_n->input_count, 0);
                        set_input(f, new_phi, region, 0);
                        set_input(f, new_phi, latest[i], use_i);
                        new_latest[i] = new_phi;
                        tb_pass_mark(p, new_phi);

                        nl_table_put(&ctx->phi2alias, new_phi, (void*) (i+1));
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
                tb_pass_mark(p, use_n);
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

void tb_pass_split_locals(TB_Passes* p) {
    TB_Function* f = p->f;

    assert(dyn_array_length(p->worklist.items) == 0);
    worklist_clear_visited(&p->worklist);

    // which locals can never alias (no ptr uses)
    FOR_USERS(u, f->root_node) {
        if (u->n->type != TB_LOCAL) continue;
        TB_Node* addr = u->n;

        bool good = true;
        FOR_USERS(mem, addr) {
            if (mem->slot == 1 && (mem->n->type == TB_MEMBER_ACCESS || mem->n->type == TB_ARRAY_ACCESS)) {
                // pointer arith are also fair game, since they'd stay in bounds (given no UB)
            } else if (mem->slot != 2 || !good_mem_op(mem->n)) {
                good = false;
                break;
            }
        }

        if (good) {
            tb_pass_mark(p, addr);
        }
    }

    if (dyn_array_length(p->worklist.items) == 0) {
        return;
    }

    // locals + root mem
    LocalSplitter ctx = { .alias_cnt = dyn_array_length(p->worklist.items)+1 };
    int new_aliases = p->alias_n;
    p->alias_n += ctx.alias_cnt-1;

    ctx.projs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
    ctx.addrs = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));

    TB_ArenaSavepoint sp = tb_arena_save(tmp_arena);
    ctx.addrs[0] = NULL;
    FOREACH_N(i, 1, ctx.alias_cnt) {
        ctx.addrs[i] = p->worklist.items[i-1];
    }

    ctx.phi2alias = nl_table_alloc(200);

    // new final memory node
    TB_Node* merge = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 2 + ctx.alias_cnt, 0);
    set_input(f, merge, f->root_node->inputs[0], 0);

    // new initial memory node
    TB_Node* split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_TUPLE, 2, sizeof(TB_NodeMemSplit) + ctx.alias_cnt*sizeof(int));
    set_input(f, split, f->params[0], 0);
    set_input(f, merge, split, 1);

    TB_NodeMemSplit* split_extra = TB_NODE_GET_EXTRA(split);
    split_extra->alias_cnt = ctx.alias_cnt;
    FOREACH_N(i, 0, ctx.alias_cnt) {
        split_extra->alias_idx[i] = i ? new_aliases+i-1 : 0;
        ctx.projs[i] = make_proj_node(f, TB_TYPE_MEMORY, split, i);
        tb_pass_mark(p, ctx.projs[i]);

        if (i) {
            set_input(f, merge, ctx.projs[i], 2+i);
        }
    }
    tb_pass_mark(p, split);

    // stitch memory effects
    subsume_node2(f, f->params[1], ctx.projs[0]);
    set_input(f, split, f->params[1], 1);
    set_input(f, merge, f->root_node->inputs[1], 2);
    set_input(f, f->root_node, merge, 1);

    // walk from the last memory effect until the next phi node.
    TB_Node* first_mem = next_mem_user(ctx.projs[0]);
    if (first_mem) {
        TB_Node** latest = tb_arena_alloc(tmp_arena, ctx.alias_cnt * sizeof(TB_Node*));
        FOREACH_N(i, 0, ctx.alias_cnt) { latest[i] = ctx.projs[i]; }

        fixup_mem_node(f, p, &ctx, first_mem, latest);
    }
    tb_arena_restore(tmp_arena, sp);

    tb_pass_mark(p, merge);
    tb_pass_mark_users(p, ctx.projs[0]);

    nl_table_free(ctx.phi2alias);

    // fold away the loads
    tb_pass_peephole(p);

    // let's dead code some of these
    FOREACH_N(i, 1, ctx.alias_cnt) {
        TB_Node* addr = ctx.addrs[i];
        if (!all_stores_dead(addr)) {
            continue;
        }

        // delete users without weird iteration invalidation issues
        expunge(f, addr);
        set_input(f, merge, ctx.projs[i], 2+i);
    }
}
