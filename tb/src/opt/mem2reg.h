// Based on Dominance Frontiers
//    https://www.ed.tus.ac.jp/j-mune/keio/m/ssa2.pdf
typedef enum {
    COHERENCY_GOOD,

    // failure states
    COHERENCY_USES_ADDRESS,
    COHERENCY_BAD_DATA_TYPE,
    COHERENCY_UNINITIALIZED,
    COHERENCY_VOLATILE
} Coherency;

// Region -> Value
typedef NL_Map(TB_Node*, TB_Node*) Mem2Reg_Def;

typedef struct Promotion Promotion;
struct Promotion {
    Promotion* next;
    TB_Node* n;
};

typedef struct Mem2Reg_Ctx {
    TB_TemporaryStorage* tls;
    TB_Function* f;
    TB_Passes* p;

    TB_Node* poison;

    // Stack slots we're going to convert into
    // SSA form
    size_t to_promote_count;
    TB_Node** to_promote;

    // [to_promote_count]
    Mem2Reg_Def* defs;

    TB_PostorderWalk order;
} Mem2Reg_Ctx;

static int bits_in_data_type(int pointer_size, TB_DataType dt);
static Coherency tb_get_stack_slot_coherency(TB_Passes* p, TB_Function* f, TB_Node* address, TB_DataType* dt);

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Node* r) {
    // TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
    FOREACH_N(i, 0, c->to_promote_count) {
        if (c->to_promote[i] == r) return (int)i;
    }

    return -1;
}

static int find_traversal_index2(Mem2Reg_Ctx* restrict ctx, TB_Node* bb) {
    FOREACH_N(i, 0, ctx->order.count) {
        if (ctx->order.traversal[i] == bb) return i;
    }

    tb_todo();
}

// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Node* new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, int var, TB_Node* block, TB_DataType dt) {
    TB_Node* n = tb_alloc_node(f, TB_PHI, dt, 1 + block->input_count, 0);
    FOREACH_N(i, 0, 1 + block->input_count) n->inputs[i] = NULL;

    set_input(c->p, n, block, 0);

    // append variable attrib
    /*for (TB_Attrib* a = c->to_promote[var]->first_attrib; a; a = a->next) if (a->type == TB_ATTRIB_VARIABLE) {
        tb_node_append_attrib(n, a);
        break;
    }*/

    DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%p: insert new PHI node (in %p)", n, block));
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
    DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%p: adding %p to PHI", phi_node, node));

    // the slot to fill is based on the predecessor list of the region
    FOREACH_N(i, 0, phi_region->input_count) {
        TB_Node* pred = phi_region->inputs[i];
        while (pred->type != TB_REGION && pred->type != TB_START) pred = pred->inputs[0];

        if (pred == bb) {
            set_input(c->p, phi_node, node, i+1);
            break;
        }
    }

    tb_pass_mark_users(c->p, phi_node);
    // tb_unreachable();
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
            if (c->poison != NULL) {
                // we probably already inserted a poison in this pass, nothing
                // says we can't reuse it.
                top = c->poison;
            } else {
                top = c->poison = tb_alloc_node(f, TB_POISON, TB_TYPE_VOID, 0, 0);
            }
        } else {
            top = stack[var][dyn_array_length(stack[var]) - 1];
        }

        bool found = false;
        FOREACH_N(j, 0, dst->input_count) {
            if (dst->inputs[j] == bb) {
                // try to replace
                set_input(c->p, phi_reg, top, j + 1);
                found = true;
                break;
            }
        }

        if (!found) {
            add_phi_operand(c, f, phi_reg, bb, top);
        }
    }
}

static void ssa_rename_node(Mem2Reg_Ctx* c, TB_Node* n, DynArray(TB_Node*)* stack) {
    if (n->type != TB_REGION && n->type != TB_START) {
        ssa_rename_node(c, n->inputs[0], stack);
    }

    // find promoted stack slots
    bool kill = false;
    if (n->type == TB_STORE) {
        int var = get_variable_id(c, n->inputs[1]);
        if (var >= 0) {
            // push new store value onto the stack
            dyn_array_put(stack[var], n->inputs[2]);
            kill = true;
        }
    }

    // check for any loads and replace them
    for (User* u = find_users(c->p, n); u; u = u->next) {
        TB_Node* use = u->n;

        if (use->type == TB_LOAD) {
            int var = get_variable_id(c, use->inputs[1]);
            if (var >= 0) {
                TB_Node* val;
                if (dyn_array_length(stack[var]) == 0) {
                    // this is UB since it implies we've read before initializing the
                    // stack slot.
                    val = c->poison;
                    log_warn("%p: found load-before-init in mem2reg, this is UB", use);
                } else {
                    val = stack[var][dyn_array_length(stack[var]) - 1];
                }

                set_input(c->p, use, NULL, 1); // unlink first
                subsume_node(c->p, c->f, use, val);
            }
        }
    }

    if (kill) {
        // log_info("%p: pass to %p", n, n->inputs[0]);
        TB_Node* into = n->inputs[0];
        set_input(c->p, n, NULL, 0);
        subsume_node(c->p, c->f, n, into);
    }
}

static void ssa_rename(Mem2Reg_Ctx* c, TB_Function* f, TB_Node* bb, DynArray(TB_Node*)* stack) {
    assert(bb);

    // push phi nodes
    size_t* old_len = tb_tls_push(c->tls, sizeof(size_t) * c->to_promote_count);
    FOREACH_N(var, 0, c->to_promote_count) {
        ptrdiff_t search = nl_map_get(c->defs[var], bb);
        if (search >= 0 && c->defs[var][search].v->type == TB_PHI) {
            dyn_array_put(stack[var], c->defs[var][search].v);
        }

        old_len[var] = dyn_array_length(stack[var]);
    }

    // rewrite operations
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
    TB_Node* end = r->end;

    // for through all uses and replace their accessors
    ssa_rename_node(c, end, stack);

    // replace phi arguments on successor
    if (end != NULL) {
        if (end->type == TB_NULL || end->type == TB_STOP || end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
            /* RET can't do shit in this context */
        } else if (end->type == TB_BRANCH) {
            TB_NodeBranch* br_info = TB_NODE_GET_EXTRA(end);
            FOREACH_N(i, 0, br_info->succ_count) {
                ssa_replace_phi_arg(c, f, bb, br_info->succ[i], stack);
            }
        } else {
            tb_todo();
        }
    }

    // for each successor s of the BB in the dominator
    //    rename(s)
    //
    // TODO(NeGate): maybe we want a data structure for this because it'll
    // be "kinda" slow.
    FOREACH_N(i, 0, c->order.count) {
        TB_Node* k = c->order.traversal[i];
        TB_Node* v = idom(k);

        if (v == bb && k != bb) {
            ssa_rename(c, f, k, stack);
        }
    }

    FOREACH_N(var, 0, c->to_promote_count) {
        dyn_array_set_length(stack[var], old_len[var]);
    }
    tb_tls_restore(c->tls, old_len);
}

typedef struct {
    TB_Node* old_n;

    int64_t offset;
    TB_CharUnits size;
    TB_DataType dt;
} AggregateConfig;

static ptrdiff_t find_config(size_t config_count, AggregateConfig* configs, int64_t offset) {
    FOREACH_N(i, 0, config_count) {
        if (configs[i].offset == offset) return i;
    }

    tb_unreachable();
    return -1;
}

// -1 is a bad match
// -2 is no match, so we can add a new config
static ptrdiff_t compatible_with_configs(size_t config_count, AggregateConfig* configs, int64_t offset, TB_CharUnits size, TB_DataType dt) {
    int64_t max = offset + size;

    FOREACH_N(i, 0, config_count) {
        int64_t max2 = configs[i].offset + configs[i].size;

        if (offset >= configs[i].offset && max <= max2) {
            // they overlap... but is it a clean overlap?
            if (offset == configs[i].offset && max == max2 && TB_DATA_TYPE_EQUALS(dt, configs[i].dt)) {
                return i;
            }

            return -1;
        }
    }

    return -2;
}

static void insert_phis(Mem2Reg_Ctx* restrict ctx, TB_Node* parent, TB_Node* n) {
    if (n->type != TB_REGION && n->type != TB_START && n->type != TB_PHI) {
        insert_phis(ctx, parent, n->inputs[0]);
    }

    if (n->type == TB_MEMSET) {
        int var = get_variable_id(ctx, n->inputs[1]);
        if (var >= 0) {
            // this stores the "primary type" of the specific address
            TB_DataType dt = ctx->to_promote[var]->dt;
            (void) dt;

            tb_todo();
            /* if (dt.type == TB_FLOAT && dt.data == TB_FLT_32) {
                f->nodes[r].type = TB_FLOAT32_CONST;
                f->nodes[r].dt = dt;
                f->nodes[r].flt32.value = 0.0;
            } else if (dt.type == TB_FLOAT && dt.data == TB_FLT_64) {
                f->nodes[r].type = TB_FLOAT64_CONST;
                f->nodes[r].dt = dt;
                f->nodes[r].flt64.value = 0.0;
            } else {
                f->nodes[r].type = TB_INTEGER_CONST;
                f->nodes[r].dt = dt;
                f->nodes[r].integer.num_words = 1;
                f->nodes[r].integer.single_word = 0;
            } */

            write_variable(ctx, var, parent, n);
        }
    } else if (n->type == TB_STORE) {
        int var = get_variable_id(ctx, n->inputs[1]);
        if (var >= 0) {
            write_variable(ctx, var, parent, n->inputs[2]);
        }
    }
}

// false means failure to SROA
static bool add_configs(TB_Passes* p, TB_TemporaryStorage* tls, User* use, TB_Node* base_address, size_t base_offset, size_t* config_count, AggregateConfig* configs, int pointer_size) {
    for (; use; use = use->next) {
        TB_Node* n = use->n;

        // we can only SROA if we know we're not using the
        // address for anything but direct memory ops or TB_MEMBERs.
        if (use->slot != 1) {
            return false;
        }

        if (n->type == TB_MEMBER_ACCESS) {
            // same rules, different offset
            int64_t offset = TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset;
            if (!add_configs(p, tls, find_users(p, n), base_address, base_offset + offset, config_count, configs, pointer_size)) {
                return false;
            }
            continue;
        }

        // find direct memory op
        if (n->type != TB_LOAD && n->type != TB_STORE) {
            return false;
        }

        TB_DataType dt = n->type == TB_LOAD ? n->dt : n->inputs[2]->dt;
        TB_Node* address = n->inputs[1];
        int size = (bits_in_data_type(pointer_size, dt) + 7) / 8;

        // see if it's a compatible configuration
        int match = compatible_with_configs(*config_count, configs, base_offset, size, dt);
        if (match == -1) {
            return false;
        } else if (match == -2) {
            // add new config
            tb_tls_push(tls, sizeof(AggregateConfig));
            configs[(*config_count)++] = (AggregateConfig){ address, base_offset, size, dt };
        } else if (configs[match].old_n != address) {
            log_warn("%s: %p SROA config matches but reaches so via a different node, please idealize nodes before mem2reg", p->f->super.name, address);
            return false;
        }
    }

    return true;
}

static bool attempt_sroa(TB_Passes* p, TB_Function* f, TB_TemporaryStorage* tls, TB_Node* address) {
    int pointer_size = tb__find_code_generator(f->super.module)->pointer_size;

    size_t config_count = 0;
    AggregateConfig* configs = tb_tls_push(tls, 0);
    if (!add_configs(p, tls, find_users(p, address), address, 0, &config_count, configs, pointer_size)) {
        return false;
    }

    uint32_t alignment = TB_NODE_GET_EXTRA_T(address, TB_NodeLocal)->align;
    FOREACH_N(i, 0, config_count) {
        TB_Node* new_n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 1, sizeof(TB_NodeLocal));
        TB_NODE_SET_EXTRA(new_n, TB_NodeLocal, .size = configs[i].size, .align = alignment);

        // replace old pointer with new fancy
        subsume_node(p, f, configs[i].old_n, new_n);
        dyn_array_put(p->locals, new_n);
    }

    tb_pass_kill_node(p, address);
    return true;
}

bool tb_pass_mem2reg(TB_Passes* p) {
    cuikperf_region_start("mem2reg", NULL);
    verify_tmp_arena(p);

    TB_Function* f = p->f;
    TB_TemporaryStorage* tls = tb_tls_steal();

    ////////////////////////////////
    // Try SROA first
    ////////////////////////////////
    for (size_t i = dyn_array_length(p->locals); i--;) retry: {
        TB_Node* address = p->locals[i];

        TB_DataType dt;
        Coherency coherence = tb_get_stack_slot_coherency(p, f, address, &dt);
        if (coherence == COHERENCY_USES_ADDRESS) {
            void* mark = tb_tls_push(tls, 0);
            if (attempt_sroa(p, f, tls, address)) {
                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p was able to SROA", f->super.name, address));
                tb_tls_restore(tls, mark);
                goto retry; // but don't go the next int
            } else {
                tb_tls_restore(tls, mark);
            }
        }
    }

    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    size_t to_promote_count = 0;
    TB_Node** to_promote = tb_tls_push(tls, sizeof(TB_Node*) * dyn_array_length(p->locals));

    dyn_array_for(i, p->locals) {
        TB_Node* n = p->locals[i];

        TB_DataType dt;
        Coherency coherence = tb_get_stack_slot_coherency(p, f, n, &dt);

        switch (coherence) {
            case COHERENCY_GOOD: {
                tb_tls_push(tls, sizeof(TB_Node*));
                to_promote[to_promote_count++] = n;

                n->dt = dt;

                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p promoting to IR register", f->super.name, n));
                break;
            }
            case COHERENCY_UNINITIALIZED: {
                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p could not mem2reg (uninitialized)", f->super.name, n));
                break;
            }
            case COHERENCY_VOLATILE: {
                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p could not mem2reg (volatile load/store)", f->super.name, n));
                break;
            }
            case COHERENCY_USES_ADDRESS: {
                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p could not mem2reg (uses address directly)", f->super.name));
                break;
            }
            case COHERENCY_BAD_DATA_TYPE: {
                DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%s: %p could not mem2reg (data type is too inconsistent)", f->super.name, n));
                break;
            }
            default: tb_todo();
        }
    }

    if (to_promote_count == 0) {
        // doesn't need to mem2reg
        goto no_changes;
    }

    Mem2Reg_Ctx c = { 0 };
    c.tls = tb_tls_steal();
    c.f = f;
    c.p = p;

    c.to_promote_count = to_promote_count;
    c.to_promote = to_promote;

    c.defs = tb_tls_push(c.tls, to_promote_count * sizeof(Mem2Reg_Def));
    memset(c.defs, 0, to_promote_count * sizeof(Mem2Reg_Def));

    c.order = p->order;

    TB_DominanceFrontiers df = tb_get_dominance_frontiers(f, &c.order);

    ////////////////////////////////
    // Phase 1: Insert phi functions
    ////////////////////////////////
    // Identify the final value of all the variables in the function per basic block
    FOREACH_REVERSE_N(i, 0, c.order.count) {
        TB_Node* end = TB_NODE_GET_EXTRA_T(c.order.traversal[i], TB_NodeRegion)->end;
        insert_phis(&c, c.order.traversal[i], end);
    }

    // for each global name we'll insert phi nodes
    TB_Node** phi_p = tb_tls_push(tls, c.order.count * sizeof(TB_Node*));

    NL_HashSet ever_worked = nl_hashset_alloc(c.order.count);
    NL_HashSet has_already = nl_hashset_alloc(c.order.count);
    FOREACH_N(var, 0, c.to_promote_count) {
        nl_hashset_clear(&ever_worked);
        nl_hashset_clear(&has_already);

        size_t p_count = 0;
        FOREACH_REVERSE_N(i, 0, c.order.count) {
            TB_Node* bb = c.order.traversal[i];

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

                ptrdiff_t search = nl_map_get(df, bb);
                if (search < 0) continue;

                TB_FrontierSet* frontier = &df[search].v;
                nl_hashset_for(it, frontier) {
                    TB_Node* l = *it;
                    if (!nl_hashset_put(&has_already, l)) continue;

                    search = nl_map_get(c.defs[var], l);

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
    tb_tls_restore(tls, phi_p);

    ////////////////////////////////
    // Phase 2: Rename loads and stores
    ////////////////////////////////
    DynArray(TB_Node*)* stack = tb_tls_push(tls, c.to_promote_count * sizeof(DynArray(TB_Node*)));
    FOREACH_N(var, 0, c.to_promote_count) {
        stack[var] = dyn_array_create(TB_Node*, 16);
    }

    ssa_rename(&c, f, f->start_node, stack);

    // don't need these anymore
    FOREACH_N(var, 0, c.to_promote_count) {
        tb_pass_kill_node(c.p, c.to_promote[var]);
    }

    // annoying nested hash map freeing
    nl_map_for(i, df) {
        nl_hashset_free(df[i].v);
    }
    nl_map_free(df);
    tb_tls_restore(tls, to_promote);

    cuikperf_region_end();
    return true;

    no_changes:
    cuikperf_region_end();
    return false;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and alignment along with not needing any address usage.
static Coherency tb_get_stack_slot_coherency(TB_Passes* p, TB_Function* f, TB_Node* address, TB_DataType* out_dt) {
    ICodeGen* cg = tb__find_code_generator(f->super.module);
    int pointer_size = cg->pointer_size;
    int char_size = cg->minimum_addressable_size;

    // pick the first load/store and use that as the baseline
    TB_DataType dt = TB_TYPE_VOID;
    bool initialized = false;
    int dt_bits = 0;

    for (User* use = find_users(p, address); use; use = use->next) {
        TB_Node* n = use->n;
        if (n->type == TB_MEMSET && n->inputs[1] == address && tb_node_is_constant_zero(n->inputs[2])) {
            TB_NodeInt* size = TB_NODE_GET_EXTRA(n->inputs[2]);

            if (n->inputs[3]->type == TB_INTEGER_CONST && size->num_words == 1) {
                // untyped zeroing store
                // we're hoping all data types match in size to continue along
                int bits = char_size * size->words[0];

                if (bits == 0 || (dt_bits > 0 && bits != dt_bits)) {
                    return COHERENCY_BAD_DATA_TYPE;
                }
                dt_bits = bits;
            }
        } else if (n->type == TB_READ || n->type == TB_WRITE) {
            return COHERENCY_VOLATILE;
        } else if ((n->type == TB_LOAD || n->type == TB_STORE) && n->inputs[1] == address) {
            TB_DataType mem_dt = n->type == TB_LOAD ? n->dt : n->inputs[2]->dt;
            if (!initialized) {
                dt = mem_dt;
                initialized = true;
            } else {
                // we're hoping all data types match in size to continue along
                int bits = bits_in_data_type(pointer_size, mem_dt);
                if (bits == 0 || (dt_bits > 0 && bits != dt_bits)) {
                    return COHERENCY_BAD_DATA_TYPE;
                }
                dt_bits = bits;
            }
        } else {
            DO_IF(TB_OPTDEBUG_MEM2REG)(log_debug("%p uses pointer arithmatic (%s)", address, tb_node_get_name(n)));
            return COHERENCY_USES_ADDRESS;
        }
    }

    if (!initialized) {
        return COHERENCY_UNINITIALIZED;
    }

    *out_dt = dt;
    return COHERENCY_GOOD;
}
