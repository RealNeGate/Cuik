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
    TB_OptQueue* restrict queue;

    TB_Node* poison;

    // Stack slots we're going to convert into
    // SSA form
    size_t to_promote_count;
    TB_Node** to_promote;

    // [to_promote_count]
    Mem2Reg_Def* defs;

    TB_PostorderWalk order;
    TB_Dominators doms;
} Mem2Reg_Ctx;

static int bits_in_data_type(int pointer_size, TB_DataType dt);
static Coherency tb_get_stack_slot_coherency(TB_OptQueue* queue, TB_Function* f, TB_Node* address, TB_DataType* dt, int* out_use_count);

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
    n->inputs[0] = block;
    FOREACH_N(i, 0, block->input_count) n->inputs[1 + i] = NULL;

    // append variable attrib
    const char* name = NULL;
    for (TB_Attrib* a = c->to_promote[var]->first_attrib; a; a = a->next) if (a->type == TB_ATTRIB_VARIABLE) {
        append_attrib(f, n, a);
        name = a->var.name;
        break;
    }

    if (name) {
        log_debug("%s: %p: insert new PHI node (in %p)", name, n, block);
    } else {
        log_debug("%p: insert new PHI node (in %p)", n, block);
    }
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
    log_debug("%p: adding %p to PHI", phi_node, node);

    // the slot to fill is based on the predecessor list of the region
    FOREACH_N(i, 0, phi_region->input_count) {
        TB_Node* pred = phi_region->inputs[i];
        while (pred->type != TB_REGION && pred->type != TB_START) pred = pred->inputs[0];

        if (pred == bb) {
            set_input(c->queue, phi_node, node, i+1);
            break;
        }
    }

    tb_optqueue_mark(c->queue, phi_node, true);
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
        FOREACH_N(j, 0, phi_reg->input_count) {
            if (dst->inputs[j] == bb) {
                // try to replace
                set_input(c->queue, phi_reg, top, j);
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
    for (User* u = find_users(c->queue, n); u; u = u->next) {
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

                tb_transmute_to_pass(c->queue, use, val);
                tb_optqueue_mark(c->queue, use, true);
            }
        }
    }

    if (kill) {
        log_info("%p: pass to %p", n, n->inputs[0]);
        log_info("  user %p", find_users(c->queue, n->inputs[0])->n);

        tb_transmute_to_pass(c->queue, n, n->inputs[0]);
        tb_optqueue_mark(c->queue, n, true);

        // tb_optqueue_kill(c->queue, n);
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
        if (end->type == TB_NULL || end->type == TB_RET || end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
            /* RET can't do shit in this context */
        } else if (end->type == TB_BRANCH) {
            FOREACH_N(i, 0, r->succ_count) {
                ssa_replace_phi_arg(c, f, bb, r->succ[i], stack);
            }
        } else {
            tb_todo();
        }
    }

    // for each successor s of the BB in the dominator
    //    rename(s)
    nl_map_for(i, c->doms) {
        if (c->doms[i].v == bb && c->doms[i].k != bb) {
            ssa_rename(c, f, c->doms[i].k, stack);
        }
    }

    FOREACH_N(var, 0, c->to_promote_count) {
        dyn_array_set_length(stack[var], old_len[var]);
    }
    tb_tls_restore(c->tls, old_len);
}

typedef struct {
    TB_Node* new_reg;

    int32_t offset;
    TB_CharUnits size;
    TB_DataType dt;
} AggregateConfig;

static ptrdiff_t find_config(size_t config_count, AggregateConfig* configs, int32_t offset) {
    FOREACH_N(i, 0, config_count) {
        if (configs[i].offset == offset) return i;
    }

    tb_unreachable();
    return -1;
}

// -1 is a bad match
// -2 is no match, so we can add a new config
static ptrdiff_t compatible_with_configs(size_t config_count, AggregateConfig* configs, int32_t offset, TB_CharUnits size, TB_DataType dt) {
    int32_t max = offset + size;

    FOREACH_N(i, 0, config_count) {
        int32_t max2 = configs[i].offset + configs[i].size;

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

#if 0
static bool attempt_sroa(TB_Function* f, TB_TemporaryStorage* tls, TB_Node* address, int use_count) {
    size_t config_count = 0;
    AggregateConfig* configs = tb_tls_push(tls, 0);

    int pointer_size = tb__find_code_generator(f->super.module)->pointer_size;

    int acceptable_use_count = 0;
    FOREACH_REVERSE_N(i, 0, ctx.order.count) {
        TB_Node* bb = ctx.order.traversal[i];
    }

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_MEMSET && n->inputs[0] == address) {
                // we can assume memset is valid since the COHERENCY_BAD_DATA_TYPE
                // wouldn't have let us get this far
                //
                // TODO(NeGate): handle this case correctly
                // acceptable_use_count += 1;
                return false;
            } else if (n->type == TB_LOAD || n->type == TB_STORE) {
                int size = (bits_in_data_type(pointer_size, n->dt) + 7) / 8;

                // we don't need to worry about volatile either since COHERENCY_VOLATILE
                // handled it earlier
                int32_t offset = 0;
                if (n->inputs[0] == address) {
                    offset = 0;
                } else if (n->inputs[0]->type == TB_MEMBER_ACCESS && n->inputs[0]->inputs[0] == address) {
                    offset = TB_NODE_GET_EXTRA_T(n->inputs[0], TB_NodeMember)->offset;
                } else {
                    continue;
                }

                // see if it's a compatible configuration
                int match = compatible_with_configs(config_count, configs, offset, size, n->dt);
                if (match == -1) {
                    return false;
                } else if (match == -2) {
                    // add new config
                    tb_tls_push(tls, sizeof(AggregateConfig));
                    configs[config_count++] = (AggregateConfig){ TB_NULL_REG, offset, size, n->dt };
                }
                acceptable_use_count += 1;
            }
        }
    }

    if (acceptable_use_count != use_count) {
        return false;
    }

    // split configurations
    TB_NodeLocal* l = TB_NODE_GET_EXTRA(address);
    uint32_t alignment = l->align;

    FOREACH_N(i, 0, config_count) {
        // we can assume that address is in the entry block since HoistLocals
        TB_Node* n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 0, sizeof(TB_NodeLocal));
        tb_insert_node(f, 0, address, n);
        TB_NODE_SET_EXTRA(n, TB_NodeLocal, .size = configs[i].size, .align = alignment);

        configs[i].new_reg = n;
    }

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_LOAD || n->type == TB_STORE) {
                if (n->inputs[0] == address) {
                    ptrdiff_t slot = find_config(config_count, configs, 0);

                    n->inputs[0] = configs[slot].new_reg;
                } else if (n->inputs[0]->type == TB_MEMBER_ACCESS && n->inputs[0]->inputs[0] == address) {
                    // replace the old member access with a clean stack slot
                    TB_NodeMember* member = TB_NODE_GET_EXTRA(n->inputs[0]);
                    ptrdiff_t slot = find_config(config_count, configs, member->offset);

                    tb_optqueue_kill(queue, n->inputs[0]);
                    n->inputs[0] = configs[slot].new_reg;
                } else {
                    continue;
                }
            }
        }
    }

    tb_optqueue_kill(queue, address);
    return true;
}
#endif

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

bool mem2reg(TB_Function* f, TB_OptQueue* queue) {
    TB_TemporaryStorage* tls = tb_tls_steal();

    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    size_t to_promote_count = 0;
    TB_Node** to_promote = tb_tls_push(tls, sizeof(TB_Node*) * dyn_array_length(queue->locals));

    int changes = 0;
    dyn_array_for(i, queue->locals) {
        TB_Node* n = queue->locals[i];

        TB_DataType dt;
        int use_count;
        Coherency coherence = tb_get_stack_slot_coherency(queue, f, n, &dt, &use_count);

        switch (coherence) {
            case COHERENCY_GOOD: {
                tb_tls_push(tls, sizeof(TB_Node*));
                to_promote[to_promote_count++] = n;

                n->dt = dt;

                log_debug("%p promoting to IR register", n);
                break;
            }
            case COHERENCY_UNINITIALIZED: {
                log_debug("%p could not mem2reg a stack slot (uninitialized)", n);
                break;
            }
            case COHERENCY_VOLATILE: {
                log_debug("%p could not mem2reg a stack slot (volatile load/store)", n);
                break;
            }
            case COHERENCY_USES_ADDRESS: {
                /*if (n->type == TB_LOCAL && attempt_sroa(f, tls, n, use_count)) {
                    log_debug("SROA on %p", n);
                    changes++;
                } else {
                }*/
                break;
            }
            case COHERENCY_BAD_DATA_TYPE: {
                log_debug("%p could not mem2reg (data type is too inconsistent)");
                break;
            }
            default: tb_todo();
        }
    }

    if (to_promote_count == 0) {
        // doesn't need to mem2reg
        return (changes != 0);
    }

    Mem2Reg_Ctx c = { 0 };
    c.tls = tb_tls_steal();
    c.f = f;
    c.queue = queue;

    c.to_promote_count = to_promote_count;
    c.to_promote = to_promote;

    c.defs = tb_tls_push(c.tls, to_promote_count * sizeof(Mem2Reg_Def));
    memset(c.defs, 0, to_promote_count * sizeof(Mem2Reg_Def));

    c.order = tb_function_get_postorder(f);

    // find dominators
    c.doms = tb_get_dominators(f);

    TB_DominanceFrontiers df = tb_get_dominance_frontiers(f, c.doms, &c.order);

    ////////////////////////////////
    // Phase 1: Insert phi functions
    ////////////////////////////////
    // Identify the final value of all the variables in the function per basic block
    FOREACH_REVERSE_N(i, 0, c.order.count) {
        TB_Node* end = TB_NODE_GET_EXTRA_T(c.order.traversal[i], TB_NodeRegion)->end;
        insert_phis(&c, c.order.traversal[i], end);
    }

    // for each global name we'll insert phi nodes
    TB_Node** phi_queue = tb_tls_push(tls, c.order.count * sizeof(TB_Node*));

    NL_HashSet ever_worked = nl_hashset_alloc(c.order.count);
    NL_HashSet has_already = nl_hashset_alloc(c.order.count);
    FOREACH_N(var, 0, c.to_promote_count) {
        nl_hashset_clear(&ever_worked);
        nl_hashset_clear(&has_already);

        size_t queue_count = 0;
        FOREACH_REVERSE_N(i, 0, c.order.count) {
            TB_Node* bb = c.order.traversal[i];

            ptrdiff_t search = nl_map_get(c.defs[var], bb);
            if (search >= 0) {
                nl_hashset_put(&ever_worked, bb);
                phi_queue[queue_count++] = bb;
            }
        }

        // it's a global name
        if (queue_count > 1) {
            // insert phi per dominance of the blocks it's defined in
            for (size_t i = 0; i < queue_count; i++) {
                TB_Node* bb = phi_queue[i];
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
                        phi_queue[queue_count++] = l;
                    }
                }
            }
        }
    }
    tb_tls_restore(tls, phi_queue);

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
        tb_optqueue_kill(c.queue, c.to_promote[var]);
    }

    // annoying nested hash map freeing
    nl_map_for(i, df) {
        nl_hashset_free(df[i].v);
    }
    nl_map_free(df);
    tb_tls_restore(tls, to_promote);

    return true;
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and alignment along with not needing any address usage.
static Coherency tb_get_stack_slot_coherency(TB_OptQueue* queue, TB_Function* f, TB_Node* address, TB_DataType* out_dt, int* out_use_count) {
    ICodeGen* cg = tb__find_code_generator(f->super.module);
    int pointer_size = cg->pointer_size;
    int char_size = cg->minimum_addressable_size;

    // pick the first load/store and use that as the baseline
    TB_DataType dt = TB_TYPE_VOID;
    bool initialized = false;
    int dt_bits = 0;

    int use_count = 0;
    for (User* use = find_users(queue, address); use; use = use->next) {
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
        } else if ((n->type == TB_LOAD || n->type == TB_STORE) && n->inputs[1] == address) {
            if (TB_NODE_GET_EXTRA_T(n, TB_NodeMemAccess)->is_volatile) {
                return COHERENCY_VOLATILE;
            } else {
                if (!initialized) {
                    dt = n->dt;
                    initialized = true;
                }

                // we're hoping all data types match in size to continue along
                int bits = bits_in_data_type(pointer_size, dt);
                if (bits == 0 || (dt_bits > 0 && bits != dt_bits)) {
                    return COHERENCY_BAD_DATA_TYPE;
                }
                dt_bits = bits;
            }
        } else {
            log_debug("%p uses pointer arithmatic (%s)", address, tb_node_get_name(n));
            return COHERENCY_USES_ADDRESS;
        }

        use_count += 1;
    }
    *out_use_count = use_count;

    if (!initialized) {
        return COHERENCY_UNINITIALIZED;
    }

    *out_dt = dt;
    return COHERENCY_GOOD;
}

TB_Pass tb_opt_mem2reg(void) {
    return (TB_Pass){ .name = "Mem2Reg", .func_run = mem2reg };
}
