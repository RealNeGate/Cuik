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

typedef struct Mem2Reg_Ctx {
    TB_TemporaryStorage* tls;
    TB_Function* f;
    size_t bb_count;

    // Stack slots we're going to convert into
    // SSA form
    size_t  to_promote_count;
    TB_Node** to_promote;

    // [to_promote_count][bb_count]
    TB_Node** current_def;

    TB_Predeccesors preds;
    // [bb_count]
    TB_Label* doms;
} Mem2Reg_Ctx;

static int bits_in_data_type(int pointer_size, TB_DataType dt);
static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Node* address, TB_DataType* dt, int* out_use_count);

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Node* r) {
    // TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
    FOREACH_N(i, 0, c->to_promote_count) {
        if (c->to_promote[i] == r) return (int)i;
    }

    return -1;
}

// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Node* new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, int var, TB_Label block, TB_DataType dt) {
    size_t pred_count = c->preds.count[block];

    // phi nodes need to be allocated at the top of a BB
    TB_Node* n = tb_alloc_node(f, TB_PHI, dt, pred_count, sizeof(TB_NodePhi) + (pred_count * sizeof(TB_Label)));
    TB_NodePhi* phi = TB_NODE_GET_EXTRA(n);
    tb_insert_node(f, block, tb_node_get_first_insertion_point(f, block), n);

    // we're putting placeholder -1 here
    FOREACH_N(i, 0, pred_count) {
        phi->labels[i] = c->preds.preds[block][i];
    }

    OPTIMIZER_LOG(n, "Insert new PHI node (in L%d)", block);
    return n;
}

static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Node* phi_node, TB_Label label, TB_Node* node) {
    // we're using NULL nodes as the baseline PHI0
    if (phi_node == node) {
        return;
    }

    phi_node->dt = node->dt;
    if (phi_node->type == TB_POISON) {
        return;
    }

    assert(phi_node->type == TB_PHI);
    OPTIMIZER_LOG(phi_node, "  adding r%d to PHI", reg);

    TB_NodePhi* phi = TB_NODE_GET_EXTRA(phi_node);
    FOREACH_N(i, 0, phi_node->input_count) {
        if (phi->labels[i] == label) {
            phi_node->inputs[i] = node;
            return;
        }
    }

    // tb_unreachable();
}

static void write_variable(Mem2Reg_Ctx* c, int var, TB_Label block, TB_Node* value) {
    c->current_def[(var * c->bb_count) + block] = value;
}

static void ssa_replace_phi_arg(Mem2Reg_Ctx* c, TB_Function* f, TB_Label bb, TB_Label dst, DynArray(TB_Node*)* stack) {
    FOREACH_N(var, 0, c->to_promote_count) {
        TB_Node* phi_reg = c->current_def[(var * f->bb_count) + dst];
        if (phi_reg == NULL || phi_reg->type != TB_PHI) continue;

        TB_Node* top;
        if (dyn_array_length(stack[var]) == 0) {
            // this is UB land, insert poison
            TB_Node* basepoint = tb_node_get_first_insertion_point(f, 0);

            if (basepoint->type == TB_POISON) {
                // we probably already inserted a poison in this pass, nothing
                // says we can't reuse it.
                top = basepoint;
            } else {
                top = tb_alloc_node(f, TB_POISON, TB_TYPE_VOID, 0, 0);
                tb_insert_node(f, 0, basepoint, top);
            }
        } else {
            top = stack[var][dyn_array_length(stack[var]) - 1];
        }

        bool found = false;
        TB_NodePhi* phi = TB_NODE_GET_EXTRA(phi_reg);
        FOREACH_N(j, 0, phi_reg->input_count) {
            if (phi->labels[j] == bb) {
                // try to replace
                phi_reg->inputs[j] = top;
                found = true;
                break;
            }
        }

        if (!found) {
            add_phi_operand(c, f, phi_reg, bb, top);
        }
    }
}

static void ssa_rename(Mem2Reg_Ctx* c, TB_Function* f, TB_Label bb, DynArray(TB_Node*)* stack) {
    // push phi nodes
    size_t* old_len = tb_tls_push(c->tls, sizeof(size_t) * f->bb_count);
    FOREACH_N(var, 0, c->to_promote_count) {
        TB_Node* value = c->current_def[(var * f->bb_count) + bb];
        if (value && value->type == TB_PHI) {
            dyn_array_put(stack[var], value);
        }

        old_len[var] = dyn_array_length(stack[var]);
    }

    // rewrite operations
    TB_FOR_NODE(n, f, bb) {
        if (n->type == TB_LOCAL) {
            int var = get_variable_id(c, n);
            if (var >= 0) {
                TB_KILL_NODE(n);
            }
        } else if (n->type == TB_MEMSET) {
            int var = get_variable_id(c, n->inputs[0]);
            if (var >= 0) {
                dyn_array_put(stack[var], n);
            }
        } else if (n->type == TB_STORE) {
            int var = get_variable_id(c, n->inputs[0]);
            if (var >= 0) {
                dyn_array_put(stack[var], n->inputs[1]);
                TB_KILL_NODE(n);
            }
        } else if (n->type == TB_LOAD) {
            int var = get_variable_id(c, n->inputs[0]);
            if (var >= 0) {
                if (dyn_array_length(stack[var]) == 0) {
                    // this is UB since it implies we've read before initializing the
                    // stack slot.
                    tb_transmute_to_poison(n);
                } else {
                    tb_transmute_to_pass(n, stack[var][dyn_array_length(stack[var]) - 1]);
                }
            }
        }
    }

    // replace phi arguments on successor
    TB_Node* end = f->bbs[bb].end;
    if (end->type == TB_NULL || end->type == TB_RET || end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
        /* RET can't do shit in this context */
    } else if (end->type == TB_BRANCH) {
        TB_NodeBranch* br = TB_NODE_GET_EXTRA(end);
        ssa_replace_phi_arg(c, f, bb, br->default_label, stack);

        FOREACH_REVERSE_N(i, 0, br->count) {
            ssa_replace_phi_arg(c, f, bb, br->targets[i].value, stack);
        }
    } else {
        tb_todo();
    }

    // for each successor s of the BB in the dominator
    //    rename(s)
    FOREACH_N(s, 1, f->bb_count) {
        if (c->doms[s] == bb) {
            ssa_rename(c, f, s, stack);
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

static bool attempt_sroa(TB_Function* f, TB_TemporaryStorage* tls, TB_Node* address, int use_count) {
    size_t config_count = 0;
    AggregateConfig* configs = tb_tls_push(tls, 0);

    int pointer_size = tb__find_code_generator(f->super.module)->pointer_size;

    int acceptable_use_count = 0;
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

                    TB_KILL_NODE(n->inputs[0]);
                    n->inputs[0] = configs[slot].new_reg;
                } else {
                    continue;
                }
            }
        }
    }

    TB_KILL_NODE(address);
    return true;
}

// NOTE(NeGate): All locals were moved into the first basic block by
// opt_hoist_locals earlier
bool mem2reg(TB_Function* f, TB_TemporaryStorage* tls) {
    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    size_t to_promote_count = 0;
    TB_Node** to_promote = tb_tls_push(tls, 0);

    int changes = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_LOCAL) {
                TB_DataType dt;
                int use_count;
                Coherency coherence = tb_get_stack_slot_coherency(f, n, &dt, &use_count);

                switch (coherence) {
                    case COHERENCY_GOOD: {
                        *((TB_Node**)tb_tls_push(tls, sizeof(TB_Node*))) = n;
                        to_promote_count++;

                        n->dt = dt;

                        OPTIMIZER_LOG(n, "promoting to IR register");
                        break;
                    }
                    case COHERENCY_UNINITIALIZED: {
                        OPTIMIZER_LOG(n, "could not mem2reg a stack slot (uninitialized)");
                        break;
                    }
                    case COHERENCY_VOLATILE: {
                        OPTIMIZER_LOG(n, "could not mem2reg a stack slot (volatile load/store)");
                        break;
                    }
                    case COHERENCY_USES_ADDRESS: {
                        if (n->type == TB_LOCAL && attempt_sroa(f, tls, n, use_count)) {
                            OPTIMIZER_LOG(n, "SROA on stack structure");
                            changes++;
                        } else {
                            OPTIMIZER_LOG(n, "could not mem2reg a stack slot (uses pointer arithmatic)");
                        }
                        break;
                    }
                    case COHERENCY_BAD_DATA_TYPE: {
                        OPTIMIZER_LOG(n, "could not mem2reg a stack slot (data type is too inconsistent)");
                        break;
                    }
                    default: tb_todo();
                }
            }
        }
    }

    if (to_promote_count == 0) {
        // doesn't need to mem2reg
        return (changes != 0);
    }

    Mem2Reg_Ctx c = { 0 };
    c.tls = tls;
    c.f = f;

    c.to_promote_count = to_promote_count;
    c.to_promote = to_promote;

    c.bb_count = f->bb_count;
    c.current_def = tb_tls_push(tls, to_promote_count * c.bb_count * sizeof(TB_Node*));
    memset(c.current_def, 0, to_promote_count * c.bb_count * sizeof(TB_Node*));

    // Calculate all the immediate predecessors
    c.preds = tb_get_temp_predeccesors(f, tls);

    // find dominators
    c.doms = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    tb_get_dominators(f, c.preds, c.doms);

    TB_DominanceFrontiers df = tb_get_dominance_frontiers(f, c.preds, c.doms);

    ////////////////////////////////
    // Phase 1: Insert phi functions
    ////////////////////////////////
    // Identify the final value of all the variables in the function per basic block
    FOREACH_N(bb, 0, f->bb_count) {
        TB_FOR_NODE(n, f, bb) {
            switch (n->type) {
                case TB_MEMSET: {
                    int var = get_variable_id(&c, n->inputs[0]);
                    if (var >= 0) {
                        // this stores the "primary type" of the specific address
                        TB_DataType dt = to_promote[var]->dt;
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

                        write_variable(&c, var, bb, n);
                    }
                    break;
                }
                case TB_STORE: {
                    int var = get_variable_id(&c, n->inputs[0]);
                    if (var >= 0) {
                        write_variable(&c, var, bb, n->inputs[1]);
                    }
                    break;
                }
            }
        }
    }

    // for each global name we'll insert phi nodes
    size_t queue_count;
    TB_Label* queue = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    Set ever_worked = set_create(f->bb_count);
    Set has_already = set_create(f->bb_count);

    FOREACH_N(var, 0, c.to_promote_count) {
        set_clear(&ever_worked);
        set_clear(&has_already);
        queue_count = 0;

        FOREACH_N(bb, 0, f->bb_count) {
            TB_Node* r = c.current_def[(var * f->bb_count) + bb];
            if (r != 0) {
                set_put(&ever_worked, bb);
                queue[queue_count++] = bb;
            }
        }

        // it's a global name
        if (queue_count > 1) {
            // insert phi per dominance of the blocks it's defined in
            size_t i = 0;
            while (i < queue_count) {
                TB_Label bb = queue[i];
                TB_Node* value = c.current_def[(var * f->bb_count) + bb];
                TB_DataType dt = value->dt;

                FOREACH_N(k, 0, df.count[bb]) {
                    TB_Label l = df._[bb][k];
                    if (!set_first_time(&has_already, l)) continue;

                    TB_Node* phi_reg = c.current_def[(var * f->bb_count) + l];
                    if (phi_reg == NULL) {
                        phi_reg = new_phi(&c, f, var, l, dt);
                    } else if (phi_reg->type != TB_PHI) {
                        TB_Node* old_reg = phi_reg;
                        phi_reg = new_phi(&c, f, var, l, dt);
                        add_phi_operand(&c, f, phi_reg, l, old_reg);
                    }

                    c.current_def[(var * f->bb_count) + l] = phi_reg;
                    add_phi_operand(&c, f, phi_reg, bb, value);

                    if (set_first_time(&ever_worked, l)) {
                        queue[queue_count++] = l;
                    }
                }

                i += 1;
            }
        }
    }
    set_free(&ever_worked);
    set_free(&has_already);
    tb_tls_restore(tls, queue);

    ////////////////////////////////
    // Phase 2: Rename loads and stores
    ////////////////////////////////
    DynArray(TB_Node*)* stack = tb_tls_push(tls, c.to_promote_count * sizeof(DynArray(TB_Node*)));
    FOREACH_N(var, 0, c.to_promote_count) {
        stack[var] = dyn_array_create(TB_Node*, 16);
    }

    ssa_rename(&c, f, 0, stack);
    tb_free_dominance_frontiers(f, &df);

    return true;
}

static int bits_in_data_type(int pointer_size, TB_DataType dt) {
    switch (dt.type) {
        case TB_INT: return dt.data;
        case TB_PTR: return pointer_size;
        case TB_FLOAT:
        if (dt.data == TB_FLT_32) return 32;
        if (dt.data == TB_FLT_64) return 64;
        return 0;
        default: return 0;
    }
}

// NOTE(NeGate): a stack slot is coherent when all loads and stores share
// the same type and alignment along with not needing any address usage.
static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Node* address, TB_DataType* out_dt, int* out_use_count) {
    // if there's a difference between the times we want the value and the
    // times we want the address, then some address calculations are being done
    // and thus we can't mem2reg
    int use_count = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            TB_FOR_INPUT_IN_NODE(in, n) {
                if (*in == address) use_count += 1;
            }
        }
    }
    *out_use_count = use_count;

    int value_based_use_count = 0;

    ICodeGen* cg = tb__find_code_generator(f->super.module);
    int pointer_size = cg->pointer_size;
    int char_size = cg->minimum_addressable_size;

    // pick the first load/store and use that as the baseline
    TB_DataType dt = TB_TYPE_VOID;
    bool initialized = false;
    int dt_bits = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_MEMSET && n->inputs[0] == address && tb_node_is_constant_zero(n->inputs[1])) {
                TB_NodeInt* size = TB_NODE_GET_EXTRA(n->inputs[2]);

                if (n->inputs[2]->type == TB_INTEGER_CONST && size->num_words == 1) {
                    // untyped zeroing store
                    // we're hoping all data types match in size to continue along
                    int bits = char_size * size->words[0];

                    if (bits == 0 || (dt_bits > 0 && bits != dt_bits)) {
                        return COHERENCY_BAD_DATA_TYPE;
                    }
                    dt_bits = bits;
                    value_based_use_count += 1;
                }
            } else if ((n->type == TB_LOAD || n->type == TB_STORE) && n->inputs[0] == address) {
                value_based_use_count += 1;

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
            }
        }
    }

    if (value_based_use_count != use_count) {
        return COHERENCY_USES_ADDRESS;
    }

    if (!initialized) {
        return COHERENCY_UNINITIALIZED;
    }

    *out_dt = dt;
    return COHERENCY_GOOD;
}

const TB_Pass tb_opt_mem2reg = {
    .name = "Mem2Reg",
    .func_run = mem2reg,
};

