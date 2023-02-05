// Based on Dominance Frontiers
//    https://www.ed.tus.ac.jp/j-mune/keio/m/ssa2.pdf
#include "../tb_internal.h"

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
    TB_Reg* to_promote;

    // [to_promote_count][bb_count]
    TB_Reg* current_def;

    TB_Predeccesors preds;
    // [bb_count]
    TB_Label* doms;
} Mem2Reg_Ctx;

static int bits_in_data_type(int pointer_size, TB_DataType dt);
static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Reg address, TB_DataType* dt, int* out_use_count);

static int get_variable_id(Mem2Reg_Ctx* restrict c, TB_Reg r) {
    // TODO(NeGate): Maybe we speed this up... maybe it doesn't matter :P
    FOREACH_N(i, 0, c->to_promote_count) {
        if (c->to_promote[i] == r) return (int)i;
    }

    return -1;
}

// This doesn't really generate a PHI node, it just produces a NULL node which will
// be mutated into a PHI node by the rest of the code.
static TB_Reg new_phi(Mem2Reg_Ctx* restrict c, TB_Function* f, int var, TB_Label block, TB_DataType dt) {
    TB_Reg r = f->bbs[block].start;

    // add as first BB node
    tb_function_reserve_nodes(f, 1);
    TB_Reg new_phi_reg = f->node_count++;
    f->nodes[new_phi_reg] = (TB_Node) { .type = TB_NULL, .dt = dt, .next = r };
    f->bbs[block].start = new_phi_reg;

    OPTIMIZER_LOG(new_phi_reg, "Insert new PHI node (in L%d)", block);
    return new_phi_reg;
}

static void add_phi_operand(Mem2Reg_Ctx* restrict c, TB_Function* f, TB_Reg phi_reg, TB_Label label, TB_Reg reg) {
    // we're using NULL nodes as the baseline PHI0
    if (phi_reg == reg) {
        return;
    }

    OPTIMIZER_LOG(phi_reg, "  adding r%d to PHI", reg);
    TB_DataType dt = f->nodes[reg].dt;

    TB_Node* phi_node = &f->nodes[phi_reg];
    phi_node->dt = dt;

    if (phi_node->type == TB_NULL) {
        phi_node->type = TB_PHI1;
        phi_node->phi2.inputs[0] = (TB_PhiInput){ label, reg };
        return;
    } else if (phi_node->type == TB_PASS) {
        TB_Label input_label = tb_find_label_from_reg(f, phi_node->pass.value);

        phi_node->type = TB_PHI2;
        phi_node->phi2.inputs[0] = (TB_PhiInput){ input_label, phi_node->pass.value };
        phi_node->phi2.inputs[1] = (TB_PhiInput){ label, reg };
    }

    int count = tb_node_get_phi_width(f, phi_reg);
    TB_PhiInput* inputs = tb_node_get_phi_inputs(f, phi_reg);

    if (count == 1) {
        phi_node->type = TB_PHI2;
        phi_node->phi2.inputs[0] = inputs[0];
        phi_node->phi2.inputs[1] = (TB_PhiInput){ label, reg };
    } else if (count == 2) {
        phi_node->type = TB_PHIN;

        TB_PhiInput* new_inputs = tb_platform_heap_alloc(3 * sizeof(TB_PhiInput));
        new_inputs[0] = inputs[0];
        new_inputs[1] = inputs[1];
        new_inputs[2] = (TB_PhiInput){ label, reg };

        phi_node->phi.count = 3;
        phi_node->phi.inputs = new_inputs;
    } else {
        size_t index = phi_node->phi.count++;
        phi_node->phi.inputs = tb_platform_heap_realloc(phi_node->phi.inputs, phi_node->phi.count * sizeof(TB_PhiInput));
        if (phi_node->phi.inputs == NULL) {
            tb_panic("add_phi_operand: Out of memory!");
        }

        phi_node->phi.inputs[index] = (TB_PhiInput) { label, reg };
    }
}

static void write_variable(Mem2Reg_Ctx* c, int var, TB_Label block, TB_Reg value) {
    c->current_def[(var * c->bb_count) + block] = value;
}

static void ssa_replace_phi_arg(Mem2Reg_Ctx* c, TB_Function* f, TB_Label bb, TB_Label dst, DynArray(TB_Reg)* stack) {
    FOREACH_N(var, 0, c->to_promote_count) {
        TB_Reg phi_reg = c->current_def[(var * f->bb_count) + dst];
        if (!tb_node_is_phi_node(f, phi_reg)) continue;

        int count = tb_node_get_phi_width(f, phi_reg);
        TB_PhiInput* inputs = tb_node_get_phi_inputs(f, phi_reg);
        TB_Reg top = stack[var][dyn_array_length(stack[var]) - 1];

        bool found = false;
        FOREACH_N(j, 0, count) {
            if (inputs[j].label == bb) {
                // try to replace
                inputs[j].val = top;
                found = true;
                break;
            }
        }

        if (!found) {
            add_phi_operand(c, f, phi_reg, bb, top);
        }
    }
}

static void ssa_rename(Mem2Reg_Ctx* c, TB_Function* f, TB_Label bb, DynArray(TB_Reg)* stack) {
    // push phi nodes
    size_t* old_len = tb_tls_push(c->tls, sizeof(size_t) * f->bb_count);
    FOREACH_N(var, 0, c->to_promote_count) {
        TB_Reg value = c->current_def[(var * f->bb_count) + bb];
        if (tb_node_is_phi_node(f, value)) {
            dyn_array_put(stack[var], value);
        }

        old_len[var] = dyn_array_length(stack[var]);
    }

    // rewrite operations
    TB_FOR_NODE(r, f, bb) {
        TB_Node* n = &f->nodes[r];

        if (n->type == TB_LOCAL) {
            int var = get_variable_id(c, r);
            if (var >= 0) {
                tb_kill_op(f, r);
            }
        } else if (n->type == TB_PARAM_ADDR) {
            int var = get_variable_id(c, r);
            if (var >= 0) {
                dyn_array_put(stack[var], n->param_addr.param);
                tb_kill_op(f, r);
            }
        } else if (n->type == TB_MEMSET) {
            int var = get_variable_id(c, n->mem_op.dst);
            if (var >= 0) {
                dyn_array_put(stack[var], r);
            }
        } else if (n->type == TB_STORE) {
            int var = get_variable_id(c, n->mem_op.dst);
            if (var >= 0) {
                dyn_array_put(stack[var], n->store.value);
                tb_kill_op(f, r);
            }
        } else if (n->type == TB_LOAD) {
            int var = get_variable_id(c, n->load.address);
            if (var >= 0) {
                assert(dyn_array_length(stack[var]) > 0);

                n->type = TB_PASS;
                n->pass.value = stack[var][dyn_array_length(stack[var]) - 1];
            }
        }
    }

    // replace phi arguments on successor
    TB_Node* end = &f->nodes[f->bbs[bb].end];
    if (end->type == TB_NULL || end->type == TB_RET || end->type == TB_TRAP || end->type == TB_UNREACHABLE) {
        /* RET can't do shit in this context */
    } else if (end->type == TB_GOTO) {
        ssa_replace_phi_arg(c, f, bb, end->goto_.label, stack);
    } else if (end->type == TB_IF) {
        ssa_replace_phi_arg(c, f, bb, end->if_.if_true, stack);
        ssa_replace_phi_arg(c, f, bb, end->if_.if_false, stack);
    } else if (end->type == TB_SWITCH) {
        // each entry takes up two slots in the VLA storage (i just put random crap in there like arguments for function calls)
        size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
        TB_SwitchEntry* entries = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start];

        ssa_replace_phi_arg(c, f, bb, end->switch_.default_label, stack);

        FOREACH_REVERSE_N(i, 0, entry_count) {
            ssa_replace_phi_arg(c, f, bb, entries[i].value, stack);
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
    TB_Reg new_reg;

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

static bool attempt_sroa(TB_Function* f, TB_TemporaryStorage* tls, TB_Reg address, int use_count) {
    size_t config_count = 0;
    AggregateConfig* configs = tb_tls_push(tls, 0);

    int pointer_size = tb__find_code_generator(f->super.module)->pointer_size;

    int acceptable_use_count = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_MEMSET && n->mem_op.dst == address) {
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
                if (n->load.address == address) {
                    offset = 0;
                } else if (f->nodes[n->load.address].type == TB_MEMBER_ACCESS &&
                    f->nodes[n->load.address].member_access.base == address) {
                    offset = f->nodes[n->load.address].member_access.offset;
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
    uint32_t alignment = f->nodes[address].local.alignment;

    FOREACH_N(i, 0, config_count) {
        // we can assume that address is in the entry block since HoistLocals
        TB_Reg split = tb_function_insert_after(f, 0, address);
        f->nodes[split].type = TB_LOCAL;
        f->nodes[split].dt = TB_TYPE_PTR;
        f->nodes[split].local.alignment = alignment;
        f->nodes[split].local.size = configs[i].size;

        configs[i].new_reg = split;
    }

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_LOAD || n->type == TB_STORE) {
                if (n->load.address == address) {
                    ptrdiff_t slot = find_config(config_count, configs, 0);

                    n->load.address = configs[slot].new_reg;
                } else if (f->nodes[n->load.address].type == TB_MEMBER_ACCESS &&
                    f->nodes[n->load.address].member_access.base == address) {
                    // replace the old member access with a clean stack slot
                    ptrdiff_t slot = find_config(config_count, configs, f->nodes[n->load.address].member_access.offset);

                    tb_murder_reg(f, n->load.address);
                    n->load.address = configs[slot].new_reg;
                } else {
                    continue;
                }
            }
        }
    }

    tb_murder_reg(f, address);
    return true;
}

// NOTE(NeGate): All locals were moved into the first basic block by
// opt_hoist_locals earlier
bool mem2reg(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    ////////////////////////////////
    // Decide which stack slots to promote
    ////////////////////////////////
    size_t to_promote_count = 0;
    TB_Reg* to_promote = tb_tls_push(tls, 0);

    int changes = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_LOCAL || n->type == TB_PARAM_ADDR) {
                TB_DataType dt;
                int use_count;
                Coherency coherence = tb_get_stack_slot_coherency(f, r, &dt, &use_count);

                switch (coherence) {
                    case COHERENCY_GOOD: {
                        *((TB_Reg*)tb_tls_push(tls, sizeof(TB_Reg))) = r;
                        to_promote_count++;

                        n->dt = dt;

                        OPTIMIZER_LOG(r, "promoting to IR register");
                        break;
                    }
                    case COHERENCY_UNINITIALIZED: {
                        OPTIMIZER_LOG(r, "could not mem2reg a stack slot (uninitialized)");
                        break;
                    }
                    case COHERENCY_VOLATILE: {
                        OPTIMIZER_LOG(r, "could not mem2reg a stack slot (volatile load/store)");
                        break;
                    }
                    case COHERENCY_USES_ADDRESS: {
                        if (n->type == TB_LOCAL && attempt_sroa(f, tls, r, use_count)) {
                            // tb_function_print(f, tb_default_print_callback, stdout, false);

                            OPTIMIZER_LOG(r, "SROA on stack structure");
                            changes++;
                        } else {
                            OPTIMIZER_LOG(r, "could not mem2reg a stack slot (uses pointer arithmatic)");
                        }
                        break;
                    }
                    case COHERENCY_BAD_DATA_TYPE: {
                        OPTIMIZER_LOG(r, "could not mem2reg a stack slot (data type is too inconsistent)");
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
    c.current_def = tb_tls_push(tls, to_promote_count * c.bb_count * sizeof(TB_Reg));
    memset(c.current_def, 0, to_promote_count * c.bb_count * sizeof(TB_Reg));

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
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            switch (n->type) {
                case TB_PARAM_ADDR: {
                    // Parameter stack slots map to parameter registers
                    // so we need to tell mem2reg about that.
                    int var = get_variable_id(&c, r);
                    if (var >= 0) {
                        write_variable(&c, var, bb, n->param_addr.param);
                    }
                    break;
                }
                case TB_MEMSET: {
                    int var = get_variable_id(&c, n->mem_op.dst);
                    if (var >= 0) {
                        // this stores the "primary type" of the specific address
                        TB_DataType dt = f->nodes[to_promote[var]].dt;

                        if (dt.type == TB_FLOAT && dt.data == TB_FLT_32) {
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
                        }

                        write_variable(&c, var, bb, r);
                    }
                    break;
                }
                case TB_STORE: {
                    int var = get_variable_id(&c, n->store.address);
                    if (var >= 0) {
                        write_variable(&c, var, bb, n->store.value);
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
            TB_Reg r = c.current_def[(var * f->bb_count) + bb];
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
                TB_Reg value = c.current_def[(var * f->bb_count) + bb];
                TB_DataType dt = f->nodes[value].dt;

                FOREACH_N(k, 0, df.count[bb]) {
                    TB_Label l = df._[bb][k];
                    if (!set_first_time(&has_already, l)) continue;

                    TB_Reg phi_reg = c.current_def[(var * f->bb_count) + l];
                    if (phi_reg == 0) {
                        phi_reg = new_phi(&c, f, var, l, dt);
                    } else if (!tb_node_is_phi_node(f, phi_reg)) {
                        TB_Reg old_reg = phi_reg;
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
    DynArray(TB_Reg)* stack = tb_tls_push(tls, c.to_promote_count * sizeof(DynArray(TB_Reg)));
    FOREACH_N(var, 0, c.to_promote_count) {
        stack[var] = dyn_array_create(TB_Reg, 16);
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
static Coherency tb_get_stack_slot_coherency(TB_Function* f, TB_Reg address, TB_DataType* out_dt, int* out_use_count) {
    // if there's a difference between the times we want the value and the
    // times we want the address, then some address calculations are being done
    // and thus we can't mem2reg
    int use_count = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            TB_FOR_INPUT_IN_NODE(it, f, n) {
                if (it.r == address) use_count += 1;
            }
        }
    }
    *out_use_count = use_count;

    int value_based_use_count = 0;
    int pointer_size = tb__find_code_generator(f->super.module)->pointer_size;

    // pick the first load/store and use that as the baseline
    TB_DataType dt = TB_TYPE_VOID;
    bool initialized = false;
    int dt_bits = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            static_assert(offsetof(TB_Node, load.address) == offsetof(TB_Node, store.address),
                "TB_Node::load.address == TB_Node::store.address");

            if (n->type == TB_MEMSET &&
                n->mem_op.dst == address &&
                f->nodes[n->mem_op.src].type == TB_INTEGER_CONST &&
                f->nodes[n->mem_op.src].integer.num_words == 1 &&
                f->nodes[n->mem_op.src].integer.single_word == 0 &&
                f->nodes[n->mem_op.size].type == TB_INTEGER_CONST &&
                f->nodes[n->mem_op.size].integer.num_words == 1) {
                // untyped zeroing store
                // we're hoping all data types match in size to continue along
                int bits = bits_in_data_type(pointer_size, f->nodes[n->mem_op.src].dt) *
                    f->nodes[n->mem_op.size].integer.single_word;

                if (bits == 0 || (dt_bits > 0 && bits != dt_bits)) {
                    return COHERENCY_BAD_DATA_TYPE;
                }
                dt_bits = bits;
                value_based_use_count += 1;
            } else if ((n->type == TB_LOAD || n->type == TB_STORE) && n->load.address == address) {
                value_based_use_count += 1;

                if (n->load.is_volatile) {
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

TB_API TB_Pass tb_opt_mem2reg(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "Mem2Reg",
        .func_run = mem2reg,
    };
}
