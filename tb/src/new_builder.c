typedef TB_Node* (*PeepFn)(TB_Function* f, TB_Node* n);

struct TB_GraphBuilder {
    TB_Function* f;
    TB_Arena* arena;

    PeepFn peep;

    // TB_SYMBOL_TABLE:
    //   we stick these to a node so peeps don't make us lose track of
    //   defs. when doing lazy phis, the defs might be NULL to signify
    //   we haven't referred to the def yet.
    //
    //   in[0]   bot_ctrl
    //   in[1]   top_ctrl
    //   in[2..] rest of the defs
    TB_Node* curr;
    TB_Node* start_syms;
};

static TB_Node* xfer_ctrl(TB_GraphBuilder* g, TB_Node* n) {
    TB_Node* old = g->curr->inputs[0];
    set_input(g->f, g->curr, n, 0);
    return old;
}

static TB_Node* peek_mem(TB_GraphBuilder* g, int mem_var) {
    return g->curr->inputs[2 + mem_var];
}

static TB_Node* xfer_mem(TB_GraphBuilder* g, TB_Node* n, int mem_var) {
    TB_Node* old = g->curr->inputs[2 + mem_var];
    TB_ASSERT(old->dt.type == TB_TAG_MEMORY);
    set_input(g->f, g->curr, n, 2 + mem_var);
    return old;
}

TB_GraphBuilder* tb_builder_enter(TB_Function* f, TB_Arena* arena1, TB_Arena* arena2, TB_ModuleSectionHandle section, TB_DebugType* dbg, TB_Worklist* ws) {
    TB_ASSERT_MSG(dbg->tag == TB_DEBUG_TYPE_FUNCTION, "type has to be a function");
    TB_ASSERT_MSG(dbg->func.return_count <= 1, "C can't do multiple returns and thus we can't lower it into C from here, try tb_function_set_prototype and do it manually");

    TB_GraphBuilder* g = tb_arena_alloc(arena2, sizeof(TB_GraphBuilder));
    *g = (TB_GraphBuilder){ .f = f, .arena = arena2 };
    g->peep = ws ? tb_opt_peep_node : tb_opt_gvn_node;

    f->arena     = arena1;
    f->tmp_arena = arena2;
    f->worklist  = ws;

    TB_ABI abi = f->super.module->target_abi;
    TB_FunctionPrototype* p = tb_prototype_from_dbg(f->super.module, dbg);

    // apply prototype
    tb_function_set_prototype(f, section, p);

    // both RPC and memory are mutable vars
    int def_count = 4 + (dbg ? dbg->func.param_count : f->param_count);
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, def_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, f->params[0], 0);
    set_input(f, syms, f->params[0], 1);
    set_input(f, syms, f->params[1], 2);
    set_input(f, syms, f->params[2], 3);
    g->curr = g->start_syms = syms;

    if (dbg) {
        size_t param_count = dbg->func.param_count;
        TB_DebugType** param_list = dbg->func.params;

        // reassemble values
        bool has_aggregate_return = dbg->func.return_count > 0 && classify_reg(abi, dbg->func.returns[0]) == RG_MEMORY;
        FOR_N(i, 0, param_count) {
            TB_DebugType* type = param_list[i]->field.type;
            const char* name = param_list[i]->field.name;
            size_t name_len = param_list[i]->field.len;

            int size = debug_type_size(abi, type);
            int align = debug_type_align(abi, type);

            // place values into memory
            TB_Node* v = tb_inst_param(f, i + has_aggregate_return);

            RegClass rg = classify_reg(abi, type);
            if (rg != RG_MEMORY) {
                TB_Node* slot = tb_builder_local(g, size, align);
                tb_builder_store(g, 0, slot, v, align);
                v = slot;
            }

            // mark debug info
            tb_function_attrib_variable(f, v, NULL, name_len, name, type);
            set_input(f, syms, v, 4 + i);
        }
    } else {
        FOR_N(i, 0, f->param_count) {
            set_input(f, syms, f->params[2 + i], 4 + i);
        }
    }
    return g;
}

void tb_builder_exit(TB_GraphBuilder* g) {
    if (g->start_syms != g->curr) {
        tb_builder_label_kill(g, g->start_syms);
    }

    if (g->curr) {
        tb_builder_label_kill(g, g->curr);
    }
}

TB_Node* tb_builder_local(TB_GraphBuilder* g, TB_CharUnits size, TB_CharUnits align) {
    assert(size > 0);
    assert(align > 0 && tb_is_power_of_two(align));

    // insert in the entry block
    TB_Node* n = tb_alloc_node(g->f, TB_LOCAL, TB_TYPE_PTR, 1, sizeof(TB_NodeLocal));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeLocal, .size = size, .align = align);
    return n;
}

TB_Node* tb_builder_bool(TB_GraphBuilder* g, bool x) {
    TB_Node* n = tb_alloc_node(g->f, TB_ICONST, TB_TYPE_BOOL, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_uint(TB_GraphBuilder* g, TB_DataType dt, uint64_t x) {
    TB_ASSERT(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));
    if (dt.type == TB_TAG_INT && dt.data < 64) {
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
        x &= mask;
    }

    TB_Node* n = tb_alloc_node(g->f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_sint(TB_GraphBuilder* g, TB_DataType dt, int64_t x) {
    TB_ASSERT(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));

    TB_Node* n = tb_alloc_node(g->f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_float32(TB_GraphBuilder* g, float imm) {
    TB_Node* n = tb_alloc_node(g->f, TB_F32CONST, TB_TYPE_F32, 1, sizeof(TB_NodeFloat32));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat32, .value = imm);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_float64(TB_GraphBuilder* g, double imm) {
    TB_Node* n = tb_alloc_node(g->f, TB_F64CONST, TB_TYPE_F64, 1, sizeof(TB_NodeFloat64));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat64, .value = imm);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_symbol(TB_GraphBuilder* g, TB_Symbol* sym) {
    TB_Node* n = tb_alloc_node(g->f, TB_SYMBOL, TB_TYPE_PTR, 1, sizeof(TB_NodeSymbol));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeSymbol, .sym = sym);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_string(TB_GraphBuilder* g, ptrdiff_t len, const char* str) {
    if (len < 0) len = strlen(str) + 1;

    TB_Function* f = g->f;
    TB_Global* dummy = tb_global_create(f->super.module, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
    tb_global_set_storage(f->super.module, tb_module_get_rdata(f->super.module), dummy, len, 1, 1);

    char* dst = tb_global_add_region(f->super.module, dummy, 0, len);
    memcpy(dst, str, len);

    return g->peep(f, tb_inst_get_symbol_address(f, (TB_Symbol*) dummy));
}

TB_Node* tb_builder_cast(TB_GraphBuilder* g, TB_DataType dt, int type, TB_Node* src) {
    TB_ASSERT(type >= TB_TRUNCATE && type <= TB_BITCAST);

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, dt, 2, 0);
    set_input(f, n, src, 1);
    return g->peep(f, n);
}

TB_Node* tb_builder_binop_int(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior ab) {
    TB_ASSERT_MSG(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");
    TB_ASSERT_MSG(a->dt.type == TB_TAG_INT, "datatype wasn't an integer");
    TB_ASSERT_MSG(type >= TB_AND && type <= TB_SMOD, "'type' wasn't an integer binop type (see TB_NodeTypeEnum)");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeBinopInt, .ab = ab);
    return g->peep(f, n);
}

TB_Node* tb_builder_binop_float(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b) {
    TB_ASSERT(type >= TB_FADD && type <= TB_FMAX);

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, 0);
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    return g->peep(f, n);
}

TB_Node* tb_builder_cmp(TB_GraphBuilder* g, int type, bool flip, TB_Node* a, TB_Node* b) {
    TB_ASSERT_MSG(type >= TB_CMP_EQ && type <= TB_CMP_FLE, "'type' wasn't a comparison node type (see TB_NodeTypeEnum)");
    TB_ASSERT_MSG(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
    if (flip) {
        set_input(f, n, b, 1);
        set_input(f, n, a, 2);
    } else {
        set_input(f, n, a, 1);
        set_input(f, n, b, 2);
    }
    TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->dt);
    return g->peep(f, n);
}

TB_Node* tb_builder_ptr_array(TB_GraphBuilder* g, TB_Node* base, TB_Node* index, int64_t stride) {
    TB_ASSERT_MSG(base->dt.type == TB_TAG_PTR,  "base on ARRAY must be an integer");
    TB_ASSERT_MSG(index->dt.type == TB_TAG_INT, "index on ARRAY must be an integer");

    TB_Function* f = g->f;
    TB_Node* con = tb_inst_sint(f, TB_TYPE_I64, stride);
    TB_Node* scl = index;
    if (stride != 1) {
        scl = tb_builder_binop_int(g, TB_MUL, index, con, 0);
    }

    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, scl,  2);
    return g->peep(f, n);
}

TB_Node* tb_builder_ptr_member(TB_GraphBuilder* g, TB_Node* base, int64_t offset) {
    if (offset == 0) {
        return base;
    }

    TB_Function* f = g->f;
    TB_Node* con = tb_opt_peep_node(f, tb_inst_sint(f, TB_TYPE_I64, offset));
    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, con,  2);
    return g->peep(f, n);
}

TB_Node* tb_builder_load(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_DataType dt, TB_Node* addr, TB_CharUnits align) {
    TB_Function* f = g->f;
    assert(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeMemAccess));
    if (ctrl_dep) {
        set_input(f, n, g->curr->inputs[0], 0);
    }
    set_input(f, n, peek_mem(g, mem_var), 1);
    set_input(f, n, addr, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
    return g->peep(f, n);
}

void tb_builder_store(TB_GraphBuilder* g, int mem_var, TB_Node* addr, TB_Node* val, TB_CharUnits align) {
    TB_Function* f = g->f;
    TB_ASSERT(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, xfer_mem(g, n, mem_var), 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
}

void tb_builder_memcpy(TB_GraphBuilder* g, int mem_var, TB_Node* dst, TB_Node* src, TB_Node* size, TB_CharUnits align) {
    TB_Function* f = g->f;
    TB_ASSERT(dst->dt.type == TB_TAG_PTR);
    TB_ASSERT(src->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_MEMCPY, TB_TYPE_MEMORY, 5, sizeof(TB_NodeMemAccess));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, xfer_mem(g, n, mem_var), 1);
    set_input(f, n, dst,  2);
    set_input(f, n, src,  3);
    set_input(f, n, size, 4);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
}

TB_Node* tb_builder_atomic_load(TB_GraphBuilder* g, int mem_var, TB_DataType dt, TB_MemoryOrder order, TB_Node* addr) {
    TB_Function* f = g->f;
    TB_ASSERT(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_LOAD, TB_TYPE_TUPLE, 3, sizeof(TB_NodeAtomic));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, addr, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 0);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);

    return tb__make_proj(f, dt, n, 1);
}

TB_Node* tb_builder_atomic_store(TB_GraphBuilder* g, int mem_var, TB_MemoryOrder order, TB_Node* addr, TB_Node* val) {
    TB_Function* f = g->f;
    TB_ASSERT(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_XCHG, TB_TYPE_TUPLE, 4, sizeof(TB_NodeAtomic));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 0);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);

    return tb__make_proj(f, val->dt, n, 1);
}

TB_Node* tb_builder_atomic_rmw(TB_GraphBuilder* g, int mem_var, int op, TB_MemoryOrder order, TB_Node* addr, TB_Node* val) {
    TB_Function* f = g->f;
    TB_ASSERT(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, op, TB_TYPE_TUPLE, 4, sizeof(TB_NodeAtomic));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, addr, 2);
    set_input(f, n, val,  3);
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 0);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);

    return tb__make_proj(f, val->dt, n, 1);
}

void tb_builder_debugbreak(TB_GraphBuilder* g) {
    TB_Node* n = tb_alloc_node(g->f, TB_DEBUGBREAK, TB_TYPE_CONTROL, 1, 0);
    set_input(g->f, n, xfer_ctrl(g, n), 0);
}

int tb_builder_decl(TB_GraphBuilder* g) {
    tb_todo();
    return 0;
}

TB_Node* tb_builder_get_var(TB_GraphBuilder* g, int id) {
    TB_ASSERT(2 + id < g->curr->input_count);

    TB_Node* curr = g->curr;
    TB_Node* r = curr->inputs[1];
    if (r->type == TB_NATURAL_LOOP) {
        TB_ASSERT(curr->inputs[2 + id]->type == TB_PHI && curr->inputs[2 + id]->inputs[0] == r);

        #if 0
        // we optimistically assume the defs need no phis until referenced
        // once that happens we'll just make one (lazy phis).
        if (curr->inputs[2 + id]->type != TB_PHI || curr->inputs[2 + id]->inputs[0] != r) {
            TB_Function* f = g->f;
            TB_Node* old = curr->inputs[2 + id];

            TB_Node* n = tb_alloc_node_dyn(f, TB_PHI, old->dt, 2, 3, 0);
            set_input(f, n, r,   0);
            set_input(f, n, old, 1);
            set_input(f, curr, n, 2 + id);
        }
        #endif
    }

    return curr->inputs[2 + id];
}

void tb_builder_set_var(TB_GraphBuilder* g, int id, TB_Node* src) {
    set_input(g->f, g->curr, src, 2 + id);
}

TB_Node* tb_builder_label_make(TB_GraphBuilder* g) {
    TB_Function* f = g->f;

    TB_Node* r = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 0, 2, sizeof(TB_NodeRegion));
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, g->curr->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, r, 0);
    set_input(f, syms, r, 1);
    return syms;
}

void tb_builder_label_kill(TB_GraphBuilder* g, TB_Node* label) {
    if (label->type != TB_NULL) {
        TB_ASSERT(label->type == TB_SYMBOL_TABLE);
        TB_ASSERT_MSG(label != g->curr, "can't kill the label we're using rn, that's rude");
        violent_kill(g->f, label);
    }
}

void tb_builder_label_complete(TB_GraphBuilder* g, TB_Node* label) {
    TB_ASSERT(label->type == TB_SYMBOL_TABLE);
    TB_NodeSymbolTable* extra = TB_NODE_GET_EXTRA(label);
    if (extra->complete) {
        return;
    }

    extra->complete = true;

    // once the label is completed we can optimize out some phis
    TB_Function* f = g->f;
    TB_Node* top_ctrl = label->inputs[1];

    if (top_ctrl->input_count != 0) {
        FOR_N(i, 2, label->input_count) {
            assert(label->inputs[i]);

            #if 1
            if (label->inputs[i]->type == TB_PHI) {
                TB_Node* k = identity_phi(f, label->inputs[i]);
                if (k != label->inputs[i]) {
                    subsume_node(f, label->inputs[i], k);
                }
            }
            #endif

            g->peep(f, label->inputs[i]);
        }
    }
}

TB_Node* tb_builder_label_get(TB_GraphBuilder* g) {
    return g->curr;
}

int tb_builder_label_pred_count(TB_GraphBuilder* g, TB_Node* label) {
    if (cfg_is_region(label->inputs[1])) {
        return label->inputs[1]->input_count;
    } else {
        return 1;
    }
}

void tb_builder_label_set(TB_GraphBuilder* g, TB_Node* label) {
    assert(label->type == TB_SYMBOL_TABLE);
    TB_NodeSymbolTable* extra = TB_NODE_GET_EXTRA(label);
    if (!extra->complete) {
        tb_builder_label_complete(g, label);
    }

    if (label->inputs[1]->input_count == 0) {
        g->curr = NULL;
    } else {
        g->curr = label;
    }
}

void tb_builder_if(TB_GraphBuilder* g, TB_Node* cond, TB_Node* paths[2]) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch));
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, cond, 1);

    TB_Node* cproj[2];
    cproj[0] = branch_cproj(f, n, 50, 0, 0);
    cproj[1] = branch_cproj(f, n, 50, 0, 1);

    TB_Node* curr = g->curr;
    g->curr = NULL;

    FOR_N(i, 0, 2) {
        TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, curr->input_count, sizeof(TB_NodeSymbolTable));
        set_input(f, syms, cproj[i], 0);
        set_input(f, syms, cproj[i], 1);
        TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);

        FOR_N(j, 2, curr->input_count) {
            set_input(f, syms, curr->inputs[j], j);
        }
        paths[i] = syms;
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->total_hits = 100;
    br->succ_count = 2;
}

void tb_builder_br(TB_GraphBuilder* g, TB_Node* target) {
    TB_Function* f = g->f;
    TB_Node* syms = g->curr;
    if (syms == NULL) {
        // we've already hit a terminator so let's just skip the branch
        return;
    }

    TB_ASSERT(target->type == TB_SYMBOL_TABLE);
    TB_ASSERT(syms->input_count == target->input_count);

    TB_Node* top_ctrl = target->inputs[1];
    bool is_loop = cfg_is_natural_loop(top_ctrl);

    FOR_N(i, 2, target->input_count) {
        TB_Node* target_def = target->inputs[i];
        if (target_def == NULL) {
            set_input(f, target, syms->inputs[i], i);
        } else if (target->inputs[i]->type == TB_PHI && target->inputs[i]->inputs[0] == top_ctrl) {
            // already phi-ified, keep appending to it
            add_input_late(f, target_def, syms->inputs[i]);
        } else if (is_loop || target_def != syms->inputs[i]) {
            // we didn't have a phi but finally found a reason for one
            TB_Node* n = tb_alloc_node(f, TB_PHI, target_def->dt, 2 + top_ctrl->input_count, 0);
            set_input(f, n, top_ctrl, 0);

            // once you place the phi, every previous path was matching def
            FOR_N(j, 0, top_ctrl->input_count) {
                set_input(f, n, target_def, 1+j);
            }
            set_input(f, n, syms->inputs[i], 1 + top_ctrl->input_count);
            set_input(f, target, n, i);
        }
    }
    add_input_late(f, top_ctrl, syms->inputs[0]);
    g->curr = NULL;
}

TB_Node* tb_builder_loop(TB_GraphBuilder* g) {
    TB_Function* f = g->f;
    TB_Node* curr = g->curr;

    TB_Node* r = tb_alloc_node_dyn(f, TB_NATURAL_LOOP, TB_TYPE_CONTROL, 1, 2, sizeof(TB_NodeRegion));
    set_input(f, r, curr->inputs[0], 0);

    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, curr->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, r, 0);
    set_input(f, syms, r, 1);

    // pre-emptively enter phis
    FOR_N(i, 2, curr->input_count) {
        TB_Node* n = tb_alloc_node_dyn(f, TB_PHI, curr->inputs[i]->dt, 2, 3, 0);
        set_input(f, n, r, 0);
        set_input(f, n, curr->inputs[i], 1);
        set_input(f, syms, n, i);
    }

    g->curr = syms;
    return syms;
}

TB_Node** tb_builder_call(TB_GraphBuilder* g, TB_FunctionPrototype* proto, int mem_var, TB_Node* target, int nargs, TB_Node** args) {
    TB_Function* f = g->f;

    size_t proj_count = 2 + (proto->return_count > 1 ? proto->return_count : 1);

    TB_Node* n = tb_alloc_node(f, TB_CALL, TB_TYPE_TUPLE, 3 + nargs, sizeof(TB_NodeCall));
    set_input(f, n, target, 2);
    FOR_N(i, 0, nargs) {
        set_input(f, n, args[i], i + 3);
    }

    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
    c->proj_count = proj_count;
    c->proto = proto;

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, xfer_ctrl(g, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);

    // create data projections
    TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(proto);
    TB_Node** rets_arr = tb_arena_alloc(g->arena, proto->return_count * sizeof(TB_Node*));

    FOR_N(i, 0, proto->return_count) {
        rets_arr[i] = tb__make_proj(f, rets[i].dt, n, i + 2);
    }

    add_input_late(f, get_callgraph(f), n);
    return rets_arr;
}

void tb_builder_ret(TB_GraphBuilder* g, int mem_var, int count, TB_Node** args) {
    TB_Function* f = g->f;

    assert(2 + mem_var < g->curr->input_count);
    TB_Node* mem_state = g->curr->inputs[2 + mem_var];

    // allocate return node
    TB_Node* ret = f->root_node->inputs[1];
    TB_ASSERT(ret->type == TB_RETURN);
    TB_Node* ctrl = ret->inputs[0];
    TB_ASSERT(ctrl->type == TB_REGION);

    // add to PHIs
    TB_ASSERT(ret->input_count >= 3 + count);
    add_input_late(f, ret->inputs[1], mem_state);

    size_t i = 3;
    for (; i < count + 3; i++) {
        TB_Node* v = args[i - 3];
        TB_ASSERT(ret->inputs[i]->dt.raw == v->dt.raw && "datatype mismatch");
        add_input_late(f, ret->inputs[i], v);
    }

    size_t phi_count = ret->input_count;
    for (; i < phi_count; i++) {
        // put poison in the leftovers?
        log_warn("%s: ir: generated poison due to inconsistent number of returned values", f->super.name);

        TB_Node* poison = tb_alloc_node(f, TB_POISON, ret->inputs[i]->dt, 1, 0);
        set_input(f, poison, ret, 0);

        poison = tb__gvn(f, poison, 0);
        add_input_late(f, ret->inputs[i], poison);
    }

    assert(g->curr->inputs[0] != NULL);
    add_input_late(f, ctrl, g->curr->inputs[0]);
    g->curr = NULL;
}

