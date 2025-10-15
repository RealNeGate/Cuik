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

    size_t param_count;
    TB_Node** params;
};

// From the optimizer
static TB_Node* make_int_node(TB_Function* f, TB_DataType dt, uint64_t x);
static TB_Node* identity_phi(TB_Function* f, TB_Node* n);

static TB_Node* get_callgraph(TB_Function* f) { return f->root_node->inputs[0]; }

static uint64_t tb_sign_ext(TB_Module* m, TB_DataType dt, uint64_t imm) {
    int bits = tb_data_type_bit_size(m, dt.type);
    return tb__sxt(imm, bits, 64);
}

TB_Node* tb_alloc_node_dyn(TB_Function* f, int type, TB_DataType dt, int input_count, int input_cap, size_t extra) {
    assert(input_count < UINT16_MAX && "too many inputs!");
    TB_Node* n = tb_arena_alloc(&f->arena, sizeof(TB_Node) + extra);

    n->type = type;
    n->input_cap = input_cap;
    n->input_count = input_count;
    n->dt = dt;
    n->gvn = f->node_count++;

    if (input_cap > 0) {
        n->inputs = tb_arena_alloc(&f->arena, input_cap * sizeof(TB_Node*));
        FOR_N(i, 0, input_cap) { n->inputs[i] = NULL; }
    } else {
        n->inputs = NULL;
    }

    // most nodes don't have many users, although the ones which do will have a shit load (root node)
    n->user_count = 0;
    n->user_cap   = 4;
    n->users = tb_arena_alloc(&f->arena, 4 * sizeof(TB_User));
    memset(n->users, 0xF0, 4 * sizeof(TB_User));

    if (extra > 0) {
        memset(n->extra, 0, extra);
    }
    return n;
}

TB_Node* tb_alloc_node(TB_Function* f, int type, TB_DataType dt, int input_count, size_t extra) {
    return tb_alloc_node_dyn(f, type, dt, input_count, input_count, extra);
}

TB_Node* tb__make_proj(TB_Function* f, TB_DataType dt, TB_Node* src, int index) {
    assert(src->dt.type == TB_TAG_TUPLE);
    TB_Node* proj = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(f, proj, src, 0);
    TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = index);
    return proj;
}

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

static TB_GraphBuilder* builder_enter_raw(TB_Function* f, TB_ModuleSectionHandle section, TB_DebugType* dbg, TB_FunctionPrototype* p, TB_Worklist* ws) {
    TB_ABI abi = f->super.module->target_abi;
    if (p == NULL) {
        p = tb_prototype_from_dbg(f->super.module, dbg);
    }

    // apply prototype
    f->worklist = ws;
    tb_function_set_prototype(f, section, p);

    TB_GraphBuilder* g = tb_arena_alloc(&f->tmp_arena, sizeof(TB_GraphBuilder));
    *g = (TB_GraphBuilder){ .f = f, .arena = &f->tmp_arena };
    g->peep = ws ? tb_opt_peep_node : tb_opt_gvn_node;

    bool has_aggregate_return = dbg && dbg->func.return_count > 0 && classify_reg(abi, dbg->func.returns[0]) == RG_MEMORY;

    // both RPC and memory are mutable vars
    int def_count = 3 + (dbg ? has_aggregate_return : f->param_count);
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, def_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, f->params[0], 0);
    set_input(f, syms, f->params[0], 1);
    set_input(f, syms, f->params[1], 2);
    g->curr = g->start_syms = syms;

    TB_NodeSymbolTable* extra = TB_NODE_GET_EXTRA(syms);
    extra->complete = true;

    if (dbg) {
        size_t param_count = dbg->func.param_count;
        TB_DebugType** param_list = dbg->func.params;

        if (param_count) {
            g->params = tb_arena_alloc(&f->tmp_arena, param_count * sizeof(TB_Node*));
            g->param_count = param_count;
        }

        // reassemble values
        if (has_aggregate_return) {
            set_input(f, syms, f->params[3], 3);
        }

        FOR_N(i, 0, param_count) {
            TB_DebugType* type = param_list[i]->field.type;
            const char* name = param_list[i]->field.name;
            size_t name_len = param_list[i]->field.len;

            int size = debug_type_size(abi, type);
            int align = debug_type_align(abi, type);

            // place values into memory
            TB_Node* v = f->params[f, 3 + i + has_aggregate_return];

            RegClass rg = classify_reg(abi, type);
            if (rg != RG_MEMORY) {
                TB_Node* slot = tb_builder_local(g, size, align);
                tb_builder_store(g, 0, false, slot, v, align, false);
                v = slot;
            }

            // mark debug info
            tb_function_attrib_variable(f, v, NULL, name_len, name, type);
            g->params[i] = v;
        }
    } else {
        FOR_N(i, 0, f->param_count) {
            set_input(f, syms, f->params[3 + i], 3 + i);
        }
    }
    return g;
}

void tb_function_attrib_variable(TB_Function* f, TB_Node* n, TB_Node* parent, ptrdiff_t len, const char* name, TB_DebugType* type) {
    TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
    l->name = tb__arena_strdup(f->super.module, len, name);
    l->type = type;
}

TB_Node* tb_builder_param_addr(TB_GraphBuilder* g, int i) {
    return g->params[i];
}

void tb_function_set_features(TB_Function* f, const TB_FeatureSet* features) {
    f->features = *features;
}

TB_GraphBuilder* tb_builder_enter(TB_Function* f, TB_ModuleSectionHandle section, TB_FunctionPrototype* proto, TB_Worklist* ws) {
    return builder_enter_raw(f, section, NULL, proto, ws);
}

TB_GraphBuilder* tb_builder_enter_from_dbg(TB_Function* f, TB_ModuleSectionHandle section, TB_DebugType* dbg, TB_Worklist* ws) {
    TB_ASSERT_MSG(dbg->tag == TB_DEBUG_TYPE_FUNCTION, "type has to be a function");
    TB_ASSERT_MSG(dbg->func.return_count <= 1, "C can't do multiple returns and thus we can't lower it into C from here, try tb_function_set_prototype and do it manually");
    return builder_enter_raw(f, section, dbg, NULL, ws);
}

void tb_builder_exit(TB_GraphBuilder* g) {
    if (g->curr) {
        TB_Node* n = g->curr;
        g->curr = NULL;

        tb_builder_label_kill(g, n);
    }

    if (g->start_syms != g->curr) {
        tb_builder_label_kill(g, g->start_syms);
    }

    // if the return block was never used, throw it away
    TB_Function* f = g->f;
    TB_Node* ret = f->root_node->inputs[1];
    if (ret->type == TB_RETURN && ret->inputs[0]->type == TB_REGION && ret->inputs[0]->input_count == 0) {
        tb_kill_node(f, ret->inputs[0]);

        int last = f->root_node->input_count - 1;
        TB_Node* last_n = f->root_node->inputs[last];
        TB_ASSERT(last != 1); // can't have no termination

        set_input(f, f->root_node, NULL, last);
        set_input(f, f->root_node, last_n, 1);
        f->root_node->input_count--;

        tb_kill_node(f, ret);
    }

    // needs to be empty for the optimizer not to act up
    tb_arena_clear(g->arena);

    if (f->worklist) {
        worklist_clear(f->worklist);
        tb_opt(f, f->worklist, false);
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

void tb_builder_local_dbg(TB_GraphBuilder* g, TB_Node* n, ptrdiff_t len, const char* name, TB_DebugType* type) {
    TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
    l->name = tb__arena_strdup(g->f->super.module, len, name);
    l->type = type;
}

TB_Node* tb_builder_bool(TB_GraphBuilder* g, bool x) {
    TB_Node* n = tb_alloc_node(g->f, TB_ICONST, TB_TYPE_BOOL, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_uint(TB_GraphBuilder* g, TB_DataType dt, uint64_t x) {
    TB_ASSERT(TB_IS_INT_OR_PTR(dt) || dt.type == TB_TAG_BOOL);
    int bits = tb_data_type_bit_size(g->f->super.module, dt.type);
    if (bits < 64) {
        uint64_t mask = ~UINT64_C(0) >> (64 - bits);
        x &= mask;
    }

    TB_Node* n = tb_alloc_node(g->f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_sint(TB_GraphBuilder* g, TB_DataType dt, int64_t x) {
    TB_ASSERT(TB_IS_INT_OR_PTR(dt) || dt.type == TB_TAG_BOOL);

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

    return g->peep(f, tb_builder_symbol(g, (TB_Symbol*) dummy));
}

TB_Node* tb_builder_cast(TB_GraphBuilder* g, TB_DataType dt, int type, TB_Node* src) {
    TB_ASSERT(type >= TB_TRUNCATE && type <= TB_BITCAST);

    if (type == TB_ZERO_EXT && src->type == TB_ICONST) {
        int bits = tb_data_type_bit_size(g->f->super.module, src->dt.type);

        uint64_t y = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        return tb_builder_uint(g, dt, y & (~UINT64_C(0) >> (64 - bits)));
    } else if (type == TB_SIGN_EXT && src->type == TB_ICONST) {
        uint64_t y = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        y = tb_sign_ext(g->f->super.module, src->dt, y);
        return tb_builder_uint(g, dt, y);
    }

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, dt, 2, 0);
    set_input(f, n, src, 1);
    return g->peep(f, n);
}

TB_Node* tb_builder_binop_int(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior ab) {
    TB_ASSERT_MSG(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");
    TB_ASSERT_MSG(TB_IS_INTEGER_TYPE(a->dt), "datatype wasn't an integer");
    TB_ASSERT_MSG(type >= TB_AND && type <= TB_SMOD, "'type' wasn't an integer binop type (see TB_NodeTypeEnum)");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeBinopInt, .ab = ab);
    return g->peep(f, n);
}

TB_Node* tb_builder_binop_float(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b) {
    TB_ASSERT_MSG(type >= TB_FADD && type <= TB_FMAX, "type wasn't a float op");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, 0);
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    return g->peep(f, n);
}

TB_Node* tb_builder_select(TB_GraphBuilder* g, TB_Node* cond, TB_Node* a, TB_Node* b) {
    TB_ASSERT_MSG(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_SELECT, a->dt, 4, 0);
    set_input(f, n, cond, 1);
    set_input(f, n, a, 2);
    set_input(f, n, b, 3);
    return g->peep(f, n);
}

TB_Node* tb_builder_va_start(TB_GraphBuilder* g, TB_Node* src) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_VA_START, TB_TYPE_PTR, 2, 0);
    set_input(f, n, f->root_node, 0);
    set_input(f, n, src, 1);
    return g->peep(f, n);
}

TB_Node* tb_builder_unary(TB_GraphBuilder* g, int type, TB_Node* src) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, src->dt, 2, 0);
    set_input(f, n, src, 1);
    return g->peep(f, n);
}

TB_Node* tb_builder_not(TB_GraphBuilder* g, TB_Node* src) {
    return tb_builder_binop_int(g, TB_XOR, src, tb_builder_sint(g, src->dt, -1), 0);
}

TB_Node* tb_builder_neg(TB_GraphBuilder* g, TB_Node* src) {
    if (TB_IS_FLOAT_TYPE(src->dt)) {
        return tb_builder_unary(g, TB_FNEG, src);
    } else {
        return tb_builder_binop_int(g, TB_SUB, tb_builder_sint(g, src->dt, 0), src, 0);
    }
}

TB_Node* tb_builder_cmp(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b) {
    TB_ASSERT_MSG(type >= TB_CMP_EQ && type <= TB_CMP_FLE, "'type' wasn't a comparison node type (see TB_NodeTypeEnum)");
    TB_ASSERT_MSG(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->dt);
    return g->peep(f, n);
}

TB_Node* tb_builder_ptr_array(TB_GraphBuilder* g, TB_Node* base, TB_Node* index, int64_t stride) {
    TB_ASSERT_MSG(base->dt.type == TB_TAG_PTR,   "base on ARRAY must be an integer");
    TB_ASSERT_MSG(TB_IS_INTEGER_TYPE(index->dt), "index on ARRAY must be an integer");

    TB_Function* f = g->f;
    TB_Node* con = tb_builder_sint(g, TB_TYPE_I64, stride);
    TB_Node* scl = index;
    if (stride != 1) {
        scl = tb_builder_binop_int(g, TB_MUL, index, con, 0);
    }

    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, base->dt, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, scl,  2);
    return g->peep(f, n);
}

TB_Node* tb_builder_frame_ptr(TB_GraphBuilder* g) {
    TB_Node* n = tb_alloc_node(g->f, TB_FRAME_PTR, TB_TYPE_PTR, 1, 0);
    set_input(g->f, n, g->f->root_node, 0);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_jit_thread_ptr(TB_GraphBuilder* g) {
    TB_Node* n = tb_alloc_node(g->f, TB_MACH_JIT_THREAD_PTR, TB_TYPE_PTR, 1, 0);
    set_input(g->f, n, g->f->root_node, 0);
    return g->peep(g->f, n);
}

TB_Node* tb_builder_ptr_member(TB_GraphBuilder* g, TB_Node* base, int64_t offset) {
    if (offset == 0) {
        return base;
    }

    TB_Function* f = g->f;
    TB_Node* con = make_int_node(f, TB_TYPE_I64, offset);
    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, base->dt, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, con,  2);
    return g->peep(f, n);
}

int tb_builder_split_mem(TB_GraphBuilder* g, int in_mem, int split_count, TB_Node** out_split) {
    TB_Function* f = g->f;

    TB_Node* n = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_MEMORY, 2, 0);
    set_input(f, n, g->curr->inputs[2 + in_mem], 1);
    *out_split = n;

    int id = g->curr->input_count - 2;
    FOR_N(i, 0, split_count) {
        add_input_late(g->f, g->curr, n);
    }
    set_input(f, g->curr, n, 2 + in_mem);

    return id;
}

void tb_builder_merge_mem(TB_GraphBuilder* g, int out_mem, int split_count, int split_vars, TB_Node* split) {
    TB_Function* f = g->f;

    TB_Node* n = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 3 + split_count, 0);
    set_input(f, n, split, 1);
    set_input(f, n, g->curr->inputs[2 + out_mem], 2);
    FOR_N(i, 0, split_count) {
        set_input(f, n, g->curr->inputs[2 + split_vars + i], 3 + i);
    }
    set_input(f, g->curr, n, 2 + out_mem);
}

TB_Node* tb_builder_load(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_DataType dt, TB_Node* addr, TB_CharUnits align, bool is_volatile) {
    TB_Function* f = g->f;
    assert(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeMemAccess));
    if (ctrl_dep) {
        set_input(f, n, g->curr->inputs[0], 0);
    }
    set_input(f, n, peek_mem(g, mem_var), 1);
    set_input(f, n, addr, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align, .is_volatile = is_volatile);
    n = g->peep(f, n);

    if (is_volatile) {
        // volatile operations just have a memory barrier right after them, this will stop the
        // load from being expunged and force an ordering to the loads.
        TB_Node* bar = tb_alloc_node(f, TB_HARD_BARRIER, TB_TYPE_MEMORY, 3, 0);
        set_input(f, bar, g->curr->inputs[0], 0);
        set_input(f, bar, xfer_mem(g, bar, mem_var), 1);
        set_input(f, bar, n, 2);
    }

    return n;
}

TB_Node* tb_builder_store(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* addr, TB_Node* val, TB_CharUnits align, bool is_volatile) {
    TB_Function* f = g->f;
    TB_ASSERT(addr->dt.type == TB_TAG_PTR);

    TB_Node* n = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, xfer_mem(g, n, mem_var), 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align, .is_volatile = is_volatile);

    if (is_volatile) {
        // volatile operations just have a memory barrier right after them, this will stop the
        // load from being expunged and force an ordering to the loads.
        TB_Node* bar = tb_alloc_node(f, TB_HARD_BARRIER, TB_TYPE_MEMORY, 2, 0);
        set_input(f, bar, g->curr->inputs[0], 0);
        set_input(f, bar, xfer_mem(g, bar, mem_var), 1);
    }
    return n;
}

void tb_builder_memcpy(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* src, TB_Node* size, TB_CharUnits align, bool is_volatile) {
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

void tb_builder_memset(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* val, TB_Node* size, TB_CharUnits align, bool is_volatile) {
    TB_Function* f = g->f;
    TB_ASSERT(dst->dt.type == TB_TAG_PTR);
    TB_ASSERT_MSG(val->dt.type == TB_TAG_I8, "memset's val needs to be byte sized");

    TB_Node* n = tb_alloc_node(f, TB_MEMSET, TB_TYPE_MEMORY, 5, sizeof(TB_NodeMemAccess));
    set_input(f, n, g->curr->inputs[0], 0);
    set_input(f, n, xfer_mem(g, n, mem_var), 1);
    set_input(f, n, dst,  2);
    set_input(f, n, val,  3);
    set_input(f, n, size, 4);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
}

void tb_builder_memzero(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* size, TB_CharUnits align, bool is_volatile) {
    tb_builder_memset(g, mem_var, ctrl_dep, dst, tb_builder_uint(g, TB_TYPE_I8, 0), size, align, is_volatile);
}

TB_Node* tb_builder_atomic_load(TB_GraphBuilder* g, int mem_var, TB_DataType dt, TB_Node* addr, TB_MemoryOrder order) {
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

TB_Node* tb_builder_atomic_store(TB_GraphBuilder* g, int mem_var, TB_Node* addr, TB_Node* val, TB_MemoryOrder order) {
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

TB_Node* tb_builder_atomic_rmw(TB_GraphBuilder* g, int mem_var, int op, TB_Node* addr, TB_Node* val, TB_MemoryOrder order) {
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

int tb_builder_decl(TB_GraphBuilder* g, TB_Node* label) {
    TB_ASSERT(label->type == TB_SYMBOL_TABLE);
    int id = label->input_count - 2;
    add_input_late(g->f, label, NULL);
    return id;
}

TB_Node* tb_builder_get_var(TB_GraphBuilder* g, int id) {
    TB_ASSERT(2 + id < g->curr->input_count);

    TB_Node* curr = g->curr;
    return curr->inputs[2 + id];
}

void tb_builder_set_var(TB_GraphBuilder* g, int id, TB_Node* src) {
    TB_NodeSymbolTable* extra = TB_NODE_GET_EXTRA(g->curr);
    TB_ASSERT(extra->complete);
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

TB_Node* tb_builder_label_make2(TB_GraphBuilder* g, TB_Node* label, bool has_backward_jumps) {
    TB_Function* f = g->f;

    TB_Node* r = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 0, 2, sizeof(TB_NodeRegion));
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, label->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, r, 0);
    set_input(f, syms, r, 1);

    if (has_backward_jumps) {
        // pre-emptively enter phis
        FOR_N(i, 2, label->input_count) {
            TB_Node* n = tb_alloc_node_dyn(f, TB_PHI, label->inputs[i]->dt, 1, 3, 0);
            set_input(f, n, r, 0);
            set_input(f, syms, n, i);
        }
        TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);
    }

    return syms;
}

void tb_builder_label_kill(TB_GraphBuilder* g, TB_Node* label) {
    if (label->type != TB_NULL) {
        TB_ASSERT(label->type == TB_SYMBOL_TABLE);
        TB_ASSERT_MSG(label != g->curr, "can't kill the label we're using rn, that's rude");
        tb_violent_kill(g->f, label);
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

    if (cfg_is_region(top_ctrl)) {
        FOR_USERS(u, top_ctrl) {
            TB_Node* un = USERN(u);
            if (un->type == TB_PHI) {
                TB_ASSERT(USERI(u) == 0);
                TB_Node* k = identity_phi(f, un);
                if (k != un) { subsume_node(f, un, k); }
            }
        }
    }

    if (top_ctrl->input_count != 0) {
        FOR_N(i, 2, label->input_count) {
            if (label->inputs[i] != NULL) {
                g->peep(f, label->inputs[i]);
            }
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

TB_Node* tb_builder_label_set(TB_GraphBuilder* g, TB_Node* label) {
    TB_Node* old = g->curr;
    if (label == NULL) {
        g->curr = NULL;
        return old;
    }

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
    return old;
}

TB_Node* tb_builder_label_clone(TB_GraphBuilder* g, TB_Node* label) {
    TB_Function* f = g->f;
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, label->input_count, sizeof(TB_NodeSymbolTable));
    FOR_N(j, 0, label->input_count) if (label->inputs[j]) {
        set_input(f, syms, label->inputs[j], j);
    }

    bool complete = TB_NODE_GET_EXTRA_T(label, TB_NodeSymbolTable)->complete;
    TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = complete);
    return syms;
}

TB_Node* tb_builder_if(TB_GraphBuilder* g, TB_Node* cond, TB_Node* paths[2]) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_IF, TB_TYPE_TUPLE, 2, sizeof(TB_NodeIf));
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, cond, 1);

    TB_Node* cproj[2];
    cproj[0] = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    cproj[1] = tb__make_proj(f, TB_TYPE_CONTROL, n, 1);

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

    TB_NodeIf* br = TB_NODE_GET_EXTRA(n);
    br->prob = 0.5f;
    return n;
}

TB_Node* tb_builder_switch(TB_GraphBuilder* g, TB_Node* cond) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch));
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, cond, 1);

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->total_hits = 100;
    br->succ_count = 1;

    TB_Node* curr = g->curr;
    g->curr = NULL;
    return curr;
}

static TB_Node* branch_cproj(TB_Function* f, TB_Node* n, uint64_t taken, int64_t key, int index) {
    TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
    set_input(f, cproj, n, 0);
    TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = index, .taken = taken, .key = key);
    return cproj;
}

TB_Node* tb_builder_def_case(TB_GraphBuilder* g, TB_Node* br_syms, int prob) {
    TB_Function* f = g->f;
    TB_ASSERT(br_syms->type == TB_SYMBOL_TABLE);
    TB_ASSERT(br_syms->inputs[0]->type == TB_BRANCH);
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(br_syms->inputs[0]);

    TB_Node* cproj = branch_cproj(f, br_syms->inputs[0], prob, 0, 0);
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, br_syms->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, cproj, 0);
    set_input(f, syms, cproj, 1);
    TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);

    FOR_N(j, 2, br_syms->input_count) {
        set_input(f, syms, br_syms->inputs[j], j);
    }
    return syms;
}

TB_Node* tb_builder_key_case(TB_GraphBuilder* g, TB_Node* br_syms, uint64_t key, int prob) {
    TB_Function* f = g->f;

    TB_ASSERT(br_syms->type == TB_SYMBOL_TABLE);
    TB_ASSERT(br_syms->inputs[0]->type == TB_BRANCH);
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(br_syms->inputs[0]);

    TB_Node* cproj = branch_cproj(f, br_syms->inputs[0], prob, key, br->succ_count++);
    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, br_syms->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, cproj, 0);
    set_input(f, syms, cproj, 1);
    TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);

    FOR_N(j, 2, br_syms->input_count) {
        set_input(f, syms, br_syms->inputs[j], j);
    }
    return syms;
}

TB_Node* tb_builder_phi(TB_GraphBuilder* g, int val_count, TB_Node** vals) {
    TB_ASSERT(cfg_is_region(g->curr->inputs[1]));
    TB_ASSERT(g->curr->inputs[1]->input_count == val_count);

    TB_Function* f = g->f;
    TB_Node* phi = tb_alloc_node(f, TB_PHI, vals[0]->dt, 1 + val_count, 0);
    set_input(f, phi, g->curr->inputs[1], 0);
    FOR_N(i, 0, val_count) {
        set_input(f, phi, vals[i], i+1);
    }
    return phi;
}

void tb_builder_br(TB_GraphBuilder* g, TB_Node* target) {
    TB_Function* f = g->f;
    TB_Node* syms = g->curr;
    if (syms == NULL) {
        // we've already hit a terminator so let's just skip the branch
        return;
    }

    TB_ASSERT(target->type == TB_SYMBOL_TABLE);
    TB_ASSERT(syms->input_count >= target->input_count);

    TB_Node* top_ctrl = target->inputs[1];
    FOR_N(i, 2, target->input_count) {
        TB_Node* target_def = target->inputs[i];
        if (target_def == NULL) {
            set_input(f, target, syms->inputs[i], i);
        } else if (target->inputs[i]->type == TB_PHI && target->inputs[i]->inputs[0] == top_ctrl) {
            // already phi-ified, keep appending to it
            add_input_late(f, target_def, syms->inputs[i]);
        } else if (target_def != syms->inputs[i]) {
            TB_ASSERT(!TB_NODE_GET_EXTRA_T(target, TB_NodeSymbolTable)->complete);

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
    TB_ASSERT(curr);

    TB_Node* r = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 1, 2, sizeof(TB_NodeRegion));
    set_input(f, r, curr->inputs[0], 0);

    TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, curr->input_count, sizeof(TB_NodeSymbolTable));
    set_input(f, syms, r, 0);
    set_input(f, syms, r, 1);
    TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);

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

void tb_builder_loc(TB_GraphBuilder* g, int mem_var, TB_SourceFile* file, int line, int column) {
    TB_Function* f = g->f;
    TB_Node* old_ctrl = g->curr->inputs[0];
    TB_Node* old_mem  = g->curr->inputs[2 + mem_var];

    // if there's already a line entry, throw this one away (should we replace the original? idk)
    if (old_ctrl->type == TB_PROJ && old_ctrl->inputs[0]->type == TB_DEBUG_LOCATION &&
        old_mem->type == TB_PROJ  && old_mem->inputs[0] == old_ctrl->inputs[0] &&
        // if it's the first debug location, we wanna keep that one because it's placed above the prologue
        old_ctrl->inputs[0]->inputs[0] != f->params[0]
    ) {
        return;
    }

    TB_Node* n = tb_alloc_node(f, TB_DEBUG_LOCATION, TB_TYPE_TUPLE, 2, sizeof(TB_NodeDbgLoc));
    TB_NODE_SET_EXTRA(n, TB_NodeDbgLoc, .file = file, .line = line, .column = column);

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, xfer_ctrl(g, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);
}

TB_Node** tb_builder_call(TB_GraphBuilder* g, TB_FunctionPrototype* proto, int mem_var, TB_Node* target, int nargs, TB_Node** args) {
    TB_Function* f = g->f;
    size_t proj_count = 2 + proto->return_count;

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

    add_input_late(f, get_callgraph(f), n);

    // create data projections
    if (proto->return_count == 0) {
        return NULL;
    }

    TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(proto);
    TB_Node** rets_arr = tb_arena_alloc(g->arena, proto->return_count * sizeof(TB_Node*));

    FOR_N(i, 0, proto->return_count) {
        rets_arr[i] = tb__make_proj(f, rets[i].dt, n, i + 2);
    }

    return rets_arr;
}

TB_Node* tb_builder_syscall(TB_GraphBuilder* g, TB_DataType dt, int mem_var, TB_Node* target, int nargs, TB_Node** args) {
    TB_Function* f = g->f;

    TB_Node* n = tb_alloc_node(f, TB_SYSCALL, TB_TYPE_TUPLE, 3 + nargs, sizeof(TB_NodeCall));
    set_input(f, n, target, 2);
    FOR_N(i, 0, nargs) {
        set_input(f, n, args[i], i + 3);
    }

    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
    c->proj_count = 3;

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, xfer_ctrl(g, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, xfer_mem(g, mproj, mem_var), 1);

    return tb__make_proj(f, dt, n, 2);
}

void tb_builder_unreachable(TB_GraphBuilder* g, int mem_var) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_UNREACHABLE, TB_TYPE_CONTROL, 2, 0);
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, peek_mem(g, mem_var), 1);
    add_input_late(f, f->root_node, n);
    g->curr = NULL;
}

void tb_builder_trap(TB_GraphBuilder* g, int mem_var) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_TRAP, TB_TYPE_CONTROL, 2, 0);
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, peek_mem(g, mem_var), 1);
    add_input_late(f, f->root_node, n);
    g->curr = NULL;
}

void tb_builder_debugbreak(TB_GraphBuilder* g, int mem_var) {
    TB_Node* n = tb_alloc_node(g->f, TB_DEBUGBREAK, TB_TYPE_CONTROL, 2, 0);
    set_input(g->f, n, xfer_ctrl(g, n), 0);
    set_input(g->f, n, peek_mem(g, mem_var), 1);
}

void tb_builder_blackhole(TB_GraphBuilder* g, int count, TB_Node** args) {
    TB_Node* n = tb_alloc_node(g->f, TB_BLACKHOLE, TB_TYPE_CONTROL, 1 + count, 0);
    set_input(g->f, n, xfer_ctrl(g, n), 0);
    FOR_N(i, 0, count) {
        set_input(g->f, n, args[i], 1+i);
    }
}

void tb_builder_entry_fork(TB_GraphBuilder* g, int count, TB_Node* paths[]) {
    TB_Function* f = g->f;
    TB_ASSERT_MSG(g->curr->inputs[0] == f->params[0], "entry fork can only be applied right after the entry's cproj");

    TB_Node* n = tb_alloc_node(f, TB_ENTRY_FORK, TB_TYPE_TUPLE, 2, 0);
    set_input(f, n, xfer_ctrl(g, n), 0);
    set_input(f, n, peek_mem(g, 0), 1);

    TB_Node* curr = g->curr;
    g->curr = NULL;

    FOR_N(i, 0, count) {
        TB_Node* cproj = tb_alloc_node(f, TB_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeProj));
        set_input(f, cproj, n, 0);
        TB_NODE_SET_EXTRA(cproj, TB_NodeProj, .index = i);

        TB_Node* syms = tb_alloc_node(f, TB_SYMBOL_TABLE, TB_TYPE_VOID, curr->input_count, sizeof(TB_NodeSymbolTable));
        set_input(f, syms, cproj, 0);
        set_input(f, syms, cproj, 1);
        TB_NODE_SET_EXTRA(syms, TB_NodeSymbolTable, .complete = true);

        FOR_N(j, 2, curr->input_count) {
            set_input(f, syms, curr->inputs[j], j);
        }
        paths[i] = syms;
    }
}

void tb_builder_ret(TB_GraphBuilder* g, int mem_var, int count, TB_Node** args) {
    TB_Function* f = g->f;

    TB_ASSERT(2 + mem_var < g->curr->input_count);
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
        set_input(f, poison, f->root_node, 0);

        poison = tb__gvn(f, poison, 0);
        add_input_late(f, ret->inputs[i], poison);
    }

    assert(g->curr->inputs[0] != NULL);
    add_input_late(f, ctrl, g->curr->inputs[0]);
    g->curr = NULL;
}

TB_Node* tb_builder_cycle_counter(TB_GraphBuilder* g) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_CYCLE_COUNTER, TB_TYPE_I64, 1, 0);
    set_input(f, n, g->curr->inputs[0], 0);
    return n;
}

TB_Node* tb_builder_prefetch(TB_GraphBuilder* g, TB_Node* addr, int level) {
    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_PREFETCH, TB_TYPE_MEMORY, 2, sizeof(TB_NodePrefetch));
    set_input(f, n, g->curr->inputs[0], 0);
    TB_NODE_SET_EXTRA(n, TB_NodePrefetch, .level = level);
    return n;
}
