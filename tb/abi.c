// This is gonna get complicated but we can push through :p

////////////////////////////////
// x86-64
////////////////////////////////
// Our two ABIs are System-V and Win64, important to note that
// returns go through 0=RAX, 1=RDX so references to these in terms
// of returns will mean that.

// we finna retrofit systemv terms onto
// windows, it's not too big of a deal.
typedef enum {
    RG_NONE,
    // GPRs
    RG_INTEGER,
    // vector registers
    RG_SSE, RG_SSEUP,
    // stack slot
    RG_MEMORY,
} RegClass;

static int debug_type_size(TB_ABI abi, TB_DebugType* t) {
    while (t->tag == TB_DEBUG_TYPE_ALIAS) {
        t = t->alias.type;
    }

    switch (t->tag) {
        case TB_DEBUG_TYPE_VOID: return 0;
        case TB_DEBUG_TYPE_BOOL: return 1;
        case TB_DEBUG_TYPE_UINT: return (t->int_bits + 7) / 8;
        case TB_DEBUG_TYPE_INT:  return (t->int_bits + 7) / 8;

        case TB_DEBUG_TYPE_FUNCTION: return 8;
        case TB_DEBUG_TYPE_ARRAY:    return 8;
        case TB_DEBUG_TYPE_POINTER:  return 8;

        case TB_DEBUG_TYPE_FLOAT32:  return 4;
        case TB_DEBUG_TYPE_FLOAT64:  return 8;

        case TB_DEBUG_TYPE_STRUCT:
        case TB_DEBUG_TYPE_UNION:
        return t->record.size;

        default: tb_todo();
    }
    return 0;
}

static int debug_type_align(TB_ABI abi, TB_DebugType* t) {
    while (t->tag == TB_DEBUG_TYPE_ALIAS) {
        t = t->alias.type;
    }

    if (t->tag == TB_DEBUG_TYPE_STRUCT || t->tag == TB_DEBUG_TYPE_UNION) {
        return t->record.align;
    }

    return debug_type_size(abi, t);
}

static RegClass classify_reg(TB_ABI abi, TB_DebugType* t) {
    switch (abi) {
        // [https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention]
        // Any argument that doesn't fit in 8 bytes, or isn't 1, 2, 4, or 8 bytes,
        // must be passed by reference. A single argument is never spread across
        // multiple registers.
        case TB_ABI_WIN64: {
            int s = debug_type_size(abi, t);
            if (s == 1 || s == 2 || s == 4 || s == 8) {
                return (t->tag >= TB_DEBUG_TYPE_FLOAT32 && t->tag <= TB_DEBUG_TYPE_FLOAT64) ? RG_SSE : RG_INTEGER;
            }

            return RG_MEMORY;
        }

        case TB_ABI_SYSTEMV: {
            int s = debug_type_size(abi, t);
            if (s <= 8) {
                return (t->tag >= TB_DEBUG_TYPE_FLOAT32 && t->tag <= TB_DEBUG_TYPE_FLOAT64) ? RG_SSE : RG_INTEGER;
            }

            return RG_MEMORY;
        }

        default: tb_todo();
    }
}

static TB_DataType debug_type_to_tb(TB_DebugType* t) {
    switch (t->tag) {
        case TB_DEBUG_TYPE_VOID: return TB_TYPE_VOID;
        case TB_DEBUG_TYPE_BOOL: return TB_TYPE_I8;

        case TB_DEBUG_TYPE_UINT:
        case TB_DEBUG_TYPE_INT: {
            switch (t->int_bits) {
                case 1:  return TB_TYPE_BOOL;
                case 8:  return TB_TYPE_I8;
                case 16: return TB_TYPE_I16;
                case 32: return TB_TYPE_I32;
                case 64: return TB_TYPE_I64;
                default: TB_ASSERT(0); return TB_TYPE_VOID;
            }
        }

        case TB_DEBUG_TYPE_FUNCTION: return TB_TYPE_PTR;
        case TB_DEBUG_TYPE_ARRAY:    return TB_TYPE_PTR;
        case TB_DEBUG_TYPE_POINTER:  return TB_TYPE_PTR;

        case TB_DEBUG_TYPE_FLOAT32:  return TB_TYPE_F32;
        case TB_DEBUG_TYPE_FLOAT64:  return TB_TYPE_F64;

        default: TB_ASSERT(0); return TB_TYPE_VOID;
    }
}

static TB_DataType reg_class_to_tb(TB_ABI abi, RegClass rg, TB_DebugType* type) {
    switch (rg) {
        case RG_MEMORY:  return TB_TYPE_PTR;
        case RG_INTEGER: {
            if (type->tag == TB_DEBUG_TYPE_POINTER) { return TB_TYPE_PTR; }
            switch (debug_type_size(abi, type)) {
                case 1:  return TB_TYPE_I8;
                case 2:  return TB_TYPE_I16;
                case 4:  return TB_TYPE_I32;
                case 8:  return TB_TYPE_I64;
                default: TB_ASSERT(0); return TB_TYPE_VOID;
            }
        }

        case RG_SSE: {
            assert(type->tag >= TB_DEBUG_TYPE_FLOAT32 && type->tag <= TB_DEBUG_TYPE_FLOAT64);
            return (TB_DataType){ .type = type->tag == TB_DEBUG_TYPE_FLOAT32 ? TB_TAG_F32 : TB_TAG_F64 };
        }

        default: TB_ASSERT(0); return TB_TYPE_VOID;
    }
}

TB_API TB_DebugType* tb_debug_field_type(TB_DebugType* type) {
    assert(type->tag == TB_DEBUG_TYPE_FIELD);
    return type->field.type;
}

TB_PassingRule tb_get_passing_rule_from_dbg(TB_Module* mod, TB_DebugType* param_type, bool is_return) {
    return classify_reg(mod->target_abi, param_type) == RG_MEMORY ? TB_PASSING_INDIRECT : TB_PASSING_DIRECT;
}

TB_API TB_FunctionPrototype* tb_prototype_from_dbg(TB_Module* m, TB_DebugType* dbg) {
    TB_ABI abi = m->target_abi;

    // aggregate return means the first parameter will be used for
    // a pointer to where the output should be written.
    //
    // TODO(NeGate): it's uninitialized by default but we don't communicate
    // this to the IR yet.
    RegClass return_rg = RG_NONE;
    TB_PrototypeParam ret = { TB_TYPE_VOID };
    if (dbg->func.return_count == 1) {
        return_rg = classify_reg(abi, dbg->func.returns[0]);

        ret.debug_type = dbg->func.returns[0];
        ret.dt = reg_class_to_tb(abi, return_rg, dbg->func.returns[0]);
        ret.name = "$ret";
    }

    bool has_aggregate_return = return_rg == RG_MEMORY;

    // estimate the number of parameters:
    // * in win64 this is easy, parameters don't split.
    // * in sysv this is a nightmare, structs are usually
    // the culprit because they can be split up.
    size_t param_count = dbg->func.param_count;
    TB_DebugType** param_list = dbg->func.params;
    if (abi == TB_ABI_SYSTEMV) {
        // tb_todo();
    }

    // build up prototype param types
    size_t return_count = dbg->func.return_count;
    size_t size = sizeof(TB_FunctionPrototype) + ((param_count + has_aggregate_return + return_count) * sizeof(TB_PrototypeParam));
    TB_FunctionPrototype* p = tb_arena_alloc(get_permanent_arena(m), size);
    p->call_conv = dbg->func.cc;
    p->has_varargs = dbg->func.has_varargs;
    p->return_count = return_count;
    p->param_count = has_aggregate_return + param_count;

    if (dbg->func.param_count > 0) {
        FOR_N(i, 0, dbg->func.param_count) {
            TB_DebugType* type = param_list[i]->field.type;
            RegClass rg = classify_reg(abi, type);

            TB_PrototypeParam param = {
                .name = param_list[i]->field.name,
                .debug_type = type,
                .dt = reg_class_to_tb(abi, rg, type),
            };

            p->params[has_aggregate_return + i] = param;
        }
    }

    if (p->return_count == 1) {
        if (has_aggregate_return) {
            p->params[0] = ret;
        }

        p->params[p->param_count] = ret;
    }

    return p;
}
