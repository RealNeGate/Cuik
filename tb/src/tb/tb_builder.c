// IR BUILDER
//
// Handles generating the TB_Function IR via C functions.
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
#include "tb_internal.h"

TB_API int tb_function_get_label_count(TB_Function* f) {
    return f->bb_count;
}

TB_API TB_Node* tb_function_get_node(TB_Function* f, TB_Reg r) {
    tb_assume(r >= 0 && r < f->node_count);
    return &f->nodes[r];
}

TB_API void tb_get_function_get_local_info(TB_Function* f, TB_Reg r, int* size, int* align) {
    tb_assume(f->nodes[r].type == TB_LOCAL);

    *size  = f->nodes[r].local.size;
    *align = f->nodes[r].local.alignment;
}

TB_API bool tb_node_is_phi_node(TB_Function* f, TB_Reg r) {
    return f->nodes[r].type == TB_PHI1 ||
        f->nodes[r].type == TB_PHI2 ||
        f->nodes[r].type == TB_PHIN;
}

TB_API int tb_node_get_phi_width(TB_Function* f, TB_Reg r) {
    switch (f->nodes[r].type) {
        case TB_PHI1: return 1;
        case TB_PHI2: return 2;
        case TB_PHIN: return f->nodes[r].phi.count;

        default:
        tb_unreachable();
        return 0;
    }
}

TB_API TB_PhiInput* tb_node_get_phi_inputs(TB_Function* f, TB_Reg r) {
    switch (f->nodes[r].type) {
        case TB_PHI1: return f->nodes[r].phi1.inputs;
        case TB_PHI2: return f->nodes[r].phi2.inputs;
        case TB_PHIN: return f->nodes[r].phi.inputs;

        default:
        tb_unreachable();
        return NULL;
    }
}

TB_API bool tb_node_is_conditional(TB_Function* f, TB_Reg r) {
    return f->nodes[r].type == TB_IF || f->nodes[r].type == TB_SWITCH;
}

TB_API bool tb_node_is_terminator(TB_Function* f, TB_Reg r) {
    return f->nodes[r].type == TB_IF || f->nodes[r].type == TB_GOTO || f->nodes[r].type == TB_RET;
}

TB_API TB_Reg tb_node_load_get_address(TB_Function* f, TB_Reg r) {
    tb_assume(f->nodes[r].type == TB_LOAD);

    return f->nodes[r].load.address;
}

TB_API TB_Reg tb_node_arith_get_left(TB_Function* f, TB_Reg r) {
    tb_assume(f->nodes[r].type >= TB_AND && f->nodes[r].type <= TB_CMP_FLE);

    // TODO(NeGate): They share position in the union
    static_assert(offsetof(TB_Node, cmp.a) == offsetof(TB_Node, i_arith.a),
        "TB_RegPayload::cmp.a should alias TB_RegPayload::i_arith.a");
    static_assert(offsetof(TB_Node, f_arith.a) == offsetof(TB_Node, i_arith.a),
        "TB_RegPayload::f_arith.a should alias TB_RegPayload::i_arith.a");

    return f->nodes[r].i_arith.a;
}

TB_API TB_Reg tb_node_arith_get_right(TB_Function* f, TB_Reg r) {
    tb_assume(f->nodes[r].type >= TB_AND && f->nodes[r].type <= TB_CMP_FLE);

    // TODO(NeGate): They share position in the union
    static_assert(offsetof(TB_Node, cmp.b) == offsetof(TB_Node, i_arith.b),
        "TB_RegPayload::cmp.b should alias TB_RegPayload::i_arith.b");
    static_assert(offsetof(TB_Node, f_arith.b) == offsetof(TB_Node, i_arith.b),
        "TB_RegPayload::f_arith.b should alias TB_RegPayload::i_arith.b");

    return f->nodes[r].i_arith.b;
}

TB_API bool tb_node_is_constant_int(TB_Function* f, TB_Reg r, uint64_t imm) {
    if (f->nodes[r].type == TB_INTEGER_CONST && f->nodes[r].integer.num_words == 1) {
        return (f->nodes[r].integer.single_word == imm);
    }

    return false;
}

TB_API bool tb_node_is_constant_zero(TB_Function* f, TB_Reg r) {
    TB_Node* n = &f->nodes[r];
    while (n->type == TB_PASS) {
        n = &f->nodes[n->pass.value];
    }

    if (n->type == TB_INTEGER_CONST) {
        if (n->integer.num_words == 1) {
            if (n->integer.single_word == 0) {
                return true;
            }
        } else {
            if (BigInt_is_zero(n->integer.num_words, n->integer.words)) {
                return true;
            }
        }
    }

    return false;
}

static TB_Attrib* tb_make_attrib(TB_Function* f) {
    if (f->attrib_pool_count + 1 >= f->attrib_pool_capacity) {
        f->attrib_pool_capacity = (f->attrib_pool_count + 1) * 2;

        f->attrib_pool = tb_platform_heap_realloc(f->attrib_pool, sizeof(TB_Attrib) * f->attrib_pool_capacity);
        if (!f->attrib_pool) tb_panic("Out of memory");
    }

    return &f->attrib_pool[f->attrib_pool_count++];
}

static void append_attrib(TB_Function* f, TB_Reg r, TB_Attrib* a) {
    TB_Attrib* chain = f->nodes[r].first_attrib;
    if (chain == NULL) {
        f->nodes[r].first_attrib = a;
    } else {
        // skip till the end
        while (chain->next != NULL) chain = chain->next;

        chain->next = a;
    }
}

TB_API void tb_function_attrib_variable(TB_Function* f, TB_Reg r, const char* name, TB_DebugType* type) {
    assert(name != NULL);
    assert(type != NULL);

    TB_Attrib* a = tb_make_attrib(f);
    *a = (TB_Attrib) { .type = TB_ATTRIB_VARIABLE, .var = { tb_platform_string_alloc(name), type } };
    append_attrib(f, r, a);
}

static TB_Reg tb_make_reg(TB_Function* f, int type, TB_DataType dt) {
    // Cannot add registers to terminated basic blocks, except labels
    // which start new basic blocks
    tb_assume(f);
    tb_function_reserve_nodes(f, 1);

    TB_Reg r = f->node_count++;
    f->nodes[r] = (TB_Node) {
        .type = type,
        .dt = dt
    };

    TB_BasicBlock* bb = &f->bbs[f->current_label];
    if (bb->start == 0) {
        bb->start = r;
    } else {
        // append to the end of the basic block
        TB_Reg end = bb->end;
        f->nodes[end].next = r;

        // check if there's already a terminator
        #ifndef NDEBUG
        if (TB_IS_NODE_TERMINATOR(f->nodes[bb->end].type)) {
            tb_panic("Basic block already has terminator\n");
        }
        #endif
    }
    bb->end = r;
    return r;
}

TB_API TB_Reg tb_function_insert(TB_Function* f, TB_Reg r, const TB_Node n) {
    tb_todo();
}

TB_API TB_Reg tb_function_set(TB_Function* f, TB_Reg r, const TB_Node n) {
    tb_todo();
}

TB_API TB_Reg tb_function_append(TB_Function* f, const TB_Node n) {
    tb_function_reserve_nodes(f, 1);

    TB_Reg r = f->node_count++;
    f->nodes[r] = n;
    f->nodes[r].next = TB_NULL_REG;

    // append to the end of the basic block
    TB_Reg end = f->bbs[f->current_label].end;
    f->nodes[end].next = r;
    f->bbs[f->current_label].end = end;
    return r;
}

static TB_Reg tb_bin_arith(TB_Function* f, int type, TB_ArithmaticBehavior arith_behavior, TB_Reg a, TB_Reg b) {
    // tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    if (!TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt)) {
        tb_function_print(f, tb_default_print_callback, stderr, false);
        abort();
    }

    TB_Reg r = tb_make_reg(f, type, f->nodes[a].dt);
    f->nodes[r].i_arith.arith_behavior = arith_behavior;
    f->nodes[r].i_arith.a = a;
    f->nodes[r].i_arith.b = b;
    return r;
}

static TB_Reg tb_bin_farith(TB_Function* f, int type, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));

    TB_Reg r = tb_make_reg(f, type, f->nodes[a].dt);
    f->nodes[r].f_arith.a = a;
    f->nodes[r].f_arith.b = b;
    return r;
}

TB_API TB_Reg tb_inst_trunc(TB_Function* f, TB_Reg src, TB_DataType dt) {
    tb_assume(f->nodes[src].dt.width == dt.width);

    TB_Reg r = tb_make_reg(f, TB_TRUNCATE, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_int2ptr(TB_Function* f, TB_Reg src) {
    tb_assume(f->nodes[src].dt.width == 0);

    TB_Reg r = tb_make_reg(f, TB_INT2PTR, TB_TYPE_PTR);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_ptr2int(TB_Function* f, TB_Reg src, TB_DataType dt) {
    tb_assume(dt.width == 0);
    tb_assume(f->nodes[src].dt.width == 0);

    TB_Reg r = tb_make_reg(f, TB_PTR2INT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_int2float(TB_Function* f, TB_Reg src, TB_DataType dt, bool is_signed) {
    tb_assume(dt.type == TB_FLOAT);
    tb_assume(f->nodes[src].dt.type == TB_INT);
    tb_assume(f->nodes[src].dt.width == dt.width);

    if (f->nodes[src].type == TB_INTEGER_CONST && f->nodes[src].integer.num_words == 1) {
        uint64_t y = f->nodes[src].integer.single_word;
        if (is_signed) {
            y = tb__sxt(y, f->nodes[src].dt.data, 64);
        }

        if (dt.data == TB_FLT_32) {
            float x;
            if (is_signed) x = (int64_t) y;
            else x = (uint64_t) y;

            return tb_inst_float32(f, x);
        } else if (dt.data == TB_FLT_64) {
            double x;
            if (is_signed) x = (int64_t) y;
            else x = (uint64_t) y;

            return tb_inst_float64(f, x);
        }
    }

    TB_Reg r = tb_make_reg(f, is_signed ? TB_INT2FLOAT : TB_UINT2FLOAT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_float2int(TB_Function* f, TB_Reg src, TB_DataType dt, bool is_signed) {
    tb_assume(f->nodes[src].dt.width == dt.width);

    TB_Reg r = tb_make_reg(f, is_signed ? TB_FLOAT2INT : TB_FLOAT2UINT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_fpxt(TB_Function* f, TB_Reg src, TB_DataType dt) {
    tb_assume(dt.width == f->nodes[src].dt.width);

    TB_Reg r = tb_make_reg(f, TB_FLOAT_EXT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_sxt(TB_Function* f, TB_Reg src, TB_DataType dt) {
    tb_assume(dt.width == f->nodes[src].dt.width);

    TB_Reg r = tb_make_reg(f, TB_SIGN_EXT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_zxt(TB_Function* f, TB_Reg src, TB_DataType dt) {
    tb_assume(dt.width == f->nodes[src].dt.width);

    TB_Reg r = tb_make_reg(f, TB_ZERO_EXT, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_bitcast(TB_Function* f, TB_Reg src, TB_DataType dt) {
    // TODO(NeGate): Do some size checks
    TB_Reg r = tb_make_reg(f, TB_BITCAST, dt);
    f->nodes[r].unary.src = src;
    return r;
}

TB_API TB_Reg tb_inst_param(TB_Function* f, int param_id) {
    tb_assume(param_id < f->prototype->param_count);
    return f->params[param_id];
}

TB_API TB_Reg tb_inst_param_addr(TB_Function* f, int param_id) {
    tb_assume(param_id < f->prototype->param_count);

    TB_Reg param = f->params[param_id];
    int param_size = f->nodes[param].param.size;

    TB_Reg r = tb_make_reg(f, TB_PARAM_ADDR, TB_TYPE_PTR);
    f->nodes[r].param_addr.param = param;
    f->nodes[r].param_addr.size = param_size;
    f->nodes[r].param_addr.alignment = param_size;
    return r;
}

TB_API void tb_inst_unreachable(TB_Function* f) {
    tb_make_reg(f, TB_UNREACHABLE, TB_TYPE_VOID);
}

TB_API void tb_inst_debugbreak(TB_Function* f) {
    tb_make_reg(f, TB_DEBUGBREAK, TB_TYPE_VOID);
}

TB_API void tb_inst_trap(TB_Function* f) {
    tb_make_reg(f, TB_TRAP, TB_TYPE_VOID);
}

TB_API void tb_inst_keep_alive(TB_Function* f, TB_Reg src) {
    TB_Reg r = tb_make_reg(f, TB_KEEPALIVE, TB_TYPE_VOID);
    f->nodes[r].unary.src = src;
}

TB_API TB_Reg tb_inst_poison(TB_Function* f) {
    return tb_make_reg(f, TB_POISON, TB_TYPE_VOID);
}

TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line) {
    tb_assume(line >= 0);

    TB_Reg r = f->node_count - 1;
    if (f->nodes[r].type != TB_LINE_INFO) {
        r = tb_make_reg(f, TB_LINE_INFO, TB_TYPE_VOID);
    }

    f->nodes[r].line_info.file = file;
    f->nodes[r].line_info.line = line;
}

TB_API TB_Reg tb_inst_local(TB_Function* f, uint32_t size, TB_CharUnits alignment) {
    tb_assume(size > 0);
    tb_assume(alignment > 0 && tb_is_power_of_two(alignment));

    TB_Reg r = tb_make_reg(f, TB_LOCAL, TB_TYPE_PTR);
    f->nodes[r].local.alignment = alignment;
    f->nodes[r].local.size = size;
    return r;
}

TB_API TB_Reg tb_inst_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits alignment) {
    TB_Reg r = tb_make_reg(f, TB_LOAD, dt);
    f->nodes[r].load = (struct TB_NodeLoad) {
        .address = addr, .alignment = alignment
    };
    return r;
}

TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, uint32_t alignment) {
    tb_assume(addr);
    tb_assume(val);
    tb_assume(TB_DATA_TYPE_EQUALS(dt, f->nodes[val].dt));

    TB_Reg r = tb_make_reg(f, TB_STORE, dt);
    f->nodes[r].store = (struct TB_NodeStore) {
        .address = addr, .value = val, .alignment = alignment
    };
    return;
}

TB_API TB_Reg tb_inst_volatile_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits alignment) {
    TB_Reg r = tb_make_reg(f, TB_LOAD, dt);
    f->nodes[r].store = (struct TB_NodeStore) { .address = addr, .alignment = alignment, .is_volatile = true };
    return r;
}

TB_API void tb_inst_volatile_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits alignment) {
    TB_Reg r = tb_make_reg(f, TB_STORE, dt);
    f->nodes[r].store = (struct TB_NodeStore) {
        .address = addr, .value = val, .alignment = alignment, .is_volatile = true
    };
}

TB_API void tb_inst_initialize_mem(TB_Function* f, TB_Reg addr, TB_Initializer* src) {
    TB_Reg r = tb_make_reg(f, TB_INITIALIZE, TB_TYPE_PTR);
    f->nodes[r].init = (struct TB_NodeInitialize) {
        .addr = addr,
        .src  = src,
    };
}

TB_API TB_Reg tb_inst_bool(TB_Function* f, bool imm) {
    TB_Reg r = tb_make_reg(f, TB_INTEGER_CONST, TB_TYPE_BOOL);
    f->nodes[r].integer.num_words = 1;
    f->nodes[r].integer.single_word = imm;
    return r;
}

TB_API TB_Reg tb_inst_ptr(TB_Function* f, uint64_t imm) {
    TB_Reg r = tb_make_reg(f, TB_INTEGER_CONST, TB_TYPE_PTR);
    f->nodes[r].integer.num_words = 1;
    f->nodes[r].integer.single_word = imm;
    return r;
}

TB_API TB_Reg tb_inst_uint(TB_Function* f, TB_DataType dt, uint64_t imm) {
    tb_assume(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));

    TB_Reg r = tb_make_reg(f, TB_INTEGER_CONST, dt);
    f->nodes[r].integer.num_words = 1;
    f->nodes[r].integer.single_word = imm;

    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
    f->nodes[r].integer.single_word &= mask;
    return r;
}

TB_API TB_Reg tb_inst_sint(TB_Function* f, TB_DataType dt, int64_t imm) {
    tb_assume(TB_IS_POINTER_TYPE(dt) || (TB_IS_INTEGER_TYPE(dt) && (dt.data <= 64)));

    TB_Reg r = tb_make_reg(f, TB_INTEGER_CONST, dt);
    f->nodes[r].integer.num_words = 1;
    f->nodes[r].integer.single_word = imm;

    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
    f->nodes[r].integer.single_word &= mask;
    return r;
}

TB_API TB_Reg tb_inst_float32(TB_Function* f, float imm) {
    TB_Reg r = tb_make_reg(f, TB_FLOAT32_CONST, TB_TYPE_F32);
    f->nodes[r].flt32.value = imm;
    return r;
}

TB_API TB_Reg tb_inst_float64(TB_Function* f, double imm) {
    TB_Reg r = tb_make_reg(f, TB_FLOAT64_CONST, TB_TYPE_F64);
    f->nodes[r].flt64.value = imm;
    return r;
}

TB_API TB_Reg tb_inst_string(TB_Function* f, size_t len, const char* str) {
    char* newstr = tb_platform_arena_alloc(len);
    memcpy(newstr, str, len);

    TB_Reg r = tb_make_reg(f, TB_STRING_CONST, TB_TYPE_PTR);
    f->nodes[r].string = (struct TB_NodeString) { .length = len, .data = newstr };
    return r;
}

TB_API TB_Reg tb_inst_cstring(TB_Function* f, const char* str) {
    size_t len = strlen(str);
    char* newstr = tb_platform_arena_alloc(len + 1);
    memcpy(newstr, str, len);
    newstr[len] = '\0';

    TB_Reg r = tb_make_reg(f, TB_STRING_CONST, TB_TYPE_PTR);
    f->nodes[r].string = (struct TB_NodeString) { .length = len + 1, .data = newstr };
    return r;
}

TB_API TB_Reg tb_inst_array_access(TB_Function* f, TB_Reg base, TB_Reg index, uint32_t stride) {
    TB_Reg r = tb_make_reg(f, TB_ARRAY_ACCESS, TB_TYPE_PTR);
    f->nodes[r].array_access.base = base;
    f->nodes[r].array_access.index = index;
    f->nodes[r].array_access.stride = stride;
    return r;
}

TB_API TB_Reg tb_inst_member_access(TB_Function* f, TB_Reg base, int32_t offset) {
    TB_Reg r = tb_make_reg(f, TB_MEMBER_ACCESS, TB_TYPE_PTR);
    f->nodes[r].member_access.base = base;
    f->nodes[r].member_access.offset = offset;
    return r;
}

TB_API TB_Reg tb_inst_get_symbol_address(TB_Function* f, const TB_Symbol* target) {
    assert(target != NULL);

    TB_Reg r = tb_make_reg(f, TB_GET_SYMBOL_ADDRESS, TB_TYPE_PTR);
    f->nodes[r].sym.value = target;
    return r;
}

TB_Reg* tb_vla_reserve(TB_Function* f, size_t count) {
    // Reserve space for the arguments
    if (f->vla.count + count >= f->vla.capacity) {
        f->vla.capacity = tb_next_pow2(f->vla.count + count);
        if (f->vla.capacity < 16) f->vla.capacity = 16;

        f->vla.data = tb_platform_heap_realloc(f->vla.data, f->vla.capacity * sizeof(TB_Reg));
    }

    return &f->vla.data[f->vla.count];
}

TB_API TB_Reg tb_inst_syscall(TB_Function* f, TB_DataType dt, TB_Reg syscall_num, size_t param_count, const TB_Reg* params) {
    int param_start = f->vla.count;

    TB_Reg* vla = tb_vla_reserve(f, param_count);
    memcpy(vla, params, param_count * sizeof(TB_Reg));
    f->vla.count += param_count;

    int param_end = f->vla.count;

    TB_Reg r = tb_make_reg(f, TB_SCALL, dt);
    f->nodes[r].scall = (struct TB_NodeSysCall) { param_start, param_end, syscall_num };
    return r;
}

TB_API TB_Reg tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Symbol* target, size_t param_count, const TB_Reg* params) {
    assert(target->tag != TB_SYMBOL_NONE);
    int param_start = f->vla.count;

    TB_Reg* vla = tb_vla_reserve(f, param_count);
    memcpy(vla, params, param_count * sizeof(TB_Reg));
    f->vla.count += param_count;

    int param_end = f->vla.count;

    TB_Reg r = tb_make_reg(f, TB_CALL, dt);
    f->nodes[r].call = (struct TB_NodeCall) { param_start, param_end, target };
    return r;
}

TB_API TB_Reg tb_inst_vcall(TB_Function* f, TB_DataType dt, const TB_Reg target, size_t param_count, const TB_Reg* params) {
    int param_start = f->vla.count;

    TB_Reg* vla = tb_vla_reserve(f, param_count);
    memcpy(vla, params, param_count * sizeof(TB_Reg));
    f->vla.count += param_count;

    int param_end = f->vla.count;

    TB_Reg r = tb_make_reg(f, TB_VCALL, dt);
    f->nodes[r].vcall = (struct TB_NodeDynamicCall) { param_start, param_end, target };
    return r;
}

TB_API void tb_inst_memset(TB_Function* f, TB_Reg dst, TB_Reg val, TB_Reg size, TB_CharUnits align) {
    tb_assume(TB_IS_POINTER_TYPE(f->nodes[dst].dt));
    tb_assume(TB_IS_INTEGER_TYPE(f->nodes[val].dt) && f->nodes[val].dt.data == 8);

    TB_Reg r = tb_make_reg(f, TB_MEMSET, TB_TYPE_I8);
    f->nodes[r].mem_op = (struct TB_NodeMemoryOp) { dst, val, size, align };
}

TB_API void tb_inst_memcpy(TB_Function* f, TB_Reg dst, TB_Reg src, TB_Reg size, TB_CharUnits align) {
    tb_assume(TB_IS_POINTER_TYPE(f->nodes[dst].dt));
    tb_assume(TB_IS_POINTER_TYPE(f->nodes[src].dt));

    TB_Reg r = tb_make_reg(f, TB_MEMCPY, TB_TYPE_I8);
    f->nodes[r].mem_op = (struct TB_NodeMemoryOp) { dst, src, size, align };
}

TB_API void tb_inst_memclr(TB_Function* f, TB_Reg addr, TB_CharUnits size, TB_CharUnits align) {
    TB_Reg r = tb_make_reg(f, TB_MEMCLR, TB_TYPE_I8);
    f->nodes[r].clear = (struct TB_NodeMemoryClear) { addr, size, align };
}

TB_API TB_Reg tb_inst_not(TB_Function* f, TB_Reg n) {
    TB_DataType dt = f->nodes[n].dt;

    TB_Reg r = tb_make_reg(f, TB_NOT, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { n };
    return r;
}

TB_API TB_Reg tb_inst_bswap(TB_Function* f, TB_Reg n) {
    TB_DataType dt = f->nodes[n].dt;

    TB_Reg r = tb_make_reg(f, TB_BSWAP, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { n };
    return r;
}

TB_API TB_Reg tb_inst_clz(TB_Function* f, TB_Reg n) {
    TB_DataType dt = f->nodes[n].dt;

    TB_Reg r = tb_make_reg(f, TB_CLZ, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { n };
    return r;
}

TB_API TB_Reg tb_inst_neg(TB_Function* f, TB_Reg n) {
    TB_DataType dt = f->nodes[n].dt;

    if (f->nodes[n].type == TB_INTEGER_CONST && f->nodes[n].integer.num_words == 1) {
        uint64_t src = f->nodes[n].integer.single_word;
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

        // two's complement negate is just invert and add 1
        uint64_t negated = (~src + 1) & mask;

        return tb_inst_sint(f, dt, negated);
    } else if (f->nodes[n].type == TB_FLOAT32_CONST) {
        return tb_inst_float32(f, -f->nodes[n].flt32.value);
    } else if (f->nodes[n].type == TB_FLOAT64_CONST) {
        return tb_inst_float64(f, -f->nodes[n].flt64.value);
    }

    TB_Reg r = tb_make_reg(f, TB_NEG, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { n };
    return r;
}

TB_API TB_Reg tb_inst_and(TB_Function* f, TB_Reg a, TB_Reg b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_AND, 0, a, b);
}

TB_API TB_Reg tb_inst_or(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    TB_DataType dt = f->nodes[a].dt;

    if (f->nodes[a].type == TB_INTEGER_CONST && f->nodes[b].type == TB_INTEGER_CONST) {
        if (f->nodes[a].integer.single_word == 1 && f->nodes[b].integer.single_word == 1) {
            return tb_inst_uint(f, dt, f->nodes[a].integer.single_word | f->nodes[b].integer.single_word);
        }
    }

    return tb_bin_arith(f, TB_OR, 0, a, b);
}

TB_API TB_Reg tb_inst_xor(TB_Function* f, TB_Reg a, TB_Reg b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_XOR, 0, a, b);
}

TB_API TB_Reg tb_inst_select(TB_Function* f, TB_Reg cond, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_SELECT, dt);
    f->nodes[r].select = (struct TB_NodeSelect) { a, b, cond };
    return r;
}

TB_API TB_Reg tb_inst_add(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_ADD, arith_behavior, a, b);
}

TB_API TB_Reg tb_inst_sub(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SUB, arith_behavior, a, b);
}

TB_API TB_Reg tb_inst_mul(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_MUL, arith_behavior, a, b);
}

TB_API TB_Reg tb_inst_div(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    // division can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SDIV : TB_UDIV, 0, a, b);
}

TB_API TB_Reg tb_inst_mod(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    // modulo can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SMOD : TB_UMOD, 0, a, b);
}

TB_API TB_Reg tb_inst_shl(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SHL, arith_behavior, a, b);
}

////////////////////////////////
// Atomics
////////////////////////////////
TB_API TB_Reg tb_inst_atomic_test_and_set(TB_Function* f, TB_Reg addr, TB_MemoryOrder order) {
    TB_Reg r = tb_make_reg(f, TB_ATOMIC_TEST_AND_SET, TB_TYPE_BOOL);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = TB_NULL_REG;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_clear(TB_Function* f, TB_Reg addr, TB_MemoryOrder order) {
    TB_Reg r = tb_make_reg(f, TB_ATOMIC_CLEAR, TB_TYPE_BOOL);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = TB_NULL_REG;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_load(TB_Function* f, TB_Reg addr, TB_DataType dt, TB_MemoryOrder order) {
    TB_Reg r = tb_make_reg(f, TB_ATOMIC_LOAD, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = TB_NULL_REG;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_xchg(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_XCHG, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_add(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_ADD, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_sub(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_SUB, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_and(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_AND, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_xor(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_XOR, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_Reg tb_inst_atomic_or(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order) {
    TB_DataType dt = f->nodes[src].dt;

    TB_Reg r = tb_make_reg(f, TB_ATOMIC_OR, dt);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = src;
    f->nodes[r].atomic.order = order;
    f->nodes[r].atomic.order2 = TB_MEM_ORDER_SEQ_CST;
    return r;
}

TB_API TB_CmpXchgResult tb_inst_atomic_cmpxchg(TB_Function* f, TB_Reg addr, TB_Reg expected, TB_Reg desired, TB_MemoryOrder succ, TB_MemoryOrder fail) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[desired].dt, f->nodes[expected].dt));
    TB_DataType dt = f->nodes[desired].dt;

    TB_Reg r  = tb_make_reg(f, TB_ATOMIC_CMPXCHG, dt);
    TB_Reg r2 = tb_make_reg(f, TB_ATOMIC_CMPXCHG2, TB_TYPE_BOOL);

    tb_assume(f->nodes[r].next == r2);
    f->nodes[r].atomic.addr = addr;
    f->nodes[r].atomic.src = expected;
    f->nodes[r].atomic.order = succ;
    f->nodes[r].atomic.order2 = fail;

    f->nodes[r2].atomic.addr = addr;
    f->nodes[r2].atomic.src = desired;
    f->nodes[r2].atomic.order = succ;
    f->nodes[r2].atomic.order2 = fail;
    return (TB_CmpXchgResult) { .success = r2, .old_value = r };
}

// TODO(NeGate): Maybe i should split the bitshift operations into a separate kind of
// operator that has different arithmatic behaviors, maybe like trap on a large shift amount
TB_API TB_Reg tb_inst_sar(TB_Function* f, TB_Reg a, TB_Reg b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SAR, 0, a, b);
}

TB_API TB_Reg tb_inst_shr(TB_Function* f, TB_Reg a, TB_Reg b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SHR, 0, a, b);
}

TB_API TB_Reg tb_inst_fadd(TB_Function* f, TB_Reg a, TB_Reg b) {
    return tb_bin_farith(f, TB_FADD, a, b);
}

TB_API TB_Reg tb_inst_fsub(TB_Function* f, TB_Reg a, TB_Reg b) {
    return tb_bin_farith(f, TB_FSUB, a, b);
}

TB_API TB_Reg tb_inst_fmul(TB_Function* f, TB_Reg a, TB_Reg b) {
    return tb_bin_farith(f, TB_FMUL, a, b);
}

TB_API TB_Reg tb_inst_fdiv(TB_Function* f, TB_Reg a, TB_Reg b) {
    return tb_bin_farith(f, TB_FDIV, a, b);
}

TB_API TB_Reg tb_inst_va_start(TB_Function* f, TB_Reg a) {
    assert(f->nodes[a].type == TB_PARAM_ADDR);

    TB_Reg r = tb_make_reg(f, TB_VA_START, TB_TYPE_PTR);
    f->nodes[r].unary = (struct TB_NodeUnary) { a };
    return r;
}

TB_API TB_Reg tb_inst_x86_ldmxcsr(TB_Function* f, TB_Reg a) {
    assert(f->nodes[a].dt.type == TB_INT && f->nodes[a].dt.data == 32);

    TB_Reg r = tb_make_reg(f, TB_X86INTRIN_LDMXCSR, TB_TYPE_I32);
    f->nodes[r].unary = (struct TB_NodeUnary) { a };
    return r;
}

TB_API TB_Reg tb_inst_x86_rdtsc(TB_Function* f) {
    return tb_make_reg(f, TB_X86INTRIN_RDTSC, TB_TYPE_I64);
}

TB_API TB_Reg tb_inst_x86_stmxcsr(TB_Function* f) {
    return tb_make_reg(f, TB_X86INTRIN_STMXCSR, TB_TYPE_I32);
}

TB_API TB_Reg tb_inst_x86_sqrt(TB_Function* f, TB_Reg a) {
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_X86INTRIN_SQRT, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { a };
    return r;
}

TB_API TB_Reg tb_inst_x86_rsqrt(TB_Function* f, TB_Reg a) {
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_X86INTRIN_RSQRT, dt);
    f->nodes[r].unary = (struct TB_NodeUnary) { a };
    return r;
}

TB_API TB_Reg tb_inst_cmp_eq(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_EQ, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_ne(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_NE, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_ilt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_INTEGER_TYPE(f->nodes[a].dt) || TB_IS_POINTER_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_ile(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_INTEGER_TYPE(f->nodes[a].dt) || TB_IS_POINTER_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_igt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_INTEGER_TYPE(f->nodes[a].dt) || TB_IS_POINTER_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = b;
    f->nodes[r].cmp.b  = a;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_ige(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_INTEGER_TYPE(f->nodes[a].dt) || TB_IS_POINTER_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = b;
    f->nodes[r].cmp.b  = a;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_flt(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_FLOAT_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_fle(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_FLOAT_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = a;
    f->nodes[r].cmp.b  = b;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_fgt(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_FLOAT_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_FLT, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = b;
    f->nodes[r].cmp.b  = a;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_cmp_fge(TB_Function* f, TB_Reg a, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    tb_assume(TB_IS_FLOAT_TYPE(f->nodes[a].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_CMP_FLE, TB_TYPE_BOOL);
    f->nodes[r].cmp.a  = b;
    f->nodes[r].cmp.b  = a;
    f->nodes[r].cmp.dt = dt;
    return r;
}

TB_API TB_Reg tb_inst_phi2(TB_Function* f, TB_Label a_label, TB_Reg a, TB_Label b_label, TB_Reg b) {
    tb_assume(TB_DATA_TYPE_EQUALS(f->nodes[a].dt, f->nodes[b].dt));
    TB_DataType dt = f->nodes[a].dt;

    TB_Reg r = tb_make_reg(f, TB_PHI2, dt);
    f->nodes[r].phi2.inputs[0] = (TB_PhiInput){ a_label, a };
    f->nodes[r].phi2.inputs[1] = (TB_PhiInput){ b_label, b };
    return r;
}

TB_API TB_Label tb_basic_block_create(TB_Function* f) {
    if (f->bb_count + 1 >= f->bb_capacity) {
        f->bb_capacity = (f->bb_count + 1) * 2;

        f->bbs = tb_platform_heap_realloc(f->bbs, sizeof(TB_BasicBlock) * f->bb_capacity);
        if (f->bbs == NULL) tb_panic("tb_basic_block_create: Out of memory");
    }

    TB_Label bb = f->bb_count++;
    f->bbs[bb] = (TB_BasicBlock){ 0 };
    return bb;
}

TB_API bool tb_basic_block_is_complete(TB_Function* f, TB_Label bb) {
    return TB_IS_NODE_TERMINATOR(f->nodes[f->bbs[bb].end].type);
}

TB_API void tb_inst_set_label(TB_Function* f, TB_Label l) {
    if (!tb_basic_block_is_complete(f, f->current_label)) {
        tb_todo();
    }
    f->current_label = l;
}

TB_API TB_Label tb_inst_get_label(TB_Function* f) {
    return f->current_label;
}

TB_API void tb_inst_goto(TB_Function* f, TB_Label id) {
    tb_assume(id >= 0 && id < f->bb_count);

    TB_Reg r = tb_make_reg(f, TB_GOTO, TB_TYPE_VOID);
    f->nodes[r].goto_.label = id;
}

TB_API TB_Reg tb_inst_if(TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false) {
    assert(cond != TB_NULL_REG);
    TB_Reg r = tb_make_reg(f, TB_IF, TB_TYPE_VOID);
    f->nodes[r].if_.cond = cond;
    f->nodes[r].if_.if_true = if_true;
    f->nodes[r].if_.if_false = if_false;
    return r;
}

TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Reg key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries) {
    // the switch entries are 2 slots each
    size_t param_count = entry_count * 2;
    int param_start = f->vla.count;

    TB_Reg* vla = tb_vla_reserve(f, param_count);
    memcpy(vla, entries, param_count * sizeof(TB_Reg));
    f->vla.count += param_count;

    int param_end = f->vla.count;

    TB_Reg r = tb_make_reg(f, TB_SWITCH, dt);
    f->nodes[r].switch_ = (struct TB_NodeSwitch){
        .key = key,
        .default_label = default_label,
        .entries_start = param_start,
        .entries_end = param_end
    };
}

TB_API void tb_inst_ret(TB_Function* f, TB_Reg value) {
    TB_Reg r = tb_make_reg(f, TB_RET, f->prototype->return_dt);
    f->nodes[r].ret.value = value;
}
