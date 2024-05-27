// IR BUILDER
//
// Handles generating the TB_Function IR via C functions.
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
#include "tb_internal.h"

TB_API void tb_inst_set_trace(TB_Function* f, TB_Trace trace) { f->trace = trace; }
TB_API TB_Trace tb_inst_get_trace(TB_Function* f) { return f->trace; }
TB_Node* tb_inst_get_control(TB_Function* f) { return f->trace.bot_ctrl; }
TB_API TB_Node* tb_inst_root_node(TB_Function* f) { return f->root_node; }

static TB_Node* transfer_ctrl(TB_Function* f, TB_Node* n) {
    TB_Node* prev = f->trace.bot_ctrl;
    f->trace.bot_ctrl = n;
    assert(prev);
    return prev;
}

void tb_inst_set_control(TB_Function* f, TB_Node* control) {
    if (control == NULL) {
        f->trace.top_ctrl = NULL;
        f->trace.bot_ctrl = NULL;
        f->trace.mem = NULL;
    } else {
        assert(control->type == TB_REGION);
        f->trace.top_ctrl = control;
        f->trace.bot_ctrl = control;
        f->trace.mem = TB_NODE_GET_EXTRA_T(control, TB_NodeRegion)->mem_in;
    }
}

TB_Node* tb_inst_region_mem_in(TB_Function* f, TB_Node* region) {
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(region);
    return r->mem_in;
}

TB_Trace tb_inst_trace_from_region(TB_Function* f, TB_Node* region) {
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(region);
    return (TB_Trace){ region, region, r->mem_in };
}

static TB_Node* get_callgraph(TB_Function* f) { return f->root_node->inputs[0]; }
static TB_Node* peek_mem_OLD(TB_Function* f) { return f->trace.mem; }

// adds memory effect to region
static TB_Node* append_mem(TB_Function* f, TB_Node* new_mem) {
    TB_Node* old = f->trace.mem;
    f->trace.mem = new_mem;
    return old;
}

TB_Node* tb__make_proj(TB_Function* f, TB_DataType dt, TB_Node* src, int index) {
    assert(src->dt.type == TB_TAG_TUPLE);
    TB_Node* proj = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(f, proj, src, 0);
    TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = index);
    return proj;
}

bool tb_node_is_constant_int(TB_Function* f, TB_Node* n, uint64_t imm) {
    return n->type == TB_ICONST ? (TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value == imm) : false;
}

bool tb_node_is_constant_non_zero(TB_Node* n) {
    return n->type == TB_ICONST ? (TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value != 0) : false;
}

bool tb_node_is_constant_zero(TB_Node* n) {
    return n->type == TB_ICONST ? (TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value == 0) : false;
}

void tb_function_attrib_variable(TB_Function* f, TB_Node* n, TB_Node* parent, ptrdiff_t len, const char* name, TB_DebugType* type) {
    TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
    l->name = tb__arena_strdup(f->super.module, len, name);
    l->type = type;
}

void tb_function_attrib_scope(TB_Function* f, TB_Node* n, TB_Node* parent) {
}

void tb_inst_location(TB_Function* f, TB_SourceFile* file, int line, int column) {
    if (f->last_loc) {
        // don't place if the line entry already exists
        TB_NodeDbgLoc* last_loc = TB_NODE_GET_EXTRA(f->last_loc);
        if (last_loc->file == file && last_loc->line == line && last_loc->column == column) {
            return;
        }
    }

    // if there's already a line entry, throw this one away (should we replace the original? idk)
    if (f->trace.bot_ctrl->type == TB_PROJ && f->trace.bot_ctrl->inputs[0]->type == TB_DEBUG_LOCATION &&
        // if it's the first debug location, we wanna keep that one because it's placed above the prologue
        f->trace.bot_ctrl->inputs[0]->inputs[0] != f->params[0]
    ) {
        return;
    }

    TB_Node* n = tb_alloc_node(f, TB_DEBUG_LOCATION, TB_TYPE_TUPLE, 2, sizeof(TB_NodeDbgLoc));
    TB_NODE_SET_EXTRA(n, TB_NodeDbgLoc, .file = file, .line = line, .column = column);

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, transfer_ctrl(f, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, append_mem(f, mproj), 1);
    f->last_loc = n;
}

void tb_inst_set_exit_location(TB_Function* f, TB_SourceFile* file, int line, int column) {
}

TB_Node* tb_alloc_node_dyn(TB_Function* f, int type, TB_DataType dt, int input_count, int input_cap, size_t extra) {
    assert(input_count < UINT16_MAX && "too many inputs!");

    // symbol table is a bookkeeping node so storing it separately makes
    // it easier to free during construction.
    TB_Arena* arena = type == TB_SYMBOL_TABLE ? f->tmp_arena : f->arena;
    TB_Node* n = tb_arena_alloc(arena, sizeof(TB_Node) + extra);

    n->type = type;
    n->input_cap = input_cap;
    n->input_count = input_count;
    n->dt = dt;
    n->gvn = f->node_count++;

    if (input_cap > 0) {
        n->inputs = tb_arena_alloc(arena, input_cap * sizeof(TB_Node*));
        FOR_N(i, 0, input_cap) { n->inputs[i] = NULL; }
    } else {
        n->inputs = NULL;
    }

    // most nodes don't have many users, although the ones which do will have a shit load (root node)
    n->user_count = 0;
    n->user_cap   = 4;
    n->users = tb_arena_alloc(arena, 4 * sizeof(TB_User));
    memset(n->users, 0xF0, 4 * sizeof(TB_User));

    if (extra > 0) {
        memset(n->extra, 0, extra);
    }
    return n;
}

TB_Node* tb_alloc_node(TB_Function* f, int type, TB_DataType dt, int input_count, size_t extra) {
    return tb_alloc_node_dyn(f, type, dt, input_count, input_count, extra);
}

static TB_Node* tb_bin_arith(TB_Function* f, int type, TB_ArithmeticBehavior arith_behavior, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeBinopInt, .ab = arith_behavior);
    return n;
}

static TB_Node* tb_bin_farith(TB_Function* f, int type, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, 0);
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    return n;
}

static TB_Node* tb_unary(TB_Function* f, int type, TB_DataType dt, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, type, dt, 2, 0);
    set_input(f, n, src, 1);
    return n;
}

TB_Node* tb_inst_trunc(TB_Function* f, TB_Node* src, TB_DataType dt) {
    if (src->dt.type == TB_TAG_F64 && dt.type == TB_TAG_F32) {
        return tb_unary(f, TB_FLOAT_TRUNC, dt, src);
    } else {
        assert((src->dt.type == TB_TAG_INT || src->dt.type == TB_TAG_PTR)
            && (dt.type == TB_TAG_INT || dt.type == TB_TAG_PTR));

        return tb_unary(f, TB_TRUNCATE, dt, src);
    }
}

TB_Node* tb_inst_int2ptr(TB_Function* f, TB_Node* src) {
    return tb_unary(f, TB_BITCAST, TB_TYPE_PTR, src);
}

TB_Node* tb_inst_ptr2int(TB_Function* f, TB_Node* src, TB_DataType dt) {
    return tb_unary(f, TB_BITCAST, dt, src);
}

TB_Node* tb_inst_int2float(TB_Function* f, TB_Node* src, TB_DataType dt, bool is_signed) {
    assert(TB_IS_FLOAT_TYPE(dt));
    assert(src->dt.type == TB_TAG_INT);

    if (src->type == TB_ICONST) {
        uint64_t y = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        if (is_signed) {
            y = tb__sxt(y, src->dt.data, 64);
        }

        if (dt.type == TB_TAG_F32) {
            float x;
            if (is_signed) x = (int64_t) y;
            else x = (uint64_t) y;

            return tb_inst_float32(f, x);
        } else if (dt.type == TB_TAG_F64) {
            double x;
            if (is_signed) x = (int64_t) y;
            else x = (uint64_t) y;

            return tb_inst_float64(f, x);
        }
    }

    return tb_unary(f, is_signed ? TB_INT2FLOAT : TB_UINT2FLOAT, dt, src);
}

TB_Node* tb_inst_float2int(TB_Function* f, TB_Node* src, TB_DataType dt, bool is_signed) {
    return tb_unary(f, is_signed ? TB_FLOAT2INT : TB_FLOAT2UINT, dt, src);
}

TB_Node* tb_inst_fpxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    return tb_unary(f, TB_FLOAT_EXT, dt, src);
}

TB_Node* tb_inst_sxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    if (src->type == TB_ICONST) {
        uint64_t y = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        y = tb__sxt(y, src->dt.data, 64);
        return tb_inst_uint(f, dt, y);
    }

    return tb_unary(f, TB_SIGN_EXT, dt, src);
}

TB_Node* tb_inst_zxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    if (src->type == TB_ICONST) {
        uint64_t y = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        return tb_inst_uint(f, dt, y);
    }

    return tb_unary(f, TB_ZERO_EXT, dt, src);
}

TB_Node* tb_inst_bitcast(TB_Function* f, TB_Node* src, TB_DataType dt) {
    // shaw uses bitcast for zero extends sometimes, i don't feel like changing minivm
    // so i'll just extend the semantics of bitcast to consider zero extension the behavior
    // when scaling up.
    if (src->dt.type == TB_TAG_INT && dt.type == TB_TAG_INT && src->dt.data < dt.data) {
        return tb_unary(f, TB_ZERO_EXT, dt, src);
    }

    return tb_unary(f, TB_BITCAST, dt, src);
}

TB_Node* tb_inst_param(TB_Function* f, int param_id) {
    assert(param_id < f->param_count);
    return f->params[3 + param_id];
}

void tb_get_data_type_size(TB_Module* mod, TB_DataType dt, size_t* size, size_t* align) {
    const ICodeGen* restrict code_gen = tb_codegen_info(mod);
    code_gen->get_data_type_size(dt, size, align);
}

void tb_inst_unreachable(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_UNREACHABLE, TB_TYPE_CONTROL, 2, 0);
    set_input(f, n, transfer_ctrl(f, n), 0);
    set_input(f, n, peek_mem_OLD(f), 1);
    add_input_late(f, f->root_node, n);
    f->trace.bot_ctrl = NULL;
}

void tb_inst_debugbreak(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_DEBUGBREAK, TB_TYPE_CONTROL, 1, 0);
    set_input(f, n, transfer_ctrl(f, n), 0);
}

void tb_inst_trap(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_TRAP, TB_TYPE_CONTROL, 2, 0);
    set_input(f, n, transfer_ctrl(f, n), 0);
    set_input(f, n, peek_mem_OLD(f), 1);
    add_input_late(f, f->root_node, n);
    f->trace.bot_ctrl = NULL;
}

TB_Node* tb_inst_local(TB_Function* f, TB_CharUnits size, TB_CharUnits alignment) {
    assert(size > 0);
    assert(alignment > 0 && tb_is_power_of_two(alignment));

    // insert in the entry block
    TB_Node* n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 1, sizeof(TB_NodeLocal));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeLocal, .size = size, .align = alignment);
    return n;
}

void tb_inst_safepoint_poll(TB_Function* f, void* tag, TB_Node* addr, int input_count, TB_Node** inputs) {
    TB_Node* n = tb_alloc_node(f, TB_SAFEPOINT_POLL, TB_TYPE_CONTROL, 3 + input_count, sizeof(TB_NodeSafepoint));
    set_input(f, n, transfer_ctrl(f, n), 0);
    set_input(f, n, peek_mem_OLD(f), 1);
    set_input(f, n, addr, 2);
    FOR_N(i, 0, input_count) {
        set_input(f, n, inputs[i], i + 3);
    }
    TB_NODE_SET_EXTRA(n, TB_NodeSafepoint, tag);
}

TB_Node* tb_inst_load(TB_Function* f, TB_DataType dt, TB_Node* addr, TB_CharUnits alignment, bool is_volatile) {
    assert(addr);

    if (is_volatile) {
        TB_Node* n = tb_alloc_node(f, TB_READ, TB_TYPE_TUPLE, 3, 0);
        set_input(f, n, f->trace.bot_ctrl, 0);
        set_input(f, n, peek_mem_OLD(f), 1);
        set_input(f, n, addr, 2);
        append_mem(f, tb__make_proj(f, TB_TYPE_MEMORY, n, 0));
        return tb__make_proj(f, dt, n, 1);
    } else {
        TB_Node* n = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeMemAccess));
        set_input(f, n, f->trace.bot_ctrl, 0);
        set_input(f, n, peek_mem_OLD(f), 1);
        set_input(f, n, addr, 2);
        TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment);
        return n;
    }
}

void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Node* addr, TB_Node* val, uint32_t alignment, bool is_volatile) {
    assert(TB_DATA_TYPE_EQUALS(dt, val->dt));

    TB_Node* n;
    if (is_volatile) {
        n = tb_alloc_node(f, TB_WRITE, TB_TYPE_MEMORY, 4, 0);
    } else {
        n = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
        TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment);
    }
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, append_mem(f, n), 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
}

void tb_inst_memset(TB_Function* f, TB_Node* addr, TB_Node* val, TB_Node* size, TB_CharUnits align) {
    assert(TB_IS_POINTER_TYPE(addr->dt));
    assert(TB_IS_INTEGER_TYPE(val->dt) && val->dt.data == 8);

    TB_Node* n = tb_alloc_node(f, TB_MEMSET, TB_TYPE_MEMORY, 5, sizeof(TB_NodeMemAccess));
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, append_mem(f, n), 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    set_input(f, n, size, 4);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
}

void tb_inst_memcpy(TB_Function* f, TB_Node* addr, TB_Node* val, TB_Node* size, TB_CharUnits align) {
    assert(TB_IS_POINTER_TYPE(addr->dt));
    assert(TB_IS_POINTER_TYPE(val->dt));

    TB_Node* n = tb_alloc_node(f, TB_MEMCPY, TB_TYPE_MEMORY, 5, sizeof(TB_NodeMemAccess));
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, append_mem(f, n), 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    set_input(f, n, size, 4);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align);
}

void tb_inst_memzero(TB_Function* f, TB_Node* dst, TB_Node* count, TB_CharUnits align) {
    tb_inst_memset(f, dst, tb_inst_uint(f, TB_TYPE_I8, 0), count, align);
}

TB_Node* tb_inst_bool(TB_Function* f, bool imm) {
    TB_Node* n = tb_alloc_node(f, TB_ICONST, TB_TYPE_BOOL, 1, sizeof(TB_NodeInt));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = imm);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_uint(TB_Function* f, TB_DataType dt, uint64_t imm) {
    assert(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));

    if (dt.type == TB_TAG_INT && dt.data < 64) {
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
        imm &= mask;
    }

    TB_Node* n = tb_alloc_node(f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = imm);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_sint(TB_Function* f, TB_DataType dt, int64_t imm) {
    assert(TB_IS_POINTER_TYPE(dt) || (TB_IS_INTEGER_TYPE(dt) && (dt.data <= 64)));

    TB_Node* n = tb_alloc_node(f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = imm);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_float32(TB_Function* f, float imm) {
    TB_Node* n = tb_alloc_node(f, TB_F32CONST, TB_TYPE_F32, 1, sizeof(TB_NodeFloat32));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat32, .value = imm);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_float64(TB_Function* f, double imm) {
    TB_Node* n = tb_alloc_node(f, TB_F64CONST, TB_TYPE_F64, 1, sizeof(TB_NodeFloat64));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat64, .value = imm);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_string(TB_Function* f, size_t len, const char* str) {
    TB_Global* dummy = tb_global_create(f->super.module, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
    tb_global_set_storage(f->super.module, tb_module_get_rdata(f->super.module), dummy, len, 1, 1);

    char* dst = tb_global_add_region(f->super.module, dummy, 0, len);
    memcpy(dst, str, len);

    return tb_inst_get_symbol_address(f, (TB_Symbol*) dummy);
}

TB_Node* tb_inst_cstring(TB_Function* f, const char* str) {
    return tb_inst_string(f, strlen(str) + 1, str);
}

TB_Node* tb_inst_array_access(TB_Function* f, TB_Node* base, TB_Node* index, int64_t stride) {
    if (index->type == TB_ICONST) {
        uint64_t x = TB_NODE_GET_EXTRA_T(index, TB_NodeInt)->value;
        return tb_inst_member_access(f, base, x * stride);
    }

    assert(stride != 0);
    TB_Node* con = tb_inst_sint(f, TB_TYPE_I64, stride);
    TB_Node* scl = index;
    if (stride != 1) {
        scl = tb_alloc_node(f, TB_MUL, TB_TYPE_I64, 3, sizeof(TB_NodeBinopInt));
        set_input(f, scl, index, 1);
        set_input(f, scl, con,   2);
    }

    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, scl, 2);
    return n;
}

TB_Node* tb_inst_member_access(TB_Function* f, TB_Node* base, int64_t offset) {
    if (offset == 0) {
        return base;
    }

    TB_Node* con = tb_inst_sint(f, TB_TYPE_I64, offset);
    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
    set_input(f, n, base, 1);
    set_input(f, n, con,  2);
    return n;
}

TB_Node* tb_inst_get_symbol_address(TB_Function* f, TB_Symbol* target) {
    assert(target != NULL);

    TB_Node* n = tb_alloc_node(f, TB_SYMBOL, TB_TYPE_PTR, 1, sizeof(TB_NodeSymbol));
    set_input(f, n, f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeSymbol, .sym = target);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_syscall(TB_Function* f, TB_DataType dt, TB_Node* syscall_num, size_t param_count, TB_Node** params) {
    TB_Node* n = tb_alloc_node(f, TB_SYSCALL, TB_TYPE_TUPLE, 3 + param_count, sizeof(TB_NodeCall));
    set_input(f, n, syscall_num, 2);
    FOR_N(i, 0, param_count) {
        set_input(f, n, params[i], i + 3);
    }

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, transfer_ctrl(f, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, append_mem(f, mproj), 1);

    // return value
    TB_Node* dproj = tb__make_proj(f, dt, n, 2);

    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
    c->proj_count = 3;
    c->proto = NULL;
    return dproj;
}

TB_MultiOutput tb_inst_call(TB_Function* f, TB_FunctionPrototype* proto, TB_Node* target, size_t param_count, TB_Node** params) {
    size_t proj_count = 2 + (proto->return_count > 1 ? proto->return_count : 1);

    TB_Node* n = tb_alloc_node(f, TB_CALL, TB_TYPE_TUPLE, 3 + param_count, sizeof(TB_NodeCall));
    set_input(f, n, target, 2);
    FOR_N(i, 0, param_count) {
        set_input(f, n, params[i], i + 3);
    }

    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
    c->proj_count = proj_count;
    c->proto = proto;

    // control proj
    TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
    set_input(f, n, transfer_ctrl(f, cproj), 0);

    // memory proj
    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 1);
    set_input(f, n, append_mem(f, mproj), 1);

    // leaked into the IR arena, don't care rn
    TB_Node** projs = tb_arena_alloc(f->arena, sizeof(TB_Node*)*proj_count);

    // create data projections
    TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(proto);
    FOR_N(i, 0, proto->return_count) {
        projs[i + 2] = tb__make_proj(f, rets[i].dt, n, i + 2);
    }

    // we'll slot a NULL so it's easy to tell when it's empty
    if (proto->return_count == 0) {
        projs[2] = NULL;
    }

    projs[0] = cproj;
    projs[1] = mproj;
    add_input_late(f, get_callgraph(f), n);

    if (proto->return_count == 1) {
        return (TB_MultiOutput){ .count = proto->return_count, .single = projs[2] };
    } else {
        return (TB_MultiOutput){ .count = proto->return_count, .multiple = &projs[2] };
    }
}

void tb_inst_tailcall(TB_Function* f, TB_FunctionPrototype* proto, TB_Node* target, size_t param_count, TB_Node** params) {
    TB_Node* n = tb_alloc_node(f, TB_TAILCALL, TB_TYPE_CONTROL, 3 + param_count, sizeof(TB_NodeTailcall));
    set_input(f, n, transfer_ctrl(f, n), 0);
    set_input(f, n, peek_mem_OLD(f), 1);
    set_input(f, n, target, 2);
    FOR_N(i, 0, param_count) {
        set_input(f, n, params[i], i + 3);
    }

    TB_NodeTailcall* c = TB_NODE_GET_EXTRA(n);
    c->proto = proto;

    add_input_late(f, get_callgraph(f), n);
    add_input_late(f, f->root_node, n);
}

TB_Node* tb_inst_poison(TB_Function* f, TB_DataType dt) {
    TB_Node* n = tb_alloc_node(f, TB_POISON, dt, 1, 0);
    set_input(f, n, f->root_node, 0);
    return tb_opt_gvn_node(f, n);
}

TB_Node* tb_inst_not(TB_Function* f, TB_Node* src) {
    return tb_inst_xor(f, src, tb_inst_sint(f, src->dt, -1));
}

TB_Node* tb_inst_bswap(TB_Function* f, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, TB_BSWAP, src->dt, 2, 0);
    set_input(f, n, src, 1);
    return n;
}

TB_Node* tb_inst_clz(TB_Function* f, TB_Node* src) {
    assert(TB_IS_INTEGER_TYPE(src->dt));
    TB_Node* n = tb_alloc_node(f, TB_CLZ, TB_TYPE_I32, 2, 0);
    set_input(f, n, src, 1);
    return n;
}

TB_Node* tb_inst_ctz(TB_Function* f, TB_Node* src) {
    assert(TB_IS_INTEGER_TYPE(src->dt));
    TB_Node* n = tb_alloc_node(f, TB_CTZ, TB_TYPE_I32, 2, 0);
    set_input(f, n, src, 1);
    return n;
}

TB_Node* tb_inst_popcount(TB_Function* f, TB_Node* src) {
    assert(TB_IS_INTEGER_TYPE(src->dt));
    TB_Node* n = tb_alloc_node(f, TB_POPCNT, TB_TYPE_I32, 2, 0);
    set_input(f, n, src, 1);
    return n;
}

TB_Node* tb_inst_neg(TB_Function* f, TB_Node* src) {
    TB_DataType dt = src->dt;
    if (src->type == TB_ICONST) {
        uint64_t x = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->value;
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

        // two's complement negate is just invert and add 1
        uint64_t negated = (~x + 1) & mask;
        return tb_inst_sint(f, dt, negated);
    } else if (src->type == TB_F32CONST) {
        float x = TB_NODE_GET_EXTRA_T(src, TB_NodeFloat32)->value;
        return tb_inst_float32(f, -x);
    } else if (src->type == TB_F64CONST) {
        double x = TB_NODE_GET_EXTRA_T(src, TB_NodeFloat64)->value;
        return tb_inst_float64(f, -x);
    } else if (TB_IS_FLOAT_TYPE(src->dt)) {
        return tb_unary(f, TB_FNEG, src->dt, src);
    } else {
        return tb_unary(f, TB_NEG, src->dt, src);
    }
}

TB_Node* tb_inst_select(TB_Function* f, TB_Node* cond, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, TB_SELECT, a->dt, 4, 0);
    set_input(f, n, cond, 1);
    set_input(f, n, a, 2);
    set_input(f, n, b, 3);
    return n;
}

TB_Node* tb_inst_and(TB_Function* f, TB_Node* a, TB_Node* b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_AND, 0, a, b);
}

TB_Node* tb_inst_or(TB_Function* f, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));
    return tb_bin_arith(f, TB_OR, 0, a, b);
}

TB_Node* tb_inst_xor(TB_Function* f, TB_Node* a, TB_Node* b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_XOR, 0, a, b);
}

TB_Node* tb_inst_add(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_ADD, arith_behavior, a, b);
}

TB_Node* tb_inst_sub(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SUB, arith_behavior, a, b);
}

TB_Node* tb_inst_mul(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_MUL, arith_behavior, a, b);
}

TB_Node* tb_inst_div(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    TB_Node* peek = b->type == TB_SIGN_EXT ? b->inputs[1] : b;
    if (peek->type == TB_ICONST) {
        TB_NodeInt* i = TB_NODE_GET_EXTRA(peek);
        uint64_t log2 = tb_ffs(i->value) - 1;
        if (i->value == UINT64_C(1) << log2) {
            return tb_bin_arith(f, TB_SHR, 0, a, tb_inst_uint(f, a->dt, log2));
        }
    }

    // division can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SDIV : TB_UDIV, 0, a, b);
}

TB_Node* tb_inst_mod(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    // modulo can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SMOD : TB_UMOD, 0, a, b);
}

TB_Node* tb_inst_shl(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SHL, arith_behavior, a, b);
}

TB_Node* tb_inst_rol(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_arith(f, TB_ROL, 0, a, b);
}

TB_Node* tb_inst_ror(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_arith(f, TB_ROR, 0, a, b);
}

////////////////////////////////
// Atomics
////////////////////////////////
static TB_Node* atomic_op(TB_Function* f, int op, TB_DataType dt, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, op, TB_TYPE_TUPLE, src ? 4 : 3, sizeof(TB_NodeAtomic));
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, addr, 2);
    if (src) {
        set_input(f, n, src, 3);
    }

    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 0);
    TB_Node* dproj = tb__make_proj(f, dt, n, 1);
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    // memory proj
    set_input(f, n, append_mem(f, mproj), 1);
    return dproj;
}

TB_Node* tb_inst_atomic_load(TB_Function* f, TB_Node* addr, TB_DataType dt, TB_MemoryOrder order) {
    return atomic_op(f, TB_ATOMIC_LOAD, dt, addr, NULL, order);
}

TB_Node* tb_inst_atomic_xchg(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    assert(src);
    return atomic_op(f, TB_ATOMIC_XCHG, src->dt, addr, src, order);
}

TB_Node* tb_inst_atomic_add(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    assert(src);
    return atomic_op(f, TB_ATOMIC_ADD, src->dt, addr, src, order);
}

TB_Node* tb_inst_atomic_and(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    assert(src);
    return atomic_op(f, TB_ATOMIC_AND, src->dt, addr, src, order);
}

TB_Node* tb_inst_atomic_xor(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    assert(src);
    return atomic_op(f, TB_ATOMIC_XOR, src->dt, addr, src, order);
}

TB_Node* tb_inst_atomic_or(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    assert(src);
    return atomic_op(f, TB_ATOMIC_OR, src->dt, addr, src, order);
}

TB_Node* tb_inst_atomic_cmpxchg(TB_Function* f, TB_Node* addr, TB_Node* expected, TB_Node* desired, TB_MemoryOrder succ, TB_MemoryOrder fail) {
    assert(TB_DATA_TYPE_EQUALS(desired->dt, expected->dt));
    TB_DataType dt = desired->dt;

    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_CAS, TB_TYPE_TUPLE, 5, sizeof(TB_NodeAtomic));
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, addr, 2);
    set_input(f, n, expected, 3);
    set_input(f, n, desired, 4);

    TB_Node* mproj = tb__make_proj(f, TB_TYPE_MEMORY, n, 0);
    TB_Node* dproj = tb__make_proj(f, dt, n, 1);
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = succ, .order2 = fail);

    // memory proj
    set_input(f, n, append_mem(f, mproj), 1);
    return dproj;
}

// TODO(NeGate): Maybe i should split the bitshift operations into a separate kind of
// operator that has different arithmatic behaviors, maybe like trap on a large shift amount
TB_Node* tb_inst_sar(TB_Function* f, TB_Node* a, TB_Node* b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SAR, 0, a, b);
}

TB_Node* tb_inst_shr(TB_Function* f, TB_Node* a, TB_Node* b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SHR, 0, a, b);
}

TB_Node* tb_inst_fadd(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FADD, a, b);
}

TB_Node* tb_inst_fsub(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FSUB, a, b);
}

TB_Node* tb_inst_fmul(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FMUL, a, b);
}

TB_Node* tb_inst_fdiv(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FDIV, a, b);
}

TB_Node* tb_inst_va_start(TB_Function* f, TB_Node* a) {
    assert(a->type == TB_LOCAL);

    TB_Node* n = tb_alloc_node(f, TB_VA_START, TB_TYPE_PTR, 2, 0);
    set_input(f, n, a, 1);
    return n;
}

TB_Node* tb_inst_x86_ldmxcsr(TB_Function* f, TB_Node* a) {
    assert(a->dt.type == TB_TAG_INT && a->dt.data == 32);

    TB_Node* n = tb_alloc_node(f, TB_X86INTRIN_LDMXCSR, TB_TYPE_I32, 2, 0);
    set_input(f, n, a, 1);
    return n;
}

TB_Node* tb_inst_cycle_counter(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_CYCLE_COUNTER, TB_TYPE_I64, 1, 0);
    set_input(f, n, f->trace.bot_ctrl, 0);
    return n;
}

TB_Node* tb_inst_prefetch(TB_Function* f, TB_Node* addr, int level) {
    TB_Node* n = tb_alloc_node(f, TB_PREFETCH, TB_TYPE_MEMORY, 2, sizeof(TB_NodePrefetch));
    set_input(f, n, addr, 1);
    TB_NODE_SET_EXTRA(n, TB_NodePrefetch, .level = level);
    return n;
}

TB_Node* tb_inst_x86_stmxcsr(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_X86INTRIN_STMXCSR, TB_TYPE_I32, 1, 0);
    set_input(f, n, f->trace.bot_ctrl, 0);
    return n;
}

TB_Node* tb_inst_x86_sqrt(TB_Function* f, TB_Node* a) {
    return tb_unary(f, TB_X86INTRIN_SQRT, a->dt, a);
}

TB_Node* tb_inst_x86_rsqrt(TB_Function* f, TB_Node* a) {
    return tb_unary(f, TB_X86INTRIN_RSQRT, a->dt, a);
}

TB_Node* tb_inst_cmp(TB_Function* f, TB_NodeType type, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->dt);
    return n;
}

TB_Node* tb_inst_cmp_eq(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_EQ, a, b);
}

TB_Node* tb_inst_cmp_ne(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_NE, a, b);
}

TB_Node* tb_inst_cmp_ilt(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, a, b);
}

TB_Node* tb_inst_cmp_ile(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, a, b);
}

TB_Node* tb_inst_cmp_igt(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, b, a);
}

TB_Node* tb_inst_cmp_ige(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, b, a);
}

TB_Node* tb_inst_cmp_flt(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLT, a, b);
}

TB_Node* tb_inst_cmp_fle(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLE, a, b);
}

TB_Node* tb_inst_cmp_fgt(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLT, b, a);
}

TB_Node* tb_inst_cmp_fge(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLE, b, a);
}

TB_Node* tb_inst_incomplete_phi(TB_Function* f, TB_DataType dt, TB_Node* region, size_t preds) {
    TB_Node* n = tb_alloc_node(f, TB_PHI, dt, 1 + preds, 0);
    set_input(f, n, region, 0);
    return n;
}

bool tb_inst_add_phi_operand(TB_Function* f, TB_Node* phi, TB_Node* ctrl, TB_Node* val) {
    TB_Node* phi_region = phi->inputs[0];

    // the slot to fill is based on the predecessor list of the region
    FOR_N(i, 0, phi_region->input_count) {
        TB_Node* pred = phi_region->inputs[i];
        for (;;) {
            if (pred == ctrl) {
                set_input(f, phi, val, i + 1);
                return true;
            } else if (cfg_is_region(pred)) {
                break;
            }
            pred = pred->inputs[0];
        }
    }

    return false;
}

TB_Node* tb_inst_phi2(TB_Function* f, TB_Node* region, TB_Node* a, TB_Node* b) {
    assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node_dyn(f, TB_PHI, a->dt, 3, 3, 0);
    set_input(f, n, region, 0);
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    return n;
}

TB_API TB_Node* tb_inst_region(TB_Function* f) {
    return tb_inst_new_trace(f).top_ctrl;
}

TB_API TB_Trace tb_inst_new_trace(TB_Function* f) {
    TB_Node* n = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 0, 4, sizeof(TB_NodeRegion));

    TB_Node* phi = tb_alloc_node_dyn(f, TB_PHI, TB_TYPE_MEMORY, 1, 5, 0);
    set_input(f, phi, n, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeRegion, .mem_in = phi);
    return (TB_Trace){ n, n, phi };
}

void tb_inst_set_region_name(TB_Function* f, TB_Node* n, ptrdiff_t len, const char* name) {
    if (len < 0) len = strlen(name);

    TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

    char* newstr = tb_arena_alloc(f->arena, len + 1);
    memcpy(newstr, name, len + 1);
    r->tag = newstr;
}

static void add_memory_edge(TB_Function* f, TB_Node* n, TB_Node* mem_state, TB_Node* target) {
    assert(target->type == TB_REGION);
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(target);
    assert(r->mem_in && r->mem_in->type == TB_PHI);
    add_input_late(f, r->mem_in, mem_state);
}

void tb_inst_goto(TB_Function* f, TB_Node* target) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // there's no need for a branch if the path isn't diverging.
    TB_Node* n = f->trace.bot_ctrl;
    f->trace.bot_ctrl = NULL;

    // just add the edge directly.
    assert(n->dt.type == TB_TAG_CONTROL);
    add_input_late(f, target, n);
    add_memory_edge(f, n, mem_state, target);
}

void tb_inst_never_branch(TB_Function* f, TB_Node* if_true, TB_Node* if_false) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_NEVER_BRANCH, TB_TYPE_TUPLE, 1, 0);
    set_input(f, n, transfer_ctrl(f, NULL), 0);

    FOR_N(i, 0, 2) {
        TB_Node* target = i ? if_false : if_true;
        TB_Node* cproj = tb__make_proj(f, TB_TYPE_CONTROL, n, i);

        add_input_late(f, target, cproj);
        add_memory_edge(f, n, mem_state, target);
    }
}

TB_Node* tb_inst_if(TB_Function* f, TB_Node* cond, TB_Node* if_true, TB_Node* if_false) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch));
    set_input(f, n, transfer_ctrl(f, NULL), 0);
    set_input(f, n, cond, 1);

    FOR_N(i, 0, 2) {
        TB_Node* target = i ? if_false : if_true;

        TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
        set_input(f, cproj, n, 0);
        TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = i, .taken = 50, .key = 0);

        add_input_late(f, target, cproj);
        add_memory_edge(f, n, mem_state, target);
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->total_hits = 100;
    br->succ_count  = 2;
    return n;
}

TB_Node* tb_inst_if2(TB_Function* f, TB_Node* cond, TB_Node* projs[2]) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch));
    set_input(f, n, transfer_ctrl(f, NULL), 0);
    set_input(f, n, cond, 1);

    FOR_N(i, 0, 2) {
        TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
        set_input(f, cproj, n, 0);
        TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = i, .taken = 50, .key = 0);
        projs[i] = cproj;
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->total_hits = 100;
    br->succ_count = 2;
    return n;
}

// n is a TB_BRANCH with two successors, taken is the number of times it's true
void tb_inst_set_branch_freq(TB_Function* f, TB_Node* n, int total_hits, int taken) {
    tb_todo();
}

TB_Node* tb_inst_branch(TB_Function* f, TB_DataType dt, TB_Node* key, TB_Node* default_label, size_t entry_count, const TB_SwitchEntry* entries) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch));
    set_input(f, n, transfer_ctrl(f, NULL), 0);
    set_input(f, n, key, 1);

    FOR_N(i, 0, 1 + entry_count) {
        TB_Node* target = i ? entries[i - 1].value : default_label;

        TB_Node* cproj = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
        set_input(f, cproj, n, 0);
        if (i > 0) {
            TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = i, .taken = 10, .key = entries[i].key);
        } else {
            TB_NODE_SET_EXTRA(cproj, TB_NodeBranchProj, .index = i, .taken = 10, .key = 0);
        }

        add_input_late(f, target, cproj);
        add_memory_edge(f, n, mem_state, target);
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->total_hits = (1 + entry_count) * 10;
    br->succ_count = 1 + entry_count;
    return n;
}

void tb_inst_ret(TB_Function* f, size_t count, TB_Node** values) {
    TB_Node* mem_state = peek_mem_OLD(f);

    // allocate return node
    TB_Node* ret = f->root_node->inputs[1];
    assert(ret->type == TB_RETURN);
    TB_Node* ctrl = ret->inputs[0];
    assert(ctrl->type == TB_REGION);

    // add to PHIs
    assert(ret->input_count >= 3 + count);
    add_input_late(f, ret->inputs[1], mem_state);

    size_t i = 3;
    for (; i < count + 3; i++) {
        assert(ret->inputs[i]->dt.raw == values[i - 3]->dt.raw && "datatype mismatch");
        add_input_late(f, ret->inputs[i], values[i - 3]);
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

    // basically just tb_inst_goto without the memory PHI (we did it earlier)
    TB_Node* n = f->trace.bot_ctrl;
    f->trace.bot_ctrl = NULL;

    add_input_late(f, ctrl, n);
}
