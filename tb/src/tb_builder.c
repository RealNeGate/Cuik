// IR BUILDER
//
// Handles generating the TB_Function IR via C functions.
// Note that these functions can perform certain simple
// optimizations while the generation happens to improve
// the machine code output or later analysis stages.
#include "tb_internal.h"

TB_API bool tb_node_is_constant_int(TB_Function* f, TB_Node* n, uint64_t imm) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (n->type == TB_INTEGER_CONST && i->num_words == 1) {
        return (i->words[0] == imm);
    }

    return false;
}

TB_API bool tb_node_is_constant_non_zero(TB_Node* n) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (n->type == TB_INTEGER_CONST) {
        if (i->num_words == 1) {
            if (i->words[0] != 0) {
                return true;
            }
        } else {
            if (!BigInt_is_zero(i->num_words, i->words)) {
                return true;
            }
        }
    }

    return false;
}

TB_API bool tb_node_is_constant_zero(TB_Node* n) {
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    if (n->type == TB_INTEGER_CONST) {
        if (i->num_words == 1) {
            if (i->words[0] == 0) {
                return true;
            }
        } else {
            if (BigInt_is_zero(i->num_words, i->words)) {
                return true;
            }
        }
    }

    return false;
}

static void append_attrib(TB_Function* f, TB_Node* n, TB_Attrib* a) {
    TB_Attrib* chain = n->first_attrib;
    if (chain == NULL) {
        n->first_attrib = a;
    } else {
        // skip till the end
        while (chain->next != NULL) chain = chain->next;

        chain->next = a;
    }
}

TB_API void tb_function_attrib_variable(TB_Function* f, TB_Node* n, const char* name, TB_DebugType* type) {
    assert(name != NULL);
    assert(type != NULL);

    TB_Attrib* a = tb_platform_heap_alloc(sizeof(TB_Attrib));
    *a = (TB_Attrib) { .type = TB_ATTRIB_VARIABLE, .var = { tb__arena_strdup(f->super.module, name), type } };
    append_attrib(f, n, a);
}

static void* alloc_from_node_arena(TB_Function* f, size_t necessary_size) {
    assert(f->arena);
    return f->arena->alloc(f->arena, necessary_size, _Alignof(TB_Node));
}

TB_Node* tb_alloc_node(TB_Function* f, int type, TB_DataType dt, int input_count, size_t extra) {
    assert(input_count < UINT16_MAX && "too many inputs!");
    f->node_count += 1;

    if (type == TB_REGION) {
        f->control_node_count += 1;
    }

    TB_Node* n = alloc_from_node_arena(f, sizeof(TB_Node) + extra);
    n->type = type;
    n->dt = dt;
    n->input_count = input_count;
    n->extra_count = extra;
    n->first_attrib = NULL;
    if (input_count > 0) {
        n->inputs = alloc_from_node_arena(f, input_count * sizeof(TB_Node*));
    } else {
        n->inputs = NULL;
    }

    if (n->extra_count > 0) {
        memset(n->extra, 0, n->extra_count);
    }

    if (f->line_attrib != NULL) {
        append_attrib(f, n, f->line_attrib);
    }

    return n;
}

static TB_Node* tb_bin_arith(TB_Function* f, int type, TB_ArithmeticBehavior arith_behavior, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, a->dt, 2, sizeof(TB_NodeBinopInt));
    n->inputs[0] = a;
    n->inputs[1] = b;
    TB_NODE_SET_EXTRA(n, TB_NodeBinopInt, .ab = arith_behavior);
    return n;
}

static TB_Node* tb_bin_farith(TB_Function* f, int type, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, a->dt, 2, 0);
    n->inputs[0] = a;
    n->inputs[1] = b;
    return n;
}

TB_API TB_Node* tb_inst_trunc(TB_Function* f, TB_Node* src, TB_DataType dt) {
    tb_assume(src->dt.width == dt.width);

    TB_Node* n = tb_alloc_node(f, TB_TRUNCATE, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_int2ptr(TB_Function* f, TB_Node* src) {
    tb_assume(src->dt.width == 0);

    TB_Node* n = tb_alloc_node(f, TB_INT2PTR, TB_TYPE_PTR, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_ptr2int(TB_Function* f, TB_Node* src, TB_DataType dt) {
    tb_assume(dt.width == 0);
    tb_assume(src->dt.width == 0);

    TB_Node* n = tb_alloc_node(f, TB_PTR2INT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_int2float(TB_Function* f, TB_Node* src, TB_DataType dt, bool is_signed) {
    tb_assume(dt.type == TB_FLOAT);
    tb_assume(src->dt.type == TB_INT);
    tb_assume(src->dt.width == dt.width);

    TB_NodeInt* i = TB_NODE_GET_EXTRA(src);
    if (src->type == TB_INTEGER_CONST && i->num_words == 1) {
        uint64_t y = i->words[0];
        if (is_signed) {
            y = tb__sxt(y, src->dt.data, 64);
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

    TB_Node* n = tb_alloc_node(f, is_signed ? TB_INT2FLOAT : TB_UINT2FLOAT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_float2int(TB_Function* f, TB_Node* src, TB_DataType dt, bool is_signed) {
    tb_assume(src->dt.width == dt.width);

    TB_Node* n = tb_alloc_node(f, is_signed ? TB_FLOAT2INT : TB_FLOAT2UINT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_fpxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    tb_assume(dt.width == src->dt.width);

    TB_Node* n = tb_alloc_node(f, TB_FLOAT_EXT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_sxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    tb_assume(dt.width == src->dt.width);

    TB_Node* n = tb_alloc_node(f, TB_SIGN_EXT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_zxt(TB_Function* f, TB_Node* src, TB_DataType dt) {
    tb_assume(dt.width == src->dt.width);

    TB_Node* n = tb_alloc_node(f, TB_ZERO_EXT, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_bitcast(TB_Function* f, TB_Node* src, TB_DataType dt) {
    // TODO(NeGate): Do some size checks
    TB_Node* n = tb_alloc_node(f, TB_BITCAST, dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_param(TB_Function* f, int param_id) {
    tb_assume(param_id < f->prototype->param_count);
    return TB_NODE_GET_EXTRA_T(f->start_node, TB_NodeRegion)->projs[param_id];
}

TB_API TB_Node* tb_inst_param_addr(TB_Function* f, int param_id) {
    tb_assume(param_id < f->prototype->param_count);
    const ICodeGen* restrict code_gen = tb__find_code_generator(f->super.module);

    TB_Node* param = tb_inst_param(f, param_id);
    TB_CharUnits size, align;
    if (code_gen) {
        code_gen->get_data_type_size(param->dt, &size, &align);
    } else {
        size = align = 8;
    }

    TB_Node* n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 0, sizeof(TB_NodeLocal));
    TB_NODE_SET_EXTRA(n, TB_NodeLocal, .size = size, .align = align);

    TB_Node* n2 = tb_alloc_node(f, TB_STORE, param->dt, 3, sizeof(TB_NodeMemAccess));
    n2->inputs[0] = f->active_control_node; // control edge
    n2->inputs[1] = n;
    n2->inputs[2] = param;
    TB_NODE_SET_EXTRA(n2, TB_NodeMemAccess, .align = align);

    f->active_control_node = n2;
    return n;
}

TB_API void tb_inst_set_control(TB_Function* f, TB_Node* control) {
    f->active_control_node = control;
}

TB_API TB_Node* tb_inst_get_control(TB_Function* f) {
    return f->active_control_node;
}

TB_API void tb_inst_unreachable(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_UNREACHABLE, TB_TYPE_VOID, 0, 0);
    n->inputs[0] = f->active_control_node;
    f->active_control_node = NULL;
}

TB_API void tb_inst_debugbreak(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_DEBUGBREAK, TB_TYPE_VOID, 1, 0);
    n->inputs[0] = f->active_control_node;
    f->active_control_node = n;
}

TB_API void tb_inst_trap(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_TRAP, TB_TYPE_VOID, 1, 0);
    n->inputs[0] = f->active_control_node;
    f->active_control_node = NULL;
}

TB_API void tb_inst_keep_alive(TB_Function* f, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, TB_KEEPALIVE, TB_TYPE_VOID, 1, 0);
    n->inputs[0] = src;
}

TB_API TB_Node* tb_inst_poison(TB_Function* f) {
    return tb_alloc_node(f, TB_POISON, TB_TYPE_VOID, 0, 0);
}

TB_API void tb_inst_set_location(TB_Function* f, TB_FileID file, int line) {
    TB_Attrib* a = alloc_from_node_arena(f, sizeof(TB_Attrib));
    *a = (TB_Attrib) { .type = TB_ATTRIB_LOCATION, .loc = { file, line } };
    f->line_attrib = a;
}

TB_API TB_Node* tb_inst_local(TB_Function* f, uint32_t size, TB_CharUnits alignment) {
    tb_assume(size > 0);
    tb_assume(alignment > 0 && tb_is_power_of_two(alignment));

    // insert in the entry block
    TB_Node* n = tb_alloc_node(f, TB_LOCAL, TB_TYPE_PTR, 0, sizeof(TB_NodeLocal));
    TB_NODE_SET_EXTRA(n, TB_NodeLocal, .size = size, .align = alignment);
    return n;
}

TB_API TB_Node* tb_inst_load(TB_Function* f, TB_DataType dt, TB_Node* addr, TB_CharUnits alignment, bool is_volatile) {
    TB_Node* n = tb_alloc_node(f, TB_LOAD, dt, 2, sizeof(TB_NodeMemAccess));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment, .is_volatile = is_volatile);

    if (is_volatile) {
        f->active_control_node = n;
    }
    return n;
}

TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Node* addr, TB_Node* val, uint32_t alignment, bool is_volatile) {
    tb_assume(TB_DATA_TYPE_EQUALS(dt, val->dt));

    TB_Node* n = tb_alloc_node(f, TB_STORE, dt, 3, sizeof(TB_NodeMemAccess));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = val;
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment, .is_volatile = is_volatile);

    f->active_control_node = n;
}

TB_API TB_Node* tb_inst_bool(TB_Function* f, bool imm) {
    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, TB_TYPE_BOOL, 0, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = imm;
    return n;
}

TB_API TB_Node* tb_inst_uint(TB_Function* f, TB_DataType dt, uint64_t imm) {
    tb_assume(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));

    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
    imm &= mask;

    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = imm;
    return n;
}

TB_API TB_Node* tb_inst_sint(TB_Function* f, TB_DataType dt, int64_t imm) {
    tb_assume(TB_IS_POINTER_TYPE(dt) || (TB_IS_INTEGER_TYPE(dt) && (dt.data <= 64)));

    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = imm;
    return n;
}

TB_API TB_Node* tb_inst_float32(TB_Function* f, float imm) {
    TB_Node* n = tb_alloc_node(f, TB_FLOAT32_CONST, TB_TYPE_F32, 0, sizeof(TB_NodeFloat32));
    TB_NODE_SET_EXTRA(n, TB_NodeFloat32, .value = imm);
    return n;
}

TB_API TB_Node* tb_inst_float64(TB_Function* f, double imm) {
    TB_Node* n = tb_alloc_node(f, TB_FLOAT64_CONST, TB_TYPE_F64, 0, sizeof(TB_NodeFloat64));
    TB_NODE_SET_EXTRA(n, TB_NodeFloat64, .value = imm);
    return n;
}

TB_API TB_Node* tb_inst_string(TB_Function* f, size_t len, const char* str) {
    TB_Global* dummy = tb_global_create(f->super.module, NULL, NULL, TB_LINKAGE_PRIVATE);
    tb_global_set_storage(f->super.module, &f->super.module->rdata, dummy, len, 1, 1);

    char* dst = tb_global_add_region(f->super.module, dummy, 0, len);
    memcpy(dst, str, len);

    return tb_inst_get_symbol_address(f, (TB_Symbol*) dummy);
}

TB_API TB_Node* tb_inst_cstring(TB_Function* f, const char* str) {
    return tb_inst_string(f, strlen(str) + 1, str);
}

TB_API TB_Node* tb_inst_array_access(TB_Function* f, TB_Node* base, TB_Node* index, int64_t stride) {
    TB_Node* n = tb_alloc_node(f, TB_ARRAY_ACCESS, TB_TYPE_PTR, 2, sizeof(TB_NodeArray));
    n->inputs[0] = base;
    n->inputs[1] = index;
    TB_NODE_SET_EXTRA(n, TB_NodeArray, .stride = stride);
    return n;
}

TB_API TB_Node* tb_inst_member_access(TB_Function* f, TB_Node* base, int64_t offset) {
    if (offset == 0) {
        return base;
    }

    TB_Node* n = tb_alloc_node(f, TB_MEMBER_ACCESS, TB_TYPE_PTR, 1, sizeof(TB_NodeMember));
    n->inputs[0] = base;
    TB_NODE_SET_EXTRA(n, TB_NodeMember, .offset = offset);
    return n;
}

TB_API TB_Node* tb_inst_get_symbol_address(TB_Function* f, TB_Symbol* target) {
    assert(target != NULL);

    TB_Node* n = tb_alloc_node(f, TB_GET_SYMBOL_ADDRESS, TB_TYPE_PTR, 0, sizeof(TB_NodeSymbol));
    TB_NODE_SET_EXTRA(n, TB_NodeSymbol, .sym = target);
    return n;
}

TB_API TB_Node* tb_inst_safepoint(TB_Function* f, size_t param_count, TB_Node** params) {
    TB_Node* n = tb_alloc_node(f, TB_SAFEPOINT, TB_TYPE_CONTROL, param_count, sizeof(TB_NodeSafepoint));
    memcpy(n->inputs, params, param_count * sizeof(TB_Node*));
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = f->safepoint_count++);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_syscall(TB_Function* f, TB_DataType dt, TB_Node* syscall_num, size_t param_count, TB_Node** params) {
    TB_Node* n = tb_alloc_node(f, TB_SCALL, dt, 2 + param_count, 0);
    n->inputs[0] = f->active_control_node;
    n->inputs[1] = syscall_num;
    memcpy(n->inputs, params, param_count * sizeof(TB_Node*));

    f->active_control_node = n;
    return n;
}

TB_API TB_MultiOutput tb_inst_call(TB_Function* f, TB_FunctionPrototype* proto, TB_Node* target, size_t param_count, TB_Node** params) {
    size_t proj_count = proto->return_count > 1 ? proto->return_count : 0;

    TB_DataType dt;
    if (proto->return_count > 1) {
        dt = TB_TYPE_TUPLE;
    } else if (proto->return_count == 1) {
        dt = TB_PROTOTYPE_RETURNS(proto)->dt;
    } else {
        dt = TB_TYPE_VOID;
    }

    TB_Node* n = tb_alloc_node(f, TB_CALL, dt, 2 + param_count, sizeof(TB_NodeCall) + (sizeof(TB_Node*)*proj_count));
    n->inputs[0] = f->active_control_node;
    n->inputs[1] = target;
    memcpy(n->inputs + 2, params, param_count * sizeof(TB_Node*));
    f->active_control_node = n;

    TB_NodeCall* c = TB_NODE_GET_EXTRA(n);
    c->proto = proto;

    if (proto->return_count > 1) {
        // create projections
        TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(proto);
        FOREACH_N(i, 0, proto->return_count) {
            TB_Node* proj = tb_alloc_node(f, TB_PROJ, rets[i].dt, 1, sizeof(TB_NodeProj));
            proj->inputs[0] = n;
            TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = i);
        }

        return (TB_MultiOutput){ .count = proto->return_count, .multiple = c->projs };
    } else {
        return (TB_MultiOutput){ .count = proto->return_count, .single = n };
    }
}

TB_API void tb_inst_memset(TB_Function* f, TB_Node* dst, TB_Node* val, TB_Node* size, TB_CharUnits align, bool is_volatile) {
    tb_assume(TB_IS_POINTER_TYPE(dst->dt));
    tb_assume(TB_IS_INTEGER_TYPE(val->dt) && val->dt.data == 8);

    TB_Node* n = tb_alloc_node(f, TB_MEMSET, TB_TYPE_VOID, 4, sizeof(TB_NodeMemAccess));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = dst;
    n->inputs[2] = val;
    n->inputs[3] = size;
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align, .is_volatile = is_volatile);

    f->active_control_node = n;
}

TB_API void tb_inst_memcpy(TB_Function* f, TB_Node* dst, TB_Node* val, TB_Node* size, TB_CharUnits align, bool is_volatile) {
    tb_assume(TB_IS_POINTER_TYPE(dst->dt));
    tb_assume(TB_IS_POINTER_TYPE(val->dt));

    TB_Node* n = tb_alloc_node(f, TB_MEMCPY, TB_TYPE_VOID, 4, sizeof(TB_NodeMemAccess));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = dst;
    n->inputs[2] = val;
    n->inputs[3] = size;
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = align, .is_volatile = is_volatile);

    f->active_control_node = n;
}

TB_API TB_Node* tb_inst_not(TB_Function* f, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, TB_NOT, src->dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_bswap(TB_Function* f, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, TB_BSWAP, src->dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_clz(TB_Function* f, TB_Node* src) {
    tb_assume(TB_IS_INTEGER_TYPE(src->dt));
    uint64_t bits = tb_ffs(src->dt.data) - 1;

    TB_Node* n = tb_alloc_node(f, TB_CLZ, TB_TYPE_INTN(bits), 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_ctz(TB_Function* f, TB_Node* src) {
    tb_assume(TB_IS_INTEGER_TYPE(src->dt));
    uint64_t bits = tb_ffs(src->dt.data) - 1;

    TB_Node* n = tb_alloc_node(f, TB_CTZ, TB_TYPE_INTN(bits), 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_popcount(TB_Function* f, TB_Node* src) {
    tb_assume(TB_IS_INTEGER_TYPE(src->dt));
    uint64_t bits = tb_ffs(src->dt.data) - 1;

    TB_Node* n = tb_alloc_node(f, TB_POPCNT, TB_TYPE_INTN(bits), 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_neg(TB_Function* f, TB_Node* src) {
    TB_DataType dt = src->dt;
    if (src->type == TB_INTEGER_CONST && TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->num_words == 1) {
        uint64_t x = TB_NODE_GET_EXTRA_T(src, TB_NodeInt)->words[0];
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

        // two's complement negate is just invert and add 1
        uint64_t negated = (~x + 1) & mask;

        return tb_inst_sint(f, dt, negated);
    } else if (src->type == TB_FLOAT32_CONST) {
        float x = TB_NODE_GET_EXTRA_T(src, TB_NodeFloat32)->value;
        return tb_inst_float32(f, -x);
    } else if (src->type == TB_FLOAT64_CONST) {
        double x = TB_NODE_GET_EXTRA_T(src, TB_NodeFloat64)->value;
        return tb_inst_float64(f, -x);
    }

    TB_Node* n = tb_alloc_node(f, TB_NEG, src->dt, 1, 0);
    n->inputs[0] = src;
    return n;
}

TB_API TB_Node* tb_inst_select(TB_Function* f, TB_Node* cond, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, TB_SELECT, a->dt, 3, 0);
    n->inputs[0] = cond;
    n->inputs[1] = a;
    n->inputs[2] = b;
    return n;
}

TB_API TB_Node* tb_inst_and(TB_Function* f, TB_Node* a, TB_Node* b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_AND, 0, a, b);
}

TB_API TB_Node* tb_inst_or(TB_Function* f, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));
    return tb_bin_arith(f, TB_OR, 0, a, b);
}

TB_API TB_Node* tb_inst_xor(TB_Function* f, TB_Node* a, TB_Node* b) {
    // bitwise operators can't wrap
    return tb_bin_arith(f, TB_XOR, 0, a, b);
}

TB_API TB_Node* tb_inst_add(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_ADD, arith_behavior, a, b);
}

TB_API TB_Node* tb_inst_sub(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SUB, arith_behavior, a, b);
}

TB_API TB_Node* tb_inst_mul(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_MUL, arith_behavior, a, b);
}

TB_API TB_Node* tb_inst_div(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    // division can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SDIV : TB_UDIV, 0, a, b);
}

TB_API TB_Node* tb_inst_mod(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    // modulo can't wrap or overflow
    return tb_bin_arith(f, signedness ? TB_SMOD : TB_UMOD, 0, a, b);
}

TB_API TB_Node* tb_inst_shl(TB_Function* f, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior arith_behavior) {
    return tb_bin_arith(f, TB_SHL, arith_behavior, a, b);
}

////////////////////////////////
// Atomics
////////////////////////////////
TB_API TB_Node* tb_inst_atomic_test_and_set(TB_Function* f, TB_Node* addr, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_TEST_AND_SET, TB_TYPE_BOOL, 2, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_clear(TB_Function* f, TB_Node* addr, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_CLEAR, TB_TYPE_BOOL, 2, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_load(TB_Function* f, TB_Node* addr, TB_DataType dt, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_LOAD, dt, 2, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);
    return n;
}

TB_API TB_Node* tb_inst_atomic_xchg(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_DataType dt = src->dt;

    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_XCHG, dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_add(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_ADD, src->dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_sub(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_SUB, src->dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_and(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_AND, src->dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_xor(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_XOR, src->dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_or(TB_Function* f, TB_Node* addr, TB_Node* src, TB_MemoryOrder order) {
    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_OR, src->dt, 3, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = src;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = order, .order2 = TB_MEM_ORDER_SEQ_CST);

    f->active_control_node = n;
    return n;
}

TB_API TB_Node* tb_inst_atomic_cmpxchg(TB_Function* f, TB_Node* addr, TB_Node* expected, TB_Node* desired, TB_MemoryOrder succ, TB_MemoryOrder fail) {
    tb_assume(TB_DATA_TYPE_EQUALS(desired->dt, expected->dt));
    TB_DataType dt = desired->dt;

    TB_Node* n = tb_alloc_node(f, TB_ATOMIC_CMPXCHG, dt, 4, sizeof(TB_NodeAtomic));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = addr;
    n->inputs[2] = expected;
    n->inputs[3] = desired;
    TB_NODE_SET_EXTRA(n, TB_NodeAtomic, .order = succ, .order2 = fail);

    f->active_control_node = n;
    return n;
}

// TODO(NeGate): Maybe i should split the bitshift operations into a separate kind of
// operator that has different arithmatic behaviors, maybe like trap on a large shift amount
TB_API TB_Node* tb_inst_sar(TB_Function* f, TB_Node* a, TB_Node* b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SAR, 0, a, b);
}

TB_API TB_Node* tb_inst_shr(TB_Function* f, TB_Node* a, TB_Node* b) {
    // shift right can't wrap or overflow
    return tb_bin_arith(f, TB_SHR, 0, a, b);
}

TB_API TB_Node* tb_inst_fadd(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FADD, a, b);
}

TB_API TB_Node* tb_inst_fsub(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FSUB, a, b);
}

TB_API TB_Node* tb_inst_fmul(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FMUL, a, b);
}

TB_API TB_Node* tb_inst_fdiv(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_bin_farith(f, TB_FDIV, a, b);
}

TB_API TB_Node* tb_inst_va_start(TB_Function* f, TB_Node* a) {
    assert(a->type == TB_LOCAL);

    TB_Node* n = tb_alloc_node(f, TB_VA_START, TB_TYPE_PTR, 1, 0);
    n->inputs[0] = a;
    return n;
}

TB_API TB_Node* tb_inst_x86_ldmxcsr(TB_Function* f, TB_Node* a) {
    assert(a->dt.type == TB_INT && a->dt.data == 32);

    TB_Node* n = tb_alloc_node(f, TB_X86INTRIN_LDMXCSR, TB_TYPE_I32, 1, 0);
    n->inputs[0] = a;
    return n;
}

TB_API TB_Node* tb_inst_x86_rdtsc(TB_Function* f) {
    return tb_alloc_node(f, TB_X86INTRIN_RDTSC, TB_TYPE_I64, 0, 0);
}

TB_API TB_Node* tb_inst_x86_stmxcsr(TB_Function* f) {
    return tb_alloc_node(f, TB_X86INTRIN_STMXCSR, TB_TYPE_I32, 0, 0);
}

TB_API TB_Node* tb_inst_x86_sqrt(TB_Function* f, TB_Node* a) {
    TB_Node* n = tb_alloc_node(f, TB_X86INTRIN_SQRT, a->dt, 1, 0);
    n->inputs[0] = a;
    return n;
}

TB_API TB_Node* tb_inst_x86_rsqrt(TB_Function* f, TB_Node* a) {
    TB_Node* n = tb_alloc_node(f, TB_X86INTRIN_RSQRT, a->dt, 1, 0);
    n->inputs[0] = a;
    return n;
}

TB_API TB_Node* tb_inst_cmp(TB_Function* f, TB_NodeType type, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, type, TB_TYPE_BOOL, 2, sizeof(TB_NodeCompare));
    n->inputs[0] = a;
    n->inputs[1] = b;
    TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->dt);
    return n;
}

TB_API TB_Node* tb_inst_cmp_eq(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_EQ, a, b);
}

TB_API TB_Node* tb_inst_cmp_ne(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_NE, a, b);
}

TB_API TB_Node* tb_inst_cmp_ilt(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, a, b);
}

TB_API TB_Node* tb_inst_cmp_ile(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, a, b);
}

TB_API TB_Node* tb_inst_cmp_igt(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLT : TB_CMP_ULT, b, a);
}

TB_API TB_Node* tb_inst_cmp_ige(TB_Function* f, TB_Node* a, TB_Node* b, bool signedness) {
    return tb_inst_cmp(f, signedness ? TB_CMP_SLE : TB_CMP_ULE, b, a);
}

TB_API TB_Node* tb_inst_cmp_flt(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLT, a, b);
}

TB_API TB_Node* tb_inst_cmp_fle(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLE, a, b);
}

TB_API TB_Node* tb_inst_cmp_fgt(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLT, b, a);
}

TB_API TB_Node* tb_inst_cmp_fge(TB_Function* f, TB_Node* a, TB_Node* b) {
    return tb_inst_cmp(f, TB_CMP_FLE, b, a);
}

TB_API TB_Node* tb_inst_incomplete_phi(TB_Function* f, TB_DataType dt, TB_Node* region, size_t preds) {
    TB_Node* n = tb_alloc_node(f, TB_PHI, dt, 1 + preds, 0);
    n->inputs[0] = region;
    memset(n->inputs + 1, 0, preds * sizeof(TB_Node*));
    return n;
}

TB_API bool tb_inst_add_phi_operand(TB_Function* f, TB_Node* phi, TB_Node* region, TB_Node* val) {
    TB_Node* phi_region = phi->inputs[0];

    // the slot to fill is based on the predecessor list of the region
    FOREACH_N(i, 0, phi_region->input_count) {
        TB_Node* pred = phi_region->inputs[i];
        while (pred->type != TB_REGION && pred->type != TB_START) pred = pred->inputs[0];

        if (pred == region) {
            phi->inputs[i+1] = val;
            return true;
        }
    }

    return false;
}

TB_API TB_Node* tb_inst_phi2(TB_Function* f, TB_Node* region, TB_Node* a, TB_Node* b) {
    tb_assume(TB_DATA_TYPE_EQUALS(a->dt, b->dt));

    TB_Node* n = tb_alloc_node(f, TB_PHI, a->dt, 3, 0);
    n->inputs[0] = region;
    n->inputs[1] = a;
    n->inputs[2] = b;

    // add new phi to region (NOT EFFICIENT)
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(region);

    size_t old_count = r->proj_count++;
    TB_Node** new_projs = alloc_from_node_arena(f, r->proj_count * sizeof(TB_Node*));
    if (old_count != 0) {
        memcpy(new_projs, r->projs, old_count * sizeof(TB_Node*));
    }

    new_projs[old_count] = n;
    r->projs = new_projs;
    return n;
}

void tb_inst_set_phis_to_region(TB_Function* f, TB_Node* region, size_t phi_count, TB_Node** phis) {
    // add new phi to region (NOT EFFICIENT)
    TB_NodeRegion* r = TB_NODE_GET_EXTRA(region);

    TB_Node** new_phis = alloc_from_node_arena(f, phi_count * sizeof(TB_Node*));
    if (phi_count != 0) {
        memcpy(new_phis, phis, phi_count * sizeof(TB_Node*));
    }

    r->projs = new_phis;
    r->proj_count = phi_count;
}

TB_API TB_Node* tb_inst_region(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_REGION, TB_TYPE_TUPLE, 0, sizeof(TB_NodeRegion));
    return n;
}

static void add_region_pred(TB_Function* f, TB_Node* n, TB_Node* pred) {
    // detach old predecessor list, make bigger one
    assert(n->type == TB_REGION);

    size_t old_count = n->input_count;
    TB_Node** new_inputs = alloc_from_node_arena(f, (old_count + 1) * sizeof(TB_Node*));
    memcpy(new_inputs, n->inputs, old_count * sizeof(TB_Node*));
    new_inputs[old_count] = pred;

    n->inputs = new_inputs;
    n->input_count = old_count + 1;
}

static TB_Node** add_successors(TB_Function* f, TB_Node* terminator, size_t count) {
    TB_NodeRegion* bb = TB_NODE_GET_EXTRA(tb_get_parent_region(f->active_control_node));
    bb->end = terminator;
    bb->succ_count = count;
    bb->succ = alloc_from_node_arena(f, count * sizeof(TB_Node*));
    return bb->succ;
}

TB_API void tb_inst_goto(TB_Function* f, TB_Node* target) {
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 1, sizeof(TB_NodeBranch));
    n->inputs[0] = f->active_control_node; // control edge

    TB_Node** succ = add_successors(f, n, 1);
    succ[0] = target;
    f->active_control_node = NULL;

    {
        TB_Node* proj = tb_alloc_node(f, TB_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeProj));
        proj->inputs[0] = n;
        TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = 0);

        add_region_pred(f, target, proj);
    }
}

TB_API void tb_inst_if(TB_Function* f, TB_Node* cond, TB_Node* if_true, TB_Node* if_false) {
    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch) + sizeof(int64_t));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = cond;

    FOREACH_N(i, 0, 2) {
        TB_Node* target = i ? if_false : if_true;

        TB_Node* proj = tb_alloc_node(f, TB_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeProj));
        proj->inputs[0] = n;
        TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = i);

        add_region_pred(f, target, proj);
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    br->keys[0] = 0;

    TB_Node** succ = add_successors(f, n, 2);
    succ[0] = if_true;
    succ[1] = if_false;
    f->active_control_node = NULL;
}

TB_API void tb_inst_branch(TB_Function* f, TB_DataType dt, TB_Node* key, TB_Node* default_label, size_t entry_count, const TB_SwitchEntry* entries) {
    assert(f->active_control_node != NULL);

    // generate control projections
    TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch) + (sizeof(int64_t) * entry_count));
    n->inputs[0] = f->active_control_node; // control edge
    n->inputs[1] = key;

    FOREACH_N(i, 0, entry_count) {
        TB_Node* target = i ? entries[i].value : default_label;

        TB_Node* proj = tb_alloc_node(f, TB_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeProj));
        proj->inputs[0] = n;
        TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = i);

        add_region_pred(f, target, proj);
    }

    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    FOREACH_N(i, 0, entry_count) {
        br->keys[i] = entries[i].key;
    }

    TB_Node** succ = add_successors(f, n, 1 + entry_count);
    succ[0] = default_label;
    FOREACH_N(i, 0, entry_count) {
        succ[1 + i] = entries[i].value;
    }

    f->active_control_node = NULL;
}

TB_API void tb_inst_ret(TB_Function* f, size_t count, TB_Node** values) {
    TB_Node* n = tb_alloc_node(f, TB_RET, TB_TYPE_VOID, 1 + count, 0);
    n->inputs[0] = f->active_control_node;
    if (count > 0) {
        memcpy(n->inputs + 1, values, count * sizeof(TB_Node*));
    }

    TB_Node* bb = tb_get_parent_region(f->active_control_node);
    TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end = n;
    f->active_control_node = NULL;
}
