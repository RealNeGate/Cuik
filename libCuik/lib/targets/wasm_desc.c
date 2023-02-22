#include "targets.h"
#include <front/sema.h>

static void set_defines(const Cuik_Target* target, Cuik_CPP* cpp) {
    target_generic_set_defines(cpp, target->system, true, true);
    cuikpp_define_cstr(cpp, "__WASM__", "1");
}

static Cuik_Type* type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, const char* builtin_value, int arg_count, Expr** args) {
    return target_generic_type_check_builtin(tu, e, name, builtin_value, arg_count, args);
}

static TB_Reg compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args) {
    return target_generic_compile_builtin(tu, func, name, arg_count, args).r;
}

#ifdef CUIK_USE_TB
static bool should_pass_on_value_stack(TranslationUnit* tu, Cuik_Type* type) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        return false;
    } else {
        return type->size < 8;
    }
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, Cuik_Type* type) {
    TB_Module* m = tu->ir_mod;
    bool is_aggregate_return = !should_pass_on_value_stack(tu, cuik_canonical_type(type->func.return_type));

    // parameters
    Param* param_list = type->func.param_list;
    size_t param_count = type->func.param_count;

    // estimate parameter count
    size_t real_param_count = (is_aggregate_return ? 1 : 0) + param_count;

    TB_DataType return_dt = TB_TYPE_PTR;
    if (!is_aggregate_return) return_dt = ctype_to_tbtype(cuik_canonical_type(type->func.return_type));

    TB_DebugType* ret_type = NULL;
    if (tu->has_tb_debug_info && !is_aggregate_return) {
        ret_type = cuik__as_tb_debug_type(m, cuik_canonical_type(type->func.return_type));
    }

    TB_FunctionPrototype* proto = tb_prototype_create(tu->ir_mod, TB_STDCALL, return_dt, ret_type, real_param_count, type->func.has_varargs);
    if (is_aggregate_return) {
        if (tu->has_tb_debug_info) {
            // it's a pointer to the debug type here
            TB_DebugType* dbg_type = cuik__as_tb_debug_type(m, cuik_canonical_type(type->func.return_type));
            tb_prototype_add_param_named(tu->ir_mod, proto, TB_TYPE_PTR, "$retval", tb_debug_create_ptr(m, dbg_type));
        } else {
            tb_prototype_add_param(tu->ir_mod, proto, TB_TYPE_PTR);
        }
    }

    for (size_t i = 0; i < param_count; i++) {
        Param* p = &param_list[i];
        Cuik_Type* type = cuik_canonical_type(p->type);

        if (should_pass_on_value_stack(tu, type)) {
            TB_DataType dt = ctype_to_tbtype(type);

            assert(dt.width < 8);
            if (tu->has_tb_debug_info) {
                tb_prototype_add_param_named(tu->ir_mod, proto, dt, p->name, cuik__as_tb_debug_type(tu->ir_mod, type));
            } else {
                tb_prototype_add_param(tu->ir_mod, proto, dt);
            }
        } else {
            if (tu->has_tb_debug_info) {
                TB_DebugType* dbg_type = cuik__as_tb_debug_type(tu->ir_mod, type);
                tb_prototype_add_param_named(tu->ir_mod, proto, TB_TYPE_PTR, p->name, tb_debug_create_ptr(m, dbg_type));
            } else {
                tb_prototype_add_param(tu->ir_mod, proto, TB_TYPE_PTR);
            }
        }
    }

    return proto;
}

static bool pass_return_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    return should_pass_on_value_stack(tu, type);
}

static int deduce_parameter_usage(TranslationUnit* tu, Cuik_QualType type) {
    return 1;
}

static int pass_parameter(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Reg* out_param) {
    Cuik_Type* arg_type = cuik_canonical_type(e->type);

    if (!should_pass_on_value_stack(tu, arg_type)) {
        // const pass-by-value is considered as a const ref
        // since it doesn't mutate
        IRVal arg = irgen_expr(tu, func, e);
        TB_Reg arg_addr = TB_NULL_REG;
        switch (arg.value_type) {
            case LVALUE:
            arg_addr = arg.reg;
            break;
            case LVALUE_SYMBOL:
            arg_addr = tb_inst_get_symbol_address(func, arg.sym);
            break;
            case RVALUE: {
                // spawn a lil temporary
                TB_CharUnits size = arg_type->size;
                TB_CharUnits align = arg_type->align;
                TB_DataType dt = tb_function_get_node(func, arg.reg)->dt;

                arg_addr = tb_inst_local(func, size, align);
                tb_inst_store(func, dt, arg_addr, arg.reg, align);
                break;
            }
            default:
            break;
        }
        assert(arg_addr);

        // TODO(NeGate): we might wanna define some TB instruction
        // for killing locals since some have really limited lifetimes
        TB_CharUnits size = arg_type->size;
        TB_CharUnits align = arg_type->align;

        if (0 /* arg_type->is_const */) {
            out_param[0] = arg_addr;
        } else {
            TB_Reg temp_slot = tb_inst_local(func, size, align);
            TB_Reg size_reg = tb_inst_uint(func, TB_TYPE_I64, size);

            tb_inst_memcpy(func, temp_slot, arg_addr, size_reg, align);

            out_param[0] = temp_slot;
        }

        return 1;
    } else {
        TB_Reg arg = irgen_as_rvalue(tu, func, e);
        TB_DataType dt = tb_function_get_node(func, arg)->dt;

        if (is_vararg && dt.type == TB_FLOAT && dt.data == TB_FLT_64 && dt.width == 0) {
            // convert any float variadic arguments into integers
            arg = tb_inst_bitcast(func, arg, TB_TYPE_I64);
        }

        out_param[0] = arg;
        return 1;
    }
}
#endif

Cuik_Target* cuik_target_wasm(Cuik_System system, Cuik_Environment env) {
    NL_Strmap(const char*) builtins = NULL;
    target_generic_fill_builtin_table(&builtins);

    Cuik_Target* t = cuik_malloc(sizeof(Cuik_Target));
    *t = (Cuik_Target){
        .env = env,
        .system = system,

        .int_bits = { 8, 16, 32, 32, 64 },

        #ifdef CUIK_USE_TB
        .arch = TB_ARCH_WASM32,
        #endif

        .builtin_func_map = builtins,
        .set_defines = set_defines,
        #ifdef CUIK_USE_TB
        .create_prototype = create_prototype,
        .pass_return_via_reg = pass_return_via_reg,
        .deduce_parameter_usage = deduce_parameter_usage,
        .pass_parameter = pass_parameter,
        .compile_builtin = compile_builtin,
        #endif /* CUIK_USE_TB */
        .type_check_builtin = type_check_builtin,
    };

    cuik_target_build(t);
    return t;
}
