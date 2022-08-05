#include "targets.h"
#include <front/sema.h>

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

static void set_defines(Cuik_CPP* cpp, Cuik_System sys) {
    cuikpp_define_empty(cpp, "_CUIK_TARGET_64BIT_");
    cuikpp_define(cpp, "__LITTLE_ENDIAN__", "1");

    if (sys == CUIK_SYSTEM_WINDOWS) {
        cuikpp_define(cpp, "_M_X64", "100");
        cuikpp_define(cpp, "_AMD64_", "100");
        cuikpp_define(cpp, "_M_AMD64", "100");

        cuikpp_define(cpp, "_WIN32", "1");
        cuikpp_define(cpp, "_WIN64", "1");
    } else if (sys == CUIK_SYSTEM_LINUX) {
        cuikpp_define(cpp, "__x86_64__", "1");
        cuikpp_define(cpp, "__LP64__", "1");

        cuikpp_define(cpp, "__linux", "1");
        cuikpp_define(cpp, "linux", "1");
    }

    // stdatomic.h lock free stuff
    cuikpp_define(cpp, "__CUIK_ATOMIC_BOOL_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_CHAR_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_CHAR16_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_CHAR32_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_WCHAR_T_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_SHORT_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_INT_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_LONG_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_LLONG_LOCK_FREE", "1");
    cuikpp_define(cpp, "__CUIK_ATOMIC_POINTER_LOCK_FREE", "1");
}

// returns the pointer's base type for whatever expression was resolved
static Cuik_Type* expect_pointer(TranslationUnit* tu, Expr* e, Expr* arg) {
    Cuik_Type* dst_type = sema_expr(tu, arg);
    if (dst_type->kind != KIND_PTR) {
        REPORT_EXPR(ERROR, arg, "argument should be a pointer");
        return &builtin_types[TYPE_INT];
    }

    return dst_type->ptr_to;
}

static Expr* resolve_memory_order_expr(TranslationUnit* tu, Expr* e) {
    e = cuik__optimize_ast(tu, e);
    e->cast_type = &builtin_types[TYPE_INT];

    if (e->op != EXPR_INT) {
        REPORT_EXPR(ERROR, e, "Memory order must be a constant expression");
        return e;
    }

    return e;
}

// TODO(NeGate): Add some type checking utilities to match against a list of types since that's kinda important :p
static Cuik_Type* type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, int arg_count, Expr** args) {
    if (strcmp(name, "__va_start") == 0) {
        if (arg_count != 2) {
            REPORT_EXPR(ERROR, e, "%s requires 2 arguments", name);
            return &builtin_types[TYPE_INT];
        }

        // only type check the first param
        args[0]->cast_type = new_pointer(tu, &builtin_types[TYPE_CHAR]);
        cuik__type_check_args(tu, e, 1, args);

        // second is completely generic so long as it's a parameter
        if (args[1]->op != EXPR_PARAM) {
            REPORT_EXPR(ERROR, e, "va_start's second parameter must be a parameter name", name);
            return &builtin_types[TYPE_INT];
        }

        args[1]->cast_type = sema_expr(tu, args[1]);
        return &builtin_types[TYPE_VOID];
    } else if (strcmp(name, "__builtin_expect") == 0) {
        if (arg_count != 2) {
            REPORT_EXPR(ERROR, e, "%s requires 2 arguments", name);
            return &builtin_types[TYPE_VOID];
        }

        Cuik_Type* ty = get_common_type(
            tu, sema_expr(tu, args[0]), sema_expr(tu, args[1])
        );

        args[0]->cast_type = ty;
        args[1]->cast_type = ty;
        return args[0]->cast_type;
    } else if (strcmp(name, "__builtin_trap") == 0) {
        if (arg_count != 0) {
            REPORT_EXPR(ERROR, e, "%s doesn't require arguments", name);
            return &builtin_types[TYPE_VOID];
        }

        return &builtin_types[TYPE_VOID];
    } else if (strcmp(name, "__c11_atomic_compare_exchange_strong") == 0) {
        if (arg_count != 5) {
            REPORT_EXPR(ERROR, e, "%s requires 5 arguments", name);
            return &builtin_types[TYPE_INT];
        }

        Cuik_Type* base_type = expect_pointer(tu, e, args[0]);

        // fn(T* obj, T* expected, T desired, int succ, int fail)
        args[0]->cast_type = args[0]->type;
        args[1]->cast_type = args[0]->type;
        args[2]->cast_type = base_type;
        args[3]->cast_type = &builtin_types[TYPE_INT];
        args[4]->cast_type = &builtin_types[TYPE_INT];

        cuik__type_check_args(tu, e, arg_count, args);
        return &builtin_types[TYPE_BOOL];
    } else if (strcmp(name, "__c11_atomic_load") == 0) {
        if (arg_count != 2) {
            REPORT_EXPR(ERROR, e, "%s requires 2 arguments", name);
            return &builtin_types[TYPE_INT];
        }

        Cuik_Type* base_type = expect_pointer(tu, e, args[0]);

        // fn(T* obj, int order)
        args[0]->cast_type = args[0]->type;
        args[1] = resolve_memory_order_expr(tu, args[1]);

        cuik__type_check_args(tu, e, arg_count, args);
        return base_type;
    } else if (strcmp(name, "__c11_atomic_exchange") == 0 ||
        strcmp(name, "__c11_atomic_fetch_add") == 0 ||
        strcmp(name, "__c11_atomic_fetch_sub") == 0 ||
        strcmp(name, "__c11_atomic_fetch_or") == 0 ||
        strcmp(name, "__c11_atomic_fetch_xor") == 0 ||
        strcmp(name, "__c11_atomic_fetch_and") == 0) {
        if (arg_count != 3) {
            REPORT_EXPR(ERROR, e, "%s requires 3 arguments", name);
            return &builtin_types[TYPE_INT];
        }

        // fn(T* obj, T arg, int order)
        Cuik_Type* base_type = expect_pointer(tu, e, args[0]);

        args[0]->cast_type = args[0]->type;
        args[1]->cast_type = base_type;
        args[2] = resolve_memory_order_expr(tu, args[1]);

        cuik__type_check_args(tu, e, arg_count, args);
        return base_type;
    } else if (strcmp(name, "__builtin_mul_overflow") == 0) {
        if (arg_count != 3) {
            REPORT_EXPR(ERROR, e, "%s requires 3 arguments", name);
            goto failure;
        }

        Cuik_Type* type = sema_expr(tu, args[0]);
        if (type->kind < KIND_CHAR || type->kind > KIND_LONG) {
            REPORT_EXPR(ERROR, e, "%s can only be applied onto integers");
            goto failure;
        }
        args[0]->cast_type = &builtin_types[TYPE_INT];

        for (size_t i = 1; i < arg_count; i++) {
            Cuik_Type* arg_type = sema_expr(tu, args[i]);

            if (i == 2) {
                if (arg_type->kind != KIND_PTR) {
                    type_as_string(tu, sizeof(temp_string0), temp_string0, type);
                    REPORT_EXPR(ERROR, args[i], "Expected pointer to '%s' for the 3rd argument", temp_string0);
                    goto failure;
                }

                arg_type = arg_type->ptr_to;
            }

            if (!type_compatible(tu, arg_type, type, args[i])) {
                type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
                type_as_string(tu, sizeof(temp_string1), temp_string1, type);

                REPORT_EXPR(ERROR, args[i], "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
                goto failure;
            }

            Cuik_Type* cast_type = (i == 2) ? new_pointer(tu, type) : type;
            args[i]->cast_type = cast_type;
        }

        failure:
        return &builtin_types[TYPE_BOOL];
    } else if (strcmp(name, "_mm_setcsr") == 0) {
        if (arg_count != 1) {
            REPORT_EXPR(ERROR, e, "%s requires 1 arguments", name);
            return &builtin_types[TYPE_VOID];
        }

        Cuik_Type* arg_type = sema_expr(tu, args[0]);
        Cuik_Type* int_type = &builtin_types[TYPE_UINT];
        if (!type_compatible(tu, arg_type, int_type, args[0])) {
            type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
            type_as_string(tu, sizeof(temp_string1), temp_string1, int_type);

            REPORT_EXPR(ERROR, args[0], "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
            return &builtin_types[TYPE_VOID];
        }

        args[0]->cast_type = &builtin_types[TYPE_UINT];
        return &builtin_types[TYPE_VOID];
    } else if (strcmp(name, "_mm_getcsr") == 0) {
        if (arg_count != 0) {
            REPORT_EXPR(ERROR, e, "%s requires 0 arguments", name);
        }

        return &builtin_types[TYPE_UINT];
    } else if (strcmp(name, "__builtin_unreachable") == 0 || strcmp(name, "__debugbreak") == 0) {
        if (arg_count != 0) {
            REPORT_EXPR(ERROR, e, "%s requires 0 arguments", name);
        }

        return &builtin_types[TYPE_VOID];
    } else if (strcmp(name, "_InterlockedExchange") == 0) {
        if (arg_count != 2) {
            REPORT_EXPR(ERROR, e, "%s requires 2 arguments", name);
        }

        // fn(long* obj, long val)
        args[0]->cast_type = new_pointer(tu, &builtin_types[TYPE_INT]);
        args[1]->cast_type = &builtin_types[TYPE_INT];

        cuik__type_check_args(tu, e, arg_count, args);
        return &builtin_types[TYPE_INT];
    } else if (strcmp(name, "_InterlockedCompareExchange") == 0) {
        if (arg_count != 3) {
            REPORT_EXPR(ERROR, e, "%s requires 3 arguments", name);
        }

        // fn(long* obj, long exchange, long comparand)
        args[0]->cast_type = new_pointer(tu, &builtin_types[TYPE_INT]);
        args[1]->cast_type = &builtin_types[TYPE_INT];
        args[2]->cast_type = &builtin_types[TYPE_INT];

        cuik__type_check_args(tu, e, arg_count, args);
        return &builtin_types[TYPE_INT];
    } else {
        REPORT_EXPR(ERROR, e->call.target, "unimplemented builtin '%s'", name);
        return &builtin_types[TYPE_VOID];
    }
}

#ifdef CUIK_USE_TB
// on Win64 all structs that have a size of 1,2,4,8
// or any scalars are passed via registers
static bool win64_should_pass_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        switch (type->size) {
            case 1:
            case 2:
            case 4:
            case 8:
            return true;
            default:
            return false;
        }
    } else {
        return true;
    }
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, Cuik_Type* type) {
    // decide if return value is aggregate
    bool is_aggregate_return = !win64_should_pass_via_reg(tu, type->func.return_type);

    // parameters
    Param* param_list = type->func.param_list;
    size_t param_count = type->func.param_count;

    // estimate parameter count
    size_t real_param_count = (is_aggregate_return ? 1 : 0) + param_count;

    TB_DataType return_dt = TB_TYPE_PTR;
    if (!is_aggregate_return) return_dt = ctype_to_tbtype(type->func.return_type);

    TB_FunctionPrototype* proto = tb_prototype_create(tu->ir_mod, TB_STDCALL, return_dt, real_param_count, type->func.has_varargs);

    if (is_aggregate_return) {
        tb_prototype_add_param(proto, TB_TYPE_PTR);
    }

    for (size_t i = 0; i < param_count; i++) {
        Param* p = &param_list[i];

        if (win64_should_pass_via_reg(tu, p->type)) {
            TB_DataType dt = ctype_to_tbtype(p->type);

            assert(dt.width < 8);
            tb_prototype_add_param(proto, dt);
        } else {
            tb_prototype_add_param(proto, TB_TYPE_PTR);
        }
    }

    return proto;
}

static bool pass_return_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    return win64_should_pass_via_reg(tu, type);
}

static int deduce_parameter_usage(TranslationUnit* tu, Cuik_Type* type) {
    return 1;
}

static int pass_parameter(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Reg* out_param) {
    Cuik_Type* arg_type = e->type;

    if (!win64_should_pass_via_reg(tu, arg_type)) {
        // const pass-by-value is considered as a const ref
        // since it doesn't mutate
        IRVal arg = irgen_expr(tu, func, e);
        TB_Reg arg_addr = TB_NULL_REG;
        switch (arg.value_type) {
            case LVALUE:
            arg_addr = arg.reg;
            break;
            case LVALUE_FUNC:
            arg_addr = tb_inst_get_func_address(func, arg.func);
            break;
            case LVALUE_EFUNC:
            arg_addr = tb_inst_get_extern_address(func, arg.ext);
            break;
            case RVALUE: {
                // spawn a lil temporary
                TB_CharUnits size = arg_type->size;
                TB_CharUnits align = arg_type->align;
                TB_DataType dt = tb_function_get_node(func, arg.reg)->dt;

                arg_addr = tb_inst_local(func, size, align, NULL);
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

        if (arg_type->is_const) {
            out_param[0] = arg_addr;
        } else {
            TB_Reg temp_slot = tb_inst_local(func, size, align, NULL);
            TB_Reg size_reg = tb_inst_uint(func, TB_TYPE_I64, size);

            tb_inst_memcpy(func, temp_slot, arg_addr, size_reg, align);

            out_param[0] = temp_slot;
        }

        return 1;
    } else {
        if (arg_type->kind == KIND_STRUCT ||
            arg_type->kind == KIND_UNION) {
            // Convert aggregate into TB scalar
            IRVal arg = irgen_expr(tu, func, e);
            TB_Reg arg_addr = TB_NULL_REG;
            switch (arg.value_type) {
                case LVALUE:
                arg_addr = arg.reg;
                break;
                case LVALUE_FUNC:
                arg_addr = tb_inst_get_func_address(func, arg.func);
                break;
                case LVALUE_EFUNC:
                arg_addr = tb_inst_get_extern_address(func, arg.ext);
                break;
                case RVALUE: {
                    // spawn a lil temporary
                    TB_CharUnits size = arg_type->size;
                    TB_CharUnits align = arg_type->align;
                    TB_DataType dt = tb_function_get_node(func, arg.reg)->dt;

                    arg_addr = tb_inst_local(func, size, align, NULL);
                    tb_inst_store(func, dt, arg_addr, arg.reg, align);
                    break;
                }
                default:
                break;
            }
            assert(arg_addr);

            TB_DataType dt = TB_TYPE_VOID;
            switch (arg_type->size) {
                case 1:
                dt = TB_TYPE_I8;
                break;
                case 2:
                dt = TB_TYPE_I16;
                break;
                case 4:
                dt = TB_TYPE_I32;
                break;
                case 8:
                dt = TB_TYPE_I64;
                break;
                default:
                break;
            }

            out_param[0] = tb_inst_load(func, dt, arg_addr, arg_type->align);
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
}

static TB_Reg compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args) {
    if (strcmp(name, "_InterlockedExchange") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        return tb_inst_atomic_xchg(func, dst, src, TB_MEM_ORDER_SEQ_CST);
    } else if (strcmp(name, "__c11_atomic_compare_exchange_strong") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg comparand = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg exchange = irgen_as_rvalue(tu, func, args[2]);

        assert(args[1]->cast_type->kind == KIND_PTR);
        Cuik_Type* ty = args[1]->cast_type->ptr_to;

        // for odd reasons C11 compare exchange uses a pointer for the comparand
        comparand = tb_inst_load(func, ctype_to_tbtype(ty), comparand, ty->align);

        TB_CmpXchgResult r = tb_inst_atomic_cmpxchg(func, addr, comparand, exchange, TB_MEM_ORDER_SEQ_CST, TB_MEM_ORDER_SEQ_CST);
        return r.old_value;
    } else if (strcmp(name, "_InterlockedCompareExchange") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg exchange = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg comparand = irgen_as_rvalue(tu, func, args[2]);

        TB_CmpXchgResult r = tb_inst_atomic_cmpxchg(func, addr, comparand, exchange, TB_MEM_ORDER_SEQ_CST, TB_MEM_ORDER_SEQ_CST);
        return r.old_value;
    } else if (strcmp(name, "__c11_atomic_thread_fence") == 0) {
        printf("TODO __c11_atomic_thread_fence!");
        abort();
    } else if (strcmp(name, "__c11_atomic_signal_fence") == 0) {
        printf("TODO __c11_atomic_signal_fence!");
        abort();
    } else if (strcmp(name, "__c11_atomic_exchange") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        assert(args[2]->op == EXPR_INT && args[2]->int_num.num < 6);
        int order = args[2]->int_num.num;
        return tb_inst_atomic_xchg(func, dst, src, order);
    } else if (strcmp(name, "__c11_atomic_load") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);

        assert(args[0]->cast_type->kind == KIND_PTR);
        TB_DataType dt = ctype_to_tbtype(args[0]->cast_type->ptr_to);

        assert(args[1]->op == EXPR_INT && args[1]->int_num.num < 6);
        int order = args[1]->int_num.num;
        return tb_inst_atomic_load(func, addr, dt, order);
    } else if (strcmp(name, "__c11_atomic_fetch_add") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        assert(args[2]->op == EXPR_INT && args[2]->int_num.num < 6);
        int order = args[2]->int_num.num;
        return tb_inst_atomic_add(func, dst, src, order);
    } else if (strcmp(name, "__c11_atomic_fetch_sub") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        assert(args[2]->op == EXPR_INT && args[2]->int_num.num < 6);
        int order = args[2]->int_num.num;
        return tb_inst_atomic_sub(func, dst, src, order);
    } else if (strcmp(name, "__builtin_mul_overflow") == 0) {
        Cuik_Type* type = args[0]->cast_type;
        TB_DataType dt = ctype_to_tbtype(type);

        TB_Reg a = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg b = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg c = irgen_as_rvalue(tu, func, args[2]);

        TB_Reg result = tb_inst_mul(func, a, b, 0);
        tb_inst_store(func, dt, c, result, type->align);

        return tb_inst_cmp_ilt(func, result, a, false);
    } else if (strcmp(name, "__builtin_unreachable") == 0) {
        tb_inst_unreachable(func);
        tb_inst_label(func, tb_inst_new_label_id(func));
        return 0;
    } else if (strcmp(name, "__builtin_expect") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        return dst;
    } else if (strcmp(name, "__builtin_trap") == 0) {
        // switch this out for a proper trap
        tb_inst_debugbreak(func);
        return 0;
    } else if (strcmp(name, "__debugbreak") == 0) {
        tb_inst_debugbreak(func);
        return 0;
    } else if (strcmp(name, "__va_start") == 0) {
        // TODO(NeGate): Remove this later because it will emotionally damage our optimizer.
        // the issue is that it blatantly accesses out of bounds and we should probably just
        // have a node for va_start in the backend instead.
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        IRVal src = irgen_expr(tu, func, args[1]);
        assert(src.value_type == LVALUE);

        tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_va_start(func, src.reg), 8);
        return 0;
    } else if (strcmp(name, "_mm_setcsr") == 0) {
        TB_Reg src = irgen_as_rvalue(tu, func, args[0]);
        return tb_inst_x86_ldmxcsr(func, src);
    } else if (strcmp(name, "_mm_getcsr") == 0) {
        return tb_inst_x86_stmxcsr(func);
    } else if (strcmp(name, "_umul128") == 0) {
        return tb_inst_uint(func, TB_TYPE_I64, 0);
    } else if (strcmp(name, "_umul128") == 0) {
        return tb_inst_uint(func, TB_TYPE_I64, 0);
    } else {
        internal_error("unimplemented builtin! %s", name);
        return 0;
    }
}
#endif /* CUIK_USE_TB */

const Cuik_ArchDesc* cuik_get_x64_target_desc(void) {
    static Cuik_ArchDesc t = { 0 };
    if (t.builtin_func_map == NULL) {
        // TODO(NeGate): make this thread safe
        BuiltinBinding* builtins = NULL;

        // gcc/clang
        shput(builtins, "__builtin_expect", 1);
        shput(builtins, "__builtin_trap", 1);
        shput(builtins, "__builtin_unreachable", 1);
        shput(builtins, "__builtin_mul_overflow", 1);

        // cuik internal
        shput(builtins, "__c11_atomic_compare_exchange_strong", 1);
        shput(builtins, "__c11_atomic_thread_fence", 1);
        shput(builtins, "__c11_atomic_signal_fence", 1);
        shput(builtins, "__c11_atomic_is_lock_free", 1);
        shput(builtins, "__c11_atomic_load", 1);
        shput(builtins, "__c11_atomic_store", 1);
        shput(builtins, "__c11_atomic_exchange", 1);
        shput(builtins, "__c11_atomic_compare_exchange_strong", 1);
        shput(builtins, "__c11_atomic_compare_exchange_weak", 1);
        shput(builtins, "__c11_atomic_fetch_add", 1);
        shput(builtins, "__c11_atomic_fetch_sub", 1);
        shput(builtins, "__c11_atomic_fetch_or", 1);
        shput(builtins, "__c11_atomic_fetch_xor", 1);
        shput(builtins, "__c11_atomic_fetch_and", 1);

        // msvc intrinsics
        shput(builtins, "_mm_getcsr", 1);
        shput(builtins, "_mm_setcsr", 1);
        shput(builtins, "__debugbreak", 1);
        shput(builtins, "__va_start", 1);
        shput(builtins, "_umul128", 1);
        shput(builtins, "_mul128", 1);
        shput(builtins, "_InterlockedExchange", 1);
        shput(builtins, "_InterlockedCompareExchange", 1);

        t = (Cuik_ArchDesc){
            .arch = TB_ARCH_X86_64,

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
    }

    return &t;
}
