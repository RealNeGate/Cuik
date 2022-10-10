#ifdef CUIK_USE_TB
#include "targets.h"
#include <front/sema.h>

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

void target_generic_set_defines(Cuik_CPP* cpp, Cuik_System sys, bool is_64bit, bool is_little_endian) {
    cuikpp_define_empty_cstr(cpp, is_64bit ? "_CUIK_TARGET_64BIT_" : "_CUIK_TARGET_32BIT_");
    cuikpp_define_cstr(cpp, is_little_endian ? "__LITTLE_ENDIAN__" : "__BIG_ENDIAN__", "1");
    cuikpp_define_cstr(cpp, is_little_endian ? "__BIG_ENDIAN__" : "__LITTLE_ENDIAN__", "0");

    if (sys == CUIK_SYSTEM_WINDOWS) {
        cuikpp_define_cstr(cpp, "_WIN32", "1");
        cuikpp_define_cstr(cpp, "_WIN64", "1");
    } else if (sys == CUIK_SYSTEM_LINUX) {
        cuikpp_define_cstr(cpp, "__LP64__", "1");
        cuikpp_define_cstr(cpp, "__linux", "1");
        cuikpp_define_cstr(cpp, "linux", "1");
    }

    // stdatomic.h lock free stuff
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_BOOL_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_CHAR_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_CHAR16_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_CHAR32_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_WCHAR_T_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_SHORT_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_INT_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_LONG_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_LLONG_LOCK_FREE", "1");
    cuikpp_define_cstr(cpp, "__CUIK_ATOMIC_POINTER_LOCK_FREE", "1");
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

Cuik_Type* target_generic_type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, int arg_count, Expr** args) {
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
    } else if (strcmp(name, "__builtin_syscall") == 0) {
        if (arg_count < 1) {
            REPORT_EXPR(ERROR, e, "%s requires at least one argument (the syscall number)", name);
            return &builtin_types[TYPE_VOID];
        }

        // fn(syscall number, ...)
        args[0]->cast_type = &builtin_types[TYPE_INT];
        cuik__type_check_args(tu, e, 1, args);

        for (size_t i = 1; i < arg_count; i++) {
            args[i]->cast_type = sema_expr(tu, args[i]);
        }

        return &builtin_types[TYPE_LONG];
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
    } else if (strcmp(name, "__assume") == 0) {
        if (arg_count != 1) {
            REPORT_EXPR(ERROR, e, "%s requires 1 argument", name);
        }

        // fn(int)
        args[0]->cast_type = &builtin_types[TYPE_INT];

        cuik__type_check_args(tu, e, arg_count, args);
        return &builtin_types[TYPE_VOID];
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
        return NULL;
    }
}

#define ZZZ(x) (BuiltinResult){ x }
BuiltinResult target_generic_compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args) {
    if (strcmp(name, "_InterlockedExchange") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        return ZZZ(tb_inst_atomic_xchg(func, dst, src, TB_MEM_ORDER_SEQ_CST));
    } else if (strcmp(name, "__c11_atomic_compare_exchange_strong") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg comparand = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg exchange = irgen_as_rvalue(tu, func, args[2]);

        assert(args[1]->cast_type->kind == KIND_PTR);
        Cuik_Type* ty = args[1]->cast_type->ptr_to;

        // for odd reasons C11 compare exchange uses a pointer for the comparand
        comparand = tb_inst_load(func, ctype_to_tbtype(ty), comparand, ty->align);

        TB_CmpXchgResult r = tb_inst_atomic_cmpxchg(func, addr, comparand, exchange, TB_MEM_ORDER_SEQ_CST, TB_MEM_ORDER_SEQ_CST);
        return ZZZ(r.old_value);
    } else if (strcmp(name, "_InterlockedCompareExchange") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg exchange = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg comparand = irgen_as_rvalue(tu, func, args[2]);

        TB_CmpXchgResult r = tb_inst_atomic_cmpxchg(func, addr, comparand, exchange, TB_MEM_ORDER_SEQ_CST, TB_MEM_ORDER_SEQ_CST);
        return ZZZ(r.old_value);
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
        return ZZZ(tb_inst_atomic_xchg(func, dst, src, order));
    } else if (strcmp(name, "__c11_atomic_load") == 0) {
        TB_Reg addr = irgen_as_rvalue(tu, func, args[0]);

        assert(args[0]->cast_type->kind == KIND_PTR);
        TB_DataType dt = ctype_to_tbtype(args[0]->cast_type->ptr_to);

        assert(args[1]->op == EXPR_INT && args[1]->int_num.num < 6);
        int order = args[1]->int_num.num;
        return ZZZ(tb_inst_atomic_load(func, addr, dt, order));
    } else if (strcmp(name, "__c11_atomic_fetch_add") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        assert(args[2]->op == EXPR_INT && args[2]->int_num.num < 6);
        int order = args[2]->int_num.num;
        return ZZZ(tb_inst_atomic_add(func, dst, src, order));
    } else if (strcmp(name, "__c11_atomic_fetch_sub") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg src = irgen_as_rvalue(tu, func, args[1]);

        assert(args[2]->op == EXPR_INT && args[2]->int_num.num < 6);
        int order = args[2]->int_num.num;
        return ZZZ(tb_inst_atomic_sub(func, dst, src, order));
    } else if (strcmp(name, "__builtin_mul_overflow") == 0) {
        Cuik_Type* type = args[0]->cast_type;
        TB_DataType dt = ctype_to_tbtype(type);

        TB_Reg a = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg b = irgen_as_rvalue(tu, func, args[1]);
        TB_Reg c = irgen_as_rvalue(tu, func, args[2]);

        TB_Reg result = tb_inst_mul(func, a, b, 0);
        tb_inst_store(func, dt, c, result, type->align);

        return ZZZ(tb_inst_cmp_ilt(func, result, a, false));
    } else if (strcmp(name, "__builtin_unreachable") == 0) {
        tb_inst_unreachable(func);
        tb_inst_set_label(func, tb_basic_block_create(func));
        return ZZZ(TB_NULL_REG);
    } else if (strcmp(name, "__builtin_expect") == 0) {
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        return ZZZ(dst);
    } else if (strcmp(name, "__builtin_trap") == 0) {
        tb_inst_trap(func);
        tb_inst_set_label(func, tb_basic_block_create(func));
        return ZZZ(TB_NULL_REG);
    } else if (strcmp(name, "__builtin_syscall") == 0) {
        TB_Reg num = irgen_as_rvalue(tu, func, args[0]);
        TB_Reg* arg_regs = tls_push((arg_count - 1) * sizeof(TB_Reg));
        for (size_t i = 1; i < arg_count; i++) {
            arg_regs[i - 1] = irgen_as_rvalue(tu, func, args[i]);
        }

        TB_Reg result = tb_inst_syscall(func, TB_TYPE_I64, num, arg_count - 1, arg_regs);
        tls_restore(arg_regs);

        return ZZZ(result);
    } else if (strcmp(name, "__assume") == 0) {
        TB_Reg cond = irgen_as_rvalue(tu, func, args[0]);
        TB_Label no_reach = tb_basic_block_create(func);
        TB_Label skip = tb_basic_block_create(func);

        tb_inst_if(func, cond, skip, no_reach);
        tb_inst_set_label(func, no_reach);
        tb_inst_unreachable(func);
        tb_inst_set_label(func, skip);

        return ZZZ(TB_NULL_REG);
    } else if (strcmp(name, "__debugbreak") == 0) {
        tb_inst_debugbreak(func);
        return ZZZ(TB_NULL_REG);
    } else if (strcmp(name, "__va_start") == 0) {
        // TODO(NeGate): Remove this later because it will emotionally damage our optimizer.
        // the issue is that it blatantly accesses out of bounds and we should probably just
        // have a node for va_start in the backend instead.
        TB_Reg dst = irgen_as_rvalue(tu, func, args[0]);
        IRVal src = irgen_expr(tu, func, args[1]);
        assert(src.value_type == LVALUE);

        tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_va_start(func, src.reg), 8);
        return ZZZ(TB_NULL_REG);
    } else if (strcmp(name, "_umul128") == 0) {
        fprintf(stderr, "TODO _umul128\n");
        return ZZZ(tb_inst_uint(func, TB_TYPE_I64, 0));
    } else {
        return (BuiltinResult){ 0, true };
    }
}

void target_generic_fill_builtin_table(NL_Strmap(bool)* builtins) {
    // gcc/clang
    nl_strmap_put_cstr(*builtins, "__builtin_expect", 1);
    nl_strmap_put_cstr(*builtins, "__builtin_trap", 1);
    nl_strmap_put_cstr(*builtins, "__builtin_syscall", 1);
    nl_strmap_put_cstr(*builtins, "__builtin_unreachable", 1);
    nl_strmap_put_cstr(*builtins, "__builtin_mul_overflow", 1);

    // cuik internal
    nl_strmap_put_cstr(*builtins, "__c11_atomic_compare_exchange_strong", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_thread_fence", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_signal_fence", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_is_lock_free", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_load", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_store", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_exchange", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_compare_exchange_strong", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_compare_exchange_weak", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_fetch_add", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_fetch_sub", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_fetch_or", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_fetch_xor", 1);
    nl_strmap_put_cstr(*builtins, "__c11_atomic_fetch_and", 1);

    // msvc intrinsics
    nl_strmap_put_cstr(*builtins, "__assume", 1);
    nl_strmap_put_cstr(*builtins, "__debugbreak", 1);
    nl_strmap_put_cstr(*builtins, "__va_start", 1);
    nl_strmap_put_cstr(*builtins, "_umul128", 1);
    nl_strmap_put_cstr(*builtins, "_mul128", 1);
    nl_strmap_put_cstr(*builtins, "_InterlockedExchange", 1);
    nl_strmap_put_cstr(*builtins, "_InterlockedCompareExchange", 1);
}
#endif