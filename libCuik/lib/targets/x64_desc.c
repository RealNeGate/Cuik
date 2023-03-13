#include "targets.h"
#include "../front/sema.h"

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

static void set_defines(const Cuik_Target* target, Cuik_CPP* cpp) {
    target_generic_set_defines(cpp, target->system, true, true);

    cuikpp_define_cstr(cpp, "__x86_64",   "1");
    cuikpp_define_cstr(cpp, "__x86_64__", "1");
    cuikpp_define_cstr(cpp, "__amd64",    "1");
    cuikpp_define_cstr(cpp, "__amd64__",  "1");

    if (target->system == CUIK_SYSTEM_WINDOWS) {
        cuikpp_define_cstr(cpp, "_M_X64", "100");
        cuikpp_define_cstr(cpp, "_AMD64_", "100");
        cuikpp_define_cstr(cpp, "_M_AMD64", "100");
    }
}

// TODO(NeGate): Add some type checking utilities to match against a list of types since that's kinda important :p
static Cuik_Type* type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, const char* builtin_value, int arg_count, Expr** args) {
    Cuik_Type* t = target_generic_type_check_builtin(tu, e, name, builtin_value, arg_count, args);
    if (t != NULL) {
        return t;
    }

    Cuik_Type* target_signed_ints = tu->target->signed_ints;
    Cuik_Type* target_unsigned_ints = tu->target->unsigned_ints;

    if (strcmp(name, "_mm_setcsr") == 0) {
        if (arg_count != 1) {
            diag_err(&tu->tokens, e->loc, "parameter mismatch (got %s, expected %d)", name, 1);
            return &cuik__builtin_void;
        }

        Cuik_Type* arg_type = sema_expr(tu, args[0]);
        Cuik_Type* int_type = &target_unsigned_ints[CUIK_BUILTIN_INT];
        if (!type_compatible(tu, arg_type, int_type, args[0])) {
            diag_err(&tu->tokens, args[0]->loc, "Could not implicitly convert type %!T into %!T.", arg_type, int_type);
            return &cuik__builtin_void;
        }

        args[0]->cast_type = cuik_uncanonical_type(int_type);
        return &cuik__builtin_void;
    } else if (strcmp(name, "_mm_getcsr") == 0) {
        if (arg_count != 0) {
            diag_err(&tu->tokens, e->loc, "%s requires 0 arguments", name);
        }

        return &target_unsigned_ints[CUIK_BUILTIN_INT];
    } else if (strcmp(name, "__rdtsc") == 0) {
        return &target_unsigned_ints[CUIK_BUILTIN_LLONG];
    } else if (strcmp(name, "__readgsqword") == 0) {
        return &target_unsigned_ints[CUIK_BUILTIN_SHORT];
    } else {
        diag_err(&tu->tokens, e->loc, "unimplemented builtin %s", name);
        return &cuik__builtin_void;
    }
}

#ifdef CUIK_USE_TB
// on Win64 all structs that have a size of 1,2,4,8
// or any scalars are passed via registers
static bool win64_should_pass_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        return type->size <= 8;
    } else {
        return true;
    }
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, Cuik_Type* type) {
    return target_generic_create_prototype(win64_should_pass_via_reg, tu, type);
}

static bool pass_return_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    return win64_should_pass_via_reg(tu, type);
}

static TB_Node* get_parameter(TranslationUnit* tu, TB_Function* func, Cuik_Type* type, TB_Node* reg) {
    if (win64_should_pass_via_reg(tu, type)) {
        return reg;
    } else {
        return tb_inst_load(func, TB_TYPE_PTR, reg, 8, false);
    }
}

static int deduce_parameter_usage(TranslationUnit* tu, Cuik_QualType type) {
    return 1;
}

static int pass_parameter(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Node** out_param) {
    return target_generic_pass_parameter(win64_should_pass_via_reg, tu, func, e, is_vararg, out_param);
}

static TB_Node* compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args) {
    BuiltinResult r = target_generic_compile_builtin(tu, func, name, arg_count, args);
    if (!r.failure) {
        return r.r;
    }

    // x64 specific builtins
    if (strcmp(name, "_mm_setcsr") == 0) {
        return tb_inst_x86_ldmxcsr(func, irgen_as_rvalue(tu, func, args[0]));
    } else if (strcmp(name, "_mm_getcsr") == 0) {
        return tb_inst_x86_stmxcsr(func);
    } else if (strcmp(name, "__rdtsc") == 0) {
        return tb_inst_x86_rdtsc(func);
    } else if (strcmp(name, "__readgsqword") == 0) {
        // TODO(NeGate): implement readgs/writegs with all the type variants
        return tb_inst_uint(func, TB_TYPE_I16, 0);
    } else {
        assert(0 && "unimplemented builtin!");
        return 0;
    }
}
#endif /* CUIK_USE_TB */

Cuik_Target* cuik_target_x64(Cuik_System system, Cuik_Environment env) {
    NL_Strmap(const char*) builtins = NULL;
    target_generic_fill_builtin_table(&builtins);
    nl_strmap_put_cstr(builtins, "_mm_getcsr", NULL);
    nl_strmap_put_cstr(builtins, "_mm_setcsr", NULL);
    nl_strmap_put_cstr(builtins, "__readgsqword", NULL);
    nl_strmap_put_cstr(builtins, "__rdtsc", NULL);

    Cuik_Target* t = cuik_malloc(sizeof(Cuik_Target));
    *t = (Cuik_Target){
        .env = env,
        .system = system,

        .int_bits = { 8, 16, 32, 64, 64 },

        #ifdef CUIK_USE_TB
        .arch = TB_ARCH_X86_64,
        #endif

        .builtin_func_map = builtins,
        .set_defines = set_defines,
        #ifdef CUIK_USE_TB
        .create_prototype = create_prototype,
        .pass_return_via_reg = pass_return_via_reg,
        .get_parameter = get_parameter,
        .deduce_parameter_usage = deduce_parameter_usage,
        .pass_parameter = pass_parameter,
        .compile_builtin = compile_builtin,
        #endif /* CUIK_USE_TB */
        .type_check_builtin = type_check_builtin,
    };

    // on MSVC, long means 32bit and this is necessary because of stuff like
    // DWORD being defined as unsigned long.
    if (env == CUIK_ENV_MSVC) {
        t->int_bits[CUIK_BUILTIN_LONG] = 32;
    }

    t->size_type = &t->unsigned_ints[CUIK_BUILTIN_LLONG];
    t->ptrdiff_type = &t->signed_ints[CUIK_BUILTIN_LLONG];

    cuik_target_build(t);
    return t;
}
