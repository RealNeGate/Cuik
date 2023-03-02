#include "targets.h"
#include <front/sema.h>

static void set_defines(const Cuik_Target* target, Cuik_CPP* cpp) {
    target_generic_set_defines(cpp, target->system, true, true);
    cuikpp_define_cstr(cpp, "__WASM__", "1");
}

static Cuik_Type* type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, const char* builtin_value, int arg_count, Expr** args) {
    return target_generic_type_check_builtin(tu, e, name, builtin_value, arg_count, args);
}

static TB_Node* compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args) {
    return target_generic_compile_builtin(tu, func, name, arg_count, args).r;
}

#ifdef CUIK_USE_TB
static bool should_pass_on_reg(TranslationUnit* tu, Cuik_Type* type) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        return false;
    } else {
        return type->size < 8;
    }
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, Cuik_Type* type) {
    return target_generic_create_prototype(should_pass_on_reg, tu, type);
}

static bool pass_return_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    return should_pass_on_reg(tu, type);
}

static int deduce_parameter_usage(TranslationUnit* tu, Cuik_QualType type) {
    return 1;
}

static TB_Node* get_parameter(TranslationUnit* tu, TB_Function* func, Cuik_Type* type, TB_Node* reg) {
    if (should_pass_on_reg(tu, type)) {
        return tb_inst_load(func, TB_TYPE_PTR, reg, 8, false);
    } else {
        return reg;
    }
}

static int pass_parameter(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Node** out_param) {
    return target_generic_pass_parameter(should_pass_on_reg, tu, func, e, is_vararg, out_param);
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

    t->size_type = &t->unsigned_ints[CUIK_BUILTIN_LLONG];
    t->ptrdiff_type = &t->signed_ints[CUIK_BUILTIN_LLONG];

    cuik_target_build(t);
    return t;
}
