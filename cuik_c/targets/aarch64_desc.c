#include "targets.h"
#include "../sema.h"

static void set_defines(const Cuik_Target* target, Cuik_CPP* cpp) {
    target_generic_set_defines(cpp, target->system, true, true);

    cuikpp_define_cstr(cpp, "__arm",          "1");
    cuikpp_define_cstr(cpp, "__arm__",        "1");
    cuikpp_define_cstr(cpp, "__ARM_ARCH_8__", "1");

    if (target->system == CUIK_SYSTEM_WINDOWS) {
        cuikpp_define_cstr(cpp, "_M_ARM64", "1");
    }
}

#ifdef CONFIG_HAS_TB
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

static TB_Node* compile_builtin(TranslationUnit* tu, TB_GraphBuilder* g, const char* name, int arg_count, ValDesc* args) {
    return NULL;
}
#endif /* CONFIG_HAS_TB */

Cuik_Target* cuik_target_aarch64(Cuik_System system, Cuik_Environment env) {
    BuiltinTable builtins;
    nl_map_create(builtins, 128);

    target_generic_fill_builtin_table(&builtins);

    // #define X(name, format) nl_map_put_cstr(builtins, #name, format);
    // #undef X

    Cuik_Target* t = cuik_malloc(sizeof(Cuik_Target));
    *t = (Cuik_Target){
        .env = env,
        .system = system,

        .int_bits = { 8, 16, 32, 64, 64 },
        .pointer_byte_size = 8,

        #ifdef CONFIG_HAS_TB
        .arch = TB_ARCH_AARCH64,
        #endif

        .builtin_func_map = builtins,
        .set_defines = set_defines,
        #ifdef CONFIG_HAS_TB
        .compile_builtin = compile_builtin,
        #endif /* CONFIG_HAS_TB */
    };

    // on MSVC, long means 32bit and this is necessary because of stuff like
    // DWORD being defined as unsigned long.
    if (env == CUIK_ENV_MSVC) {
        t->int_bits[CUIK_BUILTIN_LONG] = 32;
    }

    cuik_target_build(t);

    // bake out size_t and ptrdiff_t after long long is ready
    t->size_type = t->unsigned_ints[CUIK_BUILTIN_LLONG];
    t->size_type.also_known_as = "size_t";

    t->ptrdiff_type = t->signed_ints[CUIK_BUILTIN_LLONG];
    t->ptrdiff_type.also_known_as = "ptrdiff_t";

    return t;
}
