#include "targets.h"
#include "../front/sema.h"

static void set_defines(const Cuik_Target* target, Cuik_CPP* cpp) {
    target_generic_set_defines(cpp, target->system, true, true);
}

#ifdef CUIK_USE_TB
static bool win64_should_pass_via_reg(TranslationUnit* tu, Cuik_Type* type) {
    if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        return type->size <= tu->target->pointer_byte_size;
    } else {
        return true;
    }
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, Cuik_Type* type) {
    return target_generic_create_prototype(win64_should_pass_via_reg, tu, type);
}

static TB_Node* compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, IRVal* args) {
    BuiltinResult r = target_generic_compile_builtin(tu, func, name, arg_count, args);
    if (r.failure) {
        assert(0 && "unimplemented builtin!");
        return 0;
    } else {
        return r.r;
    }
}
#endif /* CUIK_USE_TB */

Cuik_Target* cuik_target_wasm32(Cuik_System system, Cuik_Environment env) {
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
        .pointer_byte_size = 4,

        #ifdef CUIK_USE_TB
        .arch = TB_ARCH_WASM32,
        #endif

        .builtin_func_map = builtins,
        .set_defines = set_defines,
        #ifdef CUIK_USE_TB
        .compile_builtin = compile_builtin,
        #endif /* CUIK_USE_TB */
    };

    cuik_target_build(t);

    // bake out size_t and ptrdiff_t after int is ready
    t->size_type = t->unsigned_ints[CUIK_BUILTIN_INT];
    t->size_type.also_known_as = "size_t";

    t->ptrdiff_type = t->signed_ints[CUIK_BUILTIN_INT];
    t->ptrdiff_type.also_known_as = "ptrdiff_t";

    return t;
}
