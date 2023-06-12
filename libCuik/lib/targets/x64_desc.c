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

static int pass_parameter(TranslationUnit* tu, TB_Function* func, IRVal arg, bool is_vararg, TB_Node** out_param) {
    return target_generic_pass_parameter(win64_should_pass_via_reg, tu, func, arg, is_vararg, out_param);
}

static TB_Node* compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, IRVal* args) {
    BuiltinResult r = target_generic_compile_builtin(tu, func, name, arg_count, args);
    if (!r.failure) {
        return r.r;
    }

    // x64 specific builtins
    if (strcmp(name, "_mm_setcsr") == 0) {
        return tb_inst_x86_ldmxcsr(func, cvt2rval(tu, func, &args[0]));
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
    BuiltinTable builtins;
    nl_map_create(builtins, 128);

    target_generic_fill_builtin_table(&builtins);

    #define X(name, format) nl_map_put_cstr(builtins, #name, format);
    X("_mm_getcsr",    "v i");
    X("_mm_setcsr",    "i v");
    X("__readgsqword", " s");
    X("__rdtsc",       " L");
    #undef X

    Cuik_Target* t = cuik_malloc(sizeof(Cuik_Target));
    *t = (Cuik_Target){
        .env = env,
        .system = system,

        .int_bits = { 8, 16, 32, 64, 64 },
        .pointer_byte_size = 8,

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
