#pragma once
#include "../common.h"
#include <back/ir_gen.h>
#include <preproc/cpp.h>

typedef struct BuiltinBinding {
    char* key;
    int value;
} BuiltinBinding;

struct Cuik_TargetDesc {
    // stringmap that goes from builtin function names to
    // an index used to refer to them later on
    BuiltinBinding* builtin_func_map;

    // initializes some target specific macro defines
    void (*set_defines)(Cuik_CPP* cpp);

    // Callee ABI handling:
    TB_FunctionPrototype* (*create_prototype)(TranslationUnit* tu, Cuik_Type* type_index);

    // Caller ABI handling:
    // returns the aggregate size, if it's zero there's no aggregate
    bool (*pass_return_via_reg)(TranslationUnit* tu, Cuik_Type* type_index);
    // Number of IR parameters generated from the data type
    int (*deduce_parameter_usage)(TranslationUnit* tu, Cuik_Type* type_index);
    int (*pass_parameter)(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Reg* out_param);

    // when one of the builtins is spotted in the semantics pass, we might need to resolve it's
    // type
    Cuik_Type* (*type_check_builtin)(TranslationUnit* tu, Expr* e, const char* name, int arg_count, Expr** args);

    // when one of the builtins are triggered we call this to generate it's code
    TB_Register (*compile_builtin)(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args);
};
