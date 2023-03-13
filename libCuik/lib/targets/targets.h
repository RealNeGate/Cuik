#pragma once
#include <common.h>
#include <arena.h>

#include "../preproc/lexer.h"

#ifdef CUIK_USE_TB
#include "../back/ir_gen.h"
#endif

enum {
    CUIK_BUILTIN_CHAR,
    CUIK_BUILTIN_SHORT,
    CUIK_BUILTIN_INT,
    CUIK_BUILTIN_LONG,
    CUIK_BUILTIN_LLONG
};

struct Cuik_Target {
    Cuik_Environment env;
    Cuik_System system;

    #ifdef CUIK_USE_TB
    TB_Arch arch;
    #endif

    // tells us if a name is maps to a builtin
    // NL_Strmap(const char*)
    const char** builtin_func_map;

    // we don't have any enforcements on primitive integers other than what
    // the spec might say.
    //   sizeof(char) <= sizeof(short) <= sizeof(int) <= sizeof(long) <= sizeof(long long)
    uint32_t int_bits[5];
    Cuik_Type signed_ints[5], unsigned_ints[5];

    // this is size_t and ptrdiff_t
    Cuik_Type *size_type, *ptrdiff_type;

    // initializes some target specific macro defines
    void (*set_defines)(const Cuik_Target* self, Cuik_CPP* cpp);

    // when one of the builtins is spotted in the semantics pass, we might need to resolve it's
    // type
    Cuik_Type* (*type_check_builtin)(TranslationUnit* tu, Expr* e, const char* name, const char* builtin_value, int arg_count, Expr** args);

    #ifdef CUIK_USE_TB
    // Callee ABI handling:
    TB_FunctionPrototype* (*create_prototype)(TranslationUnit* tu, Cuik_Type* type);
    TB_Node* (*get_parameter)(TranslationUnit* tu, TB_Function* func, Cuik_Type* type, TB_Node* reg);

    // Caller ABI handling:
    // returns the aggregate size, if it's zero there's no aggregate
    bool (*pass_return_via_reg)(TranslationUnit* tu, Cuik_Type* type);
    // Number of IR parameters generated from the data type
    int (*deduce_parameter_usage)(TranslationUnit* tu, Cuik_QualType type);
    int (*pass_parameter)(TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Node** out_param);

    // when one of the builtins are triggered we call this to generate it's code
    TB_Node* (*compile_builtin)(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args);
    #endif /* CUIK_USE_TB */
};

// Called after initializing the integer sizes along with the callbacks.
// Verify that the integer sizes are compliant and initialize real pointers
// to Cuik_Types for them.
void cuik_target_build(Cuik_Target* target);

#ifdef CUIK_USE_TB
typedef struct {
    TB_Node* r;
    bool failure;
} BuiltinResult;

BuiltinResult target_generic_compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, Expr** args);

typedef bool (*ShouldPassViaReg)(TranslationUnit* tu, Cuik_Type* type);

int target_generic_pass_parameter(ShouldPassViaReg fn, TranslationUnit* tu, TB_Function* func, Expr* e, bool is_vararg, TB_Node** out_param);
TB_FunctionPrototype* target_generic_create_prototype(ShouldPassViaReg fn, TranslationUnit* tu, Cuik_Type* type);
#endif

void target_generic_set_defines(Cuik_CPP* cpp, Cuik_System sys, bool is_64bit, bool is_little_endian);

// returns NULL type if it didn't handle the builtin
Cuik_Type* target_generic_type_check_builtin(TranslationUnit* tu, Expr* e, const char* name, const char* builtin_value, int arg_count, Expr** args);
void target_generic_fill_builtin_table(NL_Strmap(const char*)* builtins);
