#pragma once
#include <common.h>
#include <arena.h>
#include <hash_map.h>
#include <cuik.h>

#ifdef CONFIG_HAS_TB
#include "../ir_gen.h"
#endif

enum {
    CUIK_BUILTIN_CHAR,
    CUIK_BUILTIN_SHORT,
    CUIK_BUILTIN_INT,
    CUIK_BUILTIN_LONG,
    CUIK_BUILTIN_LLONG
};

typedef NL_Strmap(const char*) BuiltinTable;

struct Cuik_Target {
    Cuik_Environment env;
    Cuik_System system;

    #ifdef CONFIG_HAS_TB
    TB_Arch arch;
    #endif

    bool is_big_endian;

    // tells us if a name is maps to a builtin
    BuiltinTable builtin_func_map;

    // we don't have any enforcements on primitive integers other than what
    // the spec might say.
    //   sizeof(char) <= sizeof(short) <= sizeof(int) <= sizeof(long) <= sizeof(long long)
    uint32_t int_bits[5];
    Cuik_Type signed_ints[5], unsigned_ints[5];

    // this is size_t and ptrdiff_t
    Cuik_Type size_type, ptrdiff_type;

    // this is used for uintptr_t, intptr_t and pointer types
    size_t pointer_byte_size;

    // initializes some target specific macro defines
    void (*set_defines)(const Cuik_Target* self, Cuik_CPP* cpp);

    #ifdef CONFIG_HAS_TB
    // when one of the builtins are triggered we call this to generate it's code
    TB_Node* (*compile_builtin)(TranslationUnit* tu, TB_GraphBuilder* g, const char* name, int arg_count, ValDesc* args);
    #endif /* CONFIG_HAS_TB */
};

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN
#define TARGET_NEEDS_BYTESWAP(target) (!(target)->is_big_endian)
#else
#define TARGET_NEEDS_BYTESWAP(target) ((target)->is_big_endian)
#endif

// Called after initializing the integer sizes along with the callbacks.
// Verify that the integer sizes are compliant and initialize real pointers
// to Cuik_Types for them.
void cuik_target_build(Cuik_Target* target);

#ifdef CONFIG_HAS_TB
typedef bool (*ShouldPassViaReg)(TranslationUnit* tu, Cuik_Type* type);
TB_FunctionPrototype* target_generic_create_prototype(ShouldPassViaReg fn, TranslationUnit* tu, Cuik_Type* type);
#endif

void target_generic_set_defines(Cuik_CPP* cpp, Cuik_System sys, bool is_64bit, bool is_little_endian);
void target_generic_fill_builtin_table(BuiltinTable* builtins);
