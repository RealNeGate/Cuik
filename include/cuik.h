#pragma once
#include "cuik_prelude.h"
#include <arena.h>
#include <dyn_array.h>

#ifndef _WIN32
int sprintf_s(char* buffer, size_t len, const char* format, ...);
#endif

#ifdef CONFIG_HAS_TB
#include <tb.h>
#endif

typedef struct CompilationUnit CompilationUnit;
typedef struct TranslationUnit TranslationUnit;
typedef struct Cuik_Toolchain Cuik_Toolchain;
typedef struct Cuik_DriverArgs Cuik_DriverArgs;

////////////////////////////////////////////
// Target descriptor
////////////////////////////////////////////
typedef enum Cuik_System {
    CUIK_SYSTEM_WINDOWS,
    CUIK_SYSTEM_LINUX,
    CUIK_SYSTEM_MACOS,
    CUIK_SYSTEM_ANDROID,
    CUIK_SYSTEM_WEB,
} Cuik_System;

typedef enum Cuik_Subsystem {
    CUIK_SUBSYSTEM_PC,
} Cuik_Subsystem;

typedef enum Cuik_Environment {
    CUIK_ENV_MSVC,
    CUIK_ENV_GNU,
    CUIK_ENV_WEB,
} Cuik_Environment;

typedef struct Cuik_TargetDesc {
    Cuik_System sys;
} Cuik_TargetDesc;

typedef struct Cuik_Target Cuik_Target;

// the naming convention of the targets here is
//
//    cuik_target_XXX_YYY_ZZZ
//
// where XXX is arch, YYY is system, ZZZ is environment
//
CUIK_API Cuik_Target* cuik_target_x64(Cuik_System system, Cuik_Environment env);
CUIK_API Cuik_Target* cuik_target_aarch64(Cuik_System system, Cuik_Environment env);
CUIK_API Cuik_Target* cuik_target_mips32(Cuik_System system, Cuik_Environment env);
CUIK_API Cuik_Target* cuik_target_mips64(Cuik_System system, Cuik_Environment env);
CUIK_API Cuik_Target* cuik_target_wasm32(Cuik_System system, Cuik_Environment env);
CUIK_API void cuik_free_target(Cuik_Target* target);

CUIK_API Cuik_System cuik_get_target_system(const Cuik_Target* t);
CUIK_API Cuik_Environment cuik_get_target_env(const Cuik_Target* t);

CUIK_API Cuik_Target* cuik_target_host(void);

////////////////////////////////////////////
// General Cuik stuff
////////////////////////////////////////////
CUIK_API void cuik_init(bool use_crash_handler);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
#define CUIK_FOR_EACH_TU(it, cu) for (TranslationUnit* it = cuik_first_translation_unit(cu); it; it = cuik_next_translation_unit(it))

// if the translation units are in a compilation unit you can walk this chain of pointers
// to read them
CUIK_API TranslationUnit* cuik_first_translation_unit(CompilationUnit* restrict cu);
CUIK_API TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu);

CUIK_API CompilationUnit* cuik_create_compilation_unit(void);
CUIK_API void cuik_lock_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_unlock_compilation_unit(CompilationUnit* restrict cu);
CUIK_API void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
CUIK_API void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
CUIK_API size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu);

#ifdef CONFIG_HAS_TB
CUIK_API void cuik_compilation_unit_set_tb_module(CompilationUnit* restrict cu, TB_Module* mod);
CUIK_API TB_Module* cuik_compilation_unit_tb_module(CompilationUnit* restrict cu);
#endif

#include <perf.h> // from common
#include "cuik_lex.h"
#include "cuik_ast.h"
#include "cuik_parse.h"
#include "cuik_driver.h"
#include "cuik_fs.h"

#ifdef CONFIG_HAS_CUIKGO
#include "cuik_go.h"
#endif

#ifdef CONFIG_HAS_TB
#include "cuik_irgen.h"
#endif
