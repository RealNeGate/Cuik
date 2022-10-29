#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup _strdup
#else
int sprintf_s(char* buffer, size_t len, const char* format, ...);
#endif

#ifdef CUIK_USE_TB
#include <tb.h>
#endif

typedef struct CompilationUnit CompilationUnit;
typedef struct TranslationUnit TranslationUnit;

////////////////////////////////////////////
// Interfaces
////////////////////////////////////////////
typedef struct Cuik_IThreadpool {
    // fed into the member functions here
    void* user_data;

    // runs the function fn with arg as the parameter on a thread
    void (*submit)(void* user_data, void fn(void*), void* arg);

    // tries to work one job before returning (can also not work at all)
    void (*work_one_job)(void* user_data);
} Cuik_IThreadpool;

typedef struct Cuik_IProfiler {
    void* user_data;

    void (*start)(void* user_data);
    void (*stop)(void* user_data);

    void (*begin_plot)(void* user_data, uint64_t nanos, const char* label);
    void (*end_plot)(void* user_data, uint64_t nanos);
} Cuik_IProfiler;

typedef struct Cuik_IDiagnostic {
    void* user_data;

    //void(*report)(void* user_data, const Cuik_Report* r);
} Cuik_IDiagnostic;

// for doing calls on the interfaces
#define CUIK_CALL(object, action, ...) ((object)->action((object)->user_data, ##__VA_ARGS__))

////////////////////////////////////////////
// Target descriptor
////////////////////////////////////////////
typedef enum Cuik_System {
    CUIK_SYSTEM_WINDOWS,
    CUIK_SYSTEM_MACOS,
    CUIK_SYSTEM_LINUX,
    CUIK_SYSTEM_ANDROID,
} Cuik_System;

typedef enum Cuik_Environment {
    CUIK_ENV_MSVC,
    CUIK_ENV_GNU,
} Cuik_Environment;

// these can be fed into the preprocessor and parser to define
// the correct builtin functions, types, and predefined macros
typedef struct Cuik_Target Cuik_Target;

// the naming convention of the targets here is
//
//    cuik_target_XXX_YYY_ZZZ
//
// where XXX is arch, YYY is system, ZZZ is environment
//
Cuik_Target* cuik_target_x64(Cuik_System system, Cuik_Environment env);
void cuik_free_target(Cuik_Target* target);

Cuik_System cuik_get_target_system(const Cuik_Target* t);
Cuik_Environment cuik_get_target_env(const Cuik_Target* t);

////////////////////////////////////////////
// Profiler
////////////////////////////////////////////
// lock_on_plot is true if the profiler->plot function cannot be called on multiple threads at
// the same time.
void cuik_start_global_profiler(const Cuik_IProfiler* profiler, bool lock_on_plot);
void cuik_stop_global_profiler(void);
bool cuik_is_profiling(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
uint64_t cuik_time_in_nanos(void);

// Reports a region of time to the profiler callback
void cuik_profile_region_start(uint64_t now, const char* fmt, ...);
void cuik_profile_region_end(void);

// Usage:
// CUIK_TIMED_BLOCK("Beans %d", 5) {
//   ...
// }
#define CUIK_TIMED_BLOCK(...) for (uint64_t __i = (cuik_profile_region_start(cuik_time_in_nanos(), __VA_ARGS__), 0); __i < 1; __i++, cuik_profile_region_end())

////////////////////////////////////////////
// General Cuik stuff
////////////////////////////////////////////
void cuik_init(void);

// This should be called before exiting
void cuik_free_thread_resources(void);

// locates the system includes, libraries and other tools. this is a global
// operation meaning that once it's only done once for the process.
void cuik_find_system_deps(const char* cuik_crt_directory);

// can only be called after cuik_find_system_deps
size_t cuik_get_system_search_path_count(void);
void cuik_get_system_search_paths(const char** out, size_t n);

////////////////////////////////////////////
// Compilation unit management
////////////////////////////////////////////
#define FOR_EACH_TU(it, cu) for (TranslationUnit* it = (cu)->head; it; it = cuik_next_translation_unit(it))

// if the translation units are in a compilation unit you can walk this chain of pointers
// to read them
TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu);

void cuik_create_compilation_unit(CompilationUnit* restrict cu);
void cuik_lock_compilation_unit(CompilationUnit* restrict cu);
void cuik_unlock_compilation_unit(CompilationUnit* restrict cu);
void cuik_add_to_compilation_unit(CompilationUnit* restrict cu, TranslationUnit* restrict tu);
void cuik_destroy_compilation_unit(CompilationUnit* restrict cu);
void cuik_internal_link_compilation_unit(CompilationUnit* restrict cu);
size_t cuik_num_of_translation_units_in_compilation_unit(CompilationUnit* restrict cu);

#include "cuik_lex.h"
#include "cuik_ast.h"
#include "cuik_parse.h"

#ifdef CUIK_USE_TB
#include "cuik_irgen.h"
#endif

#include "cuik_link.h"
#include "cuik_private.h"
