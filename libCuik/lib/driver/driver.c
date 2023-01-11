#include <cuik.h>
#include <threads.h>
#include "driver_fs.h"
#include "driver_arg_parse.h"

enum {
    IRGEN_TASK_BATCH_SIZE = 8192,
    TB_TASK_BATCH_SIZE = 8192,
};

#ifdef _WIN32
#define NULL_FILEPATH "NUL"
#else
#define NULL_FILEPATH "/dev/null"
#endif

Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_CompilerArgs* args, bool should_finalize) {
    Cuik_CPP* cpp = NULL;
    CUIK_TIMED_BLOCK("cuikpp_init") {
        cpp = cuikpp_make(filepath);
    }

    cuikpp_set_common_defines(cpp, args->target, !args->nocrt);
    dyn_array_for(i, args->includes) {
        cuikpp_add_include_directory(cpp, false, args->includes[i]);
    }

    dyn_array_for(i, args->defines) {
        const char* equal = strchr(args->defines[i], '=');

        if (equal == NULL) {
            cuikpp_define_empty_cstr(cpp, args->defines[i]);
        } else {
            cuikpp_define(
                cpp,
                // before equals
                equal - args->defines[i], args->defines[i],
                // after equals
                strlen(equal + 1), equal + 1
            );
        }
    }

    // run the preprocessor
    if (cuikpp_default_run(cpp) == CUIKPP_ERROR) {
        return NULL;
    }

    if (should_finalize) {
        cuikpp_finalize(cpp);
    }
    return cpp;
}

typedef struct {
    CompilationUnit* cu;
    Cuik_IThreadpool* thread_pool;
    Cuik_CompilerArgs* args;

    _Atomic int* files_with_errors;
    _Atomic int* complete;
    const char* input;

    Cuik_CPP* cpp;
} CompilerJob;

static void compile_file(void* arg);
static void preproc_file(void* arg) {
    CompilerJob* restrict job = arg;
    Cuik_CPP* cpp = cuik_driver_preprocess(job->input, job->args, true);
    if (cpp == NULL) {
        (*job->files_with_errors)++;
        return;
    }

    // dispose the preprocessor crap since we didn't need it
    if (job->args->test_preproc) {
        cuikpp_free(cpp);
        (*job->complete)++;
        return;
    }

    if (job->thread_pool != NULL) {
        CUIK_CALL(job->thread_pool, submit, compile_file, job);
    } else {
        compile_file(job);
    }
}

static void compile_file(void* arg) {
    CompilerJob* job = arg;

    Cuik_ParseResult result;
    TokenStream* tokens = cuikpp_get_token_stream(job->cpp);
    CUIK_TIMED_BLOCK_ARGS("parse", cuikpp_get_main_file(tokens)) {
        result = cuikparse_run(job->args->version, tokens, job->args->target, false);
        if (result.error_count > 0) {
            printf("Failed to parse with %d errors...\n", result.error_count);
            (*job->files_with_errors)++;
            (*job->complete)++;
            return;
        }
    }

    // #pragma comment(lib, "foo.lib")
    Cuik_ImportRequest* imports = result.imports;
    if (imports != NULL) {
        cuik_lock_compilation_unit(job->cu);
        for (; imports != NULL; imports = imports->next) {
            dyn_array_put(job->args->libraries, imports->lib_name);
        }
        cuik_unlock_compilation_unit(job->cu);
    }

    TranslationUnit* tu = result.tu;
    int r = cuiksema_run(tu, NULL);
    if (r > 0) {
        printf("Failed to type check with %d errors...\n", r);
        (*job->files_with_errors)++;
        (*job->complete)++;
        return;
    }

    cuik_set_translation_unit_user_data(tu, job->cpp);
    cuik_add_to_compilation_unit(job->cu, tu);
    (*job->complete)++;
}

typedef struct {
    TB_Module* mod;
    TranslationUnit* tu;
    int opt_level;

    Stmt** stmts;
    size_t count;

    #if CUIK_ALLOW_THREADS
    atomic_size_t* remaining;
    #endif
} IRGenTask;

static void irgen_job(void* arg) {
    IRGenTask task = *((IRGenTask*) arg);
    TB_Module* mod = task.mod;

    // simple function level passes
    TB_Pass passes[] = {
        tb_opt_instcombine(),
        tb_opt_remove_pass_nodes(),
        tb_opt_dead_expr_elim(),
        tb_opt_compact_dead_regs()
    };
    enum { PASS_COUNT = sizeof(passes) / sizeof(passes[0]) };

    CUIK_TIMED_BLOCK("IRGen") {
        size_t i = 0;
        while (i < task.count) {
            // skip all the typedefs
            if (task.stmts[i]->decl.attrs.is_typedef || !task.stmts[i]->decl.attrs.is_used) {
                i += 1;
                continue;
            }

            TB_Symbol* sym = NULL;
            const char* name = task.stmts[i]->decl.name;
            if (name == NULL) {
                // these are untracked in the gen ir because they don't map to named IR stuff
                sym = cuikcg_top_level(task.tu, mod, task.stmts[i]);
            } else {
                CUIK_TIMED_BLOCK_ARGS("FunctionIR", name) {
                    sym = cuikcg_top_level(task.tu, mod, task.stmts[i]);
                }
            }

            TB_Function* func = tb_symbol_as_function(sym);
            if (func != NULL && task.opt_level == 0) {
                CUIK_TIMED_BLOCK_ARGS("Canonicalize", name) {
                    for (size_t j = 0; j < PASS_COUNT; j++) {
                        CUIK_TIMED_BLOCK_ARGS(passes[j].name, name) {
                            passes[j].func_run(func);
                        }
                    }

                    /*#ifndef NDEBUG
                    int error_count = tb_function_validate(func);
                    if (error_count > 0) {
                        fprintf(stderr, "TB validator failed with %d error%s!\n", error_count, error_count ? "s" : "");
                        abort();
                    }
                    #endif*/
                }
            }
            i += 1;
        }
    }

    #if CUIK_ALLOW_THREADS
    if (task.remaining != NULL) *task.remaining -= 1;
    #endif
}

typedef struct {
    TB_Module* mod;
    TB_Function* start;

    #if CUIK_ALLOW_THREADS
    atomic_size_t* remaining;
    #endif
} CodegenTask;

static void codegen_job(void* arg) {
    CodegenTask task = *((CodegenTask*) arg);

    CUIK_TIMED_BLOCK("Codegen") {
        TB_Function* f = task.start;

        for (size_t i = 0; i < TB_TASK_BATCH_SIZE && f != NULL; i++) {
            tb_module_compile_function(task.mod, f, TB_ISEL_FAST);
            f = tb_next_function(f);
        }
    }

    #if CUIK_ALLOW_THREADS
    if (task.remaining != NULL) *task.remaining -= 1;
    #endif
}

static void irgen(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, CompilationUnit* restrict cu, TB_Module* mod, bool* subsystem_windows) {
    *subsystem_windows = false;

    CUIK_TIMED_BLOCK("IRGen") {
        if (thread_pool != NULL) {
            #if CUIK_ALLOW_THREADS
            size_t task_capacity = 0;
            FOR_EACH_TU(tu, cu) {
                if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                    *subsystem_windows = true;
                }

                size_t c = cuik_num_of_top_level_stmts(tu);
                task_capacity += (c + (IRGEN_TASK_BATCH_SIZE - 1)) / IRGEN_TASK_BATCH_SIZE;
            }

            IRGenTask* tasks = malloc(task_capacity * sizeof(IRGenTask));
            atomic_size_t tasks_remaining = task_capacity;

            size_t task_count = 0;
            FOR_EACH_TU(tu, cu) {
                // dispose the preprocessor crap now
                cuikpp_free((Cuik_CPP*) cuik_set_translation_unit_user_data(tu, NULL));

                size_t top_level_count = cuik_num_of_top_level_stmts(tu);
                Stmt** top_level = cuik_get_top_level_stmts(tu);
                for (size_t i = 0; i < top_level_count; i += 8192) {
                    size_t end = i + 8192;
                    if (end >= top_level_count) end = top_level_count;

                    assert(task_count < task_capacity);
                    IRGenTask* task = &tasks[task_count++];
                    *task = (IRGenTask){
                        .mod = mod,
                        .tu = tu,
                        .opt_level = args->opt_level,
                        .stmts = &top_level[i],
                        .count = end - i,
                        .remaining = &tasks_remaining
                    };

                    CUIK_CALL(thread_pool, submit, irgen_job, task);
                }
            }

            // "highway robbery on steve jobs" job stealing amirite...
            while (atomic_load(&tasks_remaining) != 0) {
                CUIK_CALL(thread_pool, work_one_job);
            }
            #else
            fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
            abort();
            #endif
            // free(tasks);
        } else {
            FOR_EACH_TU(tu, cu) {
                if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                    *subsystem_windows = true;
                }

                Cuik_CPP* cpp = cuik_get_translation_unit_user_data(tu);
                cuikpp_free(cpp);

                size_t c = cuik_num_of_top_level_stmts(tu);
                IRGenTask task = {
                    .mod = mod,
                    .tu = tu,
                    .stmts = cuik_get_top_level_stmts(tu),
                    .count = c
                };

                irgen_job(&task);
            }
        }
    }
}

static void codegen(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, CompilationUnit* restrict cu, TB_Module* mod) {
    if (thread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        size_t count = 0, capacity = (tb_module_get_function_count(mod) + TB_TASK_BATCH_SIZE - 1) / TB_TASK_BATCH_SIZE;
        atomic_size_t tasks_remaining = capacity;

        CodegenTask* tasks = malloc(capacity * sizeof(CodegenTask));
        size_t i = 0;
        TB_FOR_FUNCTIONS(f, mod) {
            if ((i % TB_TASK_BATCH_SIZE) == 0) {
                assert(count < capacity);

                tasks[count] = (CodegenTask){ .mod = mod, .start = f, .remaining = &tasks_remaining };
                CUIK_CALL(thread_pool, submit, codegen_job, &tasks[count]);
                count += 1;
            }

            i += 1;
        }

        // "highway robbery on steve jobs" job stealing amirite...
        while (atomic_load(&tasks_remaining) != 0) {
            CUIK_CALL(thread_pool, work_one_job);
        }

        free(tasks);
        #else
        fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
        abort();
        #endif /* CUIK_ALLOW_THREADS */
    } else {
        TB_FOR_FUNCTIONS(f, mod) {
            tb_module_compile_function(mod, f, TB_ISEL_FAST);
        }
    }

    if (args->verbose) printf("  IRGen: %zu functions compiled\n", tb_module_get_function_count(mod));
}

static bool export_output(Cuik_CompilerArgs* restrict args, TB_Module* mod, bool subsystem_windows) {
    // TODO(NeGate): do a smarter system (just default to whatever the different platforms like)
    TB_DebugFormat debug_fmt = (args->debug_info ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_NONE);

    bool output_path_null = false;
    const char* output_name = args->output_name;
    char output_path_no_ext[FILENAME_MAX];
    if (output_name != NULL && strcmp(output_name, "$nul") == 0) {
        output_path_null = true;
    } else {
        cuik_driver_get_output_path(args, FILENAME_MAX, output_path_no_ext);

        if (output_name == NULL) {
            #if _WIN32
            char* str = malloc(FILENAME_MAX);
            sprintf_s(str, FILENAME_MAX, "%s.exe", output_path_no_ext);
            output_name = str;
            #else
            output_name = output_path_no_ext;
            #endif
        }
    }

    if (args->based) {
        bool is_windows = (cuik_get_target_system(args->target) == CUIK_SYSTEM_WINDOWS);
        const char* extension = NULL;
        switch (args->flavor) {
            case TB_FLAVOR_OBJECT:     extension = (is_windows?".obj":".o");  break;
            case TB_FLAVOR_STATIC:     extension = (is_windows?".lib":".a");  break;
            case TB_FLAVOR_SHARED:     extension = (is_windows?".dll":".so"); break;
            case TB_FLAVOR_EXECUTABLE: extension = (is_windows?".exe":"");    break;
            default: assert(0);
        }

        char path[FILENAME_MAX];
        if (output_path_null) {
            strcpy(path, NULL_FILEPATH);
        } else {
            sprintf_s(path, FILENAME_MAX, "%s%s", output_path_no_ext, extension);
        }

        CUIK_TIMED_BLOCK("Export") {
            if (!tb_exporter_write_files(mod, args->flavor, debug_fmt, 1, &(const char*){ path })) {
                fprintf(stderr, "error: could not write output. %s\n", path);
                return false;
            }
        }

        return true;
    } else {
        char obj_output_path[FILENAME_MAX];
        if (output_path_null) {
            strcpy(obj_output_path, NULL_FILEPATH);
        } else {
            sprintf_s(
                obj_output_path, FILENAME_MAX, "%s%s", output_path_no_ext,
                cuik_get_target_system(args->target) == CUIK_SYSTEM_WINDOWS ? ".obj" : ".o"
            );
        }

        CUIK_TIMED_BLOCK("Export object") {
            if (!tb_exporter_write_files(mod, TB_FLAVOR_OBJECT, debug_fmt, 1, (const char*[]) { obj_output_path })) {
                remove(obj_output_path);
                fprintf(stderr, "error: could not write object file output. %s\n", obj_output_path);
                return false;
            }
        }

        if (args->flavor == TB_FLAVOR_ASSEMBLY) {
            char cmd[2048];
            snprintf(cmd, 2048, "dumpbin %s /disasm", obj_output_path);
            return system(cmd) == 0;
        }

        if (output_path_null || args->flavor == TB_FLAVOR_OBJECT) {
            return true;
        }

        CUIK_TIMED_BLOCK("linker") {
            Cuik_Linker l;
            if (cuiklink_init(&l)) {
                if (subsystem_windows) {
                    cuiklink_subsystem_windows(&l);
                }

                // Add system libpaths
                cuiklink_add_default_libpaths(&l);

                char lib_dir[FILENAME_MAX];
                sprintf_s(lib_dir, FILENAME_MAX, "%s/crt/lib/", args->crt_dirpath);
                cuiklink_add_libpath(&l, lib_dir);

                // Add Cuik output
                cuiklink_add_input_file(&l, obj_output_path);

                // Add input libraries
                dyn_array_for(i, args->libraries) {
                    cuiklink_add_input_file(&l, args->libraries[i]);
                }

                /*dyn_array_for(i, input_objects) {
                    cuiklink_add_input_file(&l, input_objects[i]);
                }*/

                if (!args->nocrt) {
                    #ifdef _WIN32
                    cuiklink_add_input_file(&l, "ucrt.lib");
                    cuiklink_add_input_file(&l, "msvcrt.lib");
                    cuiklink_add_input_file(&l, "vcruntime.lib");
                    cuiklink_add_input_file(&l, "win32_rt.lib");
                    #endif
                }

                cuiklink_invoke(&l, output_path_no_ext, "ucrt");
                cuiklink_deinit(&l);
            }
        }

        return true;
    }
}

int cuik_driver_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, bool destroy_cu_after_ir) {
    _Atomic int files_with_errors = 0;
    _Atomic int complete = 0;

    CompilationUnit compilation_unit = { 0 };
    cuik_create_compilation_unit(&compilation_unit);
    CUIK_TIMED_BLOCK("Frontend") {
        if (thread_pool == NULL) {
            size_t source_count = dyn_array_length(args->sources);
            CompilerJob* jobs = malloc(source_count * sizeof(CompilerJob));

            dyn_array_for(i, args->sources) {
                jobs[i] = (CompilerJob){
                    &compilation_unit, thread_pool, args, &files_with_errors, &complete, args->sources[i]
                };

                CUIK_CALL(thread_pool, submit, preproc_file, &jobs[i]);
            }

            while (complete == source_count) thrd_yield();
            free(jobs);
        } else {
            dyn_array_for(i, args->sources) {
                CompilerJob job = {
                    &compilation_unit, thread_pool, args, &files_with_errors, &complete, args->sources[i]
                };

                preproc_file(&job);
            }
        }
    }

    if (args->test_preproc) return 0;
    if (files_with_errors > 0) {
        fprintf(stderr, "%d files with %s!\n", files_with_errors, files_with_errors > 1 ? "errors" : "error");
        return 1;
    }

    TB_FeatureSet features = { 0 };
    TB_Module* mod = tb_module_create(
        TB_ARCH_X86_64, (TB_System) cuik_get_target_system(args->target), &features, false
    );

    CUIK_TIMED_BLOCK("internal link") {
        cuik_internal_link_compilation_unit(&compilation_unit, mod, args->debug_info);
    }

    if (args->syntax_only) return 0;
    if (args->types) return 0;

    if (args->ast) {
        FOR_EACH_TU(tu, &compilation_unit) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
        return 0;
    }

    ////////////////////////////////
    // backend work
    ////////////////////////////////
    CUIK_TIMED_BLOCK("Backend") {
        bool subsystem_windows;
        irgen(thread_pool, args, &compilation_unit, mod, &subsystem_windows);

        if (destroy_cu_after_ir) {
            cuik_destroy_compilation_unit(&compilation_unit);
        }

        /*if (dyn_array_length(da_passes) != 0) {
            // TODO: we probably want to do the fancy threading soon
            CUIK_TIMED_BLOCK("Optimizer") {
                tb_module_optimize(mod, dyn_array_length(da_passes), da_passes);
            }
        }*/

        if (args->ir) {
            TB_FOR_FUNCTIONS(f, mod) {
                tb_function_print(f, tb_default_print_callback, stdout, false);
                printf("\n\n");
            }

            goto cleanup_tb;
        }

        CUIK_TIMED_BLOCK("CodeGen") {
            codegen(thread_pool, args, &compilation_unit, mod);
        }

        if (!export_output(args, mod, subsystem_windows)) {
            return 1;
        }
    }

    cleanup_tb:
    tb_free_thread_resources();
    tb_module_destroy(mod);
    return 0;
}

bool cuik_driver_get_output_path(Cuik_CompilerArgs* args, int cap, char path[]) {
    assert(cap >= 1);
    if (args->output_name != NULL && strcmp(args->output_name, "$nul") == 0) {
        path[0] = 0;
        return true;
    }

    const char* filename = args->output_name ? args->output_name : args->sources[0];
    const char* ext = strrchr(filename, '.');
    size_t len = ext ? (ext - filename) : strlen(filename);

    if (filename[len - 1] == '/' || filename[len - 1] == '\\') {
        const char* slash = strrchr(args->sources[0], '/');
        if (!slash) slash = strrchr(args->sources[0], '\\');

        if (!slash) slash = args->sources[0];
        else slash += 1; // skip the slash

        size_t total = strlen(slash)+len+1;
        if (total > cap) {
            return false;
        }

        snprintf(path, total, "%.*s%s", (int)len, filename, slash);
        return true;
    } else {
        if (len >= cap) {
            return false;
        }

        memcpy(path, filename, len);
        path[len] = '\0';
        return true;
    }
}
