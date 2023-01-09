#include <cuik.h>
#include <threads.h>

static const char* arg_is_set = "set";

typedef enum ArgType {
    ARG_NONE = 0,

    #define X(name, short, long, has_args, msg) name,
    #include "driver_args.h"
} ArgType;

typedef struct {
    ArgType type;
    const char* alias; // -a
    const char* name;  // --name
    bool has_arg;
    const char* desc;
} ArgDesc;

typedef struct Arg {
    ArgType key;
    const char* value;
} Arg;

static const ArgDesc arg_descs[] = {
    #define X(name, short, long, has_args, msg) { name, short, long, has_args, msg },
    #include "driver_args.h"
};
enum { ARG_DESC_COUNT = sizeof(arg_descs) / sizeof(arg_descs[0]) };

static const ArgDesc* find_arg_desc(const char* arg, bool is_long_name) {
    if (is_long_name) {
        for (size_t i = 0; i < ARG_DESC_COUNT; i++) {
            const char* n = arg_descs[i].name;
            if (n && strncmp(arg, n, strlen(n)) == 0) {
                return &arg_descs[i];
            }
        }
    } else {
        for (size_t i = 0; i < ARG_DESC_COUNT; i++) {
            const char* n = arg_descs[i].alias;
            if (n && strncmp(arg, n, strlen(n)) == 0) {
                return &arg_descs[i];
            }
        }
    }

    return NULL;
}

static Arg read_arg(int* out_arg_length, int argc, const char* argv[]) {
    // identify argument kind
    const char* first = argv[0];

    if (first[0] == '-') {
        bool is_long_name = (first[1] == '-');

        // check for equals
        const char* equals = strchr(first, '=');
        const ArgDesc* desc = find_arg_desc(&first[is_long_name ? 2 : 1], is_long_name);
        if (desc == NULL) {
            // could not find an option
            fprintf(stderr, "error: could not find match for %s\n", first);
            *out_arg_length = 1;
            return (Arg){ 0 };
        }

        if (desc->has_arg) {
            /*if (argv[i][long_name_len] == 0) {
                if ((i + 1) >= argc) {
                    fprintf(stderr, "error: no argument after -%s\n", long_name);
                    exit_or_hook(1);
                }

                *index += 1;
                return (Arg){ arg_descs[j].type, argv[i + 1] };
            } else {
                if (argv[i][long_name_len + 3] == 0) {
                    fprintf(stderr, "error: argument for --%s is empty\n", long_name);
                    exit_or_hook(1);
                }

                return (Arg){ arg_descs[j].type, argv[i] + long_name_len + 3 };
            }*/
        } else {
        }

        *out_arg_length = 1;
        return (Arg){ desc->type, arg_is_set };
    } else {
        *out_arg_length = 1;
        return (Arg){ ARG_NONE, first };
    }
}

int cuik_parse_arg(Cuik_CompilerArgs* args, int argc, const char* argv[]) {
    int i;
    Arg arg = read_arg(&i, argc, argv);
    switch (arg.key) {
        default: break;
    }

    return i;
}

void cuik_parse_args(Cuik_CompilerArgs* args, int argc, const char* argv[]) {
    for (int i = 0; i < argc;) {
        i += cuik_parse_arg(args, argc - i, argv + i);
    }
}

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

int cuik_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, bool destroy_cu_after_ir) {
    CompilationUnit compilation_unit = { 0 };
    _Atomic int files_with_errors = 0;
    _Atomic int complete = 0;

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
        TB_ARCH_X86_64, (TB_System) cuik_get_target_system(target_desc), &features, false
    );

    TIMESTAMP("Internal link");
    CUIK_TIMED_BLOCK("internal link") {
        cuik_internal_link_compilation_unit(&compilation_unit, mod, args_debug_info);
    }

    if (args_syntax_only) return 0;
    if (args_types) return 0;

    if (args_ast) {
        FOR_EACH_TU(tu, &compilation_unit) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
        return 0;
    }

    ////////////////////////////////
    // backend work
    ////////////////////////////////
    CUIK_TIMED_BLOCK("Backend") {
        irgen();
        if (destroy_cu_after_ir) {
            cuik_destroy_compilation_unit(&compilation_unit);
        }

        if (dyn_array_length(da_passes) != 0) {
            // TODO: we probably want to do the fancy threading soon
            TIMESTAMP("Optimizer");
            CUIK_TIMED_BLOCK("Optimizer") {
                tb_module_optimize(mod, dyn_array_length(da_passes), da_passes);
            }
        }

        if (args_ir) {
            TIMESTAMP("IR Printer");
            TB_FOR_FUNCTIONS(f, mod) {
                tb_function_print(f, tb_default_print_callback, stdout, false);
                printf("\n\n");
            }

            goto cleanup_tb;
        }

        TIMESTAMP("Codegen");
        CUIK_TIMED_BLOCK("CodeGen") {
            codegen();
        }

        if (!export_output()) {
            return 1;
        }
    }

    cleanup_tb:
    tb_free_thread_resources();
    tb_module_destroy(mod);
    return 0;
}
