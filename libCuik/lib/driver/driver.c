#include <cuik.h>
#include <common.h>
#include <futex.h>
#include <arena.h>
#include <threads.h>
#include "targets/targets.h"
#include "driver_fs.h"
#include "driver_opts.h"
#include "driver_arg_parse.h"
#include <file_map.h>
#include "../front/parser.h"

#ifdef CUIK_ALLOW_THREADS
#include <stdatomic.h>
#endif

enum {
    IRGEN_TASK_BATCH_SIZE = 8192,
    TB_TASK_BATCH_SIZE = 8192,
};

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];

        ResolvedSourceLoc r = cuikpp_find_location(s, t->location);
        if (last_file != r.file->filename) {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*) r.file->filename;
            char str[FILENAME_MAX], *out = str;

            while (*in) {
                if (*in == '\\') {
                    *out++ = '\\';
                    *out++ = '\\';
                    in++;
                } else {
                    *out++ = *in++;
                }
            }
            *out++ = '\0';

            fprintf(out_file, "\n#line %d \"%s\"\t", r.line, str);
            last_file = r.file->filename;
        }

        if (last_line != r.line) {
            fprintf(out_file, "\n/* line %3d */\t", r.line);
            last_line = r.line;
        }

        fprintf(out_file, "%.*s ", (int) t->content.length, t->content.data);
    }
}

void cuik_free_args(Cuik_CompilerArgs* args) {
    dyn_array_for(i, args->sources) cuik_free(args->sources[i]);
    dyn_array_for(i, args->includes) cuik_free(args->includes[i]);
    dyn_array_for(i, args->libraries) cuik_free(args->libraries[i]);
    dyn_array_for(i, args->defines) cuik_free(args->defines[i]);

    dyn_array_destroy(args->sources);
    dyn_array_destroy(args->includes);
    dyn_array_destroy(args->libraries);
    dyn_array_destroy(args->defines);
}

Cuik_CPP* cuik_driver_preprocess(const char* filepath, const Cuik_CompilerArgs* args, bool should_finalize) {
    Cuik_CPP* cpp = NULL;
    CUIK_TIMED_BLOCK("cuikpp_init") {
        cpp = cuikpp_make(filepath, args->diag_callback, args->diag_userdata);
    }

    cuik_set_standard_defines(cpp, args);
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
        cuikdg_dump_to_file(cuikpp_get_token_stream(cpp), stderr);
        cuikpp_free(cpp);
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
    Cuik_CPP** preprocessors;
} SharedDriverState;

typedef struct {
    SharedDriverState* stuff;

    size_t input_i;
    const char* input;
    Cuik_CPP* cpp;

    _Atomic int* errors;
    Futex* remaining;
} CompilerJob;

static void dump_errors(TokenStream* tokens) {
    cuikdg_dump_to_file(tokens, stderr);

    int r = cuikdg_error_count(tokens);
    if (r) fprintf(stderr, "failed with %d errors...\n", r);
}

static void compile_file(void* arg);
static void preproc_file(void* arg) {
    CompilerJob* restrict job = arg;
    Cuik_CompilerArgs* restrict args = job->stuff->args;

    Cuik_CPP* cpp = cuik_driver_preprocess(job->input, args, true);
    job->stuff->preprocessors[job->input_i] = cpp;
    if (cpp == NULL) {
        *job->errors += 1;
        return;
    }

    // dispose the preprocessor crap since we didn't need it
    if (args->test_preproc) {
        futex_dec(job->remaining);
        return;
    }

    job->cpp = cpp;
    if (job->stuff->thread_pool != NULL) {
        CUIK_CALL(job->stuff->thread_pool, submit, compile_file, sizeof(CompilerJob), job);
    } else {
        compile_file(job);
    }
}

static void compile_file(void* arg) {
    CompilerJob* job = arg;
    CompilationUnit* cu = job->stuff->cu;
    Cuik_CompilerArgs* args = job->stuff->args;

    Cuik_ParseResult result;
    TokenStream* tokens = cuikpp_get_token_stream(job->cpp);
    CUIK_TIMED_BLOCK_ARGS("parse", cuikpp_get_main_file(tokens)) {
        result = cuikparse_run(args->version, tokens, args->target, false);

        if (result.error_count > 0) {
            futex_dec(job->remaining);
            *job->errors += 1;
            return;
        }
    }

    // #pragma comment(lib, "foo.lib")
    Cuik_ImportRequest* imports = result.imports;
    if (imports != NULL) {
        cuik_lock_compilation_unit(cu);
        for (; imports != NULL; imports = imports->next) {
            dyn_array_put(job->stuff->args->libraries, cuik_strdup(imports->lib_name));
        }
        cuik_unlock_compilation_unit(cu);
    }

    TranslationUnit* tu = result.tu;
    if (cuiksema_run(tu, NULL) > 0) {
        futex_dec(job->remaining);
        *job->errors += 1;
        return;
    }

    cuik_set_translation_unit_user_data(tu, job->cpp);
    cuik_add_to_compilation_unit(cu, result.tu);
    futex_dec(job->remaining);
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

    CUIK_TIMED_BLOCK("IRGen") {
        size_t i = 0;
        while (i < task.count) {
            // skip all the typedefs
            if (task.stmts[i]->decl.attrs.is_typedef || !task.stmts[i]->decl.attrs.is_used) {
                i += 1;
                continue;
            }

            const char* name = task.stmts[i]->decl.name;
            if (name == NULL) {
                // these are untracked in the gen ir because they don't map to named IR stuff
                cuikcg_top_level(task.tu, mod, task.stmts[i]);
            } else {
                CUIK_TIMED_BLOCK_ARGS("FunctionIR", name) {
                    cuikcg_top_level(task.tu, mod, task.stmts[i]);
                }
            }

            /*TB_Function* func = tb_symbol_as_function(sym);
            if (func != NULL && task.opt_level == 0) {
                CUIK_TIMED_BLOCK_ARGS("Canonicalize", name) {
                    for (size_t j = 0; j < PASS_COUNT; j++) {
                        CUIK_TIMED_BLOCK_ARGS(passes[j].name, name) {
                            passes[j].func_run(func);
                        }
                    }

                    #ifndef NDEBUG
                    int error_count = tb_function_validate(func);
                    if (error_count > 0) {
                        fprintf(stderr, "TB validator failed with %d error%s!\n", error_count, error_count ? "s" : "");
                        abort();
                    }
                    #endif
                }
            }*/
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
    Futex* remaining;
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

    futex_dec(task.remaining);
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

            IRGenTask* tasks = cuik_malloc(task_capacity * sizeof(IRGenTask));
            atomic_size_t tasks_remaining = task_capacity;

            size_t task_count = 0;
            FOR_EACH_TU(tu, cu) {
                size_t top_level_count = cuik_num_of_top_level_stmts(tu);
                Stmt** top_level = cuik_get_top_level_stmts(tu);
                for (size_t i = 0; i < top_level_count; i += 8192) {
                    size_t end = i + 8192;
                    if (end >= top_level_count) end = top_level_count;

                    assert(task_count < task_capacity);
                    IRGenTask task = {
                        .mod = mod,
                        .tu = tu,
                        .opt_level = args->opt_level,
                        .stmts = &top_level[i],
                        .count = end - i,
                        .remaining = &tasks_remaining
                    };

                    CUIK_CALL(thread_pool, submit, irgen_job, sizeof(task), &task);
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
        size_t capacity = (tb_module_get_function_count(mod) + TB_TASK_BATCH_SIZE - 1) / TB_TASK_BATCH_SIZE;
        Futex remaining = capacity;

        size_t i = 0;
        TB_FOR_FUNCTIONS(f, mod) {
            if ((i % TB_TASK_BATCH_SIZE) == 0) {
                CodegenTask task = { .mod = mod, .start = f, .remaining = &remaining };
                CUIK_CALL(thread_pool, submit, codegen_job, sizeof(task), &task);
            }

            i += 1;
        }

        futex_wait_eq(&remaining, 0);
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

static Cuik_Linker gimme_linker(Cuik_CompilerArgs* restrict args, bool subsystem_windows) {
    Cuik_Linker l = { .subsystem_windows = subsystem_windows };

    // Add system libpaths
    cuiklink_apply_toolchain_libs(&l, args);

    // Add input libraries
    dyn_array_for(i, args->libraries) {
        cuiklink_add_input_file(&l, args->libraries[i]);
    }

    if (!args->nocrt) {
        #ifdef _WIN32
        cuiklink_add_input_file(&l, "kernel32.lib");
        cuiklink_add_input_file(&l, "ucrt.lib");
        cuiklink_add_input_file(&l, "msvcrt.lib");
        cuiklink_add_input_file(&l, "vcruntime.lib");
        #endif
    }

    return l;
}

static bool export_output(Cuik_CompilerArgs* restrict args, TB_Module* mod, const char* output_path, bool subsystem_windows) {
    // TODO(NeGate): do a smarter system (just default to whatever the different platforms like)
    TB_DebugFormat debug_fmt = (args->debug_info ? TB_DEBUGFMT_CODEVIEW : TB_DEBUGFMT_NONE);

    Cuik_System sys = cuik_get_target_system(args->target);
    bool is_windows = (sys == CUIK_SYSTEM_WINDOWS);

    char output_name[FILENAME_MAX];
    sprintf_s(
        output_name, FILENAME_MAX, "%s%s", output_path,
        cuik_get_target_system(args->target) == CUIK_SYSTEM_WINDOWS ? ".exe" : ""
    );

    if (args->based && (args->flavor == TB_FLAVOR_SHARED || args->flavor == TB_FLAVOR_EXECUTABLE)) {
        fprintf(stderr, "LINK %s\n", output_name);

        CUIK_TIMED_BLOCK("Export linked") {
            // TB_ExecutableType exe = tb_system_executable_format((TB_System) sys);
            TB_Linker* l = NULL;
            switch (sys) {
                case CUIK_SYSTEM_WINDOWS:
                l = tb_linker_create(TB_EXECUTABLE_PE, args->target->arch);
                break;

                case CUIK_SYSTEM_LINUX:
                l = tb_linker_create(TB_EXECUTABLE_ELF, args->target->arch);
                break;

                default:
                fprintf(stderr, "unsupported platform to link with... sorry\n");
                return false;
            }
            int errors = 0;

            // locate libraries and feed them into TB... in theory this process
            // can be somewhat multithreaded so we might wanna consider that.
            Cuik_Linker tmp_linker = gimme_linker(args, subsystem_windows);
            char path[FILENAME_MAX];
            dyn_array_for(i, tmp_linker.inputs) {
                CUIK_TIMED_BLOCK(tmp_linker.inputs[i]) {
                    CUIK_TIMED_BLOCK("cuiklink_find_library") {
                        if (!cuiklink_find_library(&tmp_linker, path, tmp_linker.inputs[i])) {
                            fprintf(stderr, "could not find library: %s\n", tmp_linker.inputs[i]);
                            errors++;
                            goto skip;
                        }
                    }

                    TB_Slice s;
                    CUIK_TIMED_BLOCK("open_file_map") {
                        FileMap fm = open_file_map(path);
                        s = (TB_Slice){ fm.size, fm.data };
                    }

                    CUIK_TIMED_BLOCK("tb_linker_append_library") {
                        TB_Slice path_slice = { strlen(path), (const uint8_t*) cuik_strdup(path) };
                        tb_linker_append_library(l, path_slice, s);
                    }
                }
                skip:;
            }

            if (errors) {
                fprintf(stderr, "library search paths:\n");
                dyn_array_for(i, tmp_linker.libpaths) {
                    fprintf(stderr, "  %s\n", tmp_linker.libpaths[i]);
                }
                return false;
            }

            CUIK_TIMED_BLOCK("tb_linker_append_module") {
                tb_linker_append_module(l, mod);
            }

            TB_Exports exports = tb_linker_export(l);
            if (exports.count == 0) {
                fprintf(stderr, "\x1b[31merror\x1b[0m: could not link executable\n");
                return false;
            }

            FILE* file = NULL;
            CUIK_TIMED_BLOCK("fopen") {
                file = fopen(output_name, "wb");
                if (file == NULL) {
                    fprintf(stderr, "could not open file for writing! %s", output_name);
                    // tb_platform_heap_free(exports.files[0].data);
                    return false;
                }
            }

            CUIK_TIMED_BLOCK("fwrite") {
                fwrite(exports.files[0].data, 1, exports.files[0].length, file);
            }

            fclose(file);
            tb_exporter_free(exports);
            tb_linker_destroy(l);
        }

        return true;
    } else {
        char obj_output_path[FILENAME_MAX];
        sprintf_s(
            obj_output_path, FILENAME_MAX, "%s%s", output_path,
            cuik_get_target_system(args->target) == CUIK_SYSTEM_WINDOWS ? ".obj" : ".o"
        );

        TB_Exports exports = tb_module_object_export(mod, debug_fmt);
        if (exports.count == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not link executable\n");
            return false;
        }

        FILE* file = NULL;
        CUIK_TIMED_BLOCK("fopen") {
            file = fopen(obj_output_path, "wb");
            if (file == NULL) {
                fprintf(stderr, "\x1b[31merror\x1b[0m: could not open file for writing! %s\n", output_name);
                return false;
            }
        }

        CUIK_TIMED_BLOCK("fwrite") {
            fwrite(exports.files[0].data, 1, exports.files[0].length, file);
        }

        fclose(file);
        tb_exporter_free(exports);

        if (args->flavor == TB_FLAVOR_OBJECT) {
            return true;
        }

        // This uses the system linker which isn't fun
        fprintf(stderr, "LINK %s\n", output_path);
        CUIK_TIMED_BLOCK("linker") {
            Cuik_Linker l = gimme_linker(args, subsystem_windows);

            // Add Cuik object
            cuiklink_add_input_file(&l, obj_output_path);

            cuiklink_invoke(&l, args, output_name, output_path);
            cuiklink_deinit(&l);
        }

        return true;
    }
}

CompilationUnit* cuik_driver_compile(Cuik_IThreadpool* restrict thread_pool, Cuik_CompilerArgs* restrict args, bool destroy_cu_after_ir) {
    _Atomic int parse_errors = 0;

    CompilationUnit* cu = cuik_create_compilation_unit();
    SharedDriverState stuff = { cu, thread_pool, args };
    size_t tu_count = dyn_array_length(args->sources);
    CUIK_TIMED_BLOCK("Frontend") {
        Futex remaining = dyn_array_length(args->sources);
        stuff.preprocessors = cuik_malloc(dyn_array_length(args->sources) * sizeof(Cuik_CPP*));

        dyn_array_for(i, args->sources) {
            CompilerJob job = {
                .stuff = &stuff,

                .input_i = i,
                .input = args->sources[i],
                .errors = &parse_errors,
                .remaining = &remaining,
            };

            if (thread_pool) {
                CUIK_CALL(thread_pool, submit, preproc_file, sizeof(CompilerJob), &job);
            } else {
                preproc_file(&job);
            }
        }

        if (thread_pool) futex_wait_eq(&remaining, 0);

        for (size_t i = 0; i < tu_count; i++) {
            fprintf(stderr, "[%zu/%zu] CC %s\n", i+1, tu_count, args->sources[i]);

            if (stuff.preprocessors[i]) {
                TokenStream* tokens = cuikpp_get_token_stream(stuff.preprocessors[i]);
                if (tokens) {
                    dump_errors(tokens);

                    if (args->preprocess) {
                        dump_tokens(stdout, tokens);
                    }
                }
            }
        }
    }

    if (parse_errors > 0) goto error;
    if (args->preprocess || args->test_preproc) return cu;

    CUIK_TIMED_BLOCK("internal link") {
        cuik_internal_link_compilation_unit(cu, thread_pool, args->debug_info);
    }

    TB_Module* mod = NULL;
    CUIK_TIMED_BLOCK("allocate IR") {
        TB_FeatureSet features = { 0 };
        mod = tb_module_create(
            args->target->arch, (TB_System) cuik_get_target_system(args->target), &features, false
        );

        cuikcg_allocate_ir(cu, thread_pool, mod);
    }

    if (args->syntax_only) return cu;
    if (args->types) return cu;

    if (args->ast) {
        FOR_EACH_TU(tu, cu) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
        return cu;
    }

    ////////////////////////////////
    // backend work
    ////////////////////////////////
    bool subsystem_windows;
    CUIK_TIMED_BLOCK("Backend") {
        irgen(thread_pool, args, cu, mod, &subsystem_windows);

        for (size_t i = 0; i < tu_count; i++) {
            if (stuff.preprocessors[i]) {
                TokenStream* tokens = cuikpp_get_token_stream(stuff.preprocessors[i]);
                if (tokens) {
                    cuiklex_free_tokens(tokens);
                    cuikpp_free(stuff.preprocessors[i]);
                }
            }
        }
        cuik_free(stuff.preprocessors);

        if (destroy_cu_after_ir) {
            cuik_destroy_compilation_unit(cu);
        }

        if (args->opt_level >= 1) {
            // TODO: we probably want to do the fancy threading soon
            CUIK_TIMED_BLOCK("Optimizer") {
                tb_module_optimize(mod, COUNTOF(passes_O1), passes_O1);
            }
        }

        if (args->ir) {
            TB_FOR_FUNCTIONS(f, mod) {
                tb_function_print(f, tb_default_print_callback, stdout, false);
                printf("\n\n");
            }

            goto cleanup_tb;
        }

        CUIK_TIMED_BLOCK("CodeGen") {
            codegen(thread_pool, args, cu, mod);
        }
    }

    char output_path[FILENAME_MAX];
    cuik_driver_get_output_name(args, FILENAME_MAX, output_path);

    if (!export_output(args, mod, output_path, subsystem_windows)) {
        goto error;
    }

    ////////////////////////////////
    // Running executable
    ////////////////////////////////
    if (args->run) {
        #ifdef _WIN32
        for (char* s = output_path; *s; s++) {
            if (*s == '/') *s = '\\';
        }
        #endif

        printf("\n\nRunning: %s...\n", output_path);
        int exit_code = system(output_path);

        printf("Exit code: %d\n", exit_code);
        if (exit_code) goto error;
    }

    cleanup_tb:
    tb_free_thread_resources();
    tb_module_destroy(mod);
    return cu;

    error:
    cuik_destroy_compilation_unit(cu);
    return NULL;
}

bool cuik_driver_get_output_name(Cuik_CompilerArgs* args, int cap, char path[]) {
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

CUIK_API void cuik_toolchain_free(Cuik_Toolchain* toolchain) {
    cuik_free(toolchain->ctx);
}

CUIK_API Cuik_Toolchain cuik_toolchain_host(void) {
    #ifdef _MSC_VER
    return cuik_toolchain_msvc();
    #elif __APPLE__
    return cuik_toolchain_darwin();
    #else
    return (Cuik_Toolchain){ 0 };
    #endif
}
