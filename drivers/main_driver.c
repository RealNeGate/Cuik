#include <cuik.h>
#include "helper.h"
#include "cli_parser.h"
#include "json_perf.h"
#include "threadpool.h"

// compiler arguments
static DynArray(const char*) include_directories;
static DynArray(const char*) input_libraries;
static DynArray(const char*) input_files;
static const char* output_name;
static char output_path_no_ext[FILENAME_MAX];

static bool args_ir;
static bool args_ast;
static bool args_types;
static bool args_run;
static bool args_assembly;
static bool args_time;
static bool args_verbose;
static bool args_preprocess;
static bool args_optimize;
static bool args_object_only;

static TB_Module* mod;
static threadpool_t* thread_pool;
static Cuik_IThreadpool ithread_pool;
static const Cuik_TargetDesc* target_desc;
static CompilationUnit compilation_unit;

static int calculate_worker_thread_count(void) {
    #ifdef _WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);

    // Just kinda guesses something that seems ok ish for now
    // eventually we'll wanna use all cores but it's not honestly
    // helpful currently since code gen is the only parallel stage.
    int r = sysinfo.dwNumberOfProcessors - 4;
    return (r <= 0) ? 1 : r;
    #else
    return 1;
    #endif
}

static void tp_submit(void* user_data, void fn(void*), void* arg) {
    threadpool_t* tp = user_data;
    threadpool_submit(tp, fn, arg);
}

static void tp_work_one_job(void* user_data) {
    threadpool_t* tp = user_data;
    threadpool_work_one_job(tp);
}

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuik_get_tokens(s);
    size_t count = cuik_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];
        SourceLoc* loc = &s->locations[SOURCE_LOC_GET_DATA(t->location)];

        if (last_file != loc->line->filepath && strcmp(loc->line->filepath, "<temp>") != 0) {
            char str[FILENAME_MAX];

            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*)loc->line->filepath;
            char* out = str;

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

            fprintf(out_file, "\n#line %d \"%s\"\t", loc->line->line, str);
            last_file = loc->line->filepath;
        }

        if (last_line != loc->line->line) {
            fprintf(out_file, "\n/* line %3d */\t", loc->line->line);
            last_line = loc->line->line;
        }

        fprintf(out_file, "%.*s ", (int)(t->end - t->start), t->start);
    }
}

static void irgen_visitor(TranslationUnit* restrict tu, Stmt* restrict s, void* user_data) {
    TB_Module* mod = cuik_get_tb_module(tu);
    TB_Function* func = cuik_stmt_gen_ir(tu, s);

    if (func != NULL) {
        if (args_optimize) {
            tb_function_optimize(func);
        }

        if (args_ir) {
            tb_function_print(func, tb_default_print_callback, stdout);
            printf("\n\n");
        } else {
            tb_module_compile_func(mod, func, args_optimize ? TB_ISEL_COMPLEX : TB_ISEL_FAST);
        }

        tb_function_free(func);
    }
}

static void compile_file(void* arg) {
    const char* input = (const char*)arg;

    // preproc
    Cuik_CPP cpp;
    TokenStream tokens = cuik_preprocess_simple(
        &cpp, input, &cuik_default_fs, target_desc, true,
        dyn_array_length(include_directories),
        &include_directories[0]
    );

    cuikpp_finalize(&cpp);

    // parse
    TranslationUnit* tu = cuik_parse_translation_unit(mod, &tokens, target_desc, &ithread_pool);
    cuik_add_to_compilation_unit(&compilation_unit, tu);
}

// we can do a bit of filter such as '*.c' where it'll take all
// paths in the folder that end with .c
static void append_input_path(const char* path) {
    // avoid using the filters if we dont need to :p
    bool needs_filter = false;
    for (const char* p = path; *p; p++) {
        if (*p == '*') {
            needs_filter = true;
            break;
        }
    }

    if (needs_filter) {
        #ifdef _WIN32
        const char* slash = path;
        for (const char* p = path; *p; p++) {
            if (*p == '/' || *p == '\\') {
                slash = p;
            }
        }

        WIN32_FIND_DATA find_data;
        HANDLE find_handle = FindFirstFile(path, &find_data);
        if (find_handle == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "could not filter path: %s\n", path);
            abort();
        }

        do {
            char* new_path = malloc(MAX_PATH);
            if (slash == path) {
                sprintf_s(new_path, MAX_PATH, "%s", find_data.cFileName);
            } else {
                sprintf_s(new_path, MAX_PATH, "%.*s%s", (int)(slash - path) + 1, path, find_data.cFileName);
            }
            dyn_array_put(input_files, new_path);
        } while (FindNextFile(find_handle, &find_data));

        if (!FindClose(find_handle)) {
            fprintf(stderr, "internal error: failed to close filter\n");
            abort();
        }
        #else
        fprintf(stderr, "filepath filters not supported on your platform yet :(\n");
        fprintf(stderr, "umm... i mean you can probably remind me if you want :)\n");
        abort();
        #endif
    } else {
        dyn_array_put(input_files, path);
    }
}

int main(int argc, char** argv) {
    cuik_init();
    find_system_deps();

    program_name = argv[0];
    include_directories = dyn_array_create(const char*);
    input_libraries = dyn_array_create(const char*);
    input_files = dyn_array_create(const char*);

    // parse arguments
    int i = 1;
    for (;;) {
        Arg arg = get_cli_arg(&i, argc, argv);

        switch (arg.key) {
            case ARG_NONE: {
                if (arg.value == NULL) goto done_wit_args;

                append_input_path(arg.value);
                break;
            }
            case ARG_INCLUDE: {
                size_t len = strlen(arg.value);

                bool on_da_heap = false;
                const char* path = arg.value;
                if (path[len - 1] != '\\' && path[len - 1] != '/') {
                    // convert into a directory path
                    char* newstr = malloc(len + 2);
                    memcpy(newstr, arg.value, len);
                    newstr[len] = '/';
                    newstr[len + 1] = 0;

                    on_da_heap = true;
                    path = newstr;
                }

                // resolve a fullpath
                char* newstr = malloc(FILENAME_MAX);
                if (resolve_filepath(newstr, path)) {
                    dyn_array_put(include_directories, newstr);
                } else {
                    fprintf(stderr, "error: could not resolve include: %s\n", path);
                    return EXIT_FAILURE;
                }

                // free if it was actually on the heap
                if (on_da_heap) free((void*)path);
                break;
            }
            case ARG_LIB: {
                char* newstr = strdup(arg.value);

                char* ctx;
                char* a = strtok_r(newstr, ",", &ctx);
                while (a != NULL) {
                    dyn_array_put(input_libraries, a);
                    a = strtok_r(NULL, ",", &ctx);
                }
                break;
            }
            case ARG_OUT: output_name = arg.value; break;
            case ARG_OBJ: args_object_only = true; break;
            case ARG_RUN: args_run = true; break;
            case ARG_PREPROC: args_preprocess = true; break;
            case ARG_OPT: args_optimize = true; break;
            case ARG_TIME: args_time = true; break;
            case ARG_ASM: args_assembly = true; break;
            case ARG_AST: args_ast = true; break;
            case ARG_TYPES: args_types = true; break;
            case ARG_IR: args_ir = true; break;
            case ARG_VERBOSE: args_verbose = true; break;
            case ARG_HELP: {
                print_help();
                return EXIT_SUCCESS;
            }
            default: break;
        }
    }

    done_wit_args:
    if (dyn_array_length(input_files) == 0) {
        fprintf(stderr, "error: no input files!\n");
        return EXIT_FAILURE;
    }

    if (args_assembly) {
        fprintf(stderr, "error: emitting assembly doesn't work yet\n");
        return EXIT_FAILURE;
    }

    {
        const char* filename = output_name ? output_name : input_files[0];
        const char* ext = strrchr(filename, '.');
        size_t len = ext ? (ext - filename) : strlen(filename);

        if (filename[len - 1] == '/' && filename[len - 1] == '\\') {
            // we have an output directory instead of a file
            sprintf_s(output_path_no_ext, FILENAME_MAX, "%.*s%s", (int)len, filename, input_files[0]);
        } else {
            memcpy(output_path_no_ext, filename, len);
            output_path_no_ext[len] = '\0';
        }
    }

    if (args_time) {
        char* perf_output_path = malloc(FILENAME_MAX);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);

        jsonperf_profiler.user_data = perf_output_path;
        cuik_start_global_profiler(&jsonperf_profiler, true);
    }

    // spin up worker threads
    thread_pool = threadpool_create(calculate_worker_thread_count(), 4096);
    ithread_pool = (Cuik_IThreadpool){
        .user_data = thread_pool,
        .submit = tp_submit,
        .work_one_job = tp_work_one_job
    };

    cuik_create_compilation_unit(&compilation_unit);

    // get default system
    #if defined(_WIN32)
    TB_System sys = TB_SYSTEM_WINDOWS;
    #elif defined(__linux) || defined(linux)
    TB_System sys = TB_SYSTEM_LINUX;
    #elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh)
    TB_System sys = TB_SYSTEM_MACOS;
    #endif

    // get target
    target_desc = cuik_get_x64_target_desc();

    if (!args_ast && !args_types) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(TB_ARCH_X86_64, sys, TB_DEBUGFMT_NONE, &features);
    }

    if (args_preprocess) {
        // preproc only
        Cuik_CPP cpp;
        TokenStream tokens = cuik_preprocess_simple(
            &cpp, input_files[0], &cuik_default_fs, target_desc, true,
            dyn_array_length(include_directories),
            &include_directories[0]
        );

        cuikpp_finalize(&cpp);

        dump_tokens(stdout, &tokens);
        return EXIT_SUCCESS;
    }

    ////////////////////////////////
    // frontend work
    ////////////////////////////////
    if (args_verbose) printf("Frontend...\n");

    if (thread_pool != NULL) {
        // dispatch multithreaded
        dyn_array_for(i, input_files) {
            threadpool_submit(thread_pool, compile_file, (void*) input_files[i]);
        }

        CUIK_TIMED_BLOCK("wait") {
            threadpool_work_while_wait(thread_pool);
        }
    } else {
        // emit-ast is single threaded just to make it nicer to read
        dyn_array_for(i, input_files) {
            compile_file((void*) input_files[i]);
        }
    }

    if (args_verbose) printf("Internal link...\n");

    CUIK_TIMED_BLOCK("internal link") {
        cuik_internal_link_compilation_unit(&compilation_unit);
    }

    if (args_ast) {
        FOR_EACH_TU(tu, &compilation_unit) {
            cuik_dump_translation_unit(stdout, tu, true);
        }
    } else if (!args_types) {
        ////////////////////////////////
        // codegen
        ////////////////////////////////
        if (args_verbose) printf("Backend...\n");

        if (thread_pool != NULL) {
            FOR_EACH_TU(tu, &compilation_unit) {
                if (!args_ast && !args_types) {
                    cuik_visit_top_level_threaded(tu, &ithread_pool, 16384, NULL, irgen_visitor);
                }
            }

            threadpool_work_while_wait(thread_pool);
            threadpool_free(thread_pool);
            thread_pool = NULL;
        } else {
            FOR_EACH_TU(tu, &compilation_unit) {
                if (!args_ast && !args_types) {
                    cuik_visit_top_level(tu, NULL, irgen_visitor);
                }
            }
        }

        // place into a temporary directory if we don't need the obj file
        char obj_output_path[FILENAME_MAX];
        if (args_object_only) {
            if (args_verbose) printf("Exporting object file...\n");

            CUIK_TIMED_BLOCK("export") {
                sprintf_s(obj_output_path, FILENAME_MAX, "%s.obj", output_path_no_ext);

                if (!tb_module_export(mod, obj_output_path)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
                }
            }
        } else {
            char exe_path[FILENAME_MAX], lib_dir[FILENAME_MAX];
            sprintf_s(exe_path, FILENAME_MAX, "%s.exe", output_path_no_ext);
            sprintf_s(lib_dir, FILENAME_MAX, "%s/crt/lib/", crt_dirpath);

            if (0 /* use TB as the linker */) {
                size_t system_libpath_count = cuik_get_system_search_path_count();

                TB_LinkerInput link = { 0 };
                {
                    link.search_dir_count = system_libpath_count + 1;
                    link.search_dirs = malloc(link.search_dir_count * sizeof(const char*));

                    // fill search paths
                    cuik_get_system_search_paths(link.search_dirs, system_libpath_count);
                    link.search_dirs[system_libpath_count] = lib_dir;
                }

                // fill input files
                {
                    #ifdef _WIN32
                    link.input_count = dyn_array_length(input_libraries) + 4;
                    #else
                    link.input_count = dyn_array_length(input_libraries);
                    #endif

                    link.inputs = malloc(link.input_count);

                    // Add input libraries
                    const char** inputs = link.inputs;
                    dyn_array_for(i, input_libraries) {
                        *inputs++ = input_libraries[i];
                    }

                    #ifdef _WIN32
                    *inputs++ = "ucrt.lib";
                    *inputs++ = "msvcrt.lib";
                    *inputs++ = "vcruntime.lib";
                    *inputs++ = "win32_rt.lib";
                    #endif
                }

                if (!tb_module_export_exec(mod, exe_path, &link)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
                }
            } else {
                // generate a temporary file for it
                if (tmpnam(obj_output_path) == NULL) {
                    fprintf(stderr, "cannot get a temporary file for the .obj... resorting to violence\n");
                    return EXIT_FAILURE;
                }

                if (args_verbose) printf("Exporting object file...\n");

                if (!tb_module_export(mod, obj_output_path)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
                }

                if (args_verbose) printf("Linking...\n");

                // Invoke system linker
                Cuik_Linker l;
                if (cuiklink_init(&l)) {
                    bool subsystem_windows = false;
                    FOR_EACH_TU(tu, &compilation_unit) {
                        if (cuik_get_entrypoint_status(tu) == CUIK_ENTRYPOINT_WINMAIN) {
                            subsystem_windows = true;
                        }
                    }

                    if (subsystem_windows) {
                        cuiklink_subsystem_windows(&l);
                    }

                    // Add system libpaths
                    cuiklink_add_default_libpaths(&l);
                    cuiklink_add_libpath(&l, lib_dir);

                    // Add Cuik output
                    cuiklink_add_input_file(&l, obj_output_path);

                    // Add input libraries
                    dyn_array_for(i, input_libraries) {
                        cuiklink_add_input_file(&l, input_libraries[i]);
                    }

                    #ifdef _WIN32
                    cuiklink_add_input_file(&l, "ucrt.lib");
                    cuiklink_add_input_file(&l, "msvcrt.lib");
                    cuiklink_add_input_file(&l, "vcruntime.lib");
                    cuiklink_add_input_file(&l, "win32_rt.lib");
                    #endif

                    cuiklink_invoke(&l, output_path_no_ext, "ucrt");
                    //cuiklink_invoke_tb(&l, output_path_no_ext);
                    cuiklink_deinit(&l);

                    remove(obj_output_path);
                }
            }

            if (args_run) {
                #ifdef _WIN32
                for (char* i = exe_path; *i; i++) {
                    if (*i == '/') *i = '\\';
                }
                #endif

                printf("\n\nRunning: %s...\n", exe_path);
                int exit_code = system(exe_path);
                printf("Exit code: %d\n", exit_code);

                return exit_code;
            }
        }

        tb_free_thread_resources();
        tb_module_destroy(mod);
    }

    if (thread_pool != NULL) {
        threadpool_free(thread_pool);
        thread_pool = NULL;
    }

    //cuik_destroy_translation_unit(tu);
    //cuikpp_deinit(&cpp);

    //cuik_destroy_compilation_unit(&compilation_unit);
    if (args_time) cuik_stop_global_profiler();
    return 0;
}
