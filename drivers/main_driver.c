#include <cuik.h>
#include "helper.h"
#include "big_array.h"
#include "cli_parser.h"
#include "json_perf.h"

#ifndef __CUIKC__
#define CUIK_ALLOW_THREADS 1
#else
#define CUIK_ALLOW_THREADS 0
#endif

#if CUIK_ALLOW_THREADS
#include "threadpool.h"
#endif

#define HEAP_ALLOC(s) malloc(s)

// compiler arguments
static DynArray(const char*) include_directories;
static DynArray(const char*) input_libraries;
static DynArray(const char*) input_files;
static DynArray(const char*) input_defines;
static DynArray(TB_FunctionPass) da_passes;
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
static bool args_experiment;
static int args_threads = -1;

static TB_Module* mod;

static Cuik_IThreadpool* ithread_pool;
static Cuik_Target target_desc;
static CompilationUnit compilation_unit;

#define OPT(name) (TB_FunctionPass){ #name, tb_opt_ ## name }
static void initialize_opt_passes(void) {
    char custom_luapath[FILENAME_MAX];
    da_passes = dyn_array_create(TB_FunctionPass);

    dyn_array_put(da_passes, OPT(hoist_locals));
    dyn_array_put(da_passes, OPT(merge_rets));
    dyn_array_put(da_passes, OPT(mem2reg));

    dyn_array_put(da_passes, OPT(hoist_invariants));
    dyn_array_put(da_passes, OPT(canonicalize));
    dyn_array_put(da_passes, OPT(remove_pass_node));
    dyn_array_put(da_passes, OPT(canonicalize));

    if (args_experiment) {
        sprintf_s(custom_luapath, FILENAME_MAX, "%s/custom.lua", crt_dirpath);
        dyn_array_put(da_passes, tb_opt_load_lua_pass(custom_luapath));
    }

    dyn_array_put(da_passes, OPT(dead_expr_elim));
    dyn_array_put(da_passes, OPT(dead_block_elim));
    dyn_array_put(da_passes, OPT(compact_dead_regs));
}

#if CUIK_ALLOW_THREADS
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
    threadpool_submit((threadpool_t*) user_data, fn, arg);
}

static void tp_work_one_job(void* user_data) {
    threadpool_work_one_job((threadpool_t*) user_data);
}
#endif

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
            tb_function_optimize(func, dyn_array_length(da_passes), da_passes);
        }

        if (args_ir) {
            cuik_lock_compilation_unit(&compilation_unit);
            tb_function_print(func, tb_default_print_callback, stdout);
            printf("\n\n");
            cuik_unlock_compilation_unit(&compilation_unit);
        } else {
            tb_module_compile_func(mod, func, args_optimize ? TB_ISEL_COMPLEX : TB_ISEL_FAST);
        }

        tb_function_free(func);
    }
}

// it'll use the normal CLI crap to do so
static Cuik_CPP* make_preprocessor(void) {
    Cuik_CPP* cpp = HEAP_ALLOC(sizeof(Cuik_CPP));
    cuikpp_init(cpp, NULL);
    cuikpp_set_common_defines(cpp, &target_desc, true);

    dyn_array_for(i, include_directories) {
        cuikpp_add_include_directory(cpp, include_directories[i]);
    }

    dyn_array_for(i, input_defines) {
        const char* equal = strchr(input_defines[i], '=');

        if (equal == NULL) {
            cuikpp_define_empty(cpp, input_defines[i]);
        } else {
            cuikpp_define_slice(
                cpp,
                // before equals
                equal - input_defines[i], input_defines[i],
                // after equals
                strlen(equal + 1), equal + 1
            );
        }
    }

    return cpp;
}

static void compile_file(void* arg) {
    const char* input = (const char*)arg;

    // preproc
    Cuik_CPP* cpp = make_preprocessor();
    TokenStream tokens = cuikpp_run(cpp, input);
    cuikpp_finalize(cpp);

    // parse
    Cuik_ErrorStatus errors;
    TranslationUnit* tu = cuik_parse_translation_unit(&(Cuik_TranslationUnitDesc){
            .tokens      = &tokens,
            .errors      = &errors,
            .ir_module   = mod,
            .target      = &target_desc,
            #if CUIK_ALLOW_THREADS
            .thread_pool = ithread_pool ? ithread_pool : NULL,
            #endif
        });

    if (tu == NULL) {
        printf("Failed to parse with errors...");
        exit(1);
    }

    cuik_set_translation_unit_user_data(tu, cpp);
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
            char* new_path = HEAP_ALLOC(MAX_PATH);
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
    //_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF);
    //_CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_DEBUG);

    cuik_init();
    find_system_deps();

    program_name = argv[0];
    include_directories = dyn_array_create(const char*);
    input_libraries = dyn_array_create(const char*);
    input_files = dyn_array_create(const char*);
    input_defines = dyn_array_create(const char*);

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
            case ARG_DEFINE: {
                dyn_array_put(input_defines, arg.value);
                break;
            }
            case ARG_INCLUDE: {
                // resolve a fullpath
                char* newstr = HEAP_ALLOC(FILENAME_MAX);
                if (resolve_filepath(newstr, arg.value)) {
                    size_t end = strlen(newstr);

                    if (newstr[end - 1] != '\\' && newstr[end - 1] != '/') {
                        assert(end+2 < FILENAME_MAX);
                        #ifdef _WIN32
                        newstr[end] = '\\';
                        #else
                        newstr[end] = '/';
                        #endif
                        newstr[end+1] = '\0';
                    }

                    dyn_array_put(include_directories, newstr);
                } else {
                    fprintf(stderr, "error: could not resolve include: %s\n", arg.value);
                    return EXIT_FAILURE;
                }
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
            case ARG_EXPERIMENT: args_experiment = true; break;
            case ARG_THREADS: args_threads = atoi(arg.value); break;
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

        if (filename[len - 1] == '/' || filename[len - 1] == '\\') {
            const char* slash = strrchr(input_files[0], '/');
            if (!slash) slash = strrchr(input_files[0], '\\');

            if (!slash) slash = input_files[0];
            else slash += 1; // skip the slash

            // we have an output directory instead of a file
            sprintf_s(output_path_no_ext, FILENAME_MAX, "%.*s%s", (int)len, filename, slash);
        } else {
            memcpy(output_path_no_ext, filename, len);
            output_path_no_ext[len] = '\0';
        }

        if (output_name == NULL) {
            output_name = output_path_no_ext;
        }
    }

    if (args_time) {
        char* perf_output_path = HEAP_ALLOC(FILENAME_MAX);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);

        jsonperf_profiler.user_data = perf_output_path;
        cuik_start_global_profiler(&jsonperf_profiler, true);
    }

    // spin up worker threads
    #if CUIK_ALLOW_THREADS
    threadpool_t* thread_pool = NULL;
    int thread_count = args_threads >= 0 ? args_threads : calculate_worker_thread_count();
    if (thread_count >= 2) {
        thread_pool = threadpool_create(thread_count - 1, 4096);
        ithread_pool = HEAP_ALLOC(sizeof(Cuik_IThreadpool));
        *ithread_pool = (Cuik_IThreadpool){
            .user_data = thread_pool,
            .submit = tp_submit,
            .work_one_job = tp_work_one_job
        };
    }
    #endif

    initialize_opt_passes();
    cuik_create_compilation_unit(&compilation_unit);

    // get default system
    #if defined(_WIN32)
    target_desc.sys = CUIK_SYSTEM_WINDOWS;
    #elif defined(__linux) || defined(linux)
    target_desc.sys = CUIK_SYSTEM_LINUX;
    #elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh)
    target_desc.sys = CUIK_SYSTEM_MACOS;
    #endif

    // get target
    target_desc.arch = cuik_get_x64_target_desc();

    if (!args_ast && !args_types) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(TB_ARCH_X86_64, cuik_system_to_tb(target_desc.sys), TB_DEBUGFMT_CODEVIEW, &features);
    }

    if (args_preprocess) {
        // preproc only
        Cuik_CPP* cpp = make_preprocessor();
        TokenStream tokens = cuikpp_run(cpp, input_files[0]);
        cuikpp_finalize(cpp);

        dump_tokens(stdout, &tokens);
        cuikpp_deinit(cpp);
        return EXIT_SUCCESS;
    }

    ////////////////////////////////
    // frontend work
    ////////////////////////////////
    if (args_verbose) printf("Frontend...\n");

    if (CUIK_ALLOW_THREADS && ithread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        // dispatch multithreaded
        dyn_array_for(i, input_files) {
            threadpool_submit(thread_pool, compile_file, (void*) input_files[i]);
        }

        CUIK_TIMED_BLOCK("wait") {
            threadpool_work_while_wait(thread_pool);
        }
        #endif
    } else {
        dyn_array_for(i, input_files) compile_file((void*) input_files[i]);
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

        if (ithread_pool != NULL) {
            FOR_EACH_TU(tu, &compilation_unit) {
                if (!args_ast && !args_types) {
                    cuik_visit_top_level_threaded(tu, ithread_pool, 8192, NULL, irgen_visitor);
                }

                Cuik_CPP* cpp = cuik_get_translation_unit_user_data(tu);
                cuikpp_deinit(cpp);
                free(cpp);
            }
        } else {
            FOR_EACH_TU(tu, &compilation_unit) {
                if (!args_ast && !args_types) {
                    cuik_visit_top_level(tu, NULL, irgen_visitor);
                }

                Cuik_CPP* cpp = cuik_get_translation_unit_user_data(tu);
                cuikpp_deinit(cpp);
                free(cpp);
            }
        }

        // place into a temporary directory if we don't need the obj file
        char obj_output_path[FILENAME_MAX];
        if (target_desc.sys == CUIK_SYSTEM_WINDOWS){
            sprintf_s(obj_output_path, FILENAME_MAX, "%s.obj", output_path_no_ext);
        } else {
            sprintf_s(obj_output_path, FILENAME_MAX, "%s.o", output_path_no_ext);
        }

        if (args_object_only) {
            if (args_verbose) printf("Exporting object file...\n");

            CUIK_TIMED_BLOCK("export") {
                if (!tb_module_export(mod, obj_output_path)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
                }
            }
        } else if (!args_ir) {
            char lib_dir[FILENAME_MAX];
            sprintf_s(lib_dir, FILENAME_MAX, "%s/crt/lib/", crt_dirpath);

            if (0 /* use TB as the linker */) {
                size_t system_libpath_count = cuik_get_system_search_path_count();

                TB_LinkerInput link = { 0 };
                {
                    link.search_dir_count = system_libpath_count + 1;
                    link.search_dirs = HEAP_ALLOC(link.search_dir_count * sizeof(const char*));

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

                    link.inputs = HEAP_ALLOC(link.input_count * sizeof(const char*));

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

                if (!tb_module_export_exec(mod, output_name, &link)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
                }
            } else {
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

                    if (!cuiklink_invoke(&l, output_path_no_ext, "ucrt")) {
                        fprintf(stderr, "Linker failure!\n");
                        exit(1);
                    }
                    //cuiklink_invoke_tb(&l, output_path_no_ext);
                    cuiklink_deinit(&l);
                }
            }

            if (args_run) {
                char* exe = strdup(output_name);

                #ifdef _WIN32
                for (char* s = exe; *s; s++) {
                    if (*s == '/') *s = '\\';
                }
                #endif

                printf("\n\nRunning: %s...\n", exe);
                int exit_code = system(exe);
                printf("Exit code: %d\n", exit_code);

                return exit_code;
            }
        }

        tb_free_thread_resources();
        tb_module_destroy(mod);
    }

    #if CUIK_ALLOW_THREADS
    if (thread_pool != NULL) {
        threadpool_free(thread_pool);
        thread_pool = NULL;
    }
    #endif

    //cuikpp_deinit(&cpp);

    cuik_destroy_compilation_unit(&compilation_unit);
    if (args_time) cuik_stop_global_profiler();

    // _CrtDumpMemoryLeaks();
    return 0;
}
