#include <cuik.h>
#include "helper.h"
#include "cli_parser.h"

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
static bool args_preprocess;
static bool args_optimize;
static bool args_object_only;

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

                dyn_array_put(input_files, arg.value);
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
        char perf_output_path[FILENAME_MAX];
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);
        cuik_start_global_profiler(perf_output_path);
    }

    // get target
    const Cuik_TargetDesc* target = cuik_get_x64_target_desc();

    TB_Module* mod = NULL;
    if (!args_ast && !args_types) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, TB_DEBUGFMT_NONE, &features);
    }

    // preproc
    Cuik_CPP cpp;
    TokenStream tokens = cuik_preprocess_simple(
        &cpp, argv[1], &cuik_default_fs, target, true,
        dyn_array_length(include_directories),
        &include_directories[0]
    );

    cuikpp_finalize(&cpp);

    if (args_preprocess) {
        dump_tokens(stdout, &tokens);
        return EXIT_SUCCESS;
    }

    // parse
    TranslationUnit* tu = cuik_parse_translation_unit(mod, &tokens, target, NULL);

    // codegen
    if (args_types) {
        /* don't do shit */
    } else if (args_ast) {
        cuik_dump_translation_unit(stdout, tu, true);
    } else {
        cuik_visit_top_level(tu, NULL, irgen_visitor);

        if (!args_ir) {
            // place into a temporary directory if we don't need the obj file
            char obj_output_path[FILENAME_MAX];
            if (args_object_only) {
                sprintf_s(obj_output_path, FILENAME_MAX, "%s.obj", output_path_no_ext);

                if (!tb_module_export(mod, obj_output_path)) {
                    fprintf(stderr, "error: tb_module_export failed!\n");
                    abort();
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

                    if (!tb_module_export(mod, obj_output_path)) {
                        fprintf(stderr, "error: tb_module_export failed!\n");
                        abort();
                    }

                    // Invoke system linker
                    Cuik_Linker l;
                    if (cuiklink_init(&l)) {
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

            // linker
            /**/
        }
    }

    if (args_time) {
        cuik_stop_global_profiler();
    }

    cuik_destroy_translation_unit(tu);
    cuikpp_deinit(&cpp);
    return 0;
}
