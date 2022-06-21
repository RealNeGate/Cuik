#include <cuik.h>
#include "helper.h"
#include "cli_parser.h"

static DynArray(const char*) include_directories;

static void irgen_visitor(TranslationUnit* tu, Stmt* restrict s, void* user_data) {
    cuik_generate_ir(tu, s);
}

int main(int argc, char** argv) {
    cuik_init();
    program_name = argv[0];
    include_directories = dyn_array_create(const char*, false);

    // parse arguments
    int i = 1;
    for (;;) {
        Arg arg = get_cli_arg(&i, argc, argv);

        switch (arg.key) {
            case ARG_NONE: {
                if (arg.value == NULL) goto done_wit_args;

                printf("Input file: %s\n", arg.value);
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
                    abort();
                }

                // free if it was actually on the heap
                if (on_da_heap) free((void*)path);
                break;
            }
            case ARG_HELP: {
                print_help();
                return 0;
            }
            default: break;
        }
    }

    done_wit_args:
    // verbose print
    if (1) {
        printf("INCLUDES:\n");
        for (int i = 0, count = dyn_array_length(include_directories); i < count; i++) {
            printf("  [%d] = %s\n", i, include_directories[i]);
        }
    }

    // find system libraries
    Cuik_SystemLibs* system_libs = find_system_libs();

    bool dump_ast = false;
    TB_Module* mod = NULL;
    if (!dump_ast) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, TB_DEBUGFMT_NONE, &features);
    }

    // preproc
    Cuik_CPP cpp;
    TokenStream tokens = cuik_preprocess_simple(&cpp, argv[1], system_libs,
                                                dyn_array_length(include_directories),
                                                &include_directories[0]);

    cuikpp_finalize(&cpp);

    // parse
    TranslationUnit* tu = cuik_parse_translation_unit(mod, &tokens, NULL);

    // codegen
    if (dump_ast) {
        cuik_dump_translation_unit(stdout, tu, true);
    } else {
        cuik_visit_top_level(tu, NULL, irgen_visitor);

        if (!tb_module_export(mod, "a.obj")) {
            fprintf(stderr, "error: tb_module_export failed!\n");
            abort();
        }

        tb_free_thread_resources();
        tb_module_destroy(mod);
    }

    cuik_destroy_translation_unit(tu);
    cuikpp_deinit(&cpp);
    return 0;
}
