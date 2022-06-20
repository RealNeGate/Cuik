#include <cuik.h>
#include "helper.h"

// if an argument is set and has no args this is put
static const char* arg_is_set = "set";
static const char* program_name = NULL;
static int positional_arg_count = 0;

static struct {
#define OPTION(short_name, long_name, n, desc) const char* long_name;
#include "cli_options.h"
} args;

// return false for error
static bool parse_args(int argc, char** argv) {
    // parse arguments
    int positional_args = 0;
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            if (argv[i][1] == '-') {
                // long names
                const char* opt = &argv[i][2];

#define OPTION(short_name, long_name, n, desc) \
if (strncmp(opt, #long_name "=", sizeof(#long_name)) == 0) { \
if (n == 0) { \
args.long_name = arg_is_set; \
continue; \
} else {  \
if ((i + 1) >= argc) { \
fprintf(stderr, "error: expected argument after --" #long_name "\n"); \
return false; \
} \
args.long_name = argv[i] + sizeof("--" #long_name); \
i += 1; \
continue; \
} \
}
#include "cli_options.h"

                // could not find an option
                fprintf(stderr, "error: could not find option --%s\n", opt);
                return false;
            } else {
                // short names
                const char* opt = &argv[i][1];

#define OPTION(short_name, long_name, n, desc) \
if (strcmp(opt, #short_name) == 0) { \
if (n == 0) { \
args.long_name = arg_is_set; \
continue; \
} else { \
if ((i + 1) >= argc) { \
fprintf(stderr, "error: expected argument after -" #short_name "\n"); \
return false; \
} \
args.long_name = argv[i + 1]; \
i += 1; \
continue; \
} \
}
#include "cli_options.h"

                // could not find an option
                fprintf(stderr, "error: could not find option -%s\n", opt);
                return EXIT_FAILURE;
            }
        } else {
            // arguments with no options attached
            argv[positional_args++] = argv[i];
        }
    }

    positional_arg_count = positional_args;
    return true;
}

static void irgen_visitor(TranslationUnit* tu, Stmt* restrict s, void* user_data) {
    cuik_generate_ir(tu, s);
}

int main(int argc, char** argv) {
    program_name = argv[0];

    if (!parse_args(argc, argv)) {
        return EXIT_FAILURE;
    }

    if (args.help == arg_is_set) {
        printf("usage: %s <args>\n\n", program_name);

        int l = 0;

#define OPTION(short_name, long_name, n, desc) \
l = printf("  -" #short_name " | --" #long_name " "); \
for (int i = l; i < 20; i++) printf(" "); \
printf(desc "\n");
#include "cli_options.h"

        return 0;
    }

    cuik_init();

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
    TokenStream tokens = cuik_preprocess_simple(&cpp, argv[1], system_libs, 2, (const char*[]) {
                                                    "include/", "src/"
                                                });

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
