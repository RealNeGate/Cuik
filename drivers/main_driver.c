#include <cuik.h>
#include "helper.h"

static void irgen_visitor(TranslationUnit* tu, Stmt* restrict s, void* user_data) {
    cuik_generate_ir(tu, s);
}

int main(int argc, char** argv) {
    if (argc <= 1) {
        fprintf(stderr, "no input file!\n");
        return 1;
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
