#include <cuik.h>

#ifdef _WIN32
#  define WIN32_MEAN_AND_LEAN
#  include <windows.h>
#else
#  include <unistd.h>
#endif

static bool get_exe_path(char path[FILENAME_MAX]) {
#ifdef _WIN32
    return (GetModuleFileNameA(NULL, path, FILENAME_MAX) > 0);
#else
    return (readlink("/proc/self/exe", path, FILENAME_MAX) > 0);
#endif
}

// tries to walk about `steps` slashes in the filepath and return the pointer to said
// slash, if it can't reach then it'll return NULL
static const char* step_out_dir(const char path[FILENAME_MAX], int steps) {
    int slashes_hit = 0;
    const char* end = path + strlen(path);

    while (slashes_hit != steps && end-- != path) {
        if (*end == '/') slashes_hit++;
        else if (*end == '\\') slashes_hit++;
    }

    return (slashes_hit == steps) ? end : NULL;
}

static void irgen_visitor(TranslationUnit* tu, Stmt* restrict s, void* user_data) {
    cuik_generate_ir(tu, s);
}

int main(int argc, char** argv) {
    if (argc <= 1) {
        fprintf(stderr, "no input file!\n");
        return 1;
    }

    // find system libraries
    Cuik_SystemLibs* system_libs;
    {
        char crt_dir[FILENAME_MAX];
        if (!get_exe_path(crt_dir)) {
            fprintf(stderr, "error: could not locate executable path");
            return 1;
        }

        char* slash = (char*)step_out_dir(crt_dir, 2);
        if (slash == NULL) {
            fprintf(stderr, "error: could not locate executable path");
            return 1;
        }

        *slash = '\0';
        system_libs = cuik_get_system_includes(crt_dir);
    }

    bool dump_ast = false;
    TB_Module* mod = NULL;
    if (!dump_ast) {
        TB_FeatureSet features = {0};
        mod = tb_module_create(TB_ARCH_X86_64, TB_SYSTEM_WINDOWS, TB_DEBUGFMT_NONE, &features);
    }

    // preproc
    Cuik_CPP cpp;
    TokenStream tokens = cuik_preprocess_simple(&cpp, argv[1], system_libs, 0, NULL);

    int count = cuikpp_get_file_table_count(&cpp);
    Cuik_FileEntry* entries = cuikpp_get_file_table(&cpp);
    for (int i = 0; i < count; i++) {
        for (int j = 0; j < entries[i].depth; j++) printf(".");
        printf(" %s\n", entries[i].filepath);
    }

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
