#include <pool.h>

// pretends to act like link.exe
int run_link(int argc, const char** argv) {
    cuikperf_start("perf.spall");

    // find toolchain details from Cuik
    Cuik_Linker cl = { .toolchain = cuik_toolchain_host() };
    cl.toolchain.ctx = cl.toolchain.init();
    cuiklink_apply_toolchain_libs(&cl, false);

    CUIK_TIMED_BLOCK("linker") {
        #if CUIK_ALLOW_THREADS
        TPool pool;
        tpool_init(&pool, 5);
        TB_Linker* l = tb_linker_create(TB_EXECUTABLE_PE, TB_ARCH_X86_64, &pool);
        #else
        TB_Linker* l = tb_linker_create(TB_EXECUTABLE_PE, TB_ARCH_X86_64, NULL);
        #endif

        const char* output_name = "a.exe";

        dyn_array_for(i, cl.libpaths) {
            tb_linker_add_libpath(l, cl.libpaths[i]);
        }

        for (int i = 0; i < argc; i++) {
            const char* arg = argv[i];

            if (arg[0] == '-' || arg[0] == '/') {
                arg += 1;

                if (strncmp(arg, "out:", 4) == 0) {
                    output_name = arg + 4;
                } else if (strncmp(arg, "libpath:", 8) == 0) {
                    tb_linker_add_libpath(l, arg + 8);
                } else {
                    fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved option: -%s\n", arg);
                    cuikperf_region_end();
                    return EXIT_FAILURE;
                }
            } else {
                size_t path_len = strlen(arg);
                if ((path_len >= 4 && strcmp(arg + path_len - 4, ".lib") == 0) || (path_len >= 2 && strcmp(arg + path_len - 2, ".a") == 0)) {
                    tb_linker_append_library(l, arg);
                } else {
                    tb_linker_append_object(l, arg);
                }
            }
        }

        if (!tb_linker_export(l, output_name)) {
            cuikperf_region_end();
            return EXIT_FAILURE;
        }

        #if CUIK_ALLOW_THREADS
        tpool_destroy(&pool);
        #endif
    }

    cuikperf_stop();
    return EXIT_SUCCESS;
}
