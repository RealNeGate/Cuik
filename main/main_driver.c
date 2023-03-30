#include <cuik.h>
#include <dyn_array.h>

#include "spall_perf.h"
#include "live.h"

// hacky but i dont care
#include <file_map.h>

#include "objdump.h"
#include "bindgen.h"
#include "link.h"

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

// #define STB_LEAKCHECK_IMPLEMENTATION
// #include <stb_leakcheck.h>

int main(int argc, const char** argv) {
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_init("perf.spall");
    spall_auto_thread_init(1, 1ull<<28ull);
    #endif

    // Cuik can be opened as a OBJ file parser
    int status = EXIT_SUCCESS;
    if (argc >= 2) {
        if (strcmp(argv[1], "-objdump") == 0) {
            return run_objdump(argc - 2, argv + 2);
        } else if (strcmp(argv[1], "-link") == 0) {
            return run_link(argc - 2, argv + 2);
        } else if (strcmp(argv[1], "-bindgen") == 0) {
            return run_bindgen(argc - 2, argv + 2);
        }
    }

    cuik_init();

    Cuik_DriverArgs args = {
        .version = CUIK_VERSION_C23,
        .target = cuik_target_host(),
        .toolchain = cuik_toolchain_host(),
        .flavor = TB_FLAVOR_EXECUTABLE,
    };

    if (!cuik_parse_driver_args(&args, argc - 1, argv + 1)) {
        return EXIT_SUCCESS;
    }

    if (dyn_array_length(args.sources) == 0) {
        fprintf(stderr, "error: no input files!\n");
        status = EXIT_FAILURE;
        goto done;
    }

    if (args.think) {
        uint64_t t1 = cuik_time_in_nanos();
        double elapsed = 0.0;

        // 120 seconds of gamer time
        printf("Waiting around...\n\n");
        printf(
            "So people have told me that 2 minute compiles aren't really that bad\n"
            "so i figured that we should give them the freedom to waste their time\n"
        );

        int old_chars = -1;
        while (elapsed = (cuik_time_in_nanos() - t1) / 1000000000.0, elapsed < 120.0) {
            int num_chars = (int)((elapsed / 120.0) * 30.0);
            if (num_chars != old_chars) {
                old_chars = num_chars;

                printf("\r[");
                for (int i = 0; i < num_chars; i++) printf("#");
                for (int i = 0; i < 30 - num_chars; i++) printf(" ");
                printf("]");
            }

            #if CUIK_ALLOW_THREADS
            thrd_yield();
            #endif
        }
        printf("\n");
    }

    if (args.time) {
        char* perf_output_path = cuik_malloc(FILENAME_MAX);
        cuik_driver_get_output_name(&args, FILENAME_MAX, perf_output_path);
        strncat(perf_output_path, ".spall", FILENAME_MAX);

        cuikperf_start(perf_output_path, &spall_profiler, false);
        cuik_free(perf_output_path);
    }

    // spin up worker threads
    Cuik_IThreadpool* tp = NULL;
    #if CUIK_ALLOW_THREADS
    if (args.threads > 1) {
        if (args.verbose) printf("Starting with %d threads...\n", args.threads);

        tp = cuik_threadpool_create(args.threads - 1, 4096);
    }
    #endif

    if (args.live) {
        LiveCompiler l;
        do {
            printf("\x1b[2J");
            printf("OUTPUT OF %s:\n", args.sources[0]);

            CompilationUnit* cu;
            cuik_driver_compile(tp, &args, true, true, &cu);
            cuik_destroy_compilation_unit(cu);
        } while (live_compile_watch(&l, &args));
    } else {
        uint64_t start_time = args.verbose ? cuik_time_in_nanos() : 0;

        CompilationUnit* cu;
        status = !cuik_driver_compile(tp, &args, true, true, &cu);
        cuik_destroy_compilation_unit(cu);

        if (args.verbose) {
            uint64_t now = cuik_time_in_nanos();
            printf("\n\nCUIK: %f ms\n", (now - start_time) / 1000000.0);
        }
    }

    #if CUIK_ALLOW_THREADS
    cuik_threadpool_destroy(tp);
    #endif

    if (args.time) cuikperf_stop();
    cuik_free_driver_args(&args);

    done:
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    spall_auto_quit();
    #endif

    // stb_leakcheck_dumpmem();
    return status;
}
