#include <log.h>
#include <cuik.h>
#include <dyn_array.h>

#include "spall_perf.h"
#include "live.h"

// hacky but i dont care
#include <file_map.h>

#ifdef CUIK_USE_TB
#include "objdump.h"
#include "link.h"
#endif

#include "bindgen.h"

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

static bool leak_visit(const mi_heap_t* heap, const mi_heap_area_t* area, void* block, size_t block_size, void* arg) {
    if (area->used && block != NULL) {
        printf("| %p (%zu bytes)\n", block, block_size);
    }
    return true;
}

static void leak_detect(void) {
    printf("Leak:\n");
    mi_collect(true);
    mi_heap_visit_blocks(mi_heap_get_default(), true, leak_visit, NULL);
}

#ifdef CUIK_USE_SPALL_AUTO
static void spall_die(void) {
    spall_auto_thread_quit();
    spall_auto_quit();
}
#endif

int main(int argc, const char** argv) {
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_init("perf.spall");
    spall_auto_thread_init(1, 1ull<<28ull);
    atexit(spall_die);
    #endif

    int status = EXIT_SUCCESS;
    if (argc >= 2) {
        #ifdef CUIK_USE_TB
        if (strcmp(argv[1], "-objdump") == 0) return run_objdump(argc - 2, argv + 2);
        if (strcmp(argv[1], "-link")    == 0) return run_link(argc - 2, argv + 2);
        #endif

        if (strcmp(argv[1], "-bindgen") == 0) return run_bindgen(argc - 2, argv + 2);
    }

    cuik_init(true);
    log_set_level(LOG_DEBUG);

    Cuik_DriverArgs args = {
        .version   = CUIK_VERSION_C23,
        .toolchain = cuik_toolchain_host(),

        #ifdef CUIK_USE_TB
        .flavor    = TB_FLAVOR_EXECUTABLE,
        #endif
    };

    if (!cuik_parse_driver_args(&args, argc - 1, argv + 1)) {
        return EXIT_SUCCESS;
    }

    // default to host target
    if (args.target == NULL) {
        args.target = cuik_target_host();
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
        snprintf(perf_output_path, FILENAME_MAX, "%s.spall", args.output_name ? args.output_name : args.sources[0]->data);

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

    // compile source files
    size_t obj_count = dyn_array_length(args.sources);
    Cuik_BuildStep** objs = cuik_malloc(obj_count * sizeof(Cuik_BuildStep*));
    dyn_array_for(i, args.sources) {
        objs[i] = cuik_driver_cc(&args, args.sources[i]->data);
    }

    // link (if no codegen is performed this doesn't *really* do much)
    Cuik_BuildStep* linked = cuik_driver_ld(&args, obj_count, objs);
    if (!cuik_step_run(linked, tp)) {
        status = 1;
    }

    cuik_step_free(linked);
    cuik_free(objs);

    #if CUIK_ALLOW_THREADS
    cuik_threadpool_destroy(tp);
    #endif

    if (args.time) cuikperf_stop();
    cuik_free_thread_resources();

    done:
    // Free arguments
    cuik_toolchain_free(&args.toolchain);
    cuik_free_target(args.target);
    cuik_free_driver_args(&args);

    // stb_leakcheck_dumpmem();
    return status;
}
