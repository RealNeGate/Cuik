#include <cuik.h>
#include "helper.h"
#include <dyn_array.h>

#ifndef __CUIK__
#define CUIK_ALLOW_THREADS 1
#else
#define CUIK_ALLOW_THREADS 0
#endif

#if CUIK_ALLOW_THREADS
#include <threads.h>
#include <stdatomic.h>

#define THREADPOOL_IMPL
#define HAS_CUIK_CLASS
#include "threadpool.h"
#endif

#include "spall_perf.h"
#include "live.h"

/*static void initialize_opt_passes(void) {
    da_passes = dyn_array_create(TB_Pass, 32);

    if (args.opt_level) {
        dyn_array_put(da_passes, tb_opt_hoist_locals());
        dyn_array_put(da_passes, tb_opt_merge_rets());

        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());

        dyn_array_put(da_passes, tb_opt_mem2reg());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_instcombine());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_dead_expr_elim());
        dyn_array_put(da_passes, tb_opt_dead_block_elim());
        dyn_array_put(da_passes, tb_opt_subexpr_elim());
        dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        dyn_array_put(da_passes, tb_opt_compact_dead_regs());

        // dyn_array_put(da_passes, tb_opt_inline());

        // aggresive optimizations
        // TODO(NeGate): loop optimizations, data structure reordering
        // switch optimizations

        // dyn_array_put(da_passes, tb_opt_remove_pass_nodes());
        // dyn_array_put(da_passes, tb_opt_compact_dead_regs());
    }
}*/

#if CUIK_ALLOW_THREADS
static int calculate_worker_thread_count(void) {
    #ifdef _WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return (sysinfo.dwNumberOfProcessors / 4) * 3;
    #else
    return 1;
    #endif
}
#endif

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];

        ResolvedSourceLoc r = cuikpp_find_location(s, t->location);
        if (last_file != r.file->filename) {
            // TODO(NeGate): Kinda shitty but i just wanna duplicate
            // the backslashes to avoid them being treated as an escape
            const char* in = (const char*) r.file->filename;
            char str[FILENAME_MAX], *out = str;

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

            fprintf(out_file, "\n#line %d \"%s\"\t", r.line, str);
            last_file = r.file->filename;
        }

        if (last_line != r.line) {
            fprintf(out_file, "\n/* line %3d */\t", r.line);
            last_line = r.line;
        }

        fprintf(out_file, "%.*s ", (int) t->content.length, t->content.data);
    }
}

int main(int argc, const char** argv) {
    cuik_init();
    find_system_deps();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_init("perf.spall");
    spall_auto_thread_init(1, 1ull<<28ull, 65536);
    #endif

    Cuik_CompilerArgs args = {
        .version = CUIK_VERSION_C23,
        .target = cuik_target_host(),
        .toolchain = cuik_toolchain_host(),
        .flavor = TB_FLAVOR_EXECUTABLE,
        .core_dirpath = crt_dirpath,
    };
    cuik_parse_args(&args, argc - 1, argv + 1);

    if (dyn_array_length(args.sources) == 0) {
        fprintf(stderr, "error: no input files!\n");
        return EXIT_FAILURE;
    }

    if (args.time) {
        char output_path_no_ext[FILENAME_MAX];
        cuik_driver_get_output_name(&args, FILENAME_MAX, output_path_no_ext);

        char* perf_output_path = cuikperf_init(FILENAME_MAX, &spall_profiler, false);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.spall", output_path_no_ext);

        cuikperf_start();
    }

    // spin up worker threads
    Cuik_IThreadpool *tp = NULL, __tp;
    #if CUIK_ALLOW_THREADS
    if (args.threads > 1) {
        if (args.verbose) printf("Starting with %d threads...\n", args.threads);

        __tp = threadpool_create_class(args.threads - 1, 4096);
        tp = &__tp;
    }
    #endif

    if (args.preprocess) {
        // preproc only
        Cuik_CPP* cpp = cuik_driver_preprocess(args.sources[0], &args, true);

        if (cpp) {
            dump_tokens(stdout, cuikpp_get_token_stream(cpp));
            cuikpp_free(cpp);
        } else {
            fprintf(stderr, "Could not preprocess file: %s", args.sources[0]);
            return EXIT_FAILURE;
        }

        return EXIT_SUCCESS;
    }

    if (args.live) {
        LiveCompiler l;
        do {
            printf("\x1b[2J");
            printf("OUTPUT OF %s:\n", args.sources[0]);

            cuik_driver_compile(tp, &args, true);
        } while (live_compile_watch(&l, &args));
    } else {
        uint64_t start_time = args.verbose ? cuik_time_in_nanos() : 0;
        int status = cuik_driver_compile(tp, &args, true);

        if (args.verbose) {
            uint64_t now = cuik_time_in_nanos();
            printf("\n\nCUIK: %f ms\n", (now - start_time) / 1000000.0);
        }

        if (status != 0) exit(status);
    }

    #if CUIK_ALLOW_THREADS
    threadpool_destroy_class(tp);
    #endif

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

    if (args.time) cuikperf_stop();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    spall_auto_quit();
    #endif
    return 0;
}
