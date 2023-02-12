#include <cuik.h>
#include <dyn_array.h>

#if CUIK_ALLOW_THREADS
#include <threads.h>
#include <stdatomic.h>

#define THREADPOOL_IMPL
#define HAS_CUIK_CLASS
#include "threadpool.h"
#endif

#include "spall_perf.h"
#include "live.h"

static void dump_tokens(FILE* out_file, TokenStream* s) {
    const char* last_file = NULL;
    int last_line = 0;

    Token* tokens = cuikpp_get_tokens(s);
    size_t count = cuikpp_get_token_count(s);

    for (size_t i = 0; i < count; i++) {
        Token* t = &tokens[i];

        if (t->location.raw == 0) __debugbreak();
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

static void test_callback(Cuik_Diagnostics* diag, void* userdata, DiagType type) {
}

int main(int argc, const char** argv) {
    cuik_init();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_init("perf.spall");
    spall_auto_thread_init(1, 1ull<<28ull);
    #endif

    Cuik_CompilerArgs args = {
        .version = CUIK_VERSION_C23,
        .target = cuik_target_host(),
        .toolchain = cuik_toolchain_host(),
        .flavor = TB_FLAVOR_EXECUTABLE,

        // .diag_callback = test_callback,
    };
    cuik_parse_args(&args, argc - 1, argv + 1);

    int status = EXIT_SUCCESS;
    if (dyn_array_length(args.sources) == 0) {
        fprintf(stderr, "error: no input files!\n");
        status = EXIT_FAILURE;
        goto done;
    }

    if (args.time) {
        char* perf_output_path = malloc(FILENAME_MAX);
        cuik_driver_get_output_name(&args, FILENAME_MAX, perf_output_path);
        strncat(perf_output_path, ".spall", FILENAME_MAX);

        cuikperf_start(perf_output_path, &spall_profiler, false);
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
            cuikdg_dump_to_file(cuikpp_get_token_stream(cpp), stderr);

            dump_tokens(stdout, cuikpp_get_token_stream(cpp));
            cuikpp_free(cpp);
            return EXIT_SUCCESS;
        } else {
            return EXIT_FAILURE;
        }
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
        status = cuik_driver_compile(tp, &args, true);

        if (args.verbose) {
            uint64_t now = cuik_time_in_nanos();
            printf("\n\nCUIK: %f ms\n", (now - start_time) / 1000000.0);
        }
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

    done:
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    spall_auto_quit();
    #endif
    return status;
}
