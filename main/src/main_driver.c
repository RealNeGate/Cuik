#include <cuik.h>
#include "helper.h"
#include "spall_perf.h"
#include <dyn_array.h>

// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#ifdef _WIN32
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup _strdup
#endif

#ifndef __CUIK__
#define CUIK_ALLOW_THREADS 1
#else
#define CUIK_ALLOW_THREADS 0
#endif

#if CUIK_ALLOW_THREADS
#include <threads.h>
#include <stdatomic.h>
#include "threadpool.h"
#endif

static Cuik_IThreadpool* ithread_pool;

typedef struct TargetOption {
    const char* key;

    Cuik_Target* (*target)(Cuik_System, Cuik_Environment);
    Cuik_System system;
    Cuik_Environment env;
} TargetOption;
static DynArray(TargetOption) target_options;

#include "live.h"

static void exit_or_hook(int code) {
    if (IsDebuggerPresent()) {
        __debugbreak();
    }
    exit(code);
}

static void initialize_targets(void) {
    target_options = dyn_array_create(TargetOption, 32);

    #define M(a, b, c, d) dyn_array_put(target_options, (TargetOption){ a, b, c, d })
    M("x64_windows_msvc",         cuik_target_x64,       CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC);
    M("x64_macos_gnu",            cuik_target_x64,       CUIK_SYSTEM_MACOS,       CUIK_ENV_GNU);
    M("x64_linux_gnu",            cuik_target_x64,       CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU);
    M("aarch64_windows_msvc",     cuik_target_aarch64,   CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC);
    M("wasm32",                   cuik_target_wasm,      CUIK_SYSTEM_WEB,         CUIK_ENV_GNU);
}

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

static void tp_submit(void* user_data, void fn(void*), void* arg) {
    threadpool_submit((threadpool_t*) user_data, fn, arg);
}

static void tp_work_one_job(void* user_data) {
    threadpool_work_one_job((threadpool_t*) user_data);
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

static bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);

    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

#if 0
// handles the **.c *.c type stuff
static void filtered_append(const char* path, bool recursive) {
    const char* slash = path;
    for (const char* p = path; *p; p++) {
        if (*p == '/' || *p == '\\') {
            slash = p;
        }
    }

    #ifdef _WIN32
    WIN32_FIND_DATA find_data;
    HANDLE find_handle = FindFirstFile(path, &find_data);

    // loops through normal files
    if (find_handle != INVALID_HANDLE_VALUE) {
        do {
            char tmp[FILENAME_MAX];
            if (slash == path) {
                sprintf_s(tmp, MAX_PATH, "%s", find_data.cFileName);
            } else {
                sprintf_s(tmp, MAX_PATH, "%.*s%s", (int)(slash - path) + 1, path, find_data.cFileName);
            }

            char* new_path = malloc(MAX_PATH);
            if (!cuik_canonicalize_path(new_path, tmp)) {
                fprintf(stderr, "Invalid filepath! %s\n", tmp);
            }

            if (str_ends_with(new_path, ".o") || str_ends_with(new_path, ".obj")) {
                dyn_array_put(input_objects, new_path);
            } else {
                dyn_array_put(input_files, new_path);
            }
        } while (FindNextFile(find_handle, &find_data));
    }
    FindClose(find_handle);

    if (recursive) {
        char dir_path[MAX_PATH];
        sprintf_s(dir_path, sizeof(dir_path), "%.*s*", (int)(slash - path) + 1, path);
        HANDLE dir = FindFirstFile(dir_path, &find_data);

        if (dir != INVALID_HANDLE_VALUE) {
            do {
                if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && find_data.cFileName[0] != '.') {
                    char new_pattern[FILENAME_MAX];
                    sprintf_s(new_pattern, sizeof(new_pattern), "%.*s%s%s", (int)(slash - path) + 1, path, find_data.cFileName, slash);

                    filtered_append(new_pattern, true);
                }
            } while (FindNextFile(dir, &find_data));
        }
        FindClose(dir);
    }

    #else
    fprintf(stderr, "filepath filters not supported on your platform yet :(\n");
    fprintf(stderr, "umm... i mean you can probably remind me if you want :)\n");
    abort();
    #endif
}

// we can do a bit of filter such as '*.c' where it'll take all
// paths in the folder that end with .c
static void append_input_path(const char* path) {
    // we don't check this very well because we're based
    const char* star = NULL;
    for (const char* p = path; *p; p++) {
        if (*p == '*') {
            star = p;
            break;
        }
    }

    if (star != NULL) {
        // this is a really hacky way to handle recursive directories
        // but im a baller so i dont care
        filtered_append(path, star[1] == '*');
    } else {
        char* newstr = malloc(FILENAME_MAX);
        if (!cuik_canonicalize_path(newstr, path)) {
            fprintf(stderr, "Invalid filepath! %s\n", path);
        }

        if (str_ends_with(newstr, ".a") || str_ends_with(newstr, ".lib")) {
            dyn_array_put(input_libraries, newstr);
        } else if (str_ends_with(newstr, ".o") || str_ends_with(newstr, ".obj")) {
            dyn_array_put(input_objects, newstr);
        } else {
            dyn_array_put(input_files, newstr);
        }
    }
}
#endif

int main(int argc, const char** argv) {
    cuik_init();
    find_system_deps();
    initialize_targets();

    // program_name = argv[0];

    Cuik_CompilerArgs args = { .version = CUIK_VERSION_C23 };
    args.includes  = dyn_array_create(const char*, 64);
    args.libraries = dyn_array_create(const char*, 64);
    args.sources   = dyn_array_create(const char*, 64);
    args.defines   = dyn_array_create(const char*, 64);

    // get default system
    #if defined(_WIN32)
    args.target = cuik_target_x64(CUIK_SYSTEM_WINDOWS, CUIK_ENV_MSVC);
    #elif defined(__linux) || defined(linux)
    args.target = cuik_target_x64(CUIK_SYSTEM_LINUX, CUIK_ENV_MSVC);
    #elif defined(__APPLE__) || defined(__MACH__) || defined(macintosh)
    args.target = cuik_target_x64(CUIK_SYSTEM_MACOS, CUIK_ENV_MSVC);
    #endif

    cuik_parse_args(&args, argc - 1, argv + 1);

    // parse arguments
    #if 0
    int i = 1;
    for (;;) {
        Arg arg = get_cli_arg(&i, argc, argv);
        switch (arg.key) {
            case ARG_NONE: {
                if (arg.value == NULL) goto done_wit_args;

                append_input_path(arg.value);
                break;
            }
            case ARG_DEFINE: {
                dyn_array_put(input_defines, arg.value);
                break;
            }
            case ARG_INCLUDE: {
                // resolve a fullpath
                char* newstr = malloc(FILENAME_MAX);
                if (cuik_canonicalize_path(newstr, arg.value)) {
                    size_t end = strlen(newstr);

                    if (newstr[end - 1] != '\\' && newstr[end - 1] != '/') {
                        assert(end+2 < FILENAME_MAX);
                        #ifdef _WIN32
                        newstr[end] = '\\';
                        #else
                        newstr[end] = '/';
                        #endif
                        newstr[end+1] = '\0';
                    }

                    dyn_array_put(include_directories, newstr);
                } else {
                    fprintf(stderr, "error: could not resolve include: %s\n", arg.value);
                    return EXIT_FAILURE;
                }
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
            case ARG_TARGET: {
                bool success = false;
                for (size_t i = 0; i < dyn_array_length(target_options); i++) {
                    if (strcmp(arg.value, target_options[i].key) == 0) {
                        TargetOption* o = &target_options[i];
                        target_desc = o->target(o->system, o->env);
                        success = true;
                        break;
                    }
                }

                if (!success) {
                    fprintf(stderr, "unknown target: %s\n", arg.value);
                    fprintf(stderr, "Supported targets:\n");
                    for (size_t i = 0; i < dyn_array_length(target_options); i++) {
                        fprintf(stderr, "    %s\n", target_options[i].key);
                    }
                    fprintf(stderr, "\n");
                }
                break;
            }
            case ARG_LANG: {
                if (strcmp(arg.value, "c11")) args_verbose = CUIK_VERSION_C11;
                else if (strcmp(arg.value, "c23")) args_verbose = CUIK_VERSION_C23;
                else if (strcmp(arg.value, "glsl")) args_verbose = CUIK_VERSION_GLSL;
                else {
                    fprintf(stderr, "unknown compiler version: %s\n", arg.value);
                    fprintf(stderr, "supported languages: c11, c23, glsl\n");
                }
                break;
            }
            case ARG_O0: args_opt_level = 0; break;
            case ARG_O1: args_opt_level = 1; break;
            case ARG_OUTPUT: output_name = arg.value; break;
            case ARG_OBJECT: flavor = TB_FLAVOR_OBJECT; break;
            case ARG_PREPROC: args_preprocess = true; break;
            case ARG_RUN: args_run = true; break;
            case ARG_LIVE: args_live = true; break;
            case ARG_PPREPL: args_pprepl = true; break;
            case ARG_AST: args_ast = true; break;
            case ARG_ASSEMBLY: flavor = TB_FLAVOR_ASSEMBLY; break;
            case ARG_SYNTAX: args_syntax_only = true; break;
            case ARG_PPTEST: args_test_preproc = true; break;
            case ARG_VERBOSE: args_verbose = true; break;
            case ARG_THINK: args_think = true; break;
            case ARG_BASED: args_use_syslinker = false; break;
            case ARG_TIME: args_time = true; break;
            #if CUIK_ALLOW_THREADS
            case ARG_THREADS: args_threads = calculate_worker_thread_count(); break;
            #else
            case ARG_THREADS:
            printf("warning: Cuik was not built with threading support, this option will be ignored.\n");
            break;
            #endif
            case ARG_DEBUG: args_debug_info = true; break;
            case ARG_EMITIR: args_ir = true; break;
            case ARG_NOLIBC: args_nocrt = true; break;
            case ARG_HELP: {
                print_help(argv[0]);
                return EXIT_SUCCESS;
            }
            default: break;
        }
    }

    done_wit_args:
    #endif

    if (dyn_array_length(args.sources) == 0) {
        fprintf(stderr, "error: no input files!\n");
        return EXIT_FAILURE;
    }

    if (args.time) {
        char output_path_no_ext[FILENAME_MAX];
        cuik_driver_get_output_path(&args, FILENAME_MAX, output_path_no_ext);

        #if 0
        char* perf_output_path = cuikperf_init(FILENAME_MAX, &json_profiler, false);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.json", output_path_no_ext);
        #else
        char* perf_output_path = cuikperf_init(FILENAME_MAX, &spall_profiler, false);
        sprintf_s(perf_output_path, FILENAME_MAX, "%s.spall", output_path_no_ext);
        #endif

        cuikperf_start();
    }

    // spin up worker threads
    #if CUIK_ALLOW_THREADS
    threadpool_t* thread_pool = NULL;
    if (args.threads > 1) {
        if (args.verbose) printf("Starting with %d threads...\n", args.threads);

        thread_pool = threadpool_create(args.threads - 1, 4096);
        ithread_pool = malloc(sizeof(Cuik_IThreadpool));
        *ithread_pool = (Cuik_IThreadpool){
            .user_data = thread_pool,
            .submit = tp_submit,
            .work_one_job = tp_work_one_job
        };
    }
    #endif

    // initialize_opt_passes();
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

            cuik_driver_compile(ithread_pool, &args, true);
        } while (live_compile_watch(&l, &args));
    } else {
        uint64_t start_time = args.verbose ? cuik_time_in_nanos() : 0;
        int status = cuik_driver_compile(ithread_pool, &args, true);

        if (args.verbose) {
            uint64_t now = cuik_time_in_nanos();
            printf("\n\nCUIK: %f ms\n", (now - start_time) / 1000000.0);
        }

        if (status != 0) exit_or_hook(status);
    }

    #if CUIK_ALLOW_THREADS
    if (thread_pool != NULL) {
        threadpool_free(thread_pool);
        thread_pool = NULL;
    }
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

    return 0;
}