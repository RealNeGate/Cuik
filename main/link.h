#include <pool.h>
#include <ctype.h>
#include <tb_linker.h>
#include <ebr.h>

static const char* link_output_name;
static DynArray(const char*) link_default_libs;

static bool gnu_cli(TB_Linker* l, int argc, const char** argv) {
    link_output_name = "a.out";

    bool as_needed = false;

    char tmp[4096];
    for (int i = 0; i < argc; i++) {
        const char* arg = argv[i];

        if (arg[0] == '-') {
            if (arg[1] == '-') {
                if (strcmp(arg, "--as-needed") == 0) {
                    as_needed = true;
                } else if (strcmp(arg, "--no-as-needed") == 0) {
                    as_needed = false;
                } else {
                    fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved option: %s\n", arg);
                }
                continue;
            }

            switch (arg[1]) {
                case 'L': {
                    const char* val = arg[2] ? &arg[2] : argv[++i];
                    tb_linker_add_libpath(l, val);
                    break;
                }
                case 'l': {
                    const char* val = arg[2] ? &arg[2] : argv[++i];
                    tb_linker_append_library(l, val);
                    break;
                }
                case 'm': {
                    const char* val = arg[2] ? &arg[2] : argv[++i];
                    break;
                }
                case 'z': {
                    const char* val = arg[2] ? &arg[2] : argv[++i];
                    break;
                }
                case 'o': {
                    link_output_name = arg[2] ? &arg[2] : argv[++i];
                    break;
                }

                default:
                fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved option: %s\n", arg);
                break;
            }
        } else {
            tb_linker_append_object(l, arg);
        }
    }

    return true;
}

static bool str_case_prefix(const char* l, const char* r, size_t r_len) {
    for (size_t i = 0; i < r_len; i++) {
        if (*l == 0) { return false; }
        if (tolower(*l) != tolower(*r)) { return false; }
        l++, r++;
    }
    return true;
}

static bool msvc_cli(TB_Linker* l, int argc, const char** argv) {
    link_output_name = "a.exe";

    for (int i = 0; i < argc; i++) {
        const char* arg = argv[i];

        if (arg[0] == '-' || arg[0] == '/') {
            arg += 1;

            if (str_case_prefix(arg, "j", 1)) {
                // ignore it, handled earlier
            } else if (str_case_prefix(arg, "out:", 4)) {
                link_output_name = arg + 4;
            } else if (str_case_prefix(arg, "libpath:", 8)) {
                tb_linker_add_libpath(l, arg + 8);
            } else if (str_case_prefix(arg, "entry:", 6)) {
                tb_linker_set_entrypoint(l, arg + 6);
            } else if (str_case_prefix(arg, "defaultlib:", 11)) {
                dyn_array_put(link_default_libs, arg + 11);
            } else {
                fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved option: %s\n", arg);
                // return false;
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

    return true;
}

// pretends to act like link.exe
int run_link(int argc, const char** argv) {
    cuikperf_start("perf.spall");

    // find toolchain details from Cuik
    Cuik_Linker cl = { .toolchain = cuik_toolchain_host() };
    cl.toolchain.ctx = cl.toolchain.init();
    cuiklink_apply_toolchain_libs(&cl, true);

    int status = EXIT_SUCCESS;
    CUIK_TIMED_BLOCK("driver") {
        #ifdef _WIN32
        TB_ExecutableType exe = TB_EXECUTABLE_PE;
        #elif defined(__linux__)
        TB_ExecutableType exe = TB_EXECUTABLE_ELF;
        #else
        #error "Wtf is this machine?"
        #endif

        bool use_threads = false;
        for (int i = 0; i < argc; i++) {
            const char* arg = argv[i];
            if (str_case_prefix(arg, "-j", 2)) {
                use_threads = true;
                break;
            }
        }

        #if CUIK_ALLOW_THREADS
        TPool pool;
        if (use_threads) {
            tpool_init(&pool, 6);
        }
        TB_Linker* l = tb_linker_create(exe, TB_ARCH_X86_64, use_threads ? &pool : NULL);
        #else
        TB_Linker* l = tb_linker_create(exe, TB_ARCH_X86_64, NULL);
        #endif

        dyn_array_for(i, cl.libpaths) {
            tb_linker_add_libpath(l, cl.libpaths[i]);
        }

        dyn_array_for(i, cl.inputs) {
            tb_linker_append_library(l, cl.inputs[i]);
        }

        link_default_libs = dyn_array_create(char*, 32);

        #ifdef _WIN32
        bool status = msvc_cli(l, argc, argv);
        #else
        bool status = gnu_cli(l, argc, argv);
        #endif

        if (dyn_array_length(link_default_libs) > 0) {
            // finish processing the CLI objects, then handle the CLI defaultlibs, then everything else
            tb_linker_barrier(l);
            log_debug("Process CLI /defaultlibs");

            dyn_array_for(i, link_default_libs) {
                log_debug("DEFAUTLIB: %s", link_default_libs[i]);

                size_t path_len = strlen(link_default_libs[i]);
                if ((path_len >= 4 && strcmp(link_default_libs[i] + path_len - 4, ".lib") == 0)) {
                    tb_linker_append_library(l, link_default_libs[i]);
                } else {
                    char path[4096];
                    snprintf(path, 4096, "%s.lib", link_default_libs[i]);
                    tb_linker_append_library(l, path);
                }
            }
        }
        dyn_array_destroy(link_default_libs);

        if (status && !tb_linker_export(l, link_output_name)) {
            cuikperf_region_end();
            status = EXIT_FAILURE;
        }

        #if CUIK_ALLOW_THREADS
        if (use_threads) {
            tpool_destroy(&pool);
        }
        #endif
    }

    ebr_deinit();
    cuikperf_stop();
    return status;
}
