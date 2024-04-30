// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#ifdef _WIN32
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

typedef enum ArgType {
    ARG_NONE = 0,

    #define X(name, short, has_args, msg) ARG_ ## name,
    #include "driver_args.h"
} ArgType;

typedef struct {
    const char* alias; // -a
    bool has_arg;
    const char* desc;
} ArgDesc;

static const char* arg_is_set = "set";

static const ArgDesc arg_descs[] = {
    #define X(name, short, has_args, msg) [ARG_ ## name] = { short, has_args, msg },
    #include "driver_args.h"
};
enum { ARG_DESC_COUNT = sizeof(arg_descs) / sizeof(arg_descs[0]) };

typedef struct TargetOption {
    const char* key;

    Cuik_Target* (*target)(Cuik_System, Cuik_Environment);
    Cuik_System system;
    Cuik_Environment env;
    Cuik_Toolchain (*toolchain)(void);
} TargetOption;

static TargetOption target_options[] = {
    { "x64_windows_msvc",         cuik_target_x64,       CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC, cuik_toolchain_msvc   },
    { "x64_macos_gnu",            cuik_target_x64,       CUIK_SYSTEM_MACOS,       CUIK_ENV_GNU,  cuik_toolchain_darwin },
    { "x64_linux_gnu",            cuik_target_x64,       CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    },
    { "aarch64_windows_msvc",     cuik_target_aarch64,   CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC, cuik_toolchain_msvc   },
    { "aarch64_linux_gnu",        cuik_target_aarch64,   CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    },
    { "mips32_linux_gnu",         cuik_target_mips32,    CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    },
    { "mips64_linux_gnu",         cuik_target_mips64,    CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    },
    { "wasm32",                   cuik_target_wasm32,    CUIK_SYSTEM_WEB,         CUIK_ENV_WEB,  NULL                  },
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

struct Cuik_Arguments {
    TB_Arena* arena;

    // _[0] is for non-flag arguments
    size_t count[ARG_DESC_COUNT];
    Cuik_Arg* _[ARG_DESC_COUNT];
};

#define FOR_ARGS(a, arg_i) for (Cuik_Arg* a = args->_[arg_i]; a; a = a->prev)

static void print_help(void) {
    printf("OVERVIEW: Cuik C compiler (built on " __DATE__ ")\n\n");
    printf("USAGE: cuik [options] file...\n\n");
    printf("OPTIONS:\n");

    size_t split = 24;
    for (int i = 1; i < ARG_DESC_COUNT; i++) {
        printf("    ");

        size_t len = printf("-%s", arg_descs[i].alias);
        if (arg_descs[i].has_arg) {
            len += printf(" <value>");
        }

        size_t round_up = len + (split - (len % split)) % split;
        for (int j = len; j < round_up; j++) printf(" ");
        printf("%s\n", arg_descs[i].desc);
    }
    printf("\n");
}

static size_t find_arg_desc(const char* arg) {
    for (size_t i = 1; i < ARG_DESC_COUNT; i++) {
        const char* n = arg_descs[i].alias;
        if (n && strncmp(arg, n, strlen(n)) == 0) {
            return i;
        }
    }

    return 0;
}

static Cuik_Arg* insert_arg(Cuik_Arguments* restrict args, int slot) {
    Cuik_Arg* new_arg = TB_ARENA_ALLOC(args->arena, Cuik_Arg);
    new_arg->prev = args->_[slot];
    args->_[slot] = new_arg;
    args->count[slot] += 1;
    return new_arg;
}

CUIK_API Cuik_Arguments* cuik_alloc_args(void) {
    return cuik_calloc(1, sizeof(Cuik_Arguments));
}

CUIK_API void cuik_free_args(Cuik_Arguments* args) {
    tb_arena_destroy(args->arena);
    cuik_free(args);
}

CUIK_API void cuik_parse_args(Cuik_Arguments* restrict args, int argc, const char* argv[]) {
    for (int i = 0; i < argc; i++) {
        const char* first = argv[i];

        // these are necessary
        if (strcmp("-version", first) == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
            fprintf(stderr, "did you mean: '--version'?\n");
            continue;
        }
        if (strcmp("--version", first) == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
            fprintf(stderr, "did you mean: '-version'?\n");
            continue;
        }

        // non-flag argument
        if (first[0] != '-') {
            insert_arg(args, 0)->value = first;
            continue;
        }

        // flags
        size_t type = find_arg_desc(first + 1);
        if (type == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
            continue;
        }

        size_t alias_len = strlen(arg_descs[type].alias);
        const ArgDesc* desc = &arg_descs[type];
        if (desc->has_arg) {
            if (first[alias_len + 1] != 0) {
                insert_arg(args, type)->value = first + alias_len + 1;
            } else if (i + 1 >= argc) {
                fprintf(stderr, "\x1b[31merror\x1b[0m: expected argument after %s\n", first);
                insert_arg(args, type)->value = arg_is_set;
                continue;
            } else {
                insert_arg(args, type)->value = argv[i + 1];
                i += 1;
            }
        } else {
            insert_arg(args, type)->value = arg_is_set;
        }
    }
}

CUIK_API bool cuik_parse_driver_args(Cuik_DriverArgs* comp_args, int argc, const char* argv[]) {
    Cuik_Arguments* args = cuik_alloc_args();
    args->arena = tb_arena_create(TB_ARENA_SMALL_CHUNK_SIZE);

    cuik_parse_args(args, argc, argv);

    bool result = cuik_args_to_driver(comp_args, args);
    cuik_free_args(args);
    return result;
}

#define TOGGLE(a, b) if (args->_[a]) { comp_args->b = true; }
CUIK_API bool cuik_args_to_driver(Cuik_DriverArgs* comp_args, Cuik_Arguments* restrict args) {
    if (args->_[ARG_HELP]) {
        print_help();
        return false;
    }

    Cuik_Arg* target = args->_[ARG_TARGET];
    if (target) {
        bool success = false;
        for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
            if (strcmp(target->value, target_options[i].key) == 0) {
                TargetOption* o = &target_options[i];
                comp_args->target = o->target(o->system, o->env);
                if (o->toolchain) {
                    comp_args->toolchain = o->toolchain();
                } else {
                    comp_args->toolchain = (Cuik_Toolchain){ 0 };
                }
                success = true;
                break;
            }
        }

        if (!success) {
            if (target->value != arg_is_set) {
                fprintf(stderr, "unknown target: %s\n", target->value);
            }
            fprintf(stderr, "Supported targets:\n");
            for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
                fprintf(stderr, "    %s\n", target_options[i].key);
            }
            fprintf(stderr, "\n");
            return false;
        }
    }

    // initialize toolchain
    if (comp_args->toolchain.init) {
        comp_args->toolchain.ctx = comp_args->toolchain.init();
    } else {
        comp_args->toolchain.ctx = NULL;
    }

    if (args->_[ARG_OUTPUT]) {
        comp_args->output_name = cuik_strdup(args->_[ARG_OUTPUT]->value);
    }

    FOR_ARGS(a, 0) {
        append_input_path(comp_args, a->value);
    }

    FOR_ARGS(a, ARG_DEFINE) {
        dyn_array_put(comp_args->defines, cuik_strdup(a->value));
    }

    FOR_ARGS(a, ARG_INCLUDE) {
        // resolve a fullpath
        Cuik_Path* newstr = cuik_malloc(sizeof(Cuik_Path));
        if (cuikfs_canonicalize(newstr, a->value, comp_args->toolchain.case_insensitive)) {
            size_t end = newstr->length;
            if (newstr->data[end - 1] != '\\' && newstr->data[end - 1] != '/') {
                assert(end+2 < FILENAME_MAX);
                newstr->data[end] = CUIK_PATH_SLASH_SEP;
                newstr->data[end+1] = '\0';
            }

            dyn_array_put(comp_args->includes, newstr);
        } else {
            fprintf(stderr, "error: could not resolve include: %s\n", a->value);
        }
    }

    FOR_ARGS(a, ARG_LIBDIR) {
        Cuik_Path* p = cuik_malloc(sizeof(Cuik_Path));
        cuik_path_set(p, a->value);
        dyn_array_put(comp_args->libpaths, p);
    }

    FOR_ARGS(a, ARG_LIB) {
        char* newstr = cuik_strdup(a->value);

        char* ctx;
        char* arg = strtok_r(newstr, ",", &ctx);
        while (arg != NULL) {
            Cuik_Path* p = cuik_malloc(sizeof(Cuik_Path));
            cuik_path_set(p, arg);

            dyn_array_put(comp_args->libraries, p);
            arg = strtok_r(NULL, ",", &ctx);
        }
    }

    Cuik_Arg* lang = args->_[ARG_LANG];
    if (lang) {
        if (strcmp(lang->value, "c11") == 0) comp_args->version = CUIK_VERSION_C11;
        else if (strcmp(lang->value, "c23") == 0) comp_args->version = CUIK_VERSION_C23;
        else if (strcmp(lang->value, "glsl") == 0) comp_args->version = CUIK_VERSION_GLSL;
        else {
            fprintf(stderr, "unknown compiler version: %s\n", lang->value);
            fprintf(stderr, "supported languages: c11, c23, glsl\n");
            return false;
        }
    }

    Cuik_Arg* subsystem = args->_[ARG_SUBSYSTEM];
    if (subsystem) {
        if (strcmp(subsystem->value, "windows") == 0) comp_args->subsystem = TB_WIN_SUBSYSTEM_WINDOWS;
        else if (strcmp(subsystem->value, "console") == 0) comp_args->subsystem = TB_WIN_SUBSYSTEM_CONSOLE;
        else if (strcmp(subsystem->value, "efi") == 0) comp_args->subsystem = TB_WIN_SUBSYSTEM_EFI_APP;
        else {
            fprintf(stderr, "unknown subsystem: %s\n", subsystem->value);
            fprintf(stderr, "supported: windows, console, efi\n");
            return false;
        }
    }

    Cuik_Arg* entry = args->_[ARG_ENTRY];
    if (entry) {
        comp_args->entrypoint = entry->value;
    }

    Cuik_Arg* mf = args->_[ARG_DEPFILE];
    if (mf) {
        comp_args->dep_file = mf->value;
    }

    Cuik_Arg* threads = args->_[ARG_THREADS];
    if (threads) {
        if (threads->value != arg_is_set) {
            comp_args->threads = atoi(threads->value);
        } else {
            #ifdef CUIK_ALLOW_THREADS
            #ifdef _WIN32
            SYSTEM_INFO sysinfo;
            GetSystemInfo(&sysinfo);

            int n = sysinfo.dwNumberOfProcessors;
            comp_args->threads = (n < 1 ? 1 : n);
            #else
            assert(0 && "TODO(NeGate): implement core count detection on this platform");
            comp_args->threads = 1;
            #endif
            #else
            printf("warning: Cuik was not built with threading support, this option will be ignored.\n");
            #endif
        }
    }

    #ifdef CUIK_USE_TB
    if (args->_[ARG_OBJECT]) comp_args->flavor = TB_FLAVOR_OBJECT;
    if (args->_[ARG_ASSEMBLY]) comp_args->assembly = true;
    #endif

    TOGGLE(ARG_DEPS, write_deps);
    TOGGLE(ARG_OPTLVL, optimize);
    TOGGLE(ARG_PP, preprocess);
    TOGGLE(ARG_PPTEST, test_preproc);
    TOGGLE(ARG_RUN, run);
    TOGGLE(ARG_LIVE, live);
    TOGGLE(ARG_AST, ast);
    TOGGLE(ARG_SYNTAX, syntax_only);
    TOGGLE(ARG_VERBOSE, verbose);
    TOGGLE(ARG_THINK, think);
    TOGGLE(ARG_BASED, based);
    TOGGLE(ARG_TIME, time);
    TOGGLE(ARG_DEBUG, debug_info);
    TOGGLE(ARG_EMITIR, emit_ir);
    TOGGLE(ARG_NOLIBC, nocrt);

    if (comp_args->verbose) {
        comp_args->toolchain.print_verbose(comp_args->toolchain.ctx, comp_args);
    }

    return true;
}
#undef TOGGLE
