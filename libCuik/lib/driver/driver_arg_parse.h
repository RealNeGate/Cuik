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
} TargetOption;

static TargetOption target_options[] = {
    { "x64_windows_msvc",         cuik_target_x64,       CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC },
    { "x64_macos_gnu",            cuik_target_x64,       CUIK_SYSTEM_MACOS,       CUIK_ENV_GNU  },
    { "x64_linux_gnu",            cuik_target_x64,       CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU  },
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

struct Cuik_Arguments {
    Arena arena;

    // _[0] is for non-flag arguments
    Cuik_Arg* _[ARG_DESC_COUNT];
};

#define FOR_ARGS(a, arg_i) for (Cuik_Arg* a = args->_[arg_i]; a; a = a->prev)

static void print_help(void) {
    printf("OVERVIEW: Cuik C compiler\n\n");
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
    Cuik_Arg* new_arg = ARENA_ALLOC(&args->arena, Cuik_Arg);
    new_arg->prev = args->_[slot];
    args->_[slot] = new_arg;
    return new_arg;
}

CUIK_API Cuik_Arguments* cuik_alloc_args(void) {
    return cuik_calloc(1, sizeof(Cuik_Arguments));
}

CUIK_API void cuik_free_args(Cuik_Arguments* args) {
    cuik_free(args);
}

CUIK_API void cuik_parse_args(Cuik_Arguments* restrict args, int argc, const char* argv[]) {
    for (int i = 0; i < argc; i++) {
        const char* first = argv[i];

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
        char* newstr = cuik_malloc(FILENAME_MAX);
        if (cuik_canonicalize_path(newstr, a->value)) {
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

            dyn_array_put(comp_args->includes, newstr);
        } else {
            fprintf(stderr, "error: could not resolve include: %s\n", a->value);
        }
    }

    FOR_ARGS(a, ARG_LIB) {
        char* newstr = cuik_strdup(a->value);

        char* ctx;
        char* a = strtok_r(newstr, ",", &ctx);
        while (a != NULL) {
            dyn_array_put(comp_args->libraries, a);
            a = strtok_r(NULL, ",", &ctx);
        }
    }

    Cuik_Arg* lang = args->_[ARG_LANG];
    if (lang) {
        if (strcmp(lang->value, "c11")) comp_args->version = CUIK_VERSION_C11;
        else if (strcmp(lang->value, "c23")) comp_args->version = CUIK_VERSION_C23;
        else if (strcmp(lang->value, "glsl")) comp_args->version = CUIK_VERSION_GLSL;
        else {
            fprintf(stderr, "unknown compiler version: %s\n", lang->value);
            fprintf(stderr, "supported languages: c11, c23, glsl\n");
        }
    }

    Cuik_Arg* target = args->_[ARG_TARGET];
    if (target) {
        bool success = false;
        for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
            if (strcmp(target->value, target_options[i].key) == 0) {
                TargetOption* o = &target_options[i];
                comp_args->target = o->target(o->system, o->env);
                success = true;
                break;
            }
        }

        if (!success) {
            fprintf(stderr, "unknown target: %s\n", target->value);
            fprintf(stderr, "Supported targets:\n");
            for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
                fprintf(stderr, "    %s\n", target_options[i].key);
            }
            fprintf(stderr, "\n");
        }
    }

    Cuik_Arg* threads = args->_[ARG_THREADS];
    if (threads) {
        #ifdef CUIK_ALLOW_THREADS
        #ifdef _WIN32
        SYSTEM_INFO sysinfo;
        GetSystemInfo(&sysinfo);

        int n = sysinfo.dwNumberOfProcessors - 1;
        comp_args->threads = (n < 1 ? 1 : n);
        #else
        assert(0 && "TODO(NeGate): implement core count detection on this platform");
        comp_args->threads = 1;
        #endif
        #else
        printf("warning: Cuik was not built with threading support, this option will be ignored.\n");
        #endif
    }

    if (args->_[ARG_OBJECT]) comp_args->flavor = TB_FLAVOR_OBJECT;
    if (args->_[ARG_ASSEMBLY]) comp_args->flavor = TB_FLAVOR_ASSEMBLY;

    if (args->_[ARG_OPTLVL]) {
        comp_args->opt_level = atoi(args->_[ARG_OPTLVL]->value);
    }

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
    return true;
}
#undef TOGGLE
