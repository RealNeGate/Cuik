// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#ifdef _WIN32
#define strtok_r(a, b, c) strtok_s(a, b, c)
#define strdup _strdup
#endif

typedef enum ArgType {
    ARG_NONE = 0,

    #define X(name, short, long, has_args, msg) ARG_ ## name,
    #include "driver_args.h"
} ArgType;

typedef struct {
    ArgType type;
    const char* alias; // -a
    const char* name;  // --name
    bool has_arg;
    const char* desc;
} ArgDesc;

typedef struct Arg {
    ArgType key;
    const char* value;
} Arg;

static const char* arg_is_set = "set";

static const ArgDesc arg_descs[] = {
    #define X(name, short, long, has_args, msg) { ARG_ ## name, short, long, has_args, msg },
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
    { "aarch64_windows_msvc",     cuik_target_aarch64,   CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC },
    { "wasm32",                   cuik_target_wasm,      CUIK_SYSTEM_WEB,         CUIK_ENV_GNU  },
};
enum { TARGET_OPTION_COUNT = sizeof(target_options) / sizeof(target_options[0]) };

static void print_help(void) {
    printf("OVERVIEW: Cuik C compiler\n\n");
    printf("USAGE: cuik [options] file...\n\n");
    printf("OPTIONS:\n");

    size_t split = 24;
    for (int i = 0; i < ARG_DESC_COUNT; i++) {
        printf("    ");

        size_t len = 0;
        if (arg_descs[i].alias) {
            len += printf("-%s", arg_descs[i].alias);
        }

        if (arg_descs[i].name) {
            if (len > 0) {
                len += printf(", ");
            }

            len += printf("--%s", arg_descs[i].name);
        }

        if (arg_descs[i].has_arg) {
            len += printf(" <value>");
        }

        size_t round_up = len + (split - (len % split)) % split;
        for (int j = len; j < round_up; j++) printf(" ");
        printf("%s\n", arg_descs[i].desc);
    }
    printf("\n");
}

static const ArgDesc* find_arg_desc(const char* arg, bool is_long_name) {
    if (is_long_name) {
        for (size_t i = 0; i < ARG_DESC_COUNT; i++) {
            const char* n = arg_descs[i].name;
            if (n && strncmp(arg, n, strlen(n)) == 0) {
                return &arg_descs[i];
            }
        }
    } else {
        for (size_t i = 0; i < ARG_DESC_COUNT; i++) {
            const char* n = arg_descs[i].alias;
            if (n && strncmp(arg, n, strlen(n)) == 0) {
                return &arg_descs[i];
            }
        }
    }

    return NULL;
}

#define A(x, y, z) (*out_arg_length = (x), (Arg){ (y), (z) })
static Arg read_arg(int* out_arg_length, int argc, const char* argv[]) {
    // identify argument kind
    const char* first = argv[0];
    if (first[0] != '-') return A(1, ARG_NONE, first);

    // check for equals
    bool is_long_name = (first[1] == '-');
    const char* equals = strchr(first, '=');
    const ArgDesc* desc = find_arg_desc(&first[is_long_name ? 2 : 1], is_long_name);
    if (desc == NULL) {
        // could not find an option
        fprintf(stderr, "error: could not find match for %s\n", first);
        return A(1, ARG_NONE, NULL);
    }

    if (desc->has_arg) return A(1, desc->type, arg_is_set);
    if (equals != NULL) return A(1, desc->type, equals + 1);
    if (argc == 1) return A(2, desc->type, argv[1]);

    fprintf(stderr, "error: no argument after %s\n", first);
    return A(1, ARG_NONE, NULL);
}
#undef A

int cuik_parse_arg(Cuik_CompilerArgs* args, int argc, const char* argv[]) {
    int i;
    Arg arg = read_arg(&i, argc, argv);
    switch (arg.key) {
        case ARG_NONE: {
            if (arg.value == NULL) return i;

            append_input_path(args, arg.value);
            break;
        }
        case ARG_DEFINE: {
            dyn_array_put(args->defines, arg.value);
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

                dyn_array_put(args->includes, newstr);
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
                dyn_array_put(args->libraries, a);
                a = strtok_r(NULL, ",", &ctx);
            }
            break;
        }
        case ARG_TARGET: {
            bool success = false;
            for (size_t i = 0; i < TARGET_OPTION_COUNT; i++) {
                if (strcmp(arg.value, target_options[i].key) == 0) {
                    TargetOption* o = &target_options[i];
                    args->target = o->target(o->system, o->env);
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
            if (strcmp(arg.value, "c11")) args->verbose = CUIK_VERSION_C11;
            else if (strcmp(arg.value, "c23")) args->verbose = CUIK_VERSION_C23;
            else if (strcmp(arg.value, "glsl")) args->verbose = CUIK_VERSION_GLSL;
            else {
                fprintf(stderr, "unknown compiler version: %s\n", arg.value);
                fprintf(stderr, "supported languages: c11, c23, glsl\n");
            }
            break;
        }
        case ARG_OUTPUT: args->output_name = arg.value; break;
        case ARG_OBJECT: args->flavor = TB_FLAVOR_OBJECT; break;
        case ARG_PP: args->preprocess = true; break;
        case ARG_RUN: args->run = true; break;
        case ARG_LIVE: args->live = true; break;
        case ARG_PPREPL: args->pprepl = true; break;
        case ARG_AST: args->ast = true; break;
        case ARG_ASSEMBLY: args->flavor = TB_FLAVOR_ASSEMBLY; break;
        case ARG_SYNTAX: args->syntax_only = true; break;
        case ARG_VERBOSE: args->verbose = true; break;
        case ARG_THINK: args->think = true; break;
        case ARG_BASED: args->based = true; break;
        case ARG_TIME: args->time = true; break;
        case ARG_DEBUG: args->debug_info = true; break;
        case ARG_EMITIR: args->ir = true; break;
        case ARG_NOLIBC: args->nocrt = true; break;
        case ARG_HELP: {
            print_help();
            return EXIT_SUCCESS;
        }

        case ARG_THREADS:
        #if CUIK_ALLOW_THREADS
        args->threads = calculate_worker_thread_count();
        #else
        printf("warning: Cuik was not built with threading support, this option will be ignored.\n");
        #endif
        break;

        default: break;
    }

    return i;
}

void cuik_parse_args(Cuik_CompilerArgs* args, int argc, const char* argv[]) {
    for (int i = 0; i < argc;) {
        i += cuik_parse_arg(args, argc - i, argv + i);
    }
}

