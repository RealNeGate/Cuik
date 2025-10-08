// Microsoft's definition of strtok_s actually matches
// strtok_r on POSIX, not strtok_s on C11... tf
#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

#ifdef _WIN32
#define strtok_r(a, b, c) strtok_s(a, b, c)
#endif

#ifndef TB_NO_THREADS
static once_flag arg_global_init = ONCE_FLAG_INIT;
#else
static bool arg_global_init;
#endif

typedef struct {
    const char* key;
    void* val;
} CLIOptVal;

typedef enum {
    ARG_INT,
    ARG_ENUM,
    ARG_ENUM_VAL,
    ARG_BOOL,
    ARG_STR,
    ARG_STR_LIST,
    ARG_PATH,
    ARG_PATH_LIST,
    ARG_GROUP,
} ArgType;

typedef struct {
    const char* short_name;
    const char* long_name;

    ArgType type;
    const char* desc;

    size_t offset;
} Option;

#define X(type, short_name, long_name, member, desc) { short_name, long_name, ARG_ ## type, desc, offsetof(Cuik_DriverArgs, member) }
#define Y(type, short_name, long_name, value,  desc) { short_name, long_name, ARG_ ## type, desc, value }
#define Z(short_name) { short_name, .type = ARG_GROUP }
static Option options[] = {
    X(PATH_LIST,NULL,         NULL, sources,           NULL),
    X(BOOL,     "V",          NULL, verbose,           "print verbose messages"),
    X(ENUM,     "phase",      NULL, phase,                  "define which phase to stop at"),
    Y(ENUM_VAL, "preprocess", NULL, CUIK_PHASE_PREPROCESS,  "preprocessor"),
    Y(ENUM_VAL, "syntax",     NULL, CUIK_PHASE_SYNTAX,      "syntax & type-checking"),
    Y(ENUM_VAL, "ir",         NULL, CUIK_PHASE_IR_GEN,      "IR generation (and optimizer if enabled)"),
    Z("preprocessor"),
    X(STR_LIST, "D",          NULL, defines,           "defines a macro before compiling"),
    X(STR_LIST, "U",          NULL, undefs,            "undefines a macro before compiling"),
    X(PATH_LIST,"I",          NULL, includes,          "add directory to the include searches"),
    X(BOOL,     "Pp",         NULL, test_preproc,      "test preprocessor"),
    X(BOOL,     "P",          NULL, preprocess,        "print preprocessor output to stdout"),
    X(STR,      "MD",         NULL, write_deps,        "write header dependencies"),
    X(STR,      "MF",         NULL, dep_file,          "write depout from -MD into a file"),
    Z("parser"),
    X(ENUM,     "lang",       NULL, version,           "choose the language"),
    Y(ENUM_VAL, "c11",        NULL, CUIK_VERSION_C11,  ""),
    Y(ENUM_VAL, "c23",        NULL, CUIK_VERSION_C23,  ""),
    Y(ENUM_VAL, "glsl",       NULL, CUIK_VERSION_GLSL, ""),
    X(BOOL,     "ast",        NULL, ast,               "print AST into stdout"),
    X(BOOL,     "xe",         NULL, syntax_only,       "type check only"),
    Z("backend"),
    X(BOOL,     "O",          NULL, optimize,          "enable the optimizer"),
    X(BOOL,     "emit-ir",    NULL, emit_ir,           "print IR into stdout"),
    X(STR,      "o",          NULL, output_name,       "set the output filepath"),
    X(BOOL,     "c",          NULL, flavor,            "output object file"),
    X(BOOL,     "S",          NULL, assembly,          "output assembly to stdout"),
    X(BOOL,     "g",          NULL, debug_info,        "compile with debug information"),
    X(BOOL,     "nochkstk",   NULL, nochkstk,          "disable buffer checks (think of MSVC's /GS-)"),
    X(STR,      "e",          NULL, entrypoint,        "entrypoint"),
    Z("linker"),
    X(BOOL,     "based",      NULL, based,             "use the TB linker (EXPERIMENTAL)"),
    X(INT,      "j",          NULL, threads,           "enabled multithreaded compilation"),
    X(BOOL,     "T",          NULL, time,              "profile the compile times (outputs spall)"),
    X(BOOL,     "t",          NULL, time_report,       "time report of the passes"),
    X(ENUM,     "subsystem",  NULL, subsystem,         "set windows subsystem (windows only... of course)"),
    Y(ENUM_VAL, "console",    NULL, TB_WIN_SUBSYSTEM_CONSOLE,  ""),
    Y(ENUM_VAL, "windows",    NULL, TB_WIN_SUBSYSTEM_WINDOWS,  ""),
    Y(ENUM_VAL, "efi-app",    NULL, TB_WIN_SUBSYSTEM_EFI_APP,  ""),
};
#undef Z
#undef Y
#undef X
enum { OPTION_COUNT = sizeof(options) / sizeof(options[0]) };

typedef struct TargetOption {
    Cuik_Target* (*target)(Cuik_System, Cuik_Environment);
    Cuik_System system;
    Cuik_Environment env;
    Cuik_Toolchain (*toolchain)(void);
} TargetOption;

static CLIOptVal CLI_STR;
static CLIOptVal CLI_NUM;
static CLIOptVal CLI_OPT_NUM;
static CLIOptVal TARGET_ENUMS[] = {
    { "x64_windows_msvc",         &(TargetOption){ cuik_target_x64,       CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC, cuik_toolchain_msvc   } },
    { "x64_macos_gnu",            &(TargetOption){ cuik_target_x64,       CUIK_SYSTEM_MACOS,       CUIK_ENV_GNU,  cuik_toolchain_darwin } },
    { "x64_linux_gnu",            &(TargetOption){ cuik_target_x64,       CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    } },
    { "aarch64_windows_msvc",     &(TargetOption){ cuik_target_aarch64,   CUIK_SYSTEM_WINDOWS,     CUIK_ENV_MSVC, cuik_toolchain_msvc   } },
    { "aarch64_linux_gnu",        &(TargetOption){ cuik_target_aarch64,   CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    } },
    { "mips32_linux_gnu",         &(TargetOption){ cuik_target_mips32,    CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    } },
    { "mips64_linux_gnu",         &(TargetOption){ cuik_target_mips64,    CUIK_SYSTEM_LINUX,       CUIK_ENV_GNU,  cuik_toolchain_gnu    } },
    { "wasm32",                   &(TargetOption){ cuik_target_wasm32,    CUIK_SYSTEM_WEB,         CUIK_ENV_WEB,  NULL                  } },
    {}
};

typedef struct {
    const char* key;
    int val;
    bool is_short;
} ArgEntry;

static const char* arg_is_set = "set";
static ArgEntry arg_ht[256];

static uint32_t arg_hash(size_t len, const char* str) {
    if (len > 8) {
        len = 8;
    }

    uint64_t x = 0;
    for (int i = 0; i < len && *str; i++) {
        x <<= 8, x |= (uint8_t) *str++;
    }

    // scramble the first 8 bytes
    x ^= x >> 32;
    x *= 0xd6e8feb86659fd93U;
    x ^= x >> 32;
    x *= 0xd6e8feb86659fd93U;
    x ^= x >> 32;
    return x;
}

static bool arg_eq(size_t len, const char* a, const char* b) {
    for (size_t i = 0; i < len && *a && *b; a++, b++) {
        if (*a != *b) {
            return false;
        }
    }
    return true;
}

static ArgEntry* arg_get_ht(size_t len, const char* key) {
    uint32_t first = arg_hash(len, key) % COUNTOF(arg_ht), i = first;
    do {
        if (arg_ht[i].key == NULL) {
            return NULL;
        } else if (arg_eq(len, arg_ht[i].key, key)) {
            return &arg_ht[i];
        }
        i = (i + 1) % COUNTOF(arg_ht);
    } while (first != i);
    return NULL;
}

static void arg_put_ht(const char* key, int val, bool is_short) {
    if (key == NULL) {
        return;
    }

    uint32_t first = arg_hash(SIZE_MAX, key) % COUNTOF(arg_ht), i = first;
    do {
        if (arg_ht[i].key == NULL) {
            arg_ht[i].key = key;
            arg_ht[i].val = val;
            arg_ht[i].is_short = is_short;
            return;
        }
        // shouldn't any duplicates
        assert(!arg_eq(SIZE_MAX, arg_ht[i].key, key));
        i = (i + 1) % COUNTOF(arg_ht);
    } while (first != i);

    // failed to hash the arg table? how?
    abort();
}

static void arg_construct_ht(void) {
    for (size_t i = 1; i < OPTION_COUNT; i++) {
        if (options[i].type == ARG_ENUM_VAL) {
            arg_put_ht(options[i].short_name, i, false);
        } else if (options[i].type != ARG_GROUP) {
            arg_put_ht(options[i].short_name, i, true);
            arg_put_ht(options[i].long_name,  i, false);
        }
    }
}

static void print_help(void) {
    printf("OVERVIEW: Cuik C compiler (built on " __DATE__ ")\n\n");
    printf("USAGE: cuik [options] file...\n\n");
    printf("OPTIONS:\n\n");

    size_t split = 24;
    int last_enum = 0;
    for (size_t i = 1; i < OPTION_COUNT; i++) {
        if (options[i].type == ARG_GROUP) {
            printf("\n  %s:\n", options[i].short_name);
            continue;
        }

        printf("    ");

        size_t len;
        if (options[i].type == ARG_ENUM_VAL) {
            int j = 0;
            for (; options[last_enum].short_name[j]; j++) {
                printf(" ");
            }
            len = j + printf("  %s", options[i].short_name);
        } else {
            if (options[i].type == ARG_ENUM) {
                last_enum = i;
            }

            len = printf("-%s", options[i].short_name);
            switch (options[i].type) {
                case ARG_PATH: case ARG_PATH_LIST:
                len += printf(" <filename>");
                break;

                case ARG_ENUM: case ARG_STR: case ARG_STR_LIST:
                len += printf(" <value>");
                break;

                case ARG_INT:
                len += printf(" <number>");
                break;

                default:
                break;
            }
        }

        size_t round_up = len + (split - (len % split)) % split;
        for (int j = len; j < round_up; j++) printf(" ");
        printf("%s\n", options[i].desc);
    }

    /*size_t split = 24;
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
    printf("\n");*/
}

CUIK_API bool cuik_parse_driver_args(Cuik_DriverArgs* comp_args, int argc, const char* argv[]) {
    /*Cuik_Arguments* args = cuik_alloc_args();
    tb_arena_create(&args->arena, "Args");

    cuik_parse_args(args, argc, argv);

    bool result = cuik_args_to_driver(comp_args, args);
    cuik_free_args(args);
    return result;*/

    #ifndef TB_NO_THREADS
    call_once(&arg_global_init, arg_construct_ht);
    #else
    if (!arg_global_init) {
        arg_global_init = true;
        arg_construct_ht();
    }
    #endif

    int i = 0;
    char* dst_base = (char*) comp_args;
    while (i < argc) {
        const char* first = argv[i++];

        // these are necessary
        if (strcmp("-version", first) == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
            fprintf(stderr, "did you mean: '--version'?\n");
            continue;
        } else if (strcmp("--version", first) == 0) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
            fprintf(stderr, "did you mean: '-version'?\n");
            continue;
        } else if (strcmp("-h", first) == 0 || strcmp("--help", first) == 0) {
            print_help();
            continue;
        }

        Option* opt = &options[0];
        size_t len = 1;
        if (first[0] == '-') {
            // flags
            bool is_short = true;
            if (first[1] == '-') {
                is_short = false;
                len++;
            }

            // truncate after the equals or numbers (if one exists)
            while (first[1+len] && first[1+len] != '=' && (first[1+len] < '0' || first[1+len] > '9')) {
                len++;
            }

            ArgEntry* e = arg_get_ht(len, first+1);
            if (e == NULL || e->is_short != is_short || options[e->val].type == ARG_ENUM_VAL) {
                fprintf(stderr, "\x1b[31merror\x1b[0m: could not find match for %s\n", first);
                return false;
            }

            opt = &options[e->val];
        }

        void* dst = &dst_base[opt->offset];
        const char* oldstr = NULL;
        if (opt->type != ARG_BOOL) {
            int short_len = opt->short_name ? strlen(opt->short_name) : 0;
            if (short_len == 1 && first[2] != 0) {
                // short names for args might be directly followed by the
                // arg value (e.g. "-j5")
                oldstr = first+2;
            } else if (opt == &options[0]) {
                oldstr = first;
            } else {
                if (i >= argc || argv[i][0] == '-') {
                    fprintf(stderr, "\x1b[31merror\x1b[0m: expected argument after %s\n", first);
                    return false;
                } else {
                    oldstr = argv[i++];
                }
            }
        }

        if (opt->type == ARG_BOOL) {
            // HACK(NeGate): i'll add the missing CLI options soon
            if (opt->short_name[0] == 'c' && opt->short_name[1] == 0) {
                *(int*) dst = TB_FLAVOR_OBJECT;
            } else {
                *(bool*) dst = true;
            }
        } else if (opt->type == ARG_ENUM) {
            for (Option* kid = opt+1; kid != &options[OPTION_COUNT] && kid->type == ARG_ENUM_VAL; kid++) {
                if (strcmp(kid->short_name, oldstr) == 0) {
                    *(int*) dst = kid->offset;
                    goto skip;
                }
            }

            fprintf(stderr, "\x1b[31merror\x1b[0m: that's not valid (TODO error), got %s\n", oldstr);
            return false;

            skip:;
        } else if (opt->type == ARG_INT) {
            *(int*) dst = atoi(oldstr);
        } else if (opt->type == ARG_STR) {
            *(char**) dst = cuik_strdup(oldstr);
        } else if (opt->type == ARG_STR_LIST) {
            DynArray(char*)* list = (DynArray(char*)*) dst;
            dyn_array_put(*list, cuik_strdup(oldstr));
        } else if (opt->type == ARG_PATH) {
            Cuik_Path* newstr = cuik_malloc(sizeof(Cuik_Path));
            if (cuikfs_canonicalize(newstr, oldstr, comp_args->toolchain.case_insensitive)) {
                *(Cuik_Path**) dst = newstr;
            } else {
                fprintf(stderr, "\x1b[31merror\x1b[0m: invalid file! %s\n", oldstr);
                return false;
            }
        } else {
            append_input_path((DynArray(Cuik_Path*)*) dst, oldstr, comp_args->toolchain.case_insensitive);
        }
    }

    // split the library and sources from each other
    for (size_t i = 0; i < dyn_array_length(comp_args->sources);) {
        Cuik_Path* path = comp_args->sources[i];
        if (cuik_path_has_ext(path, "a") || cuik_path_has_ext(path, "lib")) {
            dyn_array_remove(comp_args->sources, i);
            dyn_array_put(comp_args->libraries, path);
        } else {
            i++;
        }
    }

    // initialize toolchain
    if (comp_args->toolchain.init) {
        comp_args->toolchain.ctx = comp_args->toolchain.init();
    } else {
        comp_args->toolchain.ctx = NULL;
    }

    return true;
}

#define TOGGLE(a, b) if (args->_[a]) { comp_args->b = true; }
CUIK_API bool cuik_args_to_driver(Cuik_DriverArgs* comp_args) {
    #if 0
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

            int n = sysinfo.dwNumberOfProcessors - 2;
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

    #ifdef CONFIG_HAS_TB
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
    TOGGLE(ARG_TIMEREPORT, time_report);
    TOGGLE(ARG_DEBUG, debug_info);
    TOGGLE(ARG_EMITIR, emit_ir);
    TOGGLE(ARG_NOLIBC, nocrt);
    TOGGLE(ARG_NOCHKSTK, nochkstk);

    if (comp_args->verbose) {
        comp_args->toolchain.print_verbose(comp_args->toolchain.ctx, comp_args);

        printf("User Includes:\n");
        dyn_array_for(i, comp_args->includes) {
            printf("  %s\n", comp_args->includes[i]->data);
        }
    }

    return true;
    #endif

    return false;
}
#undef TOGGLE
