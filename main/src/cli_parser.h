// if an argument is set and has no args this is put
static const char* program_name = NULL;
static const char* arg_is_set = "set";

typedef enum ArgType {
    ARG_NONE = 0,

    ARG_HELP,
    ARG_VERBOSE,
    ARG_DEFINE,
    ARG_UNDEF,
    ARG_INCLUDE,
    ARG_PREPROC,
    ARG_PPLOC,
    ARG_AST,
    ARG_SYNTAX,
    ARG_EMITIR,
    ARG_O0,
    ARG_O1,
    ARG_OUTPUT,
    ARG_OBJECT,
    ARG_DEBUG,
    ARG_NOLIBC,
    ARG_LIB,
    ARG_BASED,
    ARG_TARGET,
    ARG_THREADS,
    ARG_THINK,
    ARG_TIME,
    ARG_RUN,
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

static const ArgDesc arg_descs[] = {
    { ARG_HELP,    "h",        NULL,      false, "print help" },
    { ARG_VERBOSE, "V",        NULL,      false, "print verbose messages" },
    // preprocessor
    { ARG_DEFINE,  "D",        "define",  true,  "defines a macro before compiling" },
    { ARG_UNDEF,   "U",        "undef",   true,  "undefines a macro before compiling" },
    { ARG_INCLUDE, "I",        "include", true,  "add directory to the include searches" },
    { ARG_PREPROC, "P",        NULL,      false, "print preprocessor output to stdout" },
    { ARG_PPLOC,   "pploc",    NULL,      false, "count the number of preprocessor lines of code" },
    // parser
    { ARG_AST,     "ast",      NULL,      false, "" },
    { ARG_SYNTAX,  "xe",       NULL,      false, "type check only" },
    // optimizer
    { ARG_O0,      "O0",       NULL,      false, "no optimizations" },
    { ARG_O1,      "O1",       NULL,      false, "non-aggresive optimizations" },
    // backend
    { ARG_EMITIR,  "emit-ir",  NULL,      false, "print IR into stdout" },
    { ARG_OUTPUT,  "o",        NULL,      true,  "set the output filepath" },
    { ARG_OBJECT,  "c",        NULL,      false, "output object file" },
    { ARG_DEBUG,   "g",        NULL,      false, "compile with debug information" },
    // linker
    { ARG_NOLIBC,  "nostdlib", NULL,      false, "don't include and link against the default CRT" },
    { ARG_LIB,     "l",        NULL,      false, "add library name to the linking" },
    { ARG_BASED,   "based",    NULL,      false, "use the TB linker (EXPERIMENTAL)" },
    // misc
    { ARG_TARGET,  "target",   NULL,      false, "change the target system and arch" },
    { ARG_THREADS, "threads",  NULL,      false, "enabled multithreaded compilation" },
    { ARG_TIME,    "T",        NULL,      false, "profile the compile times" },
    { ARG_THINK,   "think",    NULL,      false, "aids in thinking about serious problems" },
    // run
    { ARG_RUN,     "r",        NULL,      false, "run the executable" },
};
enum { ARG_DESC_COUNT = sizeof(arg_descs) / sizeof(arg_descs[0]) };

// returns argument, *out_param is the name of the parameter
static Arg get_cli_arg(int* index, int argc, char** argv) {
    int i = *index;
    *index += 1;

    // done
    if (i >= argc) {
        return (Arg){ 0 };
    }

    if (argv[i][0] == '-') {
        if (argv[i][1] == '-') {
            // long names
            const char* opt = &argv[i][2];

            for (size_t j = 0; j < ARG_DESC_COUNT; j++) {
                const char* long_name  = arg_descs[j].name;
                if (long_name == NULL) continue;

                size_t long_name_len = long_name ? strlen(long_name) : 0;
                if (strncmp(opt, long_name, long_name_len) == 0) {
                    if (!arg_descs[j].has_arg) {
                        return (Arg){ arg_descs[j].type, arg_is_set };
                    } else {
                        if (opt[long_name_len] == 0) {
                            if ((i + 1) >= argc) {
                                fprintf(stderr, "error: no argument after -%s\n", long_name);
                                exit(1);
                            }

                            *index += 1;
                            return (Arg){ arg_descs[j].type, argv[i + 1] };
                        } else {
                            if (argv[i][long_name_len + 3] == 0) {
                                fprintf(stderr, "error: argument for --%s is empty\n", long_name);
                                exit(1);
                            }

                            return (Arg){ arg_descs[j].type, argv[i] + long_name_len + 3 };
                        }
                    }
                }
            }

            // could not find an option
            fprintf(stderr, "error: could not find option --%s\n", opt);
            exit(1);
        } else {
            // short names
            const char* opt = &argv[i][1];

            for (size_t j = 0; j < ARG_DESC_COUNT; j++) {
                const char* short_name = arg_descs[j].alias;
                if (short_name == NULL) continue;

                size_t short_name_len = strlen(short_name) + 1;
                if (strcmp(opt, short_name) == 0) {
                    if (!arg_descs[j].has_arg) {
                        return (Arg){ arg_descs[j].type, arg_is_set };
                    } else {
                        if (argv[i][short_name_len] == 0) {
                            return (Arg){ arg_descs[j].type, &argv[i][short_name_len + 1] };
                        } else if ((i + 1) >= argc) {
                            fprintf(stderr, "error: no argument after -%s\n", short_name);
                            exit(1);
                        }

                        *index += 1;
                        return (Arg){ arg_descs[j].type, argv[i + 1] };
                    }
                }
            }

            // could not find an option
            fprintf(stderr, "error: could not find option -%s\n", opt);
            exit(1);
        }
    }

    return (Arg){ ARG_NONE, argv[i] };
}

static void print_help(const char* argv0) {
    printf("OVERVIEW: Cuik C compiler\n\n");
    printf("USAGE: %s [options] file...\n\n", argv0);
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

