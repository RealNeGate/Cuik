// if an argument is set and has no args this is put
static const char* program_name = NULL;
static const char* arg_is_set = "set";

typedef enum ArgType {
    ARG_NONE = 0,

    #define X(name, short, long, has_args, msg) name,
    #include "cli_args.h"
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
    #define X(name, short, long, has_args, msg) { name, short, long, has_args, msg },
    #include "cli_args.h"
};
enum { ARG_DESC_COUNT = sizeof(arg_descs) / sizeof(arg_descs[0]) };

static void exit_or_hook(int code);

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
                                exit_or_hook(1);
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
            exit_or_hook(1);
        } else {
            // short names
            const char* opt = &argv[i][1];

            for (size_t j = 0; j < ARG_DESC_COUNT; j++) {
                const char* short_name = arg_descs[j].alias;
                if (short_name == NULL) continue;

                size_t short_name_len = strlen(short_name);
                if (strncmp(opt, short_name, short_name_len) == 0) {
                    if (!arg_descs[j].has_arg) {
                        return (Arg){ arg_descs[j].type, arg_is_set };
                    } else {
                        if (opt[short_name_len] != 0) {
                            return (Arg){ arg_descs[j].type, &opt[short_name_len] };
                        } else if ((i + 1) >= argc) {
                            fprintf(stderr, "error: no argument after -%s\n", short_name);
                            exit_or_hook(1);
                        }

                        *index += 1;
                        return (Arg){ arg_descs[j].type, argv[i + 1] };
                    }
                }
            }

            // could not find an option
            fprintf(stderr, "error: could not find option -%s\n", opt);
            exit_or_hook(1);
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
