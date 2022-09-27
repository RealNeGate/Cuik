// if an argument is set and has no args this is put
static const char* program_name = NULL;
static const char* arg_is_set = "set";

typedef enum ArgType {
    ARG_NONE = 0,

    // this one's a hack:
    //   -Dname
    //   -Dname=value
    ARG_DEFINE = 1,

    #define OPTION(type, short_name, long_name, n, desc) \
    ARG_ ## type,
    #include "cli_options.h"
} ArgType;

typedef struct Arg {
    ArgType key;
    const char* value;
} Arg;

// returns argument, *out_param is the name of the parameter
static Arg get_cli_arg(int* index, int argc, char** argv) {
    static const char* names[] = {
        #define OPTION(type, short_name, long_name, n, desc) #short_name, #long_name,
        #include "cli_options.h"
    };

    static int types[] = {
        #define OPTION(type, short_name, long_name, n, desc) ARG_ ## type,
        #include "cli_options.h"
    };

    static bool has_args[] = {
        #define OPTION(type, short_name, long_name, n, desc) n,
        #include "cli_options.h"
    };

    size_t count = sizeof(names) / (sizeof(names[0]) * 2);
    int i = *index;
    *index += 1;

    // done
    if (i >= argc) {
        return (Arg){ 0 };
    }

    if (argv[i][0] == '-') {
        if (argv[i][1] == 'D') {
            return (Arg){ ARG_DEFINE, argv[i] + 2 };
        } else if (argv[i][1] == '-') {
            // long names
            const char* opt = &argv[i][2];

            for (size_t j = 0; j < count; j++) {
                const char* long_name  = names[j*2 + 1];
                size_t long_name_len = strlen(long_name);

                if (strncmp(opt, long_name, long_name_len) == 0) {
                    if (!has_args[j]) {
                        return (Arg){ types[j], arg_is_set };
                    } else {
                        if (opt[long_name_len] == 0) {
                            if ((i + 1) >= argc) {
                                fprintf(stderr, "error: no argument after -%s\n", long_name);
                                exit(1);
                            }

                            *index += 1;
                            return (Arg){ types[j], argv[i + 1] };
                        } else {
                            if (argv[i][long_name_len + 3] == 0) {
                                fprintf(stderr, "error: argument for --%s is empty\n", long_name);
                                exit(1);
                            }

                            return (Arg){ types[j], argv[i] + long_name_len + 3 };
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

            for (size_t j = 0; j < count; j++) {
                const char* short_name = names[j*2 + 0];

                if (strcmp(opt, short_name) == 0) {
                    if (!has_args[j]) {
                        return (Arg){ types[j], arg_is_set };
                    } else {
                        if ((i + 1) >= argc) {
                            fprintf(stderr, "error: no argument after -%s\n", short_name);
                            exit(1);
                        }

                        *index += 1;
                        return (Arg){ types[j], argv[i + 1] };
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

static void print_help(void) {
    printf("OVERVIEW: Cuik C compiler\n\n");
    printf("USAGE: %s [options] file...\n\n", program_name);

    int l = 0;

    #define OPTION(type, short_name, long_name, n, desc)    \
    if (#short_name[0] == '_') {                            \
        l = printf("  --" #long_name " ");                  \
        for (int i = l; i < 20; i++) printf(" ");           \
        printf(desc "\n");                                  \
    } else {                                                \
        l = printf("  -" #short_name " --" #long_name " "); \
        for (int i = l; i < 20; i++) printf(" ");           \
        printf(desc "\n");                                  \
    }
    #include "cli_options.h"
}
