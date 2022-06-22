// if an argument is set and has no args this is put
static const char* program_name = NULL;
static const char* arg_is_set = "set";

typedef enum ArgType {
    ARG_NONE = 0,

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

            #define OPTION(type, short_name, long_name, n, desc)                              \
            if (strncmp(opt, #long_name, sizeof(#long_name)-1) == 0) {                        \
                if (n == 0) {                                                                 \
                    return (Arg){ ARG_ ## type, arg_is_set };                                 \
                } else {                                                                      \
                    if (argv[i][sizeof("--" #long_name "=")] == '\0') {                       \
                        fprintf(stderr, "error: expected argument after --" #long_name "\n"); \
                        exit(1);                                                              \
                    }                                                                         \
                    *index += 1;                                                              \
                    return (Arg){ ARG_ ## type, argv[i] + sizeof("--" #long_name) };          \
                }                                                                             \
            }
            #include "cli_options.h"

            // could not find an option
            fprintf(stderr, "error: could not find option --%s\n", opt);
            exit(1);
        } else {
            // short names
            const char* opt = &argv[i][1];

            #define OPTION(type, short_name, long_name, n, desc)                              \
            if (strcmp(opt, #short_name) == 0) {                                              \
                if (n == 0) {                                                                 \
                    return (Arg){ ARG_ ## type, arg_is_set };                                 \
                } else {                                                                      \
                    if ((i + 1) >= argc) {                                                    \
                        fprintf(stderr, "error: expected argument after -" #short_name "\n"); \
                        exit(1);                                                              \
                    }                                                                         \
                    *index += 1;                                                              \
                    return (Arg){ ARG_ ## type, argv[i + 1] };                                \
                }                                                                             \
            }
            #include "cli_options.h"

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
