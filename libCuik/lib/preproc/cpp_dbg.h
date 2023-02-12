// This is an optional internal tool for debugging the preprocessor output
#define CPP_DBG 1

static char* breakpoints[100];

static bool strprefix(const char* str, const char* pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

static void cppdbg__break(void) {
    char line[255];
    for (;;) {
        // read
        printf("> ");
        while (fgets(line, 255, stdin) == NULL) {}

        char* nl = strchr(line, '\n');
        if (nl) *nl = 0;

        if (strprefix(line, "b ")) {
            for (size_t i = 0; i < 100; i++) if (breakpoints[i] == NULL) {
                fprintf(stderr, "set breakpoint: %s\n", line + 2);
                breakpoints[i] = cuik_strdup(line + 2);
                break;
            }
        } else if (strcmp(line, "debugger") == 0) {
            __debugbreak();
            break;
        } else if (strcmp(line, "next") == 0) {
            break;
        } else {
            fprintf(stderr, "unknown command: %s\n", line);
        }
    }
}
