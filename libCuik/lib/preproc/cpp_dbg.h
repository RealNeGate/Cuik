// This is an optional internal tool for debugging the preprocessor output
#define CPP_DBG 1

static char* breakpoints[100];
static MacroArgs* cppdbg__arglist;
static int cppdbg__log;

static bool strprefix(const char* str, const char* pre) {
    return strncmp(pre, str, strlen(pre)) == 0;
}

static int cppdbg__break(void) {
    cppdbg__log = 0;

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
        } else if (strcmp(line, "args") == 0) {
            // printf("FUNCTION MACRO: %.*s    %.*s\n", (int)t.content.length, t.content.data, (int)def.length, def.data);
            if (cppdbg__arglist) {
                for (size_t i = 0; i < cppdbg__arglist->value_count; i++) {
                    printf("  ['%.*s'] = '%.*s'\n", (int) cppdbg__arglist->keys[i].length, cppdbg__arglist->keys[i].data, (int) cppdbg__arglist->values[i].content.length, cppdbg__arglist->values[i].content.data);
                }
            } else {
                printf("NONE\n");
            }
        } else if (strcmp(line, "log") == 0) {
            printf("Logging will continue until next break...\n");
            cppdbg__log = 1;
        } else if (strcmp(line, "dump") == 0) {
            return 1;
        } else if (strcmp(line, "next") == 0) {
            return 0;
        } else {
            fprintf(stderr, "unknown command: %s\n", line);
        }
    }

    return 0;
}
