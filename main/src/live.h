typedef struct {
    uint64_t og_last_write;
} LiveCompiler;

#if _WIN32
static uint64_t get_last_write_time(const char* filepath) {
    WIN32_FIND_DATA data;
    HANDLE handle = FindFirstFile(filepath, &data);

    ULARGE_INTEGER i;
    i.LowPart = data.ftLastWriteTime.dwLowDateTime;
    i.HighPart = data.ftLastWriteTime.dwHighDateTime;

    FindClose(handle);
    return i.QuadPart;
}

// currently we only support modifying the main file when the live loop is active
// it'll return true when it changes happen, it returns false once the user asks to quit.
static bool live_compile_watch(LiveCompiler* l, Cuik_CompilerArgs* args) {
    if (dyn_array_length(args->sources) > 1) {
        printf("TODO: live compile does not support multiple files yet!");
        return false;
    }

    const char* source_file = args->sources[0];
    l->og_last_write = get_last_write_time(source_file);

    // Wait for the user to save again
    uint64_t current_last_write;
    while (current_last_write = get_last_write_time(source_file), l->og_last_write == current_last_write) {
        SleepEx(100, FALSE);
    }

    l->og_last_write = current_last_write;

    // wait for it to finish writing before trying to compile
    int ticks = 0;

    while (GetFileAttributesA(source_file) == INVALID_FILE_ATTRIBUTES) {
        SleepEx(1, FALSE);

        if (ticks++ > 100) {
            printf("live-compiler error: file locked (tried multiple times)\n");
            return false;
        }
    }

    return true;
}
#else
static uint64_t get_last_write_time(const char* filepath) {
    return 0;
}

static bool live_compile_watch(LiveCompiler* l, Cuik_CompilerArgs* args) {
    if (dyn_array_length(args->sources) > 1) {
        printf("TODO: live compile does not support multiple files yet!");
        return false;
    }

    const char* source_file = args->sources[0];
    l->og_last_write = get_last_write_time(source_file);
    return false;
}
#endif
