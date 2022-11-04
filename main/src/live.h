
#if _WIN32
static uint64_t get_last_write_time(const char filepath[]) {
    WIN32_FIND_DATA data;
    HANDLE handle = FindFirstFile(filepath, &data);

    ULARGE_INTEGER i;
    i.LowPart = data.ftLastWriteTime.dwLowDateTime;
    i.HighPart = data.ftLastWriteTime.dwHighDateTime;

    FindClose(handle);
    return i.QuadPart;
}
#else
#error "TODO: implement get_last_write_time for this platform"
#endif

typedef struct {
    uint64_t og_last_write;
} LiveCompiler;

// currently we only support modifying the main file when the live loop is active
// it'll return true when it changes happen, it returns false once the user asks to quit.
static bool live_compile_watch(LiveCompiler* l) {
    if (dyn_array_length(input_files) > 1) {
        printf("TODO: live compile does not support multiple files yet!");
        return false;
    }

    const char* source_file = input_files[0];
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
