
typedef struct {
    uint64_t og_last_write;
} LiveCompiler;

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static uint64_t get_last_write_time(const char* filepath) {
    WIN32_FIND_DATA data;
    HANDLE handle = FindFirstFile(filepath, &data);

    ULARGE_INTEGER i;
    i.LowPart = data.ftLastWriteTime.dwLowDateTime;
    i.HighPart = data.ftLastWriteTime.dwHighDateTime;

    FindClose(handle);
    return i.QuadPart;
}

// returns false if no changes are made, true if the file is ready to be reloaded
static bool live_compile_watch(LiveCompiler* l, const char* source_file) {
    // Wait for the user to save again
    uint64_t current_last_write;
    if (current_last_write = get_last_write_time(source_file), l->og_last_write == current_last_write) {
        return false;
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

static bool live_compile_watch(LiveCompiler* l, const char* source_file) {
    l->og_last_write = get_last_write_time(source_file);
    return false;
}
#endif
