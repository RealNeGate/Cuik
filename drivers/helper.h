#ifdef _WIN32
#  define WIN32_MEAN_AND_LEAN
#  include <windows.h>
#else
#  include <unistd.h>
#endif

static bool get_exe_path(char path[FILENAME_MAX]) {
#ifdef _WIN32
    return (GetModuleFileNameA(NULL, path, FILENAME_MAX) > 0);
#else
    return (readlink("/proc/self/exe", path, FILENAME_MAX) > 0);
#endif
}

// tries to walk about `steps` slashes in the filepath and return the pointer to said
// slash, if it can't reach then it'll return NULL
static const char* step_out_dir(const char path[FILENAME_MAX], int steps) {
    int slashes_hit = 0;
    const char* end = path + strlen(path);

    while (slashes_hit != steps && end-- != path) {
        if (*end == '/') slashes_hit++;
        else if (*end == '\\') slashes_hit++;
    }

    return (slashes_hit == steps) ? end : NULL;
}

static Cuik_SystemLibs* find_system_libs(void) {
    char crt_dir[FILENAME_MAX];
    if (!get_exe_path(crt_dir)) {
        fprintf(stderr, "error: could not locate executable path");
        abort();
    }

    char* slash = (char*)step_out_dir(crt_dir, 2);
    if (slash == NULL) {
        fprintf(stderr, "error: could not locate executable path");
        abort();
    }

    *slash = '\0';
    return cuik_get_system_includes(crt_dir);
}