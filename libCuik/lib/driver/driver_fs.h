#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <glob.h>
#endif

static bool str_ends_with(const char* cstr, const char* postfix) {
    const size_t cstr_len = strlen(cstr);
    const size_t postfix_len = strlen(postfix);

    return postfix_len <= cstr_len && strcmp(cstr + cstr_len - postfix_len, postfix) == 0;
}

// handles the **.c *.c type stuff
static void filtered_append(Cuik_DriverArgs* args, const char* path, bool recursive) {
    const char* slash = path;
    for (const char* p = path; *p; p++) {
        if (*p == '/' || *p == '\\') {
            slash = p;
        }
    }

    #ifdef _WIN32
    WIN32_FIND_DATA find_data;
    HANDLE find_handle = FindFirstFile(path, &find_data);

    // loops through normal files
    if (find_handle != INVALID_HANDLE_VALUE) {
        do {
            char tmp[FILENAME_MAX];
            if (slash == path) {
                sprintf_s(tmp, MAX_PATH, "%s", find_data.cFileName);
            } else {
                sprintf_s(tmp, MAX_PATH, "%.*s%s", (int)(slash - path) + 1, path, find_data.cFileName);
            }

            Cuik_Path* new_path = cuik_malloc(sizeof(Cuik_Path));
            if (!cuikfs_canonicalize(new_path, tmp, false)) {
                fprintf(stderr, "Invalid filepath! %s\n", tmp);
            }

            if (cuik_path_has_ext(new_path, "a") || cuik_path_has_ext(new_path, "lib")) {
                dyn_array_put(args->libraries, new_path);
            } else {
                dyn_array_put(args->sources, new_path);
            }
        } while (FindNextFile(find_handle, &find_data));
    }
    FindClose(find_handle);

    if (recursive) {
        char dir_path[MAX_PATH];
        sprintf_s(dir_path, sizeof(dir_path), "%.*s*", (int)(slash - path) + 1, path);
        HANDLE dir = FindFirstFile(dir_path, &find_data);

        if (dir != INVALID_HANDLE_VALUE) {
            do {
                if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && find_data.cFileName[0] != '.') {
                    char new_pattern[FILENAME_MAX];
                    sprintf_s(new_pattern, sizeof(new_pattern), "%.*s%s%s", (int)(slash - path) + 1, path, find_data.cFileName, slash);

                    filtered_append(args, new_pattern, true);
                }
            } while (FindNextFile(dir, &find_data));
        }
        FindClose(dir);
    }

    #else
    #error "filtered_append isn't implemented on this platform yet"
    #endif
}

static void append_input_path(Cuik_DriverArgs* args, const char* path) {
    // we don't check this very well because we're based
    const char* star = NULL;
    for (const char* p = path; *p; p++) {
        if (*p == '*') {
            star = p;
            break;
        }
    }

    if (star != NULL) {
        filtered_append(args, path, star[1] == '*');
    } else {
        Cuik_Path* newstr = cuik_malloc(sizeof(Cuik_Path));
        if (cuikfs_canonicalize(newstr, path, args->toolchain.case_insensitive)) {
            if (cuik_path_has_ext(newstr, "a") || cuik_path_has_ext(newstr, "lib")) {
                dyn_array_put(args->libraries, newstr);
            } else {
                dyn_array_put(args->sources, newstr);
            }
        } else {
            fprintf(stderr, "Invalid filepath! %s\n", path);
        }
    }
}
