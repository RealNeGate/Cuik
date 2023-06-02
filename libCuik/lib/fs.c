#include <cuik_fs.h>
#include <log.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dirent.h>
#include <unistd.h>
#endif

struct Cuik_File {
    int id;
};

#ifndef _WIN32
// r must have strlen(path) + 2 bytes
static int casepath(const char* path, char* r) {
    size_t l = strlen(path);
    char _[FILENAME_MAX], *p = _;
    strcpy(p, path);
    size_t rl = 0;

    DIR* d = NULL;
    if (p[0] == '/') {
        d = opendir("/");
        p = p + 1;
    } else {
        d = opendir(".");
        r[0] = '.';
        r[1] = 0;
        rl = 1;
    }

    int last = 0;
    char* c = strsep(&p, "/");
    while (c != NULL) {
        if (d == NULL) {
            return 0;
        }

        if (last) {
            closedir(d);
            return 0;
        }

        r[rl] = '/';
        rl += 1;
        r[rl] = 0;

        struct dirent *e = readdir(d);
        while (e != NULL) {
            if (strcasecmp(c, e->d_name) == 0) {
                strcpy(r + rl, e->d_name);
                rl += strlen(e->d_name);

                closedir(d);
                d = opendir(r);
                break;
            }

            e = readdir(d);
        }

        if (e == NULL) {
            strcpy(r + rl, c);
            rl += strlen(c);
            last = 1;
        }

        c = strsep(&p, "/");
    }

    if (d) closedir(d);
    return 1;
}
#endif

bool cuikfs_canonicalize(Cuik_Path* restrict output, const char* path, bool case_insensitive) {
    #ifdef _WIN32
    char* filepart;
    if (GetFullPathNameA(path, FILENAME_MAX, output->data, &filepart) == 0) {
        return false;
    }

    // Convert file paths into something more comfortable
    // The windows file paths are case insensitive
    char* p = output->data;
    for (; *p; p++) {
        if (*p == '\\') {
            *p = '/';
        } else if (*p >= 'A' && *p <= 'Z') {
            *p -= ('A' - 'a');
        }
    }

    output->length = p - output->data;
    return true;
    #else
    if (case_insensitive) {
        return casepath(path, output->data);
    } else {
        if (realpath(path, output->data) == NULL) {
            return false;
        }

        output->length = strlen(output->data);
        return true;
    }
    #endif
}

Cuik_File* cuikfs_open(const char* path, bool write) {
    #ifdef _WIN32
    if (write) {
        return (Cuik_File*) CreateFileA(path, GENERIC_WRITE, FILE_SHARE_WRITE, 0, CREATE_NEW, 0, 0);
    } else {
        return (Cuik_File*) CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    }
    #else
    Cuik_File* f = cuik_malloc(sizeof(Cuik_File));
    f->id = open(path, write ? (O_CREAT | O_WRONLY) : O_RDONLY);
    return f;
    #endif
}

void cuikfs_close(Cuik_File* file) {
    #ifdef _WIN32
    CloseHandle(file);
    #else
    close(file->id);
    #endif
}

bool cuikfs_get_length(Cuik_File* file, size_t* out_length) {
    #ifdef _WIN32
    LARGE_INTEGER file_size;
    if (GetFileSizeEx((HANDLE) file, &file_size)) {
        return false;
    }

    *out_length = file_size.QuadPart;
    return true;
    #else
    struct stat file_stats;
    if (fstat(file->id, &file_stats) == -1) {
        return false;
    }

    *out_length = file_stats.st_size;
    return true;
    #endif
}

bool cuikfs_read(Cuik_File* file, void* data, size_t count) {
    #ifdef _WIN32
    DWORD bytes_read;
    if (ReadFile((HANDLE) file, data, count, &bytes_read, NULL)) {
        return count == bytes_read;
    } else {
        return false;
    }
    #else
    return read(file->id, data, count) == count;
    #endif
}
