#pragma once

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef struct {
    HANDLE file;
    HANDLE mapping;
    size_t size;
    void* data;
} FileMap;

static FileMap open_file_map_write(const char* filepath, size_t size) {
    HANDLE file = CreateFileA(file_name, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) {
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    HANDLE mapping = CreateFileMappingA(file, NULL, PAGE_READWRITE, 0, size, NULL);
    if (mapping == NULL) {
        CloseHandle(file);
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    void* output = MapViewOfFileEx(mapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, size, NULL);
    if (output == NULL) {
        CloseHandle(mapping);
        CloseHandle(file);
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    return (FileMap){
        .file = file,
        .mapping = mapping,
        .size = size,
        .data = memory
    };
}

static FileMap open_file_map_read(const char* filepath) {
    HANDLE file = CreateFileA(filepath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file == INVALID_HANDLE_VALUE) {
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    LARGE_INTEGER size;
    if (!GetFileSizeEx(file, &size)) {
        CloseHandle(file);
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    HANDLE mapping = CreateFileMappingA(file, NULL, PAGE_READONLY, 0, 0, 0);
    if (!mapping) {
        fprintf(stderr, "Could not map file! %s", filepath);
        CloseHandle(file);
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    void* memory = MapViewOfFileEx(mapping, FILE_MAP_READ, 0, 0, 0, 0);
    if (!memory) {
        fprintf(stderr, "Could not view mapped file! %s", filepath);
        CloseHandle(mapping);
        CloseHandle(file);
        return (FileMap){ INVALID_HANDLE_VALUE };
    }

    return (FileMap){
        .file = file,
        .mapping = mapping,
        .size = size.QuadPart,
        .data = memory
    };
}

static void close_file_map(FileMap* file_map) {
    UnmapViewOfFile(file_map->data);
    CloseHandle(file_map->mapping);
    CloseHandle(file_map->file);
}
#else
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

typedef struct {
    int fd;
    size_t size;
    void* data;
} FileMap;

static FileMap open_file_map_write(const char* filepath, size_t size) {
    int fd = open(filepath, O_CREAT | O_RDWR, 0666);
    if (fd <= 0) {
        return (FileMap){ 0 };
    }

    ftruncate(fd, size);

    void* output = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (output == MAP_FAILED) {
        return (FileMap){ 0 };
    }

    return (FileMap){ fd, size, output };
}

static FileMap open_file_map_read(const char* filepath) {
    int fd = open(filepath, O_RDONLY);
    if (fd <= 0) {
        return (FileMap){ 0 };
    }

    struct stat file_stats;
    if (fstat(fd, &file_stats) == -1) {
        return (FileMap){ 0 };
    }

    void* buffer = mmap(NULL, file_stats.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (buffer == MAP_FAILED) {
        return (FileMap){ 0 };
    }

    return (FileMap){ fd, file_stats.st_size, buffer };
}

static void close_file_map(FileMap* file_map) {
    munmap(file_map->data, file_map->size);
    close(file_map->fd);
}
#endif
