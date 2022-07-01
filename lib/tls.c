#include "common.h"
#include <stdalign.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif

#define TEMPORARY_STORAGE_SIZE (1 << 20)

typedef struct TemporaryStorage {
    size_t used;
    size_t _;

    uint8_t data[];
} TemporaryStorage;

static _Thread_local TemporaryStorage* temp_storage;

void* cuik__valloc(size_t size) {
    #ifdef _WIN32
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    #else
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    #endif
}

void cuik__vfree(void* ptr, size_t size) {
    #ifdef _WIN32
    VirtualFree(ptr, size, MEM_RELEASE);
    #else
    munmap(ptr, size);
    #endif
}

void tls_init(void) {
    if (temp_storage == NULL) {
        #ifdef _WIN32
        temp_storage = VirtualAlloc(NULL, TEMPORARY_STORAGE_SIZE, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
        #else
        temp_storage = aligned_alloc(16, TEMPORARY_STORAGE_SIZE);
        #endif
    }

    temp_storage->used = 0;
}

void tls_reset(void) {
    temp_storage->used = 0;
}

void* tls_push(size_t size) {
    if (sizeof(TemporaryStorage) + temp_storage->used + size >= TEMPORARY_STORAGE_SIZE) {
        printf("temporary storage: out of memory!\n");
        abort();
    }

    void* ptr = &temp_storage->data[temp_storage->used];
    temp_storage->used += size;
    return ptr;
}

void* tls_pop(size_t size) {
    assert(sizeof(TemporaryStorage) + temp_storage->used > size);

    temp_storage->used -= size;
    return &temp_storage->data[temp_storage->used];
}

void* tls_save() {
    assert(sizeof(TemporaryStorage) + temp_storage->used);

    //size_t align_mask = sizeof(max_align_t) - 1;
    //temp_storage->used = (temp_storage->used + align_mask) & ~align_mask;

    return &temp_storage->data[temp_storage->used];
}

void tls_restore(void* p) {
    size_t i = ((uint8_t*)p) - temp_storage->data;
    assert(i <= temp_storage->used);

    temp_storage->used = i;
}
