#include "common.h"
#include <stdalign.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif

#define TEMPORARY_STORAGE_SIZE (32 << 20)
#define TEMPORARY_COMMIT_SIZE (1 << 20)

typedef struct TemporaryStorage {
    size_t used;
    size_t committed;

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

#ifdef _WIN32
LONG WINAPI temp_storage_fault_handler(struct _EXCEPTION_POINTERS* info) {
    EXCEPTION_RECORD* e = info->ExceptionRecord;

    // this handler only exists to expand temporary storage
    if (temp_storage == NULL) {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    if (e->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        // if we read or write
        if (e->ExceptionInformation[0] == 0 ||
            e->ExceptionInformation[0] == 1) {
            char* address = (char*) (uintptr_t) e->ExceptionInformation[1];

            char* low = (char*)temp_storage;
            char* high = ((char*)temp_storage) + TEMPORARY_STORAGE_SIZE;

            if (address >= low && address < high) {
                // check if we exceed the space
                if (temp_storage->committed + TEMPORARY_COMMIT_SIZE >= TEMPORARY_STORAGE_SIZE) {
                    fprintf(stderr, "temporary storage: out of memory!\n");
                    exit(2);
                }

                // commit sum pages
                void* result = VirtualAlloc(
                    ((char*) temp_storage) + temp_storage->committed + TEMPORARY_COMMIT_SIZE,
                    TEMPORARY_COMMIT_SIZE, MEM_COMMIT, PAGE_READWRITE
                );
                if (result == NULL) {
                    fprintf(stderr, "temporary storage: out of memory!\n");
                    exit(2);
                }

                //printf("Committed! %zu -> %zu\n", temp_storage->committed, temp_storage->committed + TEMPORARY_COMMIT_SIZE);
                temp_storage->committed += TEMPORARY_COMMIT_SIZE;
                return EXCEPTION_CONTINUE_EXECUTION;
            }
        }
    }

    return EXCEPTION_CONTINUE_SEARCH;
}
#endif

void tls_init(void) {
    if (temp_storage == NULL) {
        #ifdef _WIN32
        temp_storage = VirtualAlloc(NULL, TEMPORARY_STORAGE_SIZE, MEM_RESERVE, PAGE_READWRITE);
        if (temp_storage == NULL) {
            fprintf(stderr, "error: could not reserve temporary storage\n");
            exit(2);
        }

        // commit first region
        void* result = VirtualAlloc(temp_storage, TEMPORARY_COMMIT_SIZE, MEM_COMMIT, PAGE_READWRITE);
        if (result == NULL) {
            fprintf(stderr, "error: could not commit initial temporary storage pages\n");
            exit(2);
        }
        #else
        temp_storage = cuik__valloc(TEMPORARY_STORAGE_SIZE);
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
