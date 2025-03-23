#include "common.h"
#include <stdalign.h>

#define TEMPORARY_STORAGE_SIZE (32 << 20)

typedef struct TemporaryStorage {
    size_t used;
    size_t committed;

    uint8_t data[];
} TemporaryStorage;

static _Thread_local TemporaryStorage* temp_storage;

void tls_init(void) {
    if (temp_storage == NULL) {
        temp_storage = cuik__valloc(TEMPORARY_STORAGE_SIZE);
        if (temp_storage == NULL) {
            fprintf(stderr, "error: could not reserve temporary storage\n");
            abort();
        }
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

void* tls_save(void) {
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
