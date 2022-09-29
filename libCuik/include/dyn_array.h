#pragma once
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <cuik.h>

#define INITIAL_CAP 4096

typedef struct DynArrayHeader {
    // honestly storing the type size is kinda weird
    size_t size, capacity;
    char data[];
} DynArrayHeader;

inline static void* dyn_array_internal_create(size_t type_size, size_t cap) {
    DynArrayHeader* header = malloc(sizeof(DynArrayHeader) + (type_size * cap));

    *header = (DynArrayHeader){
        .capacity = cap
    };
    return &header->data[0];
}

inline static void dyn_array_internal_destroy(void* ptr) {
    if (ptr != NULL) {
        DynArrayHeader* header = ((DynArrayHeader*)ptr) - 1;
        free(header);
    }
}

inline static void* dyn_array_internal_reserve(void* ptr, size_t type_size, size_t extra) {
    DynArrayHeader* header = ((DynArrayHeader*)ptr) - 1;

    if (header->size + extra >= header->capacity) {
        header->capacity = (header->size + extra) * 2;
        DynArrayHeader* new_ptr = realloc(header, sizeof(DynArrayHeader) + (type_size * header->capacity));
        if (!new_ptr) {
            fprintf(stderr, "error: out of memory!");
            abort();
        }

        return &new_ptr->data[0];
    }

    return ptr;
}

inline static void* dyn_array_internal_trim(void* ptr, size_t type_size) {
    DynArrayHeader* header = ((DynArrayHeader*)ptr) - 1;
    header->capacity = header->size;

    DynArrayHeader* new_ptr = realloc(header, sizeof(DynArrayHeader) + (type_size * header->capacity));
    assert(new_ptr != NULL);
    return &new_ptr->data[0];
}

#define DynArray(T) T*
#define dyn_array_create(T) dyn_array_internal_create(sizeof(T), INITIAL_CAP)
#define dyn_array_create_with_initial_cap(T, cap) dyn_array_internal_create(sizeof(T), cap)
#define dyn_array_destroy(arr) (dyn_array_internal_destroy(arr), (arr) = NULL)

#define dyn_array_put(arr, ...)                             \
do {                                                        \
    arr = dyn_array_internal_reserve(arr, sizeof(*arr), 1); \
    DynArrayHeader* header = ((DynArrayHeader*)arr) - 1;    \
    arr[header->size++] = __VA_ARGS__;                      \
} while (0)

#define dyn_array_put_uninit(arr, extra)                         \
do {                                                             \
    size_t extra_ = (extra);                                     \
    arr = dyn_array_internal_reserve(arr, sizeof(*arr), extra_); \
    DynArrayHeader* header = ((DynArrayHeader*)arr) - 1;         \
    header->size += extra_;                                      \
} while (0)

#define dyn_array_trim(arr) (arr = dyn_array_internal_trim(arr, sizeof(*arr)))
#define dyn_array_clear(arr) (((((DynArrayHeader*)(arr)) - 1)->size) = 0)
#define dyn_array_set_length(arr, newlen) (((((DynArrayHeader*)(arr)) - 1)->size) = (newlen))
#define dyn_array_length(arr) ((((DynArrayHeader*)(arr)) - 1)->size)
#define dyn_array_for(it, arr) for (ptrdiff_t it = 0, _count_ = dyn_array_length(arr); it < _count_; it++)
