#pragma once
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#define DYN_ARRAY_USE_MIMALLOC 1

#if DYN_ARRAY_USE_MIMALLOC
// use mimalloc
extern void* mi_malloc(size_t s);
extern void* mi_realloc(void* p, size_t s);
extern void mi_free(void* p);
#endif

#define INITIAL_CAP 4096

typedef struct DynArrayHeader {
    // honestly storing the type size is kinda weird
    size_t size, capacity;
    char data[];
} DynArrayHeader;

inline static void* dyn_array_internal_create(size_t type_size) {
    #if DYN_ARRAY_USE_MIMALLOC
    DynArrayHeader* header = mi_malloc(sizeof(DynArrayHeader) + (type_size * INITIAL_CAP));
    #elif defined(_WIN32) && defined(_DEBUG)
    DynArrayHeader* header = _malloc_dbg(sizeof(DynArrayHeader) + (type_size * INITIAL_CAP), _NORMAL_BLOCK, __FILE__, __LINE__);
    #else
    DynArrayHeader* header = malloc(sizeof(DynArrayHeader) + (type_size * INITIAL_CAP));
    #endif

    *header = (DynArrayHeader){
        .capacity = INITIAL_CAP
    };
    return &header->data[0];
}

inline static void dyn_array_internal_destroy(void* ptr) {
    if (ptr != NULL) {
        DynArrayHeader* header = ((DynArrayHeader*)ptr) - 1;
        #if DYN_ARRAY_USE_MIMALLOC
        mi_free(header);
        #else
        free(header);
        #endif
    }
}

inline static void* dyn_array_internal_reserve(void* ptr, size_t type_size, size_t extra) {
    DynArrayHeader* header = ((DynArrayHeader*)ptr) - 1;

    if (header->size + extra >= header->capacity) {
        size_t old = header->capacity;
        header->capacity = (header->size + extra) * 2;
        // fprintf(stderr, "info: resize! %zu -> %zu\n", old, header->capacity);

        #if DYN_ARRAY_USE_MIMALLOC
        DynArrayHeader* new_ptr = mi_realloc(header, sizeof(DynArrayHeader) + (type_size * header->capacity));
        #elif defined(_WIN32) && defined(_DEBUG)
        DynArrayHeader* new_ptr = _realloc_dbg(header, sizeof(DynArrayHeader) + (type_size * header->capacity), _NORMAL_BLOCK, __FILE__, __LINE__);
        #else
        DynArrayHeader* new_ptr = realloc(header, sizeof(DynArrayHeader) + (type_size * header->capacity));
        #endif

        if (!new_ptr) {
            fprintf(stderr, "error: out of memory!");
            abort();
        }

        return &new_ptr->data[0];
    }

    return ptr;
}

#define DynArray(T) T*
#define dyn_array_create(T) dyn_array_internal_create(sizeof(T))
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

#define dyn_array_clear(arr) (((((DynArrayHeader*)(arr)) - 1)->size) = 0)
#define dyn_array_set_length(arr, newlen) (((((DynArrayHeader*)(arr)) - 1)->size) = (newlen))
#define dyn_array_length(arr) ((((DynArrayHeader*)(arr)) - 1)->size)
#define dyn_array_for(it, arr) for (ptrdiff_t it = 0, count = dyn_array_length(arr); it < count; it++)
