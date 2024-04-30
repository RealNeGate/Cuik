// Just a dynamic array that fits well into the arena
#pragma once
#include <common.h>
#include <arena.h>

typedef struct AArray {
    TB_Arena* arena;
    uint32_t length, capacity;
    char data[];
} AArray;

static void* aarray__create(TB_Arena* arena, size_t type_size, size_t cap) {
    AArray* header = tb_arena_alloc(arena, sizeof(AArray) + (type_size * cap));
    *header = (AArray){ .arena = arena, .capacity = cap };
    return &header->data[0];
}

static void* aarray__reserve(void* ptr, size_t type_size, size_t min_size) {
    AArray* header = ((AArray*)ptr) - 1;
    if (min_size >= header->capacity) {
        size_t old = header->capacity;
        header->capacity = min_size * 2;

        AArray* new_ptr = tb_arena_realloc(header->arena, header, sizeof(AArray) + (type_size * header->capacity));
        if (!new_ptr) {
            fprintf(stderr, "error: out of memory!");
            abort();
        }
        return &new_ptr->data[0];
    }

    return ptr;
}

#define ArenaArray(T) T*
#define aarray_create(arena, T, cap) aarray__create(arena, sizeof(T), cap)
#define aarray_length(arr)           ((((AArray*) (arr)) - 1)->length)
#define aarray_set_length(arr, len)  ((((AArray*) (arr)) - 1)->length = (len))
#define aarray_clear(arr)            ((((AArray*) (arr)) - 1)->length = 0)
#define aarray_insert(arr, i, ...)   ((arr) = aarray__reserve(arr, sizeof(*(arr)), (i)), (arr)[i] = __VA_ARGS__)
#define aarray_push(arr, ...)        ((arr) = aarray__reserve(arr, sizeof(*(arr)), aarray_length(arr)), (arr)[aarray_length(arr)++] = __VA_ARGS__)
#define aarray_pop(arr)              ((arr)[(((AArray*)(arr)) - 1)->length -= 1])
#define aarray_top(arr)              ((arr)[(((AArray*)(arr)) - 1)->length - 1])
#define aarray_reserve(arr, i)       ((arr) = aarray__reserve(arr, sizeof(*(arr)), (i)), aarray_length(arr) = (i))
#define aarray_for(i, arr)           for (ptrdiff_t i = 0, end_ = aarray_length(arr); i < end_; i++)
#define aarray_remove(arr, i)        ((arr)[i] = (arr)[(((AArray*)(arr)) - 1)->length -= 1])
