#pragma once
#include <stdbool.h>
#include <stdlib.h>

#define INITIAL_CAP 4096

typedef struct BigArrayHeader {
    // honestly storing the type size is kinda weird
    size_t size, capacity;
    char data[];
} BigArrayHeader;

void* big_array_internal_create(size_t type_size, bool has_zero_slot);
void big_array_internal_destroy(void* ptr);
void* big_array_internal_reserve(void* ptr, size_t type_size, size_t extra);

#define BigArray(T) T*
#define big_array_create(T, zero_slot) big_array_internal_create(sizeof(T), zero_slot)
#define big_array_destroy(arr) big_array_internal_destroy(arr)
#define big_array_put(arr, new_data)                            \
do {                                                        \
arr = big_array_internal_reserve(arr, sizeof(*arr), 1); \
BigArrayHeader* header = ((BigArrayHeader*)arr) - 1;    \
arr[header->size++] = new_data;                         \
} while (0)
#define big_array_length(arr) ((((BigArrayHeader*)(arr)) - 1)->size)

#define big_array_put_uninit(arr, extra)                             \
do {                                                             \
size_t extra_ = (extra);                                     \
arr = big_array_internal_reserve(arr, sizeof(*arr), extra_); \
BigArrayHeader* header = ((BigArrayHeader*)arr) - 1;         \
header->size += extra_;                                      \
} while (0)
