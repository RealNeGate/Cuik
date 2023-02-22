#pragma once
#include "common.h"

#define ARENA_SEGMENT_SIZE (16 * 1024 * 1024)

// It's a linked list :)
typedef struct ArenaSegment {
    struct ArenaSegment* next;
    size_t used;
    size_t capacity;
    size_t _pad;

    unsigned char data[];
} ArenaSegment;

typedef struct {
    ArenaSegment* base;
    ArenaSegment* top;
} Arena;

#define ARENA_ALLOC(arena, T) arena_alloc(arena, sizeof(T), _Alignof(T))
#define ARENA_ARR_ALLOC(arena, count, T) arena_alloc(arena, (count) * sizeof(T), _Alignof(T))

void* arena_alloc(Arena* arena, size_t size, size_t align);
void arena_clear(Arena* arena);
void arena_free(Arena* arena);
void arena_trim(Arena* arena);
void arena_append(Arena* arena, Arena* other);
size_t arena_get_memory_usage(Arena* arena);
