#pragma once
#include <stddef.h>
#include <stdbool.h>

enum {
    ARENA_SMALL_CHUNK_SIZE  =        4 * 1024,
    ARENA_MEDIUM_CHUNK_SIZE =      512 * 1024,
    ARENA_LARGE_CHUNK_SIZE  = 2 * 1024 * 1024,

    ARENA_ALIGNMENT = 16,
};

typedef struct ArenaChunk ArenaChunk;
struct ArenaChunk {
    ArenaChunk* next;
    size_t pad;
    char data[];
};

typedef struct Arena {
    size_t chunk_size;
    ArenaChunk* base;
    ArenaChunk* top;

    // top of the allocation space
    char* watermark;
    char* high_point; // &top->data[chunk_size]
} Arena;

typedef struct ArenaSavepoint {
    ArenaChunk* top;
    char* watermark;
} ArenaSavepoint;

#define ARENA_FOR(it, arena) for (ArenaChunk* it = (arena)->base; it != NULL; it = it->next)

#define ARENA_ALLOC(arena, T) arena_alloc(arena, sizeof(T))
#define ARENA_ARR_ALLOC(arena, count, T) arena_alloc(arena, (count) * sizeof(T))

void arena_create(Arena* restrict arena, size_t chunk_size);
void arena_destroy(Arena* restrict arena);

void* arena_unaligned_alloc(Arena* restrict arena, size_t size);
void* arena_alloc(Arena* restrict arena, size_t size);

// asserts if ptr+size != watermark
void arena_pop(Arena* restrict arena, void* ptr, size_t size);

// in case you wanna mix unaligned and aligned arenas
void arena_realign(Arena* restrict arena);

bool arena_is_empty(Arena* arena);

// savepoints
ArenaSavepoint arena_save(Arena* arena);
void arena_restore(Arena* arena, ArenaSavepoint sp);

// resets to only having one chunk
void arena_clear(Arena* arena);
