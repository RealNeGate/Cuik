#pragma once
#include <stddef.h>
#include <stdbool.h>

enum {
    TB_ARENA_SMALL_CHUNK_SIZE  =        4 * 1024,
    TB_ARENA_MEDIUM_CHUNK_SIZE =      512 * 1024,
    TB_ARENA_LARGE_CHUNK_SIZE  = 2 * 1024 * 1024,

    TB_ARENA_ALIGNMENT = 16,
};

typedef struct TB_ArenaChunk TB_ArenaChunk;
struct TB_ArenaChunk {
    TB_ArenaChunk* next;
    size_t pad;
    char data[];
};

typedef struct TB_Arena {
    size_t chunk_size;
    TB_ArenaChunk* base;
    TB_ArenaChunk* top;

    // top of the allocation space
    char* watermark;
    char* high_point; // &top->data[chunk_size]
} TB_Arena;

typedef struct TB_ArenaSavepoint {
    TB_ArenaChunk* top;
    char* watermark;
} TB_ArenaSavepoint;

#define TB_ARENA_FOR(it, arena) for (TB_ArenaChunk* it = (arena)->base; it != NULL; it = it->next)

#define TB_ARENA_ALLOC(arena, T) tb_arena_alloc(arena, sizeof(T))
#define TB_ARENA_ARR_ALLOC(arena, count, T) tb_arena_alloc(arena, (count) * sizeof(T))

void tb_arena_create(TB_Arena* restrict arena, size_t chunk_size);
void tb_arena_destroy(TB_Arena* restrict arena);

void* tb_arena_unaligned_alloc(TB_Arena* restrict arena, size_t size);
void* tb_arena_alloc(TB_Arena* restrict arena, size_t size);

// asserts if ptr+size != watermark
void tb_arena_pop(TB_Arena* restrict arena, void* ptr, size_t size);

// in case you wanna mix unaligned and aligned arenas
void tb_arena_realign(TB_Arena* restrict arena);

bool tb_arena_is_empty(TB_Arena* arena);

// savepoints
TB_ArenaSavepoint tb_arena_save(TB_Arena* arena);
void tb_arena_restore(TB_Arena* arena, TB_ArenaSavepoint sp);

// resets to only having one chunk
void tb_arena_clear(TB_Arena* arena);
