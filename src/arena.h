#pragma once
#include "common.h"

#define ARENA_SEGMENT_SIZE (1024 * 1024)

// It's a linked list :)
typedef struct ArenaSegment {
	struct ArenaSegment* next;
	size_t used;
	unsigned char data[];
} ArenaSegment;

typedef struct {
	ArenaSegment* base;
	ArenaSegment* top;
} Arena;

extern thread_local Arena thread_arena;

#define ARENA_ALLOC(arena, T) arena_alloc(arena, sizeof(T), _Alignof(T))

void* arena_alloc(Arena* arena, size_t size, size_t align);
void  arena_free(Arena* arena);
