#include "arena.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

thread_local Arena thread_arena;

void* arena_alloc(Arena* arena, size_t size, size_t align) {
	// alignment must be a power of two
	size_t align_mask = align-1;
	
	// If this ever happens... literally how...
	assert(size < ARENA_SEGMENT_SIZE);
	
	void* ptr;
	if (arena->top && arena->top->used + size + align < ARENA_SEGMENT_SIZE - sizeof(ArenaSegment)) {
		ptr = &arena->top->data[arena->top->used];
		arena->top->used = (arena->top->used + size + align_mask) & ~align_mask;
	} else {
		// Add new page
#ifdef _WIN32
		ArenaSegment* s = VirtualAlloc(NULL, ARENA_SEGMENT_SIZE, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
#else
		// umm... mmap dumbass
		ArenaSegment* s = malloc(ARENA_SEGMENT_SIZE);
#endif
		
		if (!s) {
			printf("error: arena is out of memory!\n");
			abort();
		}
		
		s->next = NULL;
		s->used = size;
		ptr = s->data;
		
		// Insert to top of nodes
		if (arena->top) arena->top->next = s;
		else arena->base = s;
		
		arena->top = s;
	}
	
	return ptr;
}

void arena_free(Arena* arena) {
	if (arena->base) {
		ArenaSegment* c = arena->base;
		while (c) {
			ArenaSegment* next = c->next;
#ifdef _WIN32
			VirtualFree(c, 0, MEM_RELEASE);
#else
			free(c);
#endif
			c = next;
		}
		
		arena->base = arena->top = NULL;
	}
}

void arena_trim(Arena* arena) {
#ifdef _WIN32
	// decommit any leftover pages
	if (arena->base) {
		for (ArenaSegment* c = arena->base; c != NULL; c = c->next) {
			size_t aligned_used = (sizeof(ArenaSegment) + c->used + 4095u) & ~4095u;
			
			if (aligned_used != ARENA_SEGMENT_SIZE) {
				VirtualFree((char*)c + aligned_used, ARENA_SEGMENT_SIZE - aligned_used, MEM_RELEASE);
			}
		}
	}
#endif
}

void arena_append(Arena* arena, Arena* other) {
	if (arena->top != NULL && other != NULL) {
		arena->top->next = other->base;
		arena->top = other->top;
	}
}

size_t arena_get_memory_usage(Arena* arena) {
	size_t c = 0;
	for (ArenaSegment* s = arena->base; s != NULL; s = s->next) c++;
	
	return c * ARENA_SEGMENT_SIZE;
}
