#include "arena.h"

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
		ArenaSegment* s = (ArenaSegment*)malloc(ARENA_SEGMENT_SIZE);
		if (!s) {
			printf("Out of memory!\n");
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
			free(c);
			c = next;
		}
		
		arena->base = arena->top = NULL;
	}
}

size_t arena_get_memory_usage(Arena* arena) {
	size_t c = 0;
	for (ArenaSegment* s = arena->base; s != NULL; s = s->next) c++;
	
	return c * ARENA_SEGMENT_SIZE;
}
