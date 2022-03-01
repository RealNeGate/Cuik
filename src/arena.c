#include "arena.h"

#define ARENA_SEGMENT_SIZE (1024 * 1024)

// It's a linked list :)
typedef struct ArenaSegment {
	struct ArenaSegment* next;
	size_t used;
	unsigned char data[];
} ArenaSegment;

thread_local ArenaSegment* arena_base;
thread_local ArenaSegment* arena_top;

void* arena_alloc(size_t size, size_t align) {
	// alignment must be a power of two
	size_t align_mask = align-1;
	
	// If this ever happens... literally how...
	assert(size < ARENA_SEGMENT_SIZE);
	
	void* ptr;
	if (arena_top && arena_top->used + size + align < ARENA_SEGMENT_SIZE - sizeof(ArenaSegment)) {
		ptr = &arena_top->data[arena_top->used];
		arena_top->used = (arena_top->used + size + align_mask) & ~align_mask;
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
		if (arena_top) arena_top->next = s;
		else arena_base = s;
		
		arena_top = s;
	}
	
	return ptr;
}

void arena_free() {
	if (arena_base) {
		ArenaSegment* c = arena_base;
		while (c) {
			ArenaSegment* next = c->next;
			free(c);
			c = next;
		}
		
		arena_base = arena_top = NULL;
	}
}
