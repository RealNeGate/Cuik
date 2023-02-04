// Common is just a bunch of crap i want accessible to all projects in the Cuik repo
#include "arena.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

void* arena_alloc(Arena* arena, size_t size, size_t align) {
    // alignment must be a power of two
    size_t align_mask = align - 1;

    // If this ever happens... literally how...
    if (size >= ARENA_SEGMENT_SIZE) {
        fprintf(stderr, "error: arena cannot allocate contigous region that big! %zu (limit is %d)\n", size, ARENA_SEGMENT_SIZE);
        exit(2);
    }

    void* ptr;
    if (arena->top && arena->top->used + size + align < ARENA_SEGMENT_SIZE - sizeof(ArenaSegment)) {
        ptr = &arena->top->data[arena->top->used];
        arena->top->used = (arena->top->used + size + align_mask) & ~align_mask;
    } else {
        // Add new page
        ArenaSegment* s = cuik__valloc(ARENA_SEGMENT_SIZE);

        if (!s) {
            fprintf(stderr, "error: arena is out of memory!\n");
            exit(2);
        }

        s->next = NULL;
        s->used = (size + align_mask) & ~align_mask;
        s->capacity = ARENA_SEGMENT_SIZE;
        s->_pad = 0;
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
            cuik__vfree(c, ARENA_SEGMENT_SIZE);
            c = next;
        }

        arena->base = arena->top = NULL;
    }
}

void arena_trim(Arena* arena) {
    // decommit any leftover pages
    if (arena->base) {
        for (ArenaSegment* c = arena->base; c != NULL; c = c->next) {
            size_t aligned_used = (sizeof(ArenaSegment) + c->used + 4095u) & ~4095u;

            if (aligned_used != c->capacity) {
                cuik__vfree((char*)c + aligned_used, c->capacity - aligned_used);
                c->capacity = aligned_used;
            }
        }
    }
}

void arena_append(Arena* arena, Arena* other) {
    if (arena->top != NULL && other != NULL) {
        arena->top->next = other->base;
        arena->top = other->top;
    }
}

size_t arena_get_memory_usage(Arena* arena) {
    size_t c = 0;
    for (ArenaSegment* s = arena->base; s != NULL; s = s->next) {
        c += s->capacity;
    }
    return c;
}
