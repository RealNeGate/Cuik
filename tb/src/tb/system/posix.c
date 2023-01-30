#ifndef _WIN32
#include "../tb_internal.h"

void* tb_platform_valloc(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void* tb_platform_valloc_guard(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void tb_platform_vfree(void* ptr, size_t size) {
    munmap(ptr, size);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    uint32_t protect;
    switch (prot) {
        case TB_PAGE_READONLY:    protect = PROT_READ; break;
        case TB_PAGE_READWRITE:   protect = PROT_READ | PROT_WRITE; break;
        case TB_PAGE_READEXECUTE: protect = PROT_READ | PROT_EXEC; break;
        default: return false;
    }

    return mprotect(ptr, size, protect) == 0;
}

void* tb_platform_heap_alloc(size_t size) {
    return malloc(size);
}

void* tb_platform_heap_realloc(void* ptr, size_t size) {
    return realloc(ptr, size);
}

void tb_platform_heap_free(void* ptr) {
    free(ptr);
}

static char* string_buffer;
static tb_atomic_size_t string_head;

char* tb_platform_string_alloc(const char* str) {
    if (!string_buffer) { string_buffer = tb_platform_valloc(64 << 20); }

    size_t len = strlen(str) + 1;
    size_t pos = tb_atomic_size_add(&string_head, len);

    char* new_str = &string_buffer[pos];
    strcpy(new_str, str);

    return new_str;
}

void tb_platform_string_free() { tb_platform_vfree(string_buffer, 64 << 20); }

////////////////////////////////
// Persistent arena allocator
////////////////////////////////
#define ARENA_SEGMENT_SIZE (4 * 1024 * 1024)

// It's a linked list :)
typedef struct Segment {
    struct Segment* next;
    size_t          used;
    unsigned char   data[];
} Segment;

static Segment* arena_base;
static Segment* arena_top;

// weird bootleg mutex because i dont get threads.h on windows :(
static tb_atomic_int arena_lock = 0;

void tb_platform_arena_init(void) {
    Segment* s = (Segment*)mmap(NULL, ARENA_SEGMENT_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (!s) abort();

    arena_base = arena_top = s;
}

void* tb_platform_arena_alloc(size_t size) {
    // align to max_align
    size_t align_mask = _Alignof(max_align_t) - 1;
    size = (size + align_mask) & ~align_mask;

    // If this ever happens... literally how...
    assert(size < ARENA_SEGMENT_SIZE);

    // lock
    int expected = 0;
    while (!__atomic_compare_exchange_n(&arena_lock, &expected, 1, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) { }

    void* ptr;
    if (arena_top->used + size - sizeof(Segment) < ARENA_SEGMENT_SIZE) {
        ptr = &arena_top->data[arena_top->used];
        arena_top->used += size;
    } else {
        // Add new page
        Segment* s = (Segment*)mmap(NULL, ARENA_SEGMENT_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (!s) {
            tb_panic("tb_platform_arena_alloc: Out of memory!");
        }

        s->next = NULL;
        s->used = size;
        ptr = s->data;

        // Insert to top of nodes
        arena_top->next = s;
        arena_top       = s;
    }

    // unlock
    __atomic_exchange_n(&arena_lock, 0, __ATOMIC_SEQ_CST);
    return ptr;
}

void tb_platform_arena_free(void) {
    Segment* c = arena_base;
    while (c) {
        Segment* next = c->next;
        munmap(c, ARENA_SEGMENT_SIZE);
        c = next;
    }
}
#endif
