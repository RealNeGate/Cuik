// If you're trying to port TB on to a new platform you'll need to fill in these
// functions with their correct behavior.
#pragma once
#include <setjmp.h>

#ifdef TB_USE_MIMALLOC
#include <mimalloc.h>

#define tb_platform_heap_alloc(size) mi_malloc(size)
#define tb_platform_heap_realloc(ptr, size) mi_realloc(ptr, size)
#define tb_platform_heap_free(ptr) mi_free(ptr)
#else
#define tb_platform_heap_alloc(size)        malloc(size)
#define tb_platform_heap_free(ptr)          free(ptr)
#define tb_platform_heap_realloc(ptr, size) realloc(ptr, size)
#endif

////////////////////////////////
// Virtual memory management
////////////////////////////////
typedef enum {
    TB_PAGE_INVALID,

    TB_PAGE_READONLY,
    TB_PAGE_READWRITE,
    TB_PAGE_READEXECUTE,
} TB_MemProtect;

// This is used for JIT compiler pages or any large scale memory allocations.
void* tb_platform_valloc(size_t size);
void  tb_platform_vfree(void* ptr, size_t size);
bool  tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot);

////////////////////////////////
// General Heap allocator
////////////////////////////////
// This is used for reallocatable and smaller allocations
// compared to the large scale arenas.
void* tb_platform_heap_alloc(size_t size);
void* tb_platform_heap_realloc(void* ptr, size_t size);
void  tb_platform_heap_free(void* ptr);

////////////////////////////////
// String arena
////////////////////////////////
char* tb_platform_string_alloc(const char* str);
void  tb_platform_string_free();

////////////////////////////////
// Persistent arena allocator
////////////////////////////////
void tb_platform_arena_init();

// this persistent arena allocator is used all of the backend
// worker threads to store data until the end of compilation.
void* tb_platform_arena_alloc(size_t size);

// NOTE(NeGate): Free is supposed to free all allocations.
void tb_platform_arena_free(void);
