// Common is just a bunch of crap i want accessible to all projects in the Cuik repo
#include "str.h"
#include "arena.h"
#include "futex.h"
#include <stdatomic.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif

#define NL_MAP_IMPL
#include <hash_map.h>

#define NL_HASH_SET_IMPL
#include <new_hash_map.h>

#define EBR_IMPL
#include <ebr.h>

#define NBHS_IMPL
#include <nbhs.h>

#define NBHM_IMPL
#include <nbhm.h>

#define NL_BUFFER_IMPL
#include <buffer.h>

#define LOG_USE_COLOR
#include "log.c"

#ifdef CUIK_ALLOW_THREADS
#include "pool.c"
#endif

#include "perf.h"

uint64_t cuik__page_size = 0;
uint64_t cuik__page_mask = 0;

void cuik_init_terminal(void) {
    #if _WIN32
    // Raw input mode
    // setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), ENABLE_PROCESSED_INPUT);

    // Enable ANSI/VT sequences on windows
    HANDLE output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (output_handle != INVALID_HANDLE_VALUE) {
        DWORD old_mode;
        if (GetConsoleMode(output_handle, &old_mode)) {
            SetConsoleMode(output_handle, old_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
        }
    }
    #endif
}

void* cuik__valloc(size_t size) {
    #ifdef _WIN32
    if (cuik__page_size == 0) {
        SYSTEM_INFO si;
        GetSystemInfo(&si);

        cuik__page_size = si.dwPageSize;
        cuik__page_mask = si.dwPageSize - 1;
    }

    // round size to page size
    size = (size + cuik__page_mask) & ~cuik__page_mask;

    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    #else
    cuik__page_size = 4096;
    cuik__page_mask = 4095;

    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    #endif
}

void cuik__vfree(void* ptr, size_t size) {
    #ifdef _WIN32
    VirtualFree(ptr, 0, MEM_RELEASE);
    #else
    munmap(ptr, size);
    #endif
}

////////////////////////////////
// Strings
////////////////////////////////
String string_cstr(const char* str) {
    return (String){ .length = strlen(str), .data = (const unsigned char*) str };
}

String string_from_range(const unsigned char* start, const unsigned char* end) {
    return (String){ .length = end-start, .data = start };
}

bool string_equals(const String* a, const String* b) {
    return a->length == b->length && memcmp(a->data, b->data, a->length) == 0;
}

bool string_equals_cstr(const String* a, const char* b) {
    return a->length == strlen(b) && memcmp(a->data, b, a->length) == 0;
}

////////////////////////////////
// Arenas
////////////////////////////////
static size_t align_up_pow2(size_t a, size_t b) { return (a + b - 1) & -b; }

static const char* tb_arena_tag(TB_Arena* arena) {
    #ifndef NDEBUG
    return arena->tag;
    #else
    return "";
    #endif
}

void tb_arena_create(TB_Arena* restrict arena, const char* tag) {
    #ifndef NDEBUG
    arena->tag = tag ? tag : "";
    #endif

    size_t chunk_size = TB_ARENA_NORMAL_CHUNK_SIZE - TB_ARENA_ALLOC_HEAD_SLACK;
    arena->top = cuik_malloc(chunk_size);
    arena->top->prev = NULL;
    arena->top->avail = arena->top->data;
    arena->top->limit = &arena->top->data[chunk_size - sizeof(TB_ArenaChunk)];
}

void tb_arena_clear(TB_Arena* arena) {
    TB_ArenaChunk* c = arena->top;
    while (c->prev != NULL) {
        TB_ArenaChunk* prev = c->prev;
        cuik_free(c);
        c = prev;
    }
    c->avail = c->data;
    arena->top = c;
}

void tb_arena_destroy(TB_Arena* restrict arena) {
    TB_ArenaChunk* c = arena->top;
    while (c != NULL) {
        TB_ArenaChunk* prev = c->prev;
        cuik_free(c);
        c = prev;
    }
    arena->top = NULL;
}

void* tb_arena_unaligned_alloc(TB_Arena* restrict arena, size_t size) {
    TB_ArenaChunk* top = arena->top;

    #ifndef NDEBUG
    arena->allocs += 1;
    arena->alloc_bytes += size;
    #endif

    char* p = top->avail;
    if (LIKELY((top->avail + size) <= top->limit)) {
        top->avail += size;
        return p;
    } else {
        // slow path, we need to allocate more chunks
        size_t chunk_size = TB_ARENA_NORMAL_CHUNK_SIZE;
        while (size > chunk_size - (sizeof(TB_ArenaChunk) + TB_ARENA_ALLOC_HEAD_SLACK)) {
            chunk_size *= 2;
        }

        // log_debug("arena: alloc %.2f KiB", chunk_size / 1024.0f);

        TB_ArenaChunk* c = top->prev;
        c = cuik_malloc(sizeof(TB_ArenaChunk) + chunk_size - TB_ARENA_ALLOC_HEAD_SLACK);
        c->prev  = arena->top;
        c->avail = c->data + size;
        c->limit = &c->data[chunk_size - sizeof(TB_ArenaChunk)];

        arena->top = c;
        return c->data;
    }
}

TB_API void* tb_arena_realloc(TB_Arena* restrict arena, void* old, size_t old_size, size_t size) {
    TB_ArenaChunk* top = arena->top;
    old_size = (old_size + TB_ARENA_ALIGNMENT - 1) & ~(TB_ARENA_ALIGNMENT - 1);
    size = (size + TB_ARENA_ALIGNMENT - 1) & ~(TB_ARENA_ALIGNMENT - 1);

    char* p = old;
    if (p + old_size == top->avail) {
        // try to resize
        top->avail = old;
    }

    char* dst = tb_arena_unaligned_alloc(arena, size);
    if (dst != p && old) {
        memcpy(dst, old, old_size);
    }
    return dst;
}

void tb_arena_pop(TB_Arena* restrict arena, void* ptr, size_t size) {
    TB_ArenaChunk* top = arena->top;
    char* p = ptr;
    assert(p + size == top->avail); // cannot pop from arena if it's not at the top

    top->avail = p;
}

bool tb_arena_free(TB_Arena* restrict arena, void* ptr, size_t size) {
    TB_ArenaChunk* top = arena->top;
    size = (size + TB_ARENA_ALIGNMENT - 1) & ~(TB_ARENA_ALIGNMENT - 1);

    char* p = ptr;
    if (p + size == top->avail) {
        top->avail = p;
        return true;
    } else {
        return false;
    }
}

void tb_arena_realign(TB_Arena* restrict arena) {
    TB_ArenaChunk* top = arena->top;
    ptrdiff_t pos = top->avail - top->data;
    assert(pos >= 0);
    pos = (pos + TB_ARENA_ALIGNMENT - 1) & ~(TB_ARENA_ALIGNMENT - 1);
    top->avail = &top->data[pos];
}

TB_ArenaSavepoint tb_arena_save(TB_Arena* arena) {
    return (TB_ArenaSavepoint){ arena->top, arena->top->avail };
}

void tb_arena_restore(TB_Arena* arena, TB_ArenaSavepoint sp) {
    // free all the chunks above
    TB_ArenaChunk* curr = arena->top;
    while (curr != sp.top) {
        TB_ArenaChunk* prev = curr->prev;
        cuik_free(curr);
        curr = prev;
    }

    sp.top->avail = sp.avail;
    arena->top = sp.top;
}

TB_ArenaSavepoint tb_arena_base(TB_Arena* arena) {
    TB_ArenaChunk* c = arena->top;
    while (c->prev) { c = c->prev; }
    return (TB_ArenaSavepoint){ .top = c, .avail = c->data };
}

bool tb_arena_is_top(TB_Arena* arena, TB_ArenaSavepoint sp) {
    return arena->top == sp.top && arena->top->avail == sp.avail;
}

void* tb_arena_alloc(TB_Arena* restrict arena, size_t size) {
    uintptr_t wm = (uintptr_t) arena->top->avail;
    assert((wm & ~0xFull) == wm);

    size = (size + TB_ARENA_ALIGNMENT - 1) & ~(TB_ARENA_ALIGNMENT - 1);
    return tb_arena_unaligned_alloc(arena, size);
}

bool tb_arena_is_empty(TB_Arena* arena) {
    return arena->top->prev == NULL && arena->top->avail == arena->top->data;
}

size_t tb_arena_chunk_size(TB_ArenaChunk* chunk) {
    return chunk->limit - (char*) chunk;
}

size_t tb_arena_current_size(TB_Arena* arena) {
    size_t total = 0;
    for (TB_ArenaChunk* c = arena->top; c; c = c->prev) {
        total += c->avail - (char*) c;
    }
    return total;
}

////////////////////////////////
// Futex functions
////////////////////////////////
#ifdef __linux__
#include <errno.h>
#include <linux/futex.h>
#include <sys/syscall.h>
#include <unistd.h>

// [https://man7.org/linux/man-pages/man2/futex.2.html]
//   glibc provides no wrapper for futex()
#define futex(...) syscall(SYS_futex, __VA_ARGS__)

void futex_signal(Futex* addr) {
    int ret = futex(addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, NULL, NULL, 0);
    if (ret == -1) {
        __builtin_trap();
    }
}

void futex_broadcast(Futex* addr) {
    int ret = futex(addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, INT32_MAX, NULL, NULL, 0);
    if (ret == -1) {
        __builtin_trap();
    }
}

void futex_wait(Futex* addr, int64_t val) {
    for (;;) {
        int ret = futex(addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL, 0);

        if (!((ret == -1 && errno == EAGAIN) || ret == 0)) {
            __builtin_trap();
        }

        if (*addr != val) {
            return;
        }
    }
}

#undef futex
#elif defined(__APPLE__)

#include <errno.h>

enum {
    UL_COMPARE_AND_WAIT = 0x00000001,
    ULF_WAKE_ALL        = 0x00000100,
    ULF_NO_ERRNO        = 0x01000000
};

/* timeout is specified in microseconds */
int __ulock_wait(uint32_t operation, void *addr, uint64_t value, uint32_t timeout);
int __ulock_wake(uint32_t operation, void *addr, uint64_t wake_value);

void futex_signal(Futex* addr) {
    for (;;) {
        int ret = __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, 0);
        if (ret >= 0) {
            return;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }
        printf("futex wake fail?\n");
        __builtin_trap();
    }
}

void _tpool_broadcast(Futex* addr) {
    for (;;) {
        int ret = __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO | ULF_WAKE_ALL, addr, 0);
        if (ret >= 0) {
            return;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }
        printf("futex wake fail?\n");
        __builtin_trap();
    }
}

void futex_wait(Futex* addr, int32_t val) {
    for (;;) {
        int ret = __ulock_wait(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, val, 0);
        if (ret >= 0) {
            if (*addr != val) {
                return;
            }
            continue;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }

        printf("futex wait fail?\n");
    }
}

#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#ifndef __GNUC__
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "Synchronization.lib")
#endif

void futex_signal(Futex* addr) {
    WakeByAddressSingle((void*) addr);
}

void futex_broadcast(Futex* addr) {
    WakeByAddressAll((void*) addr);
}

void futex_wait(Futex* addr, int64_t val) {
    for (;;) {
        WaitOnAddress(addr, (void *)&val, sizeof(val), INFINITE);
        if (*addr != val) break;
    }
}
#endif

#if defined(__APPLE__)
void futex_wait_eq(Futex* addr, int32_t val) {
    int32_t old;
    while (old = *addr, old != val) {
        futex_wait(addr, old);
    }
}
#else
void futex_wait_eq(Futex* addr, int64_t val) {
    int64_t old;
    while (old = *addr, old != val) {
        futex_wait(addr, old);
    }
}
#endif
