// Common is just a bunch of crap i want accessible to all projects in the Cuik repo
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
#include <hash_set.h>

#define LOG_USE_COLOR
#include "log.c"

#include "perf.h"

uint64_t cuik__page_size = 0;
uint64_t cuik__page_mask = 0;

void cuik_init_terminal(void) {
    #if _WIN32
    // Raw input mode
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

void* arena_alloc(Arena* arena, size_t size, size_t align) {
    if (size == 0) return NULL;

    // alignment must be a power of two
    size_t align_mask = align - 1;

    // If this ever happens... literally how...
    if (size >= ARENA_SEGMENT_SIZE) {
        fprintf(stderr, "error: arena cannot allocate contigous region that big! %zu (limit is %d)\n", size, ARENA_SEGMENT_SIZE);
        exit(2);
    }

    void* ptr;
    if (arena->top && arena->top->used + size + align < ARENA_SEGMENT_SIZE - sizeof(ArenaSegment)) {
        arena->top->used = (arena->top->used + align_mask) & ~align_mask;
        ptr = &arena->top->data[arena->top->used];
        arena->top->used += size;
    } else if (arena->top == NULL || arena->top->next == NULL) {
        if (cuikperf_is_active()) {
            char p[20];
            snprintf(p, 20, "%p", arena);
            cuikperf_region_start(cuik_time_in_nanos(), "arena chunk", p);
        }

        // Add new page
        ArenaSegment* s = cuik__valloc(ARENA_SEGMENT_SIZE);
        if (!s) {
            fprintf(stderr, "error: arena is out of memory!\n");
            exit(2);
        }

        s->next = NULL;
        s->capacity = ARENA_SEGMENT_SIZE;
        s->used = (size + align_mask) & ~align_mask;
        s->_pad = 0;
        ptr = s->data;

        // Insert to top of nodes
        if (arena->top) arena->top->next = s;
        else arena->base = s;

        arena->top = s;

        if (cuikperf_is_active()) {
            cuikperf_region_end();
        }
    } else {
        ArenaSegment* s = arena->top->next;
        s->used = (size + align_mask) & ~align_mask;
        ptr = s->data;

        // Insert to top of nodes
        if (arena->top) arena->top->next = s;
        else arena->base = s;

        arena->top = s;
    }

    return ptr;
}

void arena_clear(Arena* arena) {
    if (arena->base != NULL) {
        arena->top = arena->base;
        arena->base->used = 0;
    }
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
    if (arena->top) {
        if (other != NULL) {
            arena->top->next = other->base;
            arena->top = other->top;
        }
    } else {
        // attach to start
        arena->base = other->base;
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

////////////////////////////////
// Futex functions
////////////////////////////////
void futex_dec(Futex* f) {
    if (atomic_fetch_sub(f, 1) == 1) {
        futex_signal(f);
    }
}

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

void futex_wait(Futex* addr, Futex val) {
    for (;;) {
        int ret = futex(addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL, 0);

        if (ret == -1) {
            if (errno != EAGAIN) {
                __builtin_trap();
            }
        } else if (ret == 0) {
            if (*addr != val) {
                return;
            }
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

void futex_wait(Futex* addr, Futex val) {
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

void futex_wait(Futex* addr, Futex val) {
    for (;;) {
        WaitOnAddress(addr, (void *)&val, sizeof(val), INFINITE);
        if (*addr != val) break;
    }
}
#endif

void futex_wait_eq(Futex* addr, Futex val) {
    while (*addr != val) {
        futex_wait(addr, *addr);
    }
}
