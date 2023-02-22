// Common is just a bunch of crap i want accessible to all projects in the Cuik repo
#include "arena.h"
#include "futex.h"
#include <stdatomic.h>

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

void arena_clear(Arena* arena) {
    __debugbreak();
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

////////////////////////////////
// Futex functions
////////////////////////////////
void futex_dec(Futex* f) {
    if (atomic_fetch_sub(f, 1) == 1) {
        futex_signal(f);
    }
}

#ifdef __linux__
#include <linux/futex.h>
#include <sys/syscall.h>

void futex_signal(Futex* addr) {
    int ret = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, NULL, NULL, 0);
    if (ret == -1) {
        perror("Futex wake");
        __debugbreak();
    }
}

void futex_broadcast(Futex* addr) {
    int ret = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, INT32_MAX, NULL, NULL, 0);
    if (ret == -1) {
        perror("Futex wake");
        __debugbreak();
    }
}

void futex_wait(Futex* addr, Futex val) {
    for (;;) {
        int ret = syscall(SYS_futex, addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL, 0);
        if (ret == -1) {
            if (errno != EAGAIN) {
                perror("Futex wait");
                __debugbreak();
            } else {
                return;
            }
        } else if (ret == 0) {
            if (*addr != val) {
                return;
            }
        }
    }
}

#elif defined(__APPLE__)

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
        __debugbreak();
    }
}

void _tpool_broadcast(TPool_Futex *addr) {
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
        __debugbreak();
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

#pragma comment(lib, "Synchronization.lib")

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
