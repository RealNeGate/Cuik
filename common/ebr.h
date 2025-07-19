////////////////////////////////
// EBR - Epoch-based reclamation
////////////////////////////////
#ifndef EBR_H
#define EBR_H

#include <threads.h>
#include <stdint.h>
#include <stddef.h>
#include <stdatomic.h>

// Virtual memory allocation (since the tables are generally nicely page-size friendly)
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define EBR_VIRTUAL_ALLOC(size)     VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE)
#define EBR_VIRTUAL_FREE(ptr, size) VirtualFree(ptr, size, MEM_RELEASE)
#else
#include <sys/mman.h>

#define EBR_VIRTUAL_ALLOC(size)     mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
#define EBR_VIRTUAL_FREE(ptr, size) munmap(ptr, size)
#endif

// traditional heap ops
#ifndef EBR_REALLOC
#define EBR_REALLOC(ptr, size) realloc(ptr, size)
#endif // EBR_REALLOC

void ebr_init(void);

// Annotates the critical sections in the mutators
void ebr_enter_cs(void);
void ebr_exit_cs(void);

void ebr_free(void* ptr, size_t size);

#endif // EBR_H

#ifdef EBR_IMPL
#undef EBR_IMPL

// for the time in the ebr entry
#define EBR_PINNED_BIT (1ull << 63ull)

typedef struct EBR_Entry EBR_Entry;
struct EBR_Entry {
    _Atomic(EBR_Entry*) next;
    _Atomic(uint64_t) time;

    // keep on a separate cacheline to avoid false sharing
    _Alignas(64) int id;
};

typedef struct EBR_FreeNode EBR_FreeNode;
struct EBR_FreeNode {
    EBR_FreeNode* next;
    // space to reclaim
    void* ptr;
    size_t size;
};

_Thread_local bool ebr_thread_init;
_Thread_local EBR_Entry ebr_thread_entry;

// concurrent free-list
_Atomic(EBR_FreeNode*) nbhs_free_list;

atomic_int ebr_count;
_Atomic(EBR_Entry*) ebr_list;

static int ebr_thread_fn(void* arg) {
    int state_count = ebr_count;
    uint64_t* states = EBR_REALLOC(NULL, state_count * sizeof(uint64_t));
    FOR_N(i, 0, state_count) {
        states[i] = 0;
    }

    EBR_FreeNode* last_free_list = NULL;
    for (;;) {
        // wait for the critical sections to advance, once
        // that happens we can choose to free things.
        for (EBR_Entry* list = atomic_load(&ebr_list); list; list = list->next) {
            if (list->id < state_count && (states[list->id] & EBR_PINNED_BIT)) {
                uint64_t before_t = states[list->id], now_t;
                do {
                    // idk, maybe this should be a better spinlock
                    now_t = atomic_load(&list->time);
                } while (before_t == now_t);

                states[list->id] = now_t;
            }
        }

        if (last_free_list) {
            EBR_FreeNode* list = last_free_list;
            while (list) {
                EBR_FreeNode* next = list->next;
                EBR_REALLOC(list, 0);
                list = next;
            }
            last_free_list = NULL;
        }

        // empty the free list, it's possible that the mutators are still watching it
        // so we can't free it until the next iteration.
        EBR_FreeNode* list = last_free_list = atomic_exchange(&nbhs_free_list, NULL);
        for (; list; list = list->next) {
            EBR_VIRTUAL_FREE(list->ptr, list->size);
        }

        // once we've advanced, we can acknowledge any new threads which spawned in the mean time
        int new_state_count = ebr_count;
        if (new_state_count != state_count) {
            states = EBR_REALLOC(NULL, state_count * sizeof(uint64_t));
            for (EBR_Entry* list = atomic_load(&ebr_list); list; list = list->next) {
                if (list->id >= state_count && list->id < new_state_count) {
                    states[list->id] = list->time;
                }
            }

            state_count = new_state_count;
        }
    }
    return 0;
}

static void ebr_once_init(void) {
    thrd_t t;
    thrd_create(&t, ebr_thread_fn, NULL);
}

static once_flag ebr_init_flag = ONCE_FLAG_INIT;
void ebr_init(void) {
    call_once(&ebr_init_flag, ebr_once_init);
}

void ebr_free(void* ptr, size_t size) {
    EBR_FreeNode* node = EBR_REALLOC(NULL, sizeof(EBR_FreeNode));
    node->ptr  = ptr;
    node->size = size;

    EBR_FreeNode* list = nbhs_free_list;
    do {
        node->next = list;
    } while (!atomic_compare_exchange_strong(&nbhs_free_list, &list, node));
}

void ebr_enter_cs(void) {
    if (!ebr_thread_init) {
        ebr_thread_init = true;
        ebr_thread_entry.id = ebr_count++;

        // add to ebr list, we never free this because i don't care
        // TODO(NeGate): i do care, this is a nightmare when threads die figure it out
        EBR_Entry* old = atomic_load_explicit(&ebr_list, memory_order_relaxed);
        do {
            ebr_thread_entry.next = old;
        } while (!atomic_compare_exchange_strong(&ebr_list, &old, &ebr_thread_entry));
    }

    // flips the top bit on
    uint64_t t = atomic_load_explicit(&ebr_thread_entry.time, memory_order_relaxed);
    atomic_store_explicit(&ebr_thread_entry.time, t + EBR_PINNED_BIT, memory_order_release);
}

// flips the top bit off AND increments time by one
void ebr_exit_cs(void) {
    uint64_t t = atomic_load_explicit(&ebr_thread_entry.time, memory_order_relaxed);
    atomic_store_explicit(&ebr_thread_entry.time, t + EBR_PINNED_BIT + 1, memory_order_release);
}

#endif // EBR_IMPL
