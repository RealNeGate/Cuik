////////////////////////////////
// EBR - Epoch-based reclamation
////////////////////////////////
#ifndef EBR_H
#define EBR_H

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

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
void ebr_deinit(void);

// Annotates the critical sections in the mutators
void ebr_enter_cs(void);
void ebr_exit_cs(void);

void ebr_free(void* ptr, size_t size);

#endif // EBR_H

#ifdef EBR_IMPL
#undef EBR_IMPL

// for the time in the ebr entry
#define EBR_PINNED_BIT (1ull << 63ull)

#ifdef _WIN32
#include <tlhelp32.h>
#endif

#define EBR_DEBOOGING 0

#if EBR_DEBOOGING
void cuikperf_thread_start(void);
void cuikperf_thread_stop(void);
void cuikperf_region_start(const char* label, const char* extra);
void cuikperf_region_end(void);
#endif

#if CUIK_ALLOW_THREADS
typedef struct EBR_Entry EBR_Entry;
struct EBR_Entry {
    _Atomic(EBR_Entry*) next;
    _Atomic(uint64_t) time;

    // OS-specific thread handle
    #ifdef _WIN32
    uint32_t os_handle;
    #endif

    bool gc_mark;

    // keep on a separate cacheline to avoid false sharing
    _Alignas(64) uint64_t checkpoint_t;
};

typedef struct EBR_FreeNode EBR_FreeNode;
struct EBR_FreeNode {
    EBR_FreeNode* next;
    // space to reclaim
    void* ptr;
    size_t size;
};

static _Thread_local bool ebr_thread_init;
static _Thread_local EBR_Entry* ebr_thread_entry;

// concurrent free-list
static _Atomic(EBR_FreeNode*) ebr_free_list;
static _Atomic(EBR_Entry*) ebr_list;
static thrd_t ebr_thread;

static _Atomic bool ebr_running;
static mtx_t ebr_mutex;
static cnd_t ebr_exit_signal;

void ebr_deinit(void) {
    if (ebr_running) {
        ebr_running = false;
        cnd_broadcast(&ebr_exit_signal);

        int res;
        thrd_join(ebr_thread, &res);
    }
}

static int ebr_thread_fn(void* arg) {
    EBR_FreeNode* last_free_list = NULL;

    #if EBR_DEBOOGING
    cuikperf_thread_start();
    #endif

    ebr_running = true;
    atexit(ebr_deinit);

    while (ebr_running) {
        #if EBR_DEBOOGING
        cuikperf_region_start("cycle", NULL);
        #endif

        // clear the free list, then we wait for the checkpoint before
        // freeing the memory.
        EBR_FreeNode* free_list = atomic_exchange(&ebr_free_list, NULL);
        // must be loaded after, we need "at least" all the mutator
        // threads visible who could've written to free_list, we can scan more.
        EBR_Entry* thread_list = atomic_load(&ebr_list);

        #if EBR_DEBOOGING
        cuikperf_region_start("Checkpoint", NULL);
        #endif

        // wait for the critical sections to advance, once
        // that happens we can choose to free things.
        for (EBR_Entry* list = thread_list; list; list = list->next) {
            uint64_t before_t = list->checkpoint_t;
            if (before_t & EBR_PINNED_BIT) {
                uint64_t now_t, tries = 0;
                do {
                    // once we're at this a bunch of times, we start to yield
                    if (tries > 4) {
                        thrd_yield();
                    }

                    // idk, maybe this should be a better spinlock
                    now_t = list->time;
                    tries++;
                } while (before_t == now_t);

                list->checkpoint_t = now_t;
            }

            list->gc_mark = false;
        }

        #if EBR_DEBOOGING
        cuikperf_region_end();
        cuikperf_region_start("Reclaim memory", NULL);
        #endif

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
        for (; free_list; free_list = free_list->next) {
            EBR_VIRTUAL_FREE(free_list->ptr, free_list->size);
        }
        last_free_list = free_list;

        #if EBR_DEBOOGING
        cuikperf_region_end();
        cuikperf_region_start("Thread GC", NULL);
        #endif

        // mark the dead threads and then remove them from the list
        #ifdef _WIN32
        uint32_t pid = GetCurrentProcessId();
        HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, pid);

        // mark phase
        THREADENTRY32 te32 = { .dwSize = sizeof(THREADENTRY32) };

        #if EBR_DEBOOGING
        cuikperf_region_start("Thread32First", NULL);
        #endif

        if (Thread32First(snapshot, &te32)) {
            do {
                #if EBR_DEBOOGING
                cuikperf_region_end();
                #endif

                if (!ebr_running) {
                    break;
                }

                if (te32.th32OwnerProcessID == pid) {
                    // probably slow, don't care, in practice there's not enough threads to worry about it
                    for (EBR_Entry* list = thread_list; list; list = list->next) {
                        if (list->os_handle == te32.th32ThreadID) {
                            list->gc_mark = true;
                            break;
                        }
                    }
                }

                #if EBR_DEBOOGING
                cuikperf_region_start("Thread32Next", NULL);
                #endif
            } while (Thread32Next(snapshot, &te32));

            #if EBR_DEBOOGING
            cuikperf_region_end();
            #endif
        }
        CloseHandle(snapshot);

        // once we've advanced, we can acknowledge any thread deaths
        EBR_Entry* list = thread_list;
        EBR_Entry* prev = NULL;
        while (list) {
            EBR_Entry* next = list->next;

            // we can't free the first entry in the list without causing issues so we just
            // mark it as dead for now, next cycle we'll try again
            if (!list->gc_mark && prev != NULL) {
                // printf("THREAD %u DIED!\n", list->os_handle);

                // object was marked as dead, let's remove the entry
                atomic_store_explicit(&prev->next, next, memory_order_release);

                // we can free this immediately, it begin reclaimed early is actually ok.
                // if reclaimed early AND it's at the start of the list then things work
                // out as if the original never died. since we don't read the ebr_list root
                // node at all (just the pointer) then we wouldn't crash from holding this
                // potentially invalid ptr.
                EBR_REALLOC(list, 0);
            } else {
                prev = list;
            }
            list = next;
        }
        #endif

        #if EBR_DEBOOGING
        cuikperf_region_end();
        cuikperf_region_end();
        #endif

        // we're in no rush to free to might as well yield some time
        mtx_lock(&ebr_mutex);
        cnd_timedwait(&ebr_exit_signal, &ebr_mutex, &(struct timespec){.tv_sec=0, .tv_nsec=500000000});
        mtx_unlock(&ebr_mutex);
    }

    #if EBR_DEBOOGING
    cuikperf_thread_stop();
    #endif

    return 0;
}

static void ebr_once_init(void) {
    mtx_init(&ebr_mutex, mtx_plain);
    cnd_init(&ebr_exit_signal);
    thrd_create(&ebr_thread, ebr_thread_fn, NULL);
}

static once_flag ebr_init_flag = ONCE_FLAG_INIT;
void ebr_init(void) {
    call_once(&ebr_init_flag, ebr_once_init);
}

void ebr_free(void* ptr, size_t size) {
    EBR_FreeNode* node = EBR_REALLOC(NULL, sizeof(EBR_FreeNode));
    node->ptr  = ptr;
    node->size = size;

    EBR_FreeNode* list = ebr_free_list;
    do {
        node->next = list;
    } while (!atomic_compare_exchange_strong(&ebr_free_list, &list, node));
}

void ebr_enter_cs(void) {
    if (ebr_thread_entry == NULL) {
        EBR_Entry* new_node = EBR_REALLOC(NULL, sizeof(EBR_Entry));
        new_node->time = 0;
        #ifdef _WIN32
        new_node->os_handle = GetCurrentThreadId();
        #endif
        new_node->checkpoint_t = 0;
        ebr_thread_entry = new_node;

        // add to ebr list, we never free this because i don't care
        // TODO(NeGate): i do care, this is a nightmare when threads die figure it out
        EBR_Entry* old = atomic_load_explicit(&ebr_list, memory_order_relaxed);
        do {
            ebr_thread_entry->next = old;
        } while (!atomic_compare_exchange_strong(&ebr_list, &old, new_node));
    }

    // flips the top bit on
    uint64_t t = atomic_load_explicit(&ebr_thread_entry->time, memory_order_relaxed);
    atomic_store_explicit(&ebr_thread_entry->time, t + EBR_PINNED_BIT, memory_order_release);
}

// flips the top bit off AND increments time by one
void ebr_exit_cs(void) {
    uint64_t t = atomic_load_explicit(&ebr_thread_entry->time, memory_order_relaxed);
    atomic_store_explicit(&ebr_thread_entry->time, t + EBR_PINNED_BIT + 1, memory_order_release);
}
#else
// No threads? we can immediately free things then
void ebr_init(void) {}
void ebr_deinit(void) {}

void ebr_enter_cs(void) {}
void ebr_exit_cs(void) {}

void ebr_free(void* ptr, size_t size) {
    EBR_VIRTUAL_FREE(ptr, size);
}

#endif

#endif // EBR_IMPL
