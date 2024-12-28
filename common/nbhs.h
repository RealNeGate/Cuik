////////////////////////////////
// NBHS - Non-blocking hashset
////////////////////////////////
// You wanna intern lots of things on lots of cores? this is for you. It's
// inspired by Cliff's non-blocking hashmap.
//
// To use it, you'll need to define NBHS_FN and then include the header:
//
//   #define NBHS_FN(n) XXX_hs_ ## n
//   #include <nbhs.h>
//
// This will compile implementations of the hashset using
//
//   bool NBHS_FN(cmp)(const void* a, const void* b);
//   uint32_t NBHS_FN(hash)(const void* a);
//
// The exported functions are:
//
//   void* NBHS_FN(get)(NBHS* hs, void* val);
//   void* NBHS_FN(intern)(NBHS* hs, void* val);
//   void NBHS_FN(resize_barrier)(NBHS* hs);
//
#ifndef NBHS_H
#define NBHS_H

#include <threads.h>
#include <stdint.h>
#include <stddef.h>
#include <stdatomic.h>

// Virtual memory allocation (since the tables are generally nicely page-size friendly)
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define NBHS_VIRTUAL_ALLOC(size)     VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE)
#define NBHS_VIRTUAL_FREE(ptr, size) VirtualFree(ptr, size, MEM_RELEASE)
#else
#include <sys/mman.h>

#define NBHS_VIRTUAL_ALLOC(size)     mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
#define NBHS_VIRTUAL_FREE(ptr, size) munmap(ptr, size)
#endif

// traditional heap ops
#ifndef NBHS_REALLOC
#define NBHS_REALLOC(ptr, size) realloc(ptr, size)
#endif // NBHS_REALLOC

// personal debooging stuff
#define NBHS__DEBOOGING 0

#if NBHS__DEBOOGING
#define NBHS__BEGIN(name)      spall_auto_buffer_begin(name, sizeof(name) - 1, NULL, 0)
#define NBHS__END()            spall_auto_buffer_end()
#else
#define NBHS__BEGIN(name)
#define NBHS__END()
#endif

// for the time in the ebr entry
#define NBHS_PINNED_BIT (1ull << 63ull)

enum {
    NBHS_LOAD_FACTOR = 75,
    NBHS_MOVE_AMOUNT = 128,
};

typedef struct NBHS_EBREntry {
    _Atomic(struct NBHS_EBREntry*) next;
    _Atomic(uint64_t) time;

    // keep on a separate cacheline to avoid false sharing
    _Alignas(64) int id;
} NBHS_EBREntry;

typedef struct NBHS_Table NBHS_Table;
struct NBHS_Table {
    _Atomic(NBHS_Table*) prev;

    uint32_t cap;

    // reciprocals to compute modulo
    uint64_t a, sh;

    // tracks how many entries have
    // been moved once we're resizing
    _Atomic uint32_t moved;
    _Atomic uint32_t move_done;
    _Atomic uint32_t count;

    _Atomic(void*) data[];
};

typedef struct {
    _Atomic(NBHS_Table*) latest;
} NBHS;

static size_t nbhs_compute_cap(size_t y) {
    // minimum capacity
    if (y < 512) {
        y = 512;
    } else {
        y = ((y + 1) / 3) * 4;
    }

    size_t cap = 1ull << (64 - __builtin_clzll(y - 1));
    return cap - (sizeof(NBHS_Table) / sizeof(void*));
}

static void nbhs_compute_size(NBHS_Table* table, size_t cap) {
    // reciprocals to compute modulo
    #if defined(__GNUC__) || defined(__clang__)
    table->sh = 64 - __builtin_clzll(cap);
    #else
    uint64_t sh = 0;
    while (cap > (1ull << sh)){ sh++; }
    table->sh = sh;
    #endif

    table->sh += 63 - 64;

    #if (defined(__GNUC__) || defined(__clang__)) && defined(__x86_64__)
    uint64_t d,e;
    __asm__("divq %[v]" : "=a"(d), "=d"(e) : [v] "r"(cap), "a"(cap - 1), "d"(1ull << table->sh));
    table->a = d;
    #elif defined(_MSC_VER)
    uint64_t rem;
    table->a = _udiv128(1ull << table->sh, cap - 1, cap, &rem);
    #else
    #error "Unsupported target"
    #endif

    table->cap = cap;
}

static NBHS nbhs_alloc(size_t initial_cap) {
    size_t cap = nbhs_compute_cap(initial_cap);
    NBHS_Table* table = NBHS_VIRTUAL_ALLOC(sizeof(NBHS_Table) + cap*sizeof(void*));
    nbhs_compute_size(table, cap);
    return (NBHS){ .latest = table };
}

static void nbhs_free(NBHS* hs) {
    NBHS_Table* curr = hs->latest;
    while (curr) {
        NBHS_Table* next = curr->prev;
        NBHS_VIRTUAL_FREE(curr, sizeof(NBHS_Table) + curr->cap*sizeof(void*));
        curr = next;
    }
}

// for spooky stuff
static void** nbhs_array(NBHS* hs)    { return (void**) hs->latest->data; }
static size_t nbhs_count(NBHS* hs)    { return hs->latest->count; }
static size_t nbhs_capacity(NBHS* hs) { return hs->latest->cap; }

#define nbhs_for(it, hs) for (void **it = nbhs_array(hs), **_end_ = &it[nbhs_capacity(hs)]; it != _end_; it++) if (*it != NULL)
#endif // NBHS_H

#ifdef NBHS_IMPL
_Thread_local bool nbhs_ebr_init;
_Thread_local NBHS_EBREntry nbhs_ebr;
_Atomic(int) nbhs_ebr_count;
_Atomic(NBHS_EBREntry*) nbhs_ebr_list;
#endif // NBHS_IMPL

// Templated implementation
#ifdef NBHS_FN
extern _Thread_local bool nbhs_ebr_init;
extern _Thread_local NBHS_EBREntry nbhs_ebr;
extern _Atomic(int) nbhs_ebr_count;
extern _Atomic(NBHS_EBREntry*) nbhs_ebr_list;

extern int nbhs_thread_fn(void*);

static size_t NBHS_FN(hash2index)(NBHS_Table* table, uint64_t h) {
    // MulHi(h, table->a)
    #if defined(__GNUC__) || defined(__clang__)
    uint64_t hi = (uint64_t) (((unsigned __int128)h * table->a) >> 64);
    #elif defined(_MSC_VER)
    uint64_t hi;
    _umul128(a, b, &hi);
    #else
    #error "Unsupported target"
    #endif

    uint64_t q  = hi >> table->sh;
    uint64_t q2 = h - (q * table->cap);

    assert(q2 == h % table->cap);
    return q2;
}

static void* NBHS_FN(raw_lookup)(NBHS* hs, NBHS_Table* table, uint32_t h, void* val) {
    size_t cap = table->cap;
    size_t first = NBHS_FN(hash2index)(table, h), i = first;
    do {
        void* entry = atomic_load(&table->data[i]);
        if (entry == NULL) {
            return NULL;
        } else if (NBHS_FN(cmp)(entry, val)) {
            return entry;
        }

        // inc & wrap around
        i = (i == cap-1) ? 0 : i + 1;
    } while (i != first);

    return NULL;
}

static void* NBHS_FN(raw_intern)(NBHS* hs, NBHS_Table* latest, NBHS_Table* prev, void* val) {
    // actually lookup & insert
    void* result = NULL;
    uint32_t h = NBHS_FN(hash)(val);
    for (;;) {
        size_t cap = latest->cap;
        size_t limit = (cap * NBHS_LOAD_FACTOR) / 100;
        if (prev == NULL && latest->count >= limit) {
            // make resized table, we'll amortize the moves upward
            size_t new_cap = nbhs_compute_cap(limit*2);

            NBHS_Table* new_top = NBHS_VIRTUAL_ALLOC(sizeof(NBHS_Table) + new_cap*sizeof(void*));
            nbhs_compute_size(new_top, new_cap);

            // CAS latest -> new_table, if another thread wins the race we'll use its table
            new_top->prev = latest;
            if (!atomic_compare_exchange_strong(&hs->latest, &latest, new_top)) {
                NBHS_VIRTUAL_FREE(new_top, sizeof(NBHS_Table) + new_cap*sizeof(void*));
                prev = atomic_load(&latest->prev);
            } else {
                prev   = latest;
                latest = new_top;

                // float s = sizeof(NBHS_Table) + new_cap*sizeof(void*);
                // printf("Resize: %.2f KiB (cap=%zu)\n", s / 1024.0f, new_cap);
            }
            continue;
        }

        size_t first = NBHS_FN(hash2index)(latest, h), i = first;
        do {
            void* entry = atomic_load(&latest->data[i]);
            if (entry == NULL) {
                void* to_write = val;
                if (__builtin_expect(prev != NULL, 0)) {
                    assert(prev->prev == NULL);
                    void* old = NBHS_FN(raw_lookup)(hs, prev, h, val);
                    if (old != NULL) {
                        to_write = old;
                    }
                }

                // fight to be the one to land into the modern table
                if (atomic_compare_exchange_strong(&latest->data[i], &entry, to_write)) {
                    result = to_write;

                    // count doesn't care that it's a migration, it's at least not replacing an existing
                    // slot in this version of the table.
                    atomic_fetch_add_explicit(&latest->count, 1, memory_order_relaxed);
                    break;
                }
            }

            if (NBHS_FN(cmp)(entry, val)) {
                return entry;
            }

            // inc & wrap around
            i = (i == cap-1) ? 0 : i + 1;
        } while (i != first);

        // if the table changed before our eyes, it means someone resized which sucks
        // but it just means we need to retry
        NBHS_Table* new_latest = atomic_load(&hs->latest);
        if (latest == new_latest && result != NULL) {
            return result;
        }

        latest = new_latest;
        prev   = atomic_load(&latest->prev);
    }
}

void NBHS_FN(raw_insert)(NBHS* hs, void* val) {
    NBHS_Table* table = hs->latest;
    size_t cap = table->cap;
    uint32_t h = NBHS_FN(hash)(val);
    size_t first = NBHS_FN(hash2index)(table, h), i = first;
    do {
        void* entry = atomic_load_explicit(&table->data[i], memory_order_relaxed);
        if (entry == NULL) {
            atomic_store_explicit(&table->data[i], val, memory_order_relaxed);
            atomic_fetch_add_explicit(&table->count, 1, memory_order_relaxed);
            return;
        }

        assert(!NBHS_FN(cmp)((void*) entry, val));

        // inc & wrap around
        i = (i == cap-1) ? 0 : i + 1;
    } while (i != first);

    abort();
}

// flips the top bit on
static void NBHS_FN(enter_pinned)(void) {
    uint64_t t = atomic_load_explicit(&nbhs_ebr.time, memory_order_relaxed);
    atomic_store_explicit(&nbhs_ebr.time, t + NBHS_PINNED_BIT, memory_order_release);
}

// flips the top bit off AND increments time by one
static void NBHS_FN(exit_pinned)(void) {
    uint64_t t = atomic_load_explicit(&nbhs_ebr.time, memory_order_relaxed);
    atomic_store_explicit(&nbhs_ebr.time, t + NBHS_PINNED_BIT + 1, memory_order_release);
}

NBHS_Table* NBHS_FN(move_items)(NBHS* hs, NBHS_Table* latest, NBHS_Table* prev, int items_to_move) {
    assert(prev);
    size_t cap = prev->cap;

    // snatch up some number of items
    uint32_t old, new;
    do {
        old = atomic_load(&prev->moved);
        if (old == cap) { return prev; }
        // cap the number of items to copy... by the cap
        new = old + items_to_move;
        if (new > cap) { new = cap; }
    } while (!atomic_compare_exchange_strong(&prev->moved, &(uint32_t){ old }, new));

    if (old == new) {
        return prev;
    }

    NBHS__BEGIN("copying old");
    for (size_t i = old; i < new; i++) {
        // either NULL or complete can go thru without waiting
        void* old_p = atomic_load(&prev->data[i]);
        if (old_p) {
            // we pass NULL for prev since we already know the entries exist in prev
            NBHS_FN(raw_intern)(hs, latest, NULL, old_p);
        }
    }
    NBHS__END();

    uint32_t done = atomic_fetch_add(&prev->move_done, new - old);
    done += new - old;

    assert(done <= cap);
    if (done == cap) {
        // dettach now
        NBHS__BEGIN("detach");
        latest->prev = NULL;
        NBHS_FN(exit_pinned)();

        int state_count = nbhs_ebr_count;
        uint64_t* states = NBHS_REALLOC(NULL, state_count * sizeof(uint64_t));

        NBHS__BEGIN("scan");
        NBHS_EBREntry* us = &nbhs_ebr;
        // "snapshot" the current statuses, once the other threads either advance or aren't in the
        // hashset functions we know we can free.
        for (NBHS_EBREntry* list = atomic_load(&nbhs_ebr_list); list; list = list->next) {
            // mark sure no ptrs refer to prev
            if (list != us && list->id < state_count) {
                states[list->id] = list->time;
            }
        }

        // important bit is that pointers can't be held across the critical sections, they'd need
        // to reload from `NBHS.latest`.
        //
        // Here's the states of our "epoch" critical section thingy:
        //
        // UNPINNED(id) -> PINNED(id) -> UNPINNED(id + 1) -> UNPINNED(id + 1) -> ...
        //
        // survey on if we can free the pointer if the status changed from X -> Y:
        //
        //   # YES: if we started unlocked then we weren't holding pointers in the first place.
        //   UNPINNED(A) -> PINNED(A)
        //   UNPINNED(A) -> UNPINNED(A)
        //   UNPINNED(A) -> UNPINNED(B)
        //
        //   # YES: if we're locked we need to wait until we've stopped holding pointers.
        //   PINNED(A)   -> PINNED(B)     we're a different call so we've let it go by now.
        //   PINNED(A)   -> UNPINNED(B)   we've stopped caring about the state of the pointer at this point.
        //
        //   # NO: we're still doing shit, wait a sec.
        //   PINNED(A)   -> PINNED(A)
        //
        // these aren't quite blocking the other threads, we're simply checking what their progress is concurrently.
        for (NBHS_EBREntry* list = atomic_load(&nbhs_ebr_list); list; list = list->next) {
            if (list != us && list->id < state_count && (states[list->id] & NBHS_PINNED_BIT)) {
                uint64_t before_t = states[list->id], now_t;
                do {
                    // idk, maybe this should be a better spinlock
                    now_t = atomic_load(&list->time);
                } while (before_t == now_t);
            }
        }
        NBHS__END();

        // no more refs, we can immediately free
        NBHS_VIRTUAL_FREE(prev, sizeof(NBHS_Table) + prev->cap*sizeof(void*));
        NBHS_REALLOC(states, 0);

        NBHS_FN(enter_pinned)();
        prev = NULL;
        NBHS__END();
    }
    return prev;
}

static void NBHS_FN(ebr_try_init)(void) {
    if (!nbhs_ebr_init) {
        NBHS__BEGIN("init");
        nbhs_ebr_init = true;
        nbhs_ebr.id = nbhs_ebr_count++;

        // add to ebr list, we never free this because i don't care
        // TODO(NeGate): i do care, this is a nightmare when threads die figure it out
        NBHS_EBREntry* old;
        do {
            old = atomic_load_explicit(&nbhs_ebr_list, memory_order_relaxed);
            nbhs_ebr.next = old;
        } while (!atomic_compare_exchange_strong(&nbhs_ebr_list, &old, &nbhs_ebr));
        NBHS__END();
    }
}

void* NBHS_FN(get)(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");

    assert(val);
    NBHS_FN(ebr_try_init)();

    // modifying the tables is possible now.
    NBHS_FN(enter_pinned)();
    NBHS_Table* latest = atomic_load(&hs->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHS_Table* prev = atomic_load(&latest->prev);
    if (prev) {
        prev = NBHS_FN(move_items)(hs, latest, prev, NBHS_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hs->latest);
        }
    }

    // just lookup into the tables, we don't need to reserve
    // actually lookup & insert
    void* result = NULL;
    uint32_t cap = latest->cap;
    uint32_t h = NBHS_FN(hash)(val);
    size_t first = NBHS_FN(hash2index)(latest, h), i = first;
    do {
        void* entry = atomic_load(&latest->data[i]);
        if (entry == NULL) {
            NBHS_Table* p = prev;
            while (p != NULL) {
                result = NBHS_FN(raw_lookup)(hs, prev, h, val);
                p = atomic_load_explicit(&p->prev, memory_order_relaxed);
            }
            break;
        }

        if (NBHS_FN(cmp)(entry, val)) {
            result = entry;
            break;
        }

        // inc & wrap around
        i = (i == cap-1) ? 0 : i + 1;
    } while (i != first);

    NBHS_FN(exit_pinned)();
    NBHS__END();
    return result;
}

void* NBHS_FN(intern)(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");

    assert(val);
    NBHS_FN(ebr_try_init)();

    NBHS_FN(enter_pinned)();
    NBHS_Table* latest = atomic_load(&hs->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHS_Table* prev = atomic_load(&latest->prev);
    if (prev) {
        prev = NBHS_FN(move_items)(hs, latest, prev, NBHS_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hs->latest);
        }
    }

    void* result = NBHS_FN(raw_intern)(hs, latest, prev, val);

    NBHS_FN(exit_pinned)();
    NBHS__END();
    return result;
}

// waits for all items to be moved up before continuing
void NBHS_FN(resize_barrier)(NBHS* hs) {
    NBHS__BEGIN("intern");
    NBHS_FN(ebr_try_init)();

    NBHS_FN(enter_pinned)();
    NBHS_Table *prev, *latest = atomic_load(&hs->latest);
    while (prev = atomic_load(&latest->prev), prev != NULL) {
        NBHS_FN(move_items)(hs, latest, prev, prev->cap);
    }

    NBHS_FN(exit_pinned)();
    NBHS__END();
}

#undef NBHS_FN
#endif // NBHS_FN
