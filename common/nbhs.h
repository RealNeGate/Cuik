////////////////////////////////
// NBHS - Non-blocking hashset
////////////////////////////////
// You wanna intern lots of things on lots of cores? this is for you. It's
// inspired by Cliff's non-blocking hashmap.
#ifndef NBHS_H
#define NBHS_H

typedef void*    (*NBHS_AllocZeroMem)(size_t size);
typedef void     (*NBHS_FreeMem)(void* ptr, size_t size);
typedef uint32_t (*NBHS_Hash)(const void* a);
typedef bool     (*NBHS_Compare)(const void* a, const void* b);

typedef struct NBHS_Table NBHS_Table;
typedef struct {
    NBHS_AllocZeroMem alloc_mem;
    NBHS_FreeMem      free_mem;
    NBHS_Compare      compare;
    NBHS_Hash         hash;

    _Atomic(NBHS_Table*) latest;
} NBHS;

NBHS nbhs_alloc(size_t initial_cap, NBHS_AllocZeroMem alloc_mem, NBHS_FreeMem free_mem, NBHS_Compare compare, NBHS_Hash hash);
void nbhs_free(NBHS* hs);
void* nbhs_intern(NBHS* hs, void* val);

// thread-unsafe insert, useful during init since it's faster
// than the thread-safe stuff.
void nbhs_raw_insert(NBHS* hs, void* val);

#endif // NBHS_H

#ifdef NBHS_IMPL

// personal debooging stuff
#if 1
#define NBHS__BEGIN(name)
#define NBHS__END()
#else
#define NBHS__BEGIN(name) spall_auto_buffer_begin(name, sizeof(name) - 1, NULL, 0)
#define NBHS__END()       spall_auto_buffer_end()
#endif

// private constants (idk if i wanna undef these but don't touch i guess?)
#define NBHS_RESERVE_BIT (1ull << 63ull)

// for the time in the ebr entry
#define NBHS_LOCKED_BIT (1ull << 63ull)

struct NBHS_Table {
    _Atomic(NBHS_Table*) prev;

    uint32_t exp;

    // tracks how many entries have
    // been moved once we're resizing
    _Atomic uint32_t moved;
    _Atomic uint32_t move_done;
    _Atomic uint32_t count;

    _Atomic(uintptr_t) data[];
};

typedef struct NBHS_EBREntry {
    _Atomic(struct NBHS_EBREntry*) next;
    _Atomic(uint64_t) time;

    // keep on a separate cacheline to avoid false sharing
    _Alignas(64) int id;
} NBHS_EBREntry;

static _Thread_local bool nbhs_ebr_init;
static _Thread_local NBHS_EBREntry nbhs_ebr;
static _Atomic(int) nbhs_ebr_count;
static _Atomic(NBHS_EBREntry*) nbhs_ebr_list;

static void* nbhs__alloc_zero_mem(size_t s) { return calloc(1, s); }
static void nbhs__free_mem(void* ptr, size_t s) { free(ptr); }

static void* nbhs_try_entry(NBHS* hs, NBHS_Table* table, int i, void* val, uintptr_t entry) {
    void* entry_p = (void*) (entry & ~NBHS_RESERVE_BIT);
    if (hs->compare(entry_p, val)) {
        if ((entry & NBHS_RESERVE_BIT) == 0) {
            // not reserved, yay
            return entry_p;
        } else {
            // either NULL or complete can go thru without waiting
            uintptr_t decision;
            while (decision = atomic_load(&table->data[i]), decision & NBHS_RESERVE_BIT) {
                // idk if i should do a nicer wait than a spin-lock
            }
            return (void*) decision;
        }
    } else {
        return NULL;
    }
}

static void* nbhs_raw_lookup(NBHS* hs, NBHS_Table* table, uint32_t h, void* val) {
    size_t mask = (1 << table->exp) - 1;
    size_t first = h & mask, i = first;
    do {
        uintptr_t entry = atomic_load(&table->data[i]);
        if (entry == 0) {
            return NULL;
        }

        void* k = nbhs_try_entry(hs, table, i, val, entry);
        if (k != NULL) {
            return k;
        }
        i = (i + 1) & mask;
    } while (i != first);

    return NULL;
}

static void* nbhs_raw_intern(NBHS* hs, NBHS_Table* latest, NBHS_Table* prev, void* val) {
    // resize on 50% load factor and we're not in the moving process rn
    uint32_t threshold = (1 << latest->exp) / 2;
    if (__builtin_expect(latest->count >= threshold && prev == NULL, 0)) {
        // make resized table, we'll amortize the moves upward
        size_t new_cap = 1ull << (latest->exp + 1);

        NBHS_Table* new_top = hs->alloc_mem(sizeof(NBHS_Table) + new_cap*sizeof(uintptr_t));
        new_top->exp  = latest->exp + 1;

        // CAS latest -> new_table, if another thread wins the race we'll use its table
        new_top->prev = latest;
        if (!atomic_compare_exchange_strong(&hs->latest, &latest, new_top)) {
            hs->free_mem(new_top, sizeof(NBHS_Table) + new_cap*sizeof(uintptr_t));
        } else {
            prev   = latest;
            latest = new_top;
        }
    }

    // actually lookup & insert
    uint32_t h = hs->hash(val);
    uintptr_t reserved_form = (uintptr_t)val | NBHS_RESERVE_BIT;
    void* result = NULL;
    for (;;) {
        size_t mask = (1 << latest->exp) - 1;
        size_t first = h & mask, i = first;

        do {
            uintptr_t entry = atomic_load(&latest->data[i]);
            if (entry == 0) {
                // we insert the reserved_form to lock the entry until we've checked
                // if the entry already existed in the previous table, if there's no
                // previous table we don't need this.
                uintptr_t to_write = prev ? reserved_form : (uintptr_t) val;
                if (atomic_compare_exchange_strong(&latest->data[i], &entry, to_write)) {
                    // count doesn't care that it's a migration, it's at least not replacing an existing
                    // slot in this version of the table.
                    atomic_fetch_add_explicit(&latest->count, 1, memory_order_relaxed);

                    if (prev != NULL) {
                        // should only be one previous table
                        assert(prev->prev == NULL);
                        void* old = nbhs_raw_lookup(hs, prev, h, val);

                        if (old != NULL) {
                            // migrate the entry up
                            atomic_exchange(&latest->data[i], (uintptr_t) old);
                            result = old;
                            break;
                        }
                    }

                    // we're the first occurrence
                    atomic_exchange(&latest->data[i], (uintptr_t) val);
                    result = val;
                    break;
                }
            }

            void* k = nbhs_try_entry(hs, latest, i, val, entry);
            if (k != NULL) {
                return k;
            }

            i = (i + 1) & mask;
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

void nbhs_raw_insert(NBHS* hs, void* val) {
    NBHS_Table* table = hs->latest;
    uint32_t h = hs->hash(val);
    size_t mask = (1 << table->exp) - 1;
    size_t first = h & mask, i = first;
    do {
        uintptr_t entry = atomic_load_explicit(&table->data[i], memory_order_relaxed);
        if (entry == 0) {
            atomic_store_explicit(&table->data[i], (uintptr_t) val, memory_order_relaxed);
            atomic_fetch_add_explicit(&table->count, 1, memory_order_relaxed);
            return;
        }

        assert(!hs->compare((void*) entry, val));
        i = (i + 1) & mask;
    } while (i != first);

    abort();
}

NBHS nbhs_alloc(size_t initial_cap, NBHS_AllocZeroMem alloc_mem, NBHS_FreeMem free_mem, NBHS_Compare compare, NBHS_Hash hash) {
    if (alloc_mem == NULL) {
        assert(free_mem == NULL);
        alloc_mem = nbhs__alloc_zero_mem;
        free_mem  = nbhs__free_mem;
    }

    size_t exp = 64 - __builtin_clzll(initial_cap - 1);
    NBHS_Table* table = alloc_mem(sizeof(NBHS_Table) + (1ull << exp)*sizeof(uintptr_t));
    table->exp = exp;
    return (NBHS){
        .alloc_mem  = alloc_mem,
        .free_mem   = free_mem,
        .compare    = compare,
        .hash       = hash,
        .latest     = table
    };
}

void nbhs_free(NBHS* hs) {
    NBHS_Table* curr = hs->latest;
    while (curr) {
        NBHS_Table* next = curr->prev;
        hs->free_mem(curr, sizeof(NBHS_Table) + (1ull*curr->exp)*sizeof(uintptr_t));
        curr = next;
    }
}

// flips the top bit on
static void nbhs_enter_critsec(void) { atomic_fetch_add(&nbhs_ebr.time, NBHS_LOCKED_BIT);     }
// flips the top bit off AND increments time by one
static void nbhs_exit_critsec(void)  { atomic_fetch_add(&nbhs_ebr.time, NBHS_LOCKED_BIT + 1); }

void* nbhs_intern(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");
    if (!nbhs_ebr_init) {
        NBHS__BEGIN("init");
        nbhs_ebr_init = true;

        // add to ebr list, we never free this because i don't care
        NBHS_EBREntry* old;
        do {
            old = atomic_load_explicit(&nbhs_ebr_list, memory_order_relaxed);
            nbhs_ebr.next = old;
        } while (!atomic_compare_exchange_strong(&nbhs_ebr_list, &old, &nbhs_ebr));
        nbhs_ebr.id = nbhs_ebr_count++;
        NBHS__END();
    }

    // enter critical section, modifying the tables is possible now.
    nbhs_enter_critsec();

    NBHS_Table* latest = atomic_load(&hs->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHS_Table* prev = atomic_load(&latest->prev);
    if (prev) {
        size_t cap = 1ull << prev->exp;

        // snatch up some number of items
        uint32_t old, new;
        do {
            old = atomic_load(&prev->moved);
            if (old == cap) { goto skip; }
            // cap the number of items to copy... by the cap
            new = old + 32;
            if (new > cap) { new = cap; }
        } while (!atomic_compare_exchange_strong(&prev->moved, &(uint32_t){ old }, new));

        NBHS__BEGIN("copying old");
        for (size_t i = old; i < new; i++) {
            // either NULL or complete can go thru without waiting
            uintptr_t decision;
            while (decision = atomic_load(&prev->data[i]), decision & NBHS_RESERVE_BIT) {
                // idk if i should do a nicer wait than a spin-lock
            }

            if (decision) {
                // we pass NULL for prev since we already know the entries exist in prev
                nbhs_raw_intern(hs, latest, NULL, (void*) decision);
            }
        }
        NBHS__END();

        uint32_t done = atomic_fetch_add(&prev->move_done, new - old);
        done += new - old;

        assert(done <= cap);
        if (done == cap) {
            // dettach now
            NBHS__BEGIN("detach");

            assert(prev->prev == NULL);
            latest->prev = NULL;

            // since we're freeing at the moment, we don't want to block up other freeing threads
            nbhs_exit_critsec();

            NBHS__BEGIN("scan");
            int state_count = nbhs_ebr_count;
            uint64_t* states = hs->alloc_mem(state_count * sizeof(uint64_t));

            // check current state, once the other threads either advance or aren't in the
            // lookup function we know we can free.
            NBHS_EBREntry* us = &nbhs_ebr;
            for (NBHS_EBREntry* list = atomic_load(&nbhs_ebr_list); list; list = list->next) {
                // mark sure no ptrs refer to prev
                if (us != list && list->id < state_count) {
                    states[list->id] = list->time;
                }
            }

            // wait on each and every thread to make progress or not be in a critical section at the time.
            for (NBHS_EBREntry* list = atomic_load(&nbhs_ebr_list); list; list = list->next) {
                // mark sure no ptrs refer to prev
                if (us != list && list->id < state_count && (states[list->id] & NBHS_LOCKED_BIT)) {
                    uint64_t before_t = states[list->id], now_t;
                    do {
                        // idk, maybe this should be a better spinlock
                        now_t = atomic_load(&list->time);
                    } while (before_t == now_t);
                }
            }

            hs->free_mem(states, state_count * sizeof(uint64_t));
            NBHS__END();

            // no more refs, we can immediately free
            hs->free_mem(prev, sizeof(NBHS_Table) + (1ull<<prev->exp)*sizeof(uintptr_t));

            nbhs_enter_critsec();
            prev = NULL;

            NBHS__END();
        }
        skip:;
    }

    void* result = nbhs_raw_intern(hs, latest, prev, val);

    nbhs_exit_critsec();
    NBHS__END();
    return result;
}

#endif // NBHS_IMPL
