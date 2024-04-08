////////////////////////////////
// NBHS - Non-blocking hashset
////////////////////////////////
// You wanna intern lots of things on lots of cores? this is for you. It's
// inspired by Cliff's non-blocking hashmap.
#ifndef NBHS_H
#define NBHS_H

#include <stdint.h>
#include <stddef.h>
#include <stdatomic.h>

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
#define NBHS__BEGIN(name) spall_auto_buffer_begin(#name, sizeof(#name) - 1, NULL, 0)
#define NBHS__END()       spall_auto_buffer_end()
#endif

// private constants (idk if i wanna undef these but don't touch i guess?)
#define NBHS_RESERVE_BIT (1ull << 63ull)

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

// single hazard pointer per thread
typedef struct NBHS_HazardEntry {
    _Atomic(struct NBHS_HazardEntry*) next;

    // normal insertion and work:
    //   0 - latest
    //   1 - prev
    // inside the prev moving code, it does an insert so
    // it needs separate hazard entries:
    //   2 - latest
    //   3 - prev
    // misc:
    //   4 - temporary
    _Atomic(NBHS_Table*) ptr[5];
} NBHS_HazardEntry;

static _Thread_local bool nbhs_hazard_init;
static _Thread_local NBHS_HazardEntry nbhs_hazard;
static _Atomic(NBHS_HazardEntry*) nbhs_hazard_list;

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

static NBHS_Table* nbhs_hazard_access(_Atomic(NBHS_Table*)* table, int slot) {
    NBHS_Table* val = atomic_load(table);
    for (;;) {
        // mark as hazard
        nbhs_hazard.ptr[slot] = val;
        // check if it's been invalidated since the previous load, if so
        // then undo hazard and try load again.
        NBHS_Table* after = atomic_load(table);
        if (val == after) {
            return val;
        }
        val = after;
    }
}

static void* nbhs_raw_intern(NBHS* hs, NBHS_Table* latest, void* val, int hazard_slot) {
    // resize on 50% load factor and we're not in the moving process rn
    uint32_t threshold = (1 << latest->exp) / 2;
    if (latest->count >= threshold && atomic_load(&latest->prev) == NULL) {
        // make resized table, we'll amortize the moves upward
        size_t new_cap = 1ull << (latest->exp + 1);

        NBHS_Table* new_top = hs->alloc_mem(sizeof(NBHS_Table) + new_cap*sizeof(uintptr_t));
        new_top->exp = latest->exp + 1;

        // CAS latest -> new_table, if another thread wins the race we'll use its table
        new_top->prev = latest;
        if (!atomic_compare_exchange_strong(&hs->latest, &latest, new_top)) {
            hs->free_mem(new_top, sizeof(NBHS_Table) + new_cap*sizeof(uintptr_t));
        } else {
            latest = new_top;
        }

        nbhs_hazard.ptr[hazard_slot] = latest;
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
                // if we spot a NULL, the entry has never been in this table, that doesn't mean
                // it's not appeared already so we'll only reserve the slot until we figure that
                // out.
                //
                // everyone else will see our pointer marked as reserved and are free to wait on
                // us if they match, if not they can move along.
                if (atomic_compare_exchange_strong(&latest->data[i], &entry, reserved_form)) {
                    void* old = NULL;
                    NBHS_Table* curr = nbhs_hazard_access(&latest->prev, hazard_slot + 1);
                    if (curr != NULL) {
                        // should only be one previous table
                        assert(curr->prev == NULL);

                        old = nbhs_raw_lookup(hs, curr, h, val);
                        nbhs_hazard.ptr[hazard_slot + 1] = NULL;
                    }

                    // count doesn't care that it's a migration, it's at least not replacing an existing
                    // slot in this version of the table.
                    atomic_fetch_add_explicit(&latest->count, 1, memory_order_relaxed);

                    if (old != NULL) {
                        atomic_exchange(&latest->data[i], (uintptr_t) old);
                        result = old;
                    } else {
                        // no entry was found, good (reserved -> val)
                        atomic_exchange(&latest->data[i], (uintptr_t) val);
                        result = val;
                    }
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
        NBHS_Table* new_latest = nbhs_hazard_access(&hs->latest, 4);
        if (latest == new_latest && result != NULL) {
            nbhs_hazard.ptr[hazard_slot] = NULL;
            nbhs_hazard.ptr[4] = NULL;
            return result;
        }

        // move to the correct hazard slot
        nbhs_hazard.ptr[hazard_slot] = new_latest;
        nbhs_hazard.ptr[4] = NULL;
        latest = new_latest;
    }
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

void* nbhs_intern(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");
    if (!nbhs_hazard_init) {
        NBHS__BEGIN("init");
        nbhs_hazard_init = true;

        // add to hazard list, we never free this because i don't care
        NBHS_HazardEntry* old;
        do {
            old = atomic_load_explicit(&nbhs_hazard_list, memory_order_relaxed);
            nbhs_hazard.next = old;
        } while (!atomic_compare_exchange_strong(&nbhs_hazard_list, &old, &nbhs_hazard));
        NBHS__END();
    }

    NBHS_Table* latest = nbhs_hazard_access(&hs->latest, 0);

    // if there's earlier versions of the table we can move up entries as we go
    // along.
    NBHS_Table* prev = nbhs_hazard_access(&latest->prev, 1);
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
                nbhs_raw_intern(hs, latest, (void*) decision, 2);
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

            NBHS__BEGIN("scan");
            // wait for all refs to stop holding on (just read the hazards until they don't match)
            NBHS_HazardEntry* us = &nbhs_hazard;
            for (NBHS_HazardEntry* list = atomic_load(&nbhs_hazard_list); list; list = list->next) {
                // mark sure no ptrs refer to prev
                if (us != list) for (size_t i = 0; i < (sizeof(list->ptr)/sizeof(void*)); i++) {
                    while (list->ptr[i] == prev) {
                        // spin-lock
                    }
                }
            }
            NBHS__END();

            // no one's referring to it and no one will ever refer to it again
            hs->free_mem(prev, sizeof(NBHS_Table) + (1ull<<prev->exp)*sizeof(uintptr_t));
            NBHS__END();
        }
        skip:;
    }
    nbhs_hazard.ptr[1] = NULL; // ok it can be freed now

    void* result = nbhs_raw_intern(hs, latest, val, 0);
    NBHS__END();
    return result;
}

#endif // NBHS_IMPL

