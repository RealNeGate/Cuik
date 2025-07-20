////////////////////////////////
// NBHM - Non-blocking hashmap
////////////////////////////////
// You wanna intern lots of things on lots of cores? this is for you. It's
// inspired by Cliff's non-blocking hashmap.
//
// To use it, you'll need to define NBHM_FN and then include the header:
//
//   #define NBHM_FN(n) XXX_hm_ ## n
//   #include <nbhm.h>
//
// This will compile implementations of the hashset using
//
//   bool NBHM_FN(cmp)(const void* a, const void* b);
//   uint32_t NBHM_FN(hash)(const void* a);
//
// The exported functions are:
//
//   void* NBHM_FN(get)(NBHM* hm, void* key);
//   void* NBHM_FN(put)(NBHM* hm, void* key, void* val);
//   void* NBHM_FN(put_if_null)(NBHM* hm, void* key, void* val);
//   void NBHM_FN(resize_barrier)(NBHM* hm);
//
#ifndef NBHM_H
#define NBHM_H

#include <threads.h>
#include <stdint.h>
#include <stddef.h>
#include <stdatomic.h>

#include "ebr.h"

// personal debooging stuff
#define NBHM__DEBOOGING 0

#if NBHM__DEBOOGING
#define NBHM__BEGIN(name)      spall_auto_buffer_begin(name, sizeof(name) - 1, NULL, 0)
#define NBHM__END()            spall_auto_buffer_end()
#else
#define NBHM__BEGIN(name)
#define NBHM__END()
#endif

// for the time in the ebr entry
#define NBHM_PRIME_BIT  (1ull << 63ull)

enum {
    NBHM_LOAD_FACTOR = 75,
    NBHM_MOVE_AMOUNT = 256,
};

typedef struct NBHM_EBREntry {
    _Atomic(struct NBHM_EBREntry*) next;
    _Atomic(uint64_t) time;

    // keep on a separate cacheline to avoid false sharing
    _Alignas(64) int id;
} NBHM_EBREntry;

typedef struct {
    _Atomic(void*) key;
    _Atomic(void*) val;
} NBHM_Entry;

typedef struct NBHM_Table NBHM_Table;
struct NBHM_Table {
    _Atomic(NBHM_Table*) prev;

    uint32_t cap;

    // reciprocals to compute modulo
    uint64_t a, sh;

    // tracks how many entries have
    // been moved once we're resizing
    _Atomic uint32_t moved;
    _Atomic uint32_t move_done;
    _Atomic uint32_t count;

    NBHM_Entry data[];
};

typedef struct {
    _Atomic(NBHM_Table*) latest;
} NBHM;

typedef struct NBHM_FreeNode NBHM_FreeNode;
struct NBHM_FreeNode {
    _Atomic(NBHM_FreeNode*) next;
    NBHM_Table* table;
};

static size_t nbhm_compute_cap(size_t y) {
    // minimum capacity
    if (y < 256) {
        y = 256;
    } else {
        y = ((y + 1) / 3) * 4;
    }

    size_t cap = 1ull << (64 - __builtin_clzll(y - 1));
    return cap - (sizeof(NBHM_Table) / sizeof(NBHM_Entry));
}

#ifndef NEGATE__DIV128_IMPL
#define NEGATE__DIV128_IMPL
// (X + Y) / Z = int(X/Z) + int(Y/Z) + (mod(X,Z) + mod(Y,Z)/Z
static uint64_t negate__div128(uint64_t numhi, uint64_t numlo, uint64_t den, uint64_t* out_rem) {
    // https://github.com/ridiculousfish/libdivide/blob/master/libdivide.h (libdivide_128_div_64_to_64)
    //
    // We work in base 2**32.
    // A uint32 holds a single digit. A uint64 holds two digits.
    // Our numerator is conceptually [num3, num2, num1, num0].
    // Our denominator is [den1, den0].
    const uint64_t b = ((uint64_t)1 << 32);

    // Check for overflow and divide by 0.
    if (numhi >= den) {
        if (out_rem) *out_rem = ~0ull;
        return ~0ull;
    }

    // Determine the normalization factor. We multiply den by this, so that its leading digit is at
    // least half b. In binary this means just shifting left by the number of leading zeros, so that
    // there's a 1 in the MSB.
    // We also shift numer by the same amount. This cannot overflow because numhi < den.
    // The expression (-shift & 63) is the same as (64 - shift), except it avoids the UB of shifting
    // by 64. The funny bitwise 'and' ensures that numlo does not get shifted into numhi if shift is
    // 0. clang 11 has an x86 codegen bug here: see LLVM bug 50118. The sequence below avoids it.
    int shift = __builtin_clzll(den) - 1;
    den <<= shift;
    numhi <<= shift;
    numhi |= (numlo >> (-shift & 63)) & (uint64_t)(-(int64_t)shift >> 63);
    numlo <<= shift;

    // Extract the low digits of the numerator and both digits of the denominator.
    uint32_t num1 = (uint32_t)(numlo >> 32);
    uint32_t num0 = (uint32_t)(numlo & 0xFFFFFFFFu);
    uint32_t den1 = (uint32_t)(den >> 32);
    uint32_t den0 = (uint32_t)(den & 0xFFFFFFFFu);

    // We wish to compute q1 = [n3 n2 n1] / [d1 d0].
    // Estimate q1 as [n3 n2] / [d1], and then correct it.
    // Note while qhat may be 2 digits, q1 is always 1 digit.
    uint64_t qhat = numhi / den1;
    uint64_t rhat = numhi % den1;
    uint64_t c1 = qhat * den0;
    uint64_t c2 = rhat * b + num1;
    if (c1 > c2) qhat -= (c1 - c2 > den) ? 2 : 1;
    uint32_t q1 = (uint32_t)qhat;

    // Compute the true (partial) remainder.
    uint64_t rem = numhi * b + num1 - q1 * den;

    // We wish to compute q0 = [rem1 rem0 n0] / [d1 d0].
    // Estimate q0 as [rem1 rem0] / [d1] and correct it.
    qhat = rem / den1;
    rhat = rem % den1;
    c1 = qhat * den0;
    c2 = rhat * b + num0;
    if (c1 > c2) qhat -= (c1 - c2 > den) ? 2 : 1;
    uint32_t q0 = (uint32_t)qhat;

    // Return remainder if requested.
    if (out_rem) *out_rem = (rem * b + num0 - q0 * den) >> shift;
    return ((uint64_t)q1 << 32) | q0;
}
#endif /* NEGATE__DIV128_IMPL */

static void nbhm_compute_size(NBHM_Table* table, size_t cap) {
    // reciprocals to compute modulo
    #if defined(__GNUC__) || defined(__clang__)
    table->sh = 64 - __builtin_clzll(cap);
    #else
    uint64_t sh = 0;
    while (cap > (1ull << sh)){ sh++; }
    table->sh = sh;
    #endif

    table->sh += 63 - 64;
    table->a = negate__div128(1ull << table->sh, cap - 1, cap, NULL);

    #if (defined(__GNUC__) || defined(__clang__)) && defined(__x86_64__)
    uint64_t d,e;
    __asm__("divq %[v]" : "=a"(d), "=d"(e) : [v] "r"(cap), "a"(cap - 1), "d"(1ull << table->sh));
    assert(d == table->a);
    #endif

    table->cap = cap;
}

static NBHM nbhm_alloc(size_t initial_cap) {
    ebr_init();

    size_t cap = nbhm_compute_cap(initial_cap);
    NBHM_Table* table = EBR_VIRTUAL_ALLOC(sizeof(NBHM_Table) + cap*sizeof(NBHM_Entry));
    nbhm_compute_size(table, cap);
    return (NBHM){ .latest = table };
}

static void nbhm_free(NBHM* hs) {
    NBHM_Table* curr = hs->latest;
    while (curr) {
        NBHM_Table* next = curr->prev;
        EBR_VIRTUAL_FREE(curr, sizeof(NBHM_Table) + curr->cap*sizeof(NBHM_Entry));
        curr = next;
    }
}

// for spooky stuff
static NBHM_Entry* nbhm_array(NBHM* hs) { return hs->latest->data; }
static size_t nbhm_count(NBHM* hs)      { return hs->latest->count; }
static size_t nbhm_capacity(NBHM* hs)   { return hs->latest->cap; }

#define nbhm_for(it, hs) for (NBHM_Entry *it = nbhm_array(hs), **_end_ = &it[nbhm_capacity(hs)]; it != _end_; it++) if (*it != NULL)
#endif // NBHM_H

#ifdef NBHM_IMPL
#if defined(_WIN32)
#pragma comment(lib, "synchronization.lib")
#endif

int NBHM_TOMBSTONE;
int NBHM_NO_MATCH_OLD;
#endif // NBHM_IMPL

// Templated implementation
#ifdef NBHM_FN
extern int NBHM_TOMBSTONE;
extern int NBHM_NO_MATCH_OLD;

static void* NBHM_FN(put_if_match)(NBHM* hs, NBHM_Table* latest, NBHM_Table* prev, void* key, void* val, void* exp);

static size_t NBHS_FN(hash2index)(NBHS_Table* table, uint64_t u) {
    uint64_t v = table->a;

    // Multiply high 64: Ripped, straight, from, Hacker's delight... mmm delight
    uint64_t u0 = u & 0xFFFFFFFF;
    uint64_t u1 = u >> 32;
    uint64_t v0 = v & 0xFFFFFFFF;
    uint64_t v1 = v >> 32;
    uint64_t w0 = u0*v0;
    uint64_t t = u1*v0 + (w0 >> 32);
    uint64_t w1 = (u0*v1) + (t & 0xFFFFFFFF);
    uint64_t w2 = (u1*v1) + (t >> 32);
    uint64_t hi = w2 + (w1 >> 32);
    // Modulo from quotient
    uint64_t q  = hi >> table->sh;
    uint64_t q2 = u - (q * table->cap);

    assert(q2 == u % table->cap);
    return q2;
}

NBHM_Table* NBHM_FN(move_items)(NBHM* hm, NBHM_Table* latest, NBHM_Table* prev, int items_to_move) {
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

    NBHM__BEGIN("copying old");
    for (size_t i = old; i < new; i++) {
        void* old_v = atomic_load(&prev->data[i].val);
        void* k     = atomic_load(&prev->data[i].key);

        // freeze the values by adding a prime bit.
        while (((uintptr_t) old_v & NBHM_PRIME_BIT) == 0) {
            uintptr_t primed_v = (old_v == &NBHM_TOMBSTONE ? 0 : (uintptr_t) old_v) | NBHM_PRIME_BIT;
            if (atomic_compare_exchange_strong(&prev->data[i].val, &old_v, (void*) primed_v)) {
                if (old_v != NULL && old_v != &NBHM_TOMBSTONE) {
                    // once we've frozen, we can move it to the new table.
                    // we pass NULL for prev since we already know the entries exist in prev.
                    NBHM_FN(put_if_match)(hm, latest, NULL, k, old_v, &NBHM_NO_MATCH_OLD);
                }
                break;
            }
            // btw, CAS updated old_v
        }
    }
    NBHM__END();

    uint32_t done = atomic_fetch_add(&prev->move_done, new - old);
    done += new - old;

    assert(done <= cap);
    if (done == cap) {
        // dettach now
        NBHM__BEGIN("detach");
        latest->prev = NULL;
        ebr_exit_cs();

        ebr_free(prev, sizeof(NBHM_Table) + prev->cap*sizeof(void*));

        ebr_enter_cs();
        prev = NULL;
        NBHM__END();
    }
    return prev;
}

static void* NBHM_FN(raw_lookup)(NBHM* hs, NBHM_Table* table, uint32_t h, void* key) {
    size_t cap = table->cap;
    size_t first = NBHM_FN(hash2index)(table, h), i = first;
    do {
        void* v = atomic_load(&table->data[i].val);
        void* k = atomic_load(&table->data[i].key);

        if (k == NULL) {
            return NULL;
        } else if (NBHM_FN(cmp)(k, key)) {
            return v != &NBHM_TOMBSTONE ? v : NULL;
        }

        // inc & wrap around
        i = (i == cap-1) ? 0 : i + 1;
    } while (i != first);

    return NULL;
}

static void* NBHM_FN(put_if_match)(NBHM* hs, NBHM_Table* latest, NBHM_Table* prev, void* key, void* val, void* exp) {
    assert(key);

    void *k, *v;
    for (;;) {
        uint32_t cap = latest->cap;
        size_t limit = (cap * NBHM_LOAD_FACTOR) / 100;
        if (prev == NULL && latest->count >= limit) {
            // make resized table, we'll amortize the moves upward
            size_t new_cap = nbhm_compute_cap(limit*2);

            NBHM_Table* new_top = EBR_VIRTUAL_ALLOC(sizeof(NBHM_Table) + new_cap*sizeof(NBHM_Entry));
            nbhm_compute_size(new_top, new_cap);

            // CAS latest -> new_table, if another thread wins the race we'll use its table
            new_top->prev = latest;
            if (!atomic_compare_exchange_strong(&hs->latest, &latest, new_top)) {
                EBR_VIRTUAL_FREE(new_top, sizeof(NBHM_Table) + new_cap*sizeof(NBHM_Entry));
                prev = atomic_load(&latest->prev);
            } else {
                prev   = latest;
                latest = new_top;

                // float s = sizeof(NBHM_Table) + new_cap*sizeof(NBHM_Entry);
                // printf("Resize: %.2f KiB (cap=%zu)\n", s / 1024.0f, new_cap);
            }
            continue;
        }

        // key claiming phase:
        //   once completed we'll have a key inserted into the latest
        //   table (the value might be NULL which means that the entry
        //   is still empty but we've at least agreed where the value
        //   goes).
        bool found = false;
        uint32_t h = NBHM_FN(hash)(key);
        size_t first = NBHM_FN(hash2index)(latest, h), i = first;
        do {
            v = atomic_load_explicit(&latest->data[i].val, memory_order_acquire);
            k = atomic_load_explicit(&latest->data[i].key, memory_order_acquire);

            if (k == NULL) {
                // key was never in the table
                if (val == &NBHM_TOMBSTONE) { return NULL; }

                // fight for empty slot
                if (atomic_compare_exchange_strong(&latest->data[i].key, &k, key)) {
                    atomic_fetch_add_explicit(&latest->count, 1, memory_order_relaxed);
                    found = true;
                    break;
                }
            }

            if (NBHM_FN(cmp)(k, key)) {
                found = true;
                break;
            }

            // inc & wrap around
            i = (i == cap-1) ? 0 : i + 1;
        } while (i != first);

        // we didn't claim a key, that means the table is entirely full, retry
        // to use or make a bigger table.
        if (!found) {
            latest = atomic_load(&hs->latest);
            prev   = atomic_load(&latest->prev);
            continue;
        }

        // migration barrier, we only insert our item once we've
        // "logically" moved it
        if (v == NULL && prev != NULL) {
            assert(prev->prev == NULL);
            void* old = NBHM_FN(raw_lookup)(hs, prev, h, val);
            if (old != NULL) {
                // the old value might've been primed, we don't want to propagate the prime bit tho
                old = (void*) (((uintptr_t) old) & ~NBHM_PRIME_BIT);

                // if we lost, then we just get replaced by a separate fella (which is fine ig)
                if (atomic_compare_exchange_strong(&latest->data[i].val, &v, old)) {
                    v = old;
                }
            }
        }

        // if the old value is a prime, we've been had (we're resizing)
        if (((uintptr_t) v) & NBHM_PRIME_BIT) {
            continue;
        }

        // if the existing value is:
        // * incompatible with the expected value, we don't write.
        // * equal, we don't write (speed opt, one CAS is slower than no CAS).
        if (v != val &&
            // exp is tombstone, we'll only insert if it's empty (and not a migration)
            (exp != &NBHM_TOMBSTONE || (v == NULL || v == &NBHM_TOMBSTONE))
        ) {
            // value writing attempt, if we lose the CAS it means someone could've written a
            // prime (thus the entry was migrated to a later table). It could also mean we lost
            // the insertion fight to another writer and in that case we'll take their value.
            if (atomic_compare_exchange_strong(&latest->data[i].val, &v, val)) {
                v = val;
            } else {
                // if we see a prime, the entry has been migrated
                // and we should write to that later table. if not,
                // we simply lost the race to update the value.
                uintptr_t v_raw = (uintptr_t) v;
                if (v_raw & NBHM_PRIME_BIT) {
                    continue;
                }
            }
        }

        return v;
    }
}

void* NBHM_FN(put)(NBHM* hm, void* key, void* val) {
    NBHM__BEGIN("put");

    assert(val);
    ebr_enter_cs();
    NBHM_Table* latest = atomic_load(&hm->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHM_Table* prev = atomic_load(&latest->prev);
    if (prev != NULL) {
        prev = NBHM_FN(move_items)(hm, latest, prev, NBHM_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hm->latest);
        }
    }

    void* v = NBHM_FN(put_if_match)(hm, latest, prev, key, val, &NBHM_NO_MATCH_OLD);
    ebr_exit_cs();
    NBHM__END();
    return v;
}

void* NBHM_FN(remove)(NBHM* hm, void* key) {
    NBHM__BEGIN("remove");

    assert(key);
    ebr_enter_cs();
    NBHM_Table* latest = atomic_load(&hm->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHM_Table* prev = atomic_load(&latest->prev);
    if (prev != NULL) {
        prev = NBHM_FN(move_items)(hm, latest, prev, NBHM_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hm->latest);
        }
    }

    void* v = NBHM_FN(put_if_match)(hm, latest, prev, key, &NBHM_TOMBSTONE, &NBHM_NO_MATCH_OLD);
    ebr_exit_cs();
    NBHM__END();
    return v;
}

void* NBHM_FN(get)(NBHM* hm, void* key) {
    NBHM__BEGIN("get");

    assert(key);
    ebr_enter_cs();
    NBHM_Table* latest = atomic_load(&hm->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHM_Table* prev = atomic_load(&latest->prev);
    if (prev != NULL) {
        prev = NBHM_FN(move_items)(hm, latest, prev, NBHM_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hm->latest);
        }
    }

    uint32_t cap = latest->cap;
    uint32_t h = NBHM_FN(hash)(key);
    size_t first = NBHM_FN(hash2index)(latest, h), i = first;

    void *k, *v;
    do {
        v = atomic_load(&latest->data[i].val);
        k = atomic_load(&latest->data[i].key);

        if (k == NULL) {
            // first time seen, maybe the entry hasn't been moved yet
            if (prev != NULL) {
                v = NBHM_FN(raw_lookup)(hm, prev, h, key);
            }
            break;
        } else if (NBHM_FN(cmp)(k, key)) {
            return v;
        }

        // inc & wrap around
        i = (i == cap-1) ? 0 : i + 1;
    } while (i != first);

    ebr_exit_cs();
    NBHM__END();
    return v;
}

void* NBHM_FN(put_if_null)(NBHM* hm, void* val) {
    NBHM__BEGIN("put");

    assert(val);
    ebr_enter_cs();
    NBHM_Table* latest = atomic_load(&hm->latest);

    // if there's earlier versions of the table we can move up entries as we go along.
    NBHM_Table* prev = atomic_load(&latest->prev);
    if (prev != NULL) {
        prev = NBHM_FN(move_items)(hm, latest, prev, NBHM_MOVE_AMOUNT);
        if (prev == NULL) {
            latest = atomic_load(&hm->latest);
        }
    }

    void* v = NBHM_FN(put_if_match)(hm, latest, prev, val, val, &NBHM_TOMBSTONE);
    ebr_exit_cs();
    NBHM__END();
    return v;
}

// waits for all items to be moved up before continuing
void NBHM_FN(resize_barrier)(NBHM* hm) {
    NBHM__BEGIN("resize_barrier");
    ebr_enter_cs();
    NBHM_Table *prev, *latest = atomic_load(&hm->latest);
    while (prev = atomic_load(&latest->prev), prev != NULL) {
        NBHM_FN(move_items)(hm, latest, prev, prev->cap);
    }
    ebr_exit_cs();
    NBHM__END();
}

#undef NBHM_FN
#endif // NBHM_FN
