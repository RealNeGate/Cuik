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

#include "ebr.h"

// personal debooging stuff
#define NBHS__DEBOOGING 0

#if NBHS__DEBOOGING
#define NBHS__BEGIN(name)      spall_auto_buffer_begin(name, sizeof(name) - 1, NULL, 0)
#define NBHS__END()            spall_auto_buffer_end()
#else
#define NBHS__BEGIN(name)
#define NBHS__END()
#endif

enum {
    NBHS_LOAD_FACTOR = 75,
    NBHS_MOVE_AMOUNT = 128,
};

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
    table->a = negate__div128(1ull << table->sh, cap - 1, cap, NULL);

    #if (defined(__GNUC__) || defined(__clang__)) && defined(__x86_64__)
    uint64_t d,e;
    __asm__("divq %[v]" : "=a"(d), "=d"(e) : [v] "r"(cap), "a"(cap - 1), "d"(1ull << table->sh));
    assert(d == table->a);
    #endif

    table->cap = cap;
}

static NBHS nbhs_alloc(size_t initial_cap) {
    ebr_init();

    size_t cap = nbhs_compute_cap(initial_cap);
    NBHS_Table* table = EBR_VIRTUAL_ALLOC(sizeof(NBHS_Table) + cap*sizeof(void*));
    nbhs_compute_size(table, cap);
    return (NBHS){ .latest = table };
}

static void nbhs_free(NBHS* hs) {
    NBHS_Table* curr = hs->latest;
    while (curr) {
        NBHS_Table* next = curr->prev;
        EBR_VIRTUAL_FREE(curr, sizeof(NBHS_Table) + curr->cap*sizeof(void*));
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
#endif // NBHS_IMPL

// Templated implementation
#ifdef NBHS_FN

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

            NBHS_Table* new_top = EBR_VIRTUAL_ALLOC(sizeof(NBHS_Table) + new_cap*sizeof(void*));
            nbhs_compute_size(new_top, new_cap);

            // CAS latest -> new_table, if another thread wins the race we'll use its table
            new_top->prev = latest;
            if (!atomic_compare_exchange_strong(&hs->latest, &latest, new_top)) {
                EBR_VIRTUAL_FREE(new_top, sizeof(NBHS_Table) + new_cap*sizeof(void*));
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
        ebr_exit_cs();

        ebr_free(prev, sizeof(NBHS_Table) + prev->cap*sizeof(void*));

        ebr_enter_cs();
        prev = NULL;
        NBHS__END();
    }
    return prev;
}

void* NBHS_FN(get)(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");

    assert(val);

    // modifying the tables is possible now.
    ebr_enter_cs();
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

    ebr_exit_cs();
    NBHS__END();
    return result;
}

void* NBHS_FN(intern)(NBHS* hs, void* val) {
    NBHS__BEGIN("intern");

    assert(val);
    ebr_enter_cs();
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

    ebr_exit_cs();
    NBHS__END();
    return result;
}

// waits for all items to be moved up before continuing
void NBHS_FN(resize_barrier)(NBHS* hs) {
    NBHS__BEGIN("intern");
    ebr_enter_cs();
    NBHS_Table *prev, *latest = atomic_load(&hs->latest);
    while (prev = atomic_load(&latest->prev), prev != NULL) {
        NBHS_FN(move_items)(hs, latest, prev, prev->cap);
    }
    ebr_exit_cs();
    NBHS__END();
}

#undef NBHS_FN
#endif // NBHS_FN
