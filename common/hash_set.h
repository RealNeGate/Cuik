// This is built specifically for pointer hash sets
#ifndef NL_HASH_SET_H
#define NL_HASH_SET_H

typedef struct NL_HashSet {
    size_t exp, count;
    void** data;
} NL_HashSet;

typedef uint32_t (*NL_HashFunc)(void* a);
typedef bool (*NL_CompareFunc)(void* a, void* b);

NL_HashSet nl_hashset_alloc(size_t cap);
void nl_hashset_free(NL_HashSet hs);

void nl_hashset_clear(NL_HashSet* restrict hs);
bool nl_hashset_put(NL_HashSet* restrict hs, void* ptr);

// this one takes a custom hash function
void* nl_hashset_put2(NL_HashSet* restrict hs, void* ptr, NL_HashFunc hash, NL_CompareFunc cmp);

#define nl_hashset_capacity(hs) (1ull << (hs)->exp)
#define nl_hashset_for(it, hs)  for (void **it = (hs)->data, **_end_ = &it[nl_hashset_capacity(hs)]; it != _end_; it++) if (*it != NULL)

#endif /* NL_HASH_SET_H */

#ifdef NL_HASH_SET_IMPL
#include <common.h>

#define NL_HASHSET_HASH(ptr) ((((uintptr_t) ptr) * 11400714819323198485ull) >> 32ull)

NL_HashSet nl_hashset_alloc(size_t cap) {
    cap = (cap * 4) / 3;
    if (cap < 4) cap = 4;

    // next power of two
    #if defined(_MSC_VER) && !defined(__clang__)
    size_t exp = 64 - _lzcnt_u64(cap - 1);
    #else
    size_t exp = 64 - __builtin_clzll(cap - 1);
    #endif

    cap = (cap == 1 ? 1 : 1 << exp);

    return (NL_HashSet){ .exp = exp, .data = cuik_calloc(cap, sizeof(void*)) };
}

void nl_hashset_free(NL_HashSet hs) {
    cuik_free(hs.data);
}

bool nl_hashset_put(NL_HashSet* restrict hs, void* ptr) {
    uint32_t h = NL_HASHSET_HASH(ptr);

    size_t mask = (1 << hs->exp) - 1;
    size_t first = h & mask, i = first;

    do {
        if (hs->data[i] == NULL) {
            hs->count++;
            hs->data[i] = ptr;
            return true;
        } else if (hs->data[i] == ptr) {
            return false;
        }

        i = (i + 1) & mask;
    } while (i != first);

    assert(0 && "Rehash...");
    return false;
}

// returns old value
void* nl_hashset_put2(NL_HashSet* restrict hs, void* ptr, NL_HashFunc hash, NL_CompareFunc cmp) {
    uint32_t h = hash(ptr);

    size_t mask = (1 << hs->exp) - 1;
    size_t first = h & mask, i = first;

    do {
        if (hs->data[i] == NULL) {
            hs->count++;
            hs->data[i] = ptr;
            return NULL;
        } else if (cmp(hs->data[i], ptr)) {
            return hs->data[i];
        }

        i = (i + 1) & mask;
    } while (i != first);

    assert(0 && "Rehash...");
    return NULL;
}

void nl_hashset_clear(NL_HashSet* restrict hs) {
    memset(hs->data, 0, nl_hashset_capacity(hs));
}

#endif /* NL_HASH_SET_IMPL */
