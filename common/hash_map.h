#ifndef NL_HASH_MAP_H
#define NL_HASH_MAP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(TB_USE_MIMALLOC) || defined(CUIK_USE_MIMALLOC)
#include <mimalloc.h>

#define NL_MALLOC(s)     mi_malloc(s)
#define NL_CALLOC(c, s)  mi_calloc(c, s)
#define NL_REALLOC(p, s) mi_realloc(p, s)
#define NL_FREE(p)       mi_free(p)
#else
#define NL_MALLOC(s)     malloc(s)
#define NL_CALLOC(c, s)  calloc(c, s)
#define NL_REALLOC(p, s) realloc(p, s)
#define NL_FREE(p)       free(p)
#endif

#ifdef NL_HASH_MAP_INLINE
#define NL_HASH_MAP_API inline static
#else
#define NL_HASH_MAP_API extern
#endif

#define NL_Map(K, V) struct { K k; V v; }*

/////////////////////////////////////////////////
// public macros
/////////////////////////////////////////////////
#define nl_map_create(map, initial_cap) ((map) = ((void*) nl_map__alloc(initial_cap, sizeof(*map))->kv_table))

#define nl_map_put(map, key, value)                                                  \
do {                                                                                 \
    NL_MapInsert ins__ = nl_map__insert((map), sizeof(*(map)), sizeof(key), &(key)); \
    (map) = ins__.new_map;                                                           \
    (map)[ins__.index].v = (value);                                                  \
} while (0)

#define nl_map_get(map, key) \
((map) != NULL ? nl_map__get(((NL_MapHeader*)(map)) - 1, sizeof(*map), sizeof(key), &(key)) : -1)

#define nl_map_get_checked(map, key) ((map)[nl_map__check(nl_map_get(map, key))].v)

#define nl_map_free(map) \
do {                                              \
    if ((map) != NULL) {                          \
        nl_map__free(((NL_MapHeader*)(map)) - 1); \
        (map) = NULL;                             \
    }                                             \
} while (0)

/////////////////////////////////////////////////
// internals
/////////////////////////////////////////////////
#define nl_map__get_header(map) (((NL_MapHeader*)(map)) - 1)
#define nl_map_get_capacity(map) (1ull << nl_map__get_header(map)->exp)

typedef struct {
    void* new_map;
    size_t index;
} NL_MapInsert;

// behind the array the user manages there's some
// information about the rest of the string map
typedef struct {
    size_t count;
    size_t exp;
    char kv_table[];
} NL_MapHeader;

#ifndef NL_HASH_MAP_INLINE
NL_HASH_MAP_API NL_MapHeader* nl_map__alloc(size_t cap, size_t entry_size);
NL_HASH_MAP_API NL_MapInsert nl_map__insert(void* map, size_t entry_size, size_t key_size, const void* key);
NL_HASH_MAP_API ptrdiff_t nl_map__get(NL_MapHeader* restrict table, size_t entry_size, size_t key_size, const void* key);
NL_HASH_MAP_API void nl_map__free(NL_MapHeader* restrict table);
#endif

inline static ptrdiff_t nl_map__check(ptrdiff_t x) {
    assert(x >= 0 && "map entry not found!");
    return x;
}

#endif /* NL_MAP_H */

#ifdef NL_MAP_IMPL

// FNV1A
inline static uint32_t nl_map__raw_hash(size_t len, const void *key) {
    const uint8_t* data = key;
    uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < len; i++) {
        h = (data[i] ^ h) * 0x01000193;
    }

    return h;
}

NL_HASH_MAP_API void nl_map__free(NL_MapHeader* restrict table) {
    NL_FREE(table);
}

NL_HASH_MAP_API NL_MapHeader* nl_map__alloc(size_t cap, size_t entry_size) {
    cap = (cap * 4) / 3;
    if (cap < 4) cap = 4;

    // next power of two
    #if defined(_MSC_VER) && !defined(__clang__)
    size_t exp = 64 - _lzcnt_u64(cap - 1);
    #else
    size_t exp = 64 - __builtin_clzll(cap - 1);
    #endif

    cap = (cap == 1 ? 1 : 1 << exp);

    NL_MapHeader* table = NL_CALLOC(1, sizeof(NL_MapHeader) + (cap * entry_size));
    table->exp = exp;
    table->count = 0;
    return table;
}

static bool nl_map__is_zero(const char* ptr, size_t size) {
    // we're almost exclusively using this code for pointer keys
    if (size == sizeof(void*)) {
        return *((uintptr_t*) ptr) == 0;
    } else {
        for (size_t i = 0; i < size; i++) {
            if (ptr[i] != 0) return false;
        }

        return true;
    }
}

NL_HASH_MAP_API NL_MapInsert nl_map__insert(void* map, size_t entry_size, size_t key_size, const void* key) {
    NL_MapHeader* table;
    if (map == NULL) {
        table = nl_map__alloc(256, entry_size);
        map = table->kv_table;
    } else {
        table = ((NL_MapHeader*)map) - 1;
    }

    uint32_t cap = 1ull << table->exp;
    if (table->count >= (cap * 4) / 3) {
        // past 75% load... resize
        __debugbreak();
    }

    uint32_t exp = table->exp;
    uint32_t mask = (1 << table->exp) - 1;
    uint32_t hash = nl_map__raw_hash(key_size, key);

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - exp)) | 1;
        i = (i + step) & mask;

        void* slot_entry = &table->kv_table[i * entry_size];
        if (nl_map__is_zero(slot_entry, key_size)) {
            table->count++;
            memcpy(slot_entry, key, key_size);
            return (NL_MapInsert){ map, i };
        } else if (memcmp(slot_entry, key, key_size) == 0) {
            return (NL_MapInsert){ map, i };
        }
    }
}

NL_HASH_MAP_API ptrdiff_t nl_map__get(NL_MapHeader* restrict table, size_t entry_size, size_t key_size, const void* key) {
    uint32_t exp = table->exp;
    uint32_t mask = (1 << table->exp) - 1;
    uint32_t hash = nl_map__raw_hash(key_size, key);

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - exp)) | 1;
        i = (i + step) & mask;

        void* slot_entry = &table->kv_table[i * entry_size];
        if (nl_map__is_zero(slot_entry, key_size)) {
            return -1;
        } else if (memcmp(slot_entry, key, key_size) == 0) {
            return i;
        }
    }
}

#endif /* NL_HASH_MAP_IMPL */
