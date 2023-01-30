#ifndef NL_HASH_MAP_H
#define NL_HASH_MAP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef NL_MALLOC
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
#define nl_map_put(map, key, value)                                                  \
do {                                                                                 \
    NL_MapInsert ins__ = nl_map__insert((map), sizeof(*(map)), sizeof(key), &(key)); \
    (map) = ins__.new_map;                                                           \
    (map)[ins__.index].v = (value);                                                  \
} while (0)

#define nl_map_get(map, key) \
((map) != NULL ? nl_map__get(((NL_MapHeader*)(map)) - 1, sizeof(*map), sizeof(key), &(key)) : -1)

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

#define NL_MAP_BUCKET_SIZE 16

typedef struct {
    void* new_map;
    size_t index;
} NL_MapInsert;

// behind the array the user manages there's some
// information about the rest of the string map
typedef struct {
    size_t size;
    uint8_t* buckets; // number of entries in the bucket
    char kv_table[];
} NL_MapHeader;

#ifndef NL_HASH_MAP_INLINE
NL_HASH_MAP_API NL_MapInsert nl_map__insert(void* map, size_t entry_size, size_t key_size, const void* key);
NL_HASH_MAP_API ptrdiff_t nl_map__get(NL_MapHeader* restrict table, size_t entry_size, size_t key_size, const void* key);
NL_HASH_MAP_API void nl_map__free(NL_MapHeader* restrict table);
#endif

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

inline static uint32_t nl_map__hash(size_t len, const void *key, size_t modulo) {
    assert((modulo & (modulo - 1)) == 0 && "modulo is not a power-of-two");
    // return nl_map__raw_hash(len, key) & (modulo - 1);

    uint32_t shift = (uint32_t) __builtin_clz(modulo - 1);
    return nl_map__raw_hash(len, key) >> shift;
}

NL_HASH_MAP_API void nl_map__free(NL_MapHeader* restrict table) {
    NL_FREE(table->buckets);
    NL_FREE(table);
}

NL_HASH_MAP_API NL_MapHeader* nl_map__alloc(size_t size, size_t entry_size) {
    assert((size & (size - 1)) == 0 && "size is not a power-of-two");

    NL_MapHeader* table = NL_MALLOC(sizeof(NL_MapHeader) + (size * entry_size));
    table->size = size;
    table->buckets = NL_CALLOC(size / NL_MAP_BUCKET_SIZE, sizeof(uint8_t));
    return table;
}

// FOR DEBUG PURPOSES ONLY
/*static void nl_map__dump(NL_MapHeader* restrict table, size_t entry_size) {
    printf("Table: %p\n", table);
    size_t bucket_count = table->size / NL_MAP_BUCKET_SIZE;
    for (size_t i = 0; i < bucket_count; i++) {
        size_t bucket_entries = table->buckets[i];

        for (size_t j = 0; j < bucket_entries; j++) {
            size_t slot = (i * NL_MAP_BUCKET_SIZE) + j;
            uint32_t* kv = (uint32_t*) &table->kv_table[slot * entry_size];

            printf("  [%d] = %d\n", kv[0], kv[1]);
        }
    }
}*/

static NL_MapHeader* nl_map__resize(NL_MapHeader* table, size_t entry_size, size_t key_size) {
    // remake & rehash... probably really slow...
    size_t old_size = table->size;
    size_t new_size = old_size * 2;

    // printf("Resize %zu -> %zu...\n", old_size, new_size);
    NL_MapHeader* new_table = nl_map__alloc(new_size, entry_size);

    // go per bucket and relocate them
    size_t old_bucket_count = old_size / NL_MAP_BUCKET_SIZE;
    for (size_t i = 0; i < old_bucket_count; i++) {
        size_t bucket_entries = table->buckets[i];

        for (size_t j = 0; j < bucket_entries; j++) {
            size_t old_slot = (i * NL_MAP_BUCKET_SIZE) + j;
            const char* entry = &table->kv_table[old_slot * entry_size];

            // rehash
            size_t new_bucket_index = nl_map__hash(key_size, entry, new_size / NL_MAP_BUCKET_SIZE);
            size_t k = new_table->buckets[new_bucket_index]++;
            assert(k < NL_MAP_BUCKET_SIZE);

            // move entry
            // const uint32_t* kv = (const uint32_t*) entry;
            // printf("REHASH [%d] => [%d] (%zd %x)\n", kv[0], kv[1], new_bucket_index, nl_map__raw_hash(key_size, entry));

            size_t new_slot = (new_bucket_index * NL_MAP_BUCKET_SIZE) + k;
            memcpy(&new_table->kv_table[new_slot * entry_size], entry, entry_size);
        }
    }

    nl_map__free(table);
    return new_table;
}

NL_HASH_MAP_API NL_MapInsert nl_map__insert(void* map, size_t entry_size, size_t key_size, const void* key) {
    NL_MapHeader* table;
    if (map == NULL) {
        table = nl_map__alloc(4096, entry_size);
        map = table->kv_table;
    } else {
        table = ((NL_MapHeader*)map) - 1;
    }

    size_t bucket = nl_map__hash(key_size, key, table->size / NL_MAP_BUCKET_SIZE);
    size_t bucket_entries = table->buckets[bucket];

    // try to rehash once we hit the threshold (load factor of 75%)
    size_t threshold = (NL_MAP_BUCKET_SIZE * 3) / 4;
    if (bucket_entries >= threshold) {
        table = nl_map__resize(table, entry_size, key_size);
        map = table->kv_table;

        // rehash new entry
        bucket = nl_map__hash(key_size, key, table->size / NL_MAP_BUCKET_SIZE);
        bucket_entries = table->buckets[bucket];
    }

    // check for duplicate first
    for (size_t i = 0; i < bucket_entries; i++) {
        size_t slot = (bucket * NL_MAP_BUCKET_SIZE) + i;
        void* key_at_slot = &table->kv_table[slot * entry_size];

        if (memcmp(key_at_slot, key, key_size) == 0) {
            return (NL_MapInsert){ map, slot };
        }
    }

    size_t index = table->buckets[bucket]++;
    size_t slot = (bucket * NL_MAP_BUCKET_SIZE) + index;

    memcpy(&table->kv_table[slot * entry_size], key, key_size);
    return (NL_MapInsert){ map, slot };
}

NL_HASH_MAP_API ptrdiff_t nl_map__get(NL_MapHeader* restrict table, size_t entry_size, size_t key_size, const void* key) {
    size_t bucket = nl_map__hash(key_size, key, table->size / NL_MAP_BUCKET_SIZE);
    size_t bucket_entries = table->buckets[bucket];

    for (size_t i = 0; i < bucket_entries; i++) {
        size_t slot = (bucket * NL_MAP_BUCKET_SIZE) + i;
        void* slot_entry = &table->kv_table[slot * entry_size];

        if (memcmp(slot_entry, key, key_size) == 0) {
            return slot;
        }
    }

    return -1;
}

#endif /* NL_HASH_MAP_IMPL */
