// really basic string map
// doesn't support remove yet, can't remove shit in Detroit
//
// Namespaces:
//   nl_*
//   NL_*
//
// if you wish to use this library without collision don't use these
// identifiers
//
// Do this:
//   #define NL_STRING_MAP_IMPL
// before you include this file into one C file
//
// Settings:
//   NL_STRING_MAP_SHORT if defined, will add aliases to all the functions/macros
//   such that nl_strmap_* is converted to strmap_*
//
//   NL_STRING_MAP_INLINE makes functions inline-hinted (not forced) which
//   may increase code size but also improve performance
//
// Macros/Functions:
//   nl_strmap_get(map, slice)
//   nl_strmap_get_cstr(map, cstr)
//     Both will return a ptrdiff_t and if it's -1 we couldn't find a match, else
//     it's an index to the match
#ifndef NL_STRING_MAP_H
#define NL_STRING_MAP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    size_t length;
    const uint8_t* data;
} NL_Slice;

#define NL_MALLOC(s)     malloc(s)
#define NL_CALLOC(c, s)  calloc(c, s)
#define NL_REALLOC(p, s) realloc(p, s)
#define NL_FREE(p)       free(p)

#ifdef NL_STRING_MAP_INLINE
#define NL_API static
#else
#define NL_API extern
#endif

#define NL_Strmap(T) T*

/////////////////////////////////////////////////
// public macros
/////////////////////////////////////////////////
// You don't have to allocate the string map before use, this is just to have an explicit
// initial size
#define nl_strmap_alloc(T, initial_size) \
(T*)(nl_strmap__alloc(initial_size, sizeof(T)) + 1)

#define nl_strmap_puti(map, key) \
nl_strmap__insert2((void**) &(map), key, sizeof(*(map)))

#define nl_strmap_puti_cstr(map, key) \
nl_strmap__insert2((void**) &(map), nl_slice__cstr(key), sizeof(*(map)))

#define nl_strmap_put(map, key, value)                                       \
do {                                                                         \
    NL_StrmapInsert ins__ = nl_strmap__insert((map), key, sizeof(*(map)));   \
    (map) = ins__.new_map;                                                   \
    (map)[ins__.val_slot] = (value);                                         \
} while (0)

#define nl_strmap_put_cstr(map, key, value)                                      \
do {                                                                             \
    NL_Slice slice__ = nl_slice__cstr(key);                                      \
    NL_StrmapInsert ins__ = nl_strmap__insert((map), slice__, sizeof(*(map)));   \
    (map) = ins__.new_map;                                                       \
    (map)[ins__.val_slot] = (value);                                             \
} while (0)

#define nl_strmap_get(map, ...) \
((map) != NULL ? nl_strmap__get(((NL_StrmapHeader*)(map)) - 1, (__VA_ARGS__)) : -1)

#define nl_strmap_get_cstr(map, key) \
((map) != NULL ? nl_strmap__get(((NL_StrmapHeader*)(map)) - 1, nl_slice__cstr(key)) : -1)

#define nl_strmap_free(map) \
do {                                                    \
    if ((map) != NULL) {                                \
        nl_strmap__free(((NL_StrmapHeader*)(map)) - 1); \
        (map) = NULL;                                   \
    }                                                   \
} while (0)

#define nl_strmap_clear(map) \
(((map) != NULL) ? nl_strmap__clear(((NL_StrmapHeader*)(map)) - 1) : (void)0)

#define nl_strmap_get_load(map) ((map) ? nl_strmap__get_header(map)->load : 0)

// Iterator
#define nl_strmap_for(it, map) \
for (size_t it = 0, size__ = nl_strmap_get_load(map); it < size__; it++)

/////////////////////////////////////////////////
// internals
/////////////////////////////////////////////////
#define nl_strmap__get_header(map) (((NL_StrmapHeader*)(map)) - 1)

#define NL_STRMAP_BUCKET_SIZE 16

typedef struct {
    void* new_map;
    size_t index;
    size_t val_slot;
} NL_StrmapInsert;

// behind the array the user manages there's some
// information about the rest of the string map
typedef struct {
    size_t size, load;

    uint8_t* buckets; // number of entries in a bucket
    NL_Slice* keys;
    ptrdiff_t* indices;

    char values[];
} NL_StrmapHeader;

#ifndef NL_STRING_MAP_INLINE
NL_API NL_Strmap__Insert nl_strmap__insert(void* map, NL_Slice key, size_t value_type_size);
NL_API ptrdiff_t nl_strmap__insert2(void** map, NL_Slice key, size_t value_type_size);
NL_API ptrdiff_t nl_strmap__get(NL_StrmapHeader* restrict table, NL_Slice key);
NL_API void nl_strmap__clear(NL_StrmapHeader* restrict table);
NL_API void nl_strmap__free(NL_StrmapHeader* restrict table);
#endif

inline static NL_Slice nl_slice__cstr(const char* key) {
    return (NL_Slice){strlen(key), (const uint8_t*)key};
}

#endif /* NL_STRING_MAP_H */

#ifdef NL_STRING_MAP_IMPL

// FNV1A
static uint32_t nl_strmap__hash(size_t len, const void *key, size_t modulo) {
    assert((modulo & (modulo - 1)) == 0 && "modulo is not a power-of-two");

    const uint8_t* data = key;
    uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < len; i++) {
        h = (data[i] ^ h) * 0x01000193;
    }

    uint32_t shift = (uint32_t) __builtin_clz(modulo - 1);
    return h >> shift;
}

/*static void nl_strmap__dump(NL_StrmapHeader* restrict table) {
    printf("Table: %p\n", table);
    for (size_t i = 0; i < table->size; i++) {
        if (table->keys[i].length > 0) {
            printf("- %.*s\n", (int) table->keys[i].length, table->keys[i].data);
        }
    }
}*/

NL_API void nl_strmap__free(NL_StrmapHeader* restrict table) {
    NL_FREE(table->indices);
    NL_FREE(table->buckets);
    NL_FREE(table->keys);
    NL_FREE(table);
}

NL_API NL_StrmapHeader* nl_strmap__alloc(size_t size, size_t value_type_size) {
    NL_StrmapHeader* table = NL_MALLOC(sizeof(NL_StrmapHeader) + (size * value_type_size));
    table->size = size;
    table->load = 0;
    table->keys = NL_CALLOC(size, sizeof(NL_Slice));
    table->buckets = NL_CALLOC(size / NL_STRMAP_BUCKET_SIZE, sizeof(uint8_t));
    table->indices = NL_MALLOC(size * sizeof(ptrdiff_t));
    return table;
}

static NL_StrmapHeader* nl_strmap__resize(NL_StrmapHeader* table, size_t value_type_size) {
    // remake & rehash... probably really slow...
    size_t old_size = table->size;
    size_t new_size = old_size * 2;

    // printf("Resize %zu -> %zu...\n", old_size, new_size);
    NL_StrmapHeader* new_table = nl_strmap__alloc(new_size, value_type_size);

    // go per bucket and relocate them
    size_t old_bucket_count = old_size / NL_STRMAP_BUCKET_SIZE;
    for (size_t i = 0; i < old_bucket_count; i++) {
        size_t bucket_entries = table->buckets[i];

        for (size_t j = 0; j < bucket_entries; j++) {
            size_t old_slot = (i * NL_STRMAP_BUCKET_SIZE) + j;
            NL_Slice* old_key = &table->keys[old_slot];
            char* old_value = &table->values[table->indices[old_slot] * value_type_size];

            // rehash
            size_t new_bucket_index = nl_strmap__hash(old_key->length, old_key->data, new_size / NL_STRMAP_BUCKET_SIZE);
            size_t k = new_table->buckets[new_bucket_index]++;
            assert(k < NL_STRMAP_BUCKET_SIZE);

            // move entry
            size_t new_slot = (new_bucket_index * NL_STRMAP_BUCKET_SIZE) + k;
            size_t new_val_slot = new_table->load++;

            new_table->keys[new_slot] = *old_key;
            new_table->indices[new_slot] = new_val_slot;
            memcpy(&new_table->values[new_val_slot * value_type_size], old_value, value_type_size);
        }
    }

    nl_strmap__free(table);
    return new_table;
}

NL_API NL_StrmapInsert nl_strmap__insert(void* map, NL_Slice key, size_t value_type_size) {
    NL_StrmapHeader* table;
    if (map == NULL) {
        table = nl_strmap__alloc(131072, value_type_size);
        map = table + 1;
    } else {
        table = ((NL_StrmapHeader*)map) - 1;
    }

    size_t bucket = nl_strmap__hash(key.length, key.data, table->size / NL_STRMAP_BUCKET_SIZE);
    size_t bucket_entries = table->buckets[bucket];

    // try to rehash once we hit the threshold (load factor of 75%)
    size_t threshold = (NL_STRMAP_BUCKET_SIZE * 3) / 4;
    if (bucket_entries >= threshold) {
        table = nl_strmap__resize(table, value_type_size);
        map = table + 1;

        // rehash new entry
        bucket = nl_strmap__hash(key.length, key.data, table->size / NL_STRMAP_BUCKET_SIZE);
        bucket_entries = table->buckets[bucket];
    }

    // check for duplicate first
    for (size_t i = 0; i < bucket_entries; i++) {
        size_t slot = (bucket * NL_STRMAP_BUCKET_SIZE) + i;

        if (table->keys[slot].length == key.length && memcmp(table->keys[slot].data, key.data, key.length) == 0) {
            return (NL_StrmapInsert){ map, slot, table->indices[slot] };
        }
    }

    size_t index = table->buckets[bucket]++;
    size_t slot = (bucket * NL_STRMAP_BUCKET_SIZE) + index;

    size_t val_slot = table->load++;
    table->keys[slot] = key;
    table->indices[slot] = val_slot;
    return (NL_StrmapInsert){ map, slot, val_slot };
}

NL_API ptrdiff_t nl_strmap__insert2(void** map, NL_Slice key, size_t value_type_size) {
    NL_StrmapInsert i = nl_strmap__insert(*map, key, value_type_size);
    *map = i.new_map;
    return i.val_slot;
}

NL_API void nl_strmap__clear(NL_StrmapHeader* restrict table) {
    memset(table->keys, 0, sizeof(NL_Slice) * table->size);
    table->load = 0;
}

NL_API ptrdiff_t nl_strmap__get(NL_StrmapHeader* restrict table, NL_Slice key) {
    size_t bucket = nl_strmap__hash(key.length, key.data, table->size / NL_STRMAP_BUCKET_SIZE);
    size_t bucket_entries = table->buckets[bucket];

    for (size_t i = 0; i < bucket_entries; i++) {
        size_t slot = (bucket * NL_STRMAP_BUCKET_SIZE) + i;

        if (table->keys[slot].length == key.length && memcmp(table->keys[slot].data, key.data, key.length) == 0) {
            return table->indices[slot];
        }
    }

    return -1;
}

#endif /* NL_STRING_MAP_IMPL */
