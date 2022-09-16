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
//   NL_STRING_MAP_HASH can be defined as a function which matches the
//   prototype `uint32_t YOUR_FUNC(size_t length, const uint8_t* data)`.
//   if you do not provide a hash function, FNV1A will be used instead
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
#define NL_API inline static
#else
#define NL_API extern
#endif

#define NL_STRMAP_BUCKET_SIZE 8

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
    NL_Strmap__Insert ins__ = nl_strmap__insert((map), key, sizeof(*(map))); \
    (map) = ins__.new_map;                                                   \
    (map)[ins__.val_slot] = (value);                                         \
} while (0)

#define nl_strmap_put_cstr(map, key, value)                                      \
do {                                                                             \
    NL_Slice slice__ = nl_slice__cstr(key);                                      \
    NL_Strmap__Insert ins__ = nl_strmap__insert((map), slice__, sizeof(*(map))); \
    (map) = ins__.new_map;                                                       \
    (map)[ins__.val_slot] = (value);                                             \
} while (0)

#define nl_strmap_get(map, key) \
((map) != NULL ? nl_strmap__get(((NL_StrmapHeader*)(map)) - 1, (key)) : -1)

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

// Iterator
#define nl_strmap_for(it, map) \
for (size_t it = 0, size__ = nl_strmap__get_header(map)->load; it < size__; it++)

/////////////////////////////////////////////////
// internals
/////////////////////////////////////////////////
#define nl_strmap__get_header(map) (((NL_StrmapHeader*)(map)) - 1)

typedef struct {
    void* new_map;
    size_t index;
    size_t val_slot;
} NL_Strmap__Insert;

// behind the array the user manages there's some
// information about the rest of the string map
typedef struct {
    size_t size;
    size_t load;

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

#ifndef NL_STRING_MAP_HASH
#define NL_STRING_MAP_HASH fnv1a
inline static uint32_t fnv1a(size_t length, const uint8_t* data) {
    uint32_t hash = 0x811C9DC5;

    for (size_t i = 0; i < length; i++) {
        hash = (data[i] ^ hash) * 0x01000193;
    }

    return hash;
}
#endif

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
    NL_FREE(table->keys);
    NL_FREE(table);
}

NL_API NL_StrmapHeader* nl_strmap__alloc(size_t size, size_t value_type_size) {
    NL_StrmapHeader* table = NL_MALLOC(sizeof(NL_StrmapHeader) + (size * value_type_size));
    table->size = size;
    table->load = 0;
    table->keys = NL_CALLOC(size, sizeof(NL_Slice));
    table->indices = NL_MALLOC(size * sizeof(ptrdiff_t));
    return table;
}

static NL_StrmapHeader* nl_strmap__resize(NL_StrmapHeader* table, size_t value_type_size) {
    // remake & rehash... probably really slow...
    size_t old_size = table->size;
    size_t new_size = old_size * 2;

    NL_Slice* restrict old_keys = table->keys;
    ptrdiff_t* restrict old_indices = table->indices;

    //printf("Resize %zu -> %zu...\n", old_size, new_size);
    table = NL_REALLOC(table, sizeof(NL_StrmapHeader) + (new_size * value_type_size));
    if (table == NULL) {
        fprintf(stderr, "error: string map out of memory!\n");
        exit(1);
    }

    table->size = new_size;
    table->load = 0;
    table->keys = NL_CALLOC(new_size, sizeof(NL_Slice));
    table->indices = NL_MALLOC(new_size * sizeof(ptrdiff_t));

    for (size_t i = 0; i < old_size; i++) {
        NL_Slice key = old_keys[i];
        size_t old_index = old_indices[i];

        if (key.length != 0) {
            // find empty slot
            size_t size_mask = (new_size - 1);
            size_t new_index = fnv1a(key.length, key.data) & size_mask;

            for (size_t j = 0; j < NL_STRMAP_BUCKET_SIZE; j++) {
                size_t slot = (j + new_index) & size_mask;

                if (table->keys[slot].length == 0) {
                    new_index = slot;
                    goto found;
                }
            }

            abort();

            // the values array doesn't get reordered so find the old index in there
            found:
            table->keys[new_index] = key;
            table->indices[new_index] = old_index;
            table->load += 1;
        }
    }

    NL_FREE(old_keys);
    NL_FREE(old_indices);

    return table;
}

NL_API NL_Strmap__Insert nl_strmap__insert(void* map, NL_Slice key, size_t value_type_size) {
    NL_StrmapHeader* table;
    if (map == NULL) {
        table = nl_strmap__alloc(131072, value_type_size);
        map = table->values;
    } else {
        table = ((NL_StrmapHeader*)map) - 1;
    }

    uint8_t first = key.data[0];
    size_t index = SIZE_MAX;
    for (;;) {
        // try to rehash once we hit the threshold (load factor of 75%)
        size_t threshold = (table->size * 3) / 4;
        if (table->load >= threshold) {
            table = nl_strmap__resize(table, value_type_size);
            map = table + 1;
        }

        size_t size_mask = (table->size - 1);
        index = fnv1a(key.length, key.data) & size_mask;

        // make sure we have an empty slot
        for (size_t i = 0; i < NL_STRMAP_BUCKET_SIZE; i++) {
            size_t slot = (index + i) & size_mask;

            if (table->keys[slot].length == 0) {
                index = slot;
                goto success;
            } else if (table->keys[slot].length == key.length &&
                table->keys[slot].data[0] == first &&
                memcmp(table->keys[slot].data, key.data, key.length) == 0) {
                // or... override previous matching slot
                index = slot;
                goto success;
            }
        }

        // resize since bucket can't fit insertion
        table = nl_strmap__resize(table, value_type_size);
        map = table + 1;
    }

    // insert
    success:
    table->keys[index] = key;

    size_t val_slot = table->load++;
    table->indices[index] = val_slot;
    return (NL_Strmap__Insert){map, index, val_slot};
}

NL_API ptrdiff_t nl_strmap__insert2(void** map, NL_Slice key, size_t value_type_size) {
    NL_Strmap__Insert i = nl_strmap__insert(*map, key, value_type_size);
    *map = i.new_map;
    return i.val_slot;
}

NL_API void nl_strmap__clear(NL_StrmapHeader* restrict table) {
    memset(table->keys, 0, sizeof(NL_Slice) * table->size);
    table->load = 0;
}

NL_API ptrdiff_t nl_strmap__get(NL_StrmapHeader* restrict table, NL_Slice key) {
    size_t index = fnv1a(key.length, key.data) % table->size;

    // make sure we have an empty slot
    for (size_t i = 0; i < NL_STRMAP_BUCKET_SIZE; i++) {
        size_t slot = (index + i) % table->size;

        if (table->keys[slot].length == key.length &&
            memcmp(table->keys[slot].data, key.data, key.length) == 0) {
            return table->indices[slot];
        }
    }

    return -1;
}

#endif /* NL_STRING_MAP_IMPL */
