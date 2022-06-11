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

#ifdef NL_STRING_MAP_INLINE
#define NL_API inline
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

#define nl_strmap_put(map, key, value)                                             \
    do {                                                                           \
        NL_Strmap__Insert ins__ = nl_strmap__insert((map), key, sizeof(*(value))); \
        (map) = ins__.new_map;                                                     \
        (map)[ins__.index] = (value);                                              \
    } while (0)

#define nl_strmap_put_cstr(map, key, value)                                            \
    do {                                                                               \
        NL_Slice slice__ = nl_slice__cstr(key);                                        \
        NL_Strmap__Insert ins__ = nl_strmap__insert((map), slice__, sizeof(*(value))); \
        (map) = ins__.new_map;                                                         \
        (map)[ins__.index] = (value);                                                  \
    } while (0)

#define nl_strmap_get(map, key) \
    ((map) != NULL ? nl_strmap__get(((NL_StrmapHeader*)(map)) - 1, (key)) : -1)

#define nl_strmap_get_cstr(map, key) \
    ((map) != NULL ? nl_strmap__get(((NL_StrmapHeader*)(map)) - 1, nl_slice__cstr(key)) : -1)

#define nl_strmap_remove(map, key) \
    ((map) != NULL ? nl_strmap__remove(((NL_StrmapHeader*)(map)) - 1, (key)) : false)

#define nl_strmap_remove_cstr(map, key) \
    ((map) != NULL ? nl_strmap__remove(((NL_StrmapHeader*)(map)) - 1, nl_slice__cstr(key)) : false)

#define nl_strmap_free(map, key, value)                     \
    do {                                                    \
        if ((map) != NULL) {                                \
            nl_strmap__free(((NL_StrmapHeader*)(map)) - 1); \
            (map) = NULL;                                   \
        }                                                   \
    } while (0)

// Iterator
#define nl_strmap_for(it, map)                                                     \
    for (size_t i = 0, size__ = nl_strmap__get_header(map)->size; i < size__; i++) \
        if (nl_strmap__get_header(map)->keys[i].length == 0)

/////////////////////////////////////////////////
// internals
/////////////////////////////////////////////////
#define nl_strmap__get_header(map) (((NL_StrmapHeader*)(map)) - 1)

typedef struct {
    void* new_map;
    size_t index;
} NL_Strmap__Insert;

// behind the array the user manages there's some
// information about the rest of the string map
typedef struct {
    size_t size;
    NL_Slice* keys;
    char values[];
} NL_StrmapHeader;

NL_API NL_Strmap__Insert nl_strmap__insert(void* map, NL_Slice key, size_t value_type_size);
NL_API ptrdiff_t nl_strmap__get(NL_StrmapHeader* restrict table, NL_Slice key);
NL_API bool nl_strmap__remove(NL_StrmapHeader* restrict table, NL_Slice key);
NL_API void nl_strmap__free(NL_StrmapHeader* restrict table);

inline NL_Slice nl_slice__cstr(const char* key) {
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

NL_API void nl_strmap__free(NL_StrmapHeader* restrict table) {
    free(table->keys);
    free(table);
}

NL_API NL_StrmapHeader* nl_strmap__alloc(size_t size, size_t value_type_size) {
    NL_StrmapHeader* table = malloc(sizeof(NL_StrmapHeader) + (size * value_type_size));
    table->size = size;
    table->keys = calloc(size, sizeof(NL_Slice));
    return table;
}

NL_API NL_Strmap__Insert nl_strmap__insert(void* map, NL_Slice key, size_t value_type_size) {
    NL_StrmapHeader* table;
    if (map == NULL) {
        table = nl_strmap__alloc(64, value_type_size);
        map = table->values;
    } else {
        table = ((NL_StrmapHeader*)map) - 1;
    }

    size_t index;
    while (true) {
        index = fnv1a(key.length, key.data) % table->size;

        // make sure we have an empty slot
        for (size_t i = 0; i < table->size; i++) {
            size_t slot = (index + i) % table->size;

            if (table->keys[slot].length == 0) {
                index = slot;
                goto success;
            } else if (table->keys[slot].length == key.length &&
                       memcmp(table->keys[slot].data, key.data, key.length) == 0) {
                // or... override previous matching slot
                index = slot;
                goto success;
            }
        }

        // remake & rehash... probably really slow...
        NL_StrmapHeader* new_table = nl_strmap__alloc(table->size * 2, value_type_size);

        for (size_t i = 0; i < table->size; i++) {
            if (table->keys[i].length != 0) {
                NL_Strmap__Insert ins = nl_strmap__insert(new_table->values, table->keys[i], value_type_size);

                memcpy(&table->values[i * value_type_size], &new_table->values[ins.index * value_type_size], value_type_size);
            }
        }

        nl_strmap__free(table);

        table = new_table;
        map = table + 1;
    }

// insert
success:
    table->keys[index] = key;
    return (NL_Strmap__Insert){map, index};
}

NL_API ptrdiff_t nl_strmap__get(NL_StrmapHeader* restrict table, NL_Slice key) {
    size_t index = fnv1a(key.length, key.data) % table->size;

    // make sure we have an empty slot
    for (size_t i = 0; i < table->size; i++) {
        size_t slot = (index + i) % table->size;

        if (table->keys[slot].length == key.length &&
            memcmp(table->keys[slot].data, key.data, key.length) == 0) {
            return slot;
        }
    }

    return -1;
}

NL_API bool nl_strmap__remove(NL_StrmapHeader* restrict table, NL_Slice key) {
    size_t index = fnv1a(key.length, key.data) % table->size;

    // make sure we have an empty slot
    for (size_t i = 0; i < table->size; i++) {
        size_t slot = (index + i) % table->size;

        if (table->keys[slot].length == key.length &&
            memcmp(table->keys[slot].data, key.data, key.length) == 0) {
            table->keys[slot].length = 0;
            table->keys[slot].data = NULL;
            return true;
        }
    }

    return false;
}

#endif /* NL_STRING_MAP_IMPL */
