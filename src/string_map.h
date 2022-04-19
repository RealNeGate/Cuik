// really basic string map
// can't remove shit in Detroit
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
#ifndef NL_STRING_MAP_H
#define NL_STRING_MAP_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
	size_t length;
	const uint8_t* data;
} NL_Slice;

typedef struct {
    size_t    size;
    NL_Slice* keys;
    void**    values;
} NL_StringMap;

#ifdef NL_STATIC
#  define NL_API static
#else
#  define NL_API extern
#endif

NL_API NL_StringMap nl_string_map_init(size_t initial_size);
NL_API void  nl_string_map_deinit(NL_StringMap* restrict table);
NL_API void  nl_string_map_insert(NL_StringMap* restrict table, const NL_Slice key, void* value);
NL_API void* nl_string_map_get(const NL_StringMap* restrict table, const NL_Slice key);
NL_API void  nl_string_map_print(const NL_StringMap* restrict table);

#endif /* NL_STRING_MAP_H */

#ifdef NL_STRING_MAP_IMPL
static uint32_t fnv1a(size_t length, const uint8_t* data) {
	uint32_t hash = 0x811C9DC5;

	for (size_t i = 0; i < length; i++) {
		hash = (data[i] ^ hash) * 0x01000193;
	}

	return hash;
}

NL_API NL_StringMap nl_string_map_init(size_t initial_size) {
    return (NL_StringMap){
        .size   = initial_size,
        .keys   = calloc(initial_size, sizeof(NL_Slice)),
        .values = malloc(initial_size * sizeof(void*))
    };
}

NL_API void nl_string_map_deinit(NL_StringMap* restrict table) {
    free(table->keys);
    free(table->values);
}

NL_API void nl_string_map_insert(NL_StringMap* restrict table, const NL_Slice key, void* value) {
    size_t index;
    while (true) {
        index = fnv1a(key.length, key.data) % table->size;

        // make sure we have an empty slot
        for (size_t i = 0; i < table->size; i++) {
            size_t slot = (index + i) % table->size;

            if (table->keys[slot].length == 0) {
                index = slot;
                goto success;
            }
        }

        // remake & rehash... probably really slow...
        NL_StringMap new_table = nl_string_map_init(table->size * 2);
        printf("REHASH %zu!!!\n", new_table.size);
        
        for (size_t i = 0; i < table->size; i++) {
            if (table->keys[i].length != 0) {
                nl_string_map_insert(&new_table, table->keys[i], table->values[i]);		
            }
        }

		nl_string_map_deinit(table);
        *table = new_table;
    }

    // insert
    success:
    table->keys[index]   = key;
    table->values[index] = value;
}

NL_API void* nl_string_map_get(const NL_StringMap* restrict table, const NL_Slice key) {
	size_t index = fnv1a(key.length, key.data) % table->size;

	// make sure we have an empty slot
	for (size_t i = 0; i < table->size; i++) {
		size_t slot = (index + i) % table->size;

		if (table->keys[slot].length == key.length &&
			memcmp(table->keys[slot].data, key.data, key.length) == 0) {
			return &table->values[slot];
		}
	}

	return NULL;
}

NL_API void nl_string_map_print(const NL_StringMap* restrict table) {
    for (size_t i = 0; i < table->size; i++) {
        if (table->keys[i].length == 0) {
            printf(" %3zu\t---\n", i);
        } else {
            printf(" %3zu\t%.*s\n", i, (int)table->keys[i].length, table->keys[i].data);
        }
    }
}

#endif /* NL_STRING_MAP_IMPL */

