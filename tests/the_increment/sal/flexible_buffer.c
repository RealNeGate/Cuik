#include <sal.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

// represents a CodeView type entry, they start with 16bits for length field
typedef struct {
	uint32_t key; // points to somewhere in the debug$T section, 0 is assumed to mean nothing
	uint16_t value; // type index
} CV_TypeEntry;

typedef struct {
	size_t capacity, count;
	uint8_t* data;
} TB_Emitter;

uint16_t tb_get2b(TB_Emitter* o, uint32_t pos);
uint8_t* tb_out_reserve(TB_Emitter* o, size_t count);
void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const uint8_t* str);

#define loop(iterator, count) \
	for (size_t iterator = 0, end__ = (count); iterator < end__; ++iterator)

#define LEFTROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

// constant sized "hash map" which is used to
// deduplicate types in the codeview
#define MAX_TYPE_ENTRY_LOOKUP_SIZE 1024

_Pre_equal_to_(length % 4 == 0)
uint16_t find_or_make_cv_type(_Inout_			 					 TB_Emitter*	sect, 
							  _In_reads_(1)							 uint32_t*		type_entry_count,
							  _In_reads_(MAX_TYPE_ENTRY_LOOKUP_SIZE) CV_TypeEntry*	lookup_table,
							  _In_ 				 					 size_t 		length,
							  _In_reads_(length) 					 uint16_t* 		key) {
	// shitty hash
	uint32_t hash = 0;
	loop(i, length) {
		hash ^= key[i] << ((i % 4) * 8);
		hash = LEFTROTATE(hash, 3);
	}
	
	// Search (if there's a collision replace the old one)
	size_t index = hash % MAX_TYPE_ENTRY_LOOKUP_SIZE;
	CV_TypeEntry lookup = lookup_table[index];
	
	//printf("Lookup %zu (%x hash, has match? %s)\n", index, hash, lookup.key ? "yea" : "naw");
	if (lookup.key) {
		// verify it even matches
		size_t lookup_size = tb_get2b(sect, lookup.key) + 2;
		
		if (length == lookup_size && memcmp(key, &sect->data[lookup.key], length) == 0) {
			//printf("Saved %zu bytes (%d)\n", length, lookup.value);
			return lookup.value;
		}
	}
	
	uint16_t type_index = *type_entry_count;
	*type_entry_count += 1;
	
	//printf("Used %zu bytes (%d)\n", length, type_index);
	
	lookup_table[index].key = sect->count;
	lookup_table[index].value = type_index;
	
	tb_out_reserve(sect, length);
	tb_outs_UNSAFE(sect, length, (const uint8_t*)key);
	return type_index;
}
