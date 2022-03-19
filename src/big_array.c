#include "big_array.h"

void* big_array_internal_create(size_t type_size, bool has_zero_slot) {
    BigArrayHeader* header = malloc(sizeof(BigArrayHeader) + (type_size * INITIAL_CAP));
    *header = (BigArrayHeader){
        .size = has_zero_slot ? 1 : 0, 
        .capacity = INITIAL_CAP
    };
	if (has_zero_slot) memset(header->data, 0, type_size);
    return &header->data[0];
}

void big_array_internal_destroy(void* ptr) {
    BigArrayHeader* header = ((BigArrayHeader*) ptr) - 1;
    free(header);
}

void* big_array_internal_reserve(void* ptr, size_t type_size, size_t extra) {
    BigArrayHeader* header = ((BigArrayHeader*) ptr) - 1;
	
    if (header->size + extra >= header->capacity) {
        header->capacity = (header->size + extra) * 2;
        BigArrayHeader* new_ptr = realloc(header, sizeof(BigArrayHeader) + (type_size * header->capacity));
        if (!new_ptr) abort();
        
        return &new_ptr->data[0];
    }
	
    return ptr;
}
