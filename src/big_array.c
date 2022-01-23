#include "big_array.h"

void* __big_array_create(size_t type_size) {
    BigArrayHeader* header = malloc(sizeof(BigArrayHeader) + (type_size * INITIAL_CAP));
    *header = (BigArrayHeader){
        .size = 1, 
        .capacity = INITIAL_CAP
    };
	memset(header->data, 0, type_size);
    return &header->data[0];
}

void __big_array_destroy(void* ptr) {
    BigArrayHeader* header = ((BigArrayHeader*) ptr) - 1;
    free(header);
}

void* __big_array_reserve(void* ptr, size_t type_size, size_t extra) {
    BigArrayHeader* header = ((BigArrayHeader*) ptr) - 1;
	
    if (header->size + extra >= header->capacity) {
        header->capacity = (header->size + extra) * 2;
        BigArrayHeader* new_ptr = realloc(header, sizeof(BigArrayHeader) + (type_size * header->capacity));
        if (!new_ptr) abort();
        
        return &new_ptr->data[0];
    }
	
    return ptr;
}
