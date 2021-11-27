#include "common.h"

#define TEMPORARY_STORAGE_SIZE (1 << 20)

typedef struct TemporaryStorage {
	uint32_t used;
	uint8_t data[];
} TemporaryStorage;

static _Thread_local TemporaryStorage* temp_storage;

void tls_init() {
	temp_storage = malloc(TEMPORARY_STORAGE_SIZE);
	temp_storage->used = 0;
}

void* tls_push(size_t size) {
	assert(sizeof(TemporaryStorage) + temp_storage->used + size < TEMPORARY_STORAGE_SIZE);
    
	void* ptr = &temp_storage->data[temp_storage->used];
	temp_storage->used += size;
	return ptr;
}

void* tls_pop(size_t size) {
	assert(sizeof(TemporaryStorage) + temp_storage->used > size);
    
	temp_storage->used -= size;
	return &temp_storage->data[temp_storage->used];
}

void* tls_save() {
	assert(sizeof(TemporaryStorage) + temp_storage->used);
	
	return &temp_storage->data[temp_storage->used];
}

void tls_restore(void* p) {
	size_t i = ((uint8_t*)p) - temp_storage->data;
	assert(i < temp_storage->used);
	
	temp_storage->used = i;
}

void* tls_peek(size_t distance) {
	assert(sizeof(TemporaryStorage) + temp_storage->used > distance);
    
	return &temp_storage->data[temp_storage->used - distance];
}
