#include "common.h"
#include <stdalign.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define TEMPORARY_STORAGE_SIZE (1 << 20)

typedef struct TemporaryStorage {
	size_t used;
	size_t _;
	
	uint8_t data[];
} TemporaryStorage;

static _Thread_local TemporaryStorage* temp_storage;

void tls_init() {
	if (temp_storage == NULL) {
#if _WIN32
		temp_storage = VirtualAlloc(NULL, TEMPORARY_STORAGE_SIZE, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
#else
		temp_storage = aligned_alloc(16, TEMPORARY_STORAGE_SIZE);
#endif
	}
	
	temp_storage->used = 0;
}

void* tls_push(size_t size) {
	if (sizeof(TemporaryStorage) + temp_storage->used + size >= TEMPORARY_STORAGE_SIZE) {
		printf("temporary storage: out of memory!\n");
		abort();
	}
    
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
	
	//size_t align_mask = sizeof(max_align_t) - 1;
	//temp_storage->used = (temp_storage->used + align_mask) & ~align_mask;
	
	return &temp_storage->data[temp_storage->used];
}

void tls_restore(void* p) {
	size_t i = ((uint8_t*)p) - temp_storage->data;
	assert(i <= temp_storage->used);
	
	temp_storage->used = i;
}
