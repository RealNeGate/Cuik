#include "memory.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void* allocate_virtual_memory(size_t sz) {
    return VirtualAlloc(NULL, sz, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void free_virtual_memory(void* ptr) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}
