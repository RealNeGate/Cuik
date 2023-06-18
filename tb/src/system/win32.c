#ifdef _WIN32
#include "../tb_internal.h"
#include <ctype.h>

// NOTE(NeGate): i'm sorry but i had to do it to em
// this is just a random hack around mimalloc needing toupper but having weird
// CRT compat issues
#ifndef __GNUC__
#pragma comment(linker, "/alternatename:__imp_toupper=toupper")
#endif

void* guard_malloc(size_t size) {
    size_t baseline = 4096 - (size % 4096);
    size_t end = (baseline + size + 4095) & ~4095;
    char* ptr = VirtualAlloc(NULL, end + 4096, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

    // add guard page at the end of the allocation
    DWORD old_protect;
    if (!VirtualProtect(ptr + end, 4096, PAGE_NOACCESS, &old_protect)) {
        tb_panic("VirtualProtect failed!\n");
    }

    return ptr + baseline;
}

void* guard_realloc(void* ptr, size_t size) {
    tb_todo();

    /* char* new_buffer = guard_malloc(size);
    char* page_start = (char*) (((uintptr_t) ptr) & ~0xFFFull);

    MEMORY_BASIC_INFORMATION range;
    VirtualQueryEx(GetCurrentProcess(), page_start, &range, size + 8192);

    __debugbreak();
    memcpy(new_buffer, ptr, range.RegionSize);
    guard_free(ptr);

    return new_buffer; */
}

void guard_free(void* ptr) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}

void* tb_platform_valloc(size_t size) {
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void tb_platform_vfree(void* ptr, size_t size) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    DWORD protect;
    switch (prot) {
        case TB_PAGE_READONLY: protect = PAGE_READONLY; break;
        case TB_PAGE_READWRITE: protect = PAGE_READWRITE; break;
        case TB_PAGE_READEXECUTE: protect = PAGE_EXECUTE_READ; break;
        default: return false;
    }

    DWORD old_protect;
    return VirtualProtect(ptr, size, protect, &old_protect);
}
#endif