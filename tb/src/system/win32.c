#ifdef _WIN32
#include "../tb_internal.h"
#include <ctype.h>

// NOTE(NeGate): i'm sorry but i had to do it to em
// this is just a random hack around mimalloc needing toupper but having weird
// CRT compat issues
#pragma comment(linker, "/alternatename:__imp_toupper=toupper")

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