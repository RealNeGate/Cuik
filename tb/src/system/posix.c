#ifdef _POSIX_C_SOURCE
#include "../tb_internal.h"

void* tb_platform_valloc(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void* tb_platform_valloc_guard(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void tb_platform_vfree(void* ptr, size_t size) {
    munmap(ptr, size);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    uint32_t protect;
    switch (prot) {
        case TB_PAGE_READONLY:    protect = PROT_READ; break;
        case TB_PAGE_READWRITE:   protect = PROT_READ | PROT_WRITE; break;
        case TB_PAGE_READEXECUTE: protect = PROT_READ | PROT_EXEC; break;
        default: return false;
    }

    return mprotect(ptr, size, protect) == 0;
}
#endif
