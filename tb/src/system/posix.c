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
        case TB_PAGE_RO:  protect = PROT_READ; break;
        case TB_PAGE_RW:  protect = PROT_READ | PROT_WRITE; break;
        case TB_PAGE_RX:  protect = PROT_READ | PROT_EXEC; break;
        case TB_PAGE_RXW: protect = PROT_READ | PROT_WRITE | PROT_EXEC; break;
        default: return false;
    }

    return mprotect(ptr, size, protect) == 0;
}
#endif
