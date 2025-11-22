#include <stddef.h>
// This is the TB unity build
void* tb_jit_stack_create(size_t);

#define TB_GEN_IMPL
#include "tb.c"
#include "hash.c"
#include "abi.c"
#include "tb_builder.c"
#include "debug_builder.c"
#include "exporter.c"
#include "disasm.c"

// JIT
#include "jit.c"

// Optimizer
#include "new_builder.c"
#include "opt/optimizer.c"

// Regalloc
#include "codegen.c"
#include "rogers_ra.c"
#include "briggs_ra.c"
#include "ra_split.c"

// Parsers
#define TB_COFF_IMPL
#include <tb_coff.h>

// Debug
#include "debug/cv.c"
#include "debug/sdg.c"

// Objects
#include "objects/coff_parse.c"
#include "objects/coff.c"
#include "objects/elf64.c"
#include "objects/macho.c"
#include "objects/wasm_obj.c"

// Sandbird (HTTP server library)
#include "sandbird/sandbird.c"

int uf_find(int* uf, int uf_len, int a) {
    if (a >= uf_len) {
        return a;
    }

    // leader
    int l = a;
    while (uf[l] != l) {
        l = uf[l];
    }

    // path compaction
    while (uf[a] != a) {
        int p = uf[a];
        uf[a] = l, a = p;
    }

    return l;
}

int uf_union(int* uf, int x, int y) {
    x = uf_find(uf, INT_MAX, x);
    y = uf_find(uf, INT_MAX, y);

    // parent should be the smaller number
    if (x > y) {
        SWAP(int, x, y);
    }

    if (x != y) {
        uf[y] = x;
    }
    return x;
}

// Platform layer
#if defined(_WIN32)
#pragma comment(lib, "onecore.lib")

void* tb_platform_valloc(size_t size) {
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void tb_platform_vfree(void* ptr, size_t size) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    DWORD protect;
    switch (prot) {
        case TB_PAGE_NONE:protect = PAGE_NOACCESS; break;
        case TB_PAGE_RO:  protect = PAGE_READONLY; break;
        case TB_PAGE_RW:  protect = PAGE_READWRITE; break;
        case TB_PAGE_RX:  protect = PAGE_EXECUTE_READ; break;
        case TB_PAGE_RXW: protect = PAGE_EXECUTE_READWRITE; break;
        default: return false;
    }

    DWORD old_protect;
    return VirtualProtect(ptr, size, protect, &old_protect);
}

#if NTDDI_VERSION >= NTDDI_WIN10_RS4
void* tb_jit_stack_create(size_t size) {
    // natural alignment stack because it makes it easy to always find
    // the base.
    MEM_EXTENDED_PARAMETER param = {
        .Type = MemExtendedParameterAddressRequirements,
        .Pointer = &(MEM_ADDRESS_REQUIREMENTS){ .Alignment = size }
    };

    return VirtualAlloc2(GetCurrentProcess(), NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE, &param, 1);
}
#endif /* NTDDI_VERSION >= NTDDI_WIN10_RS4 */
#elif defined(_POSIX_C_SOURCE)

void* tb_jit_stack_create(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_HUGETLB, -1, 0);
}

void* tb_platform_valloc(size_t size) {
    return mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void tb_platform_vfree(void* ptr, size_t size) {
    munmap(ptr, size);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    uint32_t protect;
    switch (prot) {
        case TB_PAGE_NONE:protect = 0; break;
        case TB_PAGE_RO:  protect = PROT_READ; break;
        case TB_PAGE_RW:  protect = PROT_READ | PROT_WRITE; break;
        case TB_PAGE_RX:  protect = PROT_READ | PROT_EXEC; break;
        case TB_PAGE_RXW: protect = PROT_READ | PROT_WRITE | PROT_EXEC; break;
        default: return false;
    }

    return mprotect(ptr, size, protect) == 0;
}
#endif
