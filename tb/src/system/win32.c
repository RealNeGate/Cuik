#ifdef _WIN32
#include "../tb_internal.h"
#include <ctype.h>

// NOTE(NeGate): i'm sorry but i had to do it to em
// this is just a random hack around mimalloc needing toupper but having weird
// CRT compat issues
#ifndef __GNUC__
#pragma comment(linker, "/alternatename:__imp_toupper=toupper")
#endif

void* tb_platform_valloc(size_t size) {
    return VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
}

void tb_platform_vfree(void* ptr, size_t size) {
    VirtualFree(ptr, 0, MEM_RELEASE);
}

bool tb_platform_vprotect(void* ptr, size_t size, TB_MemProtect prot) {
    DWORD protect;
    switch (prot) {
        case TB_PAGE_RO:  protect = PAGE_READONLY; break;
        case TB_PAGE_RW:  protect = PAGE_READWRITE; break;
        case TB_PAGE_RX:  protect = PAGE_EXECUTE_READ; break;
        case TB_PAGE_RXW: protect = PAGE_EXECUTE_READWRITE; break;
        default: return false;
    }

    DWORD old_protect;
    return VirtualProtect(ptr, size, protect, &old_protect);
}

size_t get_large_pages(void) {
    static bool init;
    static size_t large_page_size;

    if (!init) {
        init = true;

        unsigned long err = 0;
        HANDLE token = NULL;
        if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &token)) {
            goto err;
        }

        TOKEN_PRIVILEGES tp;
        if (!LookupPrivilegeValue(NULL, TEXT("SeLockMemoryPrivilege"), &tp.Privileges[0].Luid)) {
            goto err;
        }

        tp.PrivilegeCount = 1;
        tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (!AdjustTokenPrivileges(token, FALSE, &tp, 0, (PTOKEN_PRIVILEGES)NULL, 0)) {
            goto err;
        }

        large_page_size = GetLargePageMinimum();

        err:
        CloseHandle(token);
    }

    return large_page_size;
}

void* tb_jit_create_stack(size_t* out_size) {
    size_t size = get_large_pages();

    // TODO(NeGate): we don't support non-large page stacks
    if (size == 0) tb_todo();

    // natural alignment stack because it makes it easy to always find
    // the base.
    MEM_EXTENDED_PARAMETER param = {
        .Type = MemExtendedParameterAddressRequirements,
        .Pointer = &(MEM_ADDRESS_REQUIREMENTS){ .Alignment = size }
    };

    *out_size = size;
    return VirtualAlloc2(GetCurrentProcess(), NULL, size, MEM_RESERVE | MEM_COMMIT | MEM_LARGE_PAGES, PAGE_READWRITE, &param, 1);
}

#endif
