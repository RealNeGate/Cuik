#include "../tb_internal.h"
#include <ctype.h>

#pragma comment(lib, "onecore.lib")

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