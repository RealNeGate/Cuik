#define UNICODE
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static void find_imports(char* image_base) {
    if (image_base == NULL) {
        return;
    }

    // we expect a DOS header
    char* ptr = image_base;
    if (memcmp(ptr, "MZ", 2) == 0) {
        // we'll find the pointer to the PE header at the end of the DOS header
        ptr = image_base + *((uint32_t*) (ptr + 60));
    }

    if (memcmp(ptr, "PE\0\0", 4) != 0) {
        fprintf(stderr, "error: no PE header! got %.*s\n", 4, ptr);
        return;
    }

    IMAGE_NT_HEADERS64* nt = (IMAGE_NT_HEADERS64*) ptr;

    // import address table is [1]
    IMAGE_IMPORT_DESCRIPTOR* ilt_dir = (IMAGE_IMPORT_DESCRIPTOR*) (image_base + nt->OptionalHeader.DataDirectory[1].VirtualAddress);

    // the ILT is null terminated lmao
    for (size_t i = 0; ilt_dir[i].FirstThunk; i++) {
        printf("IMPORT %s\n", image_base + ilt_dir[i].Name);

        IMAGE_THUNK_DATA* thunk = (IMAGE_THUNK_DATA*) (image_base + ilt_dir[i].FirstThunk);
        IMAGE_THUNK_DATA* og_thunk = (IMAGE_THUNK_DATA*) (image_base + ilt_dir[i].OriginalFirstThunk);
        for (size_t j = 0; thunk[j].u1.Function; j++) {
            printf("  %-32s %p\n", image_base + og_thunk[j].u1.Function + 2, (void*) thunk[j].u1.Function);
        }
    }
}

int wmain(int argc, wchar_t** argv) {
    SYSTEM_INFO s;
    GetSystemInfo(&s);
    
    char* image_base = NULL;
    if (argc >= 2) {
        image_base = (char*) GetModuleHandle(argv[1]);

        if (image_base == NULL) {
            // if it wasn't loaded already, load it now
            printf("Loading %S...\n", argv[1]);
            image_base = (char*) LoadLibrary(argv[1]);

            if (image_base == NULL) {
                printf("Could not load %S\n", argv[1]);
                return 1;
            }
        }
    } else {
        image_base = (char*) GetModuleHandle(NULL);
    }

    find_imports(image_base);
    return 0;
}
