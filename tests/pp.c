
#define LIL_UEFI_GLUE(a,b) a##b
#define LIL_UEFI_STATIC_ASSERT(c) char LIL_UEFI_GLUE(efi_static_assert_, __LINE__)[(c)?(1):(-1)]

LIL_UEFI_STATIC_ASSERT(sizeof(int) == 4);
