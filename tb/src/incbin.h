// https://gist.github.com/mmozeiko/ed9655cf50341553d282
#define STR2(x) #x
#define STR(x) STR2(x)

// this aligns start address to 16 and terminates byte array with explict 0
// which is not really needed, feel free to change it to whatever you want/need
#define INCBIN(name, file) \
__asm__(".section .rodata\n" \
    ".global incbin_" STR(name) "_start\n" \
    ".balign 16\n" \
    "incbin_" STR(name) "_start:\n" \
    ".incbin \"" file "\"\n" \
    \
    ".global incbin_" STR(name) "_end\n" \
    ".balign 1\n" \
    "incbin_" STR(name) "_end:\n" \
    ".byte 0\n" \
); \
extern const __attribute__((aligned(16))) void* incbin_ ## name ## _start; \
extern const void* incbin_ ## name ## _end; \
