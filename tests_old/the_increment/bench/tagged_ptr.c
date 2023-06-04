#include <stdint.h>
#include <stddef.h>

typedef enum {
    TAG_NULL,
    TAG_NUMBER,
    TAG_OBJECT,
} Tag;

typedef struct {
    uint64_t tag : 16;
    uint64_t off : 48;
} TaggedPtr;
_Static_assert(sizeof(TaggedPtr) == sizeof(uint64_t), "Doesn't match :(");

extern char program_memory[];

int* get_int(TaggedPtr p) {
    if (p.tag != TAG_NUMBER) __builtin_trap();
    
    return (int*) &program_memory[p.off];
}
