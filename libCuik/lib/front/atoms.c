#include "cuik.h"
#include "atoms.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include <string_map.h>

enum { INTERNER_EXP = 24 };

thread_local static Arena atoms_arena;

thread_local static size_t interner_len;
thread_local static Atom* interner;

// murmur3 32-bit without UB unaligned accesses
// https://github.com/demetri/scribbles/blob/master/hashing/ub_aware_hash_functions.c
static uint64_t hash_ident(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k;
        memcpy(&k, &key[i * 4], sizeof(k));

        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    }

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}

void atoms_dump_stats(void) {
    printf("Atoms arena: %zu MB\n", arena_get_memory_usage(&atoms_arena) / (1024*1024));
}

Atom atoms_put(size_t len, const unsigned char* str) {
    if (!interner) {
        interner = cuik__valloc(sizeof(Atom) * (1u << INTERNER_EXP));
    }

    uint64_t hash = hash_ident(str, len);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << INTERNER_EXP) - 1;
        uint32_t step = (hash >> (64 - INTERNER_EXP)) | 1;
        i = (i + step) & mask;

        if (interner[i] == NULL) {
            // empty slot
            if (interner_len >= (1u << INTERNER_EXP)) {
                printf("Atoms arena: out of memory!\n");
                abort();
            }

            Atom newstr = arena_alloc(&atoms_arena, len + 1, 1);
            memcpy(newstr, str, len);
            newstr[len] = 0;

            interner_len++;
            interner[i] = newstr;
            return newstr;
        } else if (len == strlen(interner[i]) && memcmp(str, interner[i], len) == 0) {
            return interner[i];
        }
    }
}

Atom atoms_putc(const unsigned char* str) {
    return atoms_put(strlen((const char*)str), str);
}
