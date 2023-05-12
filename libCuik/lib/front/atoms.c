#include "cuik.h"
#include "atoms.h"

enum { INTERNER_EXP = 24 };

thread_local static Arena atoms_arena;

thread_local static size_t interner_len;
thread_local static Atom* interner;

void atoms_dump_stats(void) {
    printf("Atoms arena: %zu MB\n", arena_get_memory_usage(&atoms_arena) / (1024*1024));
}

Atom atoms_put(size_t len, const unsigned char* str) {
    if (!interner) {
        interner = cuik__valloc(sizeof(Atom) * (1u << INTERNER_EXP));
    }

    uint32_t hash = murmur3_32(str, len);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << INTERNER_EXP) - 1;
        uint32_t step = (hash >> (32 - INTERNER_EXP)) | 1;
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

Atom atoms_putc(const char* str) {
    return atoms_put(strlen(str), (const unsigned char*) str);
}

Atom atoms_putuc(const unsigned char* str) {
    return atoms_put(strlen((const char*) str), str);
}

Atom atoms_eat_token(TokenStream* restrict s) {
    Token* t = tokens_get(s);
    if (t->type != TOKEN_IDENTIFIER) {
        return NULL;
    }

    tokens_next(s);
    return atoms_put(t->content.length, t->content.data);
}
