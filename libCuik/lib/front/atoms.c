#include "cuik.h"
#include "atoms.h"

enum { INTERNER_EXP = 24 };

thread_local static TB_Arena* atoms_arena;
thread_local static Atom* interner;

void atoms_free(void) {
    CUIK_TIMED_BLOCK("free atoms") {
        tb_arena_destroy(atoms_arena);
        cuik__vfree(interner, (1u << INTERNER_EXP) * sizeof(Atom));
        interner = NULL;
    }
}

size_t atoms_len(Atom str) {
    return *(uint32_t*) &str[-4];
}

Atom atoms_put(size_t len, const unsigned char* str) {
    if (interner == NULL) {
        CUIK_TIMED_BLOCK("alloc atoms") {
            interner = cuik__valloc((1u << INTERNER_EXP) * sizeof(Atom));
            atoms_arena = tb_arena_create(TB_ARENA_MEDIUM_CHUNK_SIZE);
        }
    }

    uint32_t mask = (1 << INTERNER_EXP) - 1;
    uint32_t hash = tb__murmur3_32(str, len);
    size_t first = hash & mask, i = first;

    do {
        // linear probe
        if (LIKELY(interner[i] == NULL)) {
            Atom newstr = tb_arena_unaligned_alloc(atoms_arena, len + 5);
            uint32_t len32 = len;
            memcpy(newstr, &len32, 4);
            memcpy(newstr + 4, str, len);
            newstr[len + 4] = 0;

            interner[i] = newstr + 4;
            return &newstr[4];
        } else if (len == atoms_len(interner[i]) && memcmp(str, interner[i], len) == 0) {
            return interner[i];
        }

        i = (i + 1) & mask;
    } while (i != first);

    log_error("atoms arena: out of memory!\n");
    abort();
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
