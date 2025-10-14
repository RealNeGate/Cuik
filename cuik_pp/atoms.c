#include <cuik.h>
#include <hashes.h>
#include "atoms.h"
#include "../cuik_pp/lexer.h"

size_t atoms_len(Atom str);
static uint32_t atomhs_hash(const void* a) {
    const Atom sym = (Atom) a;
    return tb__murmur3_32(sym, atoms_len(sym));
}

static bool atomhs_cmp(const void* a, const void* b) {
    const Atom aa = (Atom) a;
    const Atom bb = (Atom) b;
    return atoms_len(aa) == atoms_len(bb) && memcmp(aa, bb, atoms_len(aa)) == 0;
}

#define NBHS_FN(n) atomhs_ ## n
#include <nbhs.h>

static NBHS atoms_table;
static thread_local bool atoms_init;
static thread_local TB_Arena atoms_arena;

#ifndef TB_NO_THREADS
static once_flag atoms_call_once;
#endif

void atoms_free(void) {
}

size_t atoms_len(Atom str) {
    return *(uint32_t*) &str[-4];
}

static void global_init_atoms(void) {
    atoms_table = nbhs_alloc(2000);
}

Atom atoms_put(size_t len, const unsigned char* str) {
    if (!atoms_init) {
        #ifndef TB_NO_THREADS
        call_once(&atoms_call_once, global_init_atoms);
        #else
        global_init_atoms();
        #endif

        atoms_init = true;
        tb_arena_create(&atoms_arena, "Atoms");
    }

    Atom newstr = tb_arena_unaligned_alloc(&atoms_arena, len + 5);
    uint32_t len32 = len;
    memcpy(newstr, &len32, 4);
    memcpy(newstr + 4, str, len);
    newstr[len + 4] = 0;

    Atom k = atomhs_intern(&atoms_table, newstr+4);
    if (k != newstr+4) {
        tb_arena_free(&atoms_arena, newstr, len + 5);
    }
    return k;
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
