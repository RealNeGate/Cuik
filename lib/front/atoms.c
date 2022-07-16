#include "cuik.h"
#include "atoms.h"

thread_local static Arena atoms_arena;

void atoms_init() {
}

void atoms_deinit() {
    arena_free(&atoms_arena);
}

Atom atoms_put(size_t len, const unsigned char* str) {
    Atom newstr = arena_alloc(&atoms_arena, len + 1, 1);
    memcpy(newstr, str, len);
    newstr[len] = 0;
    return newstr;
}

Atom atoms_putc(const unsigned char* str) {
    return atoms_put(strlen((const char*)str), str);
}
