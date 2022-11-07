#include "cuik.h"
#include "atoms.h"

#define NL_STRING_MAP_IMPL
#define NL_STRING_MAP_INLINE
#include "../string_map.h"

thread_local static Arena atoms_arena;
thread_local NL_Strmap(Atom) interner;

void atoms_init(void) {
    interner = nl_strmap_alloc(Atom, 65536);
}

void atoms_deinit(void) {
    arena_free(&atoms_arena);
}

void atoms_dump_stats(void) {
    printf("Atoms arena: %zu MB\n", arena_get_memory_usage(&atoms_arena) / (1024*1024));
}

Atom atoms_put(size_t len, const unsigned char* str) {
    ptrdiff_t search = nl_strmap_get(interner, ((NL_Slice){ len, str }));
    if (search >= 0) {
        // printf("Reused memory! %zu bytes\n", len+1);
        return interner[search];
    }

    Atom newstr = arena_alloc(&atoms_arena, len + 1, 1);
    memcpy(newstr, str, len);
    newstr[len] = 0;

    nl_strmap_put(interner, ((NL_Slice){ len, str }), newstr);
    return newstr;
}

Atom atoms_putc(const unsigned char* str) {
    return atoms_put(strlen((const char*)str), str);
}
