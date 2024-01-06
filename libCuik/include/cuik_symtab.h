// This module is mostly used for compiler construction which will eventually
// become a focus, for now it's just factoring out parser code.
#ifndef CUIK_SYMTAB_H
#define CUIK_SYMTAB_H

#include "cuik_prelude.h"

// interned string, generated with atoms_put (or it's sisters)
typedef char* Cuik_Atom;

// in C, there's two symbol tables used (one for tags, another for proper symbols)
// they're both sharing the same arena and scopes
typedef struct Cuik_SymbolTable Cuik_SymbolTable;

CUIK_API Cuik_SymbolTable* cuik_symtab_create(void* not_found);

CUIK_API void cuik_scope_open(Cuik_SymbolTable* st);
CUIK_API void cuik_scope_close(Cuik_SymbolTable* st);

// define a symbol, the pointer returned is stable across the lifetime (until it's popped
// off by a scope)
CUIK_API void* cuik_symtab_put(Cuik_SymbolTable* st, Cuik_Atom name, size_t size);
#define CUIK_SYMTAB_PUT(st, name, T) ((T*) cuik_symtab_put(st, name, sizeof(T)))

CUIK_API void* cuik_symtab_lookup(Cuik_SymbolTable* st, Cuik_Atom name);
#define CUIK_SYMTAB_LOOKUP(st, name, T) *((T*) cuik_symtab_lookup(st, name))

// Alternative to cuik_symtab_lookup (NOT AN "UPDATE"), this time we write to *in_scope
// if the value exists in the scope we're currently in
CUIK_API void* cuik_symtab_lookup2(Cuik_SymbolTable* st, Cuik_Atom name, bool* in_scope);
#define CUIK_SYMTAB_LOOKUP2(st, name, in_scope, T) *((T*) cuik_symtab_lookup2(st, name, in_scope))

// use of the globals iterator when we're not in the TU which IMPL
CUIK_API size_t cuik_symtab_global_iter(Cuik_SymbolTable* st, size_t it);
CUIK_API void* cuik_symtab_global_at(Cuik_SymbolTable* st, size_t it);

#endif // CUIK_SYMTAB_H

#ifdef CUIK_SYMTAB_IMPL
#undef CUIK_SYMTAB_IMPL

#include <common.h>
#include <arena.h>
#include <hash_map.h>

typedef struct { Cuik_Atom k; void* v; } Cuik_SymbolNamePair;

typedef struct Cuik_Scope Cuik_Scope;
struct Cuik_Scope {
    Cuik_Scope* last;

    uint32_t start;     // always 0 for global scope. index to the first symbol
    uint32_t watermark; // the restore point for the symbol's linear allocator.
};

// simple hash table for the globals, every other scope is reverse
// linear searched for now.
enum { CUIK__MAX_LOCALS = 1<<14, CUIK__BUFFER_CAP = 1u<<20u };
struct Cuik_SymbolTable {
    NL_Map(Cuik_Atom, void*) globals;

    Cuik_Scope* top;
    size_t watermark;
    uint8_t* buffer;
    void* not_found;

    TB_Arena* globals_arena;

    size_t local_count;
    Cuik_SymbolNamePair locals[CUIK__MAX_LOCALS];
};

Cuik_SymbolTable* cuik_symtab_create(void* not_found) {
    Cuik_SymbolTable* st = cuik_malloc(sizeof(Cuik_SymbolTable));
    nl_map_create(st->globals, 2048);
    st->watermark = 0;
    st->buffer = cuik_malloc(CUIK__BUFFER_CAP);
    st->local_count = 0;
    st->top = NULL;
    st->not_found = not_found;
    st->globals_arena = tb_arena_create(TB_ARENA_MEDIUM_CHUNK_SIZE);
    return st;
}

void cuik_symtab_destroy(Cuik_SymbolTable* st) {
    tb_arena_destroy(st->globals_arena);
    nl_map_free(st->globals);
    cuik_free(st->buffer);
    cuik_free(st);
}

void cuik_symtab_clear(Cuik_SymbolTable* st) {
    tb_arena_clear(st->globals_arena);
    nl_map_free(st->globals);

    st->watermark = st->local_count = 0;
    st->top = NULL;

    assert(0 && "TODO");
}

static void* cuik_symtab__alloc(Cuik_SymbolTable* st, size_t size, bool is_global) {
    if (is_global) {
        return tb_arena_alloc(st->globals_arena, size);
    }

    size_t align_mask = TB_ARENA_ALIGNMENT - 1;
    assert(st->watermark + size + align_mask < CUIK__BUFFER_CAP);

    void* ptr = &st->buffer[st->watermark];
    st->watermark = (st->watermark + size + align_mask) & ~align_mask;
    return ptr;
}

void cuik_scope_open(Cuik_SymbolTable* st) {
    // we wanna store a watermark before we alloc the scope
    size_t wm = st->watermark;

    // allocate scope
    Cuik_Scope* scope = cuik_symtab__alloc(st, sizeof(Cuik_Scope), false);
    scope->last = st->top;
    scope->watermark = wm;
    scope->start = st->local_count;
    st->top = scope;
}

void cuik_scope_close(Cuik_SymbolTable* st) {
    assert(st->top != NULL && "can't pop the global scope");

    Cuik_Scope* prev = st->top;
    st->watermark = prev->watermark;
    st->local_count = prev->start;
    st->top = prev->last;
}

void* cuik_symtab_put(Cuik_SymbolTable* st, Cuik_Atom name, size_t size) {
    void* ptr = cuik_symtab__alloc(st, size, st->top == NULL);

    if (st->top == NULL) {
        // put into global scope
        nl_map_put(st->globals, name, ptr);
    } else {
        st->locals[st->local_count++] = (Cuik_SymbolNamePair){ name, ptr };
    }

    return ptr;
}

void* cuik_symtab_lookup(Cuik_SymbolTable* st, Cuik_Atom name) {
    for (size_t i = st->local_count; i--;) {
        if (st->locals[i].k == name) {
            return st->locals[i].v;
        }
    }

    ptrdiff_t search = nl_map_get(st->globals, name);
    return search >= 0 ? st->globals[search].v : st->not_found;
}

void* cuik_symtab_lookup2(Cuik_SymbolTable* st, Cuik_Atom name, bool* in_scope) {
    for (size_t i = st->local_count; i--;) {
        if (st->locals[i].k == name) {
            *in_scope = (st->top && st->top->start >= i);
            return st->locals[i].v;
        }
    }

    ptrdiff_t search = nl_map_get(st->globals, name);
    if (search >= 0) {
        *in_scope = (st->top == NULL);
        return st->globals[search].v;
    }
    return st->not_found;
}

size_t cuik_symtab_global_iter(Cuik_SymbolTable* st, size_t it) {
    // skip until next actual global
    size_t cap = nl_map_get_capacity(st->globals);
    while (it < cap && st->globals[it].k == NULL) it++;

    return it;
}

void* cuik_symtab_global_at(Cuik_SymbolTable* st, size_t it) {
    return st->globals[it].v;
}

// uses static iterator
#define CUIK_SYMTAB_FOR_GLOBALS(it, st) for (size_t it = 0, _end_ = nl_map_get_capacity((st)->globals); it < _end_; it++) if ((st)->globals[it].k != NULL)

#else // CUIK_SYMTAB_IMPL

// uses opaque iterator
#define CUIK_SYMTAB_FOR_GLOBALS(it, st) for (size_t it = 0; (it = cuik_symtab_global_iter(st)) != SIZE_MAX; it++)

#endif
