#include <cuik.h>
#include "front/parser.h"

struct Cuik_FileCache {
    mtx_t lock;
    NL_Strmap(TokenStream) table;
};

Cuik_FileCache* cuik_fscache_create(void) {
    Cuik_FileCache* c = HEAP_ALLOC(sizeof(Cuik_FileCache));
    memset(c, 0, sizeof(Cuik_FileCache));

    c->table = nl_strmap_alloc(TokenStream, 1024);
    mtx_init(&c->lock, mtx_plain);
    return c;
}

void cuik_fscache_destroy(Cuik_FileCache* restrict c) {
    nl_strmap_for(i, c->table) {
        dyn_array_destroy(c->table[i].list.tokens);
        dyn_array_destroy(c->table[i].files);
    }

    mtx_destroy(&c->lock);
    HEAP_FREE(c);
}

void cuik_fscache_put(Cuik_FileCache* restrict c, const char* filepath, const TokenStream* tokens) {
    mtx_lock(&c->lock);
    nl_strmap_put_cstr(c->table, filepath, *tokens);
    mtx_unlock(&c->lock);
}

bool cuik_fscache_query(Cuik_FileCache* restrict c, const char* filepath) {
    return nl_strmap_get_cstr(c->table, filepath) >= 0;
}

bool cuik_fscache_lookup(Cuik_FileCache* restrict c, const char* filepath, TokenStream* out_tokens) {
    mtx_lock(&c->lock);
    ptrdiff_t search = nl_strmap_get_cstr(c->table, filepath);
    if (search >= 0) {
        if (out_tokens) *out_tokens = c->table[search];
    }
    mtx_unlock(&c->lock);

    return (search >= 0);
}
