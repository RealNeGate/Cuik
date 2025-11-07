#include <hashes.h>

static bool macro_def_compare(void* a, void* b) {
    MacroDef *x = a, *y = b;

    assert(y->key.length != MACRO_DEF_TOMBSTONE);
    if (x->key.length == MACRO_DEF_TOMBSTONE) {
        return false;
    }

    return string_equals(&x->key, &y->key);
}

static uint32_t macro_def_hash(void* a) {
    MacroDef *x = a;
    assert(x->key.length != MACRO_DEF_TOMBSTONE);
    return tb__murmur3_32((const unsigned char*) x->key.data, x->key.length);
}

static MacroDef* insert_symtab(Cuik_CPP* ctx, size_t len, const char* key) {
    MacroDef* def = tb_arena_alloc(&ctx->perm_arena, sizeof(MacroDef));
    *def = (MacroDef){ .key = { len, (const unsigned char*) key } };

    void* actual_def = nl_hashset_put2(&ctx->macros, def, macro_def_hash, macro_def_compare);
    if (actual_def == NULL) {
        actual_def = def;
    } else if (actual_def != def) {
        // doesn't happen often... i think?
        tb_arena_free(&ctx->perm_arena, def, sizeof(MacroDef));
    }
    return actual_def;
}

void cuikpp_define_empty_cstr(Cuik_CPP* ctx, const char* key) {
    assert(*key != 0);
    cuikpp_define_empty(ctx, strlen(key), key);
}

void cuikpp_define_cstr(Cuik_CPP* ctx, const char* key, const char* val) {
    assert(*val != 0);
    cuikpp_define(ctx, strlen(key), key, strlen(val), val);
}

void cuikpp_define_empty(Cuik_CPP* ctx, size_t keylen, const char* key) {
    assert(keylen > 0);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (keylen + 15) & ~15;
    char* newkey = tb_arena_alloc(&ctx->tmp_arena, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    keylen = *paren == '(' ? paren - newkey : keylen;

    insert_symtab(ctx, keylen, newkey);
}

void cuikpp_define(Cuik_CPP* ctx, size_t keylen, const char* key, size_t vallen, const char* value) {
    assert(keylen > 0);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (keylen + 15) & ~15;
    char* newkey = tb_arena_alloc(&ctx->tmp_arena, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    size_t len = *paren == '(' ? paren - newkey : keylen;

    unsigned char* newvalue;
    {
        pad_len = (vallen + 15) & ~15;
        newvalue = tb_arena_alloc(&ctx->tmp_arena, pad_len);
        memcpy(newvalue, value, vallen);

        size_t rem = pad_len - vallen;
        memset(newvalue + vallen, 0, rem);
    }

    MacroDef* def = insert_symtab(ctx, len, newkey);
    def->value = (String){ vallen, newvalue };
}

void cuikpp_undef_cstr(Cuik_CPP* ctx, const char* key) {
    return cuikpp_undef(ctx, strlen(key), key);
}

void cuikpp_undef(Cuik_CPP* ctx, size_t keylen, const char* key) {
    MacroDef def = { .key = { keylen, (const unsigned char*) key } };
    nl_hashset_remove2(&ctx->macros, &def, macro_def_hash, macro_def_compare);
}

// 16byte based compare
// it doesn't need to be aligned but the valid range must be (len + 15) & ~15
static bool memory_equals16(const unsigned char* src1, const unsigned char* src2, size_t length) {
    #if USE_INTRIN && CUIK__IS_X64
    size_t i = 0;
    size_t chunk_count = length / 16;
    while (chunk_count--) {
        __m128i in1 = _mm_loadu_si128((__m128i*)&src1[i]);
        __m128i in2 = _mm_loadu_si128((__m128i*)&src2[i]);

        int compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
        if (compare != 0xFFFF) return false;

        i += 16;
    }

    uint16_t mask = 0xFFFF << (length % 16);
    __m128i in1 = _mm_loadu_si128((__m128i*)&src1[i]);
    __m128i in2 = _mm_loadu_si128((__m128i*)&src2[i]);

    uint16_t compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
    return (compare | mask) == 0xFFFF;
    #else
    return memcmp(src1, src2, length) == 0;
    #endif
}

static MacroDef* find_define(Cuik_CPP* restrict ctx, const unsigned char* start, size_t length) {
    #if CUIK__CPP_STATS
    uint64_t start_ns = cuik_time_in_nanos();
    #endif

    MacroDef def = { .key = { length, start } };
    MacroDef* actual_def = nl_hashset_get2(&ctx->macros, &def, macro_def_hash, macro_def_compare);

    #if CUIK__CPP_STATS
    uint64_t end_ns = cuik_time_in_nanos();
    ctx->total_define_access_time += (end_ns - start_ns);
    ctx->total_define_accesses += 1;
    #endif

    return actual_def;
}

bool cuikpp_find_define_cstr(Cuik_CPP* restrict ctx, Cuik_DefineIter* out_ref, const char* key) {
    MacroDef* def = find_define(ctx, (const unsigned char*) key, strlen(key));
    out_ref->loc = def->loc;
    out_ref->key = def->key;
    out_ref->value = def->value;
    return true;
}

bool cuikpp_find_define(Cuik_CPP* restrict ctx, Cuik_DefineIter* out_ref, size_t keylen, const char key[]) {
    MacroDef* def = find_define(ctx, (const unsigned char*) key, keylen);
    out_ref->loc = def->loc;
    out_ref->key = def->key;
    out_ref->value = def->value;
    return true;
}

static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length) {
    return find_define(c, start, length);
}

static size_t hide_macro(Cuik_CPP* restrict c, MacroDef* def) {
    size_t saved = def->key.length;
    def->key.length = MACRO_DEF_TOMBSTONE;
    return saved;
}

static void unhide_macro(Cuik_CPP* restrict c, MacroDef* def, size_t saved) {
    def->key.length = saved;
}
