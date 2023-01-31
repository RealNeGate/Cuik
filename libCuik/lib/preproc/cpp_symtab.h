
static size_t insert_symtab(Cuik_CPP* ctx, size_t len, const char* key) {
    uint32_t mask = (1u << ctx->macros.exp) - 1;
    uint32_t hash = hash_ident((const unsigned char*) key, len);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - ctx->macros.exp)) | 1;
        i = (i + step) & mask;

        String* k = &ctx->macros.keys[i];
        if (k->length == 0 || k->length == MACRO_DEF_TOMBSTONE) {
            // empty slot
            if (ctx->macros.len > mask) {
                printf("Symbol table: out of memory!\n");
                abort();
            }

            ctx->macros.len++;
            ctx->macros.keys[i] = (String){ len, (const unsigned char*) key };
            return i;
        } else if (len == k->length && memcmp(key, k->data, len) == 0) {
            return i;
        }
    }
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
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    keylen = *paren == '(' ? paren - newkey : keylen;

    size_t i = insert_symtab(ctx, keylen, newkey);
    ctx->macros.vals[i] = (MacroDef){ 0 };
}

void cuikpp_define(Cuik_CPP* ctx, size_t keylen, const char* key, size_t vallen, const char* value) {
    assert(keylen > 0);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (keylen + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    size_t len = *paren == '(' ? paren - newkey : keylen;

    char* newvalue;
    {
        pad_len = (vallen + 15) & ~15;
        newvalue = gimme_the_shtuffs(ctx, pad_len);
        memcpy(newvalue, value, vallen);

        size_t rem = pad_len - vallen;
        memset(newvalue + vallen, 0, rem);
    }

    size_t i = insert_symtab(ctx, len, newkey);
    ctx->macros.vals[i] = (MacroDef){ { vallen, (const unsigned char*) newvalue } };
}

bool cuikpp_undef_cstr(Cuik_CPP* ctx, const char* key) {
    return cuikpp_undef(ctx, strlen(key), key);
}

bool cuikpp_undef(Cuik_CPP* ctx, size_t keylen, const char* key) {
    uint32_t mask = (1u << ctx->macros.exp) - 1;
    uint32_t hash = hash_ident(key, keylen);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - ctx->macros.exp)) | 1;
        i = (i + step) & mask;

        String* k = &ctx->macros.keys[i];
        if (k->length == MACRO_DEF_TOMBSTONE) {
            continue;
        } else if (k->length == 0) {
            break;
        } else if (keylen == k->length && memcmp(key, k->data, keylen) == 0) {
            ctx->macros.len--;
            ctx->macros.keys[i] = (String){ MACRO_DEF_TOMBSTONE, 0 };
            return true;
        }
    }

    return false;
}

// murmur3 32-bit without UB unaligned accesses
// https://github.com/demetri/scribbles/blob/master/hashing/ub_aware_hash_functions.c
static uint32_t hash_ident(const void* key, size_t len) {
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

// 16byte based compare
// it doesn't need to be aligned but the valid range must be (len + 15) & ~15
static bool memory_equals16(const unsigned char* src1, const unsigned char* src2, size_t length) {
    #if !USE_INTRIN
    return memcmp(src1, src2, length) == 0;
    #else
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
    #endif
}

static bool find_define(Cuik_CPP* restrict ctx, size_t* out_index, const unsigned char* start, size_t length) {
    #if CUIK__CPP_STATS
    uint64_t start_ns = cuik_time_in_nanos();
    #endif

    bool found = false;
    uint32_t mask = (1u << ctx->macros.exp) - 1;
    uint32_t hash = hash_ident(start, length);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - ctx->macros.exp)) | 1;
        i = (i + step) & mask;

        String* k = &ctx->macros.keys[i];
        if (k->length == 0) {
            break;
        } else if (length == k->length && memcmp(start, k->data, length) == 0) {
            *out_index = i;
            found = true;
            break;
        }
    }

    #if CUIK__CPP_STATS
    uint64_t end_ns = cuik_time_in_nanos();
    c->total_define_access_time += (end_ns - start_ns);
    c->total_define_accesses += 1;
    #endif
    return found;
}

bool cuikpp_find_define_cstr(Cuik_CPP* restrict ctx, Cuik_DefineIter* out_ref, const char* key) {
    size_t def_i;
    if (!find_define(ctx, &def_i, (const unsigned char*) key, strlen(key))) {
        return false;
    }

    out_ref->loc = ctx->macros.vals[def_i].loc;
    out_ref->key = ctx->macros.keys[def_i];
    out_ref->value = ctx->macros.vals[def_i].value;
    return true;
}

bool cuikpp_find_define(Cuik_CPP* restrict ctx, Cuik_DefineIter* out_ref, size_t keylen, const char key[]) {
    size_t def_i;
    if (!find_define(ctx, &def_i, (const unsigned char*) key, keylen)) {
        return false;
    }

    out_ref->loc = ctx->macros.vals[def_i].loc;
    out_ref->key = ctx->macros.keys[def_i];
    out_ref->value = ctx->macros.vals[def_i].value;
    return true;
}

static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length) {
    size_t garbage;
    return find_define(c, &garbage, start, length);
}

static size_t hide_macro(Cuik_CPP* restrict c, size_t def_index) {
    size_t saved = c->macros.keys[def_index].length;
    c->macros.keys[def_index].length = MACRO_DEF_TOMBSTONE;
    return saved;
}

static void unhide_macro(Cuik_CPP* restrict c, size_t def_index, size_t saved) {
    c->macros.keys[def_index].length = saved;
}
