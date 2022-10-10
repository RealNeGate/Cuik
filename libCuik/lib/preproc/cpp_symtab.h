
CUIK_API void cuikpp_define_empty_cstr(Cuik_CPP* ctx, const char* key) {
    cuikpp_define_empty(ctx, strlen(key), key);
}

CUIK_API void cuikpp_define_cstr(Cuik_CPP* ctx, const char* key, const char* val) {
    cuikpp_define(ctx, strlen(key), key, strlen(val), val);
}

CUIK_API void cuikpp_define_empty(Cuik_CPP* ctx, size_t keylen, const char* key) {
    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (keylen + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    keylen = *paren == '(' ? paren - newkey : keylen;

    uint64_t slot = hash_ident((const unsigned char*)newkey, keylen);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = keylen;

    ctx->macro_bucket_values_start[e] = NULL;
    ctx->macro_bucket_values_end[e] = NULL;
    ctx->macro_bucket_source_locs[e] = (SourceLoc){ 0 };
}

CUIK_API void cuikpp_define(Cuik_CPP* ctx, size_t keylen, const char* key, size_t vallen, const char* value) {
    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (keylen + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, keylen);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < keylen && *paren != '(') paren++;
    size_t len = *paren == '(' ? paren - newkey : keylen;

    uint64_t slot = hash_ident((const unsigned char*)newkey, len);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = len;

    {
        size_t pad_len = (vallen + 15) & ~15;
        char* newvalue = gimme_the_shtuffs(ctx, pad_len);
        memcpy(newvalue, value, vallen);

        size_t rem = pad_len - vallen;
        memset(newvalue + vallen, 0, rem);

        ctx->macro_bucket_values_start[e] = (const unsigned char*)newvalue;
        ctx->macro_bucket_values_end[e] = (const unsigned char*)newvalue + vallen;
        ctx->macro_bucket_source_locs[e] = (SourceLoc){ 0 };
    }
}

CUIK_API bool cuikpp_undef_cstr(Cuik_CPP* ctx, const char* key) {
    return cuikpp_undef(ctx, strlen(key), key);
}

CUIK_API bool cuikpp_undef(Cuik_CPP* ctx, size_t keylen, const char* key) {
    // Hash name
    uint64_t slot = hash_ident(key, keylen);
    size_t base = slot * SLOTS_PER_MACRO_BUCKET;
    size_t count = ctx->macro_bucket_count[slot];

    // TODO(NeGate): We might wanna invest into a faster data structure.
    for (size_t i = 0; i < count; i++) {
        size_t e = base + i;

        if (ctx->macro_bucket_keys_length[e] == keylen && memcmp(ctx->macro_bucket_keys[e], key, keylen) == 0) {
            // remove swap
            size_t last = base + (count - 1);

            if (i != last) {
                ctx->macro_bucket_keys_length[e] = ctx->macro_bucket_keys_length[last];
                ctx->macro_bucket_keys[e] = ctx->macro_bucket_keys[last];
                ctx->macro_bucket_values_start[e] = ctx->macro_bucket_values_start[last];
                ctx->macro_bucket_values_end[e] = ctx->macro_bucket_values_end[last];
                ctx->macro_bucket_source_locs[e] = ctx->macro_bucket_source_locs[last];
            }

            ctx->macro_bucket_count[slot] -= 1;
            return true;
        }
    }

    return false;
}

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
    uint8_t *tail = ((uint8_t*) key) + 4*(len/4);
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
    return (h ^ (h >> 16)) % MACRO_BUCKET_COUNT;
}

static char unsigned overhang_mask[32] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
};

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

    size_t overhang = length % 16;
    __m128i mask = _mm_loadu_si128((__m128i*)(overhang_mask + 16 - overhang));

    __m128i in1 = _mm_and_si128(_mm_loadu_si128((__m128i*)&src1[i]), mask);
    __m128i in2 = _mm_and_si128(_mm_loadu_si128((__m128i*)&src2[i]), mask);

    int compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
    return compare == 0xFFFF;
    #endif
}

static bool find_define(Cuik_CPP* restrict c, size_t* out_index, const unsigned char* start, size_t length) {
    #if CUIK__CPP_STATS
    uint64_t start_ns = cuik_time_in_nanos();
    #endif

    uint64_t slot = hash_ident(start, length);
    size_t count = c->macro_bucket_count[slot];
    size_t base = (slot * SLOTS_PER_MACRO_BUCKET);

    size_t i = 0;
    bool found = false;
    while (i < count) {
        size_t e = base + i;

        if (c->macro_bucket_keys_length[e] == length) {
            if (memory_equals16(c->macro_bucket_keys[e], start, length)) {
                *out_index = e;
                found = true;
                break;
            }
        }

        i++;
    }

    #if CUIK__CPP_STATS
    uint64_t end_ns = cuik_time_in_nanos();
    c->total_define_access_time += (end_ns - start_ns);
    c->total_define_accesses += 1;
    #endif
    return found;
}

static bool is_defined(Cuik_CPP* restrict c, const unsigned char* start, size_t length) {
    size_t garbage;
    return find_define(c, &garbage, start, length);
}

static size_t hide_macro(Cuik_CPP* restrict c, size_t def_index) {
    size_t saved = c->macro_bucket_keys_length[def_index];
    c->macro_bucket_keys_length[def_index] = 0;
    return saved;
}

static void unhide_macro(Cuik_CPP* restrict c, size_t def_index, size_t saved) {
    c->macro_bucket_keys_length[def_index] = saved;
}
