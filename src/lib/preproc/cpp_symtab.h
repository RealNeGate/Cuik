
CUIK_API void cuikpp_define_empty(Cuik_CPP* ctx, const char* key) {
    size_t len = strlen(key);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (len + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, len);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < len && *paren != '(') paren++;
    len = *paren == '(' ? paren - newkey : len;

    uint64_t slot = hash_ident((const unsigned char*)newkey, len);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = len;

    ctx->macro_bucket_values_start[e] = NULL;
    ctx->macro_bucket_values_end[e] = NULL;
    ctx->macro_bucket_source_locs[e] = SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0);
}

CUIK_API void cuikpp_define(Cuik_CPP* ctx, const char* key, const char* value) {
    // TODO(NeGate): Fix up this code a bit because i really dislike how the
    // parenthesis are detected
    size_t len = strlen(key);

    // TODO(NeGate): Work around to get any of the macro bucket
    // keys to be at 16bytes aligned
    size_t pad_len = (len + 15) & ~15;
    char* newkey = gimme_the_shtuffs(ctx, pad_len);
    memcpy(newkey, key, len);

    // Hash name, name doesn't include parenthesis part btw
    const char* paren = newkey;
    while ((paren - newkey) < len && *paren != '(') paren++;
    len = *paren == '(' ? paren - newkey : len;

    uint64_t slot = hash_ident((const unsigned char*)newkey, len);
    uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);

    // Insert into buckets
    ctx->macro_bucket_count[slot] += 1;
    ctx->macro_bucket_keys[e] = (const unsigned char*)newkey;
    ctx->macro_bucket_keys_length[e] = len;

    {
        len = strlen(value);

        size_t pad_len = (len + 15) & ~15;
        char* newvalue = gimme_the_shtuffs(ctx, pad_len);
        memcpy(newvalue, value, len);

        size_t rem = pad_len - len;
        memset(newvalue + len, 0, rem);

        ctx->macro_bucket_values_start[e] = (const unsigned char*)newvalue;
        ctx->macro_bucket_values_end[e] = (const unsigned char*)newvalue + len;
        ctx->macro_bucket_source_locs[e] = SOURCE_LOC_SET_TYPE(SOURCE_LOC_UNKNOWN, 0);
    }
}

static char unsigned overhang_mask[32] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
};

static char unsigned default_seed[16] = {
    178, 201, 95, 240, 40, 41, 143, 216,
    2, 209, 178, 114, 232, 4, 176, 188,
};

static uint64_t hash_ident(const unsigned char* at, size_t length) {
#if !USE_INTRIN
    uint32_t hash = 0x811c9dc5;

    for (size_t i = 0; i < length; i++) {
        hash ^= (uint32_t)at[i];
        hash *= 0x01000193; // 32bit magic shit
    }

    return hash % MACRO_BUCKET_COUNT;
#else
    __m128i hash = _mm_cvtsi64_si128(length);
    hash = _mm_xor_si128(hash, _mm_loadu_si128((__m128i*)default_seed));

    size_t chunk_count = length / 16;
    while (chunk_count--) {
        __m128i in = _mm_loadu_si128((__m128i*)at);
        at += 16;

        hash = _mm_xor_si128(hash, in);
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
        hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    }

    size_t overhang = length % 16;
    __m128i in = _mm_loadu_si128((__m128i*)at);

    in = _mm_and_si128(in, _mm_loadu_si128((__m128i*)(overhang_mask + 16 - overhang)));
    hash = _mm_xor_si128(hash, in);
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
    hash = _mm_aesdec_si128(hash, _mm_setzero_si128());

    return ((size_t)_mm_extract_epi32(hash, 0)) % MACRO_BUCKET_COUNT;
#endif
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

    size_t overhang = length % 16;
    __m128i mask = _mm_loadu_si128((__m128i*)(overhang_mask + 16 - overhang));

    __m128i in1 = _mm_and_si128(_mm_loadu_si128((__m128i*)&src1[i]), mask);
    __m128i in2 = _mm_and_si128(_mm_loadu_si128((__m128i*)&src2[i]), mask);

    int compare = _mm_movemask_epi8(_mm_cmpeq_epi8(in1, in2));
    return compare == 0xFFFF;
#endif
}

static bool find_define(Cuik_CPP* restrict c, size_t* out_index, const unsigned char* start, size_t length) {
    uint64_t slot = hash_ident(start, length);
    size_t count = c->macro_bucket_count[slot];
    size_t base = (slot * SLOTS_PER_MACRO_BUCKET);

    size_t i = 0;
    while (i < count) {
        size_t e = base + i;

        if (c->macro_bucket_keys_length[e] == length) {
            if (memory_equals16(c->macro_bucket_keys[e], start, length)) {
                *out_index = e;
                return true;
            }
        }

        i++;
    }

    return false;
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
