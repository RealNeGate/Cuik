
CUIK_API void cuikpp_add_include_directory(Cuik_CPP* ctx, const char dir[]) {
    dyn_array_put(ctx->system_include_dirs, strdup(dir));
}

CUIK_API Cuik_FileIter cuikpp_first_file(Cuik_CPP* ctx) {
    return (Cuik_FileIter){ .file = &ctx->files[0] };
}

CUIK_API bool cuikpp_next_file(Cuik_CPP* ctx, Cuik_FileIter* it) {
    if (it->i >= dyn_array_length(ctx->files)) {
        return false;
    }

    it->file = &ctx->files[it->i];
    it->i += 1;
    return true;
}

CUIK_API Cuik_IncludeIter cuikpp_first_include_search(Cuik_CPP* ctx) {
    return (Cuik_IncludeIter){ .directory = ctx->system_include_dirs[0] };
}

CUIK_API bool cuikpp_next_include_search(Cuik_CPP* ctx, Cuik_IncludeIter* it) {
    if (it->i >= dyn_array_length(ctx->system_include_dirs)) {
        return false;
    }

    it->directory = ctx->system_include_dirs[it->i];
    it->i += 1;
    return true;
}

CUIK_API Cuik_DefineIter cuikpp_first_define(Cuik_CPP* ctx) {
    for (int i = 0; i < MACRO_BUCKET_COUNT; i++) {
        if (ctx->macro_bucket_count[i] != 0) {
            // first slot in that non-empty bucket
            return (Cuik_DefineIter){ .bucket = i, .id = 0 };
        }
    }

    return (Cuik_DefineIter){ .bucket = MACRO_BUCKET_COUNT, .id = 0 };
}

CUIK_API bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* it) {
    // we outta bounds
    if (it->bucket >= MACRO_BUCKET_COUNT) return false;

    // find next bucket
    it->id += 1;
    while (it->id >= ctx->macro_bucket_count[it->bucket]) {
        it->id = 0;
        it->bucket += 1;

        if (it->bucket >= MACRO_BUCKET_COUNT) {
            return false;
        }
    }

    size_t e = (it->bucket * SLOTS_PER_MACRO_BUCKET) + it->id;

    size_t keylen = ctx->macro_bucket_keys_length[e];
    const char* key = (const char*)ctx->macro_bucket_keys[e];

    size_t vallen = ctx->macro_bucket_values_end[e] - ctx->macro_bucket_values_start[e];
    const char* val = (const char*)ctx->macro_bucket_values_start[e];

    it->loc = ctx->macro_bucket_source_locs[e];
    it->key = (struct Cuik_DefineKey){ keylen, key };
    it->value = (struct Cuik_DefineVal){ vallen, val };
    return true;
}
