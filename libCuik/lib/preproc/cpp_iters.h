#ifdef _WIN32
#define strdup(s) _strdup(s)
#endif

void cuikpp_add_include_directory(Cuik_CPP* ctx, bool is_system, const char dir[]) {
    Cuik_IncludeDir idir = {
        is_system, strdup(dir)
    };

    dyn_array_put(ctx->system_include_dirs, idir);
}

Cuik_File* cuikpp_next_file(Cuik_CPP* ctx, Cuik_File* f) {
    // first element
    if (f == NULL) return &ctx->tokens.files[0];

    size_t len = dyn_array_length(ctx->tokens.files);
    size_t i = f - ctx->tokens.files;
    const char* filename = f->filename;

    // skip any sequential file chunks from the same file
    while (i < len && ctx->tokens.files[i].filename == filename) {
        i += 1;
    }

    return &ctx->tokens.files[i];
}

CUIK_API Cuik_IncludeDir* cuikpp_get_include_dirs(Cuik_CPP* ctx) {
    return &ctx->system_include_dirs[0];
}

CUIK_API size_t cuikpp_get_include_dir_count(Cuik_CPP* ctx) {
    return dyn_array_length(ctx->system_include_dirs);
}

Cuik_DefineIter cuikpp_first_define(Cuik_CPP* ctx) {
    for (int i = 0; i < MACRO_BUCKET_COUNT; i++) {
        if (ctx->macro_bucket_count[i] != 0) {
            // first slot in that non-empty bucket
            return (Cuik_DefineIter){ .bucket = i, .id = 0 };
        }
    }

    return (Cuik_DefineIter){ .bucket = MACRO_BUCKET_COUNT, .id = 0 };
}

bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* it) {
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
    const unsigned char* key = ctx->macro_bucket_keys[e];

    size_t vallen = ctx->macro_bucket_values_end[e] - ctx->macro_bucket_values_start[e];
    const unsigned char* val = ctx->macro_bucket_values_start[e];

    it->loc = ctx->macro_bucket_source_locs[e];
    it->key = (String){ keylen, key };
    it->value = (String){ vallen, val };
    return true;
}
