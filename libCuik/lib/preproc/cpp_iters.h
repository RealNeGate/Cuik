
CUIK_API void cuikpp_add_include_directory(Cuik_CPP* ctx, bool is_system, const char dir[]) {
    Cuik_IncludeDir d = { is_system, gimme_the_shtuffs(ctx, sizeof(Cuik_Path)) };
    cuik_path_set(d.path, dir);

    char last = d.path->data[d.path->length - 1];
    if (last != '/' && last != '\\') {
        d.path->data[d.path->length++] = CUIK_PATH_SLASH_SEP;
        d.path->data[d.path->length] = 0;
    }

    dyn_array_put(ctx->system_include_dirs, d);
}

CUIK_API void cuikpp_add_include_directoryf(Cuik_CPP* ctx, bool is_system, const char* fmt, ...) {
    Cuik_IncludeDir d = { is_system, gimme_the_shtuffs(ctx, sizeof(Cuik_Path)) };

    va_list ap;
    va_start(ap, fmt);
    d.path->length = vsnprintf(d.path->data, FILENAME_MAX, fmt, ap);
    va_end(ap);

    char last = d.path->data[d.path->length - 1];
    if (last != '/' && last != '\\') {
        d.path->data[d.path->length++] = CUIK_PATH_SLASH_SEP;
        d.path->data[d.path->length] = 0;
    }

    dyn_array_put(ctx->system_include_dirs, d);
}

CUIK_API bool cuikpp_find_include_include(Cuik_CPP* ctx, char output[FILENAME_MAX], const char* path) {
    dyn_array_for(i, ctx->system_include_dirs) {
        sprintf_s(output, FILENAME_MAX, "%s%s", ctx->system_include_dirs[i].path->data, path);
        FILE* f = fopen(output, "r");
        if (f != NULL) {
            fclose(f);
            return true;
        }
    }

    return false;
}

Cuik_FileEntry* cuikpp_next_file(Cuik_CPP* ctx, Cuik_FileEntry* f) {
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
    size_t cap = 1u << ctx->macros.exp;
    for (size_t i = 0; i < cap; i++) {
        if (ctx->macros.keys[i].length != 0 && ctx->macros.keys[i].length != MACRO_DEF_TOMBSTONE) {
            return (Cuik_DefineIter){ .index = i };
        }
    }

    return (Cuik_DefineIter){ .index = 0 };
}

bool cuikpp_next_define(Cuik_CPP* ctx, Cuik_DefineIter* it) {
    size_t cap = 1u << ctx->macros.exp;
    size_t e = it->index;
    if (e >= cap) return false;

    it->loc = ctx->macros.vals[e].loc;
    it->key = ctx->macros.keys[e];
    it->value = ctx->macros.vals[e].value;

    for (size_t i = it->index + 1; i < cap; i++) {
        if (ctx->macros.keys[i].length != 0 && ctx->macros.keys[i].length != MACRO_DEF_TOMBSTONE) {
            it->index = i;
            break;
        }
    }

    it->index = cap;
    return true;
}
