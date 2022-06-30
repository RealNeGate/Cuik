
Cuik_File cpp__get_file(void* user_data, bool is_query, const char* path, size_t* out_length) {
    if (is_query) {

    }
}

bool cpp__canonicalize(void* user_data, char output[FILENAME_MAX], const char* input) {
    #ifdef _WIN32
    char* filepart;
    char* new_path = arena_alloc(&thread_arena, FILENAME_MAX, 1);
    if (GetFullPathNameA(path, FILENAME_MAX, new_path, &filepart) == 0) {
        int loc = l.current_line;
        fprintf(stderr, "error %s:%d: Could not resolve path: %s\n", l.filepath, loc, path);
        abort();
    }

    if (filepart == NULL) {
        int loc = l.current_line;
        fprintf(stderr, "error %s:%d: Cannot include directory %s\n", l.filepath, loc, new_path);
        abort();
    }

    // Convert file paths into something more comfortable
    // The windows file paths are case insensitive
    for (char* p = output; *p; p++) {
        if (*p == '\\') {
            *p = '/';
        } else if (*p >= 'A' && *p <= 'Z') {
            *p -= ('A' - 'a');
        }
    }
    #else
    realpath(input, output);
    return true;
    #endif
}

Cuik_IFileSystem cuik_default_fs = {
    .get_file = cpp__get_file,
    .canonicalize = cpp_canonicalize
};

