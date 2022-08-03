// static size_t file_io_memory_usage = 0;

typedef struct LoadResult {
    bool found;

    size_t length;
    char* data;
} LoadResult;

static LoadResult get_file(bool is_query, const char* path) {
    #ifdef _WIN32
    if (is_query) {
        return (LoadResult){ .found = (GetFileAttributesA(path) != INVALID_FILE_ATTRIBUTES) };
    }

    // actual file reading
    HANDLE file = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "error: could not open file '%s'!\n", path);
        return (LoadResult){ .found = false };
    }

    LARGE_INTEGER file_size;
    if (!GetFileSizeEx(file, &file_size)) {
        // must be a file stream
        fprintf(stderr, "error: could not check file size of '%s'!\n", path);
        return (LoadResult){ .found = false };
    }

    // normal file with a normal length
    if (file_size.HighPart) {
        fprintf(stderr, "error: file '%s' is too big!\n", path);
        return (LoadResult){ .found = false };
    }

    char* buffer = cuik__valloc((file_size.QuadPart + 16 + 4095) & ~4095);
    DWORD bytes_read;
    if (!ReadFile(file, buffer, file_size.LowPart, &bytes_read, NULL)) {
        fprintf(stderr, "error: could not read file '%s'!\n", path);
        return (LoadResult){ .found = false };
    }

    CloseHandle(file);

    // fat null terminator
    memset(&buffer[file_size.QuadPart], 0, 16);

    cuikpp_canonicalize_text(file_size.QuadPart, buffer);

    //file_io_memory_usage += (file_size.QuadPart + 16);
    //printf("%f MiB of files\n", (double)file_io_memory_usage / 1048576.0);
    return (LoadResult){ .found = true, .length = file_size.QuadPart, buffer };
    #else
    if (is_query) {
        struct stat buffer;
        return (LoadResult){ .found = (stat(path, &buffer) == 0) };
    }

    // actual file reading
    FILE* file = fopen(path, "rb");
    if (!file) {
        fprintf(stderr, "Could not read file: %s\n", path);
        return (LoadResult){ .found = false };
    }

    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        fprintf(stderr, "Could not figure out file size: %s\n", path);
        return (LoadResult){ .found = false };
    }

    size_t len = file_stats.st_size;
    char* text = cuik__valloc((len + 16 + 4095) & ~4095);

    fseek(file, 0, SEEK_SET);
    len = fread(text, 1, len, file);
    fclose(file);

    // fat null terminator
    memset(&text[len], 0, 16);
    cuikpp_canonicalize_text(length_read, text);

    return (LoadResult){ .found = true, .length = len, .data = text };
    #endif
}

CUIK_API bool cuikpp_default_packet_handler(Cuik_CPP* ctx, Cuikpp_Packet* packet) {
    if (packet->tag == CUIKPP_PACKET_GET_FILE || packet->tag == CUIKPP_PACKET_QUERY_FILE) {
        bool is_query = packet->tag == CUIKPP_PACKET_QUERY_FILE;
        LoadResult file = get_file(is_query, packet->file.input_path);

        packet->file.found = file.found;
        packet->file.content_length = file.length;
        packet->file.content = (uint8_t*) file.data;
        return true;
    } else if (packet->tag == CUIKPP_PACKET_CANONICALIZE) {
        #ifdef _WIN32
        char* filepart;
        if (GetFullPathNameA(packet->canonicalize.input_path, FILENAME_MAX, packet->canonicalize.output_path, &filepart) == 0) {
            return false;
        }

        // Convert file paths into something more comfortable
        // The windows file paths are case insensitive
        for (char* p = packet->canonicalize.output_path; *p; p++) {
            if (*p == '\\') {
                *p = '/';
            } else if (*p >= 'A' && *p <= 'Z') {
                *p -= ('A' - 'a');
            }
        }

        return true;
        #else
        return realpath(packet->canonicalize.input_path, packet->canonicalize.output_path) != NULL;
        #endif
    } else {
        return false;
    }
}

CUIK_API void cuikpp_free_default_loaded_file(Cuik_FileEntry* file) {
    cuik__vfree(file->content, (file->content_len + 16 + 4095) & ~4095);
}

CUIK_API void cuikpp_canonicalize_text(size_t length, char* data) {
    uint8_t* text = (uint8_t*) data;

    #if !USE_INTRIN
    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\t') text[i] = ' ';
        if (text[i] == '\v') text[i] = ' ';
        if (text[i] == 12) text[i] = ' ';
    }
    #else
    length = (length + 15ull) & ~15ull;

    // NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
    // ARM variants and such but yea.
    for (size_t i = 0; i < length; i += 16) {
        __m128i bytes = _mm_load_si128((__m128i*)&text[i]);

        // Replace all \t and \v with spaces
        __m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\v')));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8(12)));

        bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
        _mm_store_si128((__m128i*)&text[i], bytes);
    }
    #endif
}
