
typedef struct LoadResult {
    bool found;

    size_t length;
    char* data;
} LoadResult;

static LoadResult get_file(const char* path) {
    #ifdef _WIN32
    // actual file reading
    HANDLE file = CreateFileA(path, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file == INVALID_HANDLE_VALUE) {
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
    cuiklex_canonicalize(file_size.QuadPart, buffer);

    return (LoadResult){ .found = true, .length = file_size.QuadPart, buffer };
    #else
    // actual file reading
    FILE* file = fopen(path, "rb");
    if (!file) {
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
    cuiklex_canonicalize(len, text);

    return (LoadResult){ .found = true, .length = len, .data = text };
    #endif
}

// cache is NULLable and if so it won't use it
CUIK_API bool cuikpp_default_packet_handler(Cuik_CPP* ctx, Cuikpp_Packet* packet, Cuik_FileCache* cache) {
    if (packet->tag == CUIKPP_PACKET_GET_FILE) {
        if (packet->file.is_primary) {
            // we don't cache the main file
            LoadResult file = get_file(packet->file.input_path);
            if (file.found) {
                packet->file.tokens = cuiklex_buffer(packet->file.input_path, file.data);

                // the preprocessor is allowed to kill it once it's done
                packet->file.tokens.is_owned = true;
                return true;
            }
        } else {
            TokenStream tokens;
            if (cache && cuik_fscache_lookup(cache, packet->file.input_path, &tokens)) {
                packet->file.tokens = tokens;
                return true;
            } else {
                LoadResult file = get_file(packet->file.input_path);
                if (file.found) {
                    packet->file.tokens = cuiklex_buffer(packet->file.input_path, file.data);

                    if (cache) cuik_fscache_put(cache, packet->file.input_path, &packet->file.tokens);
                    return true;
                }
            }
        }

        return false;
    } else if (packet->tag == CUIKPP_PACKET_QUERY_FILE) {
        if (cache && cuik_fscache_query(cache, packet->file.input_path)) {
            packet->query.found = true;
            return true;
        }

        #ifdef _WIN32
        packet->query.found = (GetFileAttributesA(packet->query.input_path) != INVALID_FILE_ATTRIBUTES);
        #else
        struct stat buffer;
        packet->query.found = (stat(packet->query.input_path, &buffer) == 0);
        #endif

        return true;
    } else if (packet->tag == CUIKPP_PACKET_CANONICALIZE) {
        return cuik_canonicalize_path(packet->canonicalize.output_path, packet->canonicalize.input_path);
    } else {
        return false;
    }
}

CUIK_API bool cuik_canonicalize_path(char output[FILENAME_MAX], const char* input) {
    #ifdef _WIN32
    char* filepart;
    if (GetFullPathNameA(input, FILENAME_MAX, output, &filepart) == 0) {
        return false;
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

    return true;
    #else
    return realpath(input, output) != NULL;
    #endif
}

CUIK_API void cuiklex_canonicalize(size_t length, char* data) {
    uint8_t* text = (uint8_t*) data;

    #if !USE_INTRIN
    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\t') text[i] = ' ';
        if (text[i] == '\v') text[i] = ' ';
        if (text[i] == 12)   text[i] = ' ';
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
