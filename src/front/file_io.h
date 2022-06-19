#include <stdio.h>
#include <sys/stat.h>
#include <x86intrin.h>

#ifdef _WIN32
#include <windows.h>
#endif

static void remove_weird_whitespace(size_t len, char* text);

//static size_t file_io_memory_usage = 0;

#ifdef _WIN32
char* read_entire_file(const char* filepath) {
    HANDLE file = CreateFileA(filepath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file == INVALID_HANDLE_VALUE) {
        panic("error: could not open file '%s'!", filepath);
    }

    LARGE_INTEGER file_size;
    if (!GetFileSizeEx(file, &file_size)) {
        // must be a file stream
        panic("Internal preprocessor error: could not check file size of '%s'!", filepath);
    }

    // normal file with a normal length
    if (file_size.HighPart) {
        panic("Internal preprocessor error: file '%s' is too big!", filepath);
    }

    char* buffer = malloc(file_size.QuadPart + 17);
    DWORD bytes_read;
    if (!ReadFile(file, buffer, file_size.LowPart, &bytes_read, NULL)) {
        panic("Internal preprocessor error: could not read file '%s'!", filepath);
    }

    CloseHandle(file);

    // fat null terminator
    memset(&buffer[file_size.QuadPart], 0, 17);
    remove_weird_whitespace(file_size.QuadPart, buffer);

    //file_io_memory_usage += (file_size.QuadPart + 17);
    //printf("%f MiB of files\n", (double)file_io_memory_usage / 1048576.0);
    return buffer;
}
#else
char* read_entire_file(const char* file_path) {
    ////////////////////////////////
    // Read file
    ////////////////////////////////
    char* text;
    int len;
    {
        FILE* file = fopen(file_path, "rb");
        if (!file) {
            printf("Could not read file: %s\n", file_path);
            abort();
        }

        int descriptor = fileno(file);

        struct stat file_stats;
        if (fstat(descriptor, &file_stats) == -1) {
            fclose(file);
            return NULL;
        }

        len = file_stats.st_size;
        text = malloc(len + 17);

        fseek(file, 0, SEEK_SET);
        size_t length_read = fread(text, 1, len, file);

        // fat null terminator
        length_read = len;
        memset(&text[length_read], 0, 17);
        fclose(file);
    }

    remove_weird_whitespace(len, text);
    return text;
}
#endif

static bool file_exists(const char* filename) {
#ifdef _WIN32
    return GetFileAttributesA(filename) != INVALID_FILE_ATTRIBUTES;
#else
    struct stat buffer;
    return (stat(filename, &buffer) == 0);
#endif
}

static void remove_weird_whitespace(size_t len, char* text) {
#if !USE_INTRIN
    for (size_t i = 0; i < len; i++) {
        if (text[i] == '\t') text[i] = ' ';
        if (text[i] == '\v') text[i] = ' ';
        if (text[i] == 12) text[i] = ' ';
    }
#else
    // NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
    // ARM variants and such but yea.
    for (size_t i = 0; i < len; i += 16) {
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
