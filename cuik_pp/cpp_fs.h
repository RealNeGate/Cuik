#include <log.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

typedef struct InternalFile InternalFile;
struct InternalFile {
    InternalFile* next;
    const char* name;
    size_t size;
    char data[];
};

extern InternalFile* cuik__ifiles_root;

InternalFile* find_internal_file(const char* name) {
    InternalFile* f = cuik__ifiles_root;
    for (; f != NULL; f = f->next) {
        if (strcmp(name, f->name) == 0) return f;
    }

    return NULL;
}

bool cuikpp_locate_file(void* user_data, const Cuik_Path* restrict input, Cuik_Path* output, bool case_insensitive) {
    if (cuik_path_is_in(input, "$cuik")) {
        InternalFile* f = find_internal_file(input->data + sizeof("$cuik"));
        if (f) {
            *output = *input;
            return true;
        } else {
            return false;
        }
    } else {
        cuikfs_canonicalize(output, input->data, case_insensitive);
        return cuikfs_exists(output->data);
    }
}

bool cuikpp_default_fs(void* user_data, const Cuik_Path* restrict input, Cuik_FileResult* output, bool case_insensitive) {
    if (input->length == 0) {
        if (user_data == NULL) return false;
        String source = *(String*) user_data;

        // allocate proper buffer for source
        char* buffer = cuik__valloc(source.length + 17);
        memcpy(buffer, source.data, source.length);

        cuiklex_canonicalize(source.length, buffer);

        output->length = source.length;
        output->data = buffer;
        return true;
    } else if (cuik_path_is_in(input, "$cuik")) {
        InternalFile* f = find_internal_file(input->data + sizeof("$cuik"));
        if (f == NULL) return false;

        output->length = f->size;
        output->data = f->data;
        return true;
    } else {
        Cuik_Path path;
        cuikfs_canonicalize(&path, input->data, case_insensitive);

        // read entire file into virtual memory block
        Cuik_File* file = cuikfs_open(path.data, false);
        if (file == NULL) return false;

        size_t length;
        if (!cuikfs_get_length(file, &length)) goto err;

        char* buffer = cuik__valloc(length + 17);
        if (!cuikfs_read(file, buffer, length)) goto err;

        cuiklex_canonicalize(length, buffer);

        output->length = length;
        output->data = buffer;
        cuikfs_close(file);
        return true;

        err:
        cuikfs_close(file);
        return false;
    }
}

void cuiklex_canonicalize(size_t length, char* data) {
    uint8_t* text = (uint8_t*) data;

    #if USE_INTRIN && CUIK__IS_X64
    size_t simd_end = length;
    length = length & 15;
    #endif

    for (size_t i = 0; i < length; i++) {
        if (text[i] == '\t') text[i] = ' ';
        if (text[i] == '\v') text[i] = ' ';
        if (text[i] == 12)   text[i] = ' ';
    }

    #if USE_INTRIN && CUIK__IS_X64
    // NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
    // ARM variants and such but yea.
    // log_debug("SIMD starts at %zu (ends at %zu) such that the iterations are a multiple of 16", length, simd_end);
    for (size_t i = length; i < simd_end; i += 16) {
        __m128i bytes = _mm_loadu_si128((__m128i*)&text[i]);

        // Replace all \t and \v with spaces
        __m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\v')));
        test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8(12)));

        bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
        _mm_storeu_si128((__m128i*)&text[i], bytes);
    }
    #endif
}
