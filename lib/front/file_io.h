#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <x86intrin.h>

static void remove_weird_whitespace(size_t len, uint8_t* text) {
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
