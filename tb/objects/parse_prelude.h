#include <common.h>
#include <prelude.h>

static int find_char(TB_Slice name, char ch) {
    // 8 byte chunks
    uint64_t* name_u64 = (uint64_t*) name.data;

    uint64_t char_mask = (~(uint64_t)0) / 255 * (uint64_t)(ch);
    size_t j = 0;
    for (; j < (name.length/8); j++) {
        uint64_t x = name_u64[j] ^ char_mask;
        x = ((x - 0x0101010101010101ull) & ~x & 0x8080808080808080ull);

        if (x) {
            return j*8 + ((__builtin_ffsll(x) / 8) - 1);
        }
    }

    // final chunk
    if (name.length % 8) {
        uint64_t mask = UINT64_MAX >> (64 - ((name.length % 8)*8));
        uint64_t x = (name_u64[j] & mask) ^ char_mask;
        x = ((x - 0x0101010101010101ull) & ~x & 0x8080808080808080ull);

        if (x) {
            return j*8 + ((__builtin_ffsll(x) / 8) - 1);
        }
    }

    return name.length;
}

static int ideally_fast_strlen(const char* name) {
    // 8 byte chunks
    uint64_t* name_u64 = (uint64_t*) name;
    uint64_t char_mask = (~(uint64_t)0) / 255 * (uint64_t)(0);
    for (size_t j = 0;; j++) {
        uint64_t x = name_u64[j] ^ char_mask;
        x = ((x - 0x0101010101010101ull) & ~x & 0x8080808080808080ull);

        if (x) {
            size_t len = j*8 + ((__builtin_ffsll(x) / 8) - 1);
            assert(len == strlen(name));
            return len;
        }
    }
}

static long long parse_decimal_int(size_t n, const char* str) {
    const char* end = &str[n];

    int result = 0;
    while (str != end) {
        if (*str < '0' || *str > '9') break;

        result *= 10;
        result += *str - '0';
        str++;
    }

    return result;
}
