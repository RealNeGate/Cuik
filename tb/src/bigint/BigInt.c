#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>

#include "BigInt.h"

#define MaxBigIntWords 2
#define MAX_VAL UINT64_MAX

/* Bad macros */
#define MIN(A,B) (((A)<(B))?(A):(B))
#define MAX(A,B) (((A)>(B))?(A):(B))

/* Functions for shifting number in-place. */
static void _lshift_one_bit(size_t NumWords, BigInt_t * A);
static void _rshift_one_bit(size_t NumWords, BigInt_t * A);
static void _lshift_word(size_t NumWords, BigInt_t * A, int nwords);
static void _rshift_word(size_t NumWords, BigInt_t * A, int nwords);

/* Public / Exported functions. */
void BigInt_zero(size_t NumWords, BigInt_t * BigInt)
{
    for (size_t i = 0; i < NumWords; ++i) {
        BigInt[i] = 0;
    }
}

void BigInt_from_int(size_t NumWords, BigInt_t * BigInt, size_t SrcNumWords, BigInt_t * src)
{
    BigInt_zero(NumWords, BigInt);

    for (size_t i = 0; i < SrcNumWords; ++i)
        BigInt[i] = src[i];
}

int BigInt_to_int(size_t NumWords, BigInt_t * BigInt)
{
    int ret = 0;

    /* Endianness issue if machine is not little-endian? */
    #if (BigIntWordSize == 1)
    ret += BigInt[0];
    ret += BigInt[1] << 8;
    ret += BigInt[2] << 16;
    ret += BigInt[3] << 24;
    #elif (BigIntWordSize == 2)
    ret += BigInt[0];
    ret += BigInt[1] << 16;
    #elif (BigIntWordSize == 4)
    ret += BigInt[0];
    #elif (BigIntWordSize == 8)
    ret += BigInt[0];
    #endif

    return ret;
}

size_t BigInt_truncate(size_t NumWords, BigInt_t * BigInt)
{
    --NumWords;
    while (BigInt[MaxBigIntWords] == 0 && NumWords > 0) --NumWords;
    return ++NumWords;
}

static BigInt_t hex_to_word(char * Text, int Length)
{
    BigInt_t word = 0;
    for (int i = 0; i < Length; ++i)
    {
        char character = Text[i];
        word <<= 4;
        if (character >= '0' && character <= '9')
            word += character - '0';
        else if (character <= 'F' && character >= 'A')
            word += character - 'A' + 10;
        else if (character <= 'f' && character >= 'a')
            word += character - 'a' + 10;
    }
    return word;
}

void BigInt_from_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str)
{
    BigInt_zero(NumWords, BigInt);
    size_t length = strlen(Str);

    /* whole Words in this string */
    size_t num_words = length / (BigIntWordSize*2);
    if (num_words * (BigIntWordSize*2) < length) ++num_words; /* round up */

    char * string_word = Str + length;

    for (size_t i = 0; i < num_words; ++i)
    {
        /* How many characters should be read from the string */
        size_t hex_length = MIN(BigIntWordSize*2, string_word-Str);
        string_word -= (BigIntWordSize*2);
        BigInt[i] = hex_to_word(string_word, hex_length);
    }
}

void BigInt_to_hex_string(size_t NumWords, BigInt_t * BigInt, char * Str)
{
    NumWords = BigInt_truncate(NumWords, BigInt);

    size_t str_index = 0;

    for (int_fast32_t d = NumWords-1; d >= 0; --d)
    {
        BigInt_t word = BigInt[d];
        for (int BigInt = 0; BigInt < BigIntWordSize*2; ++BigInt) {
            uint8_t nibble = (word >> (BigInt_t)(BigInt*4)) & 0x0F;
            char hexchar = (nibble <= 9) ? '0' + nibble : 'a' + nibble-10;
            Str[str_index+BigIntWordSize*2-1-BigInt] = hexchar;
        }
        str_index += BigIntWordSize*2;
    }

    Str[str_index] = 0;
}

void BigInt_dec(size_t NumWords, BigInt_t * BigInt)
{
    /*BigInt_t tmp; // copy of BigInt
    BigInt_t res;

    for (size_t i = 0; i < NumWords; ++i) {
        tmp = BigInt[i];
        res = tmp - 1;
        BigInt[i] = res;

        if (!(res > tmp)) {
            break;
        }
    }*/
    abort();
}

void BigInt_inc(size_t NumWords, BigInt_t * BigInt)
{
    BigInt_t carry = 0;

    for (size_t i = 0; i < NumWords; ++i) {
        BigInt_t r = BigInt[i] + 1;
        carry = (r > BigInt[i]);
        BigInt[i] = r;

        if (carry == 0) {
            break;
        }
    }
}

int BigInt_bextr(size_t NumWords, BigInt_t * BigInt, int Bit)
{
    size_t ElemIndex, ElemBit;

    ElemIndex = Bit / (BigIntWordSize * CHAR_BIT);
    ElemBit = Bit % (BigIntWordSize * CHAR_BIT);

    return BigInt[ElemIndex] & (1u << ElemBit);
}

void BigInt_add(size_t AWords, BigInt_t * A, size_t BWords, BigInt_t * B, size_t Out_NumWords, BigInt_t * Out)
{
    /* Make it so that A will be smaller than B */
    if (AWords > BWords)
    {
        size_t temp1 = BWords;
        BWords = AWords;
        AWords = temp1;
        BigInt_t * temp2 = B;
        B = A;
        A = temp2;
    }

    int loop_to = 0;
    size_t loop1 = 0;
    size_t loop2 = 0;
    size_t loop3 = 0;

    if (Out_NumWords <= AWords) {
        loop_to = 1;
        loop1 = Out_NumWords;
    }
    else if (Out_NumWords <= BWords) {
        loop_to = 2;
        loop1 = AWords;
        loop2 = Out_NumWords;
    }
    else {
        loop_to = 3;
        loop1 = AWords;
        loop2 = BWords;
        loop3 = Out_NumWords;
    }

    int carry = 0;
    size_t i;

    for (i = 0; i < loop1; ++i)
    {
        BigInt_t r = A[i] + B[i] + carry;
        carry = (r > A[i]);
        Out[i] = r;
    }

    if (loop_to == 1) return;

    for (; i < loop2; ++i)
    {
        BigInt_t r = B[i] + carry;
        carry = (r < B[i]);
        Out[i] = r;
    }

    if (loop_to == 2) return;

    /* Do the carry, then fill the rest with zeros */
    Out[i++] = carry;
    for (; i < loop3; ++i) Out[i] = 0;
}

void BigInt_sub(size_t AWords, BigInt_t * A, size_t BWords, BigInt_t * B, size_t Out_NumWords, BigInt_t * Out)
{
    int loop_to = 0;
    size_t loop1 = 0;
    size_t loop2 = 0;
    size_t loop3 = 0;

    if (Out_NumWords <= MIN(AWords, BWords))
    {
        loop_to = 1;
        loop1 = MIN(AWords, BWords);
    }
    else if (Out_NumWords <= MAX(AWords, BWords))
    {
        loop_to = 2;
        loop1 = MIN(AWords, BWords);
        loop2 = Out_NumWords;
    }
    else {
        loop_to = 3;
        loop1 = AWords;
        loop2 = BWords;
        loop3 = Out_NumWords;
    }

    int borrow = 0;
    size_t i;
    for (i = 0; i < loop1; ++i) {
        BigInt_t res = A[i] - B[i];
        Out[i] = res;
        borrow = res > A[i];
    }

    if (loop_to == 1) return;

    if (AWords > BWords)
    {
        for (; i < loop2; ++i) {
            BigInt_t res = A[i] - borrow;
            Out[i] = res;
            borrow = res > A[i];
        }
    }
    else
    {
        for (; i < loop2; ++i) {
            BigInt_t res = B[i] - borrow;
            Out[i] = res;
            borrow = res > B[i];
        }
    }

    if (loop_to == 2) return;

    for (; i < loop3; ++i) {
        BigInt_t res = (0 - borrow);
        Out[i] = res;
        borrow = (res > borrow);
    }
}

static uint64_t BigInt__mul64x128(uint64_t lhs, uint64_t rhs, uint64_t *high)
{
    /*
     * GCC and Clang usually provide __uint128_t on 64-bit targets,
     * although Clang also defines it on WASM despite having to use
     * builtins for most purposes - including multiplication.
     */
    #if defined(__SIZEOF_INT128__) && !defined(__wasm__)
    __uint128_t product = (__uint128_t)lhs * (__uint128_t)rhs;
    *high = (uint64_t)(product >> 64);
    return (uint64_t)(product & 0xFFFFFFFFFFFFFFFF);

    /* Use the _umul128 intrinsic on MSVC x64 to hint for mulq. */
    #elif defined(_MSC_VER) && defined(_M_IX64)
    #   pragma intrinsic(_umul128)
    /* This intentionally has the same signature. */
    return _umul128(lhs, rhs, high);

    #else
    /*
     * Fast yet simple grade school multiply that avoids
     * 64-bit carries with the properties of multiplying by 11
     * and takes advantage of UMAAL on ARMv6 to only need 4
     * calculations.
     */

    /* First calculate all of the cross products. */
    uint64_t lo_lo = (lhs & 0xFFFFFFFF) * (rhs & 0xFFFFFFFF);
    uint64_t hi_lo = (lhs >> 32)        * (rhs & 0xFFFFFFFF);
    uint64_t lo_hi = (lhs & 0xFFFFFFFF) * (rhs >> 32);
    uint64_t hi_hi = (lhs >> 32)        * (rhs >> 32);

    /* Now add the products together. These will never overflow. */
    uint64_t cross = (lo_lo >> 32) + (hi_lo & 0xFFFFFFFF) + lo_hi;
    uint64_t upper = (hi_lo >> 32) + (cross >> 32)        + hi_hi;

    *high = upper;
    return (cross << 32) | (lo_lo & 0xFFFFFFFF);
    #endif /* portable */
}

static void BigInt__mul128(uint64_t lhs[2], uint64_t rhs[2], uint64_t res[2])
{
    res[0] = BigInt__mul64x128(lhs[0], rhs[0], &res[1]);
    res[1] += (lhs[1] * rhs[0]) + (lhs[0] * rhs[1]);
}

void BigInt_mul_basic(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    assert(NumWords <= MaxBigIntWords);

    BigInt_t row[MaxBigIntWords];
    BigInt_t tmp[MaxBigIntWords];
    size_t i, j;

    BigInt_zero(NumWords, Out);

    for (i = 0; i < NumWords; ++i) {
        BigInt_zero(NumWords, row);

        for (j = 0; j < NumWords; ++j) {
            if (i + j < NumWords) {
                BigInt_zero(NumWords, tmp);

                BigInt_t intermediate[2];
                intermediate[0] = BigInt__mul64x128(A[i], B[j], &intermediate[1]);
                BigInt_from_int(NumWords, tmp, 2, intermediate);

                _lshift_word(NumWords, tmp, i + j);
                BigInt_add(NumWords, tmp, NumWords, row, NumWords, row);
            }
        }
        BigInt_add(NumWords, Out, NumWords, row, NumWords, Out);
    }
}

void BigInt_div(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    assert(NumWords <= MaxBigIntWords);
    BigInt_t current[MaxBigIntWords];
    BigInt_t denom[MaxBigIntWords];
    BigInt_t tmp[MaxBigIntWords];

    BigInt_from_int(NumWords, current, 1, &(BigInt_t){ 1 }); // int current = 1;
    BigInt_copy(NumWords, denom, B); // denom = B
    BigInt_copy(NumWords, tmp, A); // tmp   = A

    BigInt_t half_max = 1 + (MAX_VAL / 2);
    int overflow = 0;
    while (BigInt_cmp(NumWords, denom, A) != LARGER) // while (denom <= A) {
    {
        if (denom[NumWords - 1] >= half_max) {
            overflow = 1;
            break;
        }
        _lshift_one_bit(NumWords, current); //   current <<= 1;
        _lshift_one_bit(NumWords, denom); //   denom <<= 1;
    }
    if (!overflow) {
        _rshift_one_bit(NumWords, denom); // denom >>= 1;
        _rshift_one_bit(NumWords, current); // current >>= 1;
    }
    BigInt_zero(NumWords, Out); // int answer = 0;

    while (!BigInt_is_zero(NumWords, current)) // while (current != 0)
    {
        if (BigInt_cmp(NumWords, tmp, denom) != SMALLER) //   if (dividend >= denom)
        {
            BigInt_sub(NumWords, tmp, NumWords, denom, NumWords, tmp); //     dividend -= denom;
            BigInt_or(NumWords, Out, current, Out); //     answer |= current;
        }
        _rshift_one_bit(NumWords, current); //   current >>= 1;
        _rshift_one_bit(NumWords, denom); //   denom >>= 1;
    }
}

void BigInt_lshift(size_t NumWords, BigInt_t * B, int nbits)
{
    /* Handle shift in multiples of word-size */
    const int nbits_pr_word = (BigIntWordSize * 8);
    int nwords = nbits / nbits_pr_word;
    if (nwords != 0) {
        _lshift_word(NumWords, B, nwords);
        nbits -= (nwords * nbits_pr_word);
    }

    if (nbits != 0) {
        size_t i;
        for (i = (NumWords - 1); i > 0; --i) {
            B[i] = (B[i] << nbits) | (B[i - 1] >> ((8 * BigIntWordSize) - nbits));
        }
        B[i] <<= nbits;
    }
}

void BigInt_rshift(size_t NumWords, BigInt_t * B, int nbits)
{
    /* Handle shift in multiples of word-size */
    const int nbits_pr_word = (BigIntWordSize * 8);
    int nwords = nbits / nbits_pr_word;
    if (nwords != 0) {
        _rshift_word(NumWords, B, nwords);
        nbits -= (nwords * nbits_pr_word);
    }

    if (nbits != 0) {
        size_t i;
        for (i = 0; i < (NumWords - 1); ++i) {
            B[i] = (B[i] >> nbits) | (B[i + 1] << ((8 * BigIntWordSize) - nbits));
        }
        B[i] >>= nbits;
    }
}

void BigInt_mod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    /* Take divmod and throw away div part */
    assert(NumWords <= MaxBigIntWords);
    BigInt_t tmp[MaxBigIntWords];
    BigInt_divmod(NumWords, A, B, tmp, Out);
}

void BigInt_divmod(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * C, BigInt_t * D)
{
    assert(NumWords <= MaxBigIntWords);
    BigInt_t tmp[MaxBigIntWords];

    /* Out = (A / B) */
    BigInt_div(NumWords, A, B, C);

    /* tmp = (Out * B) */
    BigInt_mul_basic(NumWords, C, B, tmp);

    /* Out = A - tmp */
    BigInt_sub(NumWords, A, NumWords, tmp, NumWords, D);
}

void BigInt_and(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] & B[i]);
    }
}

void BigInt_or(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] | B[i]);
    }
}

void BigInt_xor(size_t NumWords, BigInt_t * A, BigInt_t * B, BigInt_t * Out)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Out[i] = (A[i] ^ B[i]);
    }
}

void BigInt_not(size_t NumWords, BigInt_t * A)
{
    for (size_t i = 0; i < NumWords; ++i) {
        A[i] = ~A[i];
    }
}

int BigInt_cmp(size_t NumWords, BigInt_t * A, BigInt_t * B)
{
    size_t i = NumWords;
    do {
        i -= 1; /* Decrement first, to start with last array element */
        if (A[i] > B[i]) {
            return LARGER;
        } else if (A[i] < B[i]) {
            return SMALLER;
        }
    } while (i != 0);

    return EQUAL;
}

int  BigInt_is_small_num(size_t NumWords, BigInt_t * BigInt, BigInt_t Comparand)
{
    if (NumWords == 0 || BigInt[0] != Comparand) return 0;

    for (size_t i = 1; i < NumWords; ++i) {
        if (BigInt[i]) {
            return 0;
        }
    }

    return 1;
}

int BigInt_is_zero(size_t NumWords, BigInt_t * BigInt)
{
    for (size_t i = 0; i < NumWords; ++i) {
        if (BigInt[i]) {
            return 0;
        }
    }

    return 1;
}

void BigInt_copy(size_t NumWords, BigInt_t * Dst, BigInt_t * Src)
{
    for (size_t i = 0; i < NumWords; ++i) {
        Dst[i] = Src[i];
    }
}

void BigInt_copy_dif(size_t DstNumWords, BigInt_t * Dst, size_t SrcNumWords, BigInt_t * Src)
{
    size_t smallest = (DstNumWords < SrcNumWords) ? DstNumWords : SrcNumWords;
    size_t i;
    for (i = 0; i < smallest; ++i) Dst[i] = Src[i];
    for (; i < DstNumWords; ++i) Dst[i] = 0;
}

/* Private / Static functions. */
static void _rshift_word(size_t NumWords, BigInt_t * A, int nwords)
{
    size_t i;
    if (nwords >= NumWords) {
        for (i = 0; i < NumWords; ++i) {
            A[i] = 0;
        }
        return;
    }

    for (i = 0; i < NumWords - nwords; ++i) {
        A[i] = A[i + nwords];
    }
    for (; i < NumWords; ++i) {
        A[i] = 0;
    }
}

static void _lshift_word(size_t NumWords, BigInt_t * A, int nwords)
{
    int_fast32_t i;
    /* Shift whole words */
    for (i = (NumWords - 1); i >= nwords; --i) {
        A[i] = A[i - nwords];
    }
    /* Zero pad shifted words. */
    for (; i >= 0; --i) {
        A[i] = 0;
    }
}

static void _lshift_one_bit(size_t NumWords, BigInt_t * A)
{
    for (size_t i = (NumWords - 1); i > 0; --i) {
        A[i] = (A[i] << 1) | (A[i - 1] >> ((8 * BigIntWordSize) - 1));
    }
    A[0] <<= 1;
}

static void _rshift_one_bit(size_t NumWords, BigInt_t * A)
{
    for (size_t i = 0; i < (NumWords - 1); ++i) {
        A[i] = (A[i] >> 1) | (A[i + 1] << ((8 * BigIntWordSize) - 1));
    }
    A[NumWords - 1] >>= 1;
}
