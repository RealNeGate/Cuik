#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef unsigned char stbi_uc;
typedef unsigned short stbi__uint16;
typedef unsigned int stbi__uint32;
typedef short stbi__int16;

// fast-way is faster to check than jpeg huffman, but slow way is slower
#define STBI__ZFAST_BITS  9 // accelerate all cases in default tables
#define STBI__ZFAST_MASK  ((1 << STBI__ZFAST_BITS) - 1)

// zlib-style huffman encoding
// (jpegs packs from left, zlib from right, so can't share code)
typedef struct
{
    stbi__uint16 fast[1 << STBI__ZFAST_BITS];
    stbi__uint16 firstcode[16];
    int maxcode[17];
    stbi__uint16 firstsymbol[16];
    stbi_uc  size[288];
    stbi__uint16 value[288];
} stbi__zhuffman;

typedef struct
{
    stbi_uc *zbuffer, *zbuffer_end;
    int num_bits;
    stbi__uint32 code_buffer;

    char *zout;
    char *zout_start;
    char *zout_end;
    int   z_expandable;

    stbi__zhuffman z_length, z_distance;
} stbi__zbuf;

typedef struct
{
    int      (*read)  (void *user,char *data,int size);   // fill 'data' with 'size' bytes.  return number of bytes actually read
    void     (*skip)  (void *user,int n);                 // skip the next 'n' bytes, or 'unget' the last -n bytes if negative
    int      (*eof)   (void *user);                       // returns nonzero if we are at end of file/data
} stbi_io_callbacks;

typedef struct
{
    stbi__uint32 img_x, img_y;
    int img_n, img_out_n;

    stbi_io_callbacks io;
    void *io_user_data;

    int read_from_callbacks;
    int buflen;
    stbi_uc buffer_start[128];
    int callback_already_read;

    stbi_uc *img_buffer, *img_buffer_end;
    stbi_uc *img_buffer_original, *img_buffer_original_end;
} stbi__context;

/*int callee(int x, int y, int z);

int test(int x, int y) {
    return callee(x, 0, x / y);
}

unsigned int stbi__zreceive(stbi__zbuf *z, int n)
{
    unsigned int k;
    if (z->num_bits < n) stbi__fill_bits(z);
    k = z->code_buffer & ((1 << n) - 1);
    z->code_buffer >>= n;
    z->num_bits -= n;
    return k;
}

stbi_uc stbi__zget8(stbi__zbuf *z);

void stbi__fill_bits(stbi__zbuf *z)
{
    do {
        if (z->code_buffer >= (1U << z->num_bits)) {
            z->zbuffer = z->zbuffer_end;
            return;
        }
        z->code_buffer |= (unsigned int) stbi__zget8(z) << z->num_bits;
        z->num_bits += 8;
    } while (z->num_bits <= 24);
}

void stbi__skip(stbi__context *s, int n)
{
    int blen = (int) (s->img_buffer_end - s->img_buffer);
}*/

uint32_t murmur3_32(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k;
        memcpy(&k, &key[i * 4], sizeof(k));

        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    }

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}

/*int callee(int x, int y, int z);

int test(int x, int y) {
    return callee(x, 0, x / y);
}*/

static int foo() { return 42+1 & 3 * 16; }

int main() {
    // printf("Woah! %d\n", foo());
    printf("Wack! %d\n", murmur3_32("Hello", 5));
    printf("Wack! %d\n", murmur3_32("Why", 3));
    return 0;
}

#if 0
int tiling(int* a, size_t i) {
    return a[i*2] + a[i*2 + 1] + a[i*2 + 2];
}

void simple(int* a, int* n, int b) {
    a[(*n)++] = b;
}

int bounds_checks(size_t n, int* arr) {
    int sum = 0;
    // memset(&sum, 0, sizeof(sum));

    for (size_t i = 0; i < n; i++) {
        // if (i >= n) return -1; // bounds check
        sum += arr[i];
    }
    return sum;
}

// int bar(int x, int y);
// int foo(int x) { return bar(x+1 & 3 * 16, 10); }
#endif

#if 0
typedef struct Cell Cell;
struct Cell {
    Cell* car;
    Cell* cdr;
};

int foo(int n) {
    int arr[2];
    arr[0] = n;
    arr[1] = 1;

    while (arr[0] < 10) {
        arr[1] *= arr[0];
        arr[0]++;
    }

    return arr[1];
}

int select_opt(int a, int b) {
    return a > 10 && b > 10;
}

static void *stbi__malloc(size_t size)
{
    return malloc(size);
}

// return 1 if the sum is valid, 0 on overflow.
// negative terms are considered invalid.
static int stbi__addsizes_valid(int a, int b)
{
    if (b < 0) return 0;
    // now 0 <= b <= INT_MAX, hence also
    // 0 <= INT_MAX - b <= INTMAX.
    // And "a + b <= INT_MAX" (which might overflow) is the
    // same as a <= INT_MAX - b (no overflow)
    return a <= INT_MAX - b;
}

// returns 1 if the product is valid, 0 on overflow.
// negative factors are considered invalid.
static int stbi__mul2sizes_valid(int a, int b)
{
    if (a < 0 || b < 0) return 0;
    if (b == 0) return 1; // mul-by-0 is always safe
    // portable way to check for no overflows in a*b
    return a <= INT_MAX/b;
}

static int stbi__mad2sizes_valid(int a, int b, int add)
{
    return stbi__mul2sizes_valid(a, b) && stbi__addsizes_valid(a*b, add);
}

static int stbi__mad4sizes_valid(int a, int b, int c, int d, int add)
{
    return stbi__mul2sizes_valid(a, b) && stbi__mul2sizes_valid(a*b, c) &&
        stbi__mul2sizes_valid(a*b*c, d) && stbi__addsizes_valid(a*b*c*d, add);
}

void *stbi__malloc_mad4(int a, int b, int c, int d, int add)
{
    if (!stbi__mad4sizes_valid(a, b, c, d, add)) return NULL;
    return stbi__malloc(a*b*c*d + add);
}

int nil_pilled(int* ptr) {
    if (ptr) {
        int x = ptr[0];

        if (ptr != NULL) {
            return x * ptr[1];
        }
    }

    return 0;
}
#endif

#if 0
void dummy(uint64_t a, uint64_t b, uint64_t c, uint64_t d);
uint64_t sfc64(uint64_t a, uint64_t b, uint64_t c, uint64_t d) {
    uint64_t r = a + b + d++;
    a = (b >> 11) ^ b;
    b = (c << 3) + c;
    c = r + (c << 24 | c >> 40);
    dummy(a, b, c, d);
}

int cmp_test(int x, int y) {
    switch (x & y) {
        case 5: return x+y;
        case 10: return x*y;
        default: return x-y;
    }
}
#endif

#if 0
#include <stddef.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

extern void abc(size_t i, size_t limit);

void zero_array(int n, char* arr) {
    // affine: 1x+0 {0,n}
    for (size_t i = 0; i < n; i++) {
        arr[i] = 0;
    }
}

int sigma(void* p) {
    return p;
}

int bar(int x) {
    return 0 * ((5 + x) * 4);
}

unsigned int foo(unsigned int x) {
    return x / 3;
}

char* copy_until(char *dst, char *src, char *chars) {
    while (*src && !strchr(chars, *src)) {
        if (*src == '\\') { *dst++ = *src++; }
        *dst++ = *src++;
    }
    *dst = '\0';
    return src;
}

int main() {
    char tmp[50];

    const char* p = "[hello world]";
    p = copy_until(tmp, p, "]");

    printf("%s %s", p, tmp);
}
#endif

#if 0
void print(int n){}
int fibonacci1() {
    int hi = 1;
    int lo = 0;
    while (hi < 10000) {
        int tmp = hi;
        hi = hi + lo;
        lo = tmp;
        print(lo);
    }
    return hi;
}

extern int* ra1();
extern int ra2(int* a, int b, int c, int d, int e);

static int ra3(int a, int b, int c, int d) {
    ra2(ra1(), a, b, c, d);
}

int foo(int x, int y) {
    x + y == (x || y);

    (void) (foo(1, 2), foo(2, 4), 0);
}

int main() {
    printf("%d\n", fibonacci1());
}

int cse_test(int x, int y) {
    return (x*y) + (x*y);
}

int libcalls_test(char* y) {
    int x;
    memcpy(&x, y, sizeof(int));
    return x;
}

int baz(int* arr, int n) {
    int sum = 0;
    for (size_t i = 0; i < n; i++) {
        sum += arr[n - i - 1];
    }

    return sum;
}

int bar(int x) {
    return ((x + 1) * 4) * 0;
}

int arr[] = { 1, 2, 3, 4 };

int main() {
    printf("%d\n", baz(arr, 4));
    return 0;
}
#endif

