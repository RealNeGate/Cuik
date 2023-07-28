#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

void simple(int* a, int* n, int b) {
    a[(*n)++] = b;
}

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

int main() {
    printf("Woah! %d\n", foo(5));
    return 0;
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

int bounds_checks(size_t n, int* arr) {
    int sum;
    memset(&sum, 0, sizeof(sum));

    for (size_t i = 0; i < n; i++) {
        if (i >= n) return -1; // bounds check
        sum += arr[i];
    }
    return sum;
}
#endif

#if 0
uint64_t sfc64(uint64_t s[4]) {
    uint64_t r = s[0] + s[1] + s[3]++;
    s[0] = (s[1] >> 11) ^ s[1];
    s[1] = (s[2] << 3) + s[2];
    s[2] = r + (s[2] << 24 | s[2] >> 40);
    return r;
}
#endif

/*uint32_t murmur3_32(const void* key, size_t len) {
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
}*/

#if 0
int cmp_test(int x) {
    if (x == 5) {
        return 4;
    }

    return 0;
}

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

