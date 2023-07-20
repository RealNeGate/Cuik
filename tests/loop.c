#include <stddef.h>
#include <stdint.h>

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
    int sum = 0;
    for (size_t i = 0; i < n; i++) {
        if (i >= n) return -1; // bounds check
        sum += arr[i];
    }
    return sum;
}

int foo(int x) { return 1 + x; }

uint64_t sfc64(uint64_t s[4]) {
    uint64_t r = s[0] + s[1] + s[3]++;
    s[0] = (s[1] >> 11) ^ s[1];
    s[1] = (s[2] << 3) + s[2];
    s[2] = r + (s[2] << 24 | s[2] >> 40);
    return r;
}

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

