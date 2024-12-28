#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#if 0
struct s { int x, y, z; };

int f(struct s *b, int *c) {
    int a = 0, d;
    while (d = *c++) {
        if (d > 5)
            a += b[d].y;
        a += b[d].z;
    }
    return a;
}
#endif

void foo(int) { printf("H\n"); }

int cmp(char* x) {
    int n = *x;
    return n > 255;
}

/*enum { NUM_STATES = 10 };
uint8_t table[NUM_STATES][256];

uint8_t normie_dfa(const uint8_t *start, const uint8_t *end, uint8_t state) {
    for (const uint8_t *s = start; s != end; s++) {
        state = table[state][*s];
    }

    return state;
}

short big_nums_gcp(void) {
    return (short)32767 + (short)3;
}

int sccp(int x) {
    int y = x & 63;
    return y < 64 ? 1 << y : 0;
}

int small_mul(char a, char b) {
    return a * b;
}*/

int msvc_crap(int a, int b) {
    int x, y, z;

    if(a > 3) {
        x = 4;
        y = 1;
        z = b & 0xFF00;
    }
    else {
        x = 9;
        y = 2;
        z = b << 8;
    }

    int p = (x * y) * 4;
    int q = z & 0xF;
    return p >= 16 && q == 0;
}

int loop() {
    int a = 1;
    while (a < 10) {
        a = a + 3;
    }
    return a;
}

#if 0
int* tile1(int* x, int i) { return &x[i]; }
int* tile2(int* x, int i) { return &x[i + 2]; }
int  tile3(int* x, int i) { return x[i*4]; }

// const char* info() { return "hello"; }

int loop(int* arr, int n) {
    size_t i = 0;
    int a = 0;
    while (i < n) {
        if (i >= n) return -1;
        a += arr[i];
        i += 1;
    }

    return a;
}

int ctrl(int* x) {
    if (*x) {
        return 0;
    } else {
        *x -= 1;
        return 1;
    }
}

int single_if(int x, int y) {
    if (x && y) {
        return 1;
    }

    return 0;
}

int do_while(int x, int y) {
    if (x < y) {
        do {
            x += 1;
        } while(x < y);
    }

    return x;
}
#endif
