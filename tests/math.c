
#include <stdint.h>
// #include <stdio.h>

/*void bar() {
    for (int i = 0; i < 100; i++) {
        printf("%d\n", i);
    }
}

// Newtons approximation to the square root
static float square_root(float x) {
    float ep = 1e-7f;
    float guess = x;
    for (;;) {
        float next = (x/guess + guess) / 2.0f;
        if (guess-ep <= next & next <= guess+ep) {
            return guess;
        }
        guess = next;
    }
}

int64_t foo(int64_t x);
int main() {
    for (float i = 1.0f; i <= 100.0f; i += 1.0f) {
        // int64_t x = i;
        // printf("%lld %lld\n", x, foo(x));
        // printf("%llu\n", (1ull << 63ull) + foo(i));
        printf("sqrt(%f) = %f\n", i, square_root(i));
    }
}

int64_t foo(int64_t x) {
    return x / -7;
    // return x / ((1ull << 63ull) + 1ull);
}*/

extern int read();
extern int pred();
extern int printf(const char* fmt, ...);

int main() {
    #if 0
    int x = 1, y = 1;
    while (pred()) {
        x += 1, y += 1;
        printf("foo %d %d\n", x, y);
    }
    #else
    int x = 1;
    // x is the constant 1
    int z = read();
    // z is defined but unknown
    int y = z;
    // y is congruent to z here
    while (pred()) {
        // some unknown predicate
        if( y != z ) {
            // if we know y and z are congruent
            x = 2;
        }
        // then we do not destroy the constant at x
        x = 2 - x;
        // destroy easy congruence between x and 1
        if( x != 1 ) {
            // if we know x is a constant
            y = 2;
        }
        // then we do not destroy the y-z congruence
    }
    printf("x is %d\n", x); // x is always 1 here
    #endif
}
