
#include <stdint.h>
#include <math.h>
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

#if 0
#define HALF_PI 1.57079633f
#define PI      3.14159265f
static float sin_approx(float x) {
    bool flip = fmodf(floorf(x/PI - 0.5f), 2.0f) > 0.0f;

    // wrap around -pi/2 to pi/2
    x = fmodf(x + HALF_PI, PI) - HALF_PI;

    // she taylor my expansion
    float y = x - (x*x*x)/6.0f + (x*x*x*x*x)/120.0f;
    return flip ? -y : y;
}
#endif

static int foo(int x) {
    int y = x + 1;
    int z = x + 1;
    int w = z + 2;
    return w - z;
}

extern int n;
extern float a[];
extern float b[];

int main() {
    #if 1
    for (int i = 0; i < n; i++) {
        a[i] = 2.0f*a[i] + b[i];
    }

    /* for (float i = 0.0f; i < 10.0f; i++) {
        if (!(i == i)) {
            printf("ERROR!!!\n");
        }

        printf("Foo: %d\n", (int) i);
    }*/
    #elif 0
    int x = 1, y = 1;
    while (pred()) {
        if (x != y) {
            y += 1;
        }

        printf("foo %d %d\n", x, y);

        x = 2 - x;
        y = 2 - y;
    }
    #else
    int i = 0;
    int x = 1;         // x is the constant 1
    int z = read();    // z is defined but unknown
    int y = z;         // y is congruent to z here
    while (i++ <=100) {// some unknown predicate
        if (y != z) {  // if we know y and z are congruent
            x = 2;     // then we do not destroy the constant at x
        }
        x = 2 - x;     // destroy easy congruence between x and 1
        if (!(x & 1)) {// if we know that in all cases x has the first bit set
            y = 2;     // then we do not destroy the y-z congruence
        }
    }
    printf("x is %d (%d)\n", x, i); // x is always 1 here
    #endif
}
