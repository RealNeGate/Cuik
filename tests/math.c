
#include <stdint.h>
#include <stdio.h>

// Newtons approximation to the square root
static float square_root(float x) {
    float ep = 1e-8f;
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
        /*int64_t x = i;
        printf("%lld %lld\n", x, foo(x));
        printf("%llu\n", (1ull << 63ull) + foo(i));*/
        printf("sqrt(%f) = %f\n", (float) i, square_root(i));
    }
}

int64_t foo(int64_t x) {
    return x / -7;
    // return x / ((1ull << 63ull) + 1ull);
}

