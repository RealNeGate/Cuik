#include <stdio.h>
#include <string.h>
#include <math.h>

const char string[] = ".,-~:;=!*#$@";

short foo(short* a, int i) {
    return a[i];
}

int k;
int main() {
    short ggg = 0;

    float A = 0.0f, B = 0, i, j, z[1760];
    char b[1760];

    printf("\x1b[2J");
    for (;;) {
        memset(b, 32, 1760);
        memset(z, 0, 7040);
        for (j = 0; 6.28 > j; j += 0.07) {
            for (i = 0; 6.28 > i; i += 0.02) {
                float c = sin(i);
                float d = cos(j);
                float e = sin(A), f = sin(j), g = cos(A);
                float h = d + 2;
                float D = 1 / (c * h * e + f * g + 5);
                float l = cos(i), m = cos(B), n = sin (B);
                float t = c * h * g - f * e;

                int x = 40 + 30 * D * (l * h * m - t * n);
                int y = 12 + 15 * D * (l * h * n + t * m);
                int o = x + 80 * y;

                int N = 8 * ((f * e - c * d * g) * m - c * d * e - f * g - l * d * n);

                if (22 > y && y > 0 && x > 0 && 80 > x && D > z[o]) {
                    z[o] = D;
                    b[o] = string[N > 0 ? N : 0];
                }
            }
        }

        printf("\x1b[H");
        for (k = 0; 1761 > k; k++) {
            putchar(k % 80 ? b[k] : 10);
        }

        A += 0.04;
        B += 0.02;
    }

    return 0;
}
