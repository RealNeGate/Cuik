#include <stdint.h>
#include <stddef.h>

/*int bar(void) {
    return 16 + -3;
}*/

/*int baz(int x, int y) {
    return (x ? 16 + x : 8 + x) * y;
}*/

/*int64_t foo() {
    return -((short) 101);
}*/

uint32_t mul_by(const uint32_t *data, size_t len, uint32_t m) {
    uint32_t sum = 0;
    for (uint32_t i = 0; i < len - 1; i++) {
        uint32_t x = data[i], y = data[i + 1];
        sum += x * y * m * i * i;
    }
    return sum;
}

/*int m(int x, int a, int d) {
    int b, c, e;
    if(x > 0) {
        e = 0;
        c = d;
    } else {
        b = 0;
        c = a;
        e = b;
    }
    return e + c;
}*/
