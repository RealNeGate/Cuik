#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

typedef unsigned _ExtInt(128) Num;

/*      UINT64_MAX 18446744073709551615ULL */
#define P10_UINT64 10000000000000000000ULL   /* 19 zeroes */
#define E10_UINT64 19

#define STRINGIZER(x)   # x
#define TO_STRING(x)    STRINGIZER(x)

static int print_num(Num n) {
    int rc;
    if (n > (Num)UINT64_MAX) {
        Num       leading  = n / (Num)P10_UINT64;
        uint64_t  trailing = n % (Num)P10_UINT64;
        rc = print_num(leading);
        rc += printf("%." TO_STRING(E10_UINT64) PRIu64, trailing);
    }
    else
    {
        rc = printf("%" PRIu64, (uint64_t)n);
    }
    return rc;
}

int main() {
    Num a = 1, b = 1;

    printf("[0");
    for (int i = 0; i < 100; i++) {
        printf(",");
        print_num(b);
        
        Num tmp = a;
        a += b;
        b = tmp;
    }
    printf("]");
    return 0;
}
