
/*int foo(int* x) {
    return *x > 4 && *x < 10;
}

int bar(int* x, int* y) {
    return *x <= 256 || *y >= 4;
}

int bar2(int x, int y) {
    return x <= 256 || y >= 4;
}

extern void baz(void);
int bar3(const char* regex) {
    while(*regex && *regex != ')') {
        baz();
        regex++;
    }

    return 0;
}*/

#include <stdint.h>

typedef struct {
    uint64_t raw;
} Ref;

typedef struct {
    int tag;
    int x, y;
} Obj;

static uint64_t global_nmt;

void mark(uint64_t r);
void obj_set_x(Ref r) {
    // load barrier
    uint64_t addr = r.raw ^ global_nmt;
    if ((addr >> 63ull) & 1) {
        mark(addr);
    }
    ((Obj*) addr)->x = 16;
}

