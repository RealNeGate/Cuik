#include <stdint.h>
#include <stdlib.h>

extern void fn_0();
extern void fn_1();
extern void fn_2();
extern void fn_3();

void dispatch(unsigned state) {
    switch (state) {
        case 0: fn_0(); return;
        case 1: fn_1(); return;
        case 2: fn_2(); return;
        case 3: fn_3(); return;
        default: abort();
    }
}
