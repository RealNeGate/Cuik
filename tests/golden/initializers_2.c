#include <stdlib.h>
#include <stdio.h>

typedef struct {
    int a, b;
} Foo;

int func(int list[], int len) {
    int sum = 0;
    for (int i = 0; i < len; i++) sum += list[i];
    return sum;
}

int main() {
    int a = func((int[]) { 1,3,4,6 }, 4);
    int b = (Foo){ 1, 2 }.a;
    printf("%d %d\n", a, b);
    return 0;
}
