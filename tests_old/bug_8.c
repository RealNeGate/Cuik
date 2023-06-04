int foo(int x) {
    return x;
}

void bar(int* a) {
    int h = foo(1);
    *a = 1;
}

int main() {
    int a = 10;
    bar(&a);
    return 0;
}
