
int foo(int* a, int i) {
    int tmp = a[i];
    a[i] = a[i+1];
    a[i+1] = tmp;
    return 0;
}
