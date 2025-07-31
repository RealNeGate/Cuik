
extern void bar();
void foo(int* arr, int n) {
    bool found = false;
    for (int i = 0; i < n && !found; i++) {
        if (arr[i] == 0) {
            found = true;
        }
    }

    if (found) {
        bar();
    }
}

