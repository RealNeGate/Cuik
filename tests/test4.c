
void print(int n){}

void fibonacci1() {
    int lo = 0;
    int hi = 1;
    while (hi < 10000) {
        int tmp = hi;
        hi = hi + lo;
        lo = tmp;
        print(lo);
    }
}

int main() {
    return 0;
}
