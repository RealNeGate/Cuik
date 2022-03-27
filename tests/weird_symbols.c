typedef int A;

void Func() {
    A hello;
    enum { A };
    hello = A;

    {
        int A;
        A = hello;
    }
}
