float foo(float a, float b) { return a + b; }

void _start() {
    foo(1.0f, 4.0f);

    __builtin_syscall(1, 1 /* stdout */, "Hello\n", 6);
    __builtin_syscall(231, 0);
}
