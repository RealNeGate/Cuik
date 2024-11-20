
void _start() {
    __builtin_syscall(1, 1 /* stdout */, "Hello\n", 6);
    __builtin_syscall(231, 0);
}
