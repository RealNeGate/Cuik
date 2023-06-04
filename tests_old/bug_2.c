
typedef struct {
    void* foo;
} MY_STUFF;

void* foo(MY_STUFF a) {
    return a.foo;
}
