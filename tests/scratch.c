
typedef struct {
    struct {
        int b;
    } a;
} Foo;

typedef struct {
    char name[8];
} Bar;

int foo() {
    Foo f = {
        .a.b = 16
    };

    Bar b = {
        { "hello" }
    };
}
