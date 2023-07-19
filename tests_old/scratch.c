
typedef struct {
    struct {
        int b;
    } a;
} Foo;

typedef struct {
    char name[8];
} Bar;

int foo(void) {
    Foo f = {
        .a.b = 16
    };

    Bar b = {
        { "hello" }
    };
}

int main(void) {
	return 0;
}
