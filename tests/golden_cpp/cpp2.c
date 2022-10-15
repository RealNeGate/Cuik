// #include <stdio.h>
#define FOO(...) printf(__VA_ARGS__)
#define FOO2(...) FOO("Hello! %s\n", __VA_ARGS__)

extern int printf(const char* fmt, ...);

int main() {
    const char *word = "cat";
    word = "cat2";

    FOO("Hello! %s\n", word);
    // FOO2(word);
    return 0;
}
