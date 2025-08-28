#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

enum { N = 10 };

typedef struct {
    const char* name;
    const char* args;
} TestCase;

static const TestCase cases[] = {
    { "nbody.c", "10000000" }
};
enum { CASE_COUNT = sizeof(cases) / sizeof(cases[0]) };

static char str[1000];
static void comp_test(const char* cmd1, const char* cmd2, const char* name, const char* args) {
    snprintf(str, sizeof(str), "hyperfine \"%s %s -c\" \"%s %s -c\"", cmd1, name, cmd2, name);
    system(str);

    printf("\n\n");

    snprintf(str, sizeof(str), "%s %s -o test_cuik.exe", cmd1, name);
    system(str);
    snprintf(str, sizeof(str), "%s %s -o test_clang.exe", cmd2, name);
    system(str);

    snprintf(str, sizeof(str), "hyperfine \"test_clang.exe %s\" \"test_cuik.exe %s\"", args, args);
    system(str);
}

int main(int argc, char** argv) {
    for (int i = 0; i < CASE_COUNT; i++) {
        comp_test("cuik -O", "clang -O1", cases[i].name, cases[i].args);
    }

    return 0;
}

