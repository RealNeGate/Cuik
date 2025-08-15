#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

enum { N = 10 };
static double mean, var;

static double get_time(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    uint64_t t = (uint64_t)ts.tv_sec*1000000000ULL + ts.tv_nsec;
    return t / 1000000000.0;
}

static void perf_test(const char* cmd) {
    static double samples[N];

    mean = 0.0, var = 0.0;
    for (int i = 0; i < N; i++) {
        double x = get_time();
        system(cmd);
        double t = get_time() - x;

        samples[i] = t;
        mean += t;
    }
    mean /= N;

    for (int i = 0; i < N; i++) {
        var += powf(samples[i] - mean, 2);
    }
    var /= N - 1;
}

typedef struct {
    const char* name;
    const char* args;
} TestCase;

static const TestCase cases[] = {
    { "nbody.c", "1000000" }
};
enum { CASE_COUNT = sizeof(cases) / sizeof(cases[0]) };

static char str[1000];
static void comp_test(const char* cmd, const char* name, const char* args) {
    printf("Test:\n");

    snprintf(str, sizeof(str), "%s %s -c", cmd, name);
    perf_test(str);
    printf("  Compile: %.4fs +- %.4f ('%s')\n", mean, var, str);

    snprintf(str, sizeof(str), "%s %s -o %s.exe", cmd, name, name);
    system(str);

    snprintf(str, sizeof(str), "%s.exe %s > NUL 2>&1", name, args);
    perf_test(str);
    printf("  Runtime: %.4fs +- %.4f ('%s')\n", mean, var, str);
}

int main(int argc, char** argv) {
    for (int i = 0; i < CASE_COUNT; i++) {
        comp_test("cuik", cases[i].name, cases[i].args);
        comp_test("clang", cases[i].name, cases[i].args);
        comp_test("cuik -O", cases[i].name, cases[i].args);
        comp_test("clang -O1", cases[i].name, cases[i].args);
    }

    return 0;
}

