#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>

// U+0000   U+007F   0yyyzzzz
// U+0080   U+07FF   110xxxyy 10yyzzzz
// U+0800   U+FFFF   1110wwww 10xxxxyy 10yyzzzz
// U+010000 U+10FFFF 11110uvv 10vvwwww 10xxxxyy 10yyzzzz
//
// UTF-8 validator:
//
//   0 -> 4 [any]
//   4 -> 4 [any]
//
//   0 -> 0 [0,   126]
//   0 -> 1 [192, 223]
//   0 -> 2 [224, 239]
//   0 -> 3 [240, 247]
//
//   3 -> 2 [127, 191]
//   2 -> 1 [127, 191]
//   1 -> 0 [127, 191]
//
uint64_t table[256];
static uint64_t run(const uint8_t *start, const uint8_t *end, uint64_t state) {
    for (const uint8_t *s = start; s != end; s++) {
        uint64_t row = table[*s];
        state = row >> (state & 63);
    }
    return state & 63;
}

static void dfa_range(int state, int min, int max, int next) {
    printf("dfa_range(%d, %d, %d, %d)\n", state, min, max, next);
    for (int i = min; i <= max; i++) {
        // erase old entry
        table[i] &= ~(0b111111 << (state*6));
        table[i] |= (next*6) << (state*6);
    }
}

static void dfa_dump(void) {
    printf("===\n");
    for (int i = 0; i < 256; i++) {
        printf("%d %016"PRIx64"\n", i, table[i]);
    }
}

int main() {
    dfa_range(0, 0,   255, 4);
    dfa_range(4, 0,   255, 4);
    dfa_dump();
    dfa_range(0, 0b00000000, 0b01111111, 0);
    dfa_dump();
    dfa_range(0, 0b11000000, 0b11011111, 1);
    dfa_dump();
    dfa_range(0, 0b11100000, 0b11101111, 2);
    dfa_dump();
    dfa_range(0, 0b11110000, 0b11110111, 2);
    dfa_dump();

    for (int i = 1; i < 4; i++) {
        // error? stick it
        dfa_range(i, 0, 255, 4);

        // step down
        dfa_range(i, 0b10000000, 0b10111111, i-1);
    }

    dfa_dump();

    FILE* fp = fopen("UTF-8-demo.txt", "rb");
    uint8_t* str = malloc(16384);
    int len = fread(str, 1, 16384, fp);
    fclose(fp);

    printf("validating %d bytes...\n", len);

    int final = run(str, str + len, 0);

    printf("final: %d\n", final);
    return 0;
}

static int count_newlines(int* arr, int n) {
    int c = 0;
    #if 1
    for (int i = 0; i < n; i++) {
        c += arr[i] == '\n';
    }
    #else
    int i = 0;

    // pre-loop with all the misaligned counts and bases
    while (i < (n & 3)) {
        c += arr[i++] == '\n';
    }

    while (i < n) {
        c += arr[i++] == '\n';
        c += arr[i++] == '\n';
        c += arr[i++] == '\n';
        c += arr[i++] == '\n';
    }
    #endif

    return c;
}

/*static int exists(int key) {
    switch (key) {
        case '\n': return false;
        case 'a': return true;
        case 'b': return true;
        case 'c': return true;
        case 'd': return true;
        case 'z': return true;
        case '{': return false;
        case '|': return false;
        case '}': return false;
        case '~': return false;
    }
}*/

