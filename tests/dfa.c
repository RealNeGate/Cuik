#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static uint8_t table[256];

static void emit_range(uint64_t min, uint64_t max, uint64_t old, uint64_t next) {
    old *= 2;

    for (size_t i = min; i < max; i++) {
        table[i] |= (next & 3) << old;
    }
}

static void emit_chars(const char* str, uint64_t old, uint64_t next) {
    old *= 2;

    for (; *str; str++) {
        table[(ptrdiff_t) *str] |= (next & 3) << old;
    }
}

void test(const char* str) {
    const char* end = str + strlen(str);

    for (;;) {
        // Skip any whitespace and comments
        // branchless space skip
        str += (*str == ' ');
        if (*str == '\0') {
            // quit, we're done
            return;
        }

        const char* token_start = str;
        bool slow = false;
        uint64_t state = 0;
        for (;;) {
            // table[state][*str]
            uint64_t row = table[(ptrdiff_t) *str];
            uint64_t next_state = (row >> (state * 2)) & 3;
            if (next_state == 0) {
                break;
            }

            if (next_state == 3) slow = true, next_state = 1;

            // move along
            state = next_state;
            str += 1;
        }

        if (state == 0) {
            break;
        }

        printf("'%.*s' %llu (slow? %s)\n", (int)(str - token_start), token_start, state, slow ? "yes" : "no");
    }
}

int main(int argc, char** argv) {
    // [A-Za-z]
    emit_range('a', 'z',   0, 1);
    emit_range('A', 'Z',   0, 1);
    emit_chars("_$\\",     0, 1);
    emit_range(0x80, 0x100,0, 1);

    // [0-9]+
    emit_range('a', 'z',   1, 1);
    emit_range('A', 'Z',   1, 1);
    emit_chars("_$",       1, 1);
    emit_range(0x80, 0x100,1, 1);
    emit_range('0', '9',   1, 1);
    emit_range('0', '9',   1, 1);
    emit_chars("\\",       1, 2);
    emit_chars("uU",       2, 3);

    printf("static uint8_t dfa[256] = {\n");
    for (int i = 0; i < 256; i += 16) {
        printf("    ");
        for (int j = 0; j < 16; j++) {
            printf("0x%02x,", table[i+j]);
        }
        printf("\n");
    }
    printf("}\n");

    test("hello wO$rl__d _b01 aa bb\\u1000bb");
    return 0;
}
