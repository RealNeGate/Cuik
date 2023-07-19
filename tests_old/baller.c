#include <stdio.h>

static const char* find_char(const char* s, int ch) {
    while (*s && *s != ch) s++;
    return *s == ch ? s + 1 : s;
}

#define BALLER(start, end, text) \
    for (const char* start = text, *end; \
    end = find_char(start, ' '), (end-start) > 1; \
    start = find_char(end, '\n'))

int main() {
    const char* text =  "Whose woods these are I think I know.\n"
                        "His house is in the village though;  \n"
                        "He will not see me stopping here     \n"
                        "To watch his woods fill up with snow.\n";

    BALLER(start, end, text) {
        printf("%.*s", (int)(end-start), start, (int)(end-start));
    }
    return 0;
}
