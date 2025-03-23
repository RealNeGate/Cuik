#include <stdio.h>
#include <string.h>

const unsigned char *
utf8_simple(const unsigned char *s, long *c)
{
    const unsigned char *next;
    if (s[0] < 0x80) {
        *c = s[0];
        next = s + 1;
    } else if ((s[0] & 0xe0) == 0xc0) {
        *c = ((long)(s[0] & 0x1f) <<  6) |
        ((long)(s[1] & 0x3f) <<  0);
        next = s + 2;
    } else if ((s[0] & 0xf0) == 0xe0) {
        *c = ((long)(s[0] & 0x0f) << 12) |
        ((long)(s[1] & 0x3f) <<  6) |
        ((long)(s[2] & 0x3f) <<  0);
        next = s + 3;
    } else if ((s[0] & 0xf8) == 0xf0 && (s[0] <= 0xf4)) {
        *c = ((long)(s[0] & 0x07) << 18) |
        ((long)(s[1] & 0x3f) << 12) |
        ((long)(s[2] & 0x3f) <<  6) |
        ((long)(s[3] & 0x3f) <<  0);
        next = s + 4;
    } else {
        *c = -1; // invalid
        next = s + 1; // skip this byte
    }
    if (*c >= 0xd800 && *c <= 0xdfff)
        *c = -1; // surrogate half
    return next;
}

int main() {
    const unsigned char* src   = "零番隊 第三官";
    const unsigned char* start = src;
    const unsigned char* end   = src + strlen(src);

    while (src != end) {
        long c;
        src = utf8_simple(src, &c);
        printf("[%zu] %ld\n", src - start, c);
    }

    return 0;
}
