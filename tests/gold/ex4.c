
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <assert.h>

#if 1
static const char str[] = "Paused my existence sesh to be here";
int main() {
    uint64_t h = xxh_64(str, sizeof(str)-1, 0);
    printf("hash = %"PRIu64"\n", h);
    return 0;
}
#endif

uint64_t xxh_64 (const void *key, int len, uint64_t h) {
    // primes used in mul-rot updates
    uint64_t p1 = 0x9e3779b185ebca87, p2 = 0xc2b2ae3d27d4eb4f,
    p3 = 0x165667b19e3779f9, p4 =0x85ebca77c2b2ae63, p5 = 0x27d4eb2f165667c5;

    // inital 32-byte (4x8) wide hash state
    uint64_t s[4] = {h+p1+p2, h+p2, h, h-p1};

    // bulk work: process all 32 byte blocks
    uint64_t *k32 = (uint64_t*) key;
    for (int i=0; i < (len/32); i+=4) {
        uint64_t b[4] = {k32[i+0], k32[i+1], k32[i+2], k32[i+3]};
        for (int j=0;j<4;j++) b[j] = b[j]*p2+s[j];
        for (int j=0;j<4;j++) s[j] = ((b[j] << 31) | (b[j] >> 33))*p1;
    }

    // mix 32-byte state down to 8-byte state, initalize to value for short keys
    uint64_t s64 = (s[2] + p5);
    if (len > 32) {
        s64  = ((s[0] << 1)  | (s[0] >> 63)) + ((s[1] << 7)  | (s[1] >> 57)) +
        ((s[2] << 12) | (s[2] >> 52)) + ((s[3] << 18) | (s[3] >> 46));
        for (int i=0; i<4;i++) {
            uint64_t ps = (((s[i]*p2) << 31) | ((s[i]*p2) >> 33))*p1;
            s64 = (s64 ^ ps)*p1 + p4;
        }
    }
    s64 += len;

    // up to 31 bytes remain, process 0-3 8 byte blocks
    uint8_t *tail = ((uint8_t *) key) + ((len/32)*32);
    for (int i=0;i < (len & 31) / 8; i++,tail+=8) {
        uint64_t b = (*((uint64_t*) tail))*p2;
        b = (((b << 31)| (b >> 33))*p1) ^ s64;
        s64 = ((b << 27) | (b >> 37))*p1 + p4;
    }

    // up to 7 bytes remain, process 0-1 4 byte block
    for (int i=0;i< (len & 7) / 4; i++, tail +=4) {
        uint64_t b = s64 ^ (*(uint32_t*)tail)*p1;
        s64 = ((b << 23) | (b >> 41))*p2 + p3;
    }

    // up to 3 bytes remain, process 0-3 1 byte blocks
    for (int i=0;i<(len & 3); i++,tail++) {
        uint64_t b = s64 ^ (*tail)*p5;
        s64 = ((b << 11) | (b >> 53))*p1;
    }

    // finalization mix
    s64 =  (s64 ^ (s64 >> 33))*p2;
    s64 =  (s64 ^ (s64 >> 29))*p3;
    return (s64 ^ (s64 >> 32));
}
