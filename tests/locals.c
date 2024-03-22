#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <stdio.h>

typedef struct {
    int type : 3;
    int val  : 29;
} Ref;

int bar(void);

#if 0
int main() {
    printf("Hello\n");
    return 0;
}
#elif 0
static uint32_t murmur3_32_piece1(const void* key, size_t len, uint32_t h, size_t i) {
    uint32_t k = ((uint32_t*) key)[i];
    k *= con1();
    k = ((k << 15) | (k >> 17))*0x1b873593;
    return (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
}

static uint32_t con1() { return 0xcc9e2d51; }

static int foo(int* work) {
    int a = work[0];
    int b = work[1];
    if (a > 0) {
        b *= con1();
    }

    return a + b;
}

uint32_t murmur3_32(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        h = murmur3_32_piece1(key, len, h, i);
    }

    // half of murmur
    return h;
}
#elif 0
void foo(int n, int* arr) {
    for (int i = 0; i < n; i++) {
        assert(i < n);
        arr[i] += n;
    }
}
#elif 0
// float c(void) { return 1.0f + 3.0f; }
void foo(int n, int* arr) {
    int a = 16;
    if (n > 0) {
        a = bar();
    }
    arr[0] = a;
}
#elif 1
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
#elif 0
uint32_t foo(uint32_t* x, uint32_t y) {
    return x >> y;
}
#elif 0
uint32_t foo(uint8_t* key, uint32_t h, size_t i) {
    uint32_t k = ((uint32_t*) key)[i];
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    return h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
}
#elif 0
int foo(int* a, int b) {
    int sum = a[b*2+0];
    sum +=    a[b*2+1];
    sum +=    4;
    return sum;
}
#else
int* bar(int* a, int b) {
    return &a[b*2 + 1];
}
#endif

/* void foo(int n, int* arr) {
    arr[0] += 1;
}

void matmul2(float* c, float* a, float* b, int i) {
    for (int j = 0; j < 4; j++) {
        float sum = 0.0f;
        for (int k = 0; k < 4; k++) {
            sum += a[k*4 + i] * b[j*4 + k];
        }
        c[j*4 + i] = sum;
    }
}

void matmul(float* c, float* a, float* b) {
    for (int i = 0; i < 4; i++) {
        matmul2(c, a, b, i);
    }
}*/









#if 0
int ref_eq(Ref a, Ref b) {
    return a.type == b.type && a.val == b.val;
}

void foo(uint64_t* dst) {
    *dst += 5;
}
#elif 0
void foo(int n, int* arr) {
    for (int i = 2; i < n; i++) {
        if (arr[n] != 0) {
            arr[0] += arr[i];
        } else {
            arr[1] += 1;
        }
    }
}
#endif

#if 0
int  stuff_a();
void stuff_b();
void stuff_c();
void stuff_d(int);

int example(void) {
    int i = 10;
    while (stuff_a()) {
        if (i == 10) {
            stuff_b();
        } else {
            stuff_c();
            i += 1;
        }
    }

    return i * 1000 * 1000 * 10;
}

int main() {
    size_t iter = 0;
    size_t stop = 1000 * 1000 * 10;
    size_t step = 1;
    size_t *ptr_iter = &iter;
    size_t *ptr_stop = &stop;
    size_t *ptr_step = &step;
    size_t **ptr_ptr_iter = &ptr_iter;
    size_t **ptr_ptr_stop = &ptr_stop;
    size_t **ptr_ptr_step = &ptr_step;
    size_t ***ptr_ptr_ptr_iter = &ptr_ptr_iter;
    size_t ***ptr_ptr_ptr_stop = &ptr_ptr_stop;
    size_t ***ptr_ptr_ptr_step = &ptr_ptr_step;
    while (***ptr_ptr_ptr_iter < ***ptr_ptr_ptr_stop) {
        ***ptr_ptr_ptr_iter += 1;
    }

    ***ptr_ptr_ptr_iter += 1;
    return ***ptr_ptr_ptr_iter;
}

void foo() {
    int x=1;
    if( false ) {
        while( true ) { x++; }
        stuff_d(x);
    }
}
#endif

