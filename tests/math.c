
#include <stdint.h>
#include <math.h>
// #include <stdio.h>

/*void bar() {
    for (int i = 0; i < 100; i++) {
        printf("%d\n", i);
    }
}

// Newtons approximation to the square root
static float square_root(float x) {
    float ep = 1e-7f;
    float guess = x;
    for (;;) {
        float next = (x/guess + guess) / 2.0f;
        if (guess-ep <= next & next <= guess+ep) {
            return guess;
        }
        guess = next;
    }
}

int64_t foo(int64_t x);
int main() {
    for (float i = 1.0f; i <= 100.0f; i += 1.0f) {
        // int64_t x = i;
        // printf("%lld %lld\n", x, foo(x));
        // printf("%llu\n", (1ull << 63ull) + foo(i));
        printf("sqrt(%f) = %f\n", i, square_root(i));
    }
}

int64_t foo(int64_t x) {
    return x / -7;
    // return x / ((1ull << 63ull) + 1ull);
}*/

typedef uint32_t stbi__uint32;
typedef unsigned char stbi_uc;

typedef struct
{
    int      (*read)  (void *user,char *data,int size);   // fill 'data' with 'size' bytes.  return number of bytes actually read
    void     (*skip)  (void *user,int n);                 // skip the next 'n' bytes, or 'unget' the last -n bytes if negative
    int      (*eof)   (void *user);                       // returns nonzero if we are at end of file/data
} stbi_io_callbacks;

typedef struct
{
    stbi__uint32 img_x, img_y;
    int img_n, img_out_n;

    stbi_io_callbacks io;
    void *io_user_data;

    int read_from_callbacks;
    int buflen;
    stbi_uc buffer_start[128];
    int callback_already_read;

    stbi_uc *img_buffer, *img_buffer_end;
    stbi_uc *img_buffer_original, *img_buffer_original_end;
} stbi__context;

void stbi__refill_buffer(stbi__context *s);
/*{
    int n = (s->io.read)(s->io_user_data,(char*)s->buffer_start,s->buflen);
    s->callback_already_read += (int) (s->img_buffer - s->img_buffer_original);
    if (n == 0) {
        // at end of file, treat same as if from memory, but need to handle case
        // where s->img_buffer isn't pointing to safe memory, e.g. 0-byte file
        s->read_from_callbacks = 0;
        s->img_buffer = s->buffer_start;
        s->img_buffer_end = s->buffer_start+1;
        *s->img_buffer = 0;
    } else {
        s->img_buffer = s->buffer_start;
        s->img_buffer_end = s->buffer_start + n;
    }
}*/

#if 0
stbi_uc stbi__get8(stbi__context *s);
#else
static stbi_uc stbi__get8(stbi__context *s)
{
    if (s->img_buffer < s->img_buffer_end)
        return *s->img_buffer++;
    if (s->read_from_callbacks) {
        stbi__refill_buffer(s);
        return *s->img_buffer++;
    }
    return 0;
}
#endif

const char *stbi__g_failure_reason;
static int stbi__err(const char *str)
{
    stbi__g_failure_reason = str;
    return 0;
}

int sample(stbi__context *s, int pal_len) {
    stbi_uc palette[1024];
    for (int i=0; i < pal_len; ++i) {
        palette[i*4+3] = 0;

        palette[i*4+0] = stbi__get8(s);
        palette[i*4+1] = stbi__get8(s);
        palette[i*4+2] = stbi__get8(s);
        palette[i*4+3] = 255;
    }

    #ifdef __CUIK__
    __builtin_blackhole(palette);
    return 0;
    #else
    return (long long) &palette;
    #endif
}

extern int pred();
static void expand(int n, char* dst, const char* src) {
    int i = 0;
    for (int j = 0; j < n; j++) {
        dst[j] = src[i];
        if (pred()) {
            i++;
        }
    }
}

extern int read();
extern int printf(const char* fmt, ...);

#if 0
#define HALF_PI 1.57079633f
#define PI      3.14159265f
static float sin_approx(float x) {
    bool flip = fmodf(floorf(x/PI - 0.5f), 2.0f) > 0.0f;

    // wrap around -pi/2 to pi/2
    x = fmodf(x + HALF_PI, PI) - HALF_PI;

    // she taylor my expansion
    float y = x - (x*x*x)/6.0f + (x*x*x*x*x)/120.0f;
    return flip ? -y : y;
}
#endif

static int foo(int x) {
    int y = x + 1;
    int z = x + 1;
    int w = z + 2;
    return w - z;
}

extern int n;
extern float a[];
extern float b[];

static void disabled() {
    #if 0
    bool foo = false;
    for (int i = 0; i < n && !foo; i++) {
        if (a[i] == 1.0f) {
            foo = true;
            break;
        }
    }

    if (foo) {
        printf("Yipee!\n");
    }
    #elif 1
    /* for (int i = 0; i < n; i++) {
        a[i] = 2.0f*a[i] + b[i];
    }

    for (float i = 0.0f; i < 10.0f; i++) {
        if (!(i == i)) {
            printf("Woah!\n");
        }
        printf("Foo: %d\n", (int) i);
    }

    float i = 0.0f;
    while (++i <=100) {
        printf("Foo: %d\n", (int) i);
        i += 2.0f;
    }*/
    #elif 0
    bool x = 1;
    while (pred()) {
        printf("foo %d\n", x);
        x = !x;
    }
    #else
    int i = 0;
    int x = 1;
    int z = read();
    int y = z;
    while (i++ <=100) {
        if (y != z) {
            x = 2;
        }
        x = 2 - x;
        if (!(x & 1)) {
            y = 2;
        }
    }
    printf("x is %d (%d)\n", x, i); // x is always 1 here
    #endif
}
