#pragma once

typedef _Vector(float, 4) __m128;
typedef _Vector(double, 2) __m128d;

// actual builtins
float  __builtin_ia32_sqrtss(__m128 a);
__m128 __builtin_ia32_sqrtps(__m128 a);
float  __builtin_ia32_rsqrtss(__m128 a);
__m128 __builtin_ia32_rsqrtps(__m128 a);
__m128 __builtin_ia32_rcpss(__m128 a, __m128 b);
__m128 __builtin_ia32_rcpps(__m128 a, __m128 b);
__m128 __builtin_ia32_minss(__m128 a, __m128 b);
__m128 __builtin_ia32_minps(__m128 a, __m128 b);
__m128 __builtin_ia32_maxss(__m128 a, __m128 b);
__m128 __builtin_ia32_maxps(__m128 a, __m128 b);

static __m128 _mm_add_ss(__m128 a, __m128 b) {
    a[0] += b[0];
    return a;
}

static __m128 _mm_sub_ss(__m128 a, __m128 b) {
    a[0] -= b[0];
    return a;
}

static __m128 _mm_mul_ss(__m128 a, __m128 b) {
    a[0] *= b[0];
    return a;
}

static __m128 _mm_div_ss(__m128 a, __m128 b) {
    a[0] /= b[0];
    return a;
}

static __m128 _mm_add_ps(__m128 a, __m128 b) { return a + b; }
static __m128 _mm_sub_ps(__m128 a, __m128 b) { return a - b; }
static __m128 _mm_mul_ps(__m128 a, __m128 b) { return a * b; }
static __m128 _mm_div_ps(__m128 a, __m128 b) { return a / b; }
static __m128 _mm_and_ps(__m128 a, __m128 b) { return a & b; }
static __m128 _mm_or_ps(__m128 a, __m128 b) { return a | b; }
static __m128 _mm_xor_ps(__m128 a, __m128 b) { return a ^ b; }
static __m128 _mm_andnot_ps(__m128 a, __m128 b) { return ~(a & b); }

static __m128 _mm_sqrt_ss(__m128 a) { return (__m128)__builtin_ia32_sqrtss(a); }
static __m128 _mm_sqrt_ps(__m128 a) { return __builtin_ia32_sqrtps(a); }

static __m128 _mm_rsqrt_ss(__m128 a) { return (__m128)__builtin_ia32_rsqrtss(a); }
static __m128 _mm_rsqrt_ps(__m128 a) { return __builtin_ia32_rsqrtps(a); }

static __m128 _mm_rcp_ss(__m128 a) { return (__m128)__builtin_ia32_rcpss(a); }
static __m128 _mm_rcp_ps(__m128 a) { return __builtin_ia32_rcpps(a); }

static __m128 _mm_min_ss(__m128 a, __m128 b) { return __builtin_ia32_minss(a, b); }
static __m128 _mm_min_ps(__m128 a, __m128 b) { return __builtin_ia32_minps(a, b); }
static __m128 _mm_max_ss(__m128 a, __m128 b) { return __builtin_ia32_maxss(a, b); }
static __m128 _mm_max_ps(__m128 a, __m128 b) { return __builtin_ia32_maxps(a, b); }

static __m128 _mm_setr_ps(float z, float y, float x, float w) { return (__m128){ z, y, x, w }; }
static __m128 _mm_setzero_ps(void) { return (__m128){ 0 }; }
