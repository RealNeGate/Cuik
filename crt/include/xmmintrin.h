#ifndef _XMMINTRIN_H_
#define _XMMINTRIN_H_

typedef _Vector(float,         4)  __m128;
typedef _Vector(unsigned char, 16) __m128i;

static inline __m128 _mm_add_ss(__m128 __a, __m128 __b) {
  __a[0] += __b[0];
  return __a;
}

static inline __m128 _mm_sub_ss(__m128 __a, __m128 __b) {
  __a[0] -= __b[0];
  return __a;
}

static inline __m128 _mm_mul_ss(__m128 __a, __m128 __b) {
  __a[0] *= __b[0];
  return __a;
}

static inline __m128 _mm_div_ss(__m128 __a, __m128 __b) {
  __a[0] /= __b[0];
  return __a;
}

static inline __m128 _mm_add_ps(__m128 __a, __m128 __b) { return __a + __b; }
static inline __m128 _mm_sub_ps(__m128 __a, __m128 __b) { return __a - __b; }
static inline __m128 _mm_mul_ps(__m128 __a, __m128 __b) { return __a * __b; }
static inline __m128 _mm_div_ps(__m128 __a, __m128 __b) { return __a / __b; }
static inline __m128 _mm_and_ps(__m128 __a, __m128 __b) { return __a & __b; }
static inline __m128 _mm_or_ps(__m128 __a, __m128 __b) { return __a | __b; }
static inline __m128 _mm_xor_ps(__m128 __a, __m128 __b) { return __a ^ __b; }
static inline __m128 _mm_andnot_ps(__m128 __a, __m128 __b) { return ~(__a & __b); }

static inline __m128 _mm_sqrt_ss(__m128 __a) { return (__m128)__builtin_ia32_sqrtss(__a); }
static inline __m128 _mm_sqrt_ps(__m128 __a) { return __builtin_ia32_sqrtps(__a); }

static inline __m128 _mm_rsqrt_ss(__m128 __a) { return (__m128)__builtin_ia32_rsqrtss(__a); }
static inline __m128 _mm_rsqrt_ps(__m128 __a) { return __builtin_ia32_rsqrtps(__a); }

static inline __m128 _mm_rcp_ss(__m128 __a) { return (__m128)__builtin_ia32_rcpss(__a); }
static inline __m128 _mm_rcp_ps(__m128 __a) { return __builtin_ia32_rcpps(__a); }

static inline __m128 _mm_min_ss(__m128 __a, __m128 __b) { return __builtin_ia32_minss(__a, __b); }
static inline __m128 _mm_min_ps(__m128 __a, __m128 __b) { return __builtin_ia32_minps(__a, __b); }
static inline __m128 _mm_max_ss(__m128 __a, __m128 __b) { return __builtin_ia32_maxss(__a, __b); }
static inline __m128 _mm_max_ps(__m128 __a, __m128 __b) { return __builtin_ia32_maxps(__a, __b); }

static inline __m128 _mm_setr_ps(float __z, float __y, float __x, float __w) { return (__m128){ __z, __y, __x, __w }; }
static inline __m128 _mm_setzero_ps(void) { return (__m128){ 0 }; }

#endif /* _XMMINTRIN_H_ */
