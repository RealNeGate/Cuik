#pragma once

typedef _Vector(unsigned char, 16) __m128i;

// m128i generic ops
static __m128i _mm_loadu_si128(const __m128i* src) { return *src; }
static __m128i _mm_load_si128(const __m128i* src) { return *src; }
static void _mm_store_si128(__m128i* dst, __m128i src) { *dst = src; }
