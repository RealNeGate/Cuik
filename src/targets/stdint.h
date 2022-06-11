//
// stdint.h
//
//      Copyright (c) Microsoft Corporation. All rights reserved.
//
// The C Standard Library <stdint.h> header.
// Based on the Microsoft UCRT, modified for Cuik
//
#pragma once
#define _STDINT

typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

// 7.20.1.2 Minimum-width integer types
// All the (u)int_leastN_t are exact for our cases since we support all of these
typedef int8_t int_least8_t;
typedef int16_t int_least16_t;
typedef int32_t int_least32_t;
typedef int64_t int_least64_t;
typedef uint8_t uint_least8_t;
typedef uint16_t uint_least16_t;
typedef uint32_t uint_least32_t;
typedef uint64_t uint_least64_t;

// 7.20.1.3 Fastest minimum-width integer types
typedef signed char int_fast8_t;
typedef int int_fast16_t;
typedef int int_fast32_t;
typedef long long int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned int uint_fast16_t;
typedef unsigned int uint_fast32_t;
typedef unsigned long long uint_fast64_t;

// 7.20.1.4 Integer types capable of holding object pointers
#ifdef _CUIK_TARGET_64BIT_
typedef int64_t intptr_t;
typedef uint64_t uintptr_t;
#else
typedef int32_t intptr_t;
typedef uint32_t uintptr_t;
#endif

// 7.20.1.5 Greatest-width integer types
typedef long long intmax_t;
typedef unsigned long long uintmax_t;

// 7.20.2 Limits of specified-width integer types
//   7.20.2.1 Limits of exact-width integer types
#define INT8_MIN (-127 - 1)
#define INT16_MIN (-32767 - 1)
#define INT32_MIN (-2147483647 - 1)
#define INT64_MIN (-9223372036854775807ll - 1)

#define INT8_MAX 127
#define INT16_MAX 32767
#define INT32_MAX 2147483647
#define INT64_MAX 9223372036854775807ll

#define UINT8_MAX 0xFFu
#define UINT16_MAX 0xFFFFu
#define UINT32_MAX 0xFFFFFFFFu
#define UINT64_MAX 0xFFFFFFFFFFFFFFFFull

//   7.20.2.2 Limits of minimum-width integer types
#define INT_LEAST8_MIN INT8_MIN
#define INT_LEAST16_MIN INT16_MIN
#define INT_LEAST32_MIN INT32_MIN
#define INT_LEAST64_MIN INT64_MIN
#define INT_LEAST8_MAX INT8_MAX
#define INT_LEAST16_MAX INT16_MAX
#define INT_LEAST32_MAX INT32_MAX
#define INT_LEAST64_MAX INT64_MAX
#define UINT_LEAST8_MAX UINT8_MAX
#define UINT_LEAST16_MAX UINT16_MAX
#define UINT_LEAST32_MAX UINT32_MAX
#define UINT_LEAST64_MAX UINT64_MAX

//   7.20.2.3 Limits of fastest minimum-width integer types
#define INT_FAST8_MIN INT8_MIN
#define INT_FAST16_MIN INT32_MIN
#define INT_FAST32_MIN INT32_MIN
#define INT_FAST64_MIN INT64_MIN
#define INT_FAST8_MAX INT8_MAX
#define INT_FAST16_MAX INT32_MAX
#define INT_FAST32_MAX INT32_MAX
#define INT_FAST64_MAX INT64_MAX
#define UINT_FAST8_MAX UINT8_MAX
#define UINT_FAST16_MAX UINT32_MAX
#define UINT_FAST32_MAX UINT32_MAX
#define UINT_FAST64_MAX UINT64_MAX

#ifdef _CUIK_TARGET_64BIT_
#define INTPTR_MIN INT64_MIN
#define INTPTR_MAX INT64_MAX
#define UINTPTR_MAX UINT64_MAX
#else
#define INTPTR_MIN INT32_MIN
#define INTPTR_MAX INT32_MAX
#define UINTPTR_MAX UINT32_MAX
#endif

#define INTMAX_MIN INT64_MIN
#define INTMAX_MAX INT64_MAX
#define UINTMAX_MAX UINT64_MAX

#define PTRDIFF_MIN INTPTR_MIN
#define PTRDIFF_MAX INTPTR_MAX

#ifndef SIZE_MAX
// SIZE_MAX definition must match exactly with limits.h for modules support.
#ifdef _CUIK_TARGET_64BIT_
#define SIZE_MAX 0xFFFFFFFFFFFFFFFFull
#else
#define SIZE_MAX 0xFFFFFFFFu
#endif
#endif

#define SIG_ATOMIC_MIN INT32_MIN
#define SIG_ATOMIC_MAX INT32_MAX

#define WCHAR_MIN 0x0000
#define WCHAR_MAX 0xFFFF

#define WINT_MIN 0x0000
#define WINT_MAX 0xFFFF

#define INT8_C(x) (x)
#define INT16_C(x) (x)
#define INT32_C(x) (x)
#define INT64_C(x) (x##ll)

#define UINT8_C(x) (x)
#define UINT16_C(x) (x)
#define UINT32_C(x) (x##u)
#define UINT64_C(x) (x##ull)

#define INTMAX_C(x) INT64_C(x)
#define UINTMAX_C(x) UINT64_C(x)
