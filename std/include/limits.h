#pragma once

// 7.20.2.1 Limits of exact-width integer types
#define INT8_MIN         (-127 - 1)
#define INT16_MIN        (-32767 - 1)
#define INT32_MIN        (-2147483647 - 1)
#define INT64_MIN        (-9223372036854775807ll - 1)
#define INT8_MAX         127
#define INT16_MAX        32767
#define INT32_MAX        2147483647
#define INT64_MAX        9223372036854775807ll
#define UINT8_MAX        0xffu
#define UINT16_MAX       0xffffu
#define UINT32_MAX       0xffffffffu
#define UINT64_MAX       0xffffffffffffffffull

// 7.20.2.2 Limits of minimum-width integer types
#define INT_LEAST8_MIN   INT8_MIN
#define INT_LEAST16_MIN  INT16_MIN
#define INT_LEAST32_MIN  INT32_MIN
#define INT_LEAST64_MIN  INT64_MIN
#define INT_LEAST8_MAX   INT8_MAX
#define INT_LEAST16_MAX  INT16_MAX
#define INT_LEAST32_MAX  INT32_MAX
#define INT_LEAST64_MAX  INT64_MAX
#define UINT_LEAST8_MAX  UINT8_MAX
#define UINT_LEAST16_MAX UINT16_MAX
#define UINT_LEAST32_MAX UINT32_MAX
#define UINT_LEAST64_MAX UINT64_MAX

// 7.20.2.3 Limits of fastest minimum-width integer types
#define INT_FAST8_MIN    INT8_MIN
#define INT_FAST16_MIN   INT32_MIN
#define INT_FAST32_MIN   INT32_MIN
#define INT_FAST64_MIN   INT64_MIN
#define INT_FAST8_MAX    INT8_MAX
#define INT_FAST16_MAX   INT32_MAX
#define INT_FAST32_MAX   INT32_MAX
#define INT_FAST64_MAX   INT64_MAX
#define UINT_FAST8_MAX   UINT8_MAX
#define UINT_FAST16_MAX  UINT32_MAX
#define UINT_FAST32_MAX  UINT32_MAX
#define UINT_FAST64_MAX  UINT64_MAX

// 7.20.2.4 Limits of integer types capable of holding object pointers
#define INTPTR_MIN       INT64_MIN
#define INTPTR_MAX       INT64_MAX
#define UINTPTR_MAX      UINT64_MAX

#define INTMAX_MIN       INT64_MIN
#define INTMAX_MAX       INT64_MAX
#define UINTMAX_MAX      UINT64_MAX

// 7.20.3 Limits of other integer types
#define PTRDIFF_MIN      INT64_MIN
#define PTRDIFF_MAX      INT64_MAX

#define SIZE_MAX 0xffffffffffffffffull
