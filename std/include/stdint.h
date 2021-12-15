#pragma once
#include <limits.h>

// 7.20.1.1 Exact-width integer types
typedef signed char          int8_t;
typedef short                int16_t;
typedef int                  int32_t;
typedef long long            int64_t;
typedef unsigned char        uint8_t;
typedef unsigned short       uint16_t;
typedef unsigned int         uint32_t;
typedef unsigned long long   uint64_t;

// 7.20.1.2 Minimum-width integer types
typedef signed char          int_least8_t;
typedef short                int_least16_t;
typedef int                  int_least32_t;
typedef long long            int_least64_t;
typedef unsigned char        uint_least8_t;
typedef unsigned short       uint_least16_t;
typedef unsigned int         uint_least32_t;
typedef unsigned long long   uint_least64_t;

// 7.20.1.3 Fastest minimum-width integer types
typedef signed char          int_fast8_t;
typedef int                  int_fast16_t;
typedef int                  int_fast32_t;
typedef long long            int_fast64_t;
typedef unsigned char        uint_fast8_t;
typedef unsigned int         uint_fast16_t;
typedef unsigned int         uint_fast32_t;
typedef unsigned long long   uint_fast64_t;

// 7.20.1.4 Integer types capable of holding object pointers
typedef long long            intptr_t;
typedef unsigned long long   uintptr_t;

// 7.20.1.5 Greatest-width integer types
typedef long long            intmax_t;
typedef unsigned long long   uintmax_t;


// missing sig atomic, wint and wchar sizes

// 7.20.4.1 Macros for minimum-width integer constants
#define INT8_C(x)    (x)
#define INT16_C(x)   (x)
#define INT32_C(x)   (x)
#define INT64_C(x)   (x ## LL)

#define UINT8_C(x)   (x)
#define UINT16_C(x)  (x)
#define UINT32_C(x)  (x ## U)
#define UINT64_C(x)  (x ## ULL)

// 7.20.4.2 Macros for greatest-width integer constants
#define INTMAX_C(x)  INT64_C(x)
#define UINTMAX_C(x) UINT64_C(x)
