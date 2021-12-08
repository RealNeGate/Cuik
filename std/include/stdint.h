#pragma once

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


