
#pragma once

#define MB_LEN_MAX 5

#define CHAR_BIT   8
#define BOOL_WIDTH 8

#define UCHAR_WIDTH 8
#define SCHAR_WIDTH 8
#define UCHAR_MAX   0xff
#define SCHAR_MIN   -0x80
#define SCHAR_MAX   +0x7f

#if defined(__CHAR_UNSIGNED__) || defined(_CHAR_UNSIGNED)
    #define CHAR_MIN UCHAR_MIN
    #define CHAR_MAX UCHAR_MAX
#else
    #define CHAR_MIN SCHAR_MIN
    #define CHAR_MAX SCHAR_MAX
#endif

#define USHRT_WIDTH 16
#define SHRT_WIDTH  16
#define USHRT_MAX   0xffff
#define SHRT_MIN    -0x8000
#define SHRT_MAX    +0x7fff

#define UINT_WIDTH  32
#define INT_WIDTH   32
#define UINT_MAX    0xffffffffu
#define INT_MIN     -0x80000000
#define INT_MAX     +0x7fffffff

#if defined(_WIN64)
    #define ULONG_WIDTH  32
    #define LONG_WIDTH   32
    #define ULONG_MAX    +0xffffffff
    #define LONG_MIN     -0x80000000
    #define LONG_MAX     +0x7fffffff
#else
    #define ULONG_WIDTH  64
    #define LONG_WIDTH   64
    #define ULONG_MAX    0xffffffffffffffffull
    #define LONG_MIN     -0x8000000000000000ll
    #define LONG_MAX     +0x7fffffffffffffffll
#endif

#define ULLONG_WIDTH     64
#define LLONG_WIDTH      64
#define ULLONG_MAX       0xffffffffffffffffull
#define LLONG_MIN        -0x8000000000000000ll
#define LLONG_MAX        +0x7fffffffffffffffll

#define PTRDIFF_WIDTH    LLONG_WIDTH
#define PTRDIFF_MIN      LLONG_MIN
#define PTRDIFF_MAX      LLONG_MAX

#define SIZE_WIDTH       ULLONG_WIDTH
#define SIZE_MAX         ULLONG_MAX

#define SIG_ATOMIC_WIDTH LLONG_WIDTH
#define SIG_ATOMIC_MIN   LLONG_MIN
#define SIG_ATOMIC_MAX   LLONG_MAX

#define WINT_WIDTH       INT_WIDTH
#define WINT_MIN         INT_MIN
#define WINT_MAX         INT_MAX

#define WCHAR_WIDTH      USHORT_WIDTH
#define WCHAR_MIN        USHORT_WIDTH
#define WCHAR_MAX        USHORT_WIDTH
