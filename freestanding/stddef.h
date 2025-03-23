#pragma once

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifdef _WIN32
// windows does it :p
//#include <corecrt.h>
#endif

// 7.19 Common definitions <stddef.h>
typedef long long          ptrdiff_t;
typedef unsigned long long size_t;
typedef long double        max_align_t;

typedef long long          intptr_t;
typedef unsigned long long uintptr_t;

#ifdef _WIN32
typedef unsigned short wchar_t;
#else
typedef int   wchar_t;
#endif

#define offsetof(s,m) ((size_t)&(((s*)0)->m))
// #define offsetof(s,m) __builtin_offsetof(s,m)
