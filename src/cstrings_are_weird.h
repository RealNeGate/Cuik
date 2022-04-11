#ifndef _CSTRINGS_ARE_WEIRD_H_
#define _CSTRINGS_ARE_WEIRD_H_

#include <stdbool.h>
#include <stddef.h>
#include <wchar.h>
#include <string.h>

#define str_length(str) _Generic((str),              \
        char*:          strlen,                      \
        wchar_t*:       wcslen,                      \
        const char*:    strlen,                      \
        const wchar_t*: wcslen)(str)                 \

#define str_find(str, str2) _Generic((str),          \
        char*:    strstr,                            \
        wchar_t*: wcsstr,                            \
        const char*:    strstr,                      \
        const wchar_t*: wcsstr)(str, str2)           \

#define str_find_ch(str, ch) _Generic((str),         \
        char*:    strchr,                            \
        wchar_t*: wcschr,                            \
        const char*:    strchr,                      \
        const wchar_t*: wcschr)(str, ch)             \

#define str_reverse_find_ch(str, ch) _Generic((str), \
		char*:    strrchr,                           \
        wchar_t*: wcsrchr,                           \
        const char*:    strrchr,                     \
        const wchar_t*: wcsrchr)(str, ch)            \

#define str_compare(str, str2) _Generic((str),       \
		char*:    strcmp,                            \
        wchar_t*: wcscmp,                            \
        const char*:    strcmp,                      \
        const wchar_t*: wcscmp)(str, str2)           \

#endif /* _CSTRINGS_ARE_WEIRD_H_ */
