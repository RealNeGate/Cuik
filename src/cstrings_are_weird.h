#ifndef _CSTRINGS_ARE_WEIRD_H_
#define _CSTRINGS_ARE_WEIRD_H_

#include <stdbool.h>
#include <stddef.h>
#include <wchar.h>
#include <string.h>

#define str_length(str) _Generic((str)[0], \
        char:    strlen,                   \
        wchar_t: wcslen)(str)              \

#define str_find(str, str2) _Generic((str)[0], \
        char:    strstr,                       \
        wchar_t: wcsstr)(str, str2)            \

#define str_find_ch(str, ch) _Generic((str)[0], \
        char:    strchr,                        \
        wchar_t: wcschr)(str, ch)               \

#define str_reverse_find_ch(str, ch) _Generic((str)[0], \
        char:    strrchr,                               \
        wchar_t: wcsrchr)(str, ch)                      \

#define str_compare(str, str2) _Generic((str)[0], \
        char:    strcmp,                          \
        wchar_t: wcscmp)(str, str2)               \

#endif /* _CSTRINGS_ARE_WEIRD_H_ */