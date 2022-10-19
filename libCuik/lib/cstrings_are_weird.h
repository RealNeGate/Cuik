#ifndef _CSTRINGS_ARE_WEIRD_H_
#define _CSTRINGS_ARE_WEIRD_H_

#ifdef _WIN32
typedef wchar_t* OS_String;
typedef wchar_t OS_Char;

#define OS_STR(x) L##x
#define OS_STR_FMT "S"
#else
typedef char* OS_String;
typedef char OS_Char;

#define OS_STR(x) x
#define OS_STR_FMT "s"

int sprintf_s(char* buffer, size_t len, const char* format, ...);
#endif

#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include <wchar.h>

#define str_length(str) _Generic((str)[0], char: strlen, wchar_t: wcslen)(str)
#define str_find(str, str2) _Generic((str)[0], char: strstr, wchar_t: wcsstr)(str, str2)
#define str_find_ch(str, ch) _Generic((str)[0], char: strchr, wchar_t: wcschr)(str, ch)
#define str_reverse_find_ch(str, ch) _Generic((str)[0], char: strrchr, wchar_t: wcsrchr)(str, ch)
#define str_compare(str, str2) _Generic((str)[0], char: strcmp, wchar_t: wcscmp)(str, str2)

#endif /* _CSTRINGS_ARE_WEIRD_H_ */