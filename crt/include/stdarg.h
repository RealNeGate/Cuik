#pragma once

typedef char* va_list;

#define va_start(ap, a) __va_start(ap, a)
#define va_arg   __va_arg
#define va_end() ((void)0)
#define va_copy(destination, source) ((destination) = (source))
