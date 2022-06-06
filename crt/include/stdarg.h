#pragma once

typedef char* va_list;

#define va_start __va_start
#define va_arg   __va_arg
#define va_end   ((void)0)
#define va_copy(destination, source) ((destination) = (source))
