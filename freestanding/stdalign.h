// 7.15 Alignment <stdalign.h>
#pragma once

#define __alignas_is_defined 1
#define alignas(n) _Alignas(n)
#define alignof(T) _Alignof(T)
