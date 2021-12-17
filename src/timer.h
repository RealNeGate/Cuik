#pragma once

#include <time.h>
#include <stdio.h>

// NOTE(NeGate): Magic amirite
inline static void __timer_end(const char name[], clock_t start) {
    clock_t end = clock();
	double delta_ms = (end - start) / (double)CLOCKS_PER_SEC;
	printf("%s took %.03f seconds\n", name, delta_ms);
}

// Usage:
// timed_block("Beans") {
//   ...
// }
#define timed_block(name) for (clock_t __t1 = clock(), __i = 0; __i < 1; __i++, __timer_end(name, __t1))
