#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <windows.h>

#define USE_INTRIN 0

#if USE_INTRIN
#include <emmintrin.h>
#endif

#define NUM_ITERS 1000

//typedef char _ArchTest[sizeof(size_t) == 8 ? 1 : -1];
_Static_assert(sizeof(size_t) == 8, "64bit compiler expected");

static uint64_t get_timer_counter() {
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    return t.QuadPart;
}

static double get_timer_frequency() {
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    return 1.0 / (double)freq.QuadPart;
}

size_t count_newlines(size_t n, const char* data) {
    size_t lines = 0;
    for (size_t i = 0; i < n; i++) {
        lines += (data[i] == '\n') ? 1 : 0;
    }

    return lines;
}

#if USE_INTRIN
size_t count_newlines_simd(size_t n, const char* data) {
	const char* ptr = data;
	
    size_t lines = 0;
    size_t chunks = n / 16;
	
    for (size_t i = 0; i < chunks; i++) {
        __m128i bytes = _mm_loadu_si128((__m128i*)ptr);
        __m128i compare = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\n'));
		
        #if _MSC_FULL_VER
        lines += __popcnt(_mm_movemask_epi8(compare));
        #else
        lines += __builtin_popcount(_mm_movemask_epi8(compare));
        #endif

		ptr += 16;
    }

    size_t overhang = n % 16;
    for (size_t i = 0; i < overhang; i++) {
        lines += (*ptr == '\n');
		ptr += 1;
    }

    return lines;
}
#endif

typedef size_t LineCounterFunc(size_t n, const char* data);

void simple_bench(size_t len, const char* data, LineCounterFunc func, size_t expected, const char* name) {
	int64_t avg = 0;
	
	// Keeps running until the variance gets really small
	int times_been_ehh = 0;
	while (times_been_ehh < 20) {
		uint64_t t1 = get_timer_counter();
		if (func(len, data) != expected) {
			printf("Fatal error: bench received incorrect results\n");
			abort();
		}
		uint64_t t2 = get_timer_counter();
		
		// check the variance
		int64_t new_avg = (uint64_t) (((t2-t1) + ((uint64_t)avg) + 1ull) >> 1ull);
		int64_t diff = new_avg - avg;
		if (diff < -10 || diff > 10) {
			times_been_ehh = 0;
		} else {
			times_been_ehh++;
		}
	}
	
    double average = avg * get_timer_frequency();
    double bandwidth = (double)len / average;
	
	printf("Test '%s':\n", name);
    printf("Total Ticks: %zu over %d iterations\n", total_ticks, NUM_ITERS);
    printf("Input length: %d\n", len);
    printf("Bandwidth: %f GB/s\n", bandwidth / 1000000000.0);
}

int main(int argc, char** argv) {
    if (argc == 1) {
        printf("No input files!\n");
        return 1;
	}
	
    FILE* f = fopen(argv[1], "rb");
	if (f == NULL) {
		printf("Invalid filepath!\n");
		return 1;
	}
	
    char *text = (char *) malloc(8 << 20);
    int len = f ? (int) fread(text, 1, 8<<20, f) : -1;
	
	// fat null terminator
	memset(&text[len], 0, 16);
	
    if (len < 0) {
        printf("Error opening file\n");
        free(text);
        fclose(f);
        return 1;
    }
    fclose(f);
	
	simple_bench(len, text, 30381, count_newlines, "No-SIMD standard opt");
#if USE_INTRIN
	simple_bench(len, text, 30381, count_newlines_simd, "Manual SIMD");
#endif
	
    //free(text);
    return 0;
}
