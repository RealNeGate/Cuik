#include <stdio.h>
#include <x86intrin.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

// NOTE(NeGate): Replaces all \t \v with spaces
static char* read_entire_file(const char* file_path) {
	////////////////////////////////
	// Read file
	////////////////////////////////
	char* text;
	int len;
	{
		FILE* file = fopen(file_path, "rb");
		if (!file) {
			printf("Could not read file: %s\n", file_path);
			abort();
		}
		
		int descriptor = fileno(file);
		
		struct stat file_stats;
		if (fstat(descriptor, &file_stats) == -1) {
			fclose(file);
			return NULL;
		}
		
		len  = file_stats.st_size;
		text = malloc(len + 17);
		
		fseek(file, 0, SEEK_SET);
		size_t length_read = fread(text, 1, len, file);
		
		// fat null terminator
		length_read = len;
		memset(&text[length_read], 0, 17);
		fclose(file);
	}
	
	////////////////////////////////
	// Remove tabs
	////////////////////////////////
	// NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
	// ARM variants and such but yea.
	for (size_t i = 0; i < len; i += 16) {
		__m128i bytes = _mm_load_si128((__m128i*) &text[i]);
		
		// Replace all \t and \v with spaces
		__m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
		test_ident = _mm_or_si128(test_ident, _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\v')));
		
		bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
		_mm_store_si128((__m128i*) &text[i], bytes);
	}
	
	return text;
}

static bool file_exists(const char *filename) {
	struct stat buffer;
	
	return (stat(filename, &buffer) == 0);
}