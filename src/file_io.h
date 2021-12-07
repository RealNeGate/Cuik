#include <stdio.h>
#include <x86intrin.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

// NOTE(NeGate): Replaces all \t \v with spaces and removing all
// carriage returns.
//
// NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
// ARM variants and such but yea.
static char* read_entire_file(const char* file_path) {
	////////////////////////////////
	// Read file
	////////////////////////////////
	char* text;
	int len;
	{
		FILE* file = fopen(file_path, "rb");
		int descriptor = fileno(file);
		
		struct stat file_stats;
		if (fstat(descriptor, &file_stats) == -1) return NULL;
		
		len  = file_stats.st_size;
		text = malloc(len + 16);
		
		fseek(file, 0, SEEK_SET);
		size_t length_read = fread(text, 1, len, file);
		
		text[length_read] = '\0';
		fclose(file);
	}
	
	////////////////////////////////
	// Remove \r
	////////////////////////////////
	{
		char* in = text;
		char* out = text;
		
		while (in[0]) {
			if (in[0] == '\r') {
				// skips 2 if it's \r\n and one if \r
				in += 1;
				in += (*in == '\n');
				
				*out++ = '\n';
			} else {
				*out++ = *in++;
			}
		}
		
		memset(out, 0, 16);
		len = (out - text);
	}
	
	////////////////////////////////
	// Remove tabs, stitch together backslash-newline cases
	////////////////////////////////
	{
		char* stream = text;
		size_t batch_count = (len + 15) / 16;
		while (batch_count--) {
			__m128i bytes = _mm_loadu_si128((__m128i*) stream);
			
			// Replace all tabs with spaces
			__m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
			bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
			
			// Try to append lines in the backslash-newline case
			__m128i test_backslash = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\\'));
			int test_backslash_mask = _mm_movemask_epi8(test_backslash);
			
			if (test_backslash_mask) {
				_mm_storeu_si128((__m128i*) stream, bytes);
				char* end = stream + 16;
				
				do {
					int offset = _tzcnt_u32(test_backslash_mask);
					
					test_backslash_mask >>= offset+1;
					stream += offset+1;
					
					// NOTE(NeGate): This stuff is pretty slow code
					// so hopefully it doesn't get called a lot
					if (*stream == '\n') {
						int current_pos = stream - text;
						assert(current_pos < len);
						
						int remaining = len - (current_pos - 1);
						memmove(stream - 1, stream + 1, remaining + (16 - 2));
						
						len -= 2;
						stream -= 2;
						batch_count = (remaining + 15) / 16;
					}
				} while (test_backslash_mask);
				
				stream = end;
			} else {
				_mm_storeu_si128((__m128i*) stream, bytes);
				stream += 16;
			}
		}
	}
	
	return text;
}

static bool file_exists(const char *filename) {
	struct stat buffer;
	
	return (stat(filename, &buffer) == 0);
}