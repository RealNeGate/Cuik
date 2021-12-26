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
		memset(&text[length_read], 0, 17);
		fclose(file);
	}
	
	////////////////////////////////
	// Remove tabs, stitch together backslash-newline cases
	////////////////////////////////
	// NOTE(NeGate): This code requires SSE4.1, it's not impossible to make
	// ARM variants and such but yea.
	{
		char* stream = text;
		size_t batch_count = (len + 15) / 16;
		while (batch_count--) {
			//ptrdiff_t pos = (uintptr_t)stream - (uintptr_t)text;
			//assert(pos < len);
			
			__m128i bytes = _mm_load_si128((__m128i*) stream);
			
			// Replace all tabs with spaces
			__m128i test_ident = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\t'));
			bytes = _mm_blendv_epi8(bytes, _mm_set1_epi8(' '), test_ident);
			
			// Try to append lines in the backslash-newline case
			__m128i test_backslash = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\\'));
			int test_backslash_mask = _mm_movemask_epi8(test_backslash);
			
			if (test_backslash_mask) {
				_mm_store_si128((__m128i*) stream, bytes);
				char* end = stream + 16;
				
				do {
					int offset = _tzcnt_u32(test_backslash_mask);
					
					test_backslash_mask >>= offset+1;
					stream += offset+1;
					
					// NOTE(NeGate): This stuff is pretty slow code
					// so hopefully it doesn't get called a lot
					if (*stream == '\r' || *stream == '\n') {
						bool has_cr = (*stream == '\r');
						int sequence_len = has_cr ? 3 : 2;
						int line_end_len = has_cr ? 2 : 1;
						
						int current_pos = stream - text;
						assert(current_pos < len);
						
						int remaining = len - (current_pos - line_end_len);
						memmove(stream - line_end_len, stream + line_end_len, remaining + (16 - sequence_len));
						
						len -= sequence_len;
						stream -= sequence_len;
						batch_count = ((remaining + 15) / 16) - 1;
					}
				} while (test_backslash_mask);
				
				stream = end;
			} else {
				_mm_store_si128((__m128i*) stream, bytes);
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