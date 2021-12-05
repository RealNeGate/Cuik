#include "preprocessor.h"
#include "memory.h"

#include <ctype.h>
#include <sys/stat.h>
#include <x86intrin.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

#define SLOTS_PER_MACRO_BUCKET 32768
#define MACRO_BUCKET_COUNT 32

typedef enum PProc_State {
	// Just copying raw
	PPROC_STATE_CLEAN,
	// Spotted an identifier
	PPROC_STATE_IDENT,
	// Directive being parsed
	PPROC_STATE_DIRECTIVE,
	// End of scope (file/macro resolution)
	PPROC_STATE_NULL
} PProc_State;

typedef struct PProc_Scope {
	const char* directory;
	const char* file_path;
	const char* contents;
} PProc_Scope;

typedef struct Context {
	// how deep into directive scopes (#if, #ifndef, #ifdef) is it
	int depth;
	// tells you if the current scope has had an entry evaluated,
	// this is important for choosing when to check #elif and #endif
	int scope_eval[64];
	
	int macro_bucket_count[MACRO_BUCKET_COUNT];
	
	const char** macro_bucket_keys;
	size_t* macro_bucket_keys_length;
	const char** macro_bucket_values_start;
	const char** macro_bucket_values_end;
} Context;

// NOTE(NeGate): Also performs normalization
static char* read_entire_file(const char* file_path);

static PProc_State classify(unsigned char ch);
static uint64_t hash_ident(const char* at, size_t length);
static const char* skip_to_macro_end(const char* curr);
static bool file_exists(const char *filename);
static bool is_defined(Context* restrict c, const char* start, size_t length);
static bool find_define(Context* restrict c, string* out_val, const char* start, size_t length);
static const char* skip_directive_body(const char* curr);

static const char* SYSTEM_LIBS[] = {
	"W:/Windows Kits/10/Include/10.0.19041.0/ucrt/",
	"W:/Windows Kits/10/Include/10.0.19041.0/um/",
	"W:/Windows Kits/10/Include/10.0.19041.0/shared/",
	"W:/Visual Studio/2019/Community/VC/Tools/MSVC/14.29.30037/include/",
};

enum { NUM_SYSTEM_LIBS = sizeof(SYSTEM_LIBS) / sizeof(SYSTEM_LIBS[0]) };

#define SKIP_SPACE() while (*input == ' ') { input++; }
#define READ_IDENT() while (classify(*input) == PPROC_STATE_IDENT){ input++; }

#include "preprocessor_eval.h"

const unsigned char* preprocess_file(const char* base_file_path) {
	char* start_output = allocate_virtual_memory(MEGABYTES(256));
	
	// TODO(NeGate): each entry is 16bytes (we can do better...) 
	size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
	
	Context c = { 0 };
	c.macro_bucket_keys = allocate_virtual_memory(sz);
	c.macro_bucket_keys_length = allocate_virtual_memory(sz);
	c.macro_bucket_values_start = allocate_virtual_memory(sz);
	c.macro_bucket_values_end = allocate_virtual_memory(sz);
	memset(c.macro_bucket_count, 0, MACRO_BUCKET_COUNT * sizeof(int));
	
	// initialize scopes
	int top = 0;
	PProc_Scope stack[32];
	
	stack[top++] = (PProc_Scope){ "", base_file_path, read_entire_file(base_file_path) };
	
	// simple state machine
	char* output = start_output;
	do {
		PProc_Scope* s = &stack[--top];
		
		const char* directory = s->directory;
		const char* file_path = s->file_path;
		const char* input = s->contents;
		const char* start_of_fresh = s->contents;
		
		while (*input) {
			PProc_State state = classify(*input);
			switch (state) {
				case PPROC_STATE_NULL: goto end_scope;
				case PPROC_STATE_CLEAN: {
					char c0 = input[0];
					char c1 = input[1];
					if (c0 == '/' && c1 == '/') {
						// Copy any clean input
						memcpy(output, start_of_fresh, input - start_of_fresh);
						output += input - start_of_fresh;
						
						// skip single comment
						input = skip_to_macro_end(input);
						start_of_fresh = input;
						break;
					} else if (c0 == '/' && c1 == '*') {
						// Copy any clean input
						memcpy(output, start_of_fresh, input - start_of_fresh);
						output += input - start_of_fresh;
						
						// skip multiline comment
						input += 1;
						
						do {
							input += 1;
							// TODO(NeGate): Optimize this condition
						} while (*input && !(input[0] == '*' && input[1] == '/'));
						
						input += 2;
						start_of_fresh = input;
						break;
					}
					
					input += 1;
					break;
				}
				case PPROC_STATE_DIRECTIVE: {
					// Copy any clean input
					if (input != start_of_fresh) {
						memcpy(output, start_of_fresh, input - start_of_fresh);
						output += input - start_of_fresh;
					}
					
					input++; // skip #
					SKIP_SPACE();
					
					// NOTE(NeGate): technically over-reads but we have slack space
					// NOTE(NeGate): We also normalized the text so we should be expecting
					// a space right after most directives
					if (memcmp(input, "define ", 7) == 0) {
						input += 7;
						SKIP_SPACE();
						
						const char* name_start = input;
						READ_IDENT();
						
						if (input == name_start) {
							printf("#define must be followed by an identifier\n");
							abort();
						}
						
						// Hash name
						size_t name_len = input - name_start;
						uint64_t slot = hash_ident(name_start, name_len);
						uint64_t e = (slot * SLOTS_PER_MACRO_BUCKET) 
							+ c.macro_bucket_count[slot];
						
						// Insert into buckets
						c.macro_bucket_count[slot]++;
						c.macro_bucket_keys[e] = name_start;
						c.macro_bucket_keys_length[e] = input - name_start;
						
						// Parse macro definition
						const char* def_start = input;
						input = skip_to_macro_end(input);
						
						c.macro_bucket_values_start[e] = def_start;
						c.macro_bucket_values_end[e] = input;
					} else if (memcmp(input, "include ", 8) == 0) {
						input += 8;
						SKIP_SPACE();
						
						const char* quote_start = input + 1;
						bool system_libs = false;
						if (*input == '\"') {
							input++;
							
							do {
								__m128i chars = _mm_loadu_si128((__m128i*) input);
								int len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\"'))));
								
								if (len) {
									input += len;
									if (input[-1] == '\"' && input[-2] != '\\') break;
								} else {
									input += 16;
								}
							} while (*input);
						} else if (*input == '<') {
							system_libs = true;
							do {
								__m128i chars = _mm_loadu_si128((__m128i*) input);
								int len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('>'))));
								
								if (len) {
									input += len;
									break;
								} else {
									input += 16;
								}
							} while (*input);
						} else {
							abort();
						}
						
						const char* quote_end = input - 1;
						if (quote_start == quote_end) abort();
						
						SKIP_SPACE();
						if (*input != '\n') {
							printf("Expected newline after include\n");
							abort();
						}
						
						// Search for file in system libs
						char path[260];
						bool success = false;
						if (system_libs) {
							for (size_t i = 0; i < NUM_SYSTEM_LIBS; i++) {
								sprintf_s(path, 260, "%s%.*s",
										  SYSTEM_LIBS[i],
										  (int) (quote_end - quote_start), quote_start);
								
								if (file_exists(path)) {
									success = true;
									break;
								}
							}
						}
						
						if (!success) {
							// Try local includes
							sprintf_s(path, 260, "%s%.*s", directory, (int) (quote_end - quote_start), quote_start);
							
							if (file_exists(path)) success = true;
						}
						
						// save out info
						stack[top++] = (PProc_Scope){ directory, file_path, input };
						
						// TODO(NeGate): We really shouldn't be making heap allocations within this
						// code... it's supposed to be faster, we're supposed to be better than that
						char* new_path = _strdup(path);
						char* new_dir = _strdup(path);
						
						// just trim at the last slash
						// TODO(NeGate): Clean up
						char* curr = new_dir;
						char* slash = NULL;
						while (*curr) {
							if (*curr == '/') slash = curr;
							curr++;
						}
						
						if (slash) {
							slash[1] = '\0';
						}
						
						stack[top++] = (PProc_Scope){ new_dir, new_path, read_entire_file(path) };
						
						// restart scope within this include
						input = start_of_fresh;
						goto end_scope;
					} else if (memcmp(input, "if ", 3) == 0) {
						input += 3;
						
						const char* end = input;
						while (*end && *end != '\n') end++; 
						
						int result = eval(&c, input);
						input = end;
						
						if (result) c.depth++;
						else input = skip_directive_body(input);
						
						c.scope_eval[c.depth] = result;
					} else if (memcmp(input, "else", 4) == 0) {
						// TODO(NeGate): Handle the whitespace right after
						input += 5;
						
						// if it didn't evaluate any of the other options
						// do this
						if (c.scope_eval[c.depth] == 0) {
							c.depth++;
							c.scope_eval[c.depth] = 1; // got something at least.
						}
						else input = skip_directive_body(input);
					} else if (memcmp(input, "elif ", 5) == 0) {
						input += 5;
						
						const char* end = input;
						while (*end && *end != '\n') end++; 
						
						int result = eval(&c, input);
						input = end;
						
						// if it didn't evaluate any of the other options
						// try to do this
						if (c.scope_eval[c.depth] == 0) {
							if (result) c.depth++;
							else input = skip_directive_body(input);
							
							c.scope_eval[c.depth] = result;
						}
						else input = skip_directive_body(input);
					} else if (memcmp(input, "ifdef ", 6) == 0) {
						input += 6;
						SKIP_SPACE();
						
						const char* name_start = input;
						READ_IDENT();
						
						if (input == name_start) {
							printf("#ifdef must be followed by an identifier\n");
							abort();
						}
						
						size_t name_len = input - name_start;
						if (!is_defined(&c, name_start, name_len)) input = skip_directive_body(input);
						else c.depth++;
					} else if (memcmp(input, "ifndef ", 7) == 0) {
						input += 7;
						SKIP_SPACE();
						
						const char* name_start = input;
						READ_IDENT();
						
						if (input == name_start) {
							printf("#ifndef must be followed by an identifier\n");
							abort();
						}
						
						size_t name_len = input - name_start;
						if (is_defined(&c, name_start, name_len)) input = skip_directive_body(input);
						else c.depth++;
					} else if (memcmp(input, "endif", 5) == 0) {
						input += 5;
						
						// TODO(NeGate): Handle the whitespace right after
						if (c.depth == 0) panic("Too many endifs\n");
						c.depth--;
					} else if (memcmp(input, "pragma ", 7) == 0) {
						input += 7;
						SKIP_SPACE();
						
						if (*input == '\0') abort();
						READ_IDENT();
					} else if (memcmp(input, "undef ", 6) == 0) {
						input += 6;
						SKIP_SPACE();
						
						const char* name_start = input;
						READ_IDENT();
						
						if (input == name_start) {
							printf("#undef must be followed by an identifier\n");
							abort();
						}
						
						// Hash name
						size_t name_len = input - name_start;
						uint64_t slot = hash_ident(name_start, name_len);
						uint64_t e = (slot * SLOTS_PER_MACRO_BUCKET) 
							+ c.macro_bucket_count[slot];
						
						if (is_defined(&c, name_start, name_len)) {
							// remove swap
							uint64_t last = c.macro_bucket_count[slot] - 1;
							if (c.macro_bucket_count[slot] > 1) {
								c.macro_bucket_keys_length[e]  = c.macro_bucket_keys_length[last];
								c.macro_bucket_keys[e]         = c.macro_bucket_keys[last];
								c.macro_bucket_values_start[e] = c.macro_bucket_values_start[last];
								c.macro_bucket_values_end[e]   = c.macro_bucket_values_end[last];
							}
							c.macro_bucket_count[slot]--;
						}
					} else {
						abort();
					}
					
					/*if (*input != '\n') {
						printf("Expected newline\n");
						abort();
					}*/
					
					SKIP_SPACE();
					input += (*input == '\n');
					
					start_of_fresh = input;
					break;
				}
				case PPROC_STATE_IDENT: {
					const char* name_start = input;
					READ_IDENT();
					size_t name_len = input - name_start;
					
					string def;
					if (find_define(&c, &def, name_start, name_len)) {
						// Copy any clean input
						memcpy(output, start_of_fresh, name_start - start_of_fresh);
						output += name_start - start_of_fresh;
						
						// Replace identifier
						memcpy(output, def.data, def.length);
						output += def.length;
						
						start_of_fresh = input;
					}
					break;
				}
			}
		}
		
		end_scope:
		// Copy any clean input
		if (input != start_of_fresh) {
			memcpy(output, start_of_fresh, input - start_of_fresh);
			output += input - start_of_fresh;
		}
	} while (top > 0);
	
	free_virtual_memory((void*)c.macro_bucket_keys);
	free_virtual_memory((void*)c.macro_bucket_keys_length);
	free_virtual_memory((void*)c.macro_bucket_values_start);
	free_virtual_memory((void*)c.macro_bucket_values_end);
	
	return (unsigned char*)start_output;
}

//
// File IO
//
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
						memmove(stream - 1, stream + 1, (len + 16) - 2);
						
						len -= 2;
						stream -= 2;
						batch_count = (len + 15) / 16;
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

//
// State Machine Classifier
// table is bit-packed of the PProc_State per
// character, it's auto-generated.
//
const static uint8_t classifier_table[256] = {
	0x03,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x54,0x55,0x55,0x55,0x55,0x55,0x15,0x40,
	0x54,0x55,0x55,0x55,0x55,0x55,0x15,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
};

static PProc_State classify(unsigned char ch) {
	// extract 2bits from the 64byte table
	size_t index = ch / 4;
	size_t shift = (ch % 4) * 2;
	
	return (PProc_State) ((classifier_table[index] >> shift) & 0b11);
}

//
// Hashing
// based on the refterm hash by Casey Muratori
//
static char unsigned overhang_mask[32] = {
    255, 255, 255, 255, 255, 255, 255, 255,
	255, 255, 255, 255, 255, 255, 255, 255,
    0,   0,   0,   0,   0,   0,   0,   0,
	0,   0,   0,   0,   0,   0,   0,   0
};

static char unsigned default_seed[16] = {
	178, 201, 95, 240, 40, 41, 143, 216,
	2, 209, 178, 114, 232, 4, 176, 188
};

static uint64_t hash_ident(const char* at, size_t length) {
	__m128i hash = _mm_cvtsi64_si128(length);
	hash = _mm_xor_si128(hash, _mm_loadu_si128((__m128i*)default_seed));
	
	size_t chunk_count = length / 16;
	while(chunk_count--) {
		__m128i in = _mm_loadu_si128((__m128i*) at);
		at += 16;
		
		hash = _mm_xor_si128(hash, in);
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	}
	
	size_t overhang = length % 16;
	__m128i in = _mm_loadu_si128((__m128i*) at);
	
	in = _mm_and_si128(in, _mm_loadu_si128((__m128i*) (overhang_mask + 16 - overhang)));
	hash = _mm_xor_si128(hash, in);
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	
	return _mm_extract_epi8(hash, 0) & 31;
}

//
// Helper crap
// Macros end at the newline character unless a backslash exists
// at the end of the line.
// TODO(NeGate): Optimize this!
//
static const char* skip_to_macro_end(const char* curr) {
    // branchless skip of a space
    curr += (*curr == ' ');
	
    do {
		switch (*curr) {
			case '\0': return NULL;
			
			// Whitespace
			// https://gist.github.com/pervognsen/12d358c8fdd531d21fbccdca251fc2cd
			case '\r':
			curr++;
			if (*curr != '\n') break;
			// Default-predicted fallthrough case for \r\n on Windows.
			case '\n': 
			return curr;
			
			case ' ':
			case '\t':
			case '\v':
			// This path should be rare: more than 1 intertoken space, more than 16 spaces after newline, or rare whitespace character.
			do { 
				curr++;
			} while (isspace(*curr)); // Probably not libc isspace but you get the idea: just eat the length mispredict.
			break;
			
			case '\\':
			curr += 1;
			curr += (*curr == '\r');
			curr += (*curr == '\n');
			break;
			
			default:
			curr += 1;
			break;
		}
	} while (true);
}

static const char* skip_directive_body(const char* input) {
	int depth = 0;
	
	while (true) {
		PProc_State state = classify(*input);
		
		switch (state) {
			case PPROC_STATE_NULL: abort(); // unfinished macro scope
			case PPROC_STATE_CLEAN: {
				char c0 = input[0];
				char c1 = input[1];
				if (c0 == '/' && c1 == '/') {
					input = skip_to_macro_end(input);
				} else if (c0 == '/' && c1 == '*') {
					// skip multiline comment
					input += 1;
					
					do {
						input += 1;
						// TODO(NeGate): Optimize this condition
					} while (*input && !(input[0] == '*' && input[1] == '/'));
					
					input += 2;
					break;
				}
				
				input += 1;
				break;
			}
			case PPROC_STATE_DIRECTIVE: {
				input++; // skip #
				
				SKIP_SPACE();
				if (*input == '\0') abort();
				
				const char* directive_start = input;
				READ_IDENT();
				
				size_t directive_len = input - directive_start;
				switch (directive_len) {
					case 2:
					if (memcmp(directive_start, "if", 2) == 0) depth++;
					break;
					case 5:
					if (memcmp(directive_start, "ifdef", 5) == 0) depth++;
					else if (memcmp(directive_start, "endif", 5) == 0) {
						if (depth == 0) goto done;
						depth--;
					}
					break;
					case 6:
					if (memcmp(directive_start, "ifndef", 6) == 0) depth++;
					break;
					default: 
					break;
				}
				
				break;
			}
			case PPROC_STATE_IDENT: input += 1; break;
		}
	}
	
	done:
	return input;
}

static bool is_defined(Context* restrict c, const char* start, size_t length) {
	uint64_t slot = hash_ident(start, length);
	size_t count = c->macro_bucket_count[slot];
	size_t base = (slot * SLOTS_PER_MACRO_BUCKET);
	
	for (size_t i = 0; i < count; i++) {
		if (c->macro_bucket_keys_length[base + i] == length &&
			memcmp(c->macro_bucket_keys[base + i], start, length) == 0) {
			return true;
		}
	}
	
	return false;
}

static bool find_define(Context* restrict c, string* out_val, const char* start, size_t length) {
	uint64_t slot = hash_ident(start, length);
	size_t count = c->macro_bucket_count[slot];
	size_t base = (slot * SLOTS_PER_MACRO_BUCKET);
	
	for (size_t i = 0; i < count; i++) {
		if (c->macro_bucket_keys_length[base + i] == length &&
			memcmp(c->macro_bucket_keys[base + i], start, length) == 0) {
			
			const char* s = c->macro_bucket_values_start[base + i];
			const char* e = c->macro_bucket_values_end[base + i];
			
			*out_val = (string) { .data = s, .length = e - s };
			return true;
		}
	}
	
	return false;
}
