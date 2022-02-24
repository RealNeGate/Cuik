#include "lexer.h"
#include <x86intrin.h>

const char keywords[][16] = {
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Generic",
    "_Imaginary",
    "_Pragma",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
    "_Typeof",
    "__cdecl",
    "__stdcall",
	"__declspec"
};

enum {
	CHAR_CLASS_NULL,
	// A-Z a-z 0-9 _
	CHAR_CLASS_IDENT,
	// 0-9
	CHAR_CLASS_NUMBER,
	// ;{}()
	CHAR_CLASS_SEPARATOR,
	// + ++ +=
	CHAR_CLASS_MULTICHAR1,
	// > >= >> >>= < <= << <<=
	CHAR_CLASS_MULTICHAR2,
	// - -> -- -=
	CHAR_CLASS_MULTICHAR3,
	// 'foo' "bar"
	CHAR_CLASS_STRING,
	// . ...
	CHAR_CLASS_DOT,
	// # ##
	CHAR_CLASS_HASH
};

static _Alignas(64) uint8_t char_classes[256] = {
	['A' ... 'Z'] = CHAR_CLASS_IDENT,
	['a' ... 'z'] = CHAR_CLASS_IDENT,
	['_'] = CHAR_CLASS_IDENT,
	
	['0' ... '9'] = CHAR_CLASS_NUMBER,
	
	['?'] = CHAR_CLASS_SEPARATOR,
	[';'] = CHAR_CLASS_SEPARATOR,
	[':'] = CHAR_CLASS_SEPARATOR,
	[','] = CHAR_CLASS_SEPARATOR,
	['['] = CHAR_CLASS_SEPARATOR,
	[']'] = CHAR_CLASS_SEPARATOR,
	['('] = CHAR_CLASS_SEPARATOR,
	[')'] = CHAR_CLASS_SEPARATOR,
	['{'] = CHAR_CLASS_SEPARATOR,
	['}'] = CHAR_CLASS_SEPARATOR,
	
	['+'] = CHAR_CLASS_MULTICHAR1,
	['*'] = CHAR_CLASS_MULTICHAR1,
	['/'] = CHAR_CLASS_MULTICHAR1,
	['%'] = CHAR_CLASS_MULTICHAR1,
	['!'] = CHAR_CLASS_MULTICHAR1,
	['='] = CHAR_CLASS_MULTICHAR1,
	['&'] = CHAR_CLASS_MULTICHAR1,
	['^'] = CHAR_CLASS_MULTICHAR1,
	['|'] = CHAR_CLASS_MULTICHAR1,
	['~'] = CHAR_CLASS_MULTICHAR1,
	
	['>'] = CHAR_CLASS_MULTICHAR2,
	['<'] = CHAR_CLASS_MULTICHAR2,
	
	['-'] = CHAR_CLASS_MULTICHAR3,
	
	['\"'] = CHAR_CLASS_STRING,
	['\''] = CHAR_CLASS_STRING,
	
	['.'] = CHAR_CLASS_DOT,
	
	['#'] = CHAR_CLASS_HASH
};

// AUTO-GENERATED
// \t \n \r *space*
static _Alignas(64) uint8_t space_char_tbl[] = {
	0x00,0x26,0x00,0x00,
	0x01,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
};

__attribute__((always_inline))
inline static bool is_space(char ch) {
	size_t i = ch;
	size_t index = i / 8;
	size_t shift = i & 7;
	return (space_char_tbl[index] >> shift) & 1;
}

uint16_t hash_with_len(const void* data, size_t len) {
    uint8_t* p = (uint8_t*)data;
    uint16_t hash = 0;
	
#pragma unroll 1
	for (size_t i = 0; i < len; i++) {
		hash ^= (p[i] << (i % 8));
	}
	
    return hash;
}

TknType classify_ident(const unsigned char* restrict str, size_t len) {
	// Auto-generated with this small C program (MAKE SURE TO UPDATE THE
	// KEYWORDS ARRAY AND TOKEN TYPES)
	//
	// https://gist.github.com/RealNeGate/397db4aaace43e0499dc8f7b429ccc17
	//
	// BINARY SEARCH ARRAYS
	const static uint16_t keys[64] = {
		
		0x00A5,0x00BA,0x0165,0x0170,0x0205,0x0211,0x0223,0x022C,
		0x0232,0x0245,0x0259,0x02A7,0x0433,0x0495,0x04AA,0x04DF,
		0x054A,0x05CF,0x05DD,0x080D,0x081E,0x0820,0x084F,0x0851,
		0x088D,0x089D,0x09A9,0x0A4B,0x10A3,0x110E,0x1145,0x11AF,
		0x137D,0x15EE,0x2155,0x21E5,0x21F0,0x22A1,0x22DD,0x2635,
		0x2681,0x2855,0x2A14,0x2B9C,0x2D8A,0x2DCD,0x2E11,0x34C2,
		0x3AC1,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,
		0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,0xFFFF,
		
	};
	const static uint8_t values[64] = {
		15,7,17,13,10,14,0,31,18,2,9,3,33,29,1,37,12,22,4,16,8,21,25,24,11,23,27,26,45,28,36,41,46,6,35,39,32,40,30,5,47,34,20,19,48,38,42,43,44,
	};
	
	// HASH STRING
	uint16_t n = hash_with_len(str, len);
	
	// BRANCHLESS BINARY SEARCH
	size_t i = 0;
	i += (keys[i + 32] <= n) * 32;
	i += (keys[i + 16] <= n) * 16;
	i += (keys[i + 8] <= n) * 8;
	i += (keys[i + 4] <= n) * 4;
	i += (keys[i + 2] <= n) * 2;
	i += (keys[i + 1] <= n) * 1;
	size_t v = values[i];
	
	// VERIFY
	__m128i kw128  = _mm_loadu_si128((__m128i*) &keywords[v]);
	__m128i str128 = _mm_loadu_si128((__m128i*) str);
	
	int kw_len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(kw128, _mm_set1_epi8('\0')))) - 1;
	
	// NOTE(NeGate): Fancy x86 strcmp crap :)
	int result = _mm_cmpestri(kw128, kw_len,
							  str128, len,
							  _SIDD_UBYTE_OPS |
							  _SIDD_CMP_EQUAL_EACH | 
							  _SIDD_NEGATIVE_POLARITY |
							  _SIDD_UNIT_MASK);
	
	return result == 16 ? (640 + v) : TOKEN_IDENTIFIER;
}

static int line_counter(size_t len, const unsigned char* str) {
#if 1
	int line_count = 0;
	for (size_t i = 0; i < len; i++) {
		line_count += (str[i] == '\n');
	}
	
	return line_count;
#else
	// TODO(NeGate): Test this out a bit before using it
	static unsigned char overhang_mask[32] = {
		255, 255, 255, 255,  255, 255, 255, 255,  255, 255, 255, 255,  255, 255, 255, 255,
		0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0
	};
	
	size_t line_count = 0;
	size_t chunk_count = len / 16;
	while (chunk_count) {
		__m128i str128 = _mm_loadu_si128((__m128i*) str);
		str += 16;
		
		unsigned int lf_mask = _mm_movemask_epi8(_mm_cmpeq_epi8(str128, _mm_set1_epi8('\n')));
		line_count += __builtin_popcount(lf_mask);
	}
	
	size_t overhang = len % 16;
	__m128 str128 = _mm_and_si128(str, _mm_loadu_si128((__m128i*) &overhang_mask[16 - overhang]));
	unsigned int lf_mask = _mm_movemask_epi8(_mm_cmpeq_epi8(str128, _mm_set1_epi8('\n')));
	line_count += __builtin_popcount(lf_mask);
	
	return line_count;
#endif
}

// NOTE(NeGate): The input string has a fat null terminator of 16bytes to allow
// for some optimizations overall, one of the important ones is being able to read
// a whole 16byte SIMD register at once for any SIMD optimizations. 
void lexer_read(Lexer* restrict l) {
	if (l->line_current2) {
		l->line_current = l->line_current2;
		l->line_current2 = NULL;
	}
	
    const unsigned char* current = l->current;
    
	// Skip any whitespace and comments
    // branchless space skip
    current += (*current == ' ');
	
	// NOTE(NeGate): We canonicalized spaces \t \v
	// in the preprocessor so we don't need to handle them
	redo_lex: {
		if (*current == '\0') {
			// quit, we're done
			l->hit_line = true;
			l->token_type = '\0';
			return;
		} else if (*current == '\r' || *current == '\n') {
			current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
			
			l->line_current = current;
			l->hit_line = true;
			l->current_line += 1;
            goto redo_lex;
		} else if (*current == ' ') {
			// SIMD space skip
			__m128i chars = _mm_loadu_si128((__m128i *)current);
			int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8(' '))));
			
			current += len - 1;
			goto redo_lex;
		} else if (*current == '/') {
			if (current[1] == '/') {
				do { current++; } while (*current && *current != '\n');
				
				current += 1;
				l->line_current = current;
				l->hit_line = true;
				l->current_line += 1;
				goto redo_lex;
			} else if (current[1] == '*') {
				current++;
				
				const unsigned char* start = current;
				do { current++; } while (*current && !(current[0] == '/' && current[-1] == '*'));
				current++;
				
				int lines_elapsed = line_counter(current - start, start);
				
				l->line_current = current;
				l->current_line += lines_elapsed;
				l->hit_line = (lines_elapsed > 0);
				goto redo_lex;
			}
		} else if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
			// this happens when there's a backslash-newline that doesn't
			// necessarily need to join tokens but just joins the lines
			current += 1;
			current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
			
			l->line_current = current;
			l->current_line += 1;
			goto redo_lex;
		}
	}
	
	////////////////////////////////
	// Try to actually parse a token
	////////////////////////////////
	const unsigned char* start = current;
	uint8_t initial_class = char_classes[*current++];
	
	// Hacky but yea
	if (start[0] == 'L' && start[1] == '\"') {
		initial_class = CHAR_CLASS_STRING;
		current++;
	}
	
	switch (initial_class) {
		case CHAR_CLASS_NULL: break;
		case CHAR_CLASS_IDENT: {
			while (char_classes[*current] == CHAR_CLASS_IDENT ||
				   char_classes[*current] == CHAR_CLASS_NUMBER) {
				current++;
			}
			
			l->token_type = TOKEN_IDENTIFIER;
			break;
		}
		case CHAR_CLASS_NUMBER: {
			if (current[-1] == '0' && current[0] == 'x') {
				current++;
				
				while ((*current >= '0' && *current <= '9') ||
					   (*current >= 'A' && *current <= 'F') ||
					   (*current >= 'a' && *current <= 'f')) { current++; }
				
				l->token_type = TOKEN_INTEGER;
				if (*current == '.') {
					// floats
					l->token_type = TOKEN_FLOAT;
					current++;
					
					while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
					
					if (*current == 'p') {
						current++;
						if (*current == '+' || *current == '-') current++;
						
						while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
					}
				}
			} else {
				while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
				l->token_type = TOKEN_INTEGER;
				
				if (*current == '.') {
					// floats
					l->token_type = TOKEN_FLOAT;
					current++;
					
					while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
				}
				
				if (*current == 'e') {
					// floats but cooler
					l->token_type = TOKEN_FLOAT;
					
					current++;
					if (*current == '+' || *current == '-') current++;
					
					while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
				}
			}
			
			// suffix
			if (*current == 'i') {
				current++;
				
				// at most it's two numbers
				current += (char_classes[*current] == CHAR_CLASS_NUMBER);
				current += (char_classes[*current] == CHAR_CLASS_NUMBER);
			} else if (*current == 'u') {
				current++;
				
				while (char_classes[*current] == CHAR_CLASS_IDENT) {
					current++;
				}
				
				// at most it's two numbers
				current += (char_classes[*current] == CHAR_CLASS_NUMBER);
				current += (char_classes[*current] == CHAR_CLASS_NUMBER);
			} else {
				while (char_classes[*current] == CHAR_CLASS_IDENT) {
					current++;
				}
			}
			break;
		}
		case CHAR_CLASS_SEPARATOR: {
			l->token_type = *start;
			break;
		}
		case CHAR_CLASS_MULTICHAR1: {
			l->token_type = *start;
			if (*current == '=') {
				l->token_type += 384;
				current++;
			} else if (*current == *start && *current != '*') {
				l->token_type += 256;
				current++;
			}
			break;
		}
		case CHAR_CLASS_MULTICHAR2: {
			l->token_type = *start;
			if (*current == '=') {
				l->token_type += 256;
				current++;
			} else if (*current == *start) {
				l->token_type += 384;
				current++;
				
				if (*current == '=') {
					l->token_type += 128;
					current++;
				}
			}
			break;
		}
		case CHAR_CLASS_MULTICHAR3: {
			l->token_type = *start;
			
			if (*current == '-') {
				l->token_type = TOKEN_DECREMENT;
				current++;
			} else if (*current == '=') {
				l->token_type = TOKEN_MINUS_EQUAL;
				current++;
			} else if (*current == '>') {
				l->token_type = TOKEN_ARROW;
				current++;
			}
			break;
		}
		case CHAR_CLASS_STRING: {
			char quote_type = *start == '\'' ? '\'' : '\"';
			__m128i pattern = _mm_set1_epi8(quote_type);
			
			do {
				__m128i bytes = _mm_loadu_si128((__m128i *)current);
				
				// strings either end at the quote or are cut off early via a 
				// newline unless you put a backslash-newline joiner.
				__m128i test_quote = _mm_cmpeq_epi8(bytes, pattern);
				__m128i test_newline = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\n'));
				__m128i test = _mm_or_si128(test_quote, test_newline);
				int len = __builtin_ffs(_mm_movemask_epi8(test));
				
				if (len) {
					current += len;
					l->current_line += (current[-1] == '\n');
					
					// backslash join
					if (current[-1] == '\n' && current[-2] == '\\') continue;
					
					// escape + quote like \"
					if (current[-1] == quote_type && current[-2] == '\\' && current[-3] != '\\') continue;
					
					break;
				} else {
					current += 16;
				}
			} while (*current);
			
			l->token_type = quote_type;
			break;
		}
		case CHAR_CLASS_DOT: {
			if (current[0] == '.' && current[1] == '.') {
				current += 2;
				
				l->token_type = TOKEN_TRIPLE_DOT;
				break;
			}
			
			l->token_type = '.';
			break;
		}
		case CHAR_CLASS_HASH: {
			if (*current == '#') {
				current++;
				l->token_type = TOKEN_DOUBLE_HASH;
				break;
			}
			
			l->token_type = TOKEN_HASH;
			break;
		}
		default:
		abort();
	}
	
	if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
		// TODO(NeGate): This code could use emotional help... if you're smart
		// and/or cool please consider providing it.
		
		// it increments the line counter but it doesn't mark a hit_line
		// because the line terminator technically is removed. 
		l->current_line += 1;
		
		// save out the original token position
		l->token_start = start;
		l->token_end = current;
		
		// skip backslash and newline
		current += 1;
		current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
		
		l->line_current2 = current;
		
		start = current;
		while (*current && *current != '\n' && *current != ' ') {
			char c0 = current[0], c1 = current[1];
			
			if (c0 == '\\' && c1 == '\n') current += 2;
			else current += 1;
		}
		
		// tally up lines
		l->current_line += line_counter(current - start, start);
		
		// generate buffer with conjoined string
		unsigned char* conjoined_buffer;
		size_t len = l->token_end - l->token_start;
		size_t len2 = current - start;
		
		{
			conjoined_buffer = arena_alloc((len + len2 + 15) & ~15, 16);
			if (!conjoined_buffer) {
				printf("Lexer error: out of memory!");
				abort();
			}
			
			l->line_current = conjoined_buffer;
			
			memcpy(conjoined_buffer, l->token_start, len);
			memcpy(conjoined_buffer + len, start, len2);
			
			// null terminator to top it off
			conjoined_buffer[len + len2] = '\0';
		}
		
		// Relex the joined string:
		// Kinda recursive in a way but... shut up?
		Lexer joined_string_lexer = (Lexer) { "", conjoined_buffer, conjoined_buffer, 1 };
		lexer_read(&joined_string_lexer);
		
		l->token_start = joined_string_lexer.token_start;
		l->token_end = joined_string_lexer.token_end;
		
		// NOTE(NeGate): Basically take the remaining token stuff we didn't parse
		// and just pass that but the issue is that these are two separate buffers
		// so we do a little "magic?".
		l->current = start + ((l->token_end - l->token_start) - len);
	} else {
		l->token_start = start;
		l->token_end = current;
		l->current = current;
	}
}

uint64_t parse_int(size_t len, const char* str, IntSuffix* out_suffix) {
	char* end;
	uint64_t i = strtoull(str, &end, 0);
	
	IntSuffix suffix = INT_SUFFIX_NONE;
	if (end != &str[len]) {
		do {
			switch (end[0]) {
				case 'u': case 'U': suffix |= 1; break;
				case 'l': case 'L': suffix += 2; break;
			}
			end++;
		} while (end != &str[len]);
		
		if (suffix >= 6) abort();
	}
	
	*out_suffix = suffix;
	return i;
}

double parse_float(size_t len, const char* str) {
	char* end;
	double i = strtod(str, &end);
	if (end != &str[len]) {
		if (*end != 'f' && *end != 'd' && *end != 'F' && *end != 'D') abort();
	}
	
	return i;
}

intptr_t parse_char(size_t len, const char* str, int* output) {
	if (str[0] != '\\') {
		*output = str[0];
		return 1;
	}
	
	// error: expected something after the backslash
	if (len < 1) return -1;
	
	int ch = 0;
	size_t i = 2;
	switch (str[1]) {
		// TODO(NeGate): Implement the rest of the C char variants
		// \U0001f34c
		case '0' ... '9': {
			unsigned int num = 0;
			
			while (i < len) {
				char ch = str[i];
				if (!(ch >= '0' && ch <= '9')) break;
				
				num *= 10;
				num += (ch - '0');
				i += 1;
			}
			
			ch = num;
			break;
		}
		case 'x': case 'X': {
			unsigned int num = 0;
			
			while (i < len) {
				char ch = str[i];
				
				if (ch >= 'A' && ch <= 'F') {
					num <<= 4;
					num |= (ch - 'A') + 0xA;
				} else if (ch >= 'a' && ch <= 'f') {
					num <<= 4;
					num |= (ch - 'a') + 0xA;
				} else if (ch >= '0' && ch <= '9') {
					num <<= 4;
					num |= (ch - '0');
				} else break;
				
				i += 1;
			}
			
			ch = num;
			break;
		}
		case '\\': ch = '\\'; break;
		case 'a': ch = '\a'; break;
		case 'b': ch = '\b'; break;
		case 't': ch = '\t'; break;
		case 'n': ch = '\n'; break;
		case 'v': ch = '\v'; break;
		case 'f': ch = '\f'; break;
		case 'r': ch = '\r'; break;
		case '\'': ch = '\''; break;
		case '\"': ch = '\"'; break;
		default: return -1;
	}
	
	*output = ch;
	return i;
}
