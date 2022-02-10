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

static unsigned char overhang_mask[16] = {
    255, 255, 255, 255, 255, 255, 255, 255,
    0,   0,   0,   0,   0,   0,   0,   0,
};

uint32_t hash_with_len(const void* data, size_t len) {
    uint32_t* p = (uint32_t*)data;
    uint32_t hash = 0;
	
    uint32_t split = len / 4;
    uint32_t split_mask = *((uint32_t*)&overhang_mask[8 - (len % 4)]);
	
    int i = 0;
    do {
        uint32_t mask = split_mask;
        if (i < split) mask = -1u;
        else if (i > split) mask = 0u;
		
        hash += p[i] & mask;
        i += 1;
    } while (i < 4);
	
    return hash;
}

TknType classify_ident(const unsigned char* restrict str, size_t len) {
	// Auto-generated with this small C program (MAKE SURE TO UPDATE THE
	// KEYWORDS ARRAY AND TOKEN TYPES)
	//
	// https://gist.github.com/RealNeGate/397db4aaace43e0499dc8f7b429ccc17
	//
	// BINARY SEARCH ARRAYS
	const static uint32_t keys[64] = {
		
		0x00006669,0x00006F64,0x00726F66,0x00746E69,0x264BA4A6,0x3330259C,0x616572CD,0x616F6CDA,
		0x61D3BDC6,0x61DAD1D9,0x6275D4D0,0x64696F76,0x64CFC2C4,0x65736163,0x65736C65,0x6574E6D7,
		0x657ACFE2,0x65D6DED8,0x676E6F6C,0x696CD3D7,0x6C6968DC,0x6D756E65,0x6E67CDD8,0x6F696EE3,
		0x6F6F42CB,0x6F746F67,0x6F747561,0x6FD7AACC,0x70DFC3C4,0x72616863,0x726F68E7,0x736E6FD7,
		0x7461D7DC,0x7469DFD6,0x7572E8D6,0x7574D3E4,0xC2DC2C38,0xC6D8D8EA,0xCDD8DCDC,0xCFDBAFC6,
		0xD1CEB9C4,0xD5D82F27,0xD9E3DDCC,0xDBCCD9E5,0xDCCDAFC6,0xE0D4C32F,0xE4E4C332,0xE5D4AFCF,
		0xE8D6CEE4,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		
	};
	const static uint32_t values[64] = {
		15,7,13,17,43,44,1,12,41,6,8,31,46,2,9,11,24,28,18,16,33,10,23,29,37,14,0,36,45,3,22,4,25,27,26,21,40,32,30,35,39,48,5,19,34,47,42,38,20,
	};
	
	// HASH STRING
	uint32_t n = hash_with_len(str, len);
	
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
					
					if (*current == 'e') {
						current++;
						if (*current == '+' || *current == '-') current++;
						
						while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
					}
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
				current += (*current == 'i'); // you can also write ui32 or just u32
				
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
					if (current[-1] == quote_type && current[-2] == '\\') continue;
					
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
		{
			size_t len = l->token_end - l->token_start;
			size_t len2 = current - start;
			
			conjoined_buffer = arena_alloc((len + len2 + 15) & ~15, 16);
			if (!conjoined_buffer) {
				printf("Lexer error: out of memory!");
				abort();
			}
			
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
		l->current = start + ((joined_string_lexer.token_end - conjoined_buffer) - (joined_string_lexer.token_end - joined_string_lexer.token_start));
	} else {
		l->token_start = start;
		l->token_end = current;
		l->current = current;
	}
}

uint64_t parse_int(size_t len, const char* str, IntSuffix* out_suffix) {
	char* end;
	uint64_t i = strtoul(str, &end, 0);
	
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
