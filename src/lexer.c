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
	//if (*str != '_' && len > 8) return TOKEN_IDENTIFIER;
	
	// BINARY SEARCH ARRAYS
	const static uint32_t keys[64] = {
		
		0x00006669,0x00006F64,0x00726F66,0x00746E69,0x264BA4A6,0x3330259C,0x616572CD,0x616F6CDA,
		0x61D3BDC6,0x61DAD1D9,0x6275D4D0,0x64696F76,0x64CFC2C4,0x65736163,0x65736C65,0x6574E6D7,
		0x657ACFE2,0x65D6DED8,0x676E6F6C,0x696CD3D7,0x6C6968DC,0x6D756E65,0x6E67CDD8,0x6F696EE3,
		0x6F6F42CB,0x6F746F67,0x6F747561,0x6FD7AACC,0x72616863,0x726F68E7,0x736E6FD7,0x7461D7DC,
		0x7469DFD6,0x7572E8D6,0x7574D3E4,0xC2DC2C38,0xC6D8D8EA,0xCDD8DCDC,0xCFDBAFC6,0xD1CEB9C4,
		0xD5D82F27,0xD9E3DDCC,0xDBCCD9E5,0xDCCDAFC6,0xE0D4C32F,0xE4E4C332,0xE5D4AFCF,0xE8D6CEE4,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		
	};
	const static uint32_t values[64] = {
		15,7,13,17,43,44,1,12,41,6,8,31,45,2,9,11,24,28,18,16,33,10,23,29,37,14,0,36,3,22,4,25,27,26,21,40,32,30,35,39,47,5,19,34,46,42,38,20,
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
	
	// short circuit, relatively helpful
	//if (keywords[v][0] != str[0]) return TOKEN_IDENTIFIER;
	
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

void lexer_read(Lexer* restrict l) {
    const unsigned char* current = l->current;
    
	// Skip any whitespace and comments
    // branchless space skip
    current += (*current == ' ');
	
	// NOTE(NeGate): We canonicalized spaces \t \r \v
	// in the preprocessor so we don't need to handle them
	redo_lex: {
		if (*current == '\0') {
			// quit, we're done
			l->hit_line = true;
			l->token_type = '\0';
			return;
		} else if (*current == '\n') {
			l->hit_line = true;
			
			// Do a branchless SIMD skip of up to 16 tabs after a newline.
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\n'))));
            current += len - 1;
			l->current_line += len - 1;
            goto redo_lex;
		} else if (*current == '\r') {
			l->hit_line = true;
			
			// Do a branchless SIMD skip of up to 16 tabs after a newline.
            __m128i chars = _mm_loadu_si128((__m128i *)current);
			__m128i mask = _mm_cmpeq_epi8(chars, _mm_set1_epi8('\r'));
			mask = _mm_or_si128(mask, _mm_cmpeq_epi8(chars, _mm_set1_epi8('\n')));
			
            int len = __builtin_ffs(~_mm_movemask_epi8(mask));
            current += len - 1;
			l->current_line += (len - 1) / 2;
            goto redo_lex;
		} else if (*current == ' ' || *current == '\t') {
			// slow path
			do { current++; } while (is_space(*current));
			goto redo_lex;
		} else if (*current == '/') {
			if (current[1] == '/') {
				do { current++; } while (*current && *current != '\n');
				
				l->hit_line = true;
				goto redo_lex;
			} else if (current[1] == '*') {
				current++;
				
				do { current++; } while (*current && !(current[0] == '/' && current[-1] == '*'));
				
				current++;
				l->hit_line = true;
				goto redo_lex;
			}
		}
	}
	
	////////////////////////////////
	// Try to actually parse a token
	////////////////////////////////
    const unsigned char* start = current;
	uint8_t initial_class = char_classes[*current++];
	
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
				
				if (*current == '.') {
					// TODO(NeGate): floats
					abort();
				}
				
				l->token_type = TOKEN_INTEGER;
			} else {
				while (char_classes[*current] == CHAR_CLASS_NUMBER) { current++; }
				l->token_type = TOKEN_INTEGER;
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
			char quote_type = *start;
			__m128i pattern = _mm_set1_epi8(quote_type);
			
			do {
				__m128i chars = _mm_loadu_si128((__m128i *)current);
				int len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(chars, pattern)));
				
				if (len) {
					current += len;
					if (current[-1] == quote_type && current[-2] != '\\') break;
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
	
	l->token_start = start;
	l->token_end = current;
	l->current = current;
}

int64_t parse_int(size_t len, const char* str) {
	char* end;
	int64_t i = strtol(str, &end, 0);
	if (end != &str[len]) abort();
	
	return i;
}
