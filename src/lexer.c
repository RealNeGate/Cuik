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
	"_Noreturn",
	"_Static_assert",
	"_Thread_local"
};

// AUTO-GENERATED
// A-Z a-z 0-9 _
static _Alignas(64) uint8_t identifier_char_tbl[] = {
	0x00,0x00,0x00,0x00,
	0x00,0x00,0xff,0x03,
	0xfe,0xff,0xff,0x87,
	0xfe,0xff,0xff,0x07,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,
};

__attribute__((always_inline))
static bool is_identifier_not_first_char(char ch) {
	size_t i = ch;
	size_t index = i / 8;
	size_t shift = i & 7;
	return (identifier_char_tbl[index] >> shift) & 1;
}

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
static bool is_space(char ch) {
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
		0x61DAD1D9,0x6275D4D0,0x64696F76,0x65736163,0x65736C65,0x6574E6D7,0x657ACFE2,0x65D6DED8,
		0x676E6F6C,0x696CD3D7,0x6C6968DC,0x6D756E65,0x6E67CDD8,0x6F696EE3,0x6F6F42CB,0x6F746F67,
		0x6F747561,0x6FD7AACC,0x72616863,0x726F68E7,0x736E6FD7,0x7461D7DC,0x7469DFD6,0x7572E8D6,
		0x7574D3E4,0xC2DC2C38,0xC6D8D8EA,0xCDD8DCDC,0xCFDBAFC6,0xD1CEB9C4,0xD9E3DDCC,0xDBCCD9E5,
		0xDCCDAFC6,0xE4E4C332,0xE5D4AFCF,0xE8D6CEE4,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		
	};
	const static uint32_t values[64] = {
		15,7,13,17,42,43,1,12,6,8,31,2,9,11,24,28,18,16,33,10,23,29,37,14,0,36,3,22,4,25,27,26,21,40,32,30,35,39,5,19,34,41,38,20,
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
	if (keywords[v][0] != str[0]) return TOKEN_IDENTIFIER;
	
	// VERIFY
	__m128i kw128  = _mm_loadu_si128((__m128i*) &keywords[v]);
	__m128i str128 = _mm_loadu_si128((__m128i*) str);
	
	// NOTE(NeGate): Fancy x86 strcmp crap :)
	int result = _mm_cmpestri(kw128, len,
							  str128, len,
							  _SIDD_UBYTE_OPS |
							  _SIDD_CMP_EQUAL_ANY | 
							  _SIDD_NEGATIVE_POLARITY |
							  _SIDD_BIT_MASK);
	
	return result == len ? (512 + v) : TOKEN_IDENTIFIER;
}

void lexer_read(Lexer* restrict l) {
    const unsigned char* current = l->current;
    
    // branchless space skip
    current += (*current == ' ');
	
    redo_lex:;
    const unsigned char* start = current;
    switch (*start) {
        case '\0':
        l->token_type = '\0';
        break;
        case '\r':
        current++;
        
        // it's expected these are next to each other because
        // Windows, fast path a fallthrough
        if (*current != '\n') goto redo_lex;
        case '\n': {
            // Do a branchless SIMD skip of up to 16 tabs after a newline.
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\n'))));
            current += len - 1;
            goto redo_lex;
        }
        case ' ':
        case '\t':
        case '\v':
        // slow path
        do {
            current++;
        } while (is_space(*current));
        goto redo_lex;
        case '\"':
        current++;
		
        do {
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\"'))));
            
            if (len) {
                current += len;
                if (current[-1] == '\"' && current[-2] != '\\') break;
            } else {
                current += 16;
            }
        } while (*current);
        
        l->token_type = TOKEN_STRING;
        break;
        case '+':
        current++;
        
        if (*current == '+') {
            current++;
            l->token_type = TOKEN_INCREMENT;
            break;
        } else if (*current == '=') {
            current++;
            l->token_type = TOKEN_PLUS_EQUAL;
            break;
        }
        
        l->token_type = TOKEN_PLUS;
        break;
        case '-':
        current++;
		
        if (*current == '-') {
            current++;
            l->token_type = TOKEN_DECREMENT;
            break;
        } else if (*current == '>') {
            current++;
            l->token_type = TOKEN_ARROW;
            break;
        } else if (*current == '=') {
            current++;
            l->token_type = TOKEN_MINUS_EQUAL;
            break;
        }
        
        l->token_type = TOKEN_MINUS;
        break;
        case '>': {
			current++;
			if (*current == '>') {
				current++;
				
				if (*current == '=') {
					current++;
					l->token_type = TOKEN_RIGHT_SHIFT_EQUAL;
				} else {
					l->token_type = TOKEN_RIGHT_SHIFT;
				}
				break;
			}
			
			l->token_type = '>';
			break;
		}
		case '<': {
			current++;
			if (*current == '<') {
				current++;
				
				if (*current == '=') {
					current++;
					l->token_type = TOKEN_LEFT_SHIFT_EQUAL;
				} else {
					l->token_type = TOKEN_LEFT_SHIFT;
				}
				break;
			}
			
			l->token_type = '<';
			break;
		}
		case '*':
		case '/':
		case '&':
		case '%':
		case '|':
		case '^':
		case '!':
		case '=': {
			int t = *current++;
			if (*current == '=') {
				current++;
				t += 256;
			}
			
			l->token_type = t;
			break;
		}
		case '#':
		current++;
		
		if (*current == '#') {
			current++;
			l->token_type = TOKEN_DOUBLE_HASH;
			break;
		}
		
		l->token_type = TOKEN_HASH;
		goto redo_lex;
		case '~':
		case '.':
		case ';':
		case ',':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		current++;
		l->token_type = *start;
		break;
		case 'A' ... 'Z':
		case 'a' ... 'z':
		case '_': {
			__builtin_prefetch(identifier_char_tbl);
			do {
				current++;
			} while (is_identifier_not_first_char(*current));
			
			l->token_type = classify_ident(start, current - start);
			break;
		}
		case '0' ... '9':
		do {
			current++;
		} while (*current >= '0' && *current <= '9');
		l->token_type = TOKEN_NUMBER;
		break;
		default: 
		l->token_type = TOKEN_INVALID;
		abort();
		break;
	}
	
	l->token_start = start;
	l->token_end = current;
	l->current = current;
}

int lexer_get_location(Lexer* restrict l) {
	int num = 1;
	
	const unsigned char* start = l->start;
	while (start != l->current) {
		num += (*start == '\n');
		start += 1;
	}
	
	return num;
}
