#include "lexer.h"
#include <x86intrin.h>

const char keywords[][16] = {
	"alignof",
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

static uint32_t fnv1a_len(const void* data, size_t len) {
	const unsigned char* ptr = (const unsigned char*)data;
	uint32_t hash = 0x811C9DC5;
	
	size_t i = len;
	while (i--) {
		hash = ((*ptr++) ^ hash) * 0x01000193;
	}
	
	return hash;
}

TokenType classify_ident(const char* restrict str, size_t len) {
	// BINARY SEARCH ARRAYS
	const static uint32_t keys[64] = {
		0x0C547726,0x0DC628CE,0x10D4792F,0x18338D5E,0x1D0E8DBE,0x221EDE24,0x2D6871C0,0x39386E06,
		0x3974EFA7,0x48B5725F,0x505E61EF,0x621CD814,0x664FD1D4,0x6AA4A81B,0x6EE13AFD,0x816CB000,
		0x85EE37BF,0x9087DDB7,0x923FA396,0x92C2BE20,0x933B5BDE,0x93E05F71,0x94E1036D,0x95E97E5E,
		0x9B2538B1,0xA0EB0F08,0xA6C45D85,0xA84C031D,0xACF38390,0xAE76E4A2,0xB1727E44,0xB5712015,
		0xBA226BD5,0xBDBF5BF0,0xC2CB5034,0xC2ECDF53,0xC919731F,0xC9648178,0xD290C23B,0xDBDED6F4,
		0xE3F06707,0xF5A30FE6,0xFDD57AE5,0xFF2BDAB7,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,
		0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF,	
	};
	const static uint8_t values[64] = {
		31,34,39,38,43,29,20,16,41,32,42,8,5,35,25,11,22,12,1,27,7,28,33,18,3,9,13,4,14,37,6,24,23,10,17,19,0,2,26,30,40,15,21,36,
	};
	
	// HASH STRING
	uint32_t n = fnv1a_len(str, len);
	
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
	
	// NOTE(NeGate): Fancy x86 strcmp crap :)
	int result = _mm_cmpestri(kw128, len,
							  str128, len,
							  _SIDD_UBYTE_OPS |
							  _SIDD_CMP_EQUAL_EACH |
							  _SIDD_POSITIVE_POLARITY);
	
	return result ? TOKEN_IDENTIFIER : (512 + v);
}

void lexer_read(Lexer* restrict l) {
    const char* current = l->current;
    
    // branchless space skip
    current += (*current == ' ');
    
    redo_lex:;
    const char* start = current;
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
            current++;
			
            // Do a branchless SIMD skip of up to 16 tabs after a newline.
            __m128i chars = _mm_loadu_si128((__m128i *)current);
            int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8('\t'))));
            current += (len ? len - 1 : 0);
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
			do {
				current++;
			} while (is_identifier_not_first_char(*((unsigned char*)current)));
			
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
	int num = 0;
	while (*l->current) {
		num += (*l->current == '\n');
		l->current++;
	}
	return num;
}
