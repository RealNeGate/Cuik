#pragma once
#include "common.h"

// NOTE(NeGate): I originally called it TokenType but windows a bih on god
typedef enum TknType {
    TOKEN_ACCESSOR = '.',
    
    TOKEN_PLUS = '+',
    TOKEN_MINUS = '-',
    TOKEN_TIMES = '*',
    TOKEN_SLASH = '/',
    TOKEN_PERCENT = '%',
    TOKEN_ASSIGN = '=',
	
    TOKEN_HASH = '#',
    
    TOKEN_LESS = '<',
    TOKEN_GREATER = '>',
	
    TOKEN_BRACKET_OPEN = '[',
    TOKEN_BRACKET_CLOSE = ']',
    
    TOKEN_PAREN_OPEN = '(',
    TOKEN_PAREN_CLOSE = ')',
    
    TOKEN_BRACE_OPEN = '{',
    TOKEN_BRACE_CLOSE = '}',
    
    TOKEN_IDENTIFIER = 256,
	TOKEN_NUMBER,
    TOKEN_STRING,
    
    TOKEN_INVALID,
	
    TOKEN_INCREMENT,                  /* ++  */
    TOKEN_DECREMENT,                  /* --  */
    TOKEN_LEFT_SHIFT,                 /* <<  */
    TOKEN_RIGHT_SHIFT,                /* >>  */
	TOKEN_LEFT_SHIFT_EQUAL,           /* >>= */
	TOKEN_RIGHT_SHIFT_EQUAL,          /* <<= */
    TOKEN_ARROW,                      /* ->  */
    TOKEN_DOUBLE_HASH   = '#' + 256,  /* ##  */
    TOKEN_PLUS_EQUAL    = '+' + 256,  /* +=  */
    TOKEN_MINUS_EQUAL   = '-' + 256,  /* -=  */
    TOKEN_TIMES_EQUAL   = '*' + 256,  /* *=  */
    TOKEN_SLASH_EQUAL   = '/' + 256,  /* /=  */
    TOKEN_PERCENT_EQUAL = '%' + 256,  /* %=  */
    TOKEN_OR_EQUAL      = '|' + 256,  /* |=  */
    TOKEN_AND_EQUAL     = '&' + 256,  /* &=  */
    TOKEN_XOR_EQUAL     = '^' + 256,  /* ^=  */
    TOKEN_NOT_EQUAL     = '!' + 256,  /* !=  */
    TOKEN_GREATER_EQUAL = '>' + 256,  /* >=  */
    TOKEN_LESS_EQUAL    = '<' + 256,  /* <=  */
    TOKEN_EQUALITY      = '=' + 256,  /* ==  */
	
	// Keywords
	TOKEN_KW_auto = 512,
	TOKEN_KW_break,
	TOKEN_KW_case,
	TOKEN_KW_char,
	TOKEN_KW_const,
	TOKEN_KW_continue,
	TOKEN_KW_default,
	TOKEN_KW_do,
	TOKEN_KW_double,
	TOKEN_KW_else,
	TOKEN_KW_enum,
	TOKEN_KW_extern,
	TOKEN_KW_float,
	TOKEN_KW_for,
	TOKEN_KW_goto,
	TOKEN_KW_if,
	TOKEN_KW_inline,
	TOKEN_KW_int,
	TOKEN_KW_long,
	TOKEN_KW_register,
	TOKEN_KW_restrict,
	TOKEN_KW_return,
	TOKEN_KW_short,
	TOKEN_KW_signed,
	TOKEN_KW_sizeof,
	TOKEN_KW_static,
	TOKEN_KW_struct,
	TOKEN_KW_switch,
	TOKEN_KW_typedef,
	TOKEN_KW_union,
	TOKEN_KW_unsigned,
	TOKEN_KW_void,
	TOKEN_KW_volatile,
	TOKEN_KW_while,
	TOKEN_KW_Alignas,
	TOKEN_KW_Alignof,
	TOKEN_KW_Atomic,
	TOKEN_KW_Bool,
	TOKEN_KW_Complex,
	TOKEN_KW_Generic,
	TOKEN_KW_Imaginary,
	TOKEN_KW_Noreturn,
	TOKEN_KW_Static_assert,
	TOKEN_KW_Thread_local,
} TknType;

typedef struct Lexer {
    const unsigned char* start;
    const unsigned char* current;
    
    // current token info
	TknType token_type;
    const unsigned char* token_start;
    const unsigned char* token_end;
} Lexer;

void lexer_read(Lexer* restrict l);
int lexer_get_location(Lexer* restrict l);

