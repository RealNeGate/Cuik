#pragma once
#include "common.h"
#include "memory.h"
#include "arena.h"
#include "lexer.h"
#include "atoms.h"
#include "tb/tb.h"

decl_arena_index(Type, type_arena)
decl_arena_index(Member, member_arena) // Members used by struct/union types
decl_arena_index(Arg, arg_arena) // refs to Types used by the function types

decl_arena_index(Stmt, stmt_arena)
decl_arena_index(StmtIndex, stmt_ref_arena)
decl_arena_index(Expr, expr_arena)
decl_arena_index(ExprIndex, expr_ref_arena)

typedef int SymbolIndex;

typedef enum TypeKind {
    KIND_VOID,
    KIND_BOOL,
    KIND_CHAR,
    KIND_SHORT,
    KIND_INT,
    KIND_LONG,
    KIND_FLOAT,
    KIND_DOUBLE,
    KIND_ENUM,
    KIND_PTR,
    KIND_FUNC,
    KIND_ARRAY,
    KIND_VLA, // variable-length array
    KIND_STRUCT,
    KIND_UNION
} TypeKind;

// Used by unions and structs
typedef struct Member {
	TypeIndex type;
	Atom name;
	
	int align;
	int offset;
	
	// Bitfield
	int bit_offset;
	int bit_width;
	bool is_bitfield;
} Member;

typedef struct Attribs {
	bool is_static  : 1;
	bool is_typedef : 1;
	bool is_inline  : 1;
	bool is_extern  : 1;
	bool is_tls     : 1;
} Attribs;

typedef struct Arg {
	TypeIndex type;
	Atom name;
} Arg;

typedef struct Type {
    TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
	
	bool is_const : 1;
    bool is_atomic : 1;
    
    union {
        // Integers
        bool is_unsigned;
        
        // Arrays
		TypeIndex array_of;
		
        // Pointers
		TypeIndex ptr_to;
		
		// Function
		struct {
			Atom name;
			TypeIndex return_type;
			ArgIndex arg_start, arg_end;
			
			// TODO(NeGate): Attributes
		} func;
		
		// Structs/Unions
		struct {
			Atom name;
			MemberIndex kids_start, kids_end;
		} record;
    };
} Type;

enum {
	TYPE_NONE,
	
	TYPE_VOID,
	TYPE_BOOL,
	TYPE_CHAR,
	TYPE_SHORT,
	TYPE_INT,
	TYPE_LONG,
	TYPE_UCHAR,
	TYPE_USHORT,
	TYPE_UINT,
	TYPE_ULONG,
	TYPE_FLOAT,
	TYPE_DOUBLE
};

// NOTE(NeGate): for loops are actually two nodes right next to each other
// it's a STMT_FOR and STMT_FOR2 where FOR stores the init and cond respectively,
// then FOR2 stores the next and body statements respectively.
typedef enum StmtOp {
	STMT_NONE,
	
	STMT_COMPOUND,
	STMT_DECL,
	
	// NOTE(NeGate): Always followed by a compound block
	STMT_FUNC_DECL,
	STMT_EXPR,
	
	STMT_IF,
	STMT_DO_WHILE,
	
	STMT_FOR,
	STMT_FOR2,
	
	STMT_WHILE,
	
	STMT_RETURN
} StmtOp;

typedef enum ExprOp {
	EXPR_NONE,
	
	EXPR_NUM,
	
	EXPR_UNKNOWN_SYMBOL,
	EXPR_SYMBOL,
	
	EXPR_CAST,
	EXPR_PARAM, // special case of EXPR_VAR
	EXPR_ASSIGN,
	EXPR_PLUS_ASSIGN,
	EXPR_MINUS_ASSIGN,
	EXPR_TIMES_ASSIGN,
	EXPR_SLASH_ASSIGN,
	EXPR_PERCENT_ASSIGN,
	EXPR_AND_ASSIGN,
	EXPR_OR_ASSIGN,
	EXPR_XOR_ASSIGN,
	EXPR_SHL_ASSIGN,
	EXPR_SHR_ASSIGN,
	
	EXPR_PLUS,
	EXPR_MINUS,
	EXPR_TIMES,
	EXPR_SLASH,
	EXPR_PERCENT,
	EXPR_AND,
	EXPR_OR,
	EXPR_XOR,
	EXPR_SHL,
	EXPR_SHR,
	
	EXPR_CMPEQ,
	EXPR_CMPNE,
	EXPR_CMPGE,
	EXPR_CMPLE,
	EXPR_CMPGT,
	EXPR_CMPLT,
	
	EXPR_LOGICAL_AND,
	EXPR_LOGICAL_OR,
	
	EXPR_DEREF,
	EXPR_ADDR,
	EXPR_SUBSCRIPT,
	EXPR_DOT,
	EXPR_CALL,
	
	EXPR_PRE_INC,
	EXPR_PRE_DEC,
	EXPR_POST_INC,
	EXPR_POST_DEC,
} ExprOp;

typedef struct Stmt {
	StmtOp op;
	// STMT_EXPR, STMT_DECL, STMT_RETURN
	ExprIndex expr;
	// TODO(NeGate): const char* pos;
	union {
		struct {
			// STMT_DECL or STMT_FUNC_DECL
			Attribs attrs;
			TypeIndex decl_type : 24;
			Atom decl_name;
		};
		struct {
			// STMT_IF, STMT_FOR, STMT_WHILE, STMT_DO_WHILE
			// STMT_DECL if it's a function
			StmtIndex body;
			
			// STMT_IF, STMT_FOR
			StmtIndex body2;
		};
		struct {
			// STMT_COMPOUND
			StmtIndex kids_start;
			StmtIndex kids_end;
		};
	};
	
	union {
		TB_Register r;
		TB_Function* f;
		TB_ExternalID e;
	} backing;
} Stmt;

typedef struct Expr {
	ExprOp op;
	int line;
	
	union {
		Atom unknown_sym;
		StmtIndex symbol;
		
		// EXPR_PARAM
		int param_num;
		
		struct {
			TypeIndex type;
			ExprIndex src;
		} cast;
		struct {
			ExprIndex left, right;
		} bin_op;
		struct {
			ExprIndex base, index;
		} subscript;
		struct {
			ExprIndex src;
		} unary_op;
		struct {
			ExprIndex base;
			Atom name;
		} dot;
		struct {
			ExprIndex target;
			ExprIndexIndex param_start, param_end;
		} call;
		long long num;
	};
} Expr;

typedef struct Decl {
	TypeIndex type;
	Atom name;
} Decl;

typedef enum StorageClass {
	STORAGE_NONE,
	
	STORAGE_STATIC_FUNC,  // .text
	STORAGE_STATIC_VAR,   // .data
	STORAGE_STATIC_CONST, // .rdata
	
	STORAGE_FUNC,
	STORAGE_PARAM,
	STORAGE_LOCAL
} StorageClass;

typedef struct Symbol {
	Atom name;
	TypeIndex type : 24;
	StorageClass storage_class : 8;
	
	union {
		// used if storage_class == STORAGE_PARAM
		int param_num;
		StmtIndex stmt;
	};
} Symbol;

decl_arena(Type, type_arena)
decl_arena(Member, member_arena)
decl_arena(Arg, arg_arena)
decl_arena(Stmt, stmt_arena)
decl_arena(StmtIndex, stmt_ref_arena)
decl_arena(Expr, expr_arena)
decl_arena(ExprIndex, expr_ref_arena)

TypeIndex new_func();
TypeIndex new_struct();
TypeIndex copy_type(TypeIndex base);
TypeIndex new_pointer(TypeIndex base);
TypeIndex new_array(TypeIndex base, int count);
TypeIndex get_common_type(TypeIndex ty1, TypeIndex ty2);

typedef struct TopLevel {
	// stb_ds array
	StmtIndex* arr;
} TopLevel;

TopLevel parse_file(TokenStream* restrict s);
