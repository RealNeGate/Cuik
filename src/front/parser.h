#pragma once
#include "common.h"
#include "memory.h"
#include "arena.h"
#include "lexer.h"
#include "atoms.h"

#include <back/tb.h>

decl_arena_index(Type, type_arena)
decl_arena_index(Member, member_arena) // Members used by struct/union types
decl_arena_index(Param, param_arena)
decl_arena_index(EnumEntry, enum_entry_arena)

decl_arena_index(Stmt, stmt_arena)
decl_arena_index(Expr, expr_arena)

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
	
	// NOTE(NeGate): In all honesty, this should probably not be
	// here since it's used in cases that aren't relevant to attribs.
	// mainly STMT_DECL, it keeps track of if anyone's referenced it
	// so we can garbage collect the symbol later.
	bool is_used    : 1;
} Attribs;

typedef struct EnumEntry {
	Atom name;
	int value;
} EnumEntry;

typedef struct Param {
	TypeIndex type;
	Atom name;
} Param;

typedef struct Type {
    TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
	
	bool is_const : 1;
    bool is_atomic : 1;
	bool is_incomplete : 1;
    
    union {
        // Integers
        bool is_unsigned;
        
        // Arrays
		struct {
			TypeIndex array_of;
			size_t array_count;
		};
		
        // Pointers
		TypeIndex ptr_to;
		
		// Function
		struct {
			Atom name;
			TypeIndex return_type;
			ParamIndex param_list, param_count;
			bool has_varargs;
			
			// TODO(NeGate): Attributes
		} func;
		
		// Structs/Unions
		struct {
			Atom name;
			MemberIndex kids_start, kids_end;
		} record;
		
		// Enumerators
		struct {
			Atom name;
			EnumEntryIndex start, end;
		} enumerator;
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
	TYPE_DOUBLE,
	TYPE_STRING,
	TYPE_WSTRING
};

typedef enum StmtOp {
	STMT_NONE,
	
	STMT_COMPOUND,
	STMT_DECL,
	
	// It's a normal decl but global
	STMT_GLOBAL_DECL,
	
	// NOTE(NeGate): It's a decl that's followed by a compound block
	STMT_FUNC_DECL,
	
	STMT_LABEL,
	STMT_EXPR,
	
	STMT_IF,
	STMT_DO_WHILE,
	STMT_GOTO,
	STMT_FOR,
	STMT_WHILE,
	
	STMT_SWITCH,
	STMT_CASE,
	STMT_DEFAULT,
	
	STMT_BREAK,
	STMT_CONTINUE,
	
	STMT_RETURN
} StmtOp;

typedef enum ExprOp {
	EXPR_NONE,
	
	EXPR_INT,
	EXPR_FLOAT,
	
	EXPR_CHAR,
	EXPR_STR,
	
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
	
	EXPR_TERNARY,
	EXPR_COMMA,
	
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
	EXPR_NEGATE,
	EXPR_NOT,
	EXPR_SUBSCRIPT,
	EXPR_DOT,
	EXPR_ARROW,
	EXPR_CALL,
	
	EXPR_SIZEOF_T, // on type
	EXPR_SIZEOF, // on expr
	
	EXPR_ALIGNOF_T, // on type
	EXPR_ALIGNOF, // on expr
	
	EXPR_INITIALIZER,
	
	EXPR_PRE_INC,
	EXPR_PRE_DEC,
	EXPR_POST_INC,
	EXPR_POST_DEC,
	
	EXPR_MAX
} ExprOp;

typedef struct ConstValue {
	bool is_signed;
	union {
		intmax_t signed_value;
		uintmax_t unsigned_value;
	};
} ConstValue;

typedef struct Stmt {
	StmtOp op;
	SourceLocIndex loc;
	
	// Used by the backend for backend-y things
	union {
		TB_Register r;
		TB_FunctionID f;
		TB_ExternalID e;
		TB_GlobalID g;
		TB_Label l;
	} backing;
	
	union {
		struct StmtCompound {
			StmtIndex* kids;
			int kids_count;
		} compound;
		struct StmtExpr {
			ExprIndex expr;
		} expr;
		struct StmtReturn {
			ExprIndex expr;
		} return_;
		struct StmtContinue {
			StmtIndex target; // loop
		} continue_;
		struct StmtBreak {
			StmtIndex target; // either a loop or switch
		} break_;
		struct StmtGoto {
			ExprIndex target;
		} goto_;
		struct StmtLabel {
			Atom name;
		} label;
		struct StmtCase {
			intmax_t key;
			
			StmtIndex body;
			StmtIndex next;
		} case_;
		struct StmtDefault {
			StmtIndex body;
			StmtIndex next;
		} default_;
		struct StmtSwitch {
			ExprIndex condition;
			StmtIndex body;
			
			// points to the first case or default,
			// and those point to next and so on,
			// linked lists
			StmtIndex next;
		} switch_;
		struct StmtDecl {
			Attribs attrs;
			TypeIndex type;
			Atom name;
			ExprIndex initial;
		} decl;
		struct StmtFor {
			StmtIndex first;
			ExprIndex cond;
			StmtIndex body;
			ExprIndex next;
		} for_;
		struct StmtWhile {
			ExprIndex cond;
			StmtIndex body;
		} while_;
		struct StmtDoWhile {
			ExprIndex cond;
			StmtIndex body;
		} do_while;
		struct StmtIf {
			ExprIndex cond;
			StmtIndex body;
			StmtIndex next;
		} if_;
	};
} Stmt;

// designated initializer member
// .x = 5
// [4] = 6
typedef enum InitNodeDesignator {
	INIT_NONE,
	INIT_ARRAY,
	INIT_MEMBER
} InitNodeDesignator;

typedef struct InitNode {
	// the children are directly after
	// this node, if the value is 0
	// then there's no children and we
	// are expected to find an expression
	// here.
	int kids_count;
	ExprIndex expr;
	InitNodeDesignator mode;
	union {
		// INIT_MEMBER
		Atom member_name;
		
		// INIT_ARRAY
		struct {
			int start, count;
		};
	};
} InitNode;

typedef struct Expr {
	ExprOp op;
	SourceLocIndex loc;
	
	TypeIndex type;
	
	// this is the type it'll be desugared into
	// for example:
	//
	//   a + b where a and b are 16bit
	//
	//   their cast_type might be int because of C's
	//   promotion rules.
	TypeIndex cast_type;
	
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
			ExprIndex left, middle, right;
		} ternary_op;
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
			// once the semantic pass runs over the AST
			// we'll have proper indices to access the Member
			MemberIndex member;
			
			ExprIndex base;
			Atom name;
		} dot;
		struct {
			// once the semantic pass runs over the AST
			// we'll have proper indices to access the Member
			MemberIndex member;
			
			ExprIndex base;
			Atom name;
		} arrow;
		struct {
			ExprIndex target;
			int param_count;
			
			ExprIndex* param_start;
		} call;
		// represent both quoted literals
		struct {
			const unsigned char* start;
			const unsigned char* end;
		} str;
		// either sizeof(T) or _Alignof(T)
		struct {
			TypeIndex type;
		} x_of_type;
		// either sizeof(expr) or _Alignof(expr)
		struct {
			ExprIndex expr;
		} x_of_expr;
		struct {
			TypeIndex type;
			int count;
			InitNode* nodes;
		} init;
		
		double float_num;
		struct ExprInt {
			unsigned long long num;
			IntSuffix suffix;
		} int_num;
	};
} Expr;

_Static_assert(sizeof(Expr) <= 32, "We shouldn't exceed 32 bytes");

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
	STORAGE_GLOBAL,
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
decl_arena(Param, param_arena)
decl_arena(EnumEntry, enum_entry_arena)

decl_arena(Stmt, stmt_arena)
decl_arena(Expr, expr_arena)

TypeIndex new_func();
TypeIndex new_enum();
TypeIndex new_record(bool is_union);
TypeIndex copy_type(TypeIndex base);
TypeIndex new_pointer(TypeIndex base);
TypeIndex new_pointer_locked(TypeIndex base);
TypeIndex new_array(TypeIndex base, int count);
TypeIndex get_common_type(TypeIndex ty1, TypeIndex ty2);
bool type_equal(TypeIndex a, TypeIndex b);

typedef struct TopLevel {
	// stb_ds array
	StmtIndex* arr;
} TopLevel;

void init_types();
StmtIndex resolve_unknown_symbol(StmtIndex i);

ConstValue const_eval(ExprIndex e);

TopLevel parse_file(TokenStream* restrict s);
void print_tree(TopLevel tl);
