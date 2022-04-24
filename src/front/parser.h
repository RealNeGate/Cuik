#pragma once
#include "common.h"
#include "memory.h"
#include "arena.h"
#include "lexer.h"
#include "atoms.h"
#include "settings.h"
#include "big_array.h"
#include "diagnostic.h"

#include <back/tb.h>

#define MAX_LOCAL_SYMBOLS (1 << 20)
#define MAX_TYPEDEF_LOCAL_SYMBOLS (1024)

// Members used by struct/union types
// NOTE(NeGate): Consider using an arena for this
typedef int MemberIndex;

typedef int TypeIndex;
typedef int ParamIndex;
typedef int EnumEntryIndex;
typedef int StmtIndex;
typedef int ExprIndex;

typedef int SymbolIndex;

typedef enum TypeKind {
    KIND_VOID,
    KIND_BOOL,
    KIND_CHAR,
    KIND_SHORT,
    KIND_INT,
    KIND_ENUM,
    KIND_LONG,
    KIND_FLOAT,
    KIND_DOUBLE,
    KIND_PTR,
    KIND_FUNC,
    KIND_ARRAY,
    KIND_VLA, // variable-length array
    KIND_STRUCT,
    KIND_UNION,
	KIND_VECTOR,
	
	// weird typeof(expr) type that gets resolved in the semantics pass
	// this is done to enable typeof to work with out of order decls...
	// it's a mess but it's worth it
	KIND_TYPEOF
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
	bool is_root    : 1;
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

typedef struct {
	TypeIndex key;
	ExprIndex value;
} C11GenericEntry;

typedef struct Type {
    TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
	SourceLocIndex loc;

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
		struct {
			TypeIndex ptr_to;
			bool is_ptr_restrict : 1;
		};
		
		// Function
		struct {
			Atom name;
			TypeIndex return_type;
			ParamIndex param_list, param_count;
			
			bool has_varargs : 1;
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
	
		struct {
			TypeIndex base;
			int       count;
		} vector_;

		// Typeof
		struct {
			ExprIndex src;
		} typeof_;
    };
} Type;

// builtin types at the start of the type table
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
	EXPR_ENUM,
	EXPR_FLOAT32,
	EXPR_FLOAT64,
	
	EXPR_CHAR,
	EXPR_STR,
	
	EXPR_UNKNOWN_SYMBOL,
	EXPR_SYMBOL,
	EXPR_GENERIC, // C11's _Generic

	EXPR_FUNCTION, // function literal
	EXPR_INITIALIZER,
	
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
	
	// these are resolved by semantics pass
	EXPR_PTRADD,
	EXPR_PTRSUB,
	EXPR_PTRDIFF,
	
	EXPR_TERNARY,
	EXPR_COMMA,
	
	EXPR_CMPEQ,
	EXPR_CMPNE,
	EXPR_CMPGE,
	EXPR_CMPLE,
	EXPR_CMPGT,
	EXPR_CMPLT,
	
	EXPR_LOGICAL_NOT,
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
			TypeIndex type;
			
			// NOTE(NeGate): This represents a stmtindex if it's a 
			// FUNC_DECL
			ExprIndex initial;
			
			// acceleration structure for scrubbing for symbols
			// it's a linked list
			ExprIndex first_symbol;
			
			Attribs attrs;
			Atom name;
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
		struct {
			Atom unknown_sym;
			
			// aliases with next_symbol_in_chain
			ExprIndex next_symbol_in_chain2;
		};
		
		// TODO(NeGate): rename this unnamed struct to 'symbol'
		struct {
			// linked list of symbols within a function used to 
			// analyze used symbols more easily
			StmtIndex symbol;
			uint32_t _;
			
			ExprIndex next_symbol_in_chain;
		};
		
		// EXPR_PARAM
		int param_num;
		
		struct ExprEnum {
			int64_t num;
			ExprIndex next_symbol_in_chain;
		} enum_val;
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
			ExprIndex controlling_expr;
			
			// if case_count == 0, then controlling_expr is the matched expression
			int case_count;
			C11GenericEntry* cases;
		} generic_;
		struct {
			ExprIndex base;
			// once the semantic pass runs over the AST
			// we'll have proper indices to access the Member
			union {
				// stinky...
				struct {
					MemberIndex member;
					uint32_t offset;
				};
				Atom name;
			};
		} dot_arrow;
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
		struct {
			StmtIndex src;
		} func;
		
		double float_num;
		struct ExprInt {
			unsigned long long num;
			IntSuffix suffix;
		} int_num;
	};
} Expr;
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, next_symbol_in_chain2), "these should be aliasing");
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, enum_val.next_symbol_in_chain), "these should be aliasing");
_Static_assert(sizeof(Expr) <= 32, "We shouldn't exceed 32 bytes");

typedef struct Decl {
	TypeIndex type;
	Atom name;
	SourceLocIndex loc;
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

// these represent exported symbols which may or may not point
// to either functions internal to the compilation unit or outside
// of it.
typedef struct {
	struct TranslationUnit* tu;
	StmtIndex stmt;
} ExportedSymbol;

typedef struct {
	Atom key;
	ExportedSymbol value;
} ExportedSymbolEntry;

typedef struct TranslationUnit {
	// circular references amirite...
	struct CompilationUnit* parent;
	
	const char* filepath;
	
	// chain of TUs for the compilation unit
	struct TranslationUnit* next;
	bool is_free;
	
	// token stream
	TokenStream tokens;
	
	// TODO(NeGate): keep track of all files loaded by this TU
	// so that we can properly free them
	BigArray(Type) types;
	BigArray(Member) members;
	BigArray(Param) params;
	BigArray(EnumEntry) enum_entries;
	BigArray(Stmt) stmts;
	BigArray(Expr) exprs;
	
	// stb_ds array
	// NOTE(NeGate): should this be an stb_ds array?
	StmtIndex* top_level_stmts;

	// this is a bit of a hack to implement the struct printing
	// functionality, if a name is passed into hack.name then it'll
	// try to find a type by that name and feed it into hack.type
	struct {
		const char* name;
		TypeIndex   type;
	} hack;
} TranslationUnit;

TypeIndex new_func(TranslationUnit* tu);
TypeIndex new_enum(TranslationUnit* tu);
TypeIndex new_record(TranslationUnit* tu, bool is_union);
TypeIndex copy_type(TranslationUnit* tu, TypeIndex base);
TypeIndex new_pointer(TranslationUnit* tu, TypeIndex base);
TypeIndex new_typeof(TranslationUnit* tu, ExprIndex src);
TypeIndex new_array(TranslationUnit* tu, TypeIndex base, int count);
TypeIndex new_vector(TranslationUnit* tu, TypeIndex base, int count);
TypeIndex get_common_type(TranslationUnit* tu, TypeIndex ty1, TypeIndex ty2);
bool type_equal(TranslationUnit* tu, TypeIndex a, TypeIndex b);
size_t type_as_string(TranslationUnit* tu, size_t max_len, char* buffer, TypeIndex type_index);

StmtIndex resolve_unknown_symbol(TranslationUnit* tu, StmtIndex i);
ConstValue const_eval(TranslationUnit* tu, ExprIndex e);
bool const_eval_try_offsetof_hack(TranslationUnit* tu, ExprIndex e, uint64_t* out);

void init_types(TranslationUnit* tu);

void translation_unit_parse(TranslationUnit* restrict tu, const char* filepath);
void translation_unit_deinit(TranslationUnit* tu);
