#pragma once
#include "common.h"
#include "memory.h"
#include "arena.h"
#include "lexer.h"
#include "atoms.h"
#include "settings.h"
#include "big_array.h"
#include "diagnostic.h"
#include <ext/threads.h>
#include <ext/threadpool.h>

#include <back/tb.h>

#define MAX_LOCAL_SYMBOLS (1 << 20)
#define MAX_LOCAL_TAGS    (1 << 16)

typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct Type Type;

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

	// these are inferred as typedefs but don't map to anything yet
	KIND_PLACEHOLDER,

	// weird typeof(expr) type that gets resolved in the semantics pass
	// this is done to enable typeof to work with out of order decls...
	// it's a mess but it's worth it
	KIND_TYPEOF
} TypeKind;

// Used by unions and structs
typedef struct {
	Type* type;
	Atom name;

	SourceLocIndex loc;
	int align;
	int offset;

	// Bitfield
	int bit_offset;
	int bit_width;
	bool is_bitfield;
} Member;

typedef struct {
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

typedef struct {
	Atom key;

	// lexer_pos is non-zero if the enum value has it's compilation delayed
	int lexer_pos;
	int value;
} EnumEntry;

typedef struct {
	Type* type;
	Atom name;
} Param;

typedef struct {
	Type* key;
	Expr* value;
} C11GenericEntry;

struct Type {
    TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
	SourceLocIndex loc;

    // used by cycle checking
    int ordinal;

	bool is_const : 1;
    bool is_atomic : 1;
	bool is_incomplete : 1;
	bool is_inprogress : 1;

    union {
        // Integers
        bool is_unsigned;

        // Arrays
		struct {
			Type* array_of;
			int array_count;

            // if non-zero, then we must execute an expression
            // parser to resolve it
            int array_count_lexer_pos;
		};

        // Pointers
		struct {
			Type* ptr_to;
            bool is_ptr_restrict : 1;
		};

		// Function
		struct {
			Atom name;
			Type* return_type;
			size_t param_count;
			Param* param_list;

			bool has_varargs : 1;
		} func;

		// Structs/Unions
		struct {
			Atom name;

			int kid_count;
			Member* kids;
		} record;

		// Enumerators
		struct {
			Atom name;
			int count;

			EnumEntry* entries;
		} enumerator;

		struct {
			int count;
			Type* base;
		} vector_;

		// Typeof
		struct {
			Expr* src;
		} typeof_;

		struct {
			Atom name;
		} placeholder;
    };
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

	EXPR_WCHAR,
	EXPR_CHAR,
	EXPR_WSTR,
	EXPR_STR,

	EXPR_BUILTIN_SYMBOL,
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
	EXPR_DOT_R,
	EXPR_ARROW_R,
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

struct Stmt {
	struct StmtHeader {
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
	};
	union {
		struct StmtCompound {
			Stmt** kids;
			int kids_count;
		} compound;
		struct StmtExpr {
			Expr* expr;
		} expr;
		struct StmtReturn {
			Expr* expr;
		} return_;
		struct StmtContinue {
			Stmt* target; // loop
		} continue_;
		struct StmtBreak {
			Stmt* target; // either a loop or switch
		} break_;
		struct StmtGoto {
			Expr* target;
		} goto_;
		struct StmtLabel {
			Atom name;
			bool placed;
		} label;
		struct StmtCase {
			int64_t key;
			Stmt* body;
			Stmt* next;
		} case_;
		struct StmtDefault {
			Stmt* body;
			Stmt* next;
		} default_;
		struct StmtSwitch {
			Expr* condition;
			Stmt* body;

			// points to the first case or default,
			// and those point to next and so on,
			// linked lists
			Stmt* next;
		} switch_;
		struct StmtDecl {
			Type* type;
			Atom name;

			// acceleration structure for scrubbing for symbols
			// it's a linked list
			Expr* first_symbol;

			// NOTE(NeGate): This represents a stmtindex if it's a
			// FUNC_DECL
			union {
				Stmt* initial_as_stmt;
				Expr* initial;
			};

			Attribs attrs;
		} decl;
		struct StmtFor {
			Stmt* first;
			Expr* cond;
			Stmt* body;
			Expr* next;
		} for_;
		struct StmtWhile {
			Expr* cond;
			Stmt* body;
		} while_;
		struct StmtDoWhile {
			Expr* cond;
			Stmt* body;
		} do_while;
		struct StmtIf {
			Expr* cond;
			Stmt* body;
			Stmt* next;
		} if_;
	};
};

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

	// Fully resolved members with a kid_count of 0 will have
	// a proper offset and type after the type checking
	uint32_t offset;
	Type* type;

	Expr* expr;
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

struct Expr {
	ExprOp op;
	SourceLocIndex loc;

	Type* type;

	// this is the type it'll be desugared into
	// for example:
	//
	//   a + b where a and b are 16bit
	//
	//   their cast_type might be int because of C's
	//   promotion rules.
	Type* cast_type;

	union {
		struct {
			Atom name;

			// aliases with next_symbol_in_chain
			Expr* next_symbol_in_chain;
		} builtin_sym;

		struct {
			Atom unknown_sym;

			// aliases with next_symbol_in_chain
			Expr* next_symbol_in_chain2;
		};

		// TODO(NeGate): rename this unnamed struct to 'symbol'
		struct {
			// linked list of symbols within a function used to
			// analyze used symbols more easily
			Stmt* symbol;
			Expr* next_symbol_in_chain;

			// are we currently iterating a symbol with is_resolving_symbol
			bool  is_resolving_symbol;
		};

		// EXPR_PARAM
		int param_num;

		struct ExprEnum {
			int* num;
			Expr* next_symbol_in_chain;
		} enum_val;
		struct {
			Type* type;
			Expr* src;
		} cast;
		struct {
			Expr* left;
			Expr* middle;
			Expr* right;
		} ternary_op;
		struct {
			Expr* left;
			Expr* right;
		} bin_op;
		struct {
			Expr* base;
			Expr* index;
		} subscript;
		struct {
			Expr* src;
		} unary_op;
		struct {
			Expr* controlling_expr;

			// if case_count == 0, then controlling_expr is the matched expression
			int case_count;
			C11GenericEntry* cases;
		} generic_;
		struct {
			Expr* base;
			uint32_t offset;

			// during semantics we'll switch between DOT or ARROW to DOT_R or ARROW_R
			// which means we use the member field
			union {
				Member* member;
				Atom name;
			};
		} dot_arrow;
		struct {
			Expr* target;
			int param_count;

			Expr** param_start;
		} call;
		// represent both quoted literals
		struct {
			const unsigned char* start;
			const unsigned char* end;
		} str;
		// either sizeof(T) or _Alignof(T)
		struct {
			Type* type;
		} x_of_type;
		// either sizeof(expr) or _Alignof(expr)
		struct {
			Expr* expr;
		} x_of_expr;
		struct {
			Type* type;
			int count;
			InitNode* nodes;
		} init;
		struct {
			Stmt* src;
		} func;

		int char_lit;
		double float_num;
		struct ExprInt {
			unsigned long long num;
			IntSuffix suffix;
		} int_num;
	};
};
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, next_symbol_in_chain2), "these should be aliasing");
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, enum_val.next_symbol_in_chain), "these should be aliasing");
_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, builtin_sym.next_symbol_in_chain), "these should be aliasing");

typedef struct Decl {
	Type* type;
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
	STORAGE_LOCAL,
	STORAGE_ENUM,
	STORAGE_TYPEDEF
} StorageClass;

typedef struct Symbol {
	Atom name;
	Type* type;
	SourceLocIndex loc;
	StorageClass storage_class;

	union {
		// used if storage_class == STORAGE_PARAM
		int param_num;

		// used if storage_class == STORAGE_ENUM, refers to an index in the entries table
		int enum_value;

		Stmt* stmt;
	};

	// when handling global symbols and delaying their parsing
	// we want to be able to store what the position of the symbol's
	// "value" aka function bodies and intitial expressions
	int current;
	int terminator;
} Symbol;

typedef struct {
	Atom key;
	Stmt* value;
} ExportedSymbolEntry;

typedef struct TranslationUnit {
	// circular references amirite...
	struct CompilationUnit* parent;

	const char* filepath;

	// chain of TUs for the compilation unit
	struct TranslationUnit* next;

	atomic_int id_gen;
	bool is_free;

	// token stream
	TokenStream tokens;

	mtx_t arena_mutex;
	Arena ast_arena;
	Arena type_arena;

	// stb_ds array
	// NOTE(NeGate): should this be an stb_ds array?
	Stmt** top_level_stmts;

	// this is a bit of a hack to implement the struct printing
	// functionality, if a name is passed into hack.name then it'll
	// try to find a type by that name and feed it into hack.type
	struct {
		const char* name;
		Type*   type;
	} hack;
} TranslationUnit;

// builtin types at the start of the type table
enum {
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
	BUILTIN_TYPE_COUNT,
};
extern Type builtin_types[BUILTIN_TYPE_COUNT];

Type* new_func(TranslationUnit* tu);
Type* new_enum(TranslationUnit* tu);
Type* new_blank_type(TranslationUnit* tu);
Type* new_record(TranslationUnit* tu, bool is_union);
Type* copy_type(TranslationUnit* tu, Type* base);
Type* new_pointer(TranslationUnit* tu, Type* base);
Type* new_typeof(TranslationUnit* tu, Expr* src);
Type* new_array(TranslationUnit* tu, Type* base, int count);
Type* new_vector(TranslationUnit* tu, Type* base, int count);
Type* get_common_type(TranslationUnit* tu, Type* ty1, Type* ty2);
bool type_equal(TranslationUnit* tu, Type* a, Type* b);
size_t type_as_string(TranslationUnit* tu, size_t max_len, char* buffer, Type* type_index);

void type_layout(TranslationUnit* restrict tu, Type* type);

Stmt* resolve_unknown_symbol(TranslationUnit* tu, Expr* e);
ConstValue const_eval(TranslationUnit* tu, const Expr* e);
bool const_eval_try_offsetof_hack(TranslationUnit* tu, const Expr* e, uint64_t* out);

// thread_pool will eventually be NULLable which means the parsing is singlethreaded
void translation_unit_parse(TranslationUnit* restrict tu, const char* filepath, threadpool_t* thread_pool);
void translation_unit_deinit(TranslationUnit* tu);
