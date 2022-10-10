#pragma once
#include "cuik.h"

typedef enum Cuik_TypeKind {
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

    // after parsing these types are removed, they exist to avoid cloning a
    // type before it's fully resolved
    KIND_QUALIFIED_TYPE,

    // these are inferred as typedefs but don't map to anything yet
    KIND_PLACEHOLDER,

    // weird typeof(expr) type that gets resolved in the semantics pass
    // this is done to enable typeof to work with out of order decls...
    // it's a mess but it's worth it
    KIND_TYPEOF
} Cuik_TypeKind;

// Used by unions and structs
typedef struct {
    Cuik_Type* type;
    Atom name;

    SourceLoc loc;
    int align;
    int offset;

    // Bitfield
    int bit_offset;
    int bit_width;
    bool is_bitfield;
} Member;

typedef struct {
    bool is_static : 1;
    bool is_typedef : 1;
    bool is_inline : 1;
    bool is_extern : 1;
    bool is_tls : 1;

    // NOTE(NeGate): In all honesty, this should probably not be
    // here since it's used in cases that aren't relevant to attribs.
    // mainly STMT_DECL, it keeps track of if anyone's referenced it
    // so we can garbage collect the symbol later.
    bool is_root : 1;
    bool is_used : 1;
} Attribs;

typedef struct {
    Atom key;

    // lexer_pos is non-zero if the enum value has it's compilation delayed
    int lexer_pos;
    int value;
} EnumEntry;

typedef struct {
    Cuik_Type* type;
    Atom name;
} Param;

typedef struct {
    Cuik_Type* key;
    Expr* value;
} C11GenericEntry;

struct Cuik_Type {
    Cuik_TypeKind kind;
    int size;  // sizeof
    int align; // _Alignof
    SourceLoc loc;

    Cuik_Type* based;
    // used by cycle checking
    Atom also_known_as;

    #ifdef CUIK_USE_TB
    TB_DebugType* debug_type;
    #else
    void* user_data;
    #endif

    int ordinal;

    bool is_const : 1;
    bool is_atomic : 1;
    bool is_incomplete : 1;
    bool is_inprogress : 1;

    union {
        Cuik_Type* qualified_ty;

        // Integers
        bool is_unsigned;

        // Arrays
        struct {
            Cuik_Type* array_of;
            int array_count;

            // if non-zero, then we must execute an expression
            // parser to resolve it
            int array_count_lexer_pos;
        };

        // Pointers
        struct {
            Cuik_Type* ptr_to;
            bool is_ptr_restrict : 1;
        };

        // Function
        struct {
            Atom name;
            Cuik_Type* return_type;
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
            Cuik_Type* base;
        } vector_;

        // Cuik_Typeof
        struct {
            Expr* src;
        } typeof_;

        struct {
            Atom name;
        } placeholder;
    };
};
#define TYPE_IS_INTEGER(x) (((x)->kind >= KIND_CHAR) && ((x)->kind <= KIND_LONG))
#define TYPE_IS_FLOAT(x) (((x)->kind >= KIND_FLOAT) && ((x)->kind <= KIND_DOUBLE))

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
    SourceLoc loc;

    // Fully resolved members with a kid_count of 0 will have
    // a proper offset and type after the type checking
    uint32_t offset;
    Cuik_Type* type;

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

typedef enum StmtOp {
    STMT_NONE,

    STMT_COMPOUND,
    STMT_DECL,

    // It's a normal decl but global
    STMT_GLOBAL_DECL,
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
    EXPR_VA_ARG,

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

    EXPR_CMPEQ,
    EXPR_CMPNE,
    EXPR_CMPGE,
    EXPR_CMPLE,
    EXPR_CMPGT,
    EXPR_CMPLT,

    // these are resolved by semantics pass
    EXPR_PTRADD,
    EXPR_PTRSUB,
    EXPR_PTRDIFF,

    EXPR_TERNARY,
    EXPR_COMMA,

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
    EXPR_SIZEOF,   // on expr

    EXPR_ALIGNOF_T, // on type
    EXPR_ALIGNOF,   // on expr

    EXPR_PRE_INC,
    EXPR_PRE_DEC,
    EXPR_POST_INC,
    EXPR_POST_DEC,

    EXPR_MAX
} ExprOp;

struct Stmt {
    struct {
        StmtOp op;
        SourceLoc loc;
        Cuik_Attribute* attr_list;

        // Used by the backend for backend-y things
        union {
            #ifdef CUIK_USE_TB
            TB_Reg r;
            TB_Label l;
            TB_Function* f;
            TB_Symbol* s;
            TB_Global* g;
            TB_External* e;

            void* user_data;
            #else
            void* user_data;
            #endif /* CUIK_USE_TB */
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
            Cuik_Type* type;
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

struct Expr {
    ExprOp op : 8;

    // some flags:
    int has_parens : 1;
    int has_visited : 1;

    SourceRange loc;
    Cuik_Type* type;

    // this is the type it'll be desugared into
    // for example:
    //
    //   a + b where a and b are 16bit
    //
    //   their cast_type might be int because of C's
    //   promotion rules.
    Cuik_Type* cast_type;

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
            bool is_resolving_symbol;
        };

        // EXPR_PARAM
        int param_num;

        struct ExprEnum {
            int* num;
        } enum_val;
        struct {
            Expr* src;
            Cuik_Type* type;
        } va_arg_;
        struct {
            Expr* src;
            Cuik_Type* type;
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
            Cuik_Type* type;
        } x_of_type;
        // either sizeof(expr) or _Alignof(expr)
        struct {
            Expr* expr;
        } x_of_expr;
        struct {
            Cuik_Type* type;
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
            Cuik_IntSuffix suffix;
        } int_num;
    };
};
//_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, next_symbol_in_chain2), "these should be aliasing");
//_Static_assert(offsetof(Expr, next_symbol_in_chain) == offsetof(Expr, builtin_sym.next_symbol_in_chain), "these should be aliasing");
