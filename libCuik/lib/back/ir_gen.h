#pragma once
#ifdef CUIK_USE_TB

#include <arena.h>
#include <common.h>
#include <threads.h>
#include "../front/parser.h"

#include <tb.h>

extern atomic_flag irgen_defined_tls_index;

typedef enum IRValType {
    RVALUE,
    RVALUE_PHI,

    LVALUE,
    LVALUE_BITS,
    LVALUE_LABEL,
    LVALUE_SYMBOL,
} IRValType;

typedef struct IRVal {
    IRValType value_type;

    union {
        TB_Node* reg;
        const TB_Symbol* sym;
        struct {
            TB_Node* reg;

            short offset;
            short width;
        } bits;
        struct {
            TB_Label if_true;
            TB_Label if_false;
        } phi;
        TB_Label label;
    };
} IRVal;

static TB_DataType ctype_to_tbtype(const Cuik_Type* t) {
    switch (t->kind) {
        case KIND_VOID:
        return TB_TYPE_VOID;
        case KIND_BOOL:
        return TB_TYPE_BOOL;
        case KIND_CHAR:
        return TB_TYPE_I8;
        case KIND_SHORT:
        return TB_TYPE_I16;
        case KIND_INT:
        return TB_TYPE_I32;
        case KIND_LONG:
        return TB_TYPE_I32;
        case KIND_LLONG:
        return TB_TYPE_I64;
        case KIND_FLOAT:
        return TB_TYPE_F32;
        case KIND_DOUBLE:
        return TB_TYPE_F64;
        case KIND_ENUM:
        return TB_TYPE_I32;

        case KIND_PTR:
        case KIND_FUNC:
        case KIND_ARRAY:
        return TB_TYPE_PTR;

        case KIND_STRUCT:
        case KIND_UNION:
        return TB_TYPE_PTR;

        default:
        abort(); // TODO
    }
}

_Noreturn void internal_error(const char* fmt, ...);
int count_max_tb_init_objects(InitNode* root_node);
TB_DebugType* cuik__as_tb_debug_type(TB_Module* mod, Cuik_Type* t);

TB_Node* irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, Expr* e);
IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, Expr* e);
void irgen_stmt(TranslationUnit* tu, TB_Function* func, Stmt* restrict s);

#endif
