#pragma once
#include <arena.h>
#include <common.h>

#include <stb_ds.h>
#include <threads.h>
#include <front/parser.h>

#include <tb.h>

extern atomic_flag irgen_defined_tls_index;

typedef enum IRValType {
    RVALUE,
    RVALUE_PHI,

    LVALUE,
    LVALUE_BITS,
    LVALUE_LABEL,
    LVALUE_FUNC,
    LVALUE_EFUNC
} IRValType;

typedef struct IRVal {
    IRValType value_type;
    Cuik_Type* type;

    union {
        TB_Register reg;
        TB_Function* func;
        TB_ExternalID ext;
        struct {
            TB_Register reg;

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

inline static TB_DataType ctype_to_tbtype(const Cuik_Type* t) {
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

InitNode* count_max_tb_init_objects(int node_count, InitNode* node, int* out_count);

// func is NULL then it's not allowed to compute any dynamic initializer expressions
InitNode* eval_initializer_objects(TranslationUnit* tu, TB_Function* func, SourceLocIndex loc, TB_InitializerID init, TB_Register addr, int node_count, InitNode* node);

TB_Register irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, Expr* e);
IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, Expr* e);
void irgen_stmt(TranslationUnit* tu, TB_Function* func, Stmt* restrict s);