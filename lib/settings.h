#pragma once
#include <tb.h>
#include <stdatomic.h>

typedef enum {
    EMIT_AST_NONE,
    EMIT_AST_MINIMAL,
    EMIT_AST_NORMAL,
} EmitAstMode;

// used for a compiler option about stopping at specific stages,
// it doesn't necessarily need to map to the exact number of stages
// in the compiler
typedef enum {
    STAGE_PREPROC,
    STAGE_TYPES,
    STAGE_IR,
    STAGE_OBJ,
    STAGE_FINAL,
} CompilerStage;

typedef struct {
    // implicitly converting between types and losing information
    bool data_loss : 1;
    bool unused_decls : 1;
    bool unused_funcs : 1;
} Warnings;

extern Warnings warnings;