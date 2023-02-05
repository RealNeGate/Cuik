// cgemit is short for Codegen Emitter
//
// for better runtime performance this is included into the specific
// files it's used in
#pragma once
#include "../tb_internal.h"

// We really only need the position where to patch
// it since it's all internal and the target is implicit.
typedef uint32_t ReturnPatch;

typedef struct LabelPatch {
    int pos;
    TB_Label target_lbl;
} LabelPatch;

typedef struct {
    // technically NULLable, just can't use patches if NULL
    TB_Function* f;

    // this is mapped to a giant buffer and is technically allow to use the entire rest
    // of said buffer
    size_t count, capacity;
    uint8_t* data;

    // Patch info
    // Not handled here
    uint32_t label_patch_count;
    uint32_t ret_patch_count;

    uint32_t* labels;
    LabelPatch* label_patches;
    ReturnPatch* ret_patches;
} TB_CGEmitter;

// Helper macros
#define EMIT1(e, b) (*((uint8_t*)  tb_cgemit_reserve(e, 1)) = (b), (e)->count += 1)
#define EMIT2(e, b) (*((uint16_t*) tb_cgemit_reserve(e, 2)) = (b), (e)->count += 2)
#define EMIT4(e, b) (*((uint32_t*) tb_cgemit_reserve(e, 4)) = (b), (e)->count += 4)
#define EMIT8(e, b) (*((uint64_t*) tb_cgemit_reserve(e, 8)) = (b), (e)->count += 8)
#define RELOC4(e, p, b) (*((uint32_t*) &(e)->data[p]) += (b))
#define PATCH4(e, p, b) (*((uint32_t*) &(e)->data[p])  = (b))
#define GET_CODE_POS(e) ((e)->count)

inline static void* tb_cgemit_reserve(TB_CGEmitter* restrict e, size_t count) {
    if (e->count + count >= e->capacity) {
        tb_panic("tb_cgemit_reserve: Out of memory!");
    }

    return &e->data[e->count];
}

inline static void tb_cgemit_commit(TB_CGEmitter* restrict e, size_t bytes) {
    e->count += bytes;
}
