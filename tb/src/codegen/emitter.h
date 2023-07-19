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
    TB_FunctionOutput* output;

    bool emit_asm;

    // this is mapped to a giant buffer and is technically
    // allow to use the entire rest of said buffer
    size_t count, capacity;
    uint8_t* data;

    NL_Map(TB_Node*, uint32_t) labels;
    uint32_t return_label;
} TB_CGEmitter;

// Helper macros
#define EMIT1(e, b) (*((uint8_t*)  tb_cgemit_reserve(e, 1)) = (b), (e)->count += 1)
#define EMIT2(e, b) (*((uint16_t*) tb_cgemit_reserve(e, 2)) = (b), (e)->count += 2)
#define EMIT4(e, b) (*((uint32_t*) tb_cgemit_reserve(e, 4)) = (b), (e)->count += 4)
#define EMIT8(e, b) (*((uint64_t*) tb_cgemit_reserve(e, 8)) = (b), (e)->count += 8)
#define RELOC4(e, p, b) (*((uint32_t*) &(e)->data[p]) += (b))
#define PATCH4(e, p, b) (*((uint32_t*) &(e)->data[p])  = (b))
#define GET_CODE_POS(e) ((e)->count)

static void tb_emit_rel32(TB_CGEmitter* restrict e, uint32_t* head, uint32_t pos) {
    uint32_t curr = *head;
    if (curr & 0x80000000) {
        // the label target is resolved, we need to do the relocation now
        uint32_t target = curr & 0x7FFFFFFF;
        PATCH4(e, pos, target - (pos + 4));
    } else {
        PATCH4(e, pos, curr);
        *head = pos;
    }
}

static void tb_resolve_rel32(TB_CGEmitter* restrict e, uint32_t* head, uint32_t target) {
    // walk previous relocations
    uint32_t curr = *head;
    while (curr != 0 && (curr & 0x80000000) == 0) {
        uint32_t next = *((uint32_t*) &e->data[curr]);
        PATCH4(e, curr, target - (curr + 4));
        curr = next;
    }

    // store the target and mark it as resolved
    *head = 0x80000000 | target;
}

static void* tb_cgemit_reserve(TB_CGEmitter* restrict e, size_t count) {
    if (e->count + count >= e->capacity) {
        // make new region
        TB_CodeRegion* new_region = tb_platform_valloc(CODE_REGION_BUFFER_SIZE);
        if (new_region == NULL) tb_panic("could not allocate code region!");

        new_region->capacity = CODE_REGION_BUFFER_SIZE - sizeof(TB_CodeRegion);
        e->output->code_region = new_region;

        // copy code into new region
        memcpy(new_region->data, e->data, e->count);
        e->data = new_region->data;
    }

    return &e->data[e->count];
}

static void tb_cgemit_commit(TB_CGEmitter* restrict e, size_t bytes) {
    e->count += bytes;
}