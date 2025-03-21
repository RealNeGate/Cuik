// cgemit is short for Codegen Emitter
//
// for better runtime performance this is included into the specific
// files it's used in
#pragma once
#include "tb_internal.h"

// We really only need the position where to patch
// it since it's all internal and the target is implicit.
typedef uint32_t ReturnPatch;

typedef struct LabelPatch {
    int pos;
    int target_lbl;
} LabelPatch;

typedef struct Comment {
    struct Comment* next;
    uint32_t pos;
    uint32_t line_len;
    char line[];
} Comment;

typedef struct {
    // technically NULLable, just can't use patches if NULL
    TB_FunctionOutput* output;
    TB_Arena* arena;

    TB_Assembly *head_asm, *tail_asm;
    uint64_t total_asm;

    // this is mapped to a giant buffer and is technically
    // allow to use the entire rest of said buffer
    size_t count, capacity;
    uint8_t* data;

    size_t label_count;
    uint32_t* labels;

    int final_order_count;
    int* final_order;

    bool has_comments;
    Comment* comment_head;
    Comment* comment_tail;
} TB_CGEmitter;

// Helper macros
#define EMITA(e, fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
#define EMIT1(e, b) (*((uint8_t*)  tb_cgemit_reserve(e, 1)) = (b), (e)->count += 1)
#define EMIT2(e, b) do { uint16_t _b = (b); memcpy(tb_cgemit_reserve(e, 2), &_b, 2); (e)->count += 2; } while (0)
#define EMIT4(e, b) do { uint32_t _b = (b); memcpy(tb_cgemit_reserve(e, 4), &_b, 4); (e)->count += 4; } while (0)
#define EMIT8(e, b) do { uint64_t _b = (b); memcpy(tb_cgemit_reserve(e, 8), &_b, 8); (e)->count += 8; } while (0)
#define PATCH2(e, p, b) do { uint16_t _b = (b); memcpy(&(e)->data[p], &_b, 2); } while (0)
#define PATCH4(e, p, b) do { uint32_t _b = (b); memcpy(&(e)->data[p], &_b, 4); } while (0)
#define GET_CODE_POS(e) ((e)->count)
#define RELOC4(e, p, b, m) tb_reloc4(e, p, b, m)

static void tb_reloc4(TB_CGEmitter* restrict e, uint32_t p, uint32_t b, uint32_t mask) {
    void* ptr = &e->data[p];

    // i love UBsan... (tbf this is the correct way to do things)
    uint32_t tmp;
    memcpy(&tmp, ptr, 4);
    tmp = (tmp & ~mask) | ((tmp + b) & mask);
    memcpy(ptr, &tmp, 4);
}

static void tb_reloc4_noadd(TB_CGEmitter* restrict e, uint32_t p, uint32_t b, uint32_t mask) {
    void* ptr = &e->data[p];

    uint32_t tmp;
    memcpy(&tmp, ptr, 4);
    tmp = (tmp & ~mask) | (b & mask);
    memcpy(ptr, &tmp, 4);
}

static int tb_emit_get_label(TB_CGEmitter* restrict e, uint32_t pos) {
    FOR_REV_N(i, 0, e->final_order_count) {
        int id = e->final_order[i];
        TB_ASSERT(e->labels[id] & 0x80000000);
        if ((e->labels[id] & ~0x80000000) == pos) {
            return id;
        } else if ((e->labels[id] & ~0x80000000) < pos) {
            return id + 1;
        }
    }

    return 0;
}

static void tb_emit_comment(TB_CGEmitter* restrict e, TB_Arena* arena, const char* fmt, ...) {
    Comment* comment = tb_arena_alloc(arena, sizeof(Comment) + 100);
    comment->next = NULL;
    comment->pos = e->count;

    va_list ap;
    va_start(ap, fmt);
    comment->line_len = vsnprintf(comment->line, 100, fmt, ap);
    va_end(ap);

    if (e->comment_tail) {
        e->comment_tail->next = comment;
        e->comment_tail = comment;
    } else {
        e->comment_head = e->comment_tail = comment;
    }
}

static int tb_asm_print(TB_CGEmitter* restrict e, const char* fmt, ...) {
    // make sure we have enough bytes for the operation
    TB_Assembly* new_head = e->tail_asm;
    if (new_head == NULL || new_head->length + 120 >= TB_ASSEMBLY_CHUNK_CAP) {
        new_head = tb_platform_valloc(TB_ASSEMBLY_CHUNK_CAP);
        // new_head->next = NULL;
        // new_head->length = 0;

        if (e->tail_asm == NULL) {
            e->tail_asm = e->head_asm = new_head;
        } else {
            e->tail_asm->next = new_head;
            e->tail_asm = new_head;
        }
    }

    va_list ap;
    va_start(ap, fmt);
    int len = vsnprintf(&new_head->data[new_head->length], 120, fmt, ap);
    va_end(ap);

    TB_ASSERT(len >= 0 && len < 120);
    new_head->length += len;
    e->total_asm += len;
    return len;
}

static void tb_emit_rel32(TB_CGEmitter* restrict e, uint32_t* head, uint32_t pos, uint32_t mask, uint32_t shift) {
    uint32_t curr = *head;
    if (curr & 0x80000000) {
        // the label target is resolved, we need to do the relocation now
        curr &= ~0x80000000;
        RELOC4(e, pos, (curr - (pos + 4)) >> shift, mask);
    } else {
        RELOC4(e, pos, curr, mask);
        *head = pos;
    }
}

static void tb_resolve_rel32(TB_CGEmitter* restrict e, uint32_t* head, uint32_t target, uint32_t mask, uint32_t shift) {
    // walk previous relocations
    uint32_t curr = *head;
    while ((curr & mask) != 0) {
        uint32_t next;
        memcpy(&next, &e->data[curr], 4);
        memset(&e->data[curr], 0, 4);
        RELOC4(e, curr, (target - (curr + 4)) >> shift, mask);
        curr = next;
    }

    // store the target and mark it as resolved
    *head = 0x80000000 | target;
}

static void* tb_cgemit_reserve(TB_CGEmitter* restrict e, size_t count) {
    if (e->count + count >= e->capacity) {
        size_t new_cap = tb_next_pow2(e->count + count);

        e->data = tb_arena_realloc(e->arena, e->data, e->capacity, new_cap);
        e->capacity = new_cap;
    }

    return &e->data[e->count];
}

static void tb_cgemit_commit(TB_CGEmitter* restrict e, size_t bytes) {
    e->count += bytes;
}

