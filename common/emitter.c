#include <prelude.h>
#include <common.h>

// EMITTER CODE:
//   Simple linear allocation for the
//   backend's to output code with.
void* tb_out_reserve(TB_Emitter* o, size_t count) {
    if (o->count + count >= o->capacity) {
        if (o->capacity == 0) {
            if (count < 64) {
                o->capacity = 64;
            } else {
                o->capacity = count;
            }
        } else {
            o->capacity += count;
            o->capacity *= 2;
        }

        o->data = cuik_realloc(o->data, o->capacity);
        if (o->data == NULL) {
            fprintf(stderr, "error: out of memory!");
            abort();
        }
    }

    return &o->data[o->count];
}

void tb_out_commit(TB_Emitter* o, size_t count) {
    assert(o->count + count < o->capacity);
    o->count += count;
}

size_t tb_out_get_pos(TB_Emitter* o, void* p) {
    return (uint8_t*)p - o->data;
}

void* tb_out_grab(TB_Emitter* o, size_t count) {
    void* p = tb_out_reserve(o, count);
    o->count += count;

    return p;
}

void* tb_out_get(TB_Emitter* o, size_t pos) {
    assert(pos < o->count);
    return &o->data[pos];
}

size_t tb_out_grab_i(TB_Emitter* o, size_t count) {
    tb_out_reserve(o, count);

    size_t old = o->count;
    o->count += count;
    return old;
}

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i) {
    assert(o->count + 1 < o->capacity);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_out1b(TB_Emitter* o, uint8_t i) {
    tb_out_reserve(o, 1);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out2b(TB_Emitter* o, uint16_t i) {
    tb_out_reserve(o, 2);

    *((uint16_t*)&o->data[o->count]) = i;
    o->count += 2;
}

void tb_out4b(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_patch1b(TB_Emitter* o, uint32_t pos, uint8_t i) {
    *((uint8_t*)&o->data[pos]) = i;
}

void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i) {
    *((uint16_t*)&o->data[pos]) = i;
}

void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i) {
    assert(pos + 4 <= o->count);
    *((uint32_t*)&o->data[pos]) = i;
}

void tb_patch8b(TB_Emitter* o, uint32_t pos, uint64_t i) {
    *((uint64_t*)&o->data[pos]) = i;
}

uint8_t tb_get1b(TB_Emitter* o, uint32_t pos) {
    return *((uint8_t*)&o->data[pos]);
}

uint16_t tb_get2b(TB_Emitter* o, uint32_t pos) {
    return *((uint16_t*)&o->data[pos]);
}

uint32_t tb_get4b(TB_Emitter* o, uint32_t pos) {
    return *((uint32_t*)&o->data[pos]);
}

void tb_out8b(TB_Emitter* o, uint64_t i) {
    tb_out_reserve(o, 8);

    *((uint64_t*)&o->data[o->count]) = i;
    o->count += 8;
}

void tb_out_zero(TB_Emitter* o, size_t len) {
    tb_out_reserve(o, len);
    memset(&o->data[o->count], 0, len);
    o->count += len;
}

size_t tb_outstr_nul_UNSAFE(TB_Emitter* o, const char* str) {
    size_t start = o->count;

    for (; *str; str++) {
        o->data[o->count++] = *str;
    }

    o->data[o->count++] = 0;
    return start;
}

void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
    while (*str) o->data[o->count++] = *str++;
}

size_t tb_outs(TB_Emitter* o, size_t len, const void* str) {
    tb_out_reserve(o, len);
    size_t start = o->count;

    memcpy(&o->data[o->count], str, len);
    o->count += len;
    return start;
}

void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const void* str) {
    memcpy(&o->data[o->count], str, len);
    o->count += len;
}

