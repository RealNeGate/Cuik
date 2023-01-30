#pragma once
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include "builtins.h"

#define SET_INITIAL_CAP 32

typedef struct Set {
    size_t capacity;
    uint64_t* data;
} Set;

inline static Set set_create(size_t cap) {
    return (Set){
        .capacity = cap,
        .data = calloc((cap + 63) / 64, sizeof(uint64_t)),
    };
}

inline static void set_free(Set* s) {
    free(s->data);
}

inline static void set_clear(Set* s) {
    memset(s->data, 0, ((s->capacity + 63) / 64) * sizeof(uint64_t));
}

inline static size_t set_popcount(Set* s) {
    size_t sum = 0;
    for (size_t i = 0; i < s->capacity; i++) {
        sum += tb_popcount64(s->data[i]);
    }

    return sum;
}

// Grabs a free slot
inline static ptrdiff_t set_pop_any(Set* s) {
    size_t slots = (s->capacity + 63) / 64;

    for (size_t i = 0; i < slots; i++) {
        if (s->data[i] == UINT64_MAX) continue;

        int index = s->data[i] != 0 ? tb_ffs64(~s->data[i]) - 1 : 0;
        if (i*64 + index >= s->capacity) continue;

        s->data[i] |= (1ull << index);
        return i*64 + index;
    }

    return -1;
}

inline static bool set_first_time(Set* s, size_t index) {
    size_t slots = (s->capacity + 63) / 64;
    lldiv_t d = lldiv(index, 64);

    if (d.quot >= slots) {
        s->capacity = index * 2;
        size_t new_slots = (s->capacity + 63) / 64;

        s->data = realloc(s->data, new_slots * sizeof(uint64_t));
        if (s->data == NULL) {
            fprintf(stderr, "TB error: Set out of memory!");
            abort();
        }

        memset(s->data + slots, 0, (new_slots - slots) * sizeof(uint64_t));
    }

    if ((s->data[d.quot] & (1ull << d.rem)) == 0) {
        s->data[d.quot] |= (1ull << d.rem);
        return true;
    }

    return false;
}

inline static void set_put(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot >= s->capacity) {
        size_t old = s->capacity;

        s->capacity = d.quot * 2;
        s->data = realloc(s->data, s->capacity * sizeof(uint64_t));
        if (s->data == NULL) {
            fprintf(stderr, "TB error: Set out of memory!");
            abort();
        }

        memset(s->data + old, 0, (s->capacity - old) * sizeof(uint64_t));
    }

    s->data[d.quot] |= (1ull << d.rem);
}

inline static void set_remove(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot < s->capacity) {
        s->data[d.quot] &= ~(1ull << d.rem);
    }
}

inline static bool set_get(Set* s, size_t index) {
    lldiv_t d = lldiv(index, 64);
    if (d.quot >= s->capacity) {
        return false;
    }

    return s->data[d.quot] & (1ull << d.rem);
}
