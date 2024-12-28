#pragma once
#include "arena.h"
#include "common.h"

typedef char* Atom;

void atoms_free(void);
Atom atoms_put(size_t len, const unsigned char* str);
Atom atoms_putuc(const unsigned char* str);
Atom atoms_putc(const char* str);

// length of an atom
size_t atoms_len(Atom str);

