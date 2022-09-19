#pragma once
#include "arena.h"
#include "common.h"

typedef char* Atom;

void atoms_init(void);
void atoms_deinit(void);
void atoms_dump_stats(void);
Atom atoms_put(size_t len, const unsigned char* str);
Atom atoms_putc(const unsigned char* str);
