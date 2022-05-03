#pragma once
#include "common.h"
#include "arena.h"

typedef unsigned char* Atom;

void atoms_init();
void atoms_deinit();
Atom atoms_put(size_t len, const unsigned char* str);
Atom atoms_putc(const unsigned char* str);
