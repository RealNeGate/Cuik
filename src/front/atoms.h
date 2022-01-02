#pragma once
#include "common.h"

typedef unsigned char* Atom;

// It's a linked list :)
typedef struct Atoms_Segment {
	struct Atoms_Segment* next;
	size_t used;
	unsigned char data[(64 * 1024) - (2 * sizeof(size_t))];
} Atoms_Segment;

void atoms_init();
void atoms_deinit();
Atom atoms_get(size_t len, const unsigned char* str);
Atom atoms_put(size_t len, const unsigned char* str);
Atom atoms_putc(const unsigned char* str);
