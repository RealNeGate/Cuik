#include "atoms.h"

// typedef struct Atom { char* key;  value; };

static Atoms_Segment* atoms_base;
static Atoms_Segment* atoms_top;
static size_t atoms_count;

void atoms_init() {
	atoms_base = atoms_top = malloc(sizeof(Atoms_Segment));
	if (!atoms_base) abort();
	
	atoms_base->next = NULL;
	atoms_base->used = 0;
}

void atoms_deinit() {
	Atoms_Segment* c = atoms_base;
	while (c) {
		Atoms_Segment* next = c->next;
		free(c);
		c = next;
	}
	
	atoms_base = atoms_top = NULL;
}

Atom atoms_put(size_t len, const unsigned char* str) {
	assert(len > 0);
	
	// Increment to atom counter
	atoms_count++;
	
	// Create new atom if it doesn't already exist
	const size_t amount_needed = len + 1;
	
	// If this ever happens... literally how...
	assert(amount_needed < sizeof(atoms_base->data));
	
	if (atoms_top->used + amount_needed < sizeof(atoms_base->data)) {
		unsigned char* ptr = &atoms_top->data[atoms_top->used];
		atoms_top->used += amount_needed;
		
		memcpy(ptr, str, len);
		ptr[len] = '\0';
		
		return ptr;
	}
	else {
		// Add new page
		Atoms_Segment* p = (Atoms_Segment*)malloc(sizeof(*atoms_base));
		if (!p) abort();
		
		p->next = NULL;
		p->used = amount_needed;
		
		memcpy(p->data, str, len);
		p->data[len] = '\0';
		
		// Insert to top of nodes
		atoms_top->next = p;
		atoms_top = p;
		
		return p->data;
	}
}

Atom atoms_putc(const unsigned char* str) {
	return atoms_put(strlen((const char*)str), str);
}
