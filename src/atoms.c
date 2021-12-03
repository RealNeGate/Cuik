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
}

Atom atoms_get(size_t len, const char* str) {
	/*const char first_char = str[0];
	Atoms_Segment* c = atoms_base;
	
	while (c && c->used) {
		char* search = c->data;
		do {
			const size_t remainder = c->used - (search - c->data);
			search = memchr(search, first_char, remainder);
			if (search == NULL) break;
			
			bool the_rest_of_the_string_matches = memcmp(search, str, len) == 0 
				&& search[len] == '\0';
			
			if (the_rest_of_the_string_matches) return search;
			
			search++;
		} while (search != &c->data[c->used]);
		
		c = c->next;
	}*/
	
	return NULL;
}

Atom atoms_put(size_t len, const char* str) {
	assert(len > 0);
	
	//Atom test = atoms_get(len, str);
	//if (test) return test;
	
	// Increment to atom counter
	atoms_count++;
	
	// Create new atom if it doesn't already exist
	const size_t amount_needed = len + 1;
	
	// If this ever happens... literally how...
	assert(amount_needed < sizeof(atoms_base->data));
	
	if (atoms_top->used + amount_needed < sizeof(atoms_base->data)) {
		char* ptr = &atoms_top->data[atoms_top->used];
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

Atom atoms_putc(const char* str) {
	return atoms_put(strlen(str), str);
}
