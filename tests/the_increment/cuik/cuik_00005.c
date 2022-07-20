#include <stddef.h>
#include <stdlib.h>

#define STBDS_CACHE_LINE_SIZE   64
#define STBDS_ALIGN_FWD(n,a)   (((n) + (a) - 1) & ~((a)-1))

struct Foo {
	int a, b, *c;
};

int main() {
	struct Foo* f = malloc(sizeof(struct Foo));
	f->c = (int*) STBDS_ALIGN_FWD((size_t) (f + 1), STBDS_CACHE_LINE_SIZE);
	
	return 0;
}
