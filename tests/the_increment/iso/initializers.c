
typedef struct {
	int a, b, c, d;
} SomeInfo;

static SomeInfo table[] = {
	{ 1, 2, 3, 4 },
	[3] = { 5, 6, 7, 8 }
};

typedef unsigned long long size_t;
extern void printf(const char* fmt, ...);

int main() {
	size_t count = sizeof(table) / sizeof(table[0]);
	printf("Table (%zu entries):\n", count);
	
	for (size_t i = 0; i < count; i++) {
		printf("[%zu] = { %d, %d, %d, %d }\n", i, table[i].a, table[i].b, table[i].c, table[i].d);
	}
	
	return 0;
}
