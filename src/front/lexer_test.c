#include "lexer.h"
#include "file_io.h"
#include <timer.h>

#define STB_DS_IMPLEMENTATION
#include <ext/stb_ds.h>

static void perf_test(Lexer* l) {
	timed_block("Lexer") {
		do { lexer_read(l); } while (l->token_type);
	}
}

static void dump_golden_test(Lexer* l, FILE* f) {
	int last_line = 0;
	size_t count = 0;
	while (true) {
		lexer_read(l);
		if (l->token_type == 0) break;
		
		if (last_line != l->current_line) {
			fprintf(f, "\nline %d: ", l->current_line);
			last_line = l->current_line;
		}
		
		fprintf(f, "%.*s ", (int)(l->token_end - l->token_start), l->token_start);
		count++;
	}
	
	printf("%zu tokens lexed...\n", count);
}

int main(int argc, char* argv[]) {
#if 0
	if (argc < 2) {
		printf("Expected file path\n");
		return 1;
	}
	
	unsigned char* text = (unsigned char*)read_entire_file(argv[1]);
	Lexer l = (Lexer) { "", text, text, 1 };
	
	perf_test(&l);
	
	return 0;
#else
	if (argc <= 3) {
		printf("Expected:\n");
		printf("\t%s bake [input C file] [output test file]\n", argv[0]);
		printf("\t\tOR \n");
		printf("\t%s test [input C file] [test file]\n", argv[0]);
		return 1;
	}
	
	const char* input_file = argv[2];
	const char* test_file = argv[3];
	
	// tests/the_pile/sqlite3.c
	unsigned char* text = (unsigned char*)read_entire_file(input_file);
	Lexer l = (Lexer) { "", text, text, 1 };
	
	if (strcmp(argv[1], "bake") == 0) {
		FILE* f = fopen(test_file, "wb");
		if (!f) {
			printf("Could not open file: %s\n", test_file);
			return -1;
		}
		
		dump_golden_test(&l, f);
		fclose(f);
		return 0;
	} else if (strcmp(argv[1], "test") == 0) {
		FILE* f = fopen("./tmp.txt", "wb");
		if (!f) {
			printf("Could not open temporary file: tmp.txt\n");
			return -1;
		}
		
		dump_golden_test(&l, f);
		fclose(f);
		
		char cmd[1024];
#if _WIN32
		sprintf(cmd, "fc tmp.txt %s", test_file);
#else
#error "Add a diff command equivalent for other platforms"
#endif
		
		if (system(cmd) != 0) {
			printf("LEXER GOLDEN FAILURE: %s!!!", input_file);
			remove("./tmp.txt");
			return 1;
		}
		
		remove("./tmp.txt");
		return 0;
	} else {
		printf("Unknown mode: either 'bake', 'test'\n");
		return -1;
	}
#endif
}
