#include "lexer.h"
#include "file_io.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

static void dump_golden_test(Lexer* l, FILE* f) {
	int last_line = 0;
	while (true) {
		lexer_read(l);
		if (l->token_type == 0) break;
		
		if (last_line != l->current_line) {
			fprintf(f, "\nline %d: ", l->current_line);
			last_line = l->current_line;
		}
		
		fprintf(f, "%.*s ", (int)(l->token_end - l->token_start), l->token_start);
	}
}

int main(int argc, char* argv[]) {
	if (argc <= 3) {
		printf("Expected:\n");
		printf("\t%s bake [input C file] [output test file]\n", argv[0]);
		printf("\t%s OR \n", argv[0]);
		printf("\t%s test [input C file] [test file]\n", argv[0]);
		return -1;
	}
	
	const char* input_file = argv[2];
	const char* test_file = argv[3];
	
	// tests/the_pile/sqlite3.c
	unsigned char* text = (unsigned char*)read_entire_file(input_file);
	Lexer l = (Lexer) { "", text, text, 1 };
	l.temp_buffer_capacity = 1 << 20;
	l.temp_buffer = malloc(1 << 20);
	
	if (strcmp(argv[1], "bake") == 0) {
		FILE* f = fopen(test_file, "w");
		if (!f) {
			printf("Could not open file: %s\n", test_file);
			return -1;
		}
		
		dump_golden_test(&l, f);
		fclose(f);
		return 0;
	} else if (strcmp(argv[1], "test") == 0) {
		FILE* f = fopen("./tmp.txt", "w");
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
}
