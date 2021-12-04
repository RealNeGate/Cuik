#include "lexer.h"
#include "parser.h"
#include "ir_gen.h"
#include "atoms.h"
#include <time.h>
#include <stdatomic.h>
#include "../ext/threads.h"

#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

static unsigned char* read_entire_file(const char* filepath) {
	FILE* file = fopen(filepath, "rb");
	if (!file) return NULL;
	
	int descriptor = fileno(file);
	
	struct stat file_stats;
	if (fstat(descriptor, &file_stats) == -1) return NULL;
	
	int length = file_stats.st_size;
	unsigned char* data = malloc(length + 1);
	
	fseek(file, 0, SEEK_SET);
	size_t length_read = fread(data, 1, length, file);
	
	data[length_read] = '\0';
	fclose(file);
	
	return data;
}

int main(int argc, char* argv[]) {
#if 0
	/*FILE* file = fopen("test5.txt", "wb");
	
	fprintf(file, "void print(int n){}\n");
	
	for (int i = 1; i <= 1000000; i++) {
		fprintf(file, "void fibonacci%d() {\n"
				"\tint lo = 0;\n"
				"\tint hi = 1;\n"
				"\twhile (hi < 10000) {\n"
				"\t\tint tmp = hi;\n"
				"\t\thi = hi + lo;\n"
				"\t\tlo = tmp;\n"
				"\t\tprint(lo);\n"
				"\t}\n"
				"}\n\n", i);
	}
	
	fprintf(file, "\nint WinMain(void* hInstance, void* hPrevInstance, char* lpCmdLine, int nCmdShow) {\n\treturn 0;\n}\n\n");
	fclose(file);*/
	
	unsigned char* text = read_entire_file("test5.txt");
	if (!text) {
		printf("Failed to read file!\n");
		return 1;
	}
	
	clock_t t1 = clock();
	
	Lexer l = (Lexer) { text, text };
    do {
        lexer_read(&l);
        //printf("%d\t%.*s\n", l.token_type, (int)(l.token_end - l.token_start), l.token_start);
    } while (l.token_type);
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("lexing took %f ms\n", delta_ms);
	printf("%f gigs / second\n", (135889014.0 / (double)(delta_ms / 1000.0)) / 1000000000.0);
#else
	clock_t t1 = clock();
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(TB_ARCH_X86_64,
						   TB_SYSTEM_WINDOWS,
						   &features,
						   TB_OPT_O0,
						   1, false);
	
	// TODO(NeGate): Preprocess file
	unsigned char* text = read_entire_file("tests/test3.txt");
	if (!text) {
		printf("Failed to read file!\n");
		return 1;
	}
	
	// Parse
	atoms_init();
	TopLevel tl = parse_file(&(Lexer) { text, text });
	
	// Generate IR
	gen_ir_stage1(tl);
	gen_ir_stage2(tl);
	
	// Compile
	if (!tb_module_compile(mod)) abort();
	
	// Generate object file
	FILE* f = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(mod, f)) abort();
	fclose(f);
	
	tb_module_destroy(mod);
	free(text);
	atoms_deinit();
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("compilation took %f ms\n", delta_ms);
#endif
	return 0;
}
