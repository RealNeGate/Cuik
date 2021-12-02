#include "lexer.h"
#include "parser.h"
#include "ir_gen.h"
#include <time.h>

#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

static char* read_entire_file(const char* filepath) {
	FILE* file = fopen(filepath, "rb");
	if (!file) return NULL;
	
	int descriptor = fileno(file);
	
	struct stat file_stats;
	if (fstat(descriptor, &file_stats) == -1) return NULL;
	
	int length = file_stats.st_size;
	char* data = malloc(length + 1);
	
	fseek(file, 0, SEEK_SET);
	size_t length_read = fread(data, 1, length, file);
	
	data[length_read] = '\0';
	fclose(file);
	
	return data;
}

int main(int argc, char* argv[]) {
#if 0
	Lexer l = (Lexer) { text };
    do {
        lexer_read(&l);
        printf("%d\t%.*s\n", l.token_type, (int)(l.token_end - l.token_start), l.token_start);
    } while (l.token_type);
#else
	clock_t t1 = clock();
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(TB_ARCH_X86_64,
						   TB_SYSTEM_WINDOWS,
						   &features,
						   TB_OPT_O0,
						   1, false);
	
	// TODO(NeGate): Preprocess file
    char* text = read_entire_file("test3.txt");
	if (!text) {
		printf("Failed to read file!\n");
		return 1;
	}
	
	// Parse
	TopLevel tl = parse_file(&(Lexer) { text, text });
	
	// Generate IR
	gen_ir(tl);
	
	// Compile
	if (!tb_module_compile(mod)) abort();
	
	// Generate object file
	FILE* f = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(mod, f)) abort();
	fclose(f);
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("compilation took %f ms\n", delta_ms);
	
	tb_module_destroy(mod);
	free(text);
#endif
	return 0;
}
