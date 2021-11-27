#include "lexer.h"
#include "parser.h"
#include "ir_gen.h"
#include <time.h>

int main(int argc, char* argv[]) {
	clock_t t1 = clock();
	
	//
	// TODO(NeGate): Preprocess file
	//
    const char* text = "int main(int argc, char* argv[]) {\n"
		"\n"
		"\tint x = 16;\n"
		"\t\n"
		"\tx = 640;\n"
		"\tx += 16;\n"
		"\tx -= 16;\n"
		"\tx *= 64;\n"
		"\tx /= 16;\n"
		"\tx &= 16;\n"
		"\tx |= 16;\n"
		"\tx ^= 16;\n"
		"\tint y = (x * x);\n"
		"\tif (y) y = 16;\n"
		"\tif (x) { y = 16; } else { x = 16; }\n"
		"\twhile (x) { x -= 1; }\n"
		"\tshort apple[16];\n"
		"\tapple[0] = 16;\n"
		"\t\n"
		"\treturn y;\n"
		"}\n";
	
#if 0
	Lexer l = (Lexer) { text };
    do {
        lexer_read(&l);
        printf("%d\t%.*s\n", l.token_type, (int)(l.token_end - l.token_start), l.token_start);
    } while (l.token_type);
#else
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(TB_ARCH_X86_64,
						   TB_SYSTEM_WINDOWS,
						   &features,
						   TB_OPT_O0,
						   1, false);
	
	// Parse
	TopLevel tl = parse_file(&(Lexer) { text });
	
	// Generate IR
	gen_ir(tl);
	
	// Compile
	if (!tb_module_compile(mod)) abort();
	
	// Generate object file
	FILE* f = fopen("./test_x64.obj", "wb");
	if (!tb_module_export(mod, f)) abort();
	fclose(f);
	
	tb_module_destroy(mod);
#endif
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("compilation took %f ms\n", delta_ms);
	return 0;
}
