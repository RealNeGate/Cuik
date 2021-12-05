#include "preprocessor.h"
#include "lexer.h"
#include "parser.h"
#include "ir_gen.h"
#include "atoms.h"
#include <time.h>
#include <stdatomic.h>
#include "../ext/threads.h"
#include "microsoft_craziness.h"

#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>

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
	unsigned char* text = read_entire_file("tests/test.txt");
	if (!text) {
		printf("Failed to read file!\n");
		return 1;
	}
	
	//clock_t t1 = clock();
	
	Lexer l = (Lexer) { text, text };
    do {
        lexer_read(&l);
        printf("%d\t%.*s\n", l.token_type, (int)(l.token_end - l.token_start), l.token_start);
    } while (l.token_type);
	
	//clock_t t2 = clock();
	//double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	//printf("lexing took %f ms\n", delta_ms);
	
	// NOTE(NeGate): Hard-coded the size of test5.txt because im lame
	//printf("%f gigs / second\n", (135889014.0 / (double)(delta_ms / 1000.0)) / 1000000000.0);
#else
	clock_t t1 = clock();
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(TB_ARCH_X86_64,
						   TB_SYSTEM_WINDOWS,
						   &features,
						   TB_OPT_O0,
						   1, false);
	
	// TODO(NeGate): Preprocess file
	const unsigned char* text = preprocess_file("tests/test5.txt");
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
	free_virtual_memory((void*)text);
	
	// Generate object file
	const char* obj_output_path = "test_x64.obj";
	FILE* f = fopen(obj_output_path, "wb");
	if (!tb_module_export(mod, f)) abort();
	fclose(f);
	
	tb_module_destroy(mod);
	atoms_deinit();
	
	clock_t t2 = clock();
	double delta_ms = ((t2 - t1) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("compilation took %f ms\n", delta_ms);
	
	// Linking
#if _WIN32
	{
		// NOTE(NeGate): Windows still a bih, im forcing the 
		// W functions because im a bitch too
		// TODO(NeGate): Clean up this code and make it so that more
		// options are exposed in the frontend.
		const char* output_path = "test_x64";
		wchar_t* cmd_line = malloc(1024 * sizeof(wchar_t));
		const char* libraries = "kernel32.lib user32.lib Gdi32.lib";
		
		MicrosoftCraziness_Find_Result vswhere = MicrosoftCraziness_find_visual_studio_and_windows_sdk();
		
		wchar_t working_dir[260];
		DWORD working_dir_len = GetCurrentDirectoryW(260, working_dir);
		for (size_t i = 0; i < working_dir_len; i++) {
			if (working_dir[i] == '\\') working_dir[i] = '/';
		}
		working_dir[working_dir_len++] = '/';
		working_dir[working_dir_len] = '\0';
		
		wchar_t output_file_no_ext[260];
		swprintf(output_file_no_ext, 260, L"%s%S", working_dir, output_path);
		
		swprintf(cmd_line, 1024,
				 L"/nologo /machine:amd64 /subsystem:console"
				 " /debug:none /entry:WinMain /pdb:%s.pdb /out:%s.exe /libpath:\"%s\""
				 " /libpath:\"%s\" /libpath:\"%s\" -nodefaultlib %S %s.obj",
				 output_file_no_ext, output_file_no_ext,
				 vswhere.vs_library_path, vswhere.windows_sdk_ucrt_library_path, vswhere.windows_sdk_um_library_path,
				 libraries, output_file_no_ext
				 );
		
		wchar_t* exe_path = malloc(260 * sizeof(wchar_t));
		swprintf(exe_path, 260, L"%s\\link.exe", vswhere.vs_exe_path);
		
		STARTUPINFOW si = {
			.cb = sizeof(STARTUPINFOW),
			.dwFlags = STARTF_USESTDHANDLES,
			.hStdInput = GetStdHandle(STD_INPUT_HANDLE),
			.hStdError = GetStdHandle(STD_ERROR_HANDLE),
			.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE)
		};
		PROCESS_INFORMATION pi = {};
		
		printf("Linker command:\n%S %S\n", exe_path, cmd_line);
		if (!CreateProcessW(exe_path, cmd_line, NULL, NULL, TRUE, 0, NULL, working_dir, &si, &pi)) {
			panic("Linker command could not be executed.");
		}
		
		// Wait until child process exits.
		WaitForSingleObject(pi.hProcess, INFINITE);
		
		// Close process and thread handles. 
		CloseHandle(pi.hProcess);
		CloseHandle(pi.hThread);
		
		MicrosoftCraziness_free_resources(&vswhere);
	}
#endif
	
	clock_t t3 = clock();
	delta_ms = ((t3 - t2) / (double)CLOCKS_PER_SEC) * 1000.0;
	printf("linking took %f ms\n", delta_ms);
#endif
	
	return 0;
}
