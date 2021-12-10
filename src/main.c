#include "preproc.h"
#include "parser.h"
#include "ir_gen.h"
#include "atoms.h"
#include "stb_ds.h"
#include <time.h>
#include <stdatomic.h>
#include "../ext/threads.h"
#include "microsoft_craziness.h"

// frontend worker threads
#define NUM_THREADS 12

// this is how many IR gen tasks it tries to grab at any one time
#define MAX_MUNCH 256

static thrd_t threads[NUM_THREADS];

static _Atomic size_t tasks_reserved;
static _Atomic size_t tasks_complete;
static _Atomic bool is_running;
static size_t tasks_count;

// Frontend IR Gen has two stages, one places all
// forward decls (0) and one places all function bodies (1)
static int frontend_stage;
static TopLevel top_level;

static int task_thread(void* param) {
	while (is_running) {
		size_t t = atomic_fetch_add(&tasks_reserved, MAX_MUNCH);
		
		if (t+(MAX_MUNCH-1) >= tasks_count) {
			if (t < tasks_count) {
				for (size_t i = t; i < tasks_count; i++) {
					if (frontend_stage == 0) gen_ir_stage1(top_level, i);
					else gen_ir_stage2(top_level, i);
				}
				tasks_complete += (tasks_count - t);
			}
			
			while (tasks_reserved >= tasks_count) { 
				thrd_yield();
				if (!is_running) return 0;
			}
			
			continue;
		}
		
		for (size_t i = 0; i < MAX_MUNCH; i++) {
			if (frontend_stage == 0) gen_ir_stage1(top_level, t+i);
			else gen_ir_stage2(top_level, t+i);
		}
		
		tasks_complete += MAX_MUNCH;
	}
	
	return 0;
}

static void dispatch_tasks(size_t count) {
	tasks_count = count;
	tasks_complete = 0;
	tasks_reserved = 0;
	
	// wait until it's completed
    while (tasks_complete < tasks_count) { 
		thrd_yield();
	}
}

int main(int argc, char* argv[]) {
#if 0
	TokenStream s = preprocess_translation_unit("tests/test8.txt");
	FILE* f = fopen("aa.txt", "w");
	
	size_t token_count = arrlen(s.tokens);
	size_t last_line = 0;
	for (size_t i = 0; i < token_count; i++) {
		Token* t = &s.tokens[i];
		if (last_line != t->line) {
			fprintf(f, "\n\t%d:\t", t->line);
			last_line = t->line;
		}
		
		fprintf(f, "%.*s ", (int)(t->end - t->start), t->start);
	}
	
	fclose(f);
	
	((void)threads);
#else
	clock_t t1 = clock();
	
	// Parse CLI
	const char* output_path = NULL;
	
	// TODO(NeGate) parse cli
	
	if (output_path == NULL) {
		output_path = "test_x64";
		printf("No output file, defaulting to test_x64.exe...\n");
	}
	
	// Preprocess file
	TokenStream s = preprocess_translation_unit("tests/test8.txt");
	
	// Parse
	atoms_init();
	top_level = parse_file(&s);
	
	// Generate IR
	{
		is_running = true;
		for (int i = 0; i < NUM_THREADS; i++) {
			thrd_create(&threads[i], task_thread, NULL);
		}
		
		TB_FeatureSet features = { 0 };
		mod = tb_module_create(TB_ARCH_X86_64,
							   TB_SYSTEM_WINDOWS,
							   &features,
							   TB_OPT_O0,
							   NUM_THREADS, false);
		
		// Forward decls
		frontend_stage = 0;
		dispatch_tasks(arrlen(top_level.arr));
		
		// Generate bodies
		frontend_stage = 1;
		dispatch_tasks(arrlen(top_level.arr));
		
		// Just let's the threads know that 
		// they might need to die now
		tasks_count = 0;
		tasks_complete = 0;
		
		is_running = false;
		for (int i = 0; i < NUM_THREADS; i++) {
			thrd_join(threads[i], NULL);
		}
	}
	
	// Compile
	if (!tb_module_compile(mod)) abort();
	arrfree(s.tokens);
	
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
#if 0 /* _WIN32 */
	{
		// NOTE(NeGate): Windows still a bih, im forcing the 
		// W functions because im a bitch too
		// TODO(NeGate): Clean up this code and make it so that more
		// options are exposed in the frontend.
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
