#pragma once
#include <back/tb.h>
#include <stdatomic.h>

typedef struct CompilerSettings {
	bool is_object_only;
	bool print_tb_ir;
	const char* output_path;
	TB_OptLevel optimization_level;
	bool pedantic;
	atomic_bool using_winmain;
	
	int num_of_worker_threads;
} CompilerSettings;

extern TB_Arch target_arch;
extern TB_System target_sys;
extern CompilerSettings settings;

