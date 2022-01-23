#pragma once
#include <back/tb.h>

typedef struct CompilerSettings {
	bool is_object_only;              // -c
	bool print_tb_ir;                 // -p
	const char* output_path;          // -o
	TB_OptLevel optimization_level;   // -O
	bool pedantic;                    // -P
	
	int num_of_worker_threads;
} CompilerSettings;

extern TB_Arch target_arch;
extern TB_System target_sys;
extern CompilerSettings settings;
