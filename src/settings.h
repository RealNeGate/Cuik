#pragma once
#include <back/tb.h>
#include <stdatomic.h>

typedef enum {
	EMIT_AST_NONE,
	EMIT_AST_MINIMAL,
	EMIT_AST_NORMAL,
} EmitAstMode;

typedef struct CompilerSettings {
	const char* output_path;
	
	// this is exclusively used for the fancy
	// type printing CLI tool
	const char* hack_type_printer_name;

	// basically defines the sizeof(long)
	// if true it's 4 bytes if not it's 8
	bool is_windows_long  : 1;
	bool is_object_only   : 1;
	bool is_time_report   : 1;
	bool is_debug_build   : 1;
	bool is_debug_info    : 1;
	bool print_tb_ir      : 1;
	bool pedantic         : 1;
	bool find_include     : 1;
	bool optimize         : 1;
	bool freestanding     : 1;
	
	EmitAstMode emit_ast;
	atomic_bool using_winmain;
	int num_of_worker_threads;
} CompilerSettings;

extern TB_Arch target_arch;
extern TB_System target_system;
extern CompilerSettings settings;
