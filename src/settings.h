#pragma once
#include <back/tb.h>
#include <stdatomic.h>

typedef enum {
	EMIT_AST_NONE,
	EMIT_AST_MINIMAL,
	EMIT_AST_NORMAL,
} EmitAstMode;

// used for a compiler option about stopping at specific stages,
// it doesn't necessarily need to map to the exact number of stages
// in the compiler
typedef enum {
	STAGE_PREPROC,
	STAGE_TYPES,
	STAGE_IR,
	STAGE_OBJ,
	STAGE_FINAL,
} CompilerStage;

typedef struct CompilerSettings {
	const char* output_path;

	// this is exclusively used for the fancy
	// type printing CLI tool
	const char* hack_type_printer_name;

	CompilerStage stage_to_stop_at;

	// basically defines the sizeof(long)
	// if true it's 4 bytes if not it's 8
	bool is_windows_long      : 1;
	bool is_object_only       : 1;
	bool is_time_report       : 1;
	bool is_debug_build       : 1;
	bool is_debug_info        : 1;
	bool exercise             : 1;
	bool pedantic             : 1;
	bool optimize             : 1;
	bool freestanding         : 1;
	bool emit_partial_results : 1;
	bool verbose              : 1;
	bool run_output           : 1;

	atomic_bool using_winmain;
	int num_of_worker_threads;
} CompilerSettings;

extern TB_Arch target_arch;
extern TB_System target_system;
extern CompilerSettings settings;
