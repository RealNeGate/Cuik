#include "../common.h"
#include <front/preproc.h>
#include <back/ir_gen.h>

typedef struct BuiltinBinding {
	char* key;
	int value;
} BuiltinBinding;

typedef struct TargetDescriptor {
	// stringmap that goes from builtin function names to
	// an index used to refer to them later on
	BuiltinBinding* builtin_func_map;
	
	// initializes some target specific macro defines
	void (*set_defines)(CPP_Context* cpp);
	
	// when one of the builtins are triggered we call this to generate it's code
	TB_Register (*compile_builtin)(TB_Function* func, const char* name,
								   int arg_count, ExprIndex* args);
} TargetDescriptor;

TargetDescriptor get_x64_target_descriptor();
// put any other options here...

extern TargetDescriptor target_desc;
