// Our precious libCuik unity build, ideally a lot of the smaller components will
// become single header libs maybe even the entire thing.
#include "tls.c"
#include "path.c"

// Used around the place
#include "hashes.h"

#include <hash_map.h>
#include <file_map.h>

#define CUIK_FS_IMPL
#include <cuik_fs.h>

#define CUIK_AST_IMPL
#include <cuik_ast.h>

// Parser
#define CUIK_SYMTAB_IMPL
#include <cuik_symtab.h>

#include "ast_dump.c"
#include "atoms.c"
#include "types.c"
#include "parser.c"
#include "sema.c"
#include "visitors.c"

// IR Gen
#ifdef CONFIG_HAS_TB
#include "ir_gen.c"
#include "ir_gen2.c"
#endif

// Linker support
#include "linker.c"

// Targets
#include "targets/target_generic.c"

// Compilation units
#include "compilation_unit.c"
#include "driver/driver.c"
#include "cuik.c"

// Must be at the bottom because it does weird windows stuff
#include "crash_handler.c"
