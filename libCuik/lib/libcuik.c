// Our precious libCuik unity build, ideally a lot of the smaller components will
// become single header libs maybe even the entire thing.
#include "tls.c"
#include "str.c"
#include "path.c"

// Used around the place
#include "hashes.h"

#include <hash_map.h>
#include <file_map.h>

#define CUIK_FS_IMPL
#include <cuik_fs.h>

// Preprocessor & Lexer
#include "preproc/lexer.c"
#include "preproc/cpp.c"

#define CUIK_AST_IMPL
#include <cuik_ast.h>

// Parser
#define CUIK_SYMTAB_IMPL
#include <cuik_symtab.h>

#include "front/ast_dump.c"
#include "front/atoms.c"
#include "front/types.c"
#include "front/parser.c"
#include "front/sema.c"
#include "front/visitors.c"

// IR Gen
#ifdef CUIK_USE_TB
#include "back/ir_gen.c"
#include "back/ir_gen2.c"
#endif

// Linker support
#include "back/linker.c"

// Targets
#include "targets/target_generic.c"

// Compilation units
#include "compilation_unit.c"
#include "driver/driver.c"
#include "cuik.c"

// Core utils
#include "diagnostic.c"

// Must be at the bottom because it does weird windows stuff
#include "crash_handler.c"

#ifdef CUIK_ALLOW_THREADS
#include "threadpool.c"
#endif
