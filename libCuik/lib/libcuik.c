// Our precious libCuik unity build
#include "tls.c"
#include "str.c"
#include "timer.c"
#include "hash.c"
#include "path.c"
#include "mem_pool.c"

#include <hash_map.h>
#include <file_map.h>

// Zip library
#include "fs.c"
#include "zip/zip.c"

// Preprocessor & Lexer
#include "preproc/lexer.c"
#include "preproc/cpp.c"

// Parser
#include "front/ast_dump.c"
#include "front/atoms.c"
#include "front/types.c"
#include "front/parser.c"
#include "front/sema.c"
#include "front/visitors.c"

// Targets
#include "targets/target_generic.c"
#include "targets/x64_desc.c"

// IR Gen
#ifdef CUIK_USE_TB
#include "back/ir_gen.c"
#include "back/linker.c"
#endif

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
