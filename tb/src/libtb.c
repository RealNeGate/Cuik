// This is the TB unity build
#include "tb.c"
#include "hash.c"
#include "analysis.c"
#include "tb_atomic.c"
#include "tb_internal.c"
#include "tb_builder.c"
#include "debug_builder.c"
#include "ir_printer.c"
#include "exporter.c"
#include "symbols.c"

#include "bigint/BigInt.c"

// JIT
#include "jit/jit.c"

// Optimizer
#include "opt/optimizer.c"

// Debug
#include "debug/cv/cv.c"
#include "debug/fut/fut.c"

// Objects
#include "objects/coff.c"
#include "objects/coff_parse.c"
#include "objects/elf64.c"
#include "objects/macho.c"

// Linker
#include "linker/linker.c"
#include "linker/pe.c"
#include "linker/elf.c"

// Platform layer
#ifdef _WIN32
#include "system/win32.c"
#else
#include "system/posix.c"
#endif
