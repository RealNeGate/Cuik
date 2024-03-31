X(HELP,        "h",        false, "print help")
X(VERBOSE,     "V",        false, "print verbose messages")
X(LIVE,        "live",     false, "runs compiler persistently, repeats compile step whenever the input changes")
// preprocessor
X(DEFINE,      "D",        true,  "defines a macro before compiling")
X(UNDEF,       "U",        true,  "undefines a macro before compiling")
X(INCLUDE,     "I",        true,  "add directory to the include searches")
X(PPTEST,      "Pp",       false, "test preprocessor")
X(PP,          "P",        false, "print preprocessor output to stdout")
// parser
X(LANG,        "lang",     true,  "choose the language (c11, c23, glsl)")
X(AST,         "ast",      false, "print AST into stdout")
X(SYNTAX,      "xe",       false, "type check only")
// dep tracking
X(DEPS,        "MD",       false, "write header dependencies")
X(DEPFILE,     "MF",       true,  "write depout from -MD into a file")
// optimizer
X(OPTLVL,      "O",        false,  "no optimizations")
// backend
X(EMITIR,      "emit-ir",  false, "print IR into stdout")
X(EMITDOT,     "emit-dot", false, "print graphviz into stdout")
X(OUTPUT,      "o",        true,  "set the output filepath")
X(OBJECT,      "c",        false, "output object file")
X(ASSEMBLY,    "S",        false, "output assembly to stdout")
X(DEBUG,       "g",        false, "compile with debug information")
// linker
X(NOLIBC,      "nostdlib", false, "don't include and link against the default CRT")
X(LIB,         "l",        true,  "add library name to the linking")
X(LIBDIR,      "L",        true,  "add library directory to search paths")
X(BASED,       "based",    false, "use the TB linker (EXPERIMENTAL)")
X(SUBSYSTEM,   "subsystem",true,  "set windows subsystem (windows only... of course)")
X(ENTRY,       "e",        true,  "set entrypoint")
// misc
X(TARGET,      "target",   true,  "change the target system and arch")
X(THREADS,     "j",        false, "enabled multithreaded compilation")
X(TIME,        "T",        false, "profile the compile times")
X(THINK,       "think",    false, "aids in thinking about serious problems")
// run
X(RUN,         "r",        false, "JIT the executable (NOT READY)")
#undef X
