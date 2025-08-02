// name        short       fmt    desc
X(VERBOSE,     "V",        NULL,         "print verbose messages")
X(LIVE,        "live",     NULL,         "runs compiler persistently, repeats compile step whenever the input changes")
// preprocessor
X(DEFINE,      "D",        &CLI_STR,     "defines a macro before compiling")
X(UNDEF,       "U",        &CLI_STR,     "undefines a macro before compiling")
X(INCLUDE,     "I",        &CLI_STR,     "add directory to the include searches")
X(PPTEST,      "Pp",       NULL,         "test preprocessor")
X(PP,          "P",        NULL,         "print preprocessor output to stdout")
// parser
X(LANG,        "lang",     LANG_ENUMS,   "choose the language")
X(AST,         "ast",      NULL,         "print AST into stdout")
X(SYNTAX,      "xe",       NULL,         "type check only")
// dep tracking
X(DEPS,        "MD",       NULL,         "write header dependencies")
X(DEPFILE,     "MF",       &CLI_STR,     "write depout from -MD into a file")
// optimizer
X(OPTLVL,      "O",        NULL,         "enabled the optimizer")
// backend
X(EMITIR,      "emit-ir",  NULL,         "print IR into stdout")
X(OUTPUT,      "o",        &CLI_STR,     "set the output filepath")
X(OBJECT,      "c",        NULL,         "output object file")
X(ASSEMBLY,    "S",        NULL,         "output assembly to stdout")
X(DEBUG,       "g",        NULL,         "compile with debug information")
X(NOCHKSTK,    "nochkstk", NULL,         "disable buffer checks (think of MSVC's /GS-)")
// linker
X(NOLIBC,      "nostdlib", NULL,         "don't include and link against the default CRT")
X(LIB,         "l",        &CLI_STR,     "add library name to the linking")
X(LIBDIR,      "L",        &CLI_STR,     "add library directory to search paths")
X(BASED,       "based",    NULL,         "use the TB linker (EXPERIMENTAL)")
X(SUBSYSTEM,   "subsystem",SUBSYS_ENUMS, "set windows subsystem (windows only... of course)")
X(const char*, ENTRY,       "e",        &CLI_STR,     "set entrypoint")
// misc
X(int,  target,      "target",   TARGET_ENUMS, "change the target system and arch")
X(int,  threads,     "j",        &CLI_OPT_NUM, "enabled multithreaded compilation")
X(bool, time,        "T",        NULL,         "")
X(bool, time_report,  "t",        NULL,         "")
X(, think,       "think",    NULL,         "aids in thinking about serious problems")
// run
OPT_BOOL(run,         "r",        NULL,         "JIT the executable (NOT READY)")
#undef X
