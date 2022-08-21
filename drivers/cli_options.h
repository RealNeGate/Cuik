// https://www.duskborn.com/posts/simple-c-command-line-parser
#ifndef OPTION
#error "Must define OPTION macro"
#endif

OPTION(HELP,       h, help,        0, "print all the options and exit")
OPTION(OUT,        o, output,      1, "set the output path")
OPTION(INCLUDE,    I, include,     1, "add include directory")
OPTION(PREPROC,    P, preprocess,  0, "preprocess file and output to stdout")
OPTION(RUN,        r, run,         0, "execute compiled program")
OPTION(LIB,        l, lib,         1, "add library to compilation unit")
OPTION(NOCRT,      _, nocrt,       0, "don't include and link against the default CRT")
OPTION(TIME,       T, time,        0, "profile the compile times")
OPTION(FLAVOR,     _, flavor,      1, "choose the output file format (`--list flavors` to list options)")
OPTION(OBJ,        c, obj,         0, "dont link, only emit the object file")
OPTION(PPLOC,      _, pploc,       0, "count number of preprocessor lines of code")
OPTION(OPT,        O, optimize,    0, "optimize the generated IR")
OPTION(ASM,        S, assembly,    0, "emit assembly in stdout")
OPTION(AST,        _, ast,         0, "emit AST into stdout")
OPTION(BINDGEN,    _, bindgen,     1, "generates header into stdout")
OPTION(TYPES,      t, typecheck,   0, "type check only")
OPTION(IR,         _, ir,          0, "compile up until the IR generation")
OPTION(TARGET,     _, target,      1, "change the target system and arch (`--list targets` to see the options)")
OPTION(LIST,       _, list,        1, "list some specific properties")
OPTION(VERBOSE,    _, verbose,     0, "verbose")
OPTION(THREADS,    _, threads,     1, "number of extra threads spawned")
OPTION(EXERCISE,   _, exercise,    0, "motion sickness from using a decent compiler")
OPTION(EXPERIMENT, _, experiment,  0, "experimental crap")

#undef OPTION
