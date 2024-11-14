////////////////////////////////////////////////////////////////////////////////
// C Language Front-end
////////////////////////////////////////////////////////////////////////////////

This is Cuik (pronounced Quick), the fast C compiler front-end in the form of a
library. IT IS NOT PRODUCTION-READY YET SO PLEASE DONT ACT LIKE IT IS.

"""
 The world needs better compiler tools, tools which are built as libraries. This
 design point allows reuse of the tools in new and novel ways. However, building
 the tools as libraries isn't enough: they must have clean APIs, be as
 decoupled from each other as possible, and be easy to modify/extend.  This
 requires clean layering, decent design, and avoiding tying the libraries to a
 specific use.  Oh yeah, did I mention that we want the resultant libraries to
 be as fast as possible? :)

    - Chris Lattner, 2007
"""

Code overview:

  AST     - this is the package that provides AST construction, printing and
            walking, it doesn't do parsing itself. depends on Common.

  Lex     - provides a Lexer capable of C (potentially C++) along with keyword
            recognition for C and the preprocessor, depends on Common.

  Parse   - the C parser, does some parsing due to the properties of C but
            only for top-level statements. depends on Preproc.

  Sema    - type checker for C. depends on Parse.

  IRGen   - handles converting type checked AST into TB IR. depends on AST.

  Driver  - handles calling libCuik as an end-to-end program, depends on...
            everything?

Key features:

  * Compiler-as-library design.
  * Faster compilation than Clang.
  * Powerful diagnostics engine.

Active TODO:

  * Redo constant evaluator
  * Fix up API (i wanna make it stable eventually :P)
  * Write C type pretty printers (for diagnostics and for user code)

Future Goals:

  * Incremental parser
  * Support for more versions of C (currently it's C11 N1590 mostly)

////////////////////////////////////////////////////////////////////////////////
// Tilde Backend (Tilde or TB for short)
////////////////////////////////////////////////////////////////////////////////

TB is compiler backend in the form of a C library. This is built as an
alternative to other larger compiler toolchains while providing the
optimizations, machine code generation and object file export functionality
necessary for the development of compilers.

# Roadmap

  Code generation:
    We're starting with x64 but will be moving focus to Aarch64 soon. I wanna

  Optimizer:
    It's almost complete with all the -O1 level passes (mostly missing inlining).
    After that we can move towards -O2 level stuff (the goal is to compete with
    LLVM so we need to be a bit ambitious).

  Debug info:
    Codeview support and DWARF has not yet been started, there's plans on making a
    new debug info format eventually.

  Output targets:
    We currently have basic ELF64, COFF64, some current work is being done for
    PE and Macho-O. We got exporting object files but I wanna go further because
    linkers ain't supposed to be separate programs.

