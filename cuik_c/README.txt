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

  * Fix up API (i wanna make it stable eventually :P)

Future Goals:

  * Incremental parser
  * Support for more versions of C (currently it's C11 N1590 mostly)

