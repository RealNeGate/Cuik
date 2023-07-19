  $ PARSECMD="$TESTDIR/../parse -std c11 -atomic-permissive-syntax"
  $ $PARSECMD < $TESTDIR/argument_scope.c
  $ $PARSECMD < $TESTDIR/atomic.c
  $ $PARSECMD < $TESTDIR/c11-noreturn.c
  $ $PARSECMD < $TESTDIR/c1x-alignas.c
  $ $PARSECMD < $TESTDIR/char-literal-printing.c
  $ $PARSECMD < $TESTDIR/c-namespace.c
  $ $PARSECMD < $TESTDIR/control-scope.c
  $ $PARSECMD < $TESTDIR/declarators.c
  $ $PARSECMD < $TESTDIR/designator.c
  $ $PARSECMD < $TESTDIR/expressions.c
  $ $PARSECMD < $TESTDIR/long-long-struct.c
  $ $PARSECMD < $TESTDIR/function-decls.c
  $ $PARSECMD < $TESTDIR/statements.c
  $ $PARSECMD < $TESTDIR/struct-recursion.c
  $ $PARSECMD < $TESTDIR/types.c
  $ $PARSECMD < $TESTDIR/local_typedef.c
  $ $PARSECMD < $TESTDIR/declaration_ambiguity.c
  $ $PARSECMD < $TESTDIR/declarator_visibility.c
  $ $PARSECMD < $TESTDIR/enum_shadows_typedef.c
  $ $PARSECMD < $TESTDIR/enum_constant_visibility.c
  $ $PARSECMD < $TESTDIR/namespaces.c
  $ $PARSECMD < $TESTDIR/local_scope.c
  $ $PARSECMD < $TESTDIR/block_scope.c
  $ $PARSECMD < $TESTDIR/if_scopes.c
  $ $PARSECMD < $TESTDIR/loop_scopes.c
  $ $PARSECMD < $TESTDIR/no_local_scope.c
  $ $PARSECMD < $TESTDIR/function_parameter_scope.c
  $ $PARSECMD < $TESTDIR/function_parameter_scope_extends.c
  $ $PARSECMD < $TESTDIR/dangling_else.c
SHOULD FAIL:
  $ $PARSECMD < $TESTDIR/dangling_else_misleading.fail.c
  Fatal error: exception Parser.MenhirBasics.Error
  [2]
  $ $PARSECMD < $TESTDIR/dangling_else_lookahead.c
  $ $PARSECMD < $TESTDIR/dangling_else_lookahead.if.c
  $ $PARSECMD < $TESTDIR/parameter_declaration_ambiguity.c
  $ $PARSECMD < $TESTDIR/parameter_declaration_ambiguity.test.c
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.c
SHOULD FAIL (does not because we do not have a semantic analysis):
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.fail.c
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.ok.c
  $ $PARSECMD < $TESTDIR/atomic_parenthesis.c
  $ $PARSECMD < $TESTDIR/aligned_struct_c18.c
  $ ls $TESTDIR/*.c
  */tests/aligned_struct_c18.c (glob)
  */tests/argument_scope.c (glob)
  */tests/atomic.c (glob)
  */tests/atomic_parenthesis.c (glob)
  */tests/bitfield_declaration_ambiguity.c (glob)
  */tests/bitfield_declaration_ambiguity.fail.c (glob)
  */tests/bitfield_declaration_ambiguity.ok.c (glob)
  */tests/block_scope.c (glob)
  */tests/c-namespace.c (glob)
  */tests/c11-noreturn.c (glob)
  */tests/c1x-alignas.c (glob)
  */tests/char-literal-printing.c (glob)
  */tests/control-scope.c (glob)
  */tests/dangling_else.c (glob)
  */tests/dangling_else_lookahead.c (glob)
  */tests/dangling_else_lookahead.if.c (glob)
  */tests/dangling_else_misleading.fail.c (glob)
  */tests/declaration_ambiguity.c (glob)
  */tests/declarator_visibility.c (glob)
  */tests/declarators.c (glob)
  */tests/designator.c (glob)
  */tests/enum-trick.c (glob)
  */tests/enum.c (glob)
  */tests/enum_constant_visibility.c (glob)
  */tests/enum_shadows_typedef.c (glob)
  */tests/expressions.c (glob)
  */tests/function-decls.c (glob)
  */tests/function_parameter_scope.c (glob)
  */tests/function_parameter_scope_extends.c (glob)
  */tests/if_scopes.c (glob)
  */tests/local_scope.c (glob)
  */tests/local_typedef.c (glob)
  */tests/long-long-struct.c (glob)
  */tests/loop_scopes.c (glob)
  */tests/namespaces.c (glob)
  */tests/no_local_scope.c (glob)
  */tests/parameter_declaration_ambiguity.c (glob)
  */tests/parameter_declaration_ambiguity.test.c (glob)
  */tests/statements.c (glob)
  */tests/struct-recursion.c (glob)
  */tests/typedef_star.c (glob)
  */tests/types.c (glob)
  */tests/variable_star.c (glob)
