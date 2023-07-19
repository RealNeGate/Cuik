

\ simple stack effects
: foo 3 4 + + ;





\ simple condition
: bar if{ 65 emit }else{ 66 emit } ;
: baz 65 emit ;

baz baz baz baz baz baz



16 bar
0  bar
16 bar
0  bar
16 bar
0  bar


\ simple loop
: loop  dup if{ dup 64 + emit 1 - loop }else{ drop } ;
: loop2 dup if{ dup . 10 loop 10 emit 1 - loop2 }else{ drop } ;


: test . ;

10 test 5 test 3 test

10 emit 10 emit

1 loop2
