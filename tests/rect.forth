

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
\ : loop dup if{ 65 emit 1 - loop }else{ drop } ;




\ 16 foo . 32 emit
\ 4  foo . 32 emit
\ 7  foo . 32 emit
\ 16 foo . 32 emit
\ 4  foo . 32 emit


\ 10 emit
\ 10 loop

