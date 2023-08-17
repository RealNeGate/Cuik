
\ simple stack effects
: foo 3 4 + + ;

\ simple condition
: bar if{ 65 emit }else{ 66 emit } ;
: baz 65 emit ;

\ dropped window dimensions
drop drop

200 100 400 200 rect
