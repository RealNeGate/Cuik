

\ global
var: x ;
var: y ;

\ ( w h -- )
: update
	drop drop

	\ draw shit
	200 300 200 200 rect

	x@ y@ 50 50 rect

	\ advance
	x@ 1 + x!
;

