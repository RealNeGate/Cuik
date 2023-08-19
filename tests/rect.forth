

\ global
var: x ;
var: y ;


\ rectangles
\ record: rect var: x y w h ; ;

: sprite x@ swap 50 50 draw-rect ;
: sprites dup if{ 1 - dup 75 * sprite tail }else{ drop } ;

\ ( w h -- )
: update
	drop drop

	\ 200 300 200 200 rect

	\ draw shit
	200 300 200 200 draw-rect

	10 sprites

	\ advance
	x@ 1 + x!
;
