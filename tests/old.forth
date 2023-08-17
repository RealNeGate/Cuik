\ run game loop
: update
	key . 10 emit
;

\ rendering
: draw_line   dup if{ '#' emit 1 - tail }else{ drop } ;
: draw_screen dup if{ 80 draw_line 10 emit 1 - tail }else{ drop } ;
: draw
	\ clear screen (i love escape codes)
	27 emit '[' emit '2' emit '6' emit 'F' emit 27 emit '[' emit '0' emit 'J' emit
	\ draw now
	25 draw_screen
;

: game-loop update draw tail ;
\ game-loop

