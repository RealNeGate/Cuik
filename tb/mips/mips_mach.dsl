
( this DSL is a forth! )
( sample MIPS code )
: j-type ( op imm -- ) 0x3FFFFFF and swap 26 shl or emit-4 ;

( just tells us that the next words will be instruction-ified )
export

: jal  ( imm -- ) 3 swap j-type ;
: beq  ( imm -- ) 4 swap j-type ;
: bne  ( imm -- ) 5 swap j-type ;
: blez ( imm -- ) 6 swap j-type ;
: bgtz ( imm -- ) 7 swap j-type ;




