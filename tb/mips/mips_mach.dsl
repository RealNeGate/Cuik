
( this DSL is a forth! )
( sample MIPS code )

: mask-16 0xFFFF and ;
: mask-5  0x1F   and ;
: mask-6  0x3F   and ;

:reg GPR
    $zr $at $v0 $v1 $a0 $a1 $a2 $a3
    $t0 $t1 $t2 $t3 $t4 $t5 $t6 $t7
    $s0 $s1 $s2 $s3 $s4 $s5 $s6 $s7
    $t8 $t9 $k0 $k1 $gp $sp $fp $ra
;

: j-type ( imm op       -- )
    mask-6 26 shl
    swap 0x3FFFFFF and or
    emit-4 ;

: i-type ( rt rs imm op -- )
         mask-6 26 shl
    swap mask-16       or
    swap mask-5 21 shl or
    swap mask-5 16 shl or
    emit-4 ;

: r-type ( rt rs rd imm op funct -- )
         mask-6
    swap mask-6 26 shl or
    swap mask-5 21 shl or
    swap mask-5 16 shl or
    swap mask-5 11 shl or
    emit-4 ;

: j-syntax { name@ " AAA" cat } ;
: i-syntax { name@ " AAA" cat } ;
: r-syntax { name@ " AAA" cat } ;

:inst jal   j-syntax 0b000011          j-type ;

:inst beq   i-syntax 0b000100          i-type ;
:inst bne   i-syntax 0b000101          i-type ;
:inst blez  i-syntax 0b000110          i-type ;
:inst bgtz  i-syntax 0b000111          i-type ;

:inst sll   r-syntax 0b000000 0b000000 r-type ;
:inst srl   r-syntax 0b000000 0b000010 r-type ;
:inst dsll  r-syntax 0b000000 0b111000 r-type ;
:inst dsrl  r-syntax 0b000000 0b111010 r-type ;
:inst sllv  r-syntax 0b000000 0b000100 r-type ;
:inst srlv  r-syntax 0b000000 0b000110 r-type ;
:inst srav  r-syntax 0b000000 0b000111 r-type ;
:inst sra   r-syntax 0b000000 0b000011 r-type ;
:inst add   r-syntax 0b000000 0b100000 r-type ;
:inst addu  r-syntax 0b000000 0b100001 r-type ;
:inst dadd  r-syntax 0b000000 0b101100 r-type ;
:inst daddu r-syntax 0b000000 0b101101 r-type ;
:inst sub   r-syntax 0b000000 0b100010 r-type ;
:inst subu  r-syntax 0b000000 0b100011 r-type ;
:inst dsub  r-syntax 0b000000 0b101110 r-type ;
:inst dsubu r-syntax 0b000000 0b101111 r-type ;
:inst and   r-syntax 0b000000 0b100100 r-type ;
:inst or    r-syntax 0b000000 0b100101 r-type ;
:inst xor   r-syntax 0b000000 0b100110 r-type ;
:inst nor   r-syntax 0b000000 0b100111 r-type ;
:inst jr    r-syntax 0b000000 0b001000 r-type ;

:inst mul   r-syntax 0b011100 0b000010 r-type ;
:inst adds  r-syntax 0b010001 0b000000 r-type ;


