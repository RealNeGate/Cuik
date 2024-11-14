

(case n
	(TB_CYCLE_COUNTER
		(emit #x0F #x31) ; rdtsc
		(shl  rdx  32  )
		(or   rax  rdx ))
	(x86_vzero
		(vxor xmm0 xmm0 :ps)))


