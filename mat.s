	.text
	.def	@feat.00;
	.scl	3;
	.type	0;
	.endef
	.globl	@feat.00
.set @feat.00, 0
	.file	"mat.c"
	.def	matmul;
	.scl	2;
	.type	32;
	.endef
	.globl	__xmm@80000000800000008000000080000000 # -- Begin function matmul
	.section	.rdata,"dr",discard,__xmm@80000000800000008000000080000000
	.p2align	4, 0x0
__xmm@80000000800000008000000080000000:
	.long	0x80000000                      # float -0
	.long	0x80000000                      # float -0
	.long	0x80000000                      # float -0
	.long	0x80000000                      # float -0
	.text
	.globl	matmul
	.p2align	4, 0x90
matmul:                                 # @matmul
.seh_proc matmul
# %bb.0:
	pushq	%r15
	.seh_pushreg %r15
	pushq	%r14
	.seh_pushreg %r14
	pushq	%r13
	.seh_pushreg %r13
	pushq	%r12
	.seh_pushreg %r12
	pushq	%rsi
	.seh_pushreg %rsi
	pushq	%rdi
	.seh_pushreg %rdi
	pushq	%rbp
	.seh_pushreg %rbp
	pushq	%rbx
	.seh_pushreg %rbx
	subq	$56, %rsp
	.seh_stackalloc 56
	movaps	%xmm8, 32(%rsp)                 # 16-byte Spill
	.seh_savexmm %xmm8, 32
	movaps	%xmm7, 16(%rsp)                 # 16-byte Spill
	.seh_savexmm %xmm7, 16
	movaps	%xmm6, (%rsp)                   # 16-byte Spill
	.seh_savexmm %xmm6, 0
	.seh_endprologue
	testq	%rcx, %rcx
	je	.LBB0_9
# %bb.1:
	xorl	%eax, %eax
	movaps	__xmm@80000000800000008000000080000000(%rip), %xmm0 # xmm0 = [-0.0E+0,-0.0E+0,-0.0E+0,-0.0E+0]
	.p2align	4, 0x90
.LBB0_2:                                # =>This Inner Loop Header: Depth=1
	movups	%xmm0, (%rcx,%rax)
	movups	%xmm0, 16(%rcx,%rax)
	movups	%xmm0, 32(%rcx,%rax)
	movups	%xmm0, 48(%rcx,%rax)
	movups	%xmm0, 64(%rcx,%rax)
	movups	%xmm0, 80(%rcx,%rax)
	movups	%xmm0, 96(%rcx,%rax)
	movups	%xmm0, 112(%rcx,%rax)
	subq	$-128, %rax
	cmpq	$1024, %rax                     # imm = 0x400
	jne	.LBB0_2
# %bb.3:
	addq	$12, %rdx
	xorl	%eax, %eax
	.p2align	4, 0x90
.LBB0_4:                                # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_5 Depth 2
                                        #       Child Loop BB0_6 Depth 3
	movq	%rax, %r9
	shlq	$4, %r9
	movq	%r9, %r10
	orq	$16, %r10
	movq	%r9, %r11
	orq	$32, %r11
	movq	%r9, %rsi
	orq	$48, %rsi
	movq	%rcx, %rdi
	xorl	%ebx, %ebx
	.p2align	4, 0x90
.LBB0_5:                                #   Parent Loop BB0_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_6 Depth 3
	movq	%rbx, %r14
	orq	%r9, %r14
	movq	%rbx, %r15
	orq	%r10, %r15
	movq	%rbx, %r12
	orq	%r11, %r12
	movq	%rbx, %r13
	orq	%rsi, %r13
	xorl	%ebp, %ebp
	.p2align	4, 0x90
.LBB0_6:                                #   Parent Loop BB0_4 Depth=1
                                        #     Parent Loop BB0_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movups	(%r8,%r14,4), %xmm0
	movups	(%r8,%r15,4), %xmm1
	movups	(%r8,%r12,4), %xmm2
	movups	(%r8,%r13,4), %xmm3
	movups	(%rdi,%rbp), %xmm4
	movss	-12(%rdx,%rbp), %xmm5           # xmm5 = mem[0],zero,zero,zero
	movss	-8(%rdx,%rbp), %xmm6            # xmm6 = mem[0],zero,zero,zero
	movss	-4(%rdx,%rbp), %xmm7            # xmm7 = mem[0],zero,zero,zero
	movss	(%rdx,%rbp), %xmm8              # xmm8 = mem[0],zero,zero,zero
	shufps	$0, %xmm5, %xmm5                # xmm5 = xmm5[0,0,0,0]
	mulps	%xmm0, %xmm5
	addps	%xmm4, %xmm5
	shufps	$0, %xmm6, %xmm6                # xmm6 = xmm6[0,0,0,0]
	mulps	%xmm1, %xmm6
	addps	%xmm5, %xmm6
	shufps	$0, %xmm7, %xmm7                # xmm7 = xmm7[0,0,0,0]
	mulps	%xmm2, %xmm7
	addps	%xmm6, %xmm7
	shufps	$0, %xmm8, %xmm8                # xmm8 = xmm8[0,0,0,0]
	mulps	%xmm3, %xmm8
	addps	%xmm7, %xmm8
	movups	%xmm8, (%rdi,%rbp)
	addq	$64, %rbp
	cmpq	$1024, %rbp                     # imm = 0x400
	jne	.LBB0_6
# %bb.7:                                #   in Loop: Header=BB0_5 Depth=2
	leaq	4(%rbx), %r14
	addq	$16, %rdi
	cmpq	$12, %rbx
	movq	%r14, %rbx
	jb	.LBB0_5
# %bb.8:                                #   in Loop: Header=BB0_4 Depth=1
	leaq	4(%rax), %r9
	addq	$16, %rdx
	cmpq	$12, %rax
	movq	%r9, %rax
	jb	.LBB0_4
.LBB0_9:
	movaps	(%rsp), %xmm6                   # 16-byte Reload
	movaps	16(%rsp), %xmm7                 # 16-byte Reload
	movaps	32(%rsp), %xmm8                 # 16-byte Reload
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	popq	%rdi
	popq	%rsi
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
	.seh_endproc
                                        # -- End function
	.addrsig
	.globl	_fltused
