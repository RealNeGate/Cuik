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
	.globl	matmul                          # -- Begin function matmul
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
	subq	$120, %rsp
	.seh_stackalloc 120
	.seh_endprologue
	movq	%rcx, 112(%rsp)                 # 8-byte Spill
	movl	(%rdx), %r14d
	movl	4(%rdx), %r13d
	movl	(%r8), %ecx
	movl	%ecx, 52(%rsp)                  # 4-byte Spill
	movl	4(%r8), %r10d
	movl	%r10d, 56(%rsp)                 # 4-byte Spill
	movl	8(%rdx), %edi
	movl	%edi, 68(%rsp)                  # 4-byte Spill
	movl	8(%r8), %eax
	movl	%eax, 60(%rsp)                  # 4-byte Spill
	movl	12(%rdx), %r9d
	movl	12(%r8), %r15d
	movl	%r15d, 64(%rsp)                 # 4-byte Spill
	movl	16(%rdx), %r11d
	movl	%r11d, 80(%rsp)                 # 4-byte Spill
	imull	%r14d, %ecx
	movl	%r10d, %esi
	imull	%r13d, %esi
	addl	%ecx, %esi
	movl	%eax, %ecx
	imull	%edi, %ecx
	imull	%r9d, %r15d
	addl	%ecx, %r15d
	movl	20(%rdx), %ebx
	addl	%esi, %r15d
	movl	24(%rdx), %eax
	movl	%eax, 72(%rsp)                  # 4-byte Spill
	movl	28(%rdx), %r10d
	movl	32(%rdx), %eax
	movl	%eax, 84(%rsp)                  # 4-byte Spill
	movl	36(%rdx), %eax
	movl	%eax, 88(%rsp)                  # 4-byte Spill
	movl	40(%rdx), %eax
	movl	%eax, 92(%rsp)                  # 4-byte Spill
	movl	44(%rdx), %eax
	movl	%eax, 76(%rsp)                  # 4-byte Spill
	movl	48(%rdx), %eax
	movl	%eax, 96(%rsp)                  # 4-byte Spill
	movl	52(%rdx), %eax
	movl	%eax, 100(%rsp)                 # 4-byte Spill
	movl	56(%rdx), %eax
	movl	%eax, 104(%rsp)                 # 4-byte Spill
	movl	60(%rdx), %eax
	movl	%eax, 108(%rsp)                 # 4-byte Spill
	movl	16(%r8), %eax
	movl	%eax, 16(%rsp)                  # 4-byte Spill
	movl	20(%r8), %ecx
	movl	%ecx, 20(%rsp)                  # 4-byte Spill
	movl	24(%r8), %edx
	movl	%edx, 24(%rsp)                  # 4-byte Spill
	movl	28(%r8), %r11d
	movl	%r11d, 32(%rsp)                 # 4-byte Spill
	movl	32(%r8), %esi
	movl	%esi, 40(%rsp)                  # 4-byte Spill
	movl	36(%r8), %edi
	movl	%edi, 8(%rsp)                   # 4-byte Spill
	movl	40(%r8), %ebp
	movl	%ebp, 44(%rsp)                  # 4-byte Spill
	movl	44(%r8), %r12d
	movl	%r12d, 28(%rsp)                 # 4-byte Spill
	movl	48(%r8), %r12d
	movl	%r12d, 4(%rsp)                  # 4-byte Spill
	movl	52(%r8), %r12d
	movl	%r12d, 12(%rsp)                 # 4-byte Spill
	movl	56(%r8), %r12d
	movl	%r12d, 36(%rsp)                 # 4-byte Spill
	movl	60(%r8), %r8d
	movl	%r8d, 48(%rsp)                  # 4-byte Spill
	movq	112(%rsp), %r12                 # 8-byte Reload
	movl	%r15d, (%r12)
	movl	%eax, %r15d
	imull	%r14d, %r15d
	movl	%ecx, %eax
	imull	%r13d, %eax
	addl	%r15d, %eax
	movl	%edx, %r15d
	movl	68(%rsp), %ecx                  # 4-byte Reload
	imull	%ecx, %r15d
	imull	%r9d, %r11d
	addl	%r15d, %r11d
	addl	%eax, %r11d
	movl	%r11d, 4(%r12)
	imull	%r14d, %esi
	imull	%r13d, %edi
	addl	%esi, %edi
	imull	%ecx, %ebp
	movl	%ecx, %esi
	movl	28(%rsp), %ecx                  # 4-byte Reload
	movl	%ecx, %r15d
	imull	%r9d, %r15d
	addl	%ebp, %r15d
	addl	%edi, %r15d
	movl	%r15d, 8(%r12)
	imull	4(%rsp), %r14d                  # 4-byte Folded Reload
	movl	12(%rsp), %r15d                 # 4-byte Reload
	imull	%r15d, %r13d
	addl	%r14d, %r13d
	movl	36(%rsp), %r11d                 # 4-byte Reload
	movl	%esi, %eax
	imull	%r11d, %eax
	imull	%r8d, %r9d
	addl	%eax, %r9d
	addl	%r13d, %r9d
	movl	%r9d, 12(%r12)
	movl	80(%rsp), %r8d                  # 4-byte Reload
	movl	%r8d, %eax
	imull	52(%rsp), %eax                  # 4-byte Folded Reload
	movl	%ebx, %edx
	imull	56(%rsp), %edx                  # 4-byte Folded Reload
	addl	%eax, %edx
	movl	72(%rsp), %ebp                  # 4-byte Reload
	movl	%ebp, %eax
	imull	60(%rsp), %eax                  # 4-byte Folded Reload
	movl	%r10d, %r9d
	imull	64(%rsp), %r9d                  # 4-byte Folded Reload
	addl	%eax, %r9d
	addl	%edx, %r9d
	movl	%r9d, 16(%r12)
	movl	16(%rsp), %eax                  # 4-byte Reload
	imull	%r8d, %eax
	movl	20(%rsp), %edx                  # 4-byte Reload
	imull	%ebx, %edx
	addl	%eax, %edx
	movl	24(%rsp), %eax                  # 4-byte Reload
	imull	%ebp, %eax
	movl	%ebp, %edi
	movl	32(%rsp), %esi                  # 4-byte Reload
	movl	%esi, %r9d
	imull	%r10d, %r9d
	addl	%eax, %r9d
	addl	%edx, %r9d
	movl	%r9d, 20(%r12)
	movl	40(%rsp), %ebp                  # 4-byte Reload
	movl	%ebp, %eax
	imull	%r8d, %eax
	movl	8(%rsp), %edx                   # 4-byte Reload
	imull	%ebx, %edx
	addl	%eax, %edx
	movl	44(%rsp), %r13d                 # 4-byte Reload
	movl	%r13d, %eax
	imull	%edi, %eax
	movl	%ecx, %r9d
	imull	%r10d, %r9d
	addl	%eax, %r9d
	addl	%edx, %r9d
	movl	%r9d, 24(%r12)
	movl	%r8d, %eax
	imull	4(%rsp), %eax                   # 4-byte Folded Reload
	imull	%r15d, %ebx
	addl	%eax, %ebx
	movl	%edi, %eax
	imull	%r11d, %eax
	imull	48(%rsp), %r10d                 # 4-byte Folded Reload
	addl	%eax, %r10d
	addl	%ebx, %r10d
	movl	%r10d, 28(%r12)
	movl	84(%rsp), %r9d                  # 4-byte Reload
	movl	%r9d, %eax
	movl	52(%rsp), %ebx                  # 4-byte Reload
	imull	%ebx, %eax
	movl	88(%rsp), %r8d                  # 4-byte Reload
	movl	%r8d, %ecx
	movl	56(%rsp), %r11d                 # 4-byte Reload
	imull	%r11d, %ecx
	addl	%eax, %ecx
	movl	92(%rsp), %r10d                 # 4-byte Reload
	movl	%r10d, %eax
	movl	60(%rsp), %r15d                 # 4-byte Reload
	imull	%r15d, %eax
	movl	76(%rsp), %r14d                 # 4-byte Reload
	movl	%r14d, %edx
	movl	64(%rsp), %edi                  # 4-byte Reload
	imull	%edi, %edx
	addl	%eax, %edx
	addl	%ecx, %edx
	movl	%edx, 32(%r12)
	movl	%r9d, %eax
	imull	16(%rsp), %eax                  # 4-byte Folded Reload
	movl	%r8d, %ecx
	imull	20(%rsp), %ecx                  # 4-byte Folded Reload
	addl	%eax, %ecx
	movl	%r10d, %eax
	imull	24(%rsp), %eax                  # 4-byte Folded Reload
	movl	%r14d, %edx
	imull	%esi, %edx
	addl	%eax, %edx
	addl	%ecx, %edx
	movl	%edx, 36(%r12)
	imull	%r9d, %ebp
	movl	8(%rsp), %ecx                   # 4-byte Reload
	imull	%r8d, %ecx
	addl	%ebp, %ecx
	imull	%r10d, %r13d
	movl	28(%rsp), %ebp                  # 4-byte Reload
	movl	%ebp, %edx
	imull	%r14d, %edx
	addl	%r13d, %edx
	addl	%ecx, %edx
	movl	%edx, 40(%r12)
	imull	4(%rsp), %r9d                   # 4-byte Folded Reload
	imull	12(%rsp), %r8d                  # 4-byte Folded Reload
	addl	%r9d, %r8d
	movl	%r8d, %eax
	movl	%r10d, %ecx
	movl	36(%rsp), %r8d                  # 4-byte Reload
	imull	%r8d, %ecx
	movl	48(%rsp), %r13d                 # 4-byte Reload
	movl	%r14d, %edx
	imull	%r13d, %edx
	addl	%ecx, %edx
	addl	%eax, %edx
	movl	%edx, 44(%r12)
	movl	96(%rsp), %esi                  # 4-byte Reload
	imull	%esi, %ebx
	movl	%r11d, %eax
	movl	100(%rsp), %r11d                # 4-byte Reload
	imull	%r11d, %eax
	addl	%ebx, %eax
	movl	104(%rsp), %r10d                # 4-byte Reload
	imull	%r10d, %r15d
	movl	108(%rsp), %r9d                 # 4-byte Reload
	imull	%r9d, %edi
	addl	%r15d, %edi
	addl	%eax, %edi
	movl	%edi, 48(%r12)
	movl	16(%rsp), %ecx                  # 4-byte Reload
	imull	%esi, %ecx
	movl	20(%rsp), %eax                  # 4-byte Reload
	imull	%r11d, %eax
	addl	%ecx, %eax
	movl	24(%rsp), %ecx                  # 4-byte Reload
	imull	%r10d, %ecx
	movl	32(%rsp), %edx                  # 4-byte Reload
	imull	%r9d, %edx
	addl	%ecx, %edx
	addl	%eax, %edx
	movl	%edx, 52(%r12)
	movl	40(%rsp), %ecx                  # 4-byte Reload
	imull	%esi, %ecx
	movl	8(%rsp), %eax                   # 4-byte Reload
	imull	%r11d, %eax
	addl	%ecx, %eax
	movl	44(%rsp), %ecx                  # 4-byte Reload
	imull	%r10d, %ecx
	imull	%r9d, %ebp
	addl	%ecx, %ebp
	addl	%eax, %ebp
	movl	%ebp, 56(%r12)
	movl	4(%rsp), %ecx                   # 4-byte Reload
	imull	%esi, %ecx
	movl	12(%rsp), %eax                  # 4-byte Reload
	imull	%r11d, %eax
	addl	%ecx, %eax
	movl	%r8d, %ecx
	imull	%r10d, %ecx
	movl	%r13d, %edx
	imull	%r9d, %edx
	addl	%ecx, %edx
	addl	%eax, %edx
	movl	%edx, 60(%r12)
	addq	$120, %rsp
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
