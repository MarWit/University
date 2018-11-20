	.file	"zadanie1.c"
	.globl	bufp0
	.data
	.align 8
	.type	bufp0, @object
	.size	bufp0, 8
bufp0:
	.quad	buf
	.local	bufp1
	.comm	bufp1,8,8
	.text
	.type	incr, @function
incr:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	count.1760(%rip), %eax
	addl	$1, %eax
	movl	%eax, count.1760(%rip)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	incr, .-incr
	.globl	swap
	.type	swap, @function
swap:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$0, %eax
	call	incr
	movq	$buf+4, bufp1(%rip)
	movq	bufp0(%rip), %rax
	movl	(%rax), %eax
	movl	%eax, -4(%rbp)
	movq	bufp0(%rip), %rax
	movq	bufp1(%rip), %rdx
	movl	(%rdx), %edx
	movl	%edx, (%rax)
	movq	bufp1(%rip), %rax
	movl	-4(%rbp), %edx
	movl	%edx, (%rax)
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	swap, .-swap
	.local	count.1760
	.comm	count.1760,4,4
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
