	.file	"zadanie3.c"
	.text
	.globl	relo3
	.type	relo3, @function
relo3:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	subl	$100, %eax
	cmpl	$5, %eax
	ja	.L2
	movl	%eax, %eax
	movq	.L4(,%rax,8), %rax
	jmp	*%rax
	.section	.rodata
	.align 8
	.align 4
.L4:
	.quad	.L3
	.quad	.L5
	.quad	.L2
	.quad	.L6
	.quad	.L6
	.quad	.L7
	.text
.L3:
	movl	-4(%rbp), %eax
	jmp	.L8
.L5:
	movl	-4(%rbp), %eax
	addl	$1, %eax
	jmp	.L8
.L6:
	movl	-4(%rbp), %eax
	addl	$3, %eax
	jmp	.L8
.L7:
	movl	-4(%rbp), %eax
	addl	$5, %eax
	jmp	.L8
.L2:
	movl	-4(%rbp), %eax
	addl	$6, %eax
.L8:
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	relo3, .-relo3
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
