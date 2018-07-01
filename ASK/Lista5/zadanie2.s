	.file	"zadanie2.c"
#APP
	.globl _original_func
_original_func:
movq %rdi, %rax
.myL3:
leaq 1(%rax), %r8
movb -1(%r8), %r9b
movq %rsi, %rdx
.myL2:
incq %rdx
movb -1(%rdx), %cl
testb %cl, %cl
je .myL7
cmpb %cl, %r9b
jne .myL2
movq %r8, %rax
jmp .myL3
.myL7:
subq %rdi, %rax
ret

#NO_APP
	.text
	.globl	my_func
	.type	my_func, @function
my_func:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -8(%rbp)
.L5:
	movq	-8(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movzbl	-1(%rax), %eax
	movb	%al, -17(%rbp)
	movq	-64(%rbp), %rax
	movq	%rax, -32(%rbp)
	addq	$1, -32(%rbp)
	movq	-32(%rbp), %rax
	movzbl	-1(%rax), %eax
	movb	%al, -33(%rbp)
	cmpb	$0, -33(%rbp)
	je	.L7
	movzbl	-17(%rbp), %eax
	cmpb	-33(%rbp), %al
	jne	.L8
	movq	-16(%rbp), %rax
	movq	%rax, -8(%rbp)
	jmp	.L5
.L7:
	nop
	jmp	.L3
.L8:
	nop
.L3:
	movq	-8(%rbp), %rdx
	movq	-56(%rbp), %rax
	subq	%rax, %rdx
	movq	%rdx, %rax
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	my_func, .-my_func
	.section	.rodata
.LC0:
	.string	"abcdef"
.LC1:
	.string	"abcdefa"
.LC2:
	.string	"%d\n"
.LC3:
	.string	"%d\n\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	$.LC0, -8(%rbp)
	movq	$.LC1, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	my_func
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movq	-8(%rbp), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	my_func
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movq	-16(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	my_func
	movl	%eax, %esi
	movl	$.LC3, %edi
	movl	$0, %eax
	call	printf
	movq	-16(%rbp), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	original_func
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movq	-8(%rbp), %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	original_func
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movq	-16(%rbp), %rdx
	movq	-16(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	original_func
	movl	%eax, %esi
	movl	$.LC2, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
