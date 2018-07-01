	.file	"zadanie8.c"
	.intel_syntax noprefix
#APP
	.globl eval
eval:
mov rax, rdi
mov rcx, [rsp + 16]
mov rdx, [rsp + 24]
mov rsi, [rdx]
mov rdx, rcx
imul rdx, rsi
mov [rdi], rdx
mov rdx, [rsp + 8]
mov rsi, rdx
sub rdi, rsi
mov [rax + 8], rdi
sub rdx, rcx
mov [rax + 16], rdx
ret

#NO_APP
	.text
	.globl	my_eval
	.type	my_eval, @function
my_eval:
.LFB0:
	.cfi_startproc
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR [rbp-40], rdi
	mov	rax, QWORD PTR [rbp-40]
	mov	rdx, QWORD PTR [rbp-32]
	mov	QWORD PTR [rax], rdx
	mov	rdx, QWORD PTR [rbp-24]
	mov	QWORD PTR [rax+8], rdx
	mov	rdx, QWORD PTR [rbp-16]
	mov	QWORD PTR [rax+16], rdx
	mov	rax, QWORD PTR [rbp-40]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	my_eval, .-my_eval
#APP
	.globl wrap
wrap:
sub rsp, 72
mov [rsp], rdx
mov rdx, rsp
lea rax, [rsp + 8]
push rdx
push rsi
push rdi
mov rdi, rax
call eval
mov rax, [rsp + 40]
add rax, [rsp + 32]
imul rax, [rsp + 48]
add rsp, 96
ret

#NO_APP
	.globl	my_wrap
	.type	my_wrap, @function
my_wrap:
.LFB1:
	.cfi_startproc
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 96
	mov	QWORD PTR [rbp-72], rdi
	mov	QWORD PTR [rbp-80], rsi
	mov	QWORD PTR [rbp-88], rdx
	mov	rax, QWORD PTR [rbp-72]
	mov	QWORD PTR [rbp-32], rax
	mov	rax, QWORD PTR [rbp-80]
	mov	QWORD PTR [rbp-24], rax
	lea	rax, [rbp-88]
	mov	QWORD PTR [rbp-16], rax
	lea	rax, [rbp-64]
	sub	rsp, 8
	push	QWORD PTR [rbp-16]
	push	QWORD PTR [rbp-24]
	push	QWORD PTR [rbp-32]
	mov	rdi, rax
	call	eval
	add	rsp, 32
	mov	rax, QWORD PTR [rbp-56]
	mov	QWORD PTR [rbp-8], rax
	mov	rax, QWORD PTR [rbp-48]
	add	QWORD PTR [rbp-8], rax
	mov	rax, QWORD PTR [rbp-64]
	mov	rdx, QWORD PTR [rbp-8]
	imul	rax, rdx
	mov	QWORD PTR [rbp-8], rax
	mov	rax, QWORD PTR [rbp-8]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	my_wrap, .-my_wrap
	.section	.rodata
.LC0:
	.string	"Real wrap: %ld\n"
.LC1:
	.string	"My wrap: %ld\n"
	.text
	.globl	main
	.type	main, @function
main:
.LFB2:
	.cfi_startproc
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	edx, 56
	mov	esi, 22
	mov	edi, 15
	call	wrap
	mov	rsi, rax
	mov	edi, OFFSET FLAT:.LC0
	mov	eax, 0
	call	printf
	mov	edx, 56
	mov	esi, 22
	mov	edi, 15
	call	my_wrap
	mov	rsi, rax
	mov	edi, OFFSET FLAT:.LC1
	mov	eax, 0
	call	printf
	mov	eax, 0
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	main, .-main
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
