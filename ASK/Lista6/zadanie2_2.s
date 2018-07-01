	.file	"zadanie2_2.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"0x%x\n"
	.text
	.globl	p2
	.type	p2, @function
p2:
.LFB11:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movsbl	main(%rip), %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE11:
	.size	p2, .-p2
	.comm	main,1,1
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
