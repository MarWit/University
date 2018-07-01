	.file	"zadanie2.c"
	.text
	.globl	main
	.type	main, @function
main:
.LFB0:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
#APP
# 5 "zadanie2.c" 1
	int3
# 0 "" 2
#NO_APP
	call	p2
#APP
# 7 "zadanie2.c" 1
	int3
# 0 "" 2
#NO_APP
	movl	$0, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 6.3.1 20170306"
	.section	.note.GNU-stack,"",@progbits
