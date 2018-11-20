.globl approx_sqrt
.type approx_sqrt, @function

x               = %xmm0
epsilon         = %xmm1
a               = %xmm2
tmp             = %xmm3
last_x          = %xmm4
flag            = %rax

TRUE            = 0xffffffffffffffff

.section .rodata
half:
    .double 0.5
    .type half, @object
    .size half, 8
zero:
    .double 0.0
    .type zero, @object
    .size zero, 8
minus_one:
    .double -1.0
    .type minus_one, @object
    .size minus_one, 8

.text
approx_sqrt:
    movsd %xmm0, a
    movsd %xmm1, epsilon
loop:
    movsd x, last_x
    movsd a, tmp
    divsd x, tmp
    addsd tmp, x
    mulsd half, x

    movsd x, tmp
    subsd last_x, tmp
    movsd tmp, last_x
    cmpltsd zero, tmp       # tmp < 0
    movq tmp, flag          # %rax = %xmm3
    cmp $TRUE, flag
    jne non_negative
    mulsd minus_one, last_x
non_negative:
    cmpltsd epsilon, last_x
    movq last_x, flag
    cmp $TRUE, flag
    jne loop

.size approx_sqrt, . - approx_sqrt
