.globl lcm_gcd
.type lcm_gcd, @function

arg1        = %rdi
arg2        = %rsi
a           = %rcx
b           = %r8
c           = %r9

.text
lcm_gcd:
    mov arg1, a
    mov arg2, b
    cmp $0, a
    jne loop
    mov $0, %rax
    mov $0, %rdx
    ret
loop:
    mov a, c
    mov b, %rax
    xor %rdx, %rdx
    div a
    mov %rdx, a
    mov c, b

    cmp $0, a
    jne loop

    mov arg1, %rax
    mul arg2
    div c

    mov c, %rdx

quit:
    ret

.size lcm_gcd, . - lcm_gcd
