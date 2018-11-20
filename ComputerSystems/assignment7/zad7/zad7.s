.globl _start
.type _start, @function

.globl buffer
.type buffer, @object
.size buffer, 0xff

read        = 0
write       = 1
exit        = 60

c           = %r8b
i           = %rcx
num         = %r9

.bss
buffer:
    .zero 0xff
    .align 16

.text
_start:
    mov $read, %rax
    mov $0, %rdi
    mov $buffer, %rsi
    mov $0xff, %rdx
    syscall

    mov %rax, num

    xor i, i
loop:
    mov buffer( i ), c

    test c, c
    je end

    cmp $122, c
    ja next
    cmp $65, c
    jb next

    cmp $97, c
    jge higher
    add $64, buffer( i )
higher:
    sub $32, buffer( i )
next:
    add $1, i
    cmp num, i
    jbe loop
end:
    mov $write, %rax
    mov $1, %rdi
    mov $buffer, %rsi
    mov num, %rdx
    syscall

    test num, num
    jne _start
quit:
    mov $exit, %rax
    mov $0, %rdi
    syscall

    ret

.size _start, . - _start
