.globl main
.type main, @function

exit        = 60

i           = %rcx
cnt         = %r8
min         = %rax
max         = %rdx
tmp         = %r9

.data
message:
    .asciz "Max: %d, min: %d\n"
    .type message, @object
    .size message, 19

.text
main:
    cmp $2, %rdi
    jb quit
    mov $1, i
    mov %rdi, cnt
convert:
    mov ( %rsi, i, 8 ), %rdi
    push %rsi
    push cnt
    push i

    call atol

    pop i
    pop cnt
    pop %rsi
    push %rax

    inc i
    cmp cnt, i
    jb convert

    sub $1, cnt

    xor i, i
    mov $0x7fffffffffffffff, min
    mov $0, max
find_max_min:
    mov ( %rsp, i, 8 ), tmp
    cmp max, tmp
    jl lower
    mov tmp, max
lower:
    cmp min, tmp
    jge bigger
    mov tmp, min
bigger:
    inc i
    cmp cnt, i
    jb find_max_min

    lea ( , cnt, 8 ), cnt
    add cnt, %rsp

    mov $message, %rdi
    mov max, %rsi
    mov min, %rdx
    mov $0, %rax
    call printf

quit:
    mov $exit, %rax
    mov $0, %rdi
    syscall

    ret

.size main, . - main
