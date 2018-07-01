.globl fibonacci
.type fibonacci, @function

arg1    = %rdi
retval  = %rax
tmp     = %r8

.text
fibonacci:
    cmp $0, arg1
    jne non_zero
    mov $0, retval
    ret
non_zero:
    cmp $1, arg1
    jne non_one
    mov $1, retval
    ret
non_one:
    push arg1
    sub $1, arg1
    call fibonacci
    pop arg1
    push retval
    sub $2, arg1
    call fibonacci
    pop tmp
    add tmp, retval
    ret

.size fibonacci, . - fibonacci
