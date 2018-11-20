.globl mulf
.type mulf, @function

arg1        = %rdi
arg2        = %rsi
retval      = %rax
tmp         = %r8
tmp2        = %r9

ONE_COMMA       = 0x800000
MANTISSA_MASK   = 0x7fffff
WOSIGN          = 0x7fffffff

.text
mulf:
    test $WOSIGN, arg1
    jne arg1_nonzero

    mov arg1, retval
    shr $31, retval
    shr $31, arg2
    xor arg2, retval
    shl $31, retval
    ret

arg1_nonzero:
    test $WOSIGN, arg2
    jne arg2_nonzero

    mov arg2, retval
    shr $31, retval
    shr $31, arg1
    xor arg1, retval
    shl $31, retval
    ret

arg2_nonzero:
    movq arg1, tmp              # tmp = arg1
    movq arg2, tmp2             # tmp2 = arg2

    shr $31, tmp
    shr $31, tmp2

    xor tmp2, tmp
    shl $31, tmp
    push tmp

    mov arg1, tmp
    mov arg2, tmp2

    and $MANTISSA_MASK, tmp          # tmp &= 0x7fffff
    and $MANTISSA_MASK, tmp2         # tmp &= 0x7fffff

    or $ONE_COMMA, tmp
    or $ONE_COMMA, tmp2

    mov tmp, %rax               # rax = tmp
    mul tmp2                    # rdx:rax = tmp * tmp2
    mov %rax, tmp2              # tmp2 = rax
    sar $23, tmp2

    sar $23, arg1               # arg1 >>= 23
    sar $23, arg2               # arg2 >>= 23

    sub $0x7f, arg1             # arg1 -= 127
    sub $0x7f, arg2             # arg2 -= 127

    add arg2, arg1              # arg1 += arg2
normalize:                      # while( 1 ) {
    test $0x7f000000, tmp2      #   if( !( tmp2 & 0x7e000000 ) )
    je end                      #       break;
    add $1, arg1                #   arg1 ++
    shr $1, tmp2                #   tmp2 >>= 1
    jmp normalize               # }
end:
    add $0x7f, arg1             # arg1 += 127
    sal $23, arg1               # arg1 <<= 23

    mov arg1, retval            # retval = arg1
    and $MANTISSA_MASK, tmp2
    add tmp2, retval            # retval += tmp2
    pop tmp
    or tmp, retval

    ret                         # return retval

.size mulf, . - mulf
