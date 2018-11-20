.globl clz
.type clz, @function

arg1        = %rdi
n           = %cl       # low byte of rcx
x           = %r8
retval      = %rax

.text
clz:                            #
    xor %rcx, %rcx              #   rcx = 0
    mov $0x40, retval           #   retval = 64
    test arg1, arg1             #   if( ! arg1 )
    jz end                      #       return retval;
    xor retval, retval          #   retval = 0;
    mov $0x20, n                #   n = 32
    mov $0xffffffff00000000, x  #   x = 0xffffffff00000000;
loop:                           #   do {
    test x, arg1                #       if( arg1 & x == 0 )
    jnz non_zero                #       {
    add %rcx, retval            #           retval += n;
    sal n, arg1                 #           arg1 <<= n;
non_zero:                       #       }
    sar $1, n                   #       n /= 2;
    sal n, x                    #       x <<= n
    test n, n                   #   } while( n )
    jnz loop                    #
end:                            #
    ret                         # return ret

.size clz, . - clz
