#include <stdio.h>

long
switch_prob( long x, long n );  // x: rdi, n: rsi
__asm__(
    ".globl switch_prob\n"
    "switch_prob:\n"
    "sub rsi, 0x3c\n"          // n - 0x3c
    "cmp rsi, 0x5\n"            // n == 5
    "ja this_jump\n"            // rsi > 0x5  --> this_jump
    "jmp []\n"                  // switch( n )
    "lea rax, [rdi * 8]\n"      // ret = x * 8    -- case0, case1
    "ret\n"
    "mov rax, rdi\n"            // ret = x        -- case4
    "sar rax, 3\n"              // ret >>= 3
    "ret\n"
    "mov rax, rdi\n"            // ret = x        -- case2
    "shl rax, 4\n"              // ret <<= 4
    "sub rax, rdi\n"            // ret -= x
    "mov rdi, rax\n"            // x = ret
    "imul rdi, rdi\n"           // x *= x;        -- case5
    "this_jump:\n"              //                -- case3
    "lea rax, [rdi + 0x4b]\n"   // ret = x + 0x4b
    "ret\n"
);

long
my_switch_prob( long x, long n ) {
    long ret;

    n -= 0x3c; // 60
    if( n <= 5 && n != 3 ) {
        switch( n ) {
            case 0:
            case 1: {
                ret = x * 8;
                return ret;
            }

            case 4: {
                ret = x;
                ret >>= 3;
                return ret;
            }

            case 2: {
                ret = x;
                ret <<= 4;
                ret -= x;
                x = ret;
            }
            case 5: {
                x *= ret;
            }
        }
    }

    ret = x + 0x4b; // 75
    return ret;
}

int
main(void) {

    return 0;
}
