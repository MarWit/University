#include <stdio.h>

typedef struct A {
    long u[ 2 ];
    long *v;
} SA;

typedef struct B {
    long p[ 2 ];
    long q;
} SB;

SB
eval( SA s );
__asm__(
    ".globl eval\n"
    "eval:\n"
    "mov rax, rdi\n"            // rax = ret structure
    "mov rcx, [rsp + 16]\n"     // rcx = y
    "mov rdx, [rsp + 24]\n"     // rdx = &z
    "mov rsi, [rdx]\n"          // rsi = z
    "mov rdx, rcx\n"            // rdx = y
    "imul rdx, rsi\n"           // y *= z
    "mov [rdi], rdx\n"          // *some_pointer = y
    "mov rdx, [rsp + 8]\n"      // rdx = x
    "mov rdi, rdx\n"            // rdi = x
    "sub rdi, rsi\n"            // some_pointer -= z
    "mov [rax + 8], rdi\n"      // *( some_pointer + 8 ) = some_pointer - z
    "sub rdx, rcx\n"            // x -= orig_y
    "mov [rax + 16], rdx\n"     // *(some_pointer + 16) = y
    "ret\n"
);

SB
my_eval( SA s ) {
    SB ret;

    ret . q      = s . u[ 0 ] - s . u[ 1 ];
    ret . p[ 0 ] = s . u[ 1 ] * *( s . v );
    ret . p[ 1 ] = s . u[ 0 ] - *( s . v );

    return ret;
}

long
wrap( long x, long y, long z );
__asm__(
    ".globl wrap\n"
    "wrap:\n"
    "sub rsp, 72\n"             // rsp - 72 (???)
    "mov [rsp], rdx\n"          // *rsp = z
    "mov rdx, rsp\n"            // rdx = &z
    "lea rax, [rsp + 8]\n"      // ret = rsp + 8
    "push rdx\n"                // push &z  <-- sub esp, 8
    "push rsi\n"                // push y   <-- sub esp, 8
    "push rdi\n"                // push x   <-- sub esp, 8
    "mov rdi, rax\n"            // x = ret
    "call eval\n"               // eval()
    "mov rax, [rsp + 40]\n"     // ret  = *(rsp + 40)
    "add rax, [rsp + 32]\n"     // ret += *(rsp + 32)
    "imul rax, [rsp + 48]\n"    // rax *= *(rsp + 48)
    "add rsp, 96\n"             // rsp += 96
    "ret\n"                     // return rax
);

long
my_wrap( long x, long y, long z ) {
    long ret;

    SA argument;
    argument . u[ 1 ] = y;
    argument . u[ 0 ] = x;
    argument . v      = &z;

    SB evaled = eval( argument );

    ret = evaled . p[ 1 ];
    ret += evaled . p[ 0 ];
    ret *= evaled . q;

    return ret;
}

int
main(void) {
    printf( "Real wrap: %ld\n", wrap( 58, 22, 56 ) );
    printf( "My wrap:   %ld\n", my_wrap( 58, 22, 56 ) );

    return 0;
}
