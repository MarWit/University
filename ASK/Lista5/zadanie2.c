#include <stdio.h>
#include <inttypes.h>

long
original_func( char * s, char * d ); // rdi: s, rsi: d
__asm__(
    ".globl original_func\n"
    "original_func:\n"
    "movq %rdi, %rax\n"         // ret = s
    ".myL3:\n"
    "leaq 1(%rax), %r8\n"       // a = ret + 1
    "movb -1(%r8), %r9b\n"      // b = *( a - 1 )
    "movq %rsi, %rdx\n"         // c = d
    ".myL2:\n"
    "incq %rdx\n"               // d ++
    "movb -1(%rdx), %cl\n"      // cl = *( d - 1 )
    "testb %cl, %cl\n"          // cl == 0
    "je .myL7\n"
    "cmpb %cl, %r9b\n"          // cl != b
    "jne .myL2\n"
    "movq %r8, %rax\n"          // ret = a
    "jmp .myL3\n"
    ".myL7:\n"
    "subq %rdi, %rax\n"         // ret -= s
    "ret\n"
);

long
my_func( char * s, char * d ) {
    char * ret = s;
    char * a;
    char b;
    char * c;
    char cl;

    while( 1 ) {
        a = ret + 1;
        b = *( a - 1 );
        c = d;

        do {
            d ++;
            cl = *( d - 1 );
        } while( cl && cl != b );

        if( ! cl ) break;

        ret = a;
    }

    ret -= (long) s;
    return (long) ret;
}

long
my_func_clean( char * s, char * d ) {
    long count = 0;

    for( ; *d; s ++ ) {
        while( *d && *d != *s ) d ++;
        if( ! *d ) break;

        count ++;
    }

    return count;
}

int
main( void ) {
    char * a = "abtyfajdfxxadj";
    char * b = "abcdbdefaxx";

    printf( "%ld\n", my_func( a, b ) );
    printf( "%ld\n", my_func( a, a ) );
    printf( "%ld\n\n", my_func( b, b ) );

    printf( "%ld\n", my_func_clean( a, b ) );
    printf( "%ld\n", my_func_clean( a, a ) );
    printf( "%ld\n\n", my_func_clean( b, b ) );

    printf( "%ld\n", original_func( a, b ) );
    printf( "%ld\n", original_func( a, a ) );
    printf( "%ld\n", original_func( b, b ) );

    return 0;
}
