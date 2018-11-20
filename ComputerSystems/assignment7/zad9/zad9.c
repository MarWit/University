#include <stdio.h>
#include <stdlib.h>

#define ADDS(X,Y,O)   __asm__(                              \
                        "mov $0x7fffffffffffffff, %%r8\n"   \
                        "cmp $0, %1\n"                      \
                        "jg positive%=\n"                   \
                        "cmp $0, %2\n"                      \
                        "jg positive%=\n"                   \
                        "mov $0x8000000000000000, %%r8\n"   \
                        "positive%=:\n"                     \
                        "mov %1, %0\n"                      \
                        "add %2, %0\n"                      \
                        "cmovo %%r8, %0\n"                  \
                        : "=r"(O) : "r"(X), "r"(Y) : "r8"   \
                    )

long
adds( long x, long y ) {
    long r;
    ADDS( x, y, r );
    return r;
}

int
main( int argc, char ** argv ) {
    if( argc < 3 ) {
        printf( "Usage: %s <val1> <val2>\n", argv[ 0 ] );
        return 0;
    }

    long a = strtol( argv[ 1 ], (void *) NULL, 10 );
    long b = strtol( argv[ 2 ], (void *) NULL, 10 );

    printf( "%ld + %ld = %ld\n", a, b, adds( a, b ) );

    return 0;
}
