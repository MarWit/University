#include <stdio.h>
#include <stdlib.h>

int clz(long);

int
main( int argc, char ** argv ) {
    if( argc < 2 ) {
        printf( "Usage: %s <num>\n", argv[ 0 ] );
        return 0;
    }

    long n = strtol( argv[ 1 ], (void *) NULL, 10 );
    printf( "clz( %lu ) = %d\n", n, clz( n ) );

    return 0;
}
