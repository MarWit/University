#include <stdio.h>
#include <stdlib.h>

unsigned long fibonacci( unsigned long );

int
main( int argc, char ** argv ) {
    if( argc < 2 ) {
        printf( "Usage: %s <number>\n", argv[ 0 ] );
        return 0;
    }

   unsigned long n = strtol( argv[ 1 ], (void *) NULL, 10 );
   printf( "fibonacci( %lu ) = %lu\n", n, fibonacci( n ) );

    return 0;
}
