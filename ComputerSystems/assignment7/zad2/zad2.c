#include <stdio.h>
#include <stdlib.h>

typedef struct {
    unsigned long lcm, gcd;
} result_t;

result_t lcm_gcd( long a, long b );

int
main( int argc, char ** argv ) {
    if( argc < 3 ) {
        printf( "Usage: %s <val1> <val2>\n", argv[ 0 ] );
        return 0;
    }

    long a = strtol( argv[ 1 ], (void *) NULL, 10 );
    long b = strtol( argv[ 2 ], (void *) NULL, 10 );

    result_t res = lcm_gcd( a, b );
    printf( "lcm: %lu, gcd: %lu\n", res . lcm, res . gcd );


    return 0;
}
