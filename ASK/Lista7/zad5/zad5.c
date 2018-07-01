#include <stdio.h>
#include <stdlib.h>

unsigned mulf( unsigned a, unsigned b );

typedef union {
    float f;
    unsigned i;
} f_to_i;

int
main( int argc, char ** argv ) {
    if( argc < 3 ) {
        printf( "Usage: %s <val2> <num2>\n", argv[ 0 ] );
        return 0;
    }

    f_to_i a;
    a . f = atof( argv[ 1 ] );

    f_to_i b;
    b . f = atof( argv[ 2 ] );

    f_to_i ret;
    ret . i = mulf( a . i, b . i );

    printf( "mulf( %f, %f ) = %f\n", a . f, b . f, ret . f );

    return 0;
}
