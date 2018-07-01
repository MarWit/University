#include <stdio.h>
#include <stdlib.h>

double approx_sqrt( double x, double epsilon );

int
main( int argc, char ** argv ) {
    if( argc < 3 ) {
        printf( "Usage: %s <x> <epsilon>\n", argv[ 0 ] );
        return 0;
    }


    double x = atof( argv[ 1 ] );
    double epsilon = atof( argv[ 2 ] );

    printf( "mulf( %f, %f ) = %f", x, epsilon, approx_sqrt( x, epsilon ) );

    return 0;
}
