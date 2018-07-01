#include <stdio.h>
#include <stdlib.h>

void insert_sort( long *first, long *last );

int
main( int argc, char ** argv ) {
    if( argc < 2 ) {
        printf( "Usage: %s <val1> [<val2> <val3> ... <valN>]\n", argv[ 0 ] );
        return 0;
    }

    long * tab = malloc( (argc - 1) * sizeof( long ) );

    printf( "Unsorted:" );
    for( int i = 1; i < argc; i ++ ) {
        tab[ i - 1 ] = strtol( argv[ i ], (void *) NULL, 10 );
        printf( " %ld", tab[ i - 1 ] );
    }
    putchar( '\n' );

    insert_sort( tab, & tab[ argc - 2 ] );

    printf( "Sorted: " );
    for( int i = 0; i < argc - 1; i ++ ) {
        printf( " %ld", tab[ i ] );
    }
    putchar( '\n' );

    free( tab );

    return 0;
}
