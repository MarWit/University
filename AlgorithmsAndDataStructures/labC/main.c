#include <stdio.h>
#include <stdint.h>
#include <limits.h>

#define MIN(A,B) ((A) < (B) ? (A) : (B))
#define INFINITY (LLONG_MAX / 2ULL)

typedef unsigned long long usize;

int
main( void ) {
    static usize distances[ 1000000 ] = { 0 };
    static usize costs[ 1000000 ] = { 0 };
    static usize dp[ 1000000 ] = { 0 };

#define FULL_COST(I) (dp[ I ] + costs[ I ])
#define IN_RANGE(I, D)  (distances[ I ] + b >= (D))

    usize n, l, b;
    scanf( "%llu %llu %llu", &n, &l, &b );

    if( b >= l ) {
        printf( "0" );
        return 0;
    }

    if( ! n ) {
        printf( "NIE" );
        return 0;
    }

    usize i;
    for( i = 0; i < n; ++ i ) {
        scanf( "%llu %llu", & distances[ i ], & costs[ i ] );

        if( distances[ i ] > b ) {
            dp[ i ] = INFINITY;
        }
    }

    if( distances[ n - 1 ] + b < l ) {
        printf( "NIE" );
        return 0;
    }

    static usize lifo[ 1000000 ] = { 0 };
    usize s = 0, e = 0;

#define PUSH_BACK(E) lifo[ e ++ ] = E
#define POP_BACK() e --
#define POP_FRONT() s ++
#define FIRST() lifo[ s ]
#define LAST() lifo[ e - 1 ]
#define EMPTY() (s == e)

    for( i = 0; i < n; ++ i ) {
        if( ! EMPTY() ) {
            dp[ i ] = MIN( dp[ i ], FULL_COST( FIRST() ) );
        }

        while( ! EMPTY() && FULL_COST( LAST() ) > FULL_COST( i ) ) {
            POP_BACK();
        }

        PUSH_BACK( i );

        while( i < n - 1 && ! EMPTY() && ! IN_RANGE( FIRST(), distances[ i + 1 ] ) ) {
            POP_FRONT();
        }
    }

    usize min = INFINITY;

    for( i = n; i > 0; i -- ) {
        if( ! IN_RANGE( i - 1, l ) ) {
            break;
        }

        min = MIN( FULL_COST( i - 1 ), min );
    }

    if( min == INFINITY ) {
        printf( "NIE" );
    } else {
        printf( "%llu", min );
    }
}
