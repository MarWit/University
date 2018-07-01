#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
    int parent;
    int rank;
} uf_t;

typedef struct {
    int x, y;
    int height;
} height_t;

typedef struct {
    int fst;
    int snd;
} tuple_t;

int
find( uf_t *set, int x ) {
    if( set[ x ].parent == -1 ) {
        return -1;
    }

    int ret = x;

    while( ret != set[ ret ].parent ) {
        ret = set[ ret ].parent;
    }

    while( x != set[ x ].parent ) {
        int p = set[ x ].parent;
        set[ x ].parent = ret;
        x = p;
    }

    return ret;
}

bool
do_union( uf_t *set, int x, int y ) {
    int rx = find( set, x );
    int ry = find( set, y );

    if( rx == -1 || ry == -1 ) {
        return false;
    }

    if( rx == ry ) {
        return false;
    }

    if( set[ rx ].rank < set[ ry ].rank ) {
        int tmp = rx;
        rx = ry; ry = tmp;
    }

    set[ ry ].parent = rx;

    if( set[ rx ].rank == set[ ry ].rank ) {
        set[ rx ].rank ++;
    }

    return true;
}

int
main( void ) {
    int n, m;
    scanf( "%d %d", &n, &m );

    uf_t *set = calloc( n * m, sizeof( uf_t ) );
    height_t *heights = calloc( n * m, sizeof( height_t ) );
#define IDX(I,J) (m*(J) + (I))

    int j, i;
    for( j = 0; j < n; ++ j ) {
        for( i = 0; i < m; ++ i ) {
            set[ IDX( i, j ) ].parent = -1;
            set[ IDX( i, j ) ].rank = 0;

            heights[ IDX( i, j ) ].x = i;
            heights[ IDX( i, j ) ].y = j;
            scanf( "%d", & heights[ IDX( i, j ) ].height );
        }
    }

    int cmp( const void * a, const void * b ) {
        return ((height_t *) b) -> height - ((height_t *) a) -> height;
    }

    qsort( heights, n * m, sizeof( height_t ), cmp );

    int T, start = -1;
    scanf( "%d", & T );

    tuple_t *queries = calloc( T, sizeof( tuple_t ) );
    for( i = 0; i < T; ++ i ) {
        scanf( "%d", &queries[ i ].fst );

        if( queries[ i ].fst >= heights[ 0 ].height ) {
            queries[ i ].snd = 0;
            if( start == -1 ) {
                start = i - 1;
            }
        } else {
            queries[ i ].snd = 1;
        }
    }

    start = start == -1 ? T - 1 : start;

    int counter = 0, p;

    for( i = 0; i < m * n; ++ i ) {
        p = IDX( heights[ i ].x, heights[ i ].y );
        set[ p ].parent = p;

        counter ++;

        j = IDX( heights[ i ].x - 1, heights[ i ].y );
        if( heights[ i ].x > 0 && do_union( set, j, p ) ) {
            counter --;
        }

        j = IDX( heights[ i ].x + 1, heights[ i ].y );
        if( heights[ i ].x + 1 < m && do_union( set, j, p ) ) {
            counter --;
        }

        j = IDX( heights[ i ].x, heights[ i ].y - 1 );
        if( heights[ i ].y > 0 && do_union( set, j, p ) ) {
            counter --;
        }

        j = IDX( heights[ i ].x, heights[ i ].y + 1 );
        if( heights[ i ].y + 1 < n && do_union( set, j, p ) ) {
            counter --;
        }

        if( i + 1 < m * n && heights[ i ].height == heights[ i + 1 ].height ) {
            continue;
        }

        while( start >= 0 && i + 1 < m * n && queries[ start ].fst >= heights[ i + 1 ].height ) {
            queries[ start -- ].snd = counter;
        }
    }

    while( start >= 0 ) {
        queries[ start -- ].snd = counter;
    }

    for( i = 0; i < T; ++ i ) {
        printf( "%d ", queries[ i ].snd );
    }

    return 0;
}
