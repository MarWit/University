#include <stdio.h>
#include <stdlib.h>

typedef unsigned int usize;
#define USIZE_FORMAT "%d"

// 20bytes + 4byte padding
typedef struct node_s {
    usize id;
    usize right;
    usize bottom;
    usize last;
    usize preorder;
    usize postorder;
} node_t;

#define MAX_SIZE 1000000
static node_t tree[ MAX_SIZE + 1 ] = {};

static inline void
node_add_right( usize node, usize child ) {
    // while( tree[ node ].right != 0 )
    //     node = tree[ node ].right;

    usize root = node;

    if( tree[ root ].right != 0 )
        node = tree[ root ].last;

    tree[ node ].right = child;
    tree[ root ].last = child;
}

static inline void
node_add_bottom( usize node, usize child ) {
    if( tree[ node ].bottom != 0 ) {
        node_add_right( tree[ node ].bottom, child );
    } else {
        tree[ node ].bottom = child;
    }
}

void
dfs( usize node ) {
    usize i = 0;
    static usize queue[ MAX_SIZE ] = { 0 };

    usize pre = 1, post = 1;

    for( ;; ) {
        tree[ node ].preorder = pre ++;

        if( tree[ node ].bottom != 0 ) {
            queue[ i ++ ] = node;
            node = tree[ node ].bottom;
        } else if( tree[ node ].right != 0 ) {
            tree[ node ].postorder = post ++;
            node = tree[ node ].right;
        }
        else if( i > 0 ) {
            tree[ node ].postorder = post ++;
            do {
                node = queue[ --i ];
                tree[ node ].postorder = post ++;
            } while( i > 0 && ! tree[ node ].right );

            if( tree[ node ].right != 0 )
                node = tree[ node ].right;
            else break;
        } else break;
    }
}

int
main( void ) {
    usize n, q;
    scanf( USIZE_FORMAT " " USIZE_FORMAT, &n, &q );

    tree[ 1 ].id = 1;
    usize i, t;

    for( i = 2; i <= n; ++ i ) {
        scanf( USIZE_FORMAT, &t );

        tree[ i ].id = i;
        node_add_bottom( t, i );
    }

    // DFS with preorder and postorder
    dfs( 1 );

    usize a, b;

    while( q-- ) {
        scanf( "%d %d", & a, & b );
        if( tree[ a ].preorder < tree[ b ].preorder &&
            tree[ a ].postorder > tree[ b ].postorder ) {
            puts( "TAK" );
        } else puts( "NIE" );
    }
}
