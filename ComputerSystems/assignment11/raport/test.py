#!/usr/bin/env python

import math

def heapify( arr ):
    n = len( arr )
    dst = [0 for i in range( n )]

    def heap_it( p, i, a, s ):
        if( p >= len( a ) ): return

        a[ p ] = s[ i ]

        heap_it( p * 2 + 1, i // 2, a, s[ :i ] )
        heap_it( p * 2 + 2, i // 2, a, s[ (i+1): ] )

    heap_it( 0, n // 2, dst, arr )
    return dst


HEIGHT = 4
WHAT = list( range( 1, 1 << HEIGHT ) )
t = heapify( WHAT )

def print_tree( prefix, tree, p, isLeft ):
    if p < len( tree ):
        print( prefix + ( '\\--', '|--' )[ isLeft ] + str( tree[ p ] ) )
        print_tree( prefix + ( '    ', '|   ' )[ isLeft  ], tree, p * 2 + 1, True )
        print_tree( prefix + ( '    ', '|   ' )[ isLeft  ], tree, p * 2 + 2, False )

print_tree( '', t, 0, False )
