#!/usr/bin/env python

import random

def opt_dist( l, n ):
    sum_ = sum( l )

    if len( l ) == n:
        return n - sum_
    else:
        value = sum( l[ :n ] )
        best_value = value

        for i in range( 1, len( l ) - n + 1 ):
            value = value - l[ i - 1 ] + l[ i + n - 1 ]

            if value > best_value:
                best_value = value

        return n + sum_ - 2 * best_value

def solve_puzzle( vert, horiz ):
    n, m = len(vert), len(horiz)

    rng = random.Random()

    horizontal = [ [ rng.randint(0, 1) for _ in range( n ) ] for _ in range( m ) ]
    vertical = [ [ horizontal[ j ][ i ] for j in range( m ) ] for i in range( n ) ]

    cnt = 0

    while True:
        if cnt > 200:
            return None

        mistakes = []

        for (i, k) in enumerate( vertical ):
            if opt_dist( k, vert[ i ] ) != 0:
                mistakes.append( (0, i) )

        for (i, k) in enumerate( horizontal ):
            if opt_dist( k, horiz[ i ] ) != 0:
                mistakes.append( (1, i) )

        if len( mistakes ) == 0:
            return vertical

        (is_horiz, idx) = rng.choice( mistakes )

        best = 9999
        a, b = -1, -1

        if rng.random() > 0.1:
            if is_horiz:
                for (i, k) in enumerate( vertical ):
                    horizontal[ idx ][ i ] ^= 1
                    vertical[ i ][ idx ] ^= 1

                    new_badness = opt_dist( horizontal[ idx ], horiz[ idx ] ) + opt_dist( vertical[ i ], vert[ i ] )
                    if new_badness < best:
                        best = new_badness
                        a, b = idx, i

                    horizontal[ idx ][ i ] ^= 1
                    vertical[ i ][ idx ] ^= 1
            else:
                for (i, k) in enumerate( horizontal ):
                    horizontal[ i ][ idx ] ^= 1
                    vertical[ idx ][ i ] ^= 1

                    new_badness = opt_dist( horizontal[ i ], horiz[ i ] ) + opt_dist( vertical[ idx ], vert[ idx ] )
                    if new_badness < best:
                        best = new_badness
                        a, b = i, idx

                    horizontal[ i ][ idx ] ^= 1
                    vertical[ idx ][ i ] ^= 1

            horizontal[ a ][ b ] ^= 1
            vertical[ b ][ a ] ^= 1
        else:
            a, b = rng.randint(0, m - 1), rng.randint(0, n - 1)
            horizontal[ a ][ b ] ^= 1
            vertical[ b ][ a ] ^= 1


def draw( solution ):
    for v in solution:
        print( ''.join( list( map( lambda e: ('.','#')[e], v ) ) ) )

input_ = [
   ([7,7,7,7,7,7,7], [7,7,7,7,7,7,7]),
   ([2,2,7,7,2,2,2], [2,2,7,7,2,2,2]),
   ([2,2,7,7,2,2,2], [4,4,2,2,2,5,5]),
   ([7,6,5,4,3,2,1], [1,2,3,4,5,6,7]),
   ([7,5,3,1,1,1,1], [1,2,3,7,3,2,1])
]

for in_ in input_:
    solution = None
    while solution is None:
        solution = solve_puzzle( *in_ )

    draw( solution )
    print( "\n" )
