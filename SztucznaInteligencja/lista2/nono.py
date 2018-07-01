#!/usr/bin/env python

import random

def opt_dist_old( l, n ):
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

def opt_dist( l, ln ):
    if len( ln ) == 1:
        return opt_dist_old( l, ln[ 0 ] )

    rest = sum( ln ) + len( ln ) - 1
    start = 0
    lowest = 99999
    prefix = 0

    while start + rest - 1 < len( l ):
        first = opt_dist_old( l[ start:start+ln[ 0 ] ], ln[ 0 ] )
        first += prefix + l[ start + ln[ 0 ] ]
        other = opt_dist( l[ start+ln[ 0 ]+1: ], ln[ 1: ] )

        if first + other < lowest:
            lowest = first + other

        prefix += l[ start ]
        start += 1

    return lowest


def opt_distXXXX( l, ln ):
    if not ln:
        return sum( l )
    else:
        start = 0
        rest = sum( map( lambda x: x + 1, ln ) ) - 1
        ret = 0

        for n in ln:
            rest -= n + 1
            prev_start = start
            best = start
            best_value = sum( l[ best:best + n ] )

            start += 1

            while start + n < len( l ) - rest:
                value = best_value - l[ start - 1 ] + l[ start + n - 1 ]

                if value > best_value:
                    best_value = value
                    best = start

                start += 1

            ret += n + sum( l[ prev_start:best + n + 1 ] ) - 2 * best_value
            start = best + n + 1

        return ret + sum( l[ start: ] )

def draw( solution ):
    for v in solution:
        print( ''.join( list( map( lambda e: ('__','##')[e], v ) ) ) )

    print( '' )

def solve_puzzle( vert, horiz ):
    n, m = len(vert), len(horiz)

    rng = random.Random()

    horizontal = [ [ rng.randint(0, 1) for _ in range( n ) ] for _ in range( m ) ]
    vertical = [ [ horizontal[ j ][ i ] for j in range( m ) ] for i in range( n ) ]

    cnt = 0

    while True:
        cnt += 1
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

        if rng.random() > 0.15:
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


with open( 'zad_input.txt', 'rt' ) as f:
    a, b = tuple(map( int, f.readline().strip().split()))

    horizontal = [ list(map(int, f.readline().strip().split())) for _ in range( a ) ]
    vertical = [ list(map(int, f.readline().strip().split())) for _ in range( b ) ]

with open( 'zad_output.txt', 'wt' ) as f:
    solved = None
    k = 0
    while not solved:
        print( k )
        solved = solve_puzzle( horizontal, vertical )
        k += 1

    draw( solved )

    for v in solved:
        f.write( ''.join( list( map( lambda e: ('.','#')[e], v ) ) ) + '\n' )
