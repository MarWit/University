#!/usr/bin/env python

def nand( a, b, c = True ):
    return not ( a and b and c )

def connect( a, b, c, d ):
    return (
        (a<<3) |
        (b<<2) |
        (c<<1) |
        d
    )

print( "a | b | c |   O" )

for i in range( 0b1000 ):
    (a, b, c) = (i>>2, (i>>1)&1, i&1)

    # ~ab~c + ~abc + a~bc
    value = connect( 0, not a, b, not c ) + connect( 0, not a, b, c ) + connect( 0, a, not b, c )
    print( "{} | {} | {} | {}".format( a, b, c, bin( value ) ) )
