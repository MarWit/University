#!/usr/bin/env python

def full_adder( a, b ):
    return (((a+b) & 3), (a + b) > 3)

def mult( a, b ):
    x = (a * b) & 15
    return ( x >> 2, x & 3 )

N1 = 0b1101
N2 = 0b1011

(ab,cd) = (N1>>2,N1&3)
(ef,gh) = (N2>>2,N2&3)

A = mult( cd, gh )
B = mult( ab, gh )
C = mult( cd, ef )
D = mult( ab, ef )

R01 = A[ 1 ]

(R23, C23_1) = full_adder( B[ 1 ], A[ 0 ] )
(R23, C23_2) = full_adder( C[ 1 ], R23 )

(C23, _) = full_adder( C23_1, C23_2 )

(R45, C45_1) = full_adder( B[ 0 ], C23 )
(R45, C45_2) = full_adder( C[ 0 ], R45 )
(R45, C45_3) = full_adder( D[ 1 ], R45 )

(C45, _) = full_adder( C45_1, C45_2 )
(C45, _) = full_adder( C45, C45_3 )

(R67, _) = full_adder( C45, D[ 0 ] )

print( bin( (R67 << 6) | (R45 << 4) | (R23 << 2) | R01 ) )
print( bin( (N1 * N2) & 0xff ) )

(R45, C45) = full_adder( B[ 0 ], C23 )




