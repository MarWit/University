#!/usr/bin/env python

import re
import subprocess

N = range( 1, 10 )
S = 300
T = 50
R = 30

rout = re.compile( r'Time elapsed: ([0-9]+.[0-9]+)' )

print( "#KB, TIME" )
for i in N:
    val = 0.0

    for _ in range( R ):
        out = subprocess.check_output( [ './cache{}'.format( i ), '-n', str( 20 ), '-t', str( T ), '-s', str( S ) ] )
        out = rout.search( out.decode( 'utf-8' ) ).group( 1 )

        val += float( out )

    val /= R
    print( '{}, {:.10f}'.format( 1 << i, val * 1000 ) )


